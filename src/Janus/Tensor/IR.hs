{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Public, AD-agnostic, differentially closed, content-addressed (Merkle
-- SHA-256) typed DAG IR for @Janus.Tensor@.
--
-- All core constructors—@For@, @Index@, @Reindex@ (stratified optics),
-- @Reduce@ (open 'TensorMonoid'), @ScatterMonoid@ / @RunAccum@ (open
-- 'TensorCommMonoid'), @RunST@ (rank-2 local state), @Scan@, @AssocScan@,
-- @Contract@ (unplanned N-ary open 'TensorSemiring' hypergraph), @Select@,
-- @CustomPrim@, and @Annotate@—are exposed so downstream libraries (such as a
-- separate automatic differentiation package) can inspect and transform IR
-- graphs before @Janus.Tensor@ plans contractions, fuses loops, and lowers to
-- Janus CPU/CUDA code.
module Janus.Tensor.IR
  ( -- * Content-Addressed Merkle Digests
    Digest (..),
    mkIRDigest,

    -- * Index Expressions
    IxExpr (..),
    ixExprShape,
    ixExprDigest,

    -- * Scalar Expressions (`RawExpr` & Level-Indexed `Expr`)
    ScalarUnOp (..),
    ScalarBinOp (..),
    ScalarCmpOp (..),
    HExprArgs (..),
    RawExpr (..),
    rawExprDigest,
    Expr (..),
    buildExpr,
    liftRawExpr,

    -- * Table Nodes (`RawNode` & Level-Indexed `TNode`)
    HNodeArgs (..),
    ContractOperand (..),
    ContractSpec (..),
    STStmt (..),
    STProgram (..),
    AccumStmt (..),
    AccumProgram (..),
    RawNode (..),
    rawNodeDigest,
    rawNodeShape,
    TNode (..),
    buildNode,
    liftRawNode,

    -- * Representable Surface Table (`Tab i a`)
    Tab (..),
    ScalarTab,
    VectorTab,
    MatrixTab,
    Tensor3Tab,
    Tensor4Tab,
    Tensor5Tab,
    Tensor6Tab,
    Tensor7Tab,
    Tensor8Tab,
    Tensor9Tab,
    TensorTab,
    fromNode,
    materialize,

    -- * Smart Constructors (Merkle-Hashed & Beta-Eta Simplifying)
    mkLit,
    mkScalarVar,
    mkOrd,
    mkShapeSize,
    mkUnOp,
    mkBinOp,
    mkCmp,
    mkSelect,
    mkCast,
    mkIndex,
    mkReduce,
    mkScalarPrim,
    mkAnnotateExpr,
    mkRunSTScalar,
    mkInput,
    mkFor,
    mkReindex,
    mkScatter,
    mkRunAccum,
    mkRunST,
    mkScan,
    mkAssocScan,
    mkContract,
    mkKernelPrimNode,
    mkAnnotateNode,
  )
where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.Int (Int64)
import qualified Data.Kind as K
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.TypeNats (Nat)
import Janus.Tensor.Algebra
  ( SomeCommMonoid,
    SomeMonoid,
    SomeSemiring,
    TensorScalar (..),
    commMonoidDigest,
    monoidDigest,
    semiringDigest,
  )
import Janus.Tensor.Custom (KernelPrimSpec (..), ScalarPrimSpec (..))
import Janus.Tensor.Index
  ( DimSpec (..),
    IxIso,
    IxKind (..),
    IxMorph,
    IxSet (..),
    SIx (..),
    eqSIx,
    isoDigest,
    isoDst,
    morphDigest,
    morphSrc,
    sixDigest,
    sixStaticSize,
  )
import Janus.Tensor.Types (PrimAttrMap, ShapeToIx)
import Linear.Conjugate (Conjugate (..))
import Linear.Metric (Metric (..))
import Linear.Trace (Trace (..))
import Linear.Vector (Additive (..))

--------------------------------------------------------------------------------
-- 1. Merkle SHA-256 Digests
--------------------------------------------------------------------------------

newtype Digest = Digest {getDigest :: BS.ByteString}
  deriving stock (Eq, Ord)

instance Show Digest where
  show (Digest bs) = "TDigest:" <> take 16 (BS8.unpack (B16.encode bs))

frame :: BS.ByteString -> BSB.Builder
frame bs = BSB.int64LE (fromIntegral (BS.length bs)) <> BSB.byteString bs

encInt :: Int -> BS.ByteString
encInt n = BS.toStrict (BSB.toLazyByteString (BSB.int64LE (fromIntegral n)))

encText :: Text.Text -> BS.ByteString
encText = Text.encodeUtf8

mkIRDigest :: BS.ByteString -> [BS.ByteString] -> [Digest] -> Digest
mkIRDigest tag imms kids =
  Digest . SHA256.hashlazy . BSB.toLazyByteString $
    frame tag
      <> BSB.int64LE (fromIntegral (length imms))
      <> foldMap frame imms
      <> BSB.int64LE (fromIntegral (length kids))
      <> foldMap (frame . getDigest) kids

--------------------------------------------------------------------------------
-- 2. Index Expressions (`IxExpr`)
--------------------------------------------------------------------------------

data IxExpr (i :: IxKind) where
  IxVar :: !Int -> !(SIx i) -> IxExpr i
  IxConst :: !Int -> !(SIx i) -> IxExpr i
  IxPair :: !(IxExpr i) -> !(IxExpr j) -> IxExpr ('IxProd i j)
  IxFst :: !(IxExpr ('IxProd i j)) -> IxExpr i
  IxSnd :: !(IxExpr ('IxProd i j)) -> IxExpr j
  IxInL :: !(SIx j) -> !(IxExpr i) -> IxExpr ('IxSum i j)
  IxInR :: !(SIx i) -> !(IxExpr j) -> IxExpr ('IxSum i j)
  IxTagE :: (KnownSymbol s) => !(Proxy s) -> !(IxExpr i) -> IxExpr ('IxTagged s i)
  IxUntagE :: !(IxExpr ('IxTagged s i)) -> IxExpr i
  IxApplyIso :: !(IxIso i j) -> !(IxExpr i) -> IxExpr j
  IxFromOrd :: !(SIx i) -> !(RawExpr Int64) -> IxExpr i

ixExprShape :: IxExpr i -> SIx i
ixExprShape = \case
  IxVar _ s -> s
  IxConst _ s -> s
  IxPair a b -> SIxProd (ixExprShape a) (ixExprShape b)
  IxFst p -> case ixExprShape p of SIxProd a _ -> a
  IxSnd p -> case ixExprShape p of SIxProd _ b -> b
  IxInL sb a -> SIxSum (ixExprShape a) sb
  IxInR sa b -> SIxSum sa (ixExprShape b)
  IxTagE p a -> SIxTagged p (ixExprShape a)
  IxUntagE a -> case ixExprShape a of SIxTagged _ s -> s
  IxApplyIso iso _ -> isoDst iso
  IxFromOrd s _ -> s

ixExprDigest :: IxExpr i -> Digest
ixExprDigest = \case
  IxVar v s -> mkIRDigest "IxVar" [encInt v, sixDigest s] []
  IxConst c s -> mkIRDigest "IxConst" [encInt c, sixDigest s] []
  IxPair a b -> mkIRDigest "IxPair" [] [ixExprDigest a, ixExprDigest b]
  IxFst a -> mkIRDigest "IxFst" [] [ixExprDigest a]
  IxSnd a -> mkIRDigest "IxSnd" [] [ixExprDigest a]
  IxInL sb a -> mkIRDigest "IxInL" [sixDigest sb] [ixExprDigest a]
  IxInR sa b -> mkIRDigest "IxInR" [sixDigest sa] [ixExprDigest b]
  IxTagE p a -> mkIRDigest "IxTag" [encText (Text.pack (symbolVal p))] [ixExprDigest a]
  IxUntagE a -> mkIRDigest "IxUntag" [] [ixExprDigest a]
  IxApplyIso iso a -> mkIRDigest "IxIso" [isoDigest iso] [ixExprDigest a]
  IxFromOrd s e -> mkIRDigest "IxOrd" [sixDigest s] [rawExprDigest e]

--------------------------------------------------------------------------------
-- 3. Scalar Operations & Expressions (`RawExpr`, `Expr`)
--------------------------------------------------------------------------------

data ScalarUnOp a where
  UNeg :: (Num a) => ScalarUnOp a
  UAbs :: (Num a) => ScalarUnOp a
  USignum :: (Num a) => ScalarUnOp a
  URecip :: (Fractional a) => ScalarUnOp a
  UExp :: (Floating a) => ScalarUnOp a
  ULog :: (Floating a) => ScalarUnOp a
  USqrt :: (Floating a) => ScalarUnOp a
  USin :: (Floating a) => ScalarUnOp a
  UCos :: (Floating a) => ScalarUnOp a
  UTan :: (Floating a) => ScalarUnOp a
  UAsin :: (Floating a) => ScalarUnOp a
  UAcos :: (Floating a) => ScalarUnOp a
  UAtan :: (Floating a) => ScalarUnOp a
  USinh :: (Floating a) => ScalarUnOp a
  UCosh :: (Floating a) => ScalarUnOp a
  UTanh :: (Floating a) => ScalarUnOp a
  UAsinh :: (Floating a) => ScalarUnOp a
  UAcosh :: (Floating a) => ScalarUnOp a
  UAtanh :: (Floating a) => ScalarUnOp a
  ULGamma :: (Floating a) => ScalarUnOp a
  UDigamma :: (Floating a) => ScalarUnOp a
  UTrigamma :: (Floating a) => ScalarUnOp a
  UErf :: (Floating a) => ScalarUnOp a
  UErfc :: (Floating a) => ScalarUnOp a
  UErfInv :: (Floating a) => ScalarUnOp a
  ULog1p :: (Floating a) => ScalarUnOp a
  UExpm1 :: (Floating a) => ScalarUnOp a
  URsqrt :: (Floating a) => ScalarUnOp a
  UNot :: ScalarUnOp Bool

unOpTag :: ScalarUnOp a -> BS.ByteString
unOpTag = \case
  UNeg -> "neg"
  UAbs -> "abs"
  USignum -> "signum"
  URecip -> "recip"
  UExp -> "exp"
  ULog -> "log"
  USqrt -> "sqrt"
  USin -> "sin"
  UCos -> "cos"
  UTan -> "tan"
  UAsin -> "asin"
  UAcos -> "acos"
  UAtan -> "atan"
  USinh -> "sinh"
  UCosh -> "cosh"
  UTanh -> "tanh"
  UAsinh -> "asinh"
  UAcosh -> "acosh"
  UAtanh -> "atanh"
  ULGamma -> "lgamma"
  UDigamma -> "digamma"
  UTrigamma -> "trigamma"
  UErf -> "erf"
  UErfc -> "erfc"
  UErfInv -> "erfinv"
  ULog1p -> "log1p"
  UExpm1 -> "expm1"
  URsqrt -> "rsqrt"
  UNot -> "not"

data ScalarBinOp a where
  BAdd :: (Num a) => ScalarBinOp a
  BSub :: (Num a) => ScalarBinOp a
  BMul :: (Num a) => ScalarBinOp a
  BDiv :: (Fractional a) => ScalarBinOp a
  BPow :: (Floating a) => ScalarBinOp a
  BQuot :: (Integral a) => ScalarBinOp a
  BRem :: (Integral a) => ScalarBinOp a
  BAnd :: ScalarBinOp Bool
  BOr :: ScalarBinOp Bool

binOpTag :: ScalarBinOp a -> BS.ByteString
binOpTag = \case
  BAdd -> "add"
  BSub -> "sub"
  BMul -> "mul"
  BDiv -> "div"
  BPow -> "pow"
  BQuot -> "quot"
  BRem -> "rem"
  BAnd -> "and"
  BOr -> "or"

data ScalarCmpOp = CEq | CNe | CLt | CLe | CGt | CGe
  deriving stock (Eq, Ord, Show)

cmpOpTag :: ScalarCmpOp -> BS.ByteString
cmpOpTag = \case
  CEq -> "eq"
  CNe -> "ne"
  CLt -> "lt"
  CLe -> "le"
  CGt -> "gt"
  CGe -> "ge"

data HExprArgs (ts :: [K.Type]) where
  HANil :: HExprArgs '[]
  HACons :: (TensorScalar a) => !(RawExpr a) -> !(HExprArgs ts) -> HExprArgs (a ': ts)

hExprArgsDigests :: HExprArgs ts -> [Digest]
hExprArgsDigests HANil = []
hExprArgsDigests (HACons e rest) = rawExprDigest e : hExprArgsDigests rest

-- | First-order, Merkle-hashed scalar expression IR.
data RawExpr (a :: K.Type) where
  ELit :: (TensorScalar a, Show a) => !Digest -> !a -> RawExpr a
  EScalarVar :: (TensorScalar a) => !Digest -> !Int -> !Text.Text -> RawExpr a
  EOrd :: !Digest -> !(IxExpr i) -> RawExpr Int64
  EShapeSize :: !Digest -> !(SIx i) -> RawExpr Int64
  EUnOp :: (TensorScalar a) => !Digest -> !(ScalarUnOp a) -> !(RawExpr a) -> RawExpr a
  EBinOp :: (TensorScalar a) => !Digest -> !(ScalarBinOp a) -> !(RawExpr a) -> !(RawExpr a) -> RawExpr a
  ECmp :: (TensorScalar a, Ord a) => !Digest -> !ScalarCmpOp -> !(RawExpr a) -> !(RawExpr a) -> RawExpr Bool
  ESelect :: (TensorScalar a) => !Digest -> !(RawExpr Bool) -> !(RawExpr a) -> !(RawExpr a) -> RawExpr a
  ECast :: (TensorScalar a, TensorScalar b) => !Digest -> !(RawExpr a) -> RawExpr b
  EIndex :: (TensorScalar a) => !Digest -> !(RawNode i a) -> !(IxExpr i) -> RawExpr a
  EReduce :: (TensorScalar a) => !Digest -> !(SomeMonoid a) -> !(SIx i) -> !Int -> !(RawExpr a) -> RawExpr a
  EScalarPrim :: (TensorScalar ret) => !Digest -> !(ScalarPrimSpec args ret) -> !(HExprArgs args) -> RawExpr ret
  EAnnotate :: (TensorScalar a) => !Digest -> !PrimAttrMap -> !(RawExpr a) -> RawExpr a
  EReadSTRef :: (TensorScalar a) => !Digest -> !Int -> RawExpr a
  EReadMTab :: (TensorScalar a) => !Digest -> !Int -> !(SIx i) -> !(IxExpr i) -> RawExpr a
  ERunSTScalar :: (TensorScalar a) => !Digest -> !(STProgram (RawExpr a)) -> RawExpr a

rawExprDigest :: RawExpr a -> Digest
rawExprDigest = \case
  ELit d _ -> d
  EScalarVar d _ _ -> d
  EOrd d _ -> d
  EShapeSize d _ -> d
  EUnOp d _ _ -> d
  EBinOp d _ _ _ -> d
  ECmp d _ _ _ -> d
  ESelect d _ _ _ -> d
  ECast d _ -> d
  EIndex d _ _ -> d
  EReduce d _ _ _ _ -> d
  EScalarPrim d _ _ -> d
  EAnnotate d _ _ -> d
  EReadSTRef d _ -> d
  EReadMTab d _ _ _ -> d
  ERunSTScalar d _ -> d

-- | Level-indexed pure functional surface scalar expression (O(1) HOAS binder
-- allocation without @IO@ or @StableName@).
newtype Expr (a :: K.Type) = Expr {unExpr :: Int -> RawExpr a}

buildExpr :: Expr a -> RawExpr a
buildExpr (Expr f) = f 0

liftRawExpr :: RawExpr a -> Expr a
liftRawExpr r = Expr (const r)

--------------------------------------------------------------------------------
-- 4. Table Nodes (`RawNode`, `TNode`), `STProgram`, `AccumProgram`, & `ContractSpec`
--------------------------------------------------------------------------------

data HNodeArgs (shapes :: [IxKind]) (a :: K.Type) where
  HNNil :: HNodeArgs '[] a
  HNCons :: !(RawNode sh a) -> !(HNodeArgs shs a) -> HNodeArgs (sh ': shs) a

hNodeArgsDigests :: HNodeArgs shs a -> [Digest]
hNodeArgsDigests HNNil = []
hNodeArgsDigests (HNCons n rest) = rawNodeDigest n : hNodeArgsDigests rest

-- | Single operand inside an unplanned N-ary Einstein contraction hypergraph.
data ContractOperand (a :: K.Type) where
  ContractOperand ::
    { coNode :: !(RawNode j a),
      coLabels :: ![Int]
    } ->
    ContractOperand a

-- | Unplanned N-ary Einstein contraction hypergraph specification.
data ContractSpec (i :: IxKind) (a :: K.Type) = ContractSpec
  { csOutShape :: !(SIx i),
    csOutLabels :: ![Int],
    csVertices :: !(Map.Map Int DimSpec),
    csOperands :: ![ContractOperand a]
  }

contractSpecDigest :: ContractSpec i a -> Digest
contractSpecDigest cs =
  let vertBytes =
        [ encInt vid <> case ds of
            DimStatic n -> "S" <> encInt n
            DimDyn t -> "D" <> encText t
          | (vid, ds) <- Map.toAscList (csVertices cs)
        ]
      opLabels = concatMap (\(ContractOperand _ ls) -> encInt (length ls) : map encInt ls) (csOperands cs)
      opDigests = [rawNodeDigest n | ContractOperand n _ <- csOperands cs]
   in mkIRDigest
        "ContractSpec"
        ([sixDigest (csOutShape cs), encInt (length (csOutLabels cs))] <> map encInt (csOutLabels cs) <> vertBytes <> opLabels)
        opDigests

-- | Structured statements inside a rank-2 locally stateful 'RunST' computation.
data STStmt where
  STNewRef :: (TensorScalar a) => !Int -> !(RawExpr a) -> STStmt
  STWriteRef :: (TensorScalar a) => !Int -> !(RawExpr a) -> STStmt
  STNewMTab :: (TensorScalar a) => !Int -> !(SIx i) -> !(RawNode i a) -> STStmt
  STWriteMTab :: (TensorScalar a) => !Int -> !(SIx i) -> !(IxExpr i) -> !(RawExpr a) -> STStmt
  STFor :: !(SIx i) -> !Int -> ![STStmt] -> STStmt
  STWhile :: !(RawExpr Bool) -> ![STStmt] -> STStmt
  STWhen :: !(RawExpr Bool) -> ![STStmt] -> STStmt

stStmtDigest :: STStmt -> Digest
stStmtDigest = \case
  STNewRef r e -> mkIRDigest "STNewRef" [encInt r] [rawExprDigest e]
  STWriteRef r e -> mkIRDigest "STWRef" [encInt r] [rawExprDigest e]
  STNewMTab t s n -> mkIRDigest "STNewMT" [encInt t, sixDigest s] [rawNodeDigest n]
  STWriteMTab t s ix e -> mkIRDigest "STWMT" [encInt t, sixDigest s] [ixExprDigest ix, rawExprDigest e]
  STFor s v body -> mkIRDigest "STFor" [sixDigest s, encInt v] (map stStmtDigest body)
  STWhile c body -> mkIRDigest "STWhile" [] (rawExprDigest c : map stStmtDigest body)
  STWhen c body -> mkIRDigest "STWhen" [] (rawExprDigest c : map stStmtDigest body)

data STProgram res = STProgram
  { stStmts :: ![STStmt],
    stResult :: !res
  }

-- | Write-only statements inside a Dex-style commutative-monoid 'RunAccum'.
data AccumStmt (i :: IxKind) (a :: K.Type) where
  AccumWrite :: !(IxExpr i) -> !(RawExpr a) -> AccumStmt i a
  AccumParFor :: !(SIx j) -> !Int -> ![AccumStmt i a] -> AccumStmt i a
  AccumWhen :: !(RawExpr Bool) -> ![AccumStmt i a] -> AccumStmt i a

accumStmtDigest :: AccumStmt i a -> Digest
accumStmtDigest = \case
  AccumWrite ix e -> mkIRDigest "AccW" [] [ixExprDigest ix, rawExprDigest e]
  AccumParFor s v body -> mkIRDigest "AccPF" [sixDigest s, encInt v] (map accumStmtDigest body)
  AccumWhen c body -> mkIRDigest "AccWh" [] (rawExprDigest c : map accumStmtDigest body)

newtype AccumProgram (i :: IxKind) (a :: K.Type) = AccumProgram
  { accumStmts :: [AccumStmt i a]
  }

-- | First-order, Merkle-hashed table IR node (all constructors differentially closed).
data RawNode (i :: IxKind) (a :: K.Type) where
  NInput :: (TensorScalar a) => !Digest -> !Text.Text -> !(SIx i) -> RawNode i a
  NFor :: (TensorScalar a) => !Digest -> !(SIx i) -> !Int -> !(RawExpr a) -> RawNode i a
  NReindex :: (TensorScalar a) => !Digest -> !(IxMorph i j) -> !(RawNode j a) -> RawNode i a
  NScatter ::
    (TensorScalar a) =>
    !Digest ->
    !(SomeCommMonoid a) ->
    !(SIx i) ->
    !(SIx j) ->
    !Int ->
    !(IxExpr i) ->
    !(RawExpr a) ->
    RawNode i a
  NRunAccum ::
    (TensorScalar a) =>
    !Digest ->
    !(SomeCommMonoid a) ->
    !(SIx i) ->
    !(AccumProgram i a) ->
    RawNode i a
  NRunST ::
    (TensorScalar a) =>
    !Digest ->
    !(SIx i) ->
    !(STProgram Int) -> -- ^ returns the frozen MTab ID of shape @i@
    RawNode i a
  NScan ::
    (TensorScalar a) =>
    !Digest ->
    !(SIx i) ->
    !(RawExpr a) -> -- ^ initial carry state
    !(RawNode i a) -> -- ^ input sequence table
    !Int -> -- ^ carry variable ID
    !Int -> -- ^ element variable ID
    !(RawExpr a) -> -- ^ step body returning next carry (= output at i)
    RawNode i a
  NAssocScan ::
    (TensorScalar a) =>
    !Digest ->
    !(SomeMonoid a) ->
    !(SIx i) ->
    !(RawNode i a) ->
    RawNode i a
  NContract ::
    (TensorScalar a) =>
    !Digest ->
    !(SomeSemiring a) ->
    !(ContractSpec i a) ->
    RawNode i a
  NKernelPrim ::
    (TensorScalar a) =>
    !Digest ->
    !(KernelPrimSpec inShapes i a) ->
    !(HNodeArgs inShapes a) ->
    RawNode i a
  NAnnotate ::
    (TensorScalar a) =>
    !Digest ->
    !PrimAttrMap ->
    !(RawNode i a) ->
    RawNode i a

rawNodeDigest :: RawNode i a -> Digest
rawNodeDigest = \case
  NInput d _ _ -> d
  NFor d _ _ _ -> d
  NReindex d _ _ -> d
  NScatter d _ _ _ _ _ _ -> d
  NRunAccum d _ _ _ -> d
  NRunST d _ _ -> d
  NScan d _ _ _ _ _ _ -> d
  NAssocScan d _ _ _ -> d
  NContract d _ _ -> d
  NKernelPrim d _ _ -> d
  NAnnotate d _ _ -> d

rawNodeShape :: RawNode i a -> SIx i
rawNodeShape = \case
  NInput _ _ s -> s
  NFor _ s _ _ -> s
  NReindex _ m _ -> morphSrc m
  NScatter _ _ s _ _ _ _ -> s
  NRunAccum _ _ s _ -> s
  NRunST _ s _ -> s
  NScan _ s _ _ _ _ _ -> s
  NAssocScan _ _ s _ -> s
  NContract _ _ cs -> csOutShape cs
  NKernelPrim _ spec _ -> kpOutShape spec
  NAnnotate _ _ n -> rawNodeShape n

newtype TNode (i :: IxKind) (a :: K.Type) = TNode {unTNode :: Int -> RawNode i a}

buildNode :: TNode i a -> RawNode i a
buildNode (TNode f) = f 0

liftRawNode :: RawNode i a -> TNode i a
liftRawNode n = TNode (const n)

--------------------------------------------------------------------------------
-- 5. Representable Surface Table (`Tab i a`)
--------------------------------------------------------------------------------

-- | Purely functional Dex-style table @i => a@.
--
-- Forms a genuine 'Functor', 'Applicative', 'Distributive', 'Representable',
-- and Kmett @linear@ 'Linear.Vector.Additive' functor! All elementwise maps,
-- zips, vector-space ops, and indexing fuse automatically via β-reduction.
data Tab (i :: IxKind) a = Tab
  { tabShape :: !(SIx i),
    tabIndex :: IxExpr i -> a
  }
  deriving stock (Functor)

-- | Rank-0 (scalar) table synonym, matching @'ShapeToIx' '[]@.
type ScalarTab a = Tab 'IxUnit a

-- | Rank-1 (vector) static table synonym, matching @'ShapeToIx' '[n]@.
type VectorTab (n :: Nat) a = Tab ('IxFin n) a

-- | Rank-2 (matrix) static table synonym, matching @'ShapeToIx' '[n, m]@.
type MatrixTab (n :: Nat) (m :: Nat) a = Tab ('IxProd ('IxFin n) ('IxFin m)) a

-- | Rank-3 static tensor table synonym (right-nested to match 'ShapeToIx').
type Tensor3Tab (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) a =
  Tab ('IxProd ('IxFin d1) ('IxProd ('IxFin d2) ('IxFin d3))) a

-- | Rank-4 static tensor table synonym (right-nested to match 'ShapeToIx').
type Tensor4Tab (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) (d4 :: Nat) a =
  Tab ('IxProd ('IxFin d1) ('IxProd ('IxFin d2) ('IxProd ('IxFin d3) ('IxFin d4)))) a

-- | Rank-5 static tensor table synonym (right-nested to match 'ShapeToIx').
type Tensor5Tab (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) (d4 :: Nat) (d5 :: Nat) a =
  Tab
    ( 'IxProd
        ('IxFin d1)
        ('IxProd ('IxFin d2) ('IxProd ('IxFin d3) ('IxProd ('IxFin d4) ('IxFin d5))))
    )
    a

-- | Rank-6 static tensor table synonym (right-nested to match 'ShapeToIx').
type Tensor6Tab (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) (d4 :: Nat) (d5 :: Nat) (d6 :: Nat) a =
  Tab
    ( 'IxProd
        ('IxFin d1)
        ( 'IxProd
            ('IxFin d2)
            ('IxProd ('IxFin d3) ('IxProd ('IxFin d4) ('IxProd ('IxFin d5) ('IxFin d6))))
        )
    )
    a

-- | Rank-7 static tensor table synonym (right-nested to match 'ShapeToIx').
type Tensor7Tab (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) (d4 :: Nat) (d5 :: Nat) (d6 :: Nat) (d7 :: Nat) a =
  Tab
    ( 'IxProd
        ('IxFin d1)
        ( 'IxProd
            ('IxFin d2)
            ( 'IxProd
                ('IxFin d3)
                ('IxProd ('IxFin d4) ('IxProd ('IxFin d5) ('IxProd ('IxFin d6) ('IxFin d7))))
            )
        )
    )
    a

-- | Rank-8 static tensor table synonym (right-nested to match 'ShapeToIx').
type Tensor8Tab (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) (d4 :: Nat) (d5 :: Nat) (d6 :: Nat) (d7 :: Nat) (d8 :: Nat) a =
  Tab
    ( 'IxProd
        ('IxFin d1)
        ( 'IxProd
            ('IxFin d2)
            ( 'IxProd
                ('IxFin d3)
                ( 'IxProd
                    ('IxFin d4)
                    ('IxProd ('IxFin d5) ('IxProd ('IxFin d6) ('IxProd ('IxFin d7) ('IxFin d8))))
                )
            )
        )
    )
    a

-- | Rank-9 static tensor table synonym (right-nested to match 'ShapeToIx').
type Tensor9Tab (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) (d4 :: Nat) (d5 :: Nat) (d6 :: Nat) (d7 :: Nat) (d8 :: Nat) (d9 :: Nat) a =
  Tab
    ( 'IxProd
        ('IxFin d1)
        ( 'IxProd
            ('IxFin d2)
            ( 'IxProd
                ('IxFin d3)
                ( 'IxProd
                    ('IxFin d4)
                    ( 'IxProd
                        ('IxFin d5)
                        ('IxProd ('IxFin d6) ('IxProd ('IxFin d7) ('IxProd ('IxFin d8) ('IxFin d9))))
                    )
                )
            )
        )
    )
    a

-- | Arbitrary-rank static tensor table synonym indexed by a type-level @[Nat]@
-- shape list (matching 'Janus.Data.Tensor.Tensor').
type TensorTab (ds :: [Nat]) a = Tab (ShapeToIx ds) a

instance (IxSet i) => Applicative (Tab i) where
  pure x = Tab (six @i) (const x)
  Tab s f <*> Tab _ g = Tab s (\ix -> f ix (g ix))

instance (IxSet i) => Distributive (Tab i) where
  distribute = distributeRep

instance (IxSet i) => Representable (Tab i) where
  type Rep (Tab i) = IxExpr i
  tabulate = Tab (six @i)
  index = tabIndex

instance (IxSet i) => Additive (Tab i) where
  zero = pure 0
  liftU2 = liftA2
  liftI2 = liftA2

-- | Static-size host index enumeration used only when @Linear.Metric.dot@ or
-- @Linear.Trace.trace@ is invoked polymorphically over host @Num a@ (for
-- embedded @Expr a@ on large/dynamic tensors, prefer 'Janus.Tensor.Linear.dotT'
-- and 'Janus.Tensor.Linear.traceT' which emit O(1) IR 'EReduce' nodes).
staticTabIndices :: forall i. (IxSet i) => [IxExpr i]
staticTabIndices =
  let sh = six @i :: SIx i
   in case sixStaticSize sh of
        Just n -> [IxConst k sh | k <- [0 .. n - 1]]
        Nothing -> error "Janus.Tensor.IR: Linear.Metric.dot / Linear.Trace.trace on dynamic IxDyn requires dotT / traceT"

instance (IxSet i) => Metric (Tab i) where
  dot u v = sum [tabIndex u ix * tabIndex v ix | ix <- staticTabIndices @i]

instance (IxSet i) => Trace (Tab i) where
  diagonal m = Tab (six @i) (\ix -> tabIndex (tabIndex m ix) ix)
  trace m = sum [tabIndex (tabIndex m ix) ix | ix <- staticTabIndices @i]

instance (TensorScalar a, Num a, Show a) => Num (Expr a) where
  fromInteger n = Expr (\_ -> mkLit (fromInteger n))
  Expr f + Expr g = Expr (\l -> mkBinOp BAdd (f l) (g l))
  Expr f - Expr g = Expr (\l -> mkBinOp BSub (f l) (g l))
  Expr f * Expr g = Expr (\l -> mkBinOp BMul (f l) (g l))
  negate (Expr f) = Expr (mkUnOp UNeg . f)
  abs (Expr f) = Expr (mkUnOp UAbs . f)
  signum (Expr f) = Expr (mkUnOp USignum . f)

instance (TensorScalar a, Fractional a, Show a) => Fractional (Expr a) where
  fromRational q = Expr (\_ -> mkLit (fromRational q))
  Expr f / Expr g = Expr (\l -> mkBinOp BDiv (f l) (g l))
  recip (Expr f) = Expr (mkUnOp URecip . f)

instance (TensorScalar a, Floating a, Show a) => Floating (Expr a) where
  pi = Expr (\_ -> mkLit pi)
  exp (Expr f) = Expr (mkUnOp UExp . f)
  log (Expr f) = Expr (mkUnOp ULog . f)
  sqrt (Expr f) = Expr (mkUnOp USqrt . f)
  Expr f ** Expr g = Expr (\l -> mkBinOp BPow (f l) (g l))
  sin (Expr f) = Expr (mkUnOp USin . f)
  cos (Expr f) = Expr (mkUnOp UCos . f)
  tan (Expr f) = Expr (mkUnOp UTan . f)
  asin (Expr f) = Expr (mkUnOp UAsin . f)
  acos (Expr f) = Expr (mkUnOp UAcos . f)
  atan (Expr f) = Expr (mkUnOp UAtan . f)
  sinh (Expr f) = Expr (mkUnOp USinh . f)
  cosh (Expr f) = Expr (mkUnOp UCosh . f)
  tanh (Expr f) = Expr (mkUnOp UTanh . f)
  asinh (Expr f) = Expr (mkUnOp UAsinh . f)
  acosh (Expr f) = Expr (mkUnOp UAcosh . f)
  atanh (Expr f) = Expr (mkUnOp UAtanh . f)

instance (TensorScalar a, Num a, Show a) => Conjugate (Expr a) where
  conjugate = id

fromNode :: (TensorScalar a) => TNode i a -> Tab i (Expr a)
fromNode (TNode mkN) =
  let sh = rawNodeShape (mkN 0)
   in Tab sh (\ix -> Expr (\lvl -> mkIndex (mkN lvl) ix))

-- | Materialize a 'Tab' into a shareable 'TNode' IR node (with automatic
-- η-contraction: @forIx (\\i -> n ! i) === n@).
materialize :: (TensorScalar a) => Tab i (Expr a) -> TNode i a
materialize (Tab sh f) = TNode $ \lvl ->
  let body = unExpr (f (IxVar lvl sh)) (lvl + 1)
   in case body of
        EIndex _ inner (IxVar v s')
          | v == lvl, Just Refl <- eqSIx sh s' ->
              inner
        _ -> mkFor sh lvl body

--------------------------------------------------------------------------------
-- 6. Smart Constructors (Merkle-Hashed & Locally Simplifying)
--------------------------------------------------------------------------------

mkLit :: forall a. (TensorScalar a, Show a) => a -> RawExpr a
mkLit x = ELit (mkIRDigest "Lit" [encText (scalarName (Proxy @a)), encText (Text.pack (show x))] []) x

mkScalarVar :: forall a. (TensorScalar a) => Int -> Text.Text -> RawExpr a
mkScalarVar vid nm = EScalarVar (mkIRDigest "SVar" [encInt vid, encText nm, encText (scalarName (Proxy @a))] []) vid nm

mkOrd :: IxExpr i -> RawExpr Int64
mkOrd ix = EOrd (mkIRDigest "Ord" [] [ixExprDigest ix]) ix

mkShapeSize :: SIx i -> RawExpr Int64
mkShapeSize sh = EShapeSize (mkIRDigest "ShSz" [sixDigest sh] []) sh

mkUnOp :: forall a. (TensorScalar a) => ScalarUnOp a -> RawExpr a -> RawExpr a
mkUnOp op x = EUnOp (mkIRDigest "UnOp" [unOpTag op, encText (scalarName (Proxy @a))] [rawExprDigest x]) op x

mkBinOp :: forall a. (TensorScalar a) => ScalarBinOp a -> RawExpr a -> RawExpr a -> RawExpr a
mkBinOp op x y = EBinOp (mkIRDigest "BinOp" [binOpTag op, encText (scalarName (Proxy @a))] [rawExprDigest x, rawExprDigest y]) op x y

mkCmp :: forall a. (TensorScalar a, Ord a) => ScalarCmpOp -> RawExpr a -> RawExpr a -> RawExpr Bool
mkCmp op x y = ECmp (mkIRDigest "Cmp" [cmpOpTag op, encText (scalarName (Proxy @a))] [rawExprDigest x, rawExprDigest y]) op x y

mkSelect :: forall a. (TensorScalar a) => RawExpr Bool -> RawExpr a -> RawExpr a -> RawExpr a
mkSelect c t f = ESelect (mkIRDigest "Sel" [encText (scalarName (Proxy @a))] [rawExprDigest c, rawExprDigest t, rawExprDigest f]) c t f

mkCast :: forall a b. (TensorScalar a, TensorScalar b) => RawExpr a -> RawExpr b
mkCast x = ECast (mkIRDigest "Cast" [encText (scalarName (Proxy @a)), encText (scalarName (Proxy @b))] [rawExprDigest x]) x

mkIndex :: forall i a. (TensorScalar a) => RawNode i a -> IxExpr i -> RawExpr a
mkIndex n ix = EIndex (mkIRDigest "Idx" [encText (scalarName (Proxy @a))] [rawNodeDigest n, ixExprDigest ix]) n ix

mkReduce :: forall i a. (TensorScalar a) => SomeMonoid a -> SIx i -> Int -> RawExpr a -> RawExpr a
mkReduce m s v body =
  EReduce (mkIRDigest "Red" [monoidDigest m, sixDigest s, encInt v] [rawExprDigest body]) m s v body

mkScalarPrim :: forall args ret. (TensorScalar ret) => ScalarPrimSpec args ret -> HExprArgs args -> RawExpr ret
mkScalarPrim spec args =
  EScalarPrim (mkIRDigest "SPrim" [encText (spName spec), encText (scalarName (Proxy @ret))] (hExprArgsDigests args)) spec args

mkAnnotateExpr :: (TensorScalar a) => PrimAttrMap -> RawExpr a -> RawExpr a
mkAnnotateExpr attrs e = EAnnotate (mkIRDigest "AnnE" [] [rawExprDigest e]) attrs e

mkRunSTScalar :: (TensorScalar a) => STProgram (RawExpr a) -> RawExpr a
mkRunSTScalar prog =
  ERunSTScalar (mkIRDigest "STSc" [] (map stStmtDigest (stStmts prog) <> [rawExprDigest (stResult prog)])) prog

mkInput :: forall i a. (TensorScalar a) => Text.Text -> SIx i -> RawNode i a
mkInput nm s = NInput (mkIRDigest "In" [encText nm, sixDigest s, encText (scalarName (Proxy @a))] []) nm s

mkFor :: forall i a. (TensorScalar a) => SIx i -> Int -> RawExpr a -> RawNode i a
mkFor s v body = NFor (mkIRDigest "For" [sixDigest s, encInt v, encText (scalarName (Proxy @a))] [rawExprDigest body]) s v body

mkReindex :: forall i j a. (TensorScalar a) => IxMorph i j -> RawNode j a -> RawNode i a
mkReindex m n = NReindex (mkIRDigest "Reix" [morphDigest m] [rawNodeDigest n]) m n

mkScatter :: forall i j a. (TensorScalar a) => SomeCommMonoid a -> SIx i -> SIx j -> Int -> IxExpr i -> RawExpr a -> RawNode i a
mkScatter cm si sj v ix val =
  NScatter (mkIRDigest "Scat" [commMonoidDigest cm, sixDigest si, sixDigest sj, encInt v] [ixExprDigest ix, rawExprDigest val]) cm si sj v ix val

mkRunAccum :: forall i a. (TensorScalar a) => SomeCommMonoid a -> SIx i -> AccumProgram i a -> RawNode i a
mkRunAccum cm s prog =
  NRunAccum (mkIRDigest "Acc" [commMonoidDigest cm, sixDigest s] (map accumStmtDigest (accumStmts prog))) cm s prog

mkRunST :: forall i a. (TensorScalar a) => SIx i -> STProgram Int -> RawNode i a
mkRunST s prog =
  NRunST (mkIRDigest "ST" [sixDigest s, encInt (stResult prog)] (map stStmtDigest (stStmts prog))) s prog

mkScan :: forall i a. (TensorScalar a) => SIx i -> RawExpr a -> RawNode i a -> Int -> Int -> RawExpr a -> RawNode i a
mkScan s initE xs cVar xVar stepE =
  NScan (mkIRDigest "Scan" [sixDigest s, encInt cVar, encInt xVar] [rawExprDigest initE, rawNodeDigest xs, rawExprDigest stepE]) s initE xs cVar xVar stepE

mkAssocScan :: forall i a. (TensorScalar a) => SomeMonoid a -> SIx i -> RawNode i a -> RawNode i a
mkAssocScan m s xs =
  NAssocScan (mkIRDigest "AScan" [monoidDigest m, sixDigest s] [rawNodeDigest xs]) m s xs

mkContract :: forall i a. (TensorScalar a) => SomeSemiring a -> ContractSpec i a -> RawNode i a
mkContract sr spec =
  NContract (mkIRDigest "Cont" [semiringDigest sr] [contractSpecDigest spec]) sr spec

mkKernelPrimNode :: forall inShapes i a. (TensorScalar a) => KernelPrimSpec inShapes i a -> HNodeArgs inShapes a -> RawNode i a
mkKernelPrimNode spec args =
  NKernelPrim (mkIRDigest "KPrim" [encText (kpName spec), sixDigest (kpOutShape spec)] (hNodeArgsDigests args)) spec args

mkAnnotateNode :: (TensorScalar a) => PrimAttrMap -> RawNode i a -> RawNode i a
mkAnnotateNode attrs n = NAnnotate (mkIRDigest "AnnN" [] [rawNodeDigest n]) attrs n
