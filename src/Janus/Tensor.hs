-- | @Janus.Tensor@: a purely functional, index-first, differentially closed
-- tensor algebra EDSL embedded in Haskell and compiled to Janus (Argobots
-- multi-threaded CPU with zero-allocation static scratchpads, direct 'IO'
-- oracle, CPU CUDA simulators 'CUDAIOM'/'CUDADejaFuM', and native 'JanusCUDA'
-- GPU kernels via 'Janus.CSE').
--
-- Inspired by Google Dex's algebraic finite index sets and Edward Kmett's
-- representable functors, optics, and @linear@ library:
--
-- * **Index-first tables (`Tab i a`)**: 'Functor', 'Applicative',
--   'Distributive', 'Representable', 'Additive', 'Metric', and 'Trace' over
--   both static ('IxFin') and dynamic branded ('IxDyn') index sets.
-- * **Open-world algebraic hierarchy**: 'TensorMonoid' (reductions &
--   associative scans), 'TensorCommMonoid' (Dex 'AccumM' & generalized
--   'scatterM' with generic lock-free @atomicCAS@ fallback), and
--   'TensorSemiring' (optimal N-ary Einstein contractions).
-- * **Rank-2 local state (`TensorST`)**: Haskell @ST@-style mutable 'TRef' /
--   'MTab' / 'forST_' / 'whileST' pure from the outside.
-- * **Optimal contraction planning & empirical selection**: exact connected-
--   subset DP, large-hypergraph heuristic + 2-opt, top-@K@ candidate
--   generation, automatic recognition, and "Janus.Bench" integration.
-- * **Open-world Janus backdoor**: 'ScalarPrimSpec' (fuses inside loops) and
--   'KernelPrimSpec' (whole-tensor CPU + CUDA kernels) with extensible
--   'PrimAttrMap' metadata hooks for downstream AD libraries.
module Janus.Tensor
  ( module Janus.Tensor.Index,
    module Janus.Tensor.Types,
    module Janus.Tensor.Algebra,
    module Janus.Tensor.Custom,
    module Janus.Tensor.IR,
    module Janus.Tensor.ST,
    module Janus.Tensor.DSL,
    module Janus.Tensor.Linear,
    module Janus.Tensor.Contract.Hypergraph,
    module Janus.Tensor.Contract.Plan,
    module Janus.Tensor.Contract.Recognize,
    module Janus.Tensor.Bench,
    module Janus.Tensor.Compile,
    module Janus.Tensor.Compile.Fuse,
    module Janus.Tensor.Compile.Schedule,
    module Janus.Tensor.Lower.CPU,
    module Janus.Tensor.Lower.CUDA,
  )
where

import Janus.Tensor.Algebra
import Janus.Tensor.Bench
import Janus.Tensor.Compile
import Janus.Tensor.Compile.Fuse
import Janus.Tensor.Compile.Schedule
import Janus.Tensor.Contract.Hypergraph
import Janus.Tensor.Contract.Plan
import Janus.Tensor.Contract.Recognize
import Janus.Tensor.Custom
import Janus.Tensor.DSL hiding (inputNode, inputTab)
import Janus.Tensor.IR hiding (mkScalarPrim)
import Janus.Tensor.Index
import Janus.Tensor.Linear
import Janus.Tensor.Lower.CPU
import Janus.Tensor.Lower.CUDA
import Janus.Tensor.ST
import Janus.Tensor.Types
