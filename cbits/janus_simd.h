#ifndef JANUS_SIMD_H
#define JANUS_SIMD_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* x86-64 Target Feature Bitmask (must match Janus.Backend.C.Target) */
#define JANUS_X86_SSE2    (1ULL << 0)
#define JANUS_X86_SSE3    (1ULL << 1)
#define JANUS_X86_SSSE3   (1ULL << 2)
#define JANUS_X86_SSE4_1  (1ULL << 3)
#define JANUS_X86_SSE4_2  (1ULL << 4)
#define JANUS_X86_POPCNT  (1ULL << 5)
#define JANUS_X86_PCLMUL  (1ULL << 6)
#define JANUS_X86_AES     (1ULL << 7)
#define JANUS_X86_AVX     (1ULL << 8)
#define JANUS_X86_F16C    (1ULL << 9)
#define JANUS_X86_FMA     (1ULL << 10)
#define JANUS_X86_AVX2    (1ULL << 11)
#define JANUS_X86_BMI1    (1ULL << 12)
#define JANUS_X86_BMI2    (1ULL << 13)
#define JANUS_X86_LZCNT   (1ULL << 14)
#define JANUS_X86_MOVBE   (1ULL << 15)

/* Element type tags */
enum JanusSimdElem {
  JANUS_SIMD_I8  = 0,
  JANUS_SIMD_U8  = 1,
  JANUS_SIMD_I16 = 2,
  JANUS_SIMD_U16 = 3,
  JANUS_SIMD_I32 = 4,
  JANUS_SIMD_U32 = 5,
  JANUS_SIMD_I64 = 6,
  JANUS_SIMD_U64 = 7,
  JANUS_SIMD_F32 = 8,
  JANUS_SIMD_F64 = 9
};

/* Binary opcodes */
enum JanusSimdBinOp {
  JANUS_OP_ADD     = 0,
  JANUS_OP_SUB     = 1,
  JANUS_OP_MUL     = 2,
  JANUS_OP_DIV     = 3,
  JANUS_OP_MIN     = 4,
  JANUS_OP_MAX     = 5,
  JANUS_OP_ADDS    = 6,
  JANUS_OP_SUBS    = 7,
  JANUS_OP_AVG     = 8,
  JANUS_OP_ABSDIFF = 9,
  JANUS_OP_AND     = 10,
  JANUS_OP_OR      = 11,
  JANUS_OP_XOR     = 12,
  JANUS_OP_ANDNOT  = 13,
  JANUS_OP_SHL_VAR = 14,
  JANUS_OP_SHR_VAR = 15,
  JANUS_OP_ZIP_LO  = 16,
  JANUS_OP_ZIP_HI  = 17,
  JANUS_OP_POW     = 18,
  JANUS_OP_HYPOT   = 19
};

/* Unary opcodes */
enum JanusSimdUnOp {
  JANUS_UNOP_NEG    = 0,
  JANUS_UNOP_ABS    = 1,
  JANUS_UNOP_ABSS   = 2,
  JANUS_UNOP_NOT    = 3,
  JANUS_UNOP_POPCNT = 4,
  JANUS_UNOP_SQRT   = 5,
  JANUS_UNOP_RSQRT  = 6,
  JANUS_UNOP_RCP    = 7,
  JANUS_UNOP_FLOOR  = 8,
  JANUS_UNOP_CEIL   = 9,
  JANUS_UNOP_TRUNC  = 10,
  JANUS_UNOP_ROUND  = 11,
  JANUS_UNOP_SIGNUM = 12,
  JANUS_UNOP_EXP    = 13,
  JANUS_UNOP_LOG    = 14,
  JANUS_UNOP_SIN    = 15,
  JANUS_UNOP_COS    = 16,
  JANUS_UNOP_TAN    = 17,
  JANUS_UNOP_ASIN   = 18,
  JANUS_UNOP_ACOS   = 19,
  JANUS_UNOP_ATAN   = 20,
  JANUS_UNOP_SINH   = 21,
  JANUS_UNOP_COSH   = 22,
  JANUS_UNOP_TANH   = 23,
  JANUS_UNOP_ASINH  = 24,
  JANUS_UNOP_ACOSH  = 25,
  JANUS_UNOP_ATANH  = 26,
  JANUS_UNOP_EXPM1  = 27,
  JANUS_UNOP_LOG1P  = 28
};

/* Ternary opcodes */
enum JanusSimdTernOp {
  JANUS_TERNOP_SELECT = 0,
  JANUS_TERNOP_FMA    = 1,
  JANUS_TERNOP_FMS    = 2,
  JANUS_TERNOP_FNMA   = 3,
  JANUS_TERNOP_FNMS   = 4,
  JANUS_TERNOP_MULADD = 5
};

/* Comparison opcodes */
enum JanusSimdCmpOp {
  JANUS_CMP_EQ = 0,
  JANUS_CMP_NE = 1,
  JANUS_CMP_LT = 2,
  JANUS_CMP_LE = 3,
  JANUS_CMP_GT = 4,
  JANUS_CMP_GE = 5
};

/* Reduction opcodes */
enum JanusSimdRedOp {
  JANUS_RED_ADD = 0,
  JANUS_RED_MIN = 1,
  JANUS_RED_MAX = 2,
  JANUS_RED_AND = 3,
  JANUS_RED_OR  = 4,
  JANUS_RED_XOR = 5
};

uint64_t janus_query_x86_features(void);
uint64_t janus_get_x86_features(void);
void janus_set_x86_features(uint64_t features);
int janus_query_sleef_libdir(char *buf, int cap);

void janus_simd_splat(int elem, int lanes, void *dst, const void *scalar_ptr);
void janus_simd_iota(int elem, int lanes, void *dst);

void janus_simd_binop(int op, int elem, int lanes, void *dst, const void *a, const void *b);
void janus_simd_unop(int op, int elem, int lanes, void *dst, const void *a);
void janus_simd_ternop(int op, int elem, int lanes, void *dst, const void *a, const void *b, const void *c);
void janus_simd_cmp(int cmp_op, int elem, int lanes, void *dst, const void *a, const void *b);
void janus_simd_shift(int is_right, int elem, int lanes, void *dst, const void *a, int32_t count);

uint32_t janus_simd_movemask(int elem, int lanes, const void *mask);
int32_t janus_simd_mask_popcount(int elem, int lanes, const void *mask);
int32_t janus_simd_mask_first_true(int elem, int lanes, const void *mask);

void janus_simd_permute_lanes(int elem, int lanes, void *dst, const void *src, const void *indices_i32);
void janus_simd_shuffle_bytes(int bytes, void *dst, const void *src, const void *ctrl);

void janus_simd_reduce(int red_op, int elem, int lanes, void *scalar_dst, const void *src);

int32_t janus_simd_dp4a_s(const void *a4, const void *b4, int32_t acc);
uint32_t janus_simd_dp4a_u(const void *a4, const void *b4, uint32_t acc);
void janus_simd_dp4a_vec_s(int groups, void *dst_i32, const void *a, const void *b, const void *acc_i32);
void janus_simd_dp4a_vec_u(int groups, void *dst_u32, const void *a, const void *b, const void *acc_u32);
int32_t janus_simd_dp2a_s(int is_hi, const void *a2_i16, const void *b4_i8, int32_t acc);
uint32_t janus_simd_dp2a_u(int is_hi, const void *a2_u16, const void *b4_u8, uint32_t acc);
uint32_t janus_simd_sad_u8(int lanes, const void *a, const void *b);

void janus_simd_cvt(int src_elem, int dst_elem, int lanes, void *dst, const void *src);
void janus_simd_pack_sat(int src_elem, int dst_elem, int half_lanes, void *dst, const void *lo, const void *hi);

void janus_simd_load(int bytes, void *dst, const void *ptr);
void janus_simd_load_aligned(int bytes, void *dst, const void *ptr);
void janus_simd_store(int bytes, void *ptr, const void *src);
void janus_simd_store_aligned(int bytes, void *ptr, const void *src);
void janus_simd_load_masked(int elem, int lanes, void *dst, const void *mask, const void *fallback, const void *ptr);
void janus_simd_store_masked(int elem, int lanes, void *ptr, const void *mask, const void *src);
void janus_simd_gather(int elem, int lanes, void *dst, const void *base_ptr, const void *indices_i32);
void janus_simd_gather_masked(int elem, int lanes, void *dst, const void *mask, const void *fallback, const void *base_ptr, const void *indices_i32);

#ifdef __cplusplus
}
#endif

#endif /* JANUS_SIMD_H */
