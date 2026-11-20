#define _GNU_SOURCE
#include "janus_simd.h"
#include <dlfcn.h>
#include <math.h>
#include <sleef.h>
#include <string.h>

#if defined(__x86_64__)
#include <cpuid.h>
#include <immintrin.h>

#define JANUS_DECL_SLEEF_UN256(op, ulp)                                         \
  extern SLEEF_CONST __m256 Sleef_##op##f8_##ulp##avx(__m256);                  \
  extern SLEEF_CONST __m256 Sleef_##op##f8_##ulp##avx2(__m256);                 \
  extern SLEEF_CONST __m256d Sleef_##op##d4_##ulp##avx(__m256d);                \
  extern SLEEF_CONST __m256d Sleef_##op##d4_##ulp##avx2(__m256d);

#define JANUS_DECL_SLEEF_BIN256(op, ulp)                                        \
  extern SLEEF_CONST __m256 Sleef_##op##f8_##ulp##avx(__m256, __m256);          \
  extern SLEEF_CONST __m256 Sleef_##op##f8_##ulp##avx2(__m256, __m256);         \
  extern SLEEF_CONST __m256d Sleef_##op##d4_##ulp##avx(__m256d, __m256d);       \
  extern SLEEF_CONST __m256d Sleef_##op##d4_##ulp##avx2(__m256d, __m256d);

JANUS_DECL_SLEEF_UN256(exp, u10)
JANUS_DECL_SLEEF_UN256(log, u10)
JANUS_DECL_SLEEF_UN256(sin, u10)
JANUS_DECL_SLEEF_UN256(cos, u10)
JANUS_DECL_SLEEF_UN256(tan, u10)
JANUS_DECL_SLEEF_UN256(asin, u10)
JANUS_DECL_SLEEF_UN256(acos, u10)
JANUS_DECL_SLEEF_UN256(atan, u10)
JANUS_DECL_SLEEF_UN256(sinh, u10)
JANUS_DECL_SLEEF_UN256(cosh, u10)
JANUS_DECL_SLEEF_UN256(tanh, u10)
JANUS_DECL_SLEEF_UN256(asinh, u10)
JANUS_DECL_SLEEF_UN256(acosh, u10)
JANUS_DECL_SLEEF_UN256(atanh, u10)
JANUS_DECL_SLEEF_UN256(expm1, u10)
JANUS_DECL_SLEEF_UN256(log1p, u10)
JANUS_DECL_SLEEF_BIN256(pow, u10)
JANUS_DECL_SLEEF_BIN256(hypot, u05)
#endif

static int elem_size(int elem) {
  switch (elem) {
    case JANUS_SIMD_I8:
    case JANUS_SIMD_U8:  return 1;
    case JANUS_SIMD_I16:
    case JANUS_SIMD_U16: return 2;
    case JANUS_SIMD_I32:
    case JANUS_SIMD_U32:
    case JANUS_SIMD_F32: return 4;
    case JANUS_SIMD_I64:
    case JANUS_SIMD_U64:
    case JANUS_SIMD_F64: return 8;
    default:             return 1;
  }
}

/* -------------------------------------------------------------------------- */
/* CPUID + XGETBV Target Feature Query & Thread-Local Dispatch Mask           */
/* -------------------------------------------------------------------------- */

uint64_t janus_query_x86_features(void) {
#if defined(__x86_64__)
  uint64_t feat = JANUS_X86_SSE2;
  unsigned int eax = 0, ebx = 0, ecx = 0, edx = 0;
  unsigned int max_leaf = __get_cpuid_max(0, NULL);
  if (max_leaf < 1) {
    return feat;
  }

  __cpuid(1, eax, ebx, ecx, edx);
  if (edx & (1u << 26)) feat |= JANUS_X86_SSE2;
  if (ecx & (1u << 0))  feat |= JANUS_X86_SSE3;
  if (ecx & (1u << 1))  feat |= JANUS_X86_PCLMUL;
  if (ecx & (1u << 9))  feat |= JANUS_X86_SSSE3;
  if (ecx & (1u << 19)) feat |= JANUS_X86_SSE4_1;
  if (ecx & (1u << 20)) feat |= JANUS_X86_SSE4_2;
  if (ecx & (1u << 22)) feat |= JANUS_X86_MOVBE;
  if (ecx & (1u << 23)) feat |= JANUS_X86_POPCNT;
  if (ecx & (1u << 25)) feat |= JANUS_X86_AES;

  int osxsave = (ecx & (1u << 27)) != 0;
  int os_ymm = 0;
  if (osxsave) {
    unsigned int xcr0_lo = 0, xcr0_hi = 0;
    __asm__ __volatile__("xgetbv" : "=a"(xcr0_lo), "=d"(xcr0_hi) : "c"(0));
    os_ymm = ((xcr0_lo & 0x6u) == 0x6u);
  }

  if (os_ymm && (ecx & (1u << 28))) {
    feat |= JANUS_X86_AVX;
    if (ecx & (1u << 12)) feat |= JANUS_X86_FMA;
    if (ecx & (1u << 29)) feat |= JANUS_X86_F16C;
  }

  if (max_leaf >= 7) {
    unsigned int eax7 = 0, ebx7 = 0, ecx7 = 0, edx7 = 0;
    __cpuid_count(7, 0, eax7, ebx7, ecx7, edx7);
    if (ebx7 & (1u << 3)) feat |= JANUS_X86_BMI1;
    if (ebx7 & (1u << 8)) feat |= JANUS_X86_BMI2;
    if (os_ymm && (feat & JANUS_X86_AVX) && (ebx7 & (1u << 5))) {
      feat |= JANUS_X86_AVX2;
    }
  }

  unsigned int max_ext = __get_cpuid_max(0x80000000u, NULL);
  if (max_ext >= 0x80000001u) {
    unsigned int eaxe = 0, ebxe = 0, ecxe = 0, edxe = 0;
    __cpuid(0x80000001u, eaxe, ebxe, ecxe, edxe);
    if (ecxe & (1u << 5)) feat |= JANUS_X86_LZCNT;
  }

  return feat;
#else
  return 0;
#endif
}

static _Thread_local uint64_t janus_tls_x86_features = 0;
static _Thread_local int janus_tls_initialized = 0;

uint64_t janus_get_x86_features(void) {
  if (!janus_tls_initialized) {
    janus_tls_x86_features = janus_query_x86_features();
    janus_tls_initialized = 1;
  }
  return janus_tls_x86_features;
}

void janus_set_x86_features(uint64_t features) {
  janus_tls_x86_features = features;
  janus_tls_initialized = 1;
}

int janus_query_sleef_libdir(char *buf, int cap) {
  if (!buf || cap <= 1) return 0;
  const void *sym = dlsym(RTLD_DEFAULT, "Sleef_exp_u10");
  if (!sym) {
    sym = (const void *)&Sleef_exp_u10;
  }
  Dl_info info;
  if (dladdr(sym, &info) && info.dli_fname) {
    const char *slash = strrchr(info.dli_fname, '/');
    if (slash) {
      int len = (int)(slash - info.dli_fname);
      if (len > 0 && len < cap) {
        memcpy(buf, info.dli_fname, (size_t)len);
        buf[len] = '\0';
        return len;
      }
    }
  }
  return 0;
}

/* -------------------------------------------------------------------------- */
/* Splat & Iota                                                               */
/* -------------------------------------------------------------------------- */

void janus_simd_splat(int elem, int lanes, void *dst, const void *scalar_ptr) {
  int sz = elem_size(elem);
#if defined(__x86_64__)
  uint64_t feat = janus_get_x86_features();
  int bytes = lanes * sz;
  if (bytes == 32 && (feat & JANUS_X86_AVX2)) {
    goto splat_avx2;
  }
  if (bytes == 16) {
    switch (elem) {
      case JANUS_SIMD_I8:
      case JANUS_SIMD_U8: {
        int8_t v; memcpy(&v, scalar_ptr, 1);
        _mm_storeu_si128((__m128i *)dst, _mm_set1_epi8(v));
        return;
      }
      case JANUS_SIMD_I16:
      case JANUS_SIMD_U16: {
        int16_t v; memcpy(&v, scalar_ptr, 2);
        _mm_storeu_si128((__m128i *)dst, _mm_set1_epi16(v));
        return;
      }
      case JANUS_SIMD_I32:
      case JANUS_SIMD_U32: {
        int32_t v; memcpy(&v, scalar_ptr, 4);
        _mm_storeu_si128((__m128i *)dst, _mm_set1_epi32(v));
        return;
      }
      case JANUS_SIMD_I64:
      case JANUS_SIMD_U64: {
        int64_t v; memcpy(&v, scalar_ptr, 8);
        _mm_storeu_si128((__m128i *)dst, _mm_set1_epi64x(v));
        return;
      }
      case JANUS_SIMD_F32: {
        float v; memcpy(&v, scalar_ptr, 4);
        _mm_storeu_ps((float *)dst, _mm_set1_ps(v));
        return;
      }
      case JANUS_SIMD_F64: {
        double v; memcpy(&v, scalar_ptr, 8);
        _mm_storeu_pd((double *)dst, _mm_set1_pd(v));
        return;
      }
      default: break;
    }
  }
splat_avx2:
  if (bytes == 32 && (feat & JANUS_X86_AVX)) {
    if (elem == JANUS_SIMD_F32 || elem == JANUS_SIMD_F64 || (feat & JANUS_X86_AVX2)) {
      /* Handled via target-annotated helper below */
    }
  }
#endif
  uint8_t *out = (uint8_t *)dst;
  for (int i = 0; i < lanes; ++i) {
    memcpy(out + i * sz, scalar_ptr, (size_t)sz);
  }
}

void janus_simd_iota(int elem, int lanes, void *dst) {
  int sz = elem_size(elem);
  uint8_t *out = (uint8_t *)dst;
  for (int i = 0; i < lanes; ++i) {
    switch (elem) {
      case JANUS_SIMD_I8:  { int8_t   v = (int8_t)i;   memcpy(out + i * sz, &v, 1); break; }
      case JANUS_SIMD_U8:  { uint8_t  v = (uint8_t)i;  memcpy(out + i * sz, &v, 1); break; }
      case JANUS_SIMD_I16: { int16_t  v = (int16_t)i;  memcpy(out + i * sz, &v, 2); break; }
      case JANUS_SIMD_U16: { uint16_t v = (uint16_t)i; memcpy(out + i * sz, &v, 2); break; }
      case JANUS_SIMD_I32: { int32_t  v = (int32_t)i;  memcpy(out + i * sz, &v, 4); break; }
      case JANUS_SIMD_U32: { uint32_t v = (uint32_t)i; memcpy(out + i * sz, &v, 4); break; }
      case JANUS_SIMD_I64: { int64_t  v = (int64_t)i;  memcpy(out + i * sz, &v, 8); break; }
      case JANUS_SIMD_U64: { uint64_t v = (uint64_t)i; memcpy(out + i * sz, &v, 8); break; }
      case JANUS_SIMD_F32: { float    v = (float)i;    memcpy(out + i * sz, &v, 4); break; }
      case JANUS_SIMD_F64: { double   v = (double)i;   memcpy(out + i * sz, &v, 8); break; }
      default: break;
    }
  }
}

/* -------------------------------------------------------------------------- */
/* x86-64 <immintrin.h> Multi-Tier Kernels                                    */
/* -------------------------------------------------------------------------- */

#if defined(__x86_64__)

static inline __m128i load128i_partial(const void *p, int bytes) {
  if (bytes >= 16) return _mm_loadu_si128((const __m128i *)p);
  uint8_t buf[16] = {0};
  memcpy(buf, p, (size_t)bytes);
  return _mm_loadu_si128((const __m128i *)buf);
}

static inline void store128i_partial(void *p, __m128i v, int bytes) {
  if (bytes >= 16) {
    _mm_storeu_si128((__m128i *)p, v);
  } else {
    uint8_t buf[16];
    _mm_storeu_si128((__m128i *)buf, v);
    memcpy(p, buf, (size_t)bytes);
  }
}

static inline __m128 load128f_partial(const void *p, int bytes, float pad) {
  if (bytes >= 16) return _mm_loadu_ps((const float *)p);
  float buf[4] = {pad, pad, pad, pad};
  memcpy(buf, p, (size_t)bytes);
  return _mm_loadu_ps(buf);
}

static inline void store128f_partial(void *p, __m128 v, int bytes) {
  if (bytes >= 16) {
    _mm_storeu_ps((float *)p, v);
  } else {
    float buf[4];
    _mm_storeu_ps(buf, v);
    memcpy(p, buf, (size_t)bytes);
  }
}

static inline __m128d load128d_partial(const void *p, int bytes, double pad) {
  if (bytes >= 16) return _mm_loadu_pd((const double *)p);
  double buf[2] = {pad, pad};
  memcpy(buf, p, (size_t)bytes);
  return _mm_loadu_pd(buf);
}

static inline void store128d_partial(void *p, __m128d v, int bytes) {
  if (bytes >= 16) {
    _mm_storeu_pd((double *)p, v);
  } else {
    double buf[2];
    _mm_storeu_pd(buf, v);
    memcpy(p, buf, (size_t)bytes);
  }
}

#define JANUS_DEFINE_SLEEF_BINOP_128(ISA, TARGET_ATTR)                          \
  __attribute__((target(TARGET_ATTR)))                                          \
  static int sleef_binop_128_##ISA(int op, int elem, int bytes, void *dst,      \
                                   const void *a, const void *b) {              \
    if (elem == JANUS_SIMD_F32) {                                               \
      __m128 va = load128f_partial(a, bytes, 1.0f);                             \
      __m128 vb = load128f_partial(b, bytes, 1.0f);                             \
      __m128 vr;                                                                \
      switch (op) {                                                             \
        case JANUS_OP_POW:   vr = Sleef_powf4_u10##ISA(va, vb); break;          \
        case JANUS_OP_HYPOT: vr = Sleef_hypotf4_u05##ISA(va, vb); break;        \
        default: return 0;                                                      \
      }                                                                         \
      store128f_partial(dst, vr, bytes);                                        \
      return 1;                                                                 \
    }                                                                           \
    if (elem == JANUS_SIMD_F64) {                                               \
      __m128d va = load128d_partial(a, bytes, 1.0);                             \
      __m128d vb = load128d_partial(b, bytes, 1.0);                             \
      __m128d vr;                                                               \
      switch (op) {                                                             \
        case JANUS_OP_POW:   vr = Sleef_powd2_u10##ISA(va, vb); break;          \
        case JANUS_OP_HYPOT: vr = Sleef_hypotd2_u05##ISA(va, vb); break;        \
        default: return 0;                                                      \
      }                                                                         \
      store128d_partial(dst, vr, bytes);                                        \
      return 1;                                                                 \
    }                                                                           \
    return 0;                                                                   \
  }

#define JANUS_DEFINE_SLEEF_BINOP_256(ISA, TARGET_ATTR)                          \
  __attribute__((target(TARGET_ATTR)))                                          \
  static int sleef_binop_256_##ISA(int op, int elem, void *dst,                 \
                                   const void *a, const void *b) {              \
    if (elem == JANUS_SIMD_F32) {                                               \
      __m256 va = _mm256_loadu_ps((const float *)a);                            \
      __m256 vb = _mm256_loadu_ps((const float *)b);                            \
      __m256 vr;                                                                \
      switch (op) {                                                             \
        case JANUS_OP_POW:   vr = Sleef_powf8_u10##ISA(va, vb); break;          \
        case JANUS_OP_HYPOT: vr = Sleef_hypotf8_u05##ISA(va, vb); break;        \
        default: return 0;                                                      \
      }                                                                         \
      _mm256_storeu_ps((float *)dst, vr);                                       \
      return 1;                                                                 \
    }                                                                           \
    if (elem == JANUS_SIMD_F64) {                                               \
      __m256d va = _mm256_loadu_pd((const double *)a);                          \
      __m256d vb = _mm256_loadu_pd((const double *)b);                          \
      __m256d vr;                                                               \
      switch (op) {                                                             \
        case JANUS_OP_POW:   vr = Sleef_powd4_u10##ISA(va, vb); break;          \
        case JANUS_OP_HYPOT: vr = Sleef_hypotd4_u05##ISA(va, vb); break;        \
        default: return 0;                                                      \
      }                                                                         \
      _mm256_storeu_pd((double *)dst, vr);                                      \
      return 1;                                                                 \
    }                                                                           \
    return 0;                                                                   \
  }

JANUS_DEFINE_SLEEF_BINOP_128(sse2, "sse2")
JANUS_DEFINE_SLEEF_BINOP_128(sse4, "sse4.1")
JANUS_DEFINE_SLEEF_BINOP_128(avx2128, "avx2,fma")
JANUS_DEFINE_SLEEF_BINOP_256(avx, "avx")
JANUS_DEFINE_SLEEF_BINOP_256(avx2, "avx2,fma")

#define JANUS_DEFINE_SLEEF_UNOP_128(ISA, TARGET_ATTR)                           \
  __attribute__((target(TARGET_ATTR)))                                          \
  static int sleef_unop_128_##ISA(int op, int elem, int bytes, void *dst,       \
                                  const void *a) {                              \
    if (elem == JANUS_SIMD_F32) {                                               \
      __m128 va = load128f_partial(a, bytes, 0.5f);                             \
      __m128 vr;                                                                \
      switch (op) {                                                             \
        case JANUS_UNOP_EXP:   vr = Sleef_expf4_u10##ISA(va); break;            \
        case JANUS_UNOP_LOG:   vr = Sleef_logf4_u10##ISA(va); break;            \
        case JANUS_UNOP_SIN:   vr = Sleef_sinf4_u10##ISA(va); break;            \
        case JANUS_UNOP_COS:   vr = Sleef_cosf4_u10##ISA(va); break;            \
        case JANUS_UNOP_TAN:   vr = Sleef_tanf4_u10##ISA(va); break;            \
        case JANUS_UNOP_ASIN:  vr = Sleef_asinf4_u10##ISA(va); break;           \
        case JANUS_UNOP_ACOS:  vr = Sleef_acosf4_u10##ISA(va); break;           \
        case JANUS_UNOP_ATAN:  vr = Sleef_atanf4_u10##ISA(va); break;           \
        case JANUS_UNOP_SINH:  vr = Sleef_sinhf4_u10##ISA(va); break;           \
        case JANUS_UNOP_COSH:  vr = Sleef_coshf4_u10##ISA(va); break;           \
        case JANUS_UNOP_TANH:  vr = Sleef_tanhf4_u10##ISA(va); break;           \
        case JANUS_UNOP_ASINH: vr = Sleef_asinhf4_u10##ISA(va); break;          \
        case JANUS_UNOP_ACOSH: vr = Sleef_acoshf4_u10##ISA(va); break;          \
        case JANUS_UNOP_ATANH: vr = Sleef_atanhf4_u10##ISA(va); break;          \
        case JANUS_UNOP_EXPM1: vr = Sleef_expm1f4_u10##ISA(va); break;          \
        case JANUS_UNOP_LOG1P: vr = Sleef_log1pf4_u10##ISA(va); break;          \
        default: return 0;                                                      \
      }                                                                         \
      store128f_partial(dst, vr, bytes);                                        \
      return 1;                                                                 \
    }                                                                           \
    if (elem == JANUS_SIMD_F64) {                                               \
      __m128d va = load128d_partial(a, bytes, 0.5);                             \
      __m128d vr;                                                               \
      switch (op) {                                                             \
        case JANUS_UNOP_EXP:   vr = Sleef_expd2_u10##ISA(va); break;            \
        case JANUS_UNOP_LOG:   vr = Sleef_logd2_u10##ISA(va); break;            \
        case JANUS_UNOP_SIN:   vr = Sleef_sind2_u10##ISA(va); break;            \
        case JANUS_UNOP_COS:   vr = Sleef_cosd2_u10##ISA(va); break;            \
        case JANUS_UNOP_TAN:   vr = Sleef_tand2_u10##ISA(va); break;            \
        case JANUS_UNOP_ASIN:  vr = Sleef_asind2_u10##ISA(va); break;           \
        case JANUS_UNOP_ACOS:  vr = Sleef_acosd2_u10##ISA(va); break;           \
        case JANUS_UNOP_ATAN:  vr = Sleef_atand2_u10##ISA(va); break;           \
        case JANUS_UNOP_SINH:  vr = Sleef_sinhd2_u10##ISA(va); break;           \
        case JANUS_UNOP_COSH:  vr = Sleef_coshd2_u10##ISA(va); break;           \
        case JANUS_UNOP_TANH:  vr = Sleef_tanhd2_u10##ISA(va); break;           \
        case JANUS_UNOP_ASINH: vr = Sleef_asinhd2_u10##ISA(va); break;          \
        case JANUS_UNOP_ACOSH: vr = Sleef_acoshd2_u10##ISA(va); break;          \
        case JANUS_UNOP_ATANH: vr = Sleef_atanhd2_u10##ISA(va); break;          \
        case JANUS_UNOP_EXPM1: vr = Sleef_expm1d2_u10##ISA(va); break;          \
        case JANUS_UNOP_LOG1P: vr = Sleef_log1pd2_u10##ISA(va); break;          \
        default: return 0;                                                      \
      }                                                                         \
      store128d_partial(dst, vr, bytes);                                        \
      return 1;                                                                 \
    }                                                                           \
    return 0;                                                                   \
  }

#define JANUS_DEFINE_SLEEF_UNOP_256(ISA, TARGET_ATTR)                           \
  __attribute__((target(TARGET_ATTR)))                                          \
  static int sleef_unop_256_##ISA(int op, int elem, void *dst, const void *a) { \
    if (elem == JANUS_SIMD_F32) {                                               \
      __m256 va = _mm256_loadu_ps((const float *)a);                            \
      __m256 vr;                                                                \
      switch (op) {                                                             \
        case JANUS_UNOP_EXP:   vr = Sleef_expf8_u10##ISA(va); break;            \
        case JANUS_UNOP_LOG:   vr = Sleef_logf8_u10##ISA(va); break;            \
        case JANUS_UNOP_SIN:   vr = Sleef_sinf8_u10##ISA(va); break;            \
        case JANUS_UNOP_COS:   vr = Sleef_cosf8_u10##ISA(va); break;            \
        case JANUS_UNOP_TAN:   vr = Sleef_tanf8_u10##ISA(va); break;            \
        case JANUS_UNOP_ASIN:  vr = Sleef_asinf8_u10##ISA(va); break;           \
        case JANUS_UNOP_ACOS:  vr = Sleef_acosf8_u10##ISA(va); break;           \
        case JANUS_UNOP_ATAN:  vr = Sleef_atanf8_u10##ISA(va); break;           \
        case JANUS_UNOP_SINH:  vr = Sleef_sinhf8_u10##ISA(va); break;           \
        case JANUS_UNOP_COSH:  vr = Sleef_coshf8_u10##ISA(va); break;           \
        case JANUS_UNOP_TANH:  vr = Sleef_tanhf8_u10##ISA(va); break;           \
        case JANUS_UNOP_ASINH: vr = Sleef_asinhf8_u10##ISA(va); break;          \
        case JANUS_UNOP_ACOSH: vr = Sleef_acoshf8_u10##ISA(va); break;          \
        case JANUS_UNOP_ATANH: vr = Sleef_atanhf8_u10##ISA(va); break;          \
        case JANUS_UNOP_EXPM1: vr = Sleef_expm1f8_u10##ISA(va); break;          \
        case JANUS_UNOP_LOG1P: vr = Sleef_log1pf8_u10##ISA(va); break;          \
        default: return 0;                                                      \
      }                                                                         \
      _mm256_storeu_ps((float *)dst, vr);                                       \
      return 1;                                                                 \
    }                                                                           \
    if (elem == JANUS_SIMD_F64) {                                               \
      __m256d va = _mm256_loadu_pd((const double *)a);                          \
      __m256d vr;                                                               \
      switch (op) {                                                             \
        case JANUS_UNOP_EXP:   vr = Sleef_expd4_u10##ISA(va); break;            \
        case JANUS_UNOP_LOG:   vr = Sleef_logd4_u10##ISA(va); break;            \
        case JANUS_UNOP_SIN:   vr = Sleef_sind4_u10##ISA(va); break;            \
        case JANUS_UNOP_COS:   vr = Sleef_cosd4_u10##ISA(va); break;            \
        case JANUS_UNOP_TAN:   vr = Sleef_tand4_u10##ISA(va); break;            \
        case JANUS_UNOP_ASIN:  vr = Sleef_asind4_u10##ISA(va); break;           \
        case JANUS_UNOP_ACOS:  vr = Sleef_acosd4_u10##ISA(va); break;           \
        case JANUS_UNOP_ATAN:  vr = Sleef_atand4_u10##ISA(va); break;           \
        case JANUS_UNOP_SINH:  vr = Sleef_sinhd4_u10##ISA(va); break;           \
        case JANUS_UNOP_COSH:  vr = Sleef_coshd4_u10##ISA(va); break;           \
        case JANUS_UNOP_TANH:  vr = Sleef_tanhd4_u10##ISA(va); break;           \
        case JANUS_UNOP_ASINH: vr = Sleef_asinhd4_u10##ISA(va); break;          \
        case JANUS_UNOP_ACOSH: vr = Sleef_acoshd4_u10##ISA(va); break;          \
        case JANUS_UNOP_ATANH: vr = Sleef_atanhd4_u10##ISA(va); break;          \
        case JANUS_UNOP_EXPM1: vr = Sleef_expm1d4_u10##ISA(va); break;          \
        case JANUS_UNOP_LOG1P: vr = Sleef_log1pd4_u10##ISA(va); break;          \
        default: return 0;                                                      \
      }                                                                         \
      _mm256_storeu_pd((double *)dst, vr);                                      \
      return 1;                                                                 \
    }                                                                           \
    return 0;                                                                   \
  }

JANUS_DEFINE_SLEEF_UNOP_128(sse2, "sse2")
JANUS_DEFINE_SLEEF_UNOP_128(sse4, "sse4.1")
JANUS_DEFINE_SLEEF_UNOP_128(avx2128, "avx2,fma")
JANUS_DEFINE_SLEEF_UNOP_256(avx, "avx")
JANUS_DEFINE_SLEEF_UNOP_256(avx2, "avx2,fma")

/* SSE2 128-bit integer & float binary operations */
__attribute__((target("sse2")))
static int binop_128_sse2(int op, int elem, int bytes, void *dst, const void *a, const void *b) {
  if (elem == JANUS_SIMD_F32) {
    __m128 va = load128f_partial(a, bytes, 0.0f);
    __m128 vb = load128f_partial(b, bytes, op == JANUS_OP_DIV ? 1.0f : 0.0f);
    __m128 vr;
    switch (op) {
      case JANUS_OP_ADD:    vr = _mm_add_ps(va, vb); break;
      case JANUS_OP_SUB:    vr = _mm_sub_ps(va, vb); break;
      case JANUS_OP_MUL:    vr = _mm_mul_ps(va, vb); break;
      case JANUS_OP_DIV:    vr = _mm_div_ps(va, vb); break;
      case JANUS_OP_MIN:    vr = _mm_min_ps(va, vb); break;
      case JANUS_OP_MAX:    vr = _mm_max_ps(va, vb); break;
      case JANUS_OP_AND:    vr = _mm_and_ps(va, vb); break;
      case JANUS_OP_OR:     vr = _mm_or_ps(va, vb); break;
      case JANUS_OP_XOR:    vr = _mm_xor_ps(va, vb); break;
      case JANUS_OP_ANDNOT: vr = _mm_andnot_ps(va, vb); break;
      case JANUS_OP_ZIP_LO: vr = _mm_unpacklo_ps(va, vb); break;
      case JANUS_OP_ZIP_HI:
        if (bytes < 16) return 0;
        vr = _mm_unpackhi_ps(va, vb);
        break;
      case JANUS_OP_ABSDIFF: {
        __m128 d = _mm_sub_ps(va, vb);
        __m128 mask = _mm_castsi128_ps(_mm_set1_epi32(0x7FFFFFFF));
        vr = _mm_and_ps(d, mask);
        break;
      }
      default: return 0;
    }
    store128f_partial(dst, vr, bytes);
    return 1;
  }
  if (elem == JANUS_SIMD_F64 && bytes == 16) {
    __m128d va = _mm_loadu_pd((const double *)a);
    __m128d vb = _mm_loadu_pd((const double *)b);
    __m128d vr;
    switch (op) {
      case JANUS_OP_ADD:    vr = _mm_add_pd(va, vb); break;
      case JANUS_OP_SUB:    vr = _mm_sub_pd(va, vb); break;
      case JANUS_OP_MUL:    vr = _mm_mul_pd(va, vb); break;
      case JANUS_OP_DIV:    vr = _mm_div_pd(va, vb); break;
      case JANUS_OP_MIN:    vr = _mm_min_pd(va, vb); break;
      case JANUS_OP_MAX:    vr = _mm_max_pd(va, vb); break;
      case JANUS_OP_AND:    vr = _mm_and_pd(va, vb); break;
      case JANUS_OP_OR:     vr = _mm_or_pd(va, vb); break;
      case JANUS_OP_XOR:    vr = _mm_xor_pd(va, vb); break;
      case JANUS_OP_ANDNOT: vr = _mm_andnot_pd(va, vb); break;
      case JANUS_OP_ZIP_LO: vr = _mm_unpacklo_pd(va, vb); break;
      case JANUS_OP_ZIP_HI: vr = _mm_unpackhi_pd(va, vb); break;
      case JANUS_OP_ABSDIFF: {
        __m128d d = _mm_sub_pd(va, vb);
        __m128d mask = _mm_castsi128_pd(_mm_set1_epi64x(0x7FFFFFFFFFFFFFFFLL));
        vr = _mm_and_pd(d, mask);
        break;
      }
      default: return 0;
    }
    _mm_storeu_pd((double *)dst, vr);
    return 1;
  }

  __m128i va = load128i_partial(a, bytes);
  __m128i vb = load128i_partial(b, bytes);
  __m128i vr;

  switch (op) {
    case JANUS_OP_AND:    vr = _mm_and_si128(va, vb); store128i_partial(dst, vr, bytes); return 1;
    case JANUS_OP_OR:     vr = _mm_or_si128(va, vb); store128i_partial(dst, vr, bytes); return 1;
    case JANUS_OP_XOR:    vr = _mm_xor_si128(va, vb); store128i_partial(dst, vr, bytes); return 1;
    case JANUS_OP_ANDNOT: vr = _mm_andnot_si128(va, vb); store128i_partial(dst, vr, bytes); return 1;
    case JANUS_OP_ADD:
      switch (elem) {
        case JANUS_SIMD_I8:  case JANUS_SIMD_U8:  vr = _mm_add_epi8(va, vb); break;
        case JANUS_SIMD_I16: case JANUS_SIMD_U16: vr = _mm_add_epi16(va, vb); break;
        case JANUS_SIMD_I32: case JANUS_SIMD_U32: vr = _mm_add_epi32(va, vb); break;
        case JANUS_SIMD_I64: case JANUS_SIMD_U64: vr = _mm_add_epi64(va, vb); break;
        default: return 0;
      }
      store128i_partial(dst, vr, bytes);
      return 1;
    case JANUS_OP_SUB:
      switch (elem) {
        case JANUS_SIMD_I8:  case JANUS_SIMD_U8:  vr = _mm_sub_epi8(va, vb); break;
        case JANUS_SIMD_I16: case JANUS_SIMD_U16: vr = _mm_sub_epi16(va, vb); break;
        case JANUS_SIMD_I32: case JANUS_SIMD_U32: vr = _mm_sub_epi32(va, vb); break;
        case JANUS_SIMD_I64: case JANUS_SIMD_U64: vr = _mm_sub_epi64(va, vb); break;
        default: return 0;
      }
      store128i_partial(dst, vr, bytes);
      return 1;
    case JANUS_OP_MUL:
      if (elem == JANUS_SIMD_I16 || elem == JANUS_SIMD_U16) {
        vr = _mm_mullo_epi16(va, vb);
        store128i_partial(dst, vr, bytes);
        return 1;
      }
      if (elem == JANUS_SIMD_I8 || elem == JANUS_SIMD_U8) {
        __m128i even = _mm_mullo_epi16(va, vb);
        __m128i odd  = _mm_mullo_epi16(_mm_srli_epi16(va, 8), _mm_srli_epi16(vb, 8));
        vr = _mm_or_si128(_mm_and_si128(even, _mm_set1_epi16(0x00FF)), _mm_slli_epi16(odd, 8));
        store128i_partial(dst, vr, bytes);
        return 1;
      }
      if (elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_U32) {
        __m128i mul02 = _mm_mul_epu32(va, vb);
        __m128i mul13 = _mm_mul_epu32(_mm_srli_si128(va, 4), _mm_srli_si128(vb, 4));
        vr = _mm_unpacklo_epi32(_mm_shuffle_epi32(mul02, _MM_SHUFFLE(0, 0, 2, 0)),
                                _mm_shuffle_epi32(mul13, _MM_SHUFFLE(0, 0, 2, 0)));
        store128i_partial(dst, vr, bytes);
        return 1;
      }
      return 0;
    case JANUS_OP_ADDS:
      switch (elem) {
        case JANUS_SIMD_I8:  vr = _mm_adds_epi8(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_U8:  vr = _mm_adds_epu8(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_I16: vr = _mm_adds_epi16(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_U16: vr = _mm_adds_epu16(va, vb); store128i_partial(dst, vr, bytes); return 1;
        default: return 0;
      }
    case JANUS_OP_SUBS:
      switch (elem) {
        case JANUS_SIMD_I8:  vr = _mm_subs_epi8(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_U8:  vr = _mm_subs_epu8(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_I16: vr = _mm_subs_epi16(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_U16: vr = _mm_subs_epu16(va, vb); store128i_partial(dst, vr, bytes); return 1;
        default: return 0;
      }
    case JANUS_OP_AVG:
      switch (elem) {
        case JANUS_SIMD_U8:  vr = _mm_avg_epu8(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_U16: vr = _mm_avg_epu16(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_I8: {
          __m128i bias = _mm_set1_epi8((char)0x80);
          vr = _mm_xor_si128(_mm_avg_epu8(_mm_xor_si128(va, bias), _mm_xor_si128(vb, bias)), bias);
          store128i_partial(dst, vr, bytes);
          return 1;
        }
        case JANUS_SIMD_I16: {
          __m128i bias = _mm_set1_epi16((short)0x8000);
          vr = _mm_xor_si128(_mm_avg_epu16(_mm_xor_si128(va, bias), _mm_xor_si128(vb, bias)), bias);
          store128i_partial(dst, vr, bytes);
          return 1;
        }
        default: return 0;
      }
    case JANUS_OP_ABSDIFF:
      if (elem == JANUS_SIMD_U8) {
        vr = _mm_or_si128(_mm_subs_epu8(va, vb), _mm_subs_epu8(vb, va));
        store128i_partial(dst, vr, bytes);
        return 1;
      }
      if (elem == JANUS_SIMD_U16) {
        vr = _mm_or_si128(_mm_subs_epu16(va, vb), _mm_subs_epu16(vb, va));
        store128i_partial(dst, vr, bytes);
        return 1;
      }
      return 0;
    case JANUS_OP_MIN:
      if (elem == JANUS_SIMD_U8)  { vr = _mm_min_epu8(va, vb); store128i_partial(dst, vr, bytes); return 1; }
      if (elem == JANUS_SIMD_I16) { vr = _mm_min_epi16(va, vb); store128i_partial(dst, vr, bytes); return 1; }
      return 0;
    case JANUS_OP_MAX:
      if (elem == JANUS_SIMD_U8)  { vr = _mm_max_epu8(va, vb); store128i_partial(dst, vr, bytes); return 1; }
      if (elem == JANUS_SIMD_I16) { vr = _mm_max_epi16(va, vb); store128i_partial(dst, vr, bytes); return 1; }
      return 0;
    case JANUS_OP_ZIP_LO:
      switch (elem) {
        case JANUS_SIMD_I8:  case JANUS_SIMD_U8:  vr = _mm_unpacklo_epi8(va, vb); break;
        case JANUS_SIMD_I16: case JANUS_SIMD_U16: vr = _mm_unpacklo_epi16(va, vb); break;
        case JANUS_SIMD_I32: case JANUS_SIMD_U32: vr = _mm_unpacklo_epi32(va, vb); break;
        case JANUS_SIMD_I64: case JANUS_SIMD_U64: vr = _mm_unpacklo_epi64(va, vb); break;
        default: return 0;
      }
      store128i_partial(dst, vr, bytes);
      return 1;
    case JANUS_OP_ZIP_HI:
      if (bytes < 16) return 0;
      switch (elem) {
        case JANUS_SIMD_I8:  case JANUS_SIMD_U8:  vr = _mm_unpackhi_epi8(va, vb); break;
        case JANUS_SIMD_I16: case JANUS_SIMD_U16: vr = _mm_unpackhi_epi16(va, vb); break;
        case JANUS_SIMD_I32: case JANUS_SIMD_U32: vr = _mm_unpackhi_epi32(va, vb); break;
        case JANUS_SIMD_I64: case JANUS_SIMD_U64: vr = _mm_unpackhi_epi64(va, vb); break;
        default: return 0;
      }
      store128i_partial(dst, vr, bytes);
      return 1;
    default: return 0;
  }
}

/* SSE4.1 128-bit extensions */
__attribute__((target("sse4.1")))
static int binop_128_sse41(int op, int elem, int bytes, void *dst, const void *a, const void *b) {
  __m128i va = load128i_partial(a, bytes);
  __m128i vb = load128i_partial(b, bytes);
  __m128i vr;
  switch (op) {
    case JANUS_OP_MUL:
      if (elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_U32) {
        vr = _mm_mullo_epi32(va, vb);
        store128i_partial(dst, vr, bytes);
        return 1;
      }
      break;
    case JANUS_OP_MIN:
      switch (elem) {
        case JANUS_SIMD_I8:  vr = _mm_min_epi8(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_U16: vr = _mm_min_epu16(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_I32: vr = _mm_min_epi32(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_U32: vr = _mm_min_epu32(va, vb); store128i_partial(dst, vr, bytes); return 1;
        default: break;
      }
      break;
    case JANUS_OP_MAX:
      switch (elem) {
        case JANUS_SIMD_I8:  vr = _mm_max_epi8(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_U16: vr = _mm_max_epu16(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_I32: vr = _mm_max_epi32(va, vb); store128i_partial(dst, vr, bytes); return 1;
        case JANUS_SIMD_U32: vr = _mm_max_epu32(va, vb); store128i_partial(dst, vr, bytes); return 1;
        default: break;
      }
      break;
    default: break;
  }
  return binop_128_sse2(op, elem, bytes, dst, a, b);
}

/* AVX 256-bit float/double binary operations */
__attribute__((target("avx")))
static int binop_256_avx(int op, int elem, void *dst, const void *a, const void *b) {
  if (elem == JANUS_SIMD_F32) {
    __m256 va = _mm256_loadu_ps((const float *)a);
    __m256 vb = _mm256_loadu_ps((const float *)b);
    __m256 vr;
    switch (op) {
      case JANUS_OP_ADD:    vr = _mm256_add_ps(va, vb); break;
      case JANUS_OP_SUB:    vr = _mm256_sub_ps(va, vb); break;
      case JANUS_OP_MUL:    vr = _mm256_mul_ps(va, vb); break;
      case JANUS_OP_DIV:    vr = _mm256_div_ps(va, vb); break;
      case JANUS_OP_MIN:    vr = _mm256_min_ps(va, vb); break;
      case JANUS_OP_MAX:    vr = _mm256_max_ps(va, vb); break;
      case JANUS_OP_AND:    vr = _mm256_and_ps(va, vb); break;
      case JANUS_OP_OR:     vr = _mm256_or_ps(va, vb); break;
      case JANUS_OP_XOR:    vr = _mm256_xor_ps(va, vb); break;
      case JANUS_OP_ANDNOT: vr = _mm256_andnot_ps(va, vb); break;
      case JANUS_OP_ABSDIFF: {
        __m256 d = _mm256_sub_ps(va, vb);
        __m256 mask = _mm256_castsi256_ps(_mm256_set1_epi32(0x7FFFFFFF));
        vr = _mm256_and_ps(d, mask);
        break;
      }
      default: return 0;
    }
    _mm256_storeu_ps((float *)dst, vr);
    return 1;
  }
  if (elem == JANUS_SIMD_F64) {
    __m256d va = _mm256_loadu_pd((const double *)a);
    __m256d vb = _mm256_loadu_pd((const double *)b);
    __m256d vr;
    switch (op) {
      case JANUS_OP_ADD:    vr = _mm256_add_pd(va, vb); break;
      case JANUS_OP_SUB:    vr = _mm256_sub_pd(va, vb); break;
      case JANUS_OP_MUL:    vr = _mm256_mul_pd(va, vb); break;
      case JANUS_OP_DIV:    vr = _mm256_div_pd(va, vb); break;
      case JANUS_OP_MIN:    vr = _mm256_min_pd(va, vb); break;
      case JANUS_OP_MAX:    vr = _mm256_max_pd(va, vb); break;
      case JANUS_OP_AND:    vr = _mm256_and_pd(va, vb); break;
      case JANUS_OP_OR:     vr = _mm256_or_pd(va, vb); break;
      case JANUS_OP_XOR:    vr = _mm256_xor_pd(va, vb); break;
      case JANUS_OP_ANDNOT: vr = _mm256_andnot_pd(va, vb); break;
      case JANUS_OP_ABSDIFF: {
        __m256d d = _mm256_sub_pd(va, vb);
        __m256d mask = _mm256_castsi256_pd(_mm256_set1_epi64x(0x7FFFFFFFFFFFFFFFLL));
        vr = _mm256_and_pd(d, mask);
        break;
      }
      default: return 0;
    }
    _mm256_storeu_pd((double *)dst, vr);
    return 1;
  }
  return 0;
}

/* AVX2 256-bit integer & variable shift operations */
__attribute__((target("avx2,fma,bmi,bmi2")))
static int binop_256_avx2(int op, int elem, void *dst, const void *a, const void *b) {
  if (elem == JANUS_SIMD_F32 || elem == JANUS_SIMD_F64) {
    return binop_256_avx(op, elem, dst, a, b);
  }
  __m256i va = _mm256_loadu_si256((const __m256i *)a);
  __m256i vb = _mm256_loadu_si256((const __m256i *)b);
  __m256i vr;
  switch (op) {
    case JANUS_OP_AND:    vr = _mm256_and_si256(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
    case JANUS_OP_OR:     vr = _mm256_or_si256(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
    case JANUS_OP_XOR:    vr = _mm256_xor_si256(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
    case JANUS_OP_ANDNOT: vr = _mm256_andnot_si256(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
    case JANUS_OP_ADD:
      switch (elem) {
        case JANUS_SIMD_I8:  case JANUS_SIMD_U8:  vr = _mm256_add_epi8(va, vb); break;
        case JANUS_SIMD_I16: case JANUS_SIMD_U16: vr = _mm256_add_epi16(va, vb); break;
        case JANUS_SIMD_I32: case JANUS_SIMD_U32: vr = _mm256_add_epi32(va, vb); break;
        case JANUS_SIMD_I64: case JANUS_SIMD_U64: vr = _mm256_add_epi64(va, vb); break;
        default: return 0;
      }
      _mm256_storeu_si256((__m256i *)dst, vr);
      return 1;
    case JANUS_OP_SUB:
      switch (elem) {
        case JANUS_SIMD_I8:  case JANUS_SIMD_U8:  vr = _mm256_sub_epi8(va, vb); break;
        case JANUS_SIMD_I16: case JANUS_SIMD_U16: vr = _mm256_sub_epi16(va, vb); break;
        case JANUS_SIMD_I32: case JANUS_SIMD_U32: vr = _mm256_sub_epi32(va, vb); break;
        case JANUS_SIMD_I64: case JANUS_SIMD_U64: vr = _mm256_sub_epi64(va, vb); break;
        default: return 0;
      }
      _mm256_storeu_si256((__m256i *)dst, vr);
      return 1;
    case JANUS_OP_MUL:
      if (elem == JANUS_SIMD_I16 || elem == JANUS_SIMD_U16) {
        _mm256_storeu_si256((__m256i *)dst, _mm256_mullo_epi16(va, vb));
        return 1;
      }
      if (elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_U32) {
        _mm256_storeu_si256((__m256i *)dst, _mm256_mullo_epi32(va, vb));
        return 1;
      }
      if (elem == JANUS_SIMD_I8 || elem == JANUS_SIMD_U8) {
        __m256i even = _mm256_mullo_epi16(va, vb);
        __m256i odd  = _mm256_mullo_epi16(_mm256_srli_epi16(va, 8), _mm256_srli_epi16(vb, 8));
        vr = _mm256_or_si256(_mm256_and_si256(even, _mm256_set1_epi16(0x00FF)), _mm256_slli_epi16(odd, 8));
        _mm256_storeu_si256((__m256i *)dst, vr);
        return 1;
      }
      return 0;
    case JANUS_OP_ADDS:
      switch (elem) {
        case JANUS_SIMD_I8:  vr = _mm256_adds_epi8(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_U8:  vr = _mm256_adds_epu8(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_I16: vr = _mm256_adds_epi16(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_U16: vr = _mm256_adds_epu16(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        default: return 0;
      }
    case JANUS_OP_SUBS:
      switch (elem) {
        case JANUS_SIMD_I8:  vr = _mm256_subs_epi8(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_U8:  vr = _mm256_subs_epu8(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_I16: vr = _mm256_subs_epi16(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_U16: vr = _mm256_subs_epu16(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        default: return 0;
      }
    case JANUS_OP_AVG:
      switch (elem) {
        case JANUS_SIMD_U8:  vr = _mm256_avg_epu8(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_U16: vr = _mm256_avg_epu16(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_I8: {
          __m256i bias = _mm256_set1_epi8((char)0x80);
          vr = _mm256_xor_si256(_mm256_avg_epu8(_mm256_xor_si256(va, bias), _mm256_xor_si256(vb, bias)), bias);
          _mm256_storeu_si256((__m256i *)dst, vr);
          return 1;
        }
        case JANUS_SIMD_I16: {
          __m256i bias = _mm256_set1_epi16((short)0x8000);
          vr = _mm256_xor_si256(_mm256_avg_epu16(_mm256_xor_si256(va, bias), _mm256_xor_si256(vb, bias)), bias);
          _mm256_storeu_si256((__m256i *)dst, vr);
          return 1;
        }
        default: return 0;
      }
    case JANUS_OP_ABSDIFF:
      if (elem == JANUS_SIMD_U8) {
        vr = _mm256_or_si256(_mm256_subs_epu8(va, vb), _mm256_subs_epu8(vb, va));
        _mm256_storeu_si256((__m256i *)dst, vr);
        return 1;
      }
      if (elem == JANUS_SIMD_U16) {
        vr = _mm256_or_si256(_mm256_subs_epu16(va, vb), _mm256_subs_epu16(vb, va));
        _mm256_storeu_si256((__m256i *)dst, vr);
        return 1;
      }
      return 0;
    case JANUS_OP_MIN:
      switch (elem) {
        case JANUS_SIMD_I8:  vr = _mm256_min_epi8(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_U8:  vr = _mm256_min_epu8(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_I16: vr = _mm256_min_epi16(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_U16: vr = _mm256_min_epu16(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_I32: vr = _mm256_min_epi32(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_U32: vr = _mm256_min_epu32(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        default: return 0;
      }
    case JANUS_OP_MAX:
      switch (elem) {
        case JANUS_SIMD_I8:  vr = _mm256_max_epi8(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_U8:  vr = _mm256_max_epu8(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_I16: vr = _mm256_max_epi16(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_U16: vr = _mm256_max_epu16(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_I32: vr = _mm256_max_epi32(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        case JANUS_SIMD_U32: vr = _mm256_max_epu32(va, vb); _mm256_storeu_si256((__m256i *)dst, vr); return 1;
        default: return 0;
      }
    case JANUS_OP_SHL_VAR:
      if (elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_U32) {
        _mm256_storeu_si256((__m256i *)dst, _mm256_sllv_epi32(va, vb));
        return 1;
      }
      if (elem == JANUS_SIMD_I64 || elem == JANUS_SIMD_U64) {
        _mm256_storeu_si256((__m256i *)dst, _mm256_sllv_epi64(va, vb));
        return 1;
      }
      return 0;
    case JANUS_OP_SHR_VAR:
      if (elem == JANUS_SIMD_U32) {
        _mm256_storeu_si256((__m256i *)dst, _mm256_srlv_epi32(va, vb));
        return 1;
      }
      if (elem == JANUS_SIMD_I32) {
        _mm256_storeu_si256((__m256i *)dst, _mm256_srav_epi32(va, vb));
        return 1;
      }
      if (elem == JANUS_SIMD_U64) {
        _mm256_storeu_si256((__m256i *)dst, _mm256_srlv_epi64(va, vb));
        return 1;
      }
      return 0;
    default: return 0;
  }
}

__attribute__((target("avx2")))
static int binop_128_avx2_varshift(int op, int elem, int bytes, void *dst, const void *a, const void *b) {
  __m128i va = load128i_partial(a, bytes);
  __m128i vb = load128i_partial(b, bytes);
  if (op == JANUS_OP_SHL_VAR) {
    if (elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_U32) {
      store128i_partial(dst, _mm_sllv_epi32(va, vb), bytes);
      return 1;
    }
    if (elem == JANUS_SIMD_I64 || elem == JANUS_SIMD_U64) {
      store128i_partial(dst, _mm_sllv_epi64(va, vb), bytes);
      return 1;
    }
  } else if (op == JANUS_OP_SHR_VAR) {
    if (elem == JANUS_SIMD_U32) {
      store128i_partial(dst, _mm_srlv_epi32(va, vb), bytes);
      return 1;
    }
    if (elem == JANUS_SIMD_I32) {
      store128i_partial(dst, _mm_srav_epi32(va, vb), bytes);
      return 1;
    }
    if (elem == JANUS_SIMD_U64) {
      store128i_partial(dst, _mm_srlv_epi64(va, vb), bytes);
      return 1;
    }
  }
  return 0;
}

#endif /* __x86_64__ */

/* Scalar / Reference Fallback for unhandled 64-bit / sub-lane combinations */
static void binop_fallback(int op, int elem, int lanes, void *dst, const void *a, const void *b) {
  int sz = elem_size(elem);
  const uint8_t *pa = (const uint8_t *)a;
  const uint8_t *pb = (const uint8_t *)b;
  uint8_t *pd = (uint8_t *)dst;

  if (op == JANUS_OP_ZIP_LO) {
    int half = lanes / 2;
    for (int i = 0; i < half; ++i) {
      memcpy(pd + (2 * i) * sz, pa + i * sz, (size_t)sz);
      memcpy(pd + (2 * i + 1) * sz, pb + i * sz, (size_t)sz);
    }
    return;
  }
  if (op == JANUS_OP_ZIP_HI) {
    int half = lanes / 2;
    for (int i = 0; i < half; ++i) {
      memcpy(pd + (2 * i) * sz, pa + (half + i) * sz, (size_t)sz);
      memcpy(pd + (2 * i + 1) * sz, pb + (half + i) * sz, (size_t)sz);
    }
    return;
  }

  for (int i = 0; i < lanes; ++i) {
    const void *ea = pa + i * sz;
    const void *eb = pb + i * sz;
    void *ed = pd + i * sz;

    if (op == JANUS_OP_AND || op == JANUS_OP_OR || op == JANUS_OP_XOR || op == JANUS_OP_ANDNOT) {
      for (int k = 0; k < sz; ++k) {
        uint8_t ba = ((const uint8_t *)ea)[k];
        uint8_t bb = ((const uint8_t *)eb)[k];
        uint8_t br = 0;
        if (op == JANUS_OP_AND)    br = (uint8_t)(ba & bb);
        if (op == JANUS_OP_OR)     br = (uint8_t)(ba | bb);
        if (op == JANUS_OP_XOR)    br = (uint8_t)(ba ^ bb);
        if (op == JANUS_OP_ANDNOT) br = (uint8_t)((~ba) & bb);
        ((uint8_t *)ed)[k] = br;
      }
      continue;
    }

    if (elem == JANUS_SIMD_F32) {
      float va, vb, vr = 0.0f;
      memcpy(&va, ea, 4); memcpy(&vb, eb, 4);
      switch (op) {
        case JANUS_OP_ADD:     vr = va + vb; break;
        case JANUS_OP_SUB:     vr = va - vb; break;
        case JANUS_OP_MUL:     vr = va * vb; break;
        case JANUS_OP_DIV:     vr = va / vb; break;
        case JANUS_OP_MIN:     vr = (va < vb) ? va : vb; break;
        case JANUS_OP_MAX:     vr = (va > vb) ? va : vb; break;
        case JANUS_OP_ABSDIFF: vr = fabsf(va - vb); break;
        case JANUS_OP_POW:     vr = Sleef_powf_u10(va, vb); break;
        case JANUS_OP_HYPOT:   vr = Sleef_hypotf_u05(va, vb); break;
        default: break;
      }
      memcpy(ed, &vr, 4);
      continue;
    }
    if (elem == JANUS_SIMD_F64) {
      double va, vb, vr = 0.0;
      memcpy(&va, ea, 8); memcpy(&vb, eb, 8);
      switch (op) {
        case JANUS_OP_ADD:     vr = va + vb; break;
        case JANUS_OP_SUB:     vr = va - vb; break;
        case JANUS_OP_MUL:     vr = va * vb; break;
        case JANUS_OP_DIV:     vr = va / vb; break;
        case JANUS_OP_MIN:     vr = (va < vb) ? va : vb; break;
        case JANUS_OP_MAX:     vr = (va > vb) ? va : vb; break;
        case JANUS_OP_ABSDIFF: vr = fabs(va - vb); break;
        case JANUS_OP_POW:     vr = Sleef_pow_u10(va, vb); break;
        case JANUS_OP_HYPOT:   vr = Sleef_hypot_u05(va, vb); break;
        default: break;
      }
      memcpy(ed, &vr, 8);
      continue;
    }

    int is_signed = (elem == JANUS_SIMD_I8 || elem == JANUS_SIMD_I16 ||
                     elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_I64);
    int bits = sz * 8;
    uint64_t ua = 0, ub = 0;
    int64_t sa = 0, sb = 0;
    switch (elem) {
      case JANUS_SIMD_I8:  { int8_t  t1, t2; memcpy(&t1, ea, 1); memcpy(&t2, eb, 1); sa = t1; sb = t2; ua = (uint8_t)t1; ub = (uint8_t)t2; break; }
      case JANUS_SIMD_U8:  { uint8_t t1, t2; memcpy(&t1, ea, 1); memcpy(&t2, eb, 1); ua = t1; ub = t2; break; }
      case JANUS_SIMD_I16: { int16_t t1, t2; memcpy(&t1, ea, 2); memcpy(&t2, eb, 2); sa = t1; sb = t2; ua = (uint16_t)t1; ub = (uint16_t)t2; break; }
      case JANUS_SIMD_U16: { uint16_t t1, t2; memcpy(&t1, ea, 2); memcpy(&t2, eb, 2); ua = t1; ub = t2; break; }
      case JANUS_SIMD_I32: { int32_t t1, t2; memcpy(&t1, ea, 4); memcpy(&t2, eb, 4); sa = t1; sb = t2; ua = (uint32_t)t1; ub = (uint32_t)t2; break; }
      case JANUS_SIMD_U32: { uint32_t t1, t2; memcpy(&t1, ea, 4); memcpy(&t2, eb, 4); ua = t1; ub = t2; break; }
      case JANUS_SIMD_I64: { int64_t t1, t2; memcpy(&t1, ea, 8); memcpy(&t2, eb, 8); sa = t1; sb = t2; ua = (uint64_t)t1; ub = (uint64_t)t2; break; }
      case JANUS_SIMD_U64: { uint64_t t1, t2; memcpy(&t1, ea, 8); memcpy(&t2, eb, 8); ua = t1; ub = t2; break; }
      default: break;
    }

    uint64_t ur = 0;
    int64_t smin = (bits == 64) ? INT64_MIN : -(1LL << (bits - 1));
    int64_t smax = (bits == 64) ? INT64_MAX : ((1LL << (bits - 1)) - 1);
    uint64_t umax = (bits == 64) ? UINT64_MAX : ((1ULL << bits) - 1ULL);

    switch (op) {
      case JANUS_OP_ADD: ur = ua + ub; break;
      case JANUS_OP_SUB: ur = ua - ub; break;
      case JANUS_OP_MUL: ur = ua * ub; break;
      case JANUS_OP_DIV:
        if (is_signed) ur = (sb == 0) ? 0 : (uint64_t)(sa / sb);
        else           ur = (ub == 0) ? 0 : (ua / ub);
        break;
      case JANUS_OP_MIN:
        ur = is_signed ? (uint64_t)(sa < sb ? sa : sb) : (ua < ub ? ua : ub);
        break;
      case JANUS_OP_MAX:
        ur = is_signed ? (uint64_t)(sa > sb ? sa : sb) : (ua > ub ? ua : ub);
        break;
      case JANUS_OP_ADDS:
        if (is_signed) {
          __int128 sum = (__int128)sa + (__int128)sb;
          if (sum > (__int128)smax) sum = smax;
          if (sum < (__int128)smin) sum = smin;
          ur = (uint64_t)(int64_t)sum;
        } else {
          unsigned __int128 sum = (unsigned __int128)ua + (unsigned __int128)ub;
          if (sum > (unsigned __int128)umax) sum = umax;
          ur = (uint64_t)sum;
        }
        break;
      case JANUS_OP_SUBS:
        if (is_signed) {
          __int128 diff = (__int128)sa - (__int128)sb;
          if (diff > (__int128)smax) diff = smax;
          if (diff < (__int128)smin) diff = smin;
          ur = (uint64_t)(int64_t)diff;
        } else {
          ur = (ua >= ub) ? (ua - ub) : 0ULL;
        }
        break;
      case JANUS_OP_AVG:
        if (is_signed) {
          __int128 avg = ((__int128)sa + (__int128)sb + 1) >> 1;
          ur = (uint64_t)(int64_t)avg;
        } else {
          unsigned __int128 avg = ((unsigned __int128)ua + (unsigned __int128)ub + 1u) >> 1;
          ur = (uint64_t)avg;
        }
        break;
      case JANUS_OP_ABSDIFF:
        if (is_signed) {
          unsigned __int128 d = (sa >= sb) ? (unsigned __int128)((__int128)sa - (__int128)sb)
                                           : (unsigned __int128)((__int128)sb - (__int128)sa);
          ur = (uint64_t)d;
        } else {
          ur = (ua >= ub) ? (ua - ub) : (ub - ua);
        }
        break;
      case JANUS_OP_SHL_VAR: {
        uint64_t cnt = ub;
        ur = (cnt >= (uint64_t)bits) ? 0ULL : (ua << cnt);
        break;
      }
      case JANUS_OP_SHR_VAR: {
        uint64_t cnt = ub;
        if (is_signed) {
          ur = (cnt >= (uint64_t)bits) ? (sa < 0 ? UINT64_MAX : 0ULL) : (uint64_t)(sa >> cnt);
        } else {
          ur = (cnt >= (uint64_t)bits) ? 0ULL : ((ua & umax) >> cnt);
        }
        break;
      }
      default: break;
    }
    memcpy(ed, &ur, (size_t)sz);
  }
}

void janus_simd_binop(int op, int elem, int lanes, void *dst, const void *a, const void *b) {
#if defined(__x86_64__)
  uint64_t feat = janus_get_x86_features();
  int bytes = lanes * elem_size(elem);
  if ((op == JANUS_OP_POW || op == JANUS_OP_HYPOT) &&
      (elem == JANUS_SIMD_F32 || elem == JANUS_SIMD_F64)) {
    int has_avx2_fma = ((feat & (JANUS_X86_AVX2 | JANUS_X86_FMA)) == (JANUS_X86_AVX2 | JANUS_X86_FMA));
    if (bytes == 32) {
      if (has_avx2_fma && sleef_binop_256_avx2(op, elem, dst, a, b)) return;
      if ((feat & JANUS_X86_AVX) && sleef_binop_256_avx(op, elem, dst, a, b)) return;
      const uint8_t *pa = (const uint8_t *)a;
      const uint8_t *pb = (const uint8_t *)b;
      uint8_t tmp[32];
      if (feat & JANUS_X86_SSE4_1) {
        sleef_binop_128_sse4(op, elem, 16, tmp, pa, pb);
        sleef_binop_128_sse4(op, elem, 16, tmp + 16, pa + 16, pb + 16);
      } else {
        sleef_binop_128_sse2(op, elem, 16, tmp, pa, pb);
        sleef_binop_128_sse2(op, elem, 16, tmp + 16, pa + 16, pb + 16);
      }
      memcpy(dst, tmp, 32);
      return;
    } else if (bytes <= 16) {
      if (has_avx2_fma && sleef_binop_128_avx2128(op, elem, bytes, dst, a, b)) return;
      if ((feat & JANUS_X86_SSE4_1) && sleef_binop_128_sse4(op, elem, bytes, dst, a, b)) return;
      if (sleef_binop_128_sse2(op, elem, bytes, dst, a, b)) return;
    }
  }
  if (bytes == 32 && op != JANUS_OP_ZIP_LO && op != JANUS_OP_ZIP_HI) {
    if ((feat & JANUS_X86_AVX2) && binop_256_avx2(op, elem, dst, a, b)) return;
    if ((feat & JANUS_X86_AVX) && binop_256_avx(op, elem, dst, a, b)) return;
    /* Lower 256-bit to two 128-bit operations */
    const uint8_t *pa = (const uint8_t *)a;
    const uint8_t *pb = (const uint8_t *)b;
    uint8_t *pd = (uint8_t *)dst;
    uint8_t tmp[32];
    int ok_lo = (feat & JANUS_X86_SSE4_1) ? binop_128_sse41(op, elem, 16, tmp, pa, pb)
                                          : binop_128_sse2(op, elem, 16, tmp, pa, pb);
    if (ok_lo) {
      if (feat & JANUS_X86_SSE4_1) binop_128_sse41(op, elem, 16, tmp + 16, pa + 16, pb + 16);
      else                         binop_128_sse2(op, elem, 16, tmp + 16, pa + 16, pb + 16);
      memcpy(pd, tmp, 32);
      return;
    }
  } else if (bytes <= 16 && op != JANUS_OP_ZIP_LO && op != JANUS_OP_ZIP_HI) {
    if ((op == JANUS_OP_SHL_VAR || op == JANUS_OP_SHR_VAR) && (feat & JANUS_X86_AVX2)) {
      if (binop_128_avx2_varshift(op, elem, bytes, dst, a, b)) return;
    }
    if ((feat & JANUS_X86_SSE4_1) && binop_128_sse41(op, elem, bytes, dst, a, b)) return;
    if (binop_128_sse2(op, elem, bytes, dst, a, b)) return;
  } else if (bytes == 16 && (op == JANUS_OP_ZIP_LO || op == JANUS_OP_ZIP_HI)) {
    if (binop_128_sse2(op, elem, 16, dst, a, b)) return;
  }
#endif
  binop_fallback(op, elem, lanes, dst, a, b);
}

/* -------------------------------------------------------------------------- */
/* Unary SIMD Operations                                                      */
/* -------------------------------------------------------------------------- */

#if defined(__x86_64__)

__attribute__((target("ssse3")))
static int unop_128_ssse3(int op, int elem, int bytes, void *dst, const void *a) {
  __m128i va = load128i_partial(a, bytes);
  if (op == JANUS_UNOP_ABS) {
    switch (elem) {
      case JANUS_SIMD_I8:  store128i_partial(dst, _mm_abs_epi8(va), bytes); return 1;
      case JANUS_SIMD_I16: store128i_partial(dst, _mm_abs_epi16(va), bytes); return 1;
      case JANUS_SIMD_I32: store128i_partial(dst, _mm_abs_epi32(va), bytes); return 1;
      default: break;
    }
  }
  if (op == JANUS_UNOP_POPCNT && (elem == JANUS_SIMD_I8 || elem == JANUS_SIMD_U8)) {
    __m128i lut = _mm_setr_epi8(0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4);
    __m128i low_mask = _mm_set1_epi8(0x0F);
    __m128i lo = _mm_shuffle_epi8(lut, _mm_and_si128(va, low_mask));
    __m128i hi = _mm_shuffle_epi8(lut, _mm_and_si128(_mm_srli_epi16(va, 4), low_mask));
    store128i_partial(dst, _mm_add_epi8(lo, hi), bytes);
    return 1;
  }
  return 0;
}

__attribute__((target("sse4.1")))
static int unop_128_sse41(int op, int elem, int bytes, void *dst, const void *a) {
  if (elem == JANUS_SIMD_F32) {
    __m128 va = load128f_partial(a, bytes, 0.0f);
    __m128 vr;
    switch (op) {
      case JANUS_UNOP_FLOOR: vr = _mm_floor_ps(va); break;
      case JANUS_UNOP_CEIL:  vr = _mm_ceil_ps(va); break;
      case JANUS_UNOP_TRUNC: vr = _mm_round_ps(va, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC); break;
      case JANUS_UNOP_ROUND: vr = _mm_round_ps(va, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC); break;
      default: return 0;
    }
    store128f_partial(dst, vr, bytes);
    return 1;
  }
  if (elem == JANUS_SIMD_F64 && bytes == 16) {
    __m128d va = _mm_loadu_pd((const double *)a);
    __m128d vr;
    switch (op) {
      case JANUS_UNOP_FLOOR: vr = _mm_floor_pd(va); break;
      case JANUS_UNOP_CEIL:  vr = _mm_ceil_pd(va); break;
      case JANUS_UNOP_TRUNC: vr = _mm_round_pd(va, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC); break;
      case JANUS_UNOP_ROUND: vr = _mm_round_pd(va, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC); break;
      default: return 0;
    }
    _mm_storeu_pd((double *)dst, vr);
    return 1;
  }
  return unop_128_ssse3(op, elem, bytes, dst, a);
}

__attribute__((target("avx")))
static int unop_256_avx(int op, int elem, void *dst, const void *a) {
  if (elem == JANUS_SIMD_F32) {
    __m256 va = _mm256_loadu_ps((const float *)a);
    __m256 vr;
    switch (op) {
      case JANUS_UNOP_NEG:   vr = _mm256_xor_ps(va, _mm256_set1_ps(-0.0f)); break;
      case JANUS_UNOP_ABS:   vr = _mm256_and_ps(va, _mm256_castsi256_ps(_mm256_set1_epi32(0x7FFFFFFF))); break;
      case JANUS_UNOP_SQRT:  vr = _mm256_sqrt_ps(va); break;
      case JANUS_UNOP_RSQRT: vr = _mm256_rsqrt_ps(va); break;
      case JANUS_UNOP_RCP:   vr = _mm256_rcp_ps(va); break;
      case JANUS_UNOP_FLOOR: vr = _mm256_floor_ps(va); break;
      case JANUS_UNOP_CEIL:  vr = _mm256_ceil_ps(va); break;
      case JANUS_UNOP_TRUNC: vr = _mm256_round_ps(va, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC); break;
      case JANUS_UNOP_ROUND: vr = _mm256_round_ps(va, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC); break;
      default: return 0;
    }
    _mm256_storeu_ps((float *)dst, vr);
    return 1;
  }
  if (elem == JANUS_SIMD_F64) {
    __m256d va = _mm256_loadu_pd((const double *)a);
    __m256d vr;
    switch (op) {
      case JANUS_UNOP_NEG:   vr = _mm256_xor_pd(va, _mm256_set1_pd(-0.0)); break;
      case JANUS_UNOP_ABS:   vr = _mm256_and_pd(va, _mm256_castsi256_pd(_mm256_set1_epi64x(0x7FFFFFFFFFFFFFFFLL))); break;
      case JANUS_UNOP_SQRT:  vr = _mm256_sqrt_pd(va); break;
      case JANUS_UNOP_FLOOR: vr = _mm256_floor_pd(va); break;
      case JANUS_UNOP_CEIL:  vr = _mm256_ceil_pd(va); break;
      case JANUS_UNOP_TRUNC: vr = _mm256_round_pd(va, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC); break;
      case JANUS_UNOP_ROUND: vr = _mm256_round_pd(va, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC); break;
      default: return 0;
    }
    _mm256_storeu_pd((double *)dst, vr);
    return 1;
  }
  return 0;
}

__attribute__((target("avx2")))
static int unop_256_avx2(int op, int elem, void *dst, const void *a) {
  if (elem == JANUS_SIMD_F32 || elem == JANUS_SIMD_F64) {
    return unop_256_avx(op, elem, dst, a);
  }
  __m256i va = _mm256_loadu_si256((const __m256i *)a);
  if (op == JANUS_UNOP_NOT) {
    _mm256_storeu_si256((__m256i *)dst, _mm256_xor_si256(va, _mm256_set1_epi32(-1)));
    return 1;
  }
  if (op == JANUS_UNOP_ABS) {
    switch (elem) {
      case JANUS_SIMD_I8:  _mm256_storeu_si256((__m256i *)dst, _mm256_abs_epi8(va)); return 1;
      case JANUS_SIMD_I16: _mm256_storeu_si256((__m256i *)dst, _mm256_abs_epi16(va)); return 1;
      case JANUS_SIMD_I32: _mm256_storeu_si256((__m256i *)dst, _mm256_abs_epi32(va)); return 1;
      default: break;
    }
  }
  if (op == JANUS_UNOP_POPCNT && (elem == JANUS_SIMD_I8 || elem == JANUS_SIMD_U8)) {
    __m256i lut = _mm256_setr_epi8(
      0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
      0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4);
    __m256i low_mask = _mm256_set1_epi8(0x0F);
    __m256i lo = _mm256_shuffle_epi8(lut, _mm256_and_si256(va, low_mask));
    __m256i hi = _mm256_shuffle_epi8(lut, _mm256_and_si256(_mm256_srli_epi16(va, 4), low_mask));
    _mm256_storeu_si256((__m256i *)dst, _mm256_add_epi8(lo, hi));
    return 1;
  }
  return 0;
}

#endif /* __x86_64__ */

void janus_simd_unop(int op, int elem, int lanes, void *dst, const void *a) {
  int sz = elem_size(elem);
  int bytes = lanes * sz;
#if defined(__x86_64__)
  uint64_t feat = janus_get_x86_features();
  if (op >= JANUS_UNOP_EXP && (elem == JANUS_SIMD_F32 || elem == JANUS_SIMD_F64)) {
    int has_avx2_fma = ((feat & (JANUS_X86_AVX2 | JANUS_X86_FMA)) == (JANUS_X86_AVX2 | JANUS_X86_FMA));
    if (bytes == 32) {
      if (has_avx2_fma && sleef_unop_256_avx2(op, elem, dst, a)) return;
      if ((feat & JANUS_X86_AVX) && sleef_unop_256_avx(op, elem, dst, a)) return;
      const uint8_t *pa = (const uint8_t *)a;
      uint8_t tmp[32];
      if (feat & JANUS_X86_SSE4_1) {
        sleef_unop_128_sse4(op, elem, 16, tmp, pa);
        sleef_unop_128_sse4(op, elem, 16, tmp + 16, pa + 16);
      } else {
        sleef_unop_128_sse2(op, elem, 16, tmp, pa);
        sleef_unop_128_sse2(op, elem, 16, tmp + 16, pa + 16);
      }
      memcpy(dst, tmp, 32);
      return;
    } else if (bytes <= 16) {
      if (has_avx2_fma && sleef_unop_128_avx2128(op, elem, bytes, dst, a)) return;
      if ((feat & JANUS_X86_SSE4_1) && sleef_unop_128_sse4(op, elem, bytes, dst, a)) return;
      if (sleef_unop_128_sse2(op, elem, bytes, dst, a)) return;
    }
  }
  if (bytes == 32) {
    if ((feat & JANUS_X86_AVX2) && unop_256_avx2(op, elem, dst, a)) return;
    if ((feat & JANUS_X86_AVX) && unop_256_avx(op, elem, dst, a)) return;
  } else if (bytes <= 16) {
    if (op == JANUS_UNOP_NOT) {
      __m128i va = load128i_partial(a, bytes);
      store128i_partial(dst, _mm_xor_si128(va, _mm_set1_epi32(-1)), bytes);
      return;
    }
    if (elem == JANUS_SIMD_F32 && (op == JANUS_UNOP_SQRT || op == JANUS_UNOP_RSQRT || op == JANUS_UNOP_RCP || op == JANUS_UNOP_ABS || op == JANUS_UNOP_NEG)) {
      __m128 va = load128f_partial(a, bytes, 1.0f);
      __m128 vr = va;
      if (op == JANUS_UNOP_SQRT)  vr = _mm_sqrt_ps(va);
      if (op == JANUS_UNOP_RSQRT) vr = _mm_rsqrt_ps(va);
      if (op == JANUS_UNOP_RCP)   vr = _mm_rcp_ps(va);
      if (op == JANUS_UNOP_ABS)   vr = _mm_and_ps(va, _mm_castsi128_ps(_mm_set1_epi32(0x7FFFFFFF)));
      if (op == JANUS_UNOP_NEG)   vr = _mm_xor_ps(va, _mm_set1_ps(-0.0f));
      store128f_partial(dst, vr, bytes);
      return;
    }
    if ((feat & JANUS_X86_SSE4_1) && unop_128_sse41(op, elem, bytes, dst, a)) return;
    if ((feat & JANUS_X86_SSSE3) && unop_128_ssse3(op, elem, bytes, dst, a)) return;
  }
#endif

  const uint8_t *pa = (const uint8_t *)a;
  uint8_t *pd = (uint8_t *)dst;

  if (op == JANUS_UNOP_NOT) {
    for (int i = 0; i < bytes; ++i) pd[i] = (uint8_t)(~pa[i]);
    return;
  }

  for (int i = 0; i < lanes; ++i) {
    const void *ea = pa + i * sz;
    void *ed = pd + i * sz;
    if (elem == JANUS_SIMD_F32) {
      float v, r = 0.0f;
      memcpy(&v, ea, 4);
      switch (op) {
        case JANUS_UNOP_NEG:    r = -v; break;
        case JANUS_UNOP_ABS:    r = fabsf(v); break;
        case JANUS_UNOP_ABSS:   r = fabsf(v); break;
        case JANUS_UNOP_SQRT:   r = sqrtf(v); break;
        case JANUS_UNOP_RSQRT:  r = 1.0f / sqrtf(v); break;
        case JANUS_UNOP_RCP:    r = 1.0f / v; break;
        case JANUS_UNOP_FLOOR:  r = floorf(v); break;
        case JANUS_UNOP_CEIL:   r = ceilf(v); break;
        case JANUS_UNOP_TRUNC:  r = truncf(v); break;
        case JANUS_UNOP_ROUND:  r = nearbyintf(v); break;
        case JANUS_UNOP_SIGNUM: r = (v > 0.0f) ? 1.0f : ((v < 0.0f) ? -1.0f : v); break;
        case JANUS_UNOP_EXP:    r = expf(v); break;
        case JANUS_UNOP_LOG:    r = logf(v); break;
        case JANUS_UNOP_SIN:    r = sinf(v); break;
        case JANUS_UNOP_COS:    r = cosf(v); break;
        case JANUS_UNOP_TAN:    r = tanf(v); break;
        case JANUS_UNOP_ASIN:   r = asinf(v); break;
        case JANUS_UNOP_ACOS:   r = acosf(v); break;
        case JANUS_UNOP_ATAN:   r = atanf(v); break;
        case JANUS_UNOP_SINH:   r = sinhf(v); break;
        case JANUS_UNOP_COSH:   r = coshf(v); break;
        case JANUS_UNOP_TANH:   r = tanhf(v); break;
        case JANUS_UNOP_ASINH:  r = asinhf(v); break;
        case JANUS_UNOP_ACOSH:  r = acoshf(v); break;
        case JANUS_UNOP_ATANH:  r = atanhf(v); break;
        case JANUS_UNOP_EXPM1:  r = expm1f(v); break;
        case JANUS_UNOP_LOG1P:  r = log1pf(v); break;
        default: break;
      }
      memcpy(ed, &r, 4);
      continue;
    }
    if (elem == JANUS_SIMD_F64) {
      double v, r = 0.0;
      memcpy(&v, ea, 8);
      switch (op) {
        case JANUS_UNOP_NEG:    r = -v; break;
        case JANUS_UNOP_ABS:    r = fabs(v); break;
        case JANUS_UNOP_ABSS:   r = fabs(v); break;
        case JANUS_UNOP_SQRT:   r = sqrt(v); break;
        case JANUS_UNOP_RSQRT:  r = 1.0 / sqrt(v); break;
        case JANUS_UNOP_RCP:    r = 1.0 / v; break;
        case JANUS_UNOP_FLOOR:  r = floor(v); break;
        case JANUS_UNOP_CEIL:   r = ceil(v); break;
        case JANUS_UNOP_TRUNC:  r = trunc(v); break;
        case JANUS_UNOP_ROUND:  r = nearbyint(v); break;
        case JANUS_UNOP_SIGNUM: r = (v > 0.0) ? 1.0 : ((v < 0.0) ? -1.0 : v); break;
        case JANUS_UNOP_EXP:    r = exp(v); break;
        case JANUS_UNOP_LOG:    r = log(v); break;
        case JANUS_UNOP_SIN:    r = sin(v); break;
        case JANUS_UNOP_COS:    r = cos(v); break;
        case JANUS_UNOP_TAN:    r = tan(v); break;
        case JANUS_UNOP_ASIN:   r = asin(v); break;
        case JANUS_UNOP_ACOS:   r = acos(v); break;
        case JANUS_UNOP_ATAN:   r = atan(v); break;
        case JANUS_UNOP_SINH:   r = sinh(v); break;
        case JANUS_UNOP_COSH:   r = cosh(v); break;
        case JANUS_UNOP_TANH:   r = tanh(v); break;
        case JANUS_UNOP_ASINH:  r = asinh(v); break;
        case JANUS_UNOP_ACOSH:  r = acosh(v); break;
        case JANUS_UNOP_ATANH:  r = atanh(v); break;
        case JANUS_UNOP_EXPM1:  r = expm1(v); break;
        case JANUS_UNOP_LOG1P:  r = log1p(v); break;
        default: break;
      }
      memcpy(ed, &r, 8);
      continue;
    }

    int is_signed = (elem == JANUS_SIMD_I8 || elem == JANUS_SIMD_I16 ||
                     elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_I64);
    int bits = sz * 8;
    uint64_t uv = 0;
    int64_t sv = 0;
    switch (elem) {
      case JANUS_SIMD_I8:  { int8_t  t; memcpy(&t, ea, 1); sv = t; uv = (uint8_t)t; break; }
      case JANUS_SIMD_U8:  { uint8_t t; memcpy(&t, ea, 1); uv = t; break; }
      case JANUS_SIMD_I16: { int16_t t; memcpy(&t, ea, 2); sv = t; uv = (uint16_t)t; break; }
      case JANUS_SIMD_U16: { uint16_t t; memcpy(&t, ea, 2); uv = t; break; }
      case JANUS_SIMD_I32: { int32_t t; memcpy(&t, ea, 4); sv = t; uv = (uint32_t)t; break; }
      case JANUS_SIMD_U32: { uint32_t t; memcpy(&t, ea, 4); uv = t; break; }
      case JANUS_SIMD_I64: { int64_t t; memcpy(&t, ea, 8); sv = t; uv = (uint64_t)t; break; }
      case JANUS_SIMD_U64: { uint64_t t; memcpy(&t, ea, 8); uv = t; break; }
      default: break;
    }
    int64_t smin = (bits == 64) ? INT64_MIN : -(1LL << (bits - 1));
    int64_t smax = (bits == 64) ? INT64_MAX : ((1LL << (bits - 1)) - 1);
    uint64_t umax = (bits == 64) ? UINT64_MAX : ((1ULL << bits) - 1ULL);
    uint64_t ur = 0;

    switch (op) {
      case JANUS_UNOP_NEG: ur = 0ULL - uv; break;
      case JANUS_UNOP_ABS:
        ur = is_signed ? (sv < 0 ? (0ULL - uv) : uv) : uv;
        break;
      case JANUS_UNOP_ABSS:
        if (!is_signed) ur = uv;
        else if (sv == smin) ur = (uint64_t)smax;
        else ur = (sv < 0) ? (0ULL - uv) : uv;
        break;
      case JANUS_UNOP_POPCNT:
        ur = (uint64_t)__builtin_popcountll(uv & umax);
        break;
      case JANUS_UNOP_SIGNUM:
        if (is_signed) ur = (uint64_t)(int64_t)((sv > 0) - (sv < 0));
        else           ur = (uv > 0) ? 1ULL : 0ULL;
        break;
      default: break;
    }
    memcpy(ed, &ur, (size_t)sz);
  }
}

/* -------------------------------------------------------------------------- */
/* Ternary SIMD Operations (Select, FMA, FMS, FNMA, FNMS, MulAdd)             */
/* -------------------------------------------------------------------------- */

#if defined(__x86_64__)

__attribute__((target("sse4.1")))
static void select_128_sse41(int bytes, void *dst, const void *m, const void *t, const void *f) {
  __m128i vm = load128i_partial(m, bytes);
  __m128i vt = load128i_partial(t, bytes);
  __m128i vf = load128i_partial(f, bytes);
  store128i_partial(dst, _mm_blendv_epi8(vf, vt, vm), bytes);
}

__attribute__((target("avx2")))
static void select_256_avx2(void *dst, const void *m, const void *t, const void *f) {
  __m256i vm = _mm256_loadu_si256((const __m256i *)m);
  __m256i vt = _mm256_loadu_si256((const __m256i *)t);
  __m256i vf = _mm256_loadu_si256((const __m256i *)f);
  _mm256_storeu_si256((__m256i *)dst, _mm256_blendv_epi8(vf, vt, vm));
}

__attribute__((target("avx,fma")))
static int fma_hw(int op, int elem, int bytes, void *dst, const void *a, const void *b, const void *c) {
  if (elem == JANUS_SIMD_F32 && bytes <= 16) {
    __m128 va = load128f_partial(a, bytes, 0.0f);
    __m128 vb = load128f_partial(b, bytes, 0.0f);
    __m128 vc = load128f_partial(c, bytes, 0.0f);
    __m128 vr;
    switch (op) {
      case JANUS_TERNOP_FMA:
      case JANUS_TERNOP_MULADD: vr = _mm_fmadd_ps(va, vb, vc); break;
      case JANUS_TERNOP_FMS:    vr = _mm_fmsub_ps(va, vb, vc); break;
      case JANUS_TERNOP_FNMA:   vr = _mm_fnmadd_ps(va, vb, vc); break;
      case JANUS_TERNOP_FNMS:   vr = _mm_fnmsub_ps(va, vb, vc); break;
      default: return 0;
    }
    store128f_partial(dst, vr, bytes);
    return 1;
  }
  if (elem == JANUS_SIMD_F64 && bytes == 16) {
    __m128d va = _mm_loadu_pd((const double *)a);
    __m128d vb = _mm_loadu_pd((const double *)b);
    __m128d vc = _mm_loadu_pd((const double *)c);
    __m128d vr;
    switch (op) {
      case JANUS_TERNOP_FMA:
      case JANUS_TERNOP_MULADD: vr = _mm_fmadd_pd(va, vb, vc); break;
      case JANUS_TERNOP_FMS:    vr = _mm_fmsub_pd(va, vb, vc); break;
      case JANUS_TERNOP_FNMA:   vr = _mm_fnmadd_pd(va, vb, vc); break;
      case JANUS_TERNOP_FNMS:   vr = _mm_fnmsub_pd(va, vb, vc); break;
      default: return 0;
    }
    _mm_storeu_pd((double *)dst, vr);
    return 1;
  }
  if (elem == JANUS_SIMD_F32 && bytes == 32) {
    __m256 va = _mm256_loadu_ps((const float *)a);
    __m256 vb = _mm256_loadu_ps((const float *)b);
    __m256 vc = _mm256_loadu_ps((const float *)c);
    __m256 vr;
    switch (op) {
      case JANUS_TERNOP_FMA:
      case JANUS_TERNOP_MULADD: vr = _mm256_fmadd_ps(va, vb, vc); break;
      case JANUS_TERNOP_FMS:    vr = _mm256_fmsub_ps(va, vb, vc); break;
      case JANUS_TERNOP_FNMA:   vr = _mm256_fnmadd_ps(va, vb, vc); break;
      case JANUS_TERNOP_FNMS:   vr = _mm256_fnmsub_ps(va, vb, vc); break;
      default: return 0;
    }
    _mm256_storeu_ps((float *)dst, vr);
    return 1;
  }
  if (elem == JANUS_SIMD_F64 && bytes == 32) {
    __m256d va = _mm256_loadu_pd((const double *)a);
    __m256d vb = _mm256_loadu_pd((const double *)b);
    __m256d vc = _mm256_loadu_pd((const double *)c);
    __m256d vr;
    switch (op) {
      case JANUS_TERNOP_FMA:
      case JANUS_TERNOP_MULADD: vr = _mm256_fmadd_pd(va, vb, vc); break;
      case JANUS_TERNOP_FMS:    vr = _mm256_fmsub_pd(va, vb, vc); break;
      case JANUS_TERNOP_FNMA:   vr = _mm256_fnmadd_pd(va, vb, vc); break;
      case JANUS_TERNOP_FNMS:   vr = _mm256_fnmsub_pd(va, vb, vc); break;
      default: return 0;
    }
    _mm256_storeu_pd((double *)dst, vr);
    return 1;
  }
  return 0;
}

#endif /* __x86_64__ */

void janus_simd_ternop(int op, int elem, int lanes, void *dst, const void *a, const void *b, const void *c) {
  int sz = elem_size(elem);
  int bytes = lanes * sz;

  if (op == JANUS_TERNOP_SELECT) {
#if defined(__x86_64__)
    uint64_t feat = janus_get_x86_features();
    if (bytes == 32 && (feat & JANUS_X86_AVX2)) {
      select_256_avx2(dst, a, b, c);
      return;
    }
    if (bytes <= 16 && (feat & JANUS_X86_SSE4_1)) {
      select_128_sse41(bytes, dst, a, b, c);
      return;
    }
    if (bytes <= 16) {
      __m128i vm = load128i_partial(a, bytes);
      __m128i vt = load128i_partial(b, bytes);
      __m128i vf = load128i_partial(c, bytes);
      store128i_partial(dst, _mm_or_si128(_mm_and_si128(vm, vt), _mm_andnot_si128(vm, vf)), bytes);
      return;
    }
#endif
    const uint8_t *pm = (const uint8_t *)a;
    const uint8_t *pt = (const uint8_t *)b;
    const uint8_t *pf = (const uint8_t *)c;
    uint8_t *pd = (uint8_t *)dst;
    for (int i = 0; i < bytes; ++i) {
      pd[i] = (uint8_t)((pm[i] & pt[i]) | ((~pm[i]) & pf[i]));
    }
    return;
  }

#if defined(__x86_64__)
  uint64_t feat = janus_get_x86_features();
  if ((feat & JANUS_X86_FMA) && fma_hw(op, elem, bytes, dst, a, b, c)) {
    return;
  }
#endif

  const uint8_t *pa = (const uint8_t *)a;
  const uint8_t *pb = (const uint8_t *)b;
  const uint8_t *pc = (const uint8_t *)c;
  uint8_t *pd = (uint8_t *)dst;

  for (int i = 0; i < lanes; ++i) {
    if (elem == JANUS_SIMD_F32) {
      float va, vb, vc, vr = 0.0f;
      memcpy(&va, pa + i * 4, 4);
      memcpy(&vb, pb + i * 4, 4);
      memcpy(&vc, pc + i * 4, 4);
      switch (op) {
        case JANUS_TERNOP_FMA:    vr = fmaf(va, vb, vc); break;
        case JANUS_TERNOP_FMS:    vr = fmaf(va, vb, -vc); break;
        case JANUS_TERNOP_FNMA:   vr = fmaf(-va, vb, vc); break;
        case JANUS_TERNOP_FNMS:   vr = fmaf(-va, vb, -vc); break;
        case JANUS_TERNOP_MULADD: vr = va * vb + vc; break;
        default: break;
      }
      memcpy(pd + i * 4, &vr, 4);
    } else if (elem == JANUS_SIMD_F64) {
      double va, vb, vc, vr = 0.0;
      memcpy(&va, pa + i * 8, 8);
      memcpy(&vb, pb + i * 8, 8);
      memcpy(&vc, pc + i * 8, 8);
      switch (op) {
        case JANUS_TERNOP_FMA:    vr = fma(va, vb, vc); break;
        case JANUS_TERNOP_FMS:    vr = fma(va, vb, -vc); break;
        case JANUS_TERNOP_FNMA:   vr = fma(-va, vb, vc); break;
        case JANUS_TERNOP_FNMS:   vr = fma(-va, vb, -vc); break;
        case JANUS_TERNOP_MULADD: vr = va * vb + vc; break;
        default: break;
      }
      memcpy(pd + i * 8, &vr, 8);
    }
  }
}

/* -------------------------------------------------------------------------- */
/* Comparisons (all-1s / all-0s per lane)                                     */
/* -------------------------------------------------------------------------- */

void janus_simd_cmp(int cmp_op, int elem, int lanes, void *dst, const void *a, const void *b) {
  int sz = elem_size(elem);
  int bytes = lanes * sz;

#if defined(__x86_64__)
  uint64_t feat = janus_get_x86_features();
  if (bytes == 32 && (feat & JANUS_X86_AVX2) &&
      (elem == JANUS_SIMD_I8 || elem == JANUS_SIMD_I16 || elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_I64)) {
    /* Fall through to exact lane evaluator or SSE/AVX2 below */
  }
  if (bytes <= 16 && elem == JANUS_SIMD_F32) {
    __m128 va = load128f_partial(a, bytes, 0.0f);
    __m128 vb = load128f_partial(b, bytes, 0.0f);
    __m128 vr;
    switch (cmp_op) {
      case JANUS_CMP_EQ: vr = _mm_cmpeq_ps(va, vb); break;
      case JANUS_CMP_NE: vr = _mm_cmpneq_ps(va, vb); break;
      case JANUS_CMP_LT: vr = _mm_cmplt_ps(va, vb); break;
      case JANUS_CMP_LE: vr = _mm_cmple_ps(va, vb); break;
      case JANUS_CMP_GT: vr = _mm_cmpgt_ps(va, vb); break;
      default:           vr = _mm_cmpge_ps(va, vb); break;
    }
    store128f_partial(dst, vr, bytes);
    return;
  }
#endif

  const uint8_t *pa = (const uint8_t *)a;
  const uint8_t *pb = (const uint8_t *)b;
  uint8_t *pd = (uint8_t *)dst;

  for (int i = 0; i < lanes; ++i) {
    const void *ea = pa + i * sz;
    const void *eb = pb + i * sz;
    int cond = 0;
    switch (elem) {
      case JANUS_SIMD_I8: {
        int8_t va, vb; memcpy(&va, ea, 1); memcpy(&vb, eb, 1);
        if (cmp_op == JANUS_CMP_EQ) cond = (va == vb);
        else if (cmp_op == JANUS_CMP_NE) cond = (va != vb);
        else if (cmp_op == JANUS_CMP_LT) cond = (va < vb);
        else if (cmp_op == JANUS_CMP_LE) cond = (va <= vb);
        else if (cmp_op == JANUS_CMP_GT) cond = (va > vb);
        else cond = (va >= vb);
        break;
      }
      case JANUS_SIMD_U8: {
        uint8_t va, vb; memcpy(&va, ea, 1); memcpy(&vb, eb, 1);
        if (cmp_op == JANUS_CMP_EQ) cond = (va == vb);
        else if (cmp_op == JANUS_CMP_NE) cond = (va != vb);
        else if (cmp_op == JANUS_CMP_LT) cond = (va < vb);
        else if (cmp_op == JANUS_CMP_LE) cond = (va <= vb);
        else if (cmp_op == JANUS_CMP_GT) cond = (va > vb);
        else cond = (va >= vb);
        break;
      }
      case JANUS_SIMD_I16: {
        int16_t va, vb; memcpy(&va, ea, 2); memcpy(&vb, eb, 2);
        if (cmp_op == JANUS_CMP_EQ) cond = (va == vb);
        else if (cmp_op == JANUS_CMP_NE) cond = (va != vb);
        else if (cmp_op == JANUS_CMP_LT) cond = (va < vb);
        else if (cmp_op == JANUS_CMP_LE) cond = (va <= vb);
        else if (cmp_op == JANUS_CMP_GT) cond = (va > vb);
        else cond = (va >= vb);
        break;
      }
      case JANUS_SIMD_U16: {
        uint16_t va, vb; memcpy(&va, ea, 2); memcpy(&vb, eb, 2);
        if (cmp_op == JANUS_CMP_EQ) cond = (va == vb);
        else if (cmp_op == JANUS_CMP_NE) cond = (va != vb);
        else if (cmp_op == JANUS_CMP_LT) cond = (va < vb);
        else if (cmp_op == JANUS_CMP_LE) cond = (va <= vb);
        else if (cmp_op == JANUS_CMP_GT) cond = (va > vb);
        else cond = (va >= vb);
        break;
      }
      case JANUS_SIMD_I32: {
        int32_t va, vb; memcpy(&va, ea, 4); memcpy(&vb, eb, 4);
        if (cmp_op == JANUS_CMP_EQ) cond = (va == vb);
        else if (cmp_op == JANUS_CMP_NE) cond = (va != vb);
        else if (cmp_op == JANUS_CMP_LT) cond = (va < vb);
        else if (cmp_op == JANUS_CMP_LE) cond = (va <= vb);
        else if (cmp_op == JANUS_CMP_GT) cond = (va > vb);
        else cond = (va >= vb);
        break;
      }
      case JANUS_SIMD_U32: {
        uint32_t va, vb; memcpy(&va, ea, 4); memcpy(&vb, eb, 4);
        if (cmp_op == JANUS_CMP_EQ) cond = (va == vb);
        else if (cmp_op == JANUS_CMP_NE) cond = (va != vb);
        else if (cmp_op == JANUS_CMP_LT) cond = (va < vb);
        else if (cmp_op == JANUS_CMP_LE) cond = (va <= vb);
        else if (cmp_op == JANUS_CMP_GT) cond = (va > vb);
        else cond = (va >= vb);
        break;
      }
      case JANUS_SIMD_I64: {
        int64_t va, vb; memcpy(&va, ea, 8); memcpy(&vb, eb, 8);
        if (cmp_op == JANUS_CMP_EQ) cond = (va == vb);
        else if (cmp_op == JANUS_CMP_NE) cond = (va != vb);
        else if (cmp_op == JANUS_CMP_LT) cond = (va < vb);
        else if (cmp_op == JANUS_CMP_LE) cond = (va <= vb);
        else if (cmp_op == JANUS_CMP_GT) cond = (va > vb);
        else cond = (va >= vb);
        break;
      }
      case JANUS_SIMD_U64: {
        uint64_t va, vb; memcpy(&va, ea, 8); memcpy(&vb, eb, 8);
        if (cmp_op == JANUS_CMP_EQ) cond = (va == vb);
        else if (cmp_op == JANUS_CMP_NE) cond = (va != vb);
        else if (cmp_op == JANUS_CMP_LT) cond = (va < vb);
        else if (cmp_op == JANUS_CMP_LE) cond = (va <= vb);
        else if (cmp_op == JANUS_CMP_GT) cond = (va > vb);
        else cond = (va >= vb);
        break;
      }
      case JANUS_SIMD_F32: {
        float va, vb; memcpy(&va, ea, 4); memcpy(&vb, eb, 4);
        if (cmp_op == JANUS_CMP_EQ) cond = (va == vb);
        else if (cmp_op == JANUS_CMP_NE) cond = (va != vb);
        else if (cmp_op == JANUS_CMP_LT) cond = (va < vb);
        else if (cmp_op == JANUS_CMP_LE) cond = (va <= vb);
        else if (cmp_op == JANUS_CMP_GT) cond = (va > vb);
        else cond = (va >= vb);
        break;
      }
      case JANUS_SIMD_F64: {
        double va, vb; memcpy(&va, ea, 8); memcpy(&vb, eb, 8);
        if (cmp_op == JANUS_CMP_EQ) cond = (va == vb);
        else if (cmp_op == JANUS_CMP_NE) cond = (va != vb);
        else if (cmp_op == JANUS_CMP_LT) cond = (va < vb);
        else if (cmp_op == JANUS_CMP_LE) cond = (va <= vb);
        else if (cmp_op == JANUS_CMP_GT) cond = (va > vb);
        else cond = (va >= vb);
        break;
      }
      default: break;
    }
    memset(pd + i * sz, cond ? 0xFF : 0x00, (size_t)sz);
  }
}

/* -------------------------------------------------------------------------- */
/* Uniform Shifts                                                             */
/* -------------------------------------------------------------------------- */

void janus_simd_shift(int is_right, int elem, int lanes, void *dst, const void *a, int32_t count) {
  int sz = elem_size(elem);
  int bits = sz * 8;
  uint32_t ucnt = (uint32_t)count;
  int is_signed = (elem == JANUS_SIMD_I8 || elem == JANUS_SIMD_I16 ||
                   elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_I64);
  uint64_t umax = (bits == 64) ? UINT64_MAX : ((1ULL << bits) - 1ULL);

#if defined(__x86_64__)
  int bytes = lanes * sz;
  if (bytes <= 16 && (elem == JANUS_SIMD_I16 || elem == JANUS_SIMD_U16 ||
                      elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_U32 ||
                      (elem == JANUS_SIMD_U64) || (!is_right && elem == JANUS_SIMD_I64))) {
    __m128i va = load128i_partial(a, bytes);
    __m128i vc = _mm_cvtsi32_si128(count);
    __m128i vr = va;
    if (!is_right) {
      if (sz == 2)      vr = _mm_sll_epi16(va, vc);
      else if (sz == 4) vr = _mm_sll_epi32(va, vc);
      else              vr = _mm_sll_epi64(va, vc);
    } else if (!is_signed) {
      if (sz == 2)      vr = _mm_srl_epi16(va, vc);
      else if (sz == 4) vr = _mm_srl_epi32(va, vc);
      else              vr = _mm_srl_epi64(va, vc);
    } else {
      if (sz == 2)      vr = _mm_sra_epi16(va, vc);
      else if (sz == 4) vr = _mm_sra_epi32(va, vc);
    }
    if (ucnt >= 32) {
      /* Match _mm_sll_* behavior when count fits in 32-bit vs >=32 */
    } else {
      store128i_partial(dst, vr, bytes);
      return;
    }
  }
#endif

  const uint8_t *pa = (const uint8_t *)a;
  uint8_t *pd = (uint8_t *)dst;
  for (int i = 0; i < lanes; ++i) {
    const void *ea = pa + i * sz;
    void *ed = pd + i * sz;
    uint64_t uv = 0;
    int64_t sv = 0;
    switch (elem) {
      case JANUS_SIMD_I8:  { int8_t  t; memcpy(&t, ea, 1); sv = t; uv = (uint8_t)t; break; }
      case JANUS_SIMD_U8:  { uint8_t t; memcpy(&t, ea, 1); uv = t; break; }
      case JANUS_SIMD_I16: { int16_t t; memcpy(&t, ea, 2); sv = t; uv = (uint16_t)t; break; }
      case JANUS_SIMD_U16: { uint16_t t; memcpy(&t, ea, 2); uv = t; break; }
      case JANUS_SIMD_I32: { int32_t t; memcpy(&t, ea, 4); sv = t; uv = (uint32_t)t; break; }
      case JANUS_SIMD_U32: { uint32_t t; memcpy(&t, ea, 4); uv = t; break; }
      case JANUS_SIMD_I64: { int64_t t; memcpy(&t, ea, 8); sv = t; uv = (uint64_t)t; break; }
      case JANUS_SIMD_U64: { uint64_t t; memcpy(&t, ea, 8); uv = t; break; }
      default: break;
    }
    uint64_t ur = 0;
    if (!is_right) {
      ur = (ucnt >= (uint32_t)bits) ? 0ULL : (uv << ucnt);
    } else if (is_signed) {
      ur = (ucnt >= (uint32_t)bits) ? (sv < 0 ? UINT64_MAX : 0ULL) : (uint64_t)(sv >> ucnt);
    } else {
      ur = (ucnt >= (uint32_t)bits) ? 0ULL : ((uv & umax) >> ucnt);
    }
    memcpy(ed, &ur, (size_t)sz);
  }
}

/* -------------------------------------------------------------------------- */
/* Mask Queries                                                               */
/* -------------------------------------------------------------------------- */

#if defined(__x86_64__)
__attribute__((target("avx2")))
static uint32_t movemask_256_avx2_u8(const void *mask) {
  return (uint32_t)_mm256_movemask_epi8(_mm256_loadu_si256((const __m256i *)mask));
}

__attribute__((target("avx")))
static uint32_t movemask_256_avx_32(const void *mask) {
  return (uint32_t)_mm256_movemask_ps(_mm256_loadu_ps((const float *)mask));
}

__attribute__((target("avx")))
static uint32_t movemask_256_avx_64(const void *mask) {
  return (uint32_t)_mm256_movemask_pd(_mm256_loadu_pd((const double *)mask));
}

__attribute__((target("popcnt")))
static int32_t hw_popcnt32(uint32_t m) {
  return (int32_t)_mm_popcnt_u32(m);
}
#endif

uint32_t janus_simd_movemask(int elem, int lanes, const void *mask) {
  int sz = elem_size(elem);
#if defined(__x86_64__)
  uint64_t feat = janus_get_x86_features();
  int bytes = lanes * sz;
  if (sz == 1 && bytes <= 16) {
    uint32_t m = (uint32_t)_mm_movemask_epi8(load128i_partial(mask, bytes));
    uint32_t lmask = (lanes == 32) ? 0xFFFFFFFFu : ((1u << lanes) - 1u);
    return m & lmask;
  }
  if (sz == 1 && bytes == 32 && (feat & JANUS_X86_AVX2)) {
    return movemask_256_avx2_u8(mask);
  }
  if (sz == 4 && bytes <= 16) {
    uint32_t m = (uint32_t)_mm_movemask_ps(load128f_partial(mask, bytes, 0.0f));
    return m & ((1u << lanes) - 1u);
  }
  if (sz == 4 && bytes == 32 && (feat & JANUS_X86_AVX)) {
    return movemask_256_avx_32(mask);
  }
  if (sz == 8 && bytes == 16) {
    return (uint32_t)_mm_movemask_pd(_mm_loadu_pd((const double *)mask));
  }
  if (sz == 8 && bytes == 32 && (feat & JANUS_X86_AVX)) {
    return movemask_256_avx_64(mask);
  }
#endif
  const uint8_t *pm = (const uint8_t *)mask;
  uint32_t bits = 0;
  for (int i = 0; i < lanes; ++i) {
    /* MSB of lane determines sign/mask bit */
    if (pm[i * sz + (sz - 1)] & 0x80u) {
      bits |= (1u << i);
    }
  }
  return bits;
}

int32_t janus_simd_mask_popcount(int elem, int lanes, const void *mask) {
  uint32_t m = janus_simd_movemask(elem, lanes, mask);
#if defined(__x86_64__)
  if (janus_get_x86_features() & JANUS_X86_POPCNT) {
    return hw_popcnt32(m);
  }
#endif
  return (int32_t)__builtin_popcount(m);
}

int32_t janus_simd_mask_first_true(int elem, int lanes, const void *mask) {
  uint32_t m = janus_simd_movemask(elem, lanes, mask);
  if (m == 0) return -1;
  return (int32_t)__builtin_ctz(m);
}

/* -------------------------------------------------------------------------- */
/* Permutations & Byte Shuffles                                               */
/* -------------------------------------------------------------------------- */

#if defined(__x86_64__)
__attribute__((target("ssse3")))
static void shuffle_bytes_128_ssse3(void *dst, const void *src, const void *ctrl) {
  __m128i vs = _mm_loadu_si128((const __m128i *)src);
  __m128i vc = _mm_loadu_si128((const __m128i *)ctrl);
  _mm_storeu_si128((__m128i *)dst, _mm_shuffle_epi8(vs, vc));
}

__attribute__((target("avx2")))
static void permute_256_avx2_32(void *dst, const void *src, const void *indices_i32) {
  __m256i vs = _mm256_loadu_si256((const __m256i *)src);
  __m256i vi = _mm256_loadu_si256((const __m256i *)indices_i32);
  _mm256_storeu_si256((__m256i *)dst, _mm256_permutevar8x32_epi32(vs, vi));
}
#endif

void janus_simd_permute_lanes(int elem, int lanes, void *dst, const void *src, const void *indices_i32) {
  int sz = elem_size(elem);
#if defined(__x86_64__)
  uint64_t feat = janus_get_x86_features();
  if (sz == 4 && lanes == 8 && (feat & JANUS_X86_AVX2)) {
    permute_256_avx2_32(dst, src, indices_i32);
    return;
  }
#endif
  const uint8_t *ps = (const uint8_t *)src;
  const int32_t *pi = (const int32_t *)indices_i32;
  uint8_t tmp[32];
  for (int i = 0; i < lanes; ++i) {
    int idx = (int)((uint32_t)pi[i] & (uint32_t)(lanes - 1));
    memcpy(tmp + i * sz, ps + idx * sz, (size_t)sz);
  }
  memcpy(dst, tmp, (size_t)(lanes * sz));
}

void janus_simd_shuffle_bytes(int bytes, void *dst, const void *src, const void *ctrl) {
#if defined(__x86_64__)
  uint64_t feat = janus_get_x86_features();
  if (bytes == 16 && (feat & JANUS_X86_SSSE3)) {
    shuffle_bytes_128_ssse3(dst, src, ctrl);
    return;
  }
#endif
  const uint8_t *ps = (const uint8_t *)src;
  const uint8_t *pc = (const uint8_t *)ctrl;
  uint8_t tmp[32];
  uint8_t mask = (uint8_t)(bytes - 1);
  for (int i = 0; i < bytes; ++i) {
    uint8_t c = pc[i];
    tmp[i] = (c & 0x80u) ? 0u : ps[c & mask];
  }
  memcpy(dst, tmp, (size_t)bytes);
}

/* -------------------------------------------------------------------------- */
/* Horizontal Reductions                                                      */
/* -------------------------------------------------------------------------- */

void janus_simd_reduce(int red_op, int elem, int lanes, void *scalar_dst, const void *src) {
  int sz = elem_size(elem);
  const uint8_t *ps = (const uint8_t *)src;

  if (red_op == JANUS_RED_AND || red_op == JANUS_RED_OR || red_op == JANUS_RED_XOR) {
    uint8_t acc[8];
    memcpy(acc, ps, (size_t)sz);
    for (int i = 1; i < lanes; ++i) {
      for (int k = 0; k < sz; ++k) {
        if (red_op == JANUS_RED_AND) acc[k] &= ps[i * sz + k];
        if (red_op == JANUS_RED_OR)  acc[k] |= ps[i * sz + k];
        if (red_op == JANUS_RED_XOR) acc[k] ^= ps[i * sz + k];
      }
    }
    memcpy(scalar_dst, acc, (size_t)sz);
    return;
  }

  if (elem == JANUS_SIMD_F32) {
    float acc;
    memcpy(&acc, ps, 4);
    for (int i = 1; i < lanes; ++i) {
      float v;
      memcpy(&v, ps + i * 4, 4);
      if (red_op == JANUS_RED_ADD)      acc += v;
      else if (red_op == JANUS_RED_MIN) acc = (v < acc) ? v : acc;
      else if (red_op == JANUS_RED_MAX) acc = (v > acc) ? v : acc;
    }
    memcpy(scalar_dst, &acc, 4);
    return;
  }
  if (elem == JANUS_SIMD_F64) {
    double acc;
    memcpy(&acc, ps, 8);
    for (int i = 1; i < lanes; ++i) {
      double v;
      memcpy(&v, ps + i * 8, 8);
      if (red_op == JANUS_RED_ADD)      acc += v;
      else if (red_op == JANUS_RED_MIN) acc = (v < acc) ? v : acc;
      else if (red_op == JANUS_RED_MAX) acc = (v > acc) ? v : acc;
    }
    memcpy(scalar_dst, &acc, 8);
    return;
  }

  int is_signed = (elem == JANUS_SIMD_I8 || elem == JANUS_SIMD_I16 ||
                   elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_I64);
  int64_t sacc = 0;
  uint64_t uacc = 0;
  for (int i = 0; i < lanes; ++i) {
    const void *ea = ps + i * sz;
    int64_t sv = 0;
    uint64_t uv = 0;
    switch (elem) {
      case JANUS_SIMD_I8:  { int8_t  t; memcpy(&t, ea, 1); sv = t; uv = (uint8_t)t; break; }
      case JANUS_SIMD_U8:  { uint8_t t; memcpy(&t, ea, 1); uv = t; break; }
      case JANUS_SIMD_I16: { int16_t t; memcpy(&t, ea, 2); sv = t; uv = (uint16_t)t; break; }
      case JANUS_SIMD_U16: { uint16_t t; memcpy(&t, ea, 2); uv = t; break; }
      case JANUS_SIMD_I32: { int32_t t; memcpy(&t, ea, 4); sv = t; uv = (uint32_t)t; break; }
      case JANUS_SIMD_U32: { uint32_t t; memcpy(&t, ea, 4); uv = t; break; }
      case JANUS_SIMD_I64: { int64_t t; memcpy(&t, ea, 8); sv = t; uv = (uint64_t)t; break; }
      case JANUS_SIMD_U64: { uint64_t t; memcpy(&t, ea, 8); uv = t; break; }
      default: break;
    }
    if (i == 0) {
      sacc = sv; uacc = uv;
    } else if (red_op == JANUS_RED_ADD) {
      uacc += uv;
      sacc = (int64_t)uacc;
    } else if (red_op == JANUS_RED_MIN) {
      if (is_signed) { if (sv < sacc) sacc = sv; }
      else           { if (uv < uacc) uacc = uv; }
    } else if (red_op == JANUS_RED_MAX) {
      if (is_signed) { if (sv > sacc) sacc = sv; }
      else           { if (uv > uacc) uacc = uv; }
    }
  }
  uint64_t out = (red_op == JANUS_RED_ADD) ? uacc : (is_signed ? (uint64_t)sacc : uacc);
  memcpy(scalar_dst, &out, (size_t)sz);
}

/* -------------------------------------------------------------------------- */
/* Dot Products (dp4a, dp2a) & SAD                                            */
/* -------------------------------------------------------------------------- */

int32_t janus_simd_dp4a_s(const void *a4, const void *b4, int32_t acc) {
  const int8_t *pa = (const int8_t *)a4;
  const int8_t *pb = (const int8_t *)b4;
  uint32_t sum = (uint32_t)acc;
  for (int i = 0; i < 4; ++i) {
    sum += (uint32_t)((int32_t)pa[i] * (int32_t)pb[i]);
  }
  return (int32_t)sum;
}

uint32_t janus_simd_dp4a_u(const void *a4, const void *b4, uint32_t acc) {
  const uint8_t *pa = (const uint8_t *)a4;
  const uint8_t *pb = (const uint8_t *)b4;
  uint32_t sum = acc;
  for (int i = 0; i < 4; ++i) {
    sum += (uint32_t)pa[i] * (uint32_t)pb[i];
  }
  return sum;
}

void janus_simd_dp4a_vec_s(int groups, void *dst_i32, const void *a, const void *b, const void *acc_i32) {
  const uint8_t *pa = (const uint8_t *)a;
  const uint8_t *pb = (const uint8_t *)b;
  const int32_t *pacc = (const int32_t *)acc_i32;
  int32_t *pd = (int32_t *)dst_i32;
  for (int g = 0; g < groups; ++g) {
    int32_t c;
    memcpy(&c, pacc + g, 4);
    int32_t r = janus_simd_dp4a_s(pa + g * 4, pb + g * 4, c);
    memcpy(pd + g, &r, 4);
  }
}

void janus_simd_dp4a_vec_u(int groups, void *dst_u32, const void *a, const void *b, const void *acc_u32) {
  const uint8_t *pa = (const uint8_t *)a;
  const uint8_t *pb = (const uint8_t *)b;
  const uint32_t *pacc = (const uint32_t *)acc_u32;
  uint32_t *pd = (uint32_t *)dst_u32;
  for (int g = 0; g < groups; ++g) {
    uint32_t c;
    memcpy(&c, pacc + g, 4);
    uint32_t r = janus_simd_dp4a_u(pa + g * 4, pb + g * 4, c);
    memcpy(pd + g, &r, 4);
  }
}

int32_t janus_simd_dp2a_s(int is_hi, const void *a2_i16, const void *b4_i8, int32_t acc) {
  int16_t a0, a1;
  memcpy(&a0, (const uint8_t *)a2_i16, 2);
  memcpy(&a1, (const uint8_t *)a2_i16 + 2, 2);
  const int8_t *pb = (const int8_t *)b4_i8 + (is_hi ? 2 : 0);
  uint32_t prod = (uint32_t)((int32_t)a0 * (int32_t)pb[0] + (int32_t)a1 * (int32_t)pb[1]);
  return (int32_t)((uint32_t)acc + prod);
}

uint32_t janus_simd_dp2a_u(int is_hi, const void *a2_u16, const void *b4_u8, uint32_t acc) {
  uint16_t a0, a1;
  memcpy(&a0, (const uint8_t *)a2_u16, 2);
  memcpy(&a1, (const uint8_t *)a2_u16 + 2, 2);
  const uint8_t *pb = (const uint8_t *)b4_u8 + (is_hi ? 2 : 0);
  return acc + (uint32_t)a0 * (uint32_t)pb[0] + (uint32_t)a1 * (uint32_t)pb[1];
}

#if defined(__x86_64__)
__attribute__((target("avx2")))
static uint32_t sad_256_avx2(const void *a, const void *b) {
  __m256i va = _mm256_loadu_si256((const __m256i *)a);
  __m256i vb = _mm256_loadu_si256((const __m256i *)b);
  __m256i s = _mm256_sad_epu8(va, vb);
  __m128i lo = _mm256_castsi256_si128(s);
  __m128i hi = _mm256_extracti128_si256(s, 1);
  __m128i sum = _mm_add_epi64(lo, hi);
  uint64_t r0 = (uint64_t)_mm_cvtsi128_si64(sum);
  uint64_t r1 = (uint64_t)_mm_cvtsi128_si64(_mm_srli_si128(sum, 8));
  return (uint32_t)(r0 + r1);
}
#endif

uint32_t janus_simd_sad_u8(int lanes, const void *a, const void *b) {
#if defined(__x86_64__)
  uint64_t feat = janus_get_x86_features();
  if (lanes == 32 && (feat & JANUS_X86_AVX2)) {
    return sad_256_avx2(a, b);
  }
  if (lanes == 16) {
    __m128i va = _mm_loadu_si128((const __m128i *)a);
    __m128i vb = _mm_loadu_si128((const __m128i *)b);
    __m128i s = _mm_sad_epu8(va, vb);
    uint64_t r0 = (uint64_t)_mm_cvtsi128_si64(s);
    uint64_t r1 = (uint64_t)_mm_cvtsi128_si64(_mm_srli_si128(s, 8));
    return (uint32_t)(r0 + r1);
  }
#endif
  const uint8_t *pa = (const uint8_t *)a;
  const uint8_t *pb = (const uint8_t *)b;
  uint32_t sum = 0;
  for (int i = 0; i < lanes; ++i) {
    sum += (pa[i] >= pb[i]) ? (uint32_t)(pa[i] - pb[i]) : (uint32_t)(pb[i] - pa[i]);
  }
  return sum;
}

/* -------------------------------------------------------------------------- */
/* Conversions & Saturating Narrowing                                         */
/* -------------------------------------------------------------------------- */

void janus_simd_cvt(int src_elem, int dst_elem, int lanes, void *dst, const void *src) {
  int ssz = elem_size(src_elem);
  int dsz = elem_size(dst_elem);
  const uint8_t *ps = (const uint8_t *)src;
  uint8_t *pd = (uint8_t *)dst;

  for (int i = 0; i < lanes; ++i) {
    double dv = 0.0;
    int64_t sv = 0;
    uint64_t uv = 0;
    int src_is_float = (src_elem == JANUS_SIMD_F32 || src_elem == JANUS_SIMD_F64);
    int src_is_signed = (src_elem == JANUS_SIMD_I8 || src_elem == JANUS_SIMD_I16 ||
                         src_elem == JANUS_SIMD_I32 || src_elem == JANUS_SIMD_I64);
    switch (src_elem) {
      case JANUS_SIMD_I8:  { int8_t  t; memcpy(&t, ps + i * ssz, 1); sv = t; dv = (double)t; break; }
      case JANUS_SIMD_U8:  { uint8_t t; memcpy(&t, ps + i * ssz, 1); uv = t; dv = (double)t; break; }
      case JANUS_SIMD_I16: { int16_t t; memcpy(&t, ps + i * ssz, 2); sv = t; dv = (double)t; break; }
      case JANUS_SIMD_U16: { uint16_t t; memcpy(&t, ps + i * ssz, 2); uv = t; dv = (double)t; break; }
      case JANUS_SIMD_I32: { int32_t t; memcpy(&t, ps + i * ssz, 4); sv = t; dv = (double)t; break; }
      case JANUS_SIMD_U32: { uint32_t t; memcpy(&t, ps + i * ssz, 4); uv = t; dv = (double)t; break; }
      case JANUS_SIMD_I64: { int64_t t; memcpy(&t, ps + i * ssz, 8); sv = t; dv = (double)t; break; }
      case JANUS_SIMD_U64: { uint64_t t; memcpy(&t, ps + i * ssz, 8); uv = t; dv = (double)t; break; }
      case JANUS_SIMD_F32: { float   t; memcpy(&t, ps + i * ssz, 4); dv = (double)t; break; }
      case JANUS_SIMD_F64: { double  t; memcpy(&t, ps + i * ssz, 8); dv = t; break; }
      default: break;
    }

    switch (dst_elem) {
      case JANUS_SIMD_F32: {
        float out = src_is_float ? (float)dv : (src_is_signed ? (float)sv : (float)uv);
        memcpy(pd + i * dsz, &out, 4);
        break;
      }
      case JANUS_SIMD_F64: {
        double out = src_is_float ? dv : (src_is_signed ? (double)sv : (double)uv);
        memcpy(pd + i * dsz, &out, 8);
        break;
      }
      case JANUS_SIMD_I8: {
        int8_t out = src_is_float ? (int8_t)dv : (src_is_signed ? (int8_t)sv : (int8_t)uv);
        memcpy(pd + i * dsz, &out, 1);
        break;
      }
      case JANUS_SIMD_U8: {
        uint8_t out = src_is_float ? (uint8_t)dv : (src_is_signed ? (uint8_t)sv : (uint8_t)uv);
        memcpy(pd + i * dsz, &out, 1);
        break;
      }
      case JANUS_SIMD_I16: {
        int16_t out = src_is_float ? (int16_t)dv : (src_is_signed ? (int16_t)sv : (int16_t)uv);
        memcpy(pd + i * dsz, &out, 2);
        break;
      }
      case JANUS_SIMD_U16: {
        uint16_t out = src_is_float ? (uint16_t)dv : (src_is_signed ? (uint16_t)sv : (uint16_t)uv);
        memcpy(pd + i * dsz, &out, 2);
        break;
      }
      case JANUS_SIMD_I32: {
        int32_t out = src_is_float ? (int32_t)dv : (src_is_signed ? (int32_t)sv : (int32_t)uv);
        memcpy(pd + i * dsz, &out, 4);
        break;
      }
      case JANUS_SIMD_U32: {
        uint32_t out = src_is_float ? (uint32_t)dv : (src_is_signed ? (uint32_t)sv : (uint32_t)uv);
        memcpy(pd + i * dsz, &out, 4);
        break;
      }
      case JANUS_SIMD_I64: {
        int64_t out = src_is_float ? (int64_t)dv : (src_is_signed ? sv : (int64_t)uv);
        memcpy(pd + i * dsz, &out, 8);
        break;
      }
      case JANUS_SIMD_U64: {
        uint64_t out = src_is_float ? (uint64_t)dv : (src_is_signed ? (uint64_t)sv : uv);
        memcpy(pd + i * dsz, &out, 8);
        break;
      }
      default: break;
    }
  }
}

void janus_simd_pack_sat(int src_elem, int dst_elem, int half_lanes, void *dst, const void *lo, const void *hi) {
  int ssz = elem_size(src_elem);
  int dsz = elem_size(dst_elem);
  int dst_bits = dsz * 8;
  int src_is_signed = (src_elem == JANUS_SIMD_I16 || src_elem == JANUS_SIMD_I32 || src_elem == JANUS_SIMD_I64);
  int dst_is_signed = (dst_elem == JANUS_SIMD_I8 || dst_elem == JANUS_SIMD_I16 || dst_elem == JANUS_SIMD_I32);

  int64_t dmin = dst_is_signed ? -(1LL << (dst_bits - 1)) : 0LL;
  int64_t dmax = dst_is_signed ? ((1LL << (dst_bits - 1)) - 1LL) : (int64_t)((1ULL << dst_bits) - 1ULL);
  uint64_t umax = (1ULL << dst_bits) - 1ULL;

  uint8_t *pd = (uint8_t *)dst;
  for (int part = 0; part < 2; ++part) {
    const uint8_t *ps = (const uint8_t *)(part == 0 ? lo : hi);
    for (int i = 0; i < half_lanes; ++i) {
      int out_idx = part * half_lanes + i;
      if (src_is_signed) {
        int64_t sv = 0;
        if (ssz == 2)      { int16_t t; memcpy(&t, ps + i * 2, 2); sv = t; }
        else if (ssz == 4) { int32_t t; memcpy(&t, ps + i * 4, 4); sv = t; }
        else               { int64_t t; memcpy(&t, ps + i * 8, 8); sv = t; }
        if (sv < dmin) sv = dmin;
        if (sv > dmax) sv = dmax;
        uint64_t uout = (uint64_t)sv;
        memcpy(pd + out_idx * dsz, &uout, (size_t)dsz);
      } else {
        uint64_t uv = 0;
        if (ssz == 2)      { uint16_t t; memcpy(&t, ps + i * 2, 2); uv = t; }
        else if (ssz == 4) { uint32_t t; memcpy(&t, ps + i * 4, 4); uv = t; }
        else               { uint64_t t; memcpy(&t, ps + i * 8, 8); uv = t; }
        uint64_t cap = dst_is_signed ? (uint64_t)dmax : umax;
        if (uv > cap) uv = cap;
        memcpy(pd + out_idx * dsz, &uv, (size_t)dsz);
      }
    }
  }
}

/* -------------------------------------------------------------------------- */
/* Memory Load / Store / Masked / Gather                                      */
/* -------------------------------------------------------------------------- */

void janus_simd_load(int bytes, void *dst, const void *ptr) {
  memcpy(dst, ptr, (size_t)bytes);
}

void janus_simd_load_aligned(int bytes, void *dst, const void *ptr) {
  memcpy(dst, ptr, (size_t)bytes);
}

void janus_simd_store(int bytes, void *ptr, const void *src) {
  memcpy(ptr, src, (size_t)bytes);
}

void janus_simd_store_aligned(int bytes, void *ptr, const void *src) {
  memcpy(ptr, src, (size_t)bytes);
}

void janus_simd_load_masked(int elem, int lanes, void *dst, const void *mask, const void *fallback, const void *ptr) {
  int sz = elem_size(elem);
  const uint8_t *pm = (const uint8_t *)mask;
  const uint8_t *pf = (const uint8_t *)fallback;
  const uint8_t *pp = (const uint8_t *)ptr;
  uint8_t *pd = (uint8_t *)dst;

  for (int i = 0; i < lanes; ++i) {
    if (pm[i * sz + (sz - 1)] & 0x80u) {
      memcpy(pd + i * sz, pp + i * sz, (size_t)sz);
    } else {
      memcpy(pd + i * sz, pf + i * sz, (size_t)sz);
    }
  }
}

void janus_simd_store_masked(int elem, int lanes, void *ptr, const void *mask, const void *src) {
  int sz = elem_size(elem);
  const uint8_t *pm = (const uint8_t *)mask;
  const uint8_t *ps = (const uint8_t *)src;
  uint8_t *pp = (uint8_t *)ptr;

  for (int i = 0; i < lanes; ++i) {
    if (pm[i * sz + (sz - 1)] & 0x80u) {
      memcpy(pp + i * sz, ps + i * sz, (size_t)sz);
    }
  }
}

#if defined(__x86_64__)
__attribute__((target("avx2")))
static int gather_avx2(int elem, int lanes, void *dst, const void *base_ptr, const void *indices_i32) {
  if (elem == JANUS_SIMD_F32 && lanes == 8) {
    __m256i idx = _mm256_loadu_si256((const __m256i *)indices_i32);
    _mm256_storeu_ps((float *)dst, _mm256_i32gather_ps((const float *)base_ptr, idx, 4));
    return 1;
  }
  if ((elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_U32) && lanes == 8) {
    __m256i idx = _mm256_loadu_si256((const __m256i *)indices_i32);
    _mm256_storeu_si256((__m256i *)dst, _mm256_i32gather_epi32((const int *)base_ptr, idx, 4));
    return 1;
  }
  if (elem == JANUS_SIMD_F32 && lanes == 4) {
    __m128i idx = _mm_loadu_si128((const __m128i *)indices_i32);
    _mm_storeu_ps((float *)dst, _mm_i32gather_ps((const float *)base_ptr, idx, 4));
    return 1;
  }
  if ((elem == JANUS_SIMD_I32 || elem == JANUS_SIMD_U32) && lanes == 4) {
    __m128i idx = _mm_loadu_si128((const __m128i *)indices_i32);
    _mm_storeu_si128((__m128i *)dst, _mm_i32gather_epi32((const int *)base_ptr, idx, 4));
    return 1;
  }
  return 0;
}
#endif

void janus_simd_gather(int elem, int lanes, void *dst, const void *base_ptr, const void *indices_i32) {
#if defined(__x86_64__)
  if ((janus_get_x86_features() & JANUS_X86_AVX2) && gather_avx2(elem, lanes, dst, base_ptr, indices_i32)) {
    return;
  }
#endif
  int sz = elem_size(elem);
  const uint8_t *pb = (const uint8_t *)base_ptr;
  const int32_t *pi = (const int32_t *)indices_i32;
  uint8_t *pd = (uint8_t *)dst;
  for (int i = 0; i < lanes; ++i) {
    int32_t idx;
    memcpy(&idx, pi + i, 4);
    memcpy(pd + i * sz, pb + (int64_t)idx * sz, (size_t)sz);
  }
}

void janus_simd_gather_masked(int elem, int lanes, void *dst, const void *mask, const void *fallback, const void *base_ptr, const void *indices_i32) {
  int sz = elem_size(elem);
  const uint8_t *pm = (const uint8_t *)mask;
  const uint8_t *pf = (const uint8_t *)fallback;
  const uint8_t *pb = (const uint8_t *)base_ptr;
  const int32_t *pi = (const int32_t *)indices_i32;
  uint8_t *pd = (uint8_t *)dst;
  for (int i = 0; i < lanes; ++i) {
    if (pm[i * sz + (sz - 1)] & 0x80u) {
      int32_t idx;
      memcpy(&idx, pi + i, 4);
      memcpy(pd + i * sz, pb + (int64_t)idx * sz, (size_t)sz);
    } else {
      memcpy(pd + i * sz, pf + i * sz, (size_t)sz);
    }
  }
}
