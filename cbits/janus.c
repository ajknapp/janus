#include <cuda.h>
#include <malloc.h>
#include <nvJitLink.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <x86gprintrin.h>

#define JANUS_SIMD_ALIGNMENT 64u

void* janus_aligned_malloc(size_t size)
{
  size_t rounded = size == 0 ? JANUS_SIMD_ALIGNMENT
                             : ((size + (JANUS_SIMD_ALIGNMENT - 1u)) & ~(size_t)(JANUS_SIMD_ALIGNMENT - 1u));
  return aligned_alloc(JANUS_SIMD_ALIGNMENT, rounded);
}

void* janus_aligned_alloc(size_t alignment, size_t size)
{
  size_t eff_al = alignment < JANUS_SIMD_ALIGNMENT ? JANUS_SIMD_ALIGNMENT : alignment;
  size_t rounded = size == 0 ? eff_al
                             : ((size + (eff_al - 1u)) & ~(eff_al - 1u));
  return aligned_alloc(eff_al, rounded);
}

void* janus_aligned_calloc(size_t nmemb, size_t size)
{
  size_t total = nmemb * size;
  void* p = janus_aligned_malloc(total);
  if (p != NULL && total > 0) {
    memset(p, 0, total);
  }
  return p;
}

void* janus_aligned_realloc(void* ptr, size_t size)
{
  if (ptr == NULL) {
    return janus_aligned_malloc(size);
  }
  if (size == 0) {
    free(ptr);
    return NULL;
  }
  size_t old_usable = malloc_usable_size(ptr);
  void* np = janus_aligned_malloc(size);
  if (np != NULL) {
    size_t copy_bytes = old_usable < size ? old_usable : size;
    memcpy(np, ptr, copy_bytes);
    free(ptr);
  }
  return np;
}

void janus_aligned_free(void* ptr)
{
  free(ptr);
}

#if defined(__x86_64__)
uint64_t janus_tic()
{
  uint64_t start_time;
  asm volatile ("rdtsc\n\t"
                "lfence\n\t"
                "shl $32, %%rdx\n\t"
                "or %%rdx, %0"
                : "=a" (start_time)
                :
                : "rdx");
  return start_time;
}

uint64_t janus_toc()
{
  uint64_t stop_time;
  asm volatile ("rdtscp\n\t"
                "shl $32, %%rdx\n\t"
                "or %%rdx, %0"
                : "=a" (stop_time)
                :
                : "rdx");
  return stop_time;
}
#elif defined(__aarch64__)
uint64_t janus_tic()
{
  struct timespec res;
  clock_gettime(CLOCK_MONOTONIC, &res);
  return ((uint64_t) res.tv_sec)*1000000000 + ((uint64_t) res.tv_nsec);
}

uint64_t janus_toc()
{
  struct timespec res;
  clock_gettime(CLOCK_MONOTONIC, &res);
  return ((uint64_t) res.tv_sec)*1000000000 + ((uint64_t) res.tv_nsec);
}
#endif

FILE* janus_stdout()
{
  return stdout;
}

FILE* janus_stderr()
{
  return stderr;
}
