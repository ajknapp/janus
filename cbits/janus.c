#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <x86gprintrin.h>

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
