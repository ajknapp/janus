#include <stdio.h>
#include <x86gprintrin.h>

unsigned long long janus_tic()
{
  unsigned long long start_time;
  asm volatile ("rdtsc\n\t"         // Memory barrier to inhibit speculation
                "lfence\n\t"          // Returns the time in EDX:EAX.
                "shl $32, %%rdx\n\t" // Shift the upper bits left.
                "or %%rdx, %0"       // 'Or' in the lower bits.
                : "=a" (start_time)
                :
                : "rdx");
  return start_time;
}

unsigned long long janus_toc()
{
  unsigned long long stop_time;
  asm volatile ("rdtscp\n\t"          // Returns the time in EDX:EAX.
                "shl $32, %%rdx\n\t"  // Shift the upper bits left.
                "or %%rdx, %0"        // 'Or' in the lower bits.
                : "=a" (stop_time)
                :
                : "rdx");
  return stop_time;
}

FILE* janus_stdout()
{
  return stdout;
}

FILE* janus_stderr()
{
  return stderr;
}
