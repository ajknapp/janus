#define _GNU_SOURCE
#include "janus_abt.h"
#include <abt.h>
#include <dlfcn.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define JANUS_ABT_MAX_XSTREAMS 128

typedef struct {
  int32_t worker_id;
  int64_t lo;
  int64_t hi;
  void *ctx;
  janus_abt_worker_fn worker;
} janus_abt_chunk_arg;

static pthread_mutex_t g_abt_mutex = PTHREAD_MUTEX_INITIALIZER;
static int g_abt_initialized = 0;
static int g_num_xstreams = 1;
static ABT_xstream g_xstreams[JANUS_ABT_MAX_XSTREAMS];
static ABT_pool g_pools[JANUS_ABT_MAX_XSTREAMS];
static ABT_sched g_scheds[JANUS_ABT_MAX_XSTREAMS];

static int clamp_xstreams(int n) {
  if (n <= 0) {
    long procs = sysconf(_SC_NPROCESSORS_ONLN);
    n = (procs > 0) ? (int)procs : 1;
  }
  if (n < 1) n = 1;
  if (n > JANUS_ABT_MAX_XSTREAMS) n = JANUS_ABT_MAX_XSTREAMS;
  return n;
}

static void janus_abt_trampoline(void *raw) {
  janus_abt_chunk_arg *arg = (janus_abt_chunk_arg *)raw;
  arg->worker(arg->worker_id, arg->lo, arg->hi, arg->ctx);
}

static void teardown_xstreams_locked(void) {
  if (!g_abt_initialized) {
    return;
  }
  int n = g_num_xstreams;
  for (int i = 1; i < n; ++i) {
    if (g_xstreams[i] != ABT_XSTREAM_NULL) {
      ABT_xstream_join(g_xstreams[i]);
      ABT_xstream_free(&g_xstreams[i]);
    }
  }
  /* Reset primary xstream back to a default scheduler before freeing custom scheds/pools
     only if we are tearing down completely; ABT_finalize will release primary ES. */
  for (int i = 0; i < n; ++i) {
    g_xstreams[i] = ABT_XSTREAM_NULL;
    g_scheds[i] = ABT_SCHED_NULL;
    g_pools[i] = ABT_POOL_NULL;
  }
  ABT_finalize();
  __atomic_store_n(&g_num_xstreams, 1, __ATOMIC_RELEASE);
  __atomic_store_n(&g_abt_initialized, 0, __ATOMIC_RELEASE);
}

static int setup_xstreams_locked(int requested) {
  int n = clamp_xstreams(requested);
  if (ABT_init(0, NULL) != ABT_SUCCESS) {
    return -1;
  }
  for (int i = 0; i < n; ++i) {
    g_xstreams[i] = ABT_XSTREAM_NULL;
    g_pools[i] = ABT_POOL_NULL;
    g_scheds[i] = ABT_SCHED_NULL;
  }
  for (int i = 0; i < n; ++i) {
    /* automatic = ABT_TRUE so schedulers/finalize cleanly reclaim pools */
    if (ABT_pool_create_basic(ABT_POOL_FIFO, ABT_POOL_ACCESS_MPMC, ABT_TRUE, &g_pools[i]) != ABT_SUCCESS) {
      ABT_finalize();
      return -1;
    }
  }
  /* Each execution stream runs a work-stealing BASIC_WAIT scheduler over all pools,
     prioritizing its own local pool first. */
  ABT_pool sched_pools[JANUS_ABT_MAX_XSTREAMS];
  for (int i = 0; i < n; ++i) {
    for (int k = 0; k < n; ++k) {
      sched_pools[k] = g_pools[(i + k) % n];
    }
    if (ABT_sched_create_basic(ABT_SCHED_BASIC_WAIT, n, sched_pools, ABT_SCHED_CONFIG_NULL, &g_scheds[i]) != ABT_SUCCESS) {
      ABT_finalize();
      return -1;
    }
  }
  if (ABT_xstream_self(&g_xstreams[0]) != ABT_SUCCESS) {
    ABT_finalize();
    return -1;
  }
  if (ABT_xstream_set_main_sched(g_xstreams[0], g_scheds[0]) != ABT_SUCCESS) {
    ABT_finalize();
    return -1;
  }
  for (int i = 1; i < n; ++i) {
    if (ABT_xstream_create(g_scheds[i], &g_xstreams[i]) != ABT_SUCCESS) {
      for (int j = 1; j < i; ++j) {
        ABT_xstream_join(g_xstreams[j]);
        ABT_xstream_free(&g_xstreams[j]);
      }
      ABT_finalize();
      return -1;
    }
  }
  __atomic_store_n(&g_num_xstreams, n, __ATOMIC_RELEASE);
  __atomic_store_n(&g_abt_initialized, 1, __ATOMIC_RELEASE);
  return n;
}

int janus_abt_init(int num_xstreams) {
  pthread_mutex_lock(&g_abt_mutex);
  int res;
  if (g_abt_initialized) {
    res = g_num_xstreams;
  } else {
    res = setup_xstreams_locked(num_xstreams);
  }
  pthread_mutex_unlock(&g_abt_mutex);
  return res;
}

void janus_abt_finalize(void) {
  pthread_mutex_lock(&g_abt_mutex);
  teardown_xstreams_locked();
  pthread_mutex_unlock(&g_abt_mutex);
}

int janus_abt_set_num_xstreams(int num_xstreams) {
  int target = clamp_xstreams(num_xstreams);
  pthread_mutex_lock(&g_abt_mutex);
  if (g_abt_initialized && g_num_xstreams == target) {
    pthread_mutex_unlock(&g_abt_mutex);
    return target;
  }
  if (g_abt_initialized) {
    teardown_xstreams_locked();
  }
  int res = setup_xstreams_locked(target);
  pthread_mutex_unlock(&g_abt_mutex);
  return res;
}

int janus_abt_num_xstreams(void) {
  if (!__atomic_load_n(&g_abt_initialized, __ATOMIC_ACQUIRE)) {
    janus_abt_init(0);
  }
  return __atomic_load_n(&g_num_xstreams, __ATOMIC_ACQUIRE);
}

void janus_abt_parallel_for(
    int64_t lo,
    int64_t hi,
    int64_t min_grain,
    void *ctx,
    janus_abt_worker_fn worker) {
  if (lo >= hi || worker == NULL) {
    return;
  }
  if (!__atomic_load_n(&g_abt_initialized, __ATOMIC_ACQUIRE)) {
    if (janus_abt_init(0) <= 0) {
      worker(0, lo, hi, ctx);
      return;
    }
  }
  int64_t total = hi - lo;
  int64_t grain = (min_grain > 0) ? min_grain : 1;
  int n_es = __atomic_load_n(&g_num_xstreams, __ATOMIC_ACQUIRE);
  if (n_es <= 1 || total <= grain) {
    worker(0, lo, hi, ctx);
    return;
  }
  int64_t max_chunks64 = (total + grain - 1) / grain;
  int n_chunks = (max_chunks64 < (int64_t)n_es) ? (int)max_chunks64 : n_es;
  if (n_chunks <= 1) {
    worker(0, lo, hi, ctx);
    return;
  }

  janus_abt_chunk_arg args[JANUS_ABT_MAX_XSTREAMS];
  ABT_thread threads[JANUS_ABT_MAX_XSTREAMS];

  int64_t base_sz = total / n_chunks;
  int64_t rem = total % n_chunks;
  int64_t cur = lo;
  for (int c = 0; c < n_chunks; ++c) {
    int64_t sz = base_sz + (c < rem ? 1 : 0);
    args[c].worker_id = (int32_t)c;
    args[c].lo = cur;
    args[c].hi = cur + sz;
    args[c].ctx = ctx;
    args[c].worker = worker;
    threads[c] = ABT_THREAD_NULL;
    cur += sz;
  }

  /* Detect whether the caller is already executing inside an Argobots ES.
     - If inside an ES: dispatch chunks 1..n_chunks-1 round-robin across all ES pools,
       run chunk 0 locally, and cooperatively join via ABT_thread_free (nested-safe!).
     - If called from an external OS thread (e.g. a GHC RTS worker): dispatch chunks
       1..n_chunks-1 onto active secondary ES pools (1..n_es-1), run chunk 0 directly
       on the calling OS thread, and join all spawned ULTs. */
  int self_rank = 0;
  int in_es = (ABT_self_get_xstream_rank(&self_rank) == ABT_SUCCESS);

  for (int c = 1; c < n_chunks; ++c) {
    int pool_idx = in_es ? ((self_rank + c) % n_es) : (1 + ((c - 1) % (n_es - 1)));
    int rc = ABT_thread_create(
        g_pools[pool_idx],
        janus_abt_trampoline,
        &args[c],
        ABT_THREAD_ATTR_NULL,
        &threads[c]);
    if (rc != ABT_SUCCESS) {
      /* Fallback: execute inline on failure */
      threads[c] = ABT_THREAD_NULL;
      worker(args[c].worker_id, args[c].lo, args[c].hi, ctx);
    }
  }

  /* Execute chunk 0 on the calling thread / ULT */
  worker(args[0].worker_id, args[0].lo, args[0].hi, ctx);

  /* Join and release all spawned ULTs */
  for (int c = 1; c < n_chunks; ++c) {
    if (threads[c] != ABT_THREAD_NULL) {
      ABT_thread_free(&threads[c]);
    }
  }
}

int janus_abt_query_libdir(char *buf, int cap) {
  if (!buf || cap <= 1) return 0;
  const void *sym = dlsym(RTLD_DEFAULT, "ABT_init");
  if (!sym) {
    sym = (const void *)&ABT_init;
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
