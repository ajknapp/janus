#ifndef JANUS_ABT_H
#define JANUS_ABT_H

#include <stdint.h>

typedef void (*janus_abt_worker_fn)(int32_t worker_id, int64_t chunk_lo, int64_t chunk_hi, void *ctx);

int janus_abt_init(int num_xstreams);
void janus_abt_finalize(void);
int janus_abt_set_num_xstreams(int num_xstreams);
int janus_abt_num_xstreams(void);
void janus_abt_parallel_for(int64_t lo, int64_t hi, int64_t min_grain, void *ctx, janus_abt_worker_fn worker);
int janus_abt_query_libdir(char *buf, int cap);

#endif /* JANUS_ABT_H */
