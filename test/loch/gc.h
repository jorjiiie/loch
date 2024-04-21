#pragma once
#ifndef GC_H
#define GC_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include <pthread.h>
#include <stdatomic.h>
#include <stdint.h>

#include "tcb.h"

typedef struct map map_t;
typedef struct set set_t;

typedef struct gc_state {
  // normal gc info
  uint64_t *heap_start;
  uint64_t *heap_end;
  uint64_t *heap_ptr;
  uint64_t HEAP_SIZE;

  // concurrency info
  pthread_mutex_t lock;
  _Atomic uint64_t active_threads;
  _Atomic uint64_t gc_ack;
  _Atomic uint8_t gc_flag;

  // map of all the threads to walk through, also to free
  // when we're done.
  map_t *map;
  set_t *seen_threads;

} gc_t;

gc_t *gc_init(uint64_t heap_size);

void print_heap(uint64_t *start, uint64_t *end);

uint64_t *copy_if_needed(uint64_t *addr, uint64_t *heap);

// gc a single tcb
uint64_t *gc_tcb(tcb_t *tcb, uint64_t *from_start, uint64_t *to_start,
                 uint64_t *to_end);

// needs to gc all the tcbs
uint64_t *gc(uint64_t *from_start, uint64_t *to_start, uint64_t *to_end);

// basically just malloc
// actually reserves memory, unlike in the assignments
uint64_t *reserve(uint64_t wanted, uint64_t *rsp, uint64_t *rbp);

#endif
