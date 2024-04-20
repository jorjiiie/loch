#pragma once
#ifndef GC_H
#define GC_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include <stdint.h>

#include "tcb.h"

typedef struct gc_state {
  // needs a list of all the tcbs?
  // needs the count of threads

} gc_t;

typedef struct loch_state {
  gc_t *gc_state;

} loch_t;

void print_heap(uint64_t *start, uint64_t *end);

uint64_t *copy_if_needed(uint64_t *addr, uint64_t *heap);

// gc a single tcb
uint64_t *gc_tcb(tcb_t *tcb, uint64_t *from_start, uint64_t *to_start,
                 uint64_t *to_end);

// needs to gc all the tcbs
uint64_t *gc(uint64_t *from_start, uint64_t *to_start, uint64_t *to_end);

#endif
