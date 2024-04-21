#pragma once
#ifndef GC_H
#define GC_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include <stdint.h>
#include <pthread.h>
#include <stdatomic.h>

#include "tcb.h"
#include "map.h"

typedef struct gc_state {
    pthread_mutex_t lock;
    uint64_t *heap_start;
    uint64_t *heap_end;
    uint64_t *heap_ptr;
    uint64_t HEAP_SIZE;

    // contains ALL threads
    map_t *map;
    _Atomic uint64_t active_threads;
    _Atomic uint64_t gc_ack;
    atomic_flag gc_flag;
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
uint64_t *reserve(uint64_t wanted, uint64_t *rsp, uint64_t* rbp);

#endif
