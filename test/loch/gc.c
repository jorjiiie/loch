#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#include <complex.h>
#include <stdatomic.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "gc.h"
#include "loch.h"
#include "map.h"
#include "set.h"

// constants
extern uint64_t NUM_TAG_MASK;
extern uint64_t CLOSURE_TAG_MASK;
extern uint64_t TUPLE_TAG_MASK;
extern uint64_t FORWARDING_TAG_MASK;
extern uint64_t THREAD_TAG_MASK;
extern uint64_t LOCK_TAG_MASK;
extern uint64_t CLOSURE_TAG;
extern uint64_t TUPLE_TAG;
extern uint64_t FORWARDING_TAG;
extern uint64_t THREAD_TAG;
extern uint64_t LOCK_TAG;
extern uint64_t NIL;
extern uint64_t tupleCounter;
extern uint64_t *STACK_BOTTOM;
extern uint64_t *FROM_S;
extern uint64_t *FROM_E;
extern uint64_t *TO_S;
extern uint64_t *TO_E;

// state stuff
extern gc_t *gc_state;
extern _Thread_local thread_state_t state;

gc_t *gc_init(uint64_t heap_size) {
  gc_t *gc = malloc(sizeof(gc_t));
  gc->HEAP_SIZE = heap_size;
  gc->heap_start = mmap(NULL, heap_size, PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (gc->heap_start == MAP_FAILED) {
    perror("mmap");
    exit(1);
  }
  gc->heap_end = gc->heap_start + heap_size;
  gc->heap_ptr = gc->heap_start;

  pthread_mutex_init(&gc->lock, NULL);

  gc->active_threads = 0;
  gc->gc_ack = 0;

  gc->gc_flag = 0;

  gc->map = map_create();
  gc->seen_threads = set_create();

  return gc;
}
uint64_t *copy_if_needed(uint64_t *addr, uint64_t *heap) {

  uint64_t v = *addr;
  if ((v & TUPLE_TAG_MASK) == TUPLE_TAG) {
    // heap alloc'd, realloc
    uint64_t *ptr = (uint64_t *)(v - TUPLE_TAG);
    if (((uint64_t)ptr | TUPLE_TAG) == NIL) {
      return heap;
    }
    if ((ptr[0] & FORWARDING_TAG_MASK) == FORWARDING_TAG) {
      *addr = ptr[0] - FORWARDING_TAG + TUPLE_TAG;
      return heap;
    }

    uint64_t size = *ptr / 2;
    uint64_t *new_ptr = heap;
    // need padding too
    heap += size + 1;
    if ((size + 1) & 1)
      heap++;
    // copy all the insides too.
    memcpy(new_ptr, ptr, (size + 1) * 8);
    ptr[0] = (uint64_t)(new_ptr) + FORWARDING_TAG;

    for (int i = 0; i < size; i++) {
      heap = copy_if_needed(new_ptr + i + 1, heap);
    }
    *addr = (uint64_t)new_ptr + TUPLE_TAG;
  } else if ((v & CLOSURE_TAG_MASK) == CLOSURE_TAG) {
    uint64_t *ptr = (uint64_t *)(v - CLOSURE_TAG);
    if ((ptr[0] & FORWARDING_TAG_MASK) == FORWARDING_TAG) {
      *addr = ptr[0] - FORWARDING_TAG + CLOSURE_TAG;
      return heap;
    }

    uint64_t size = ptr[2];
    uint64_t *new_ptr = heap;
    heap += size + 3;
    if ((size + 3) & 1)
      heap++;
    memcpy(new_ptr, ptr, (size + 3) * 8);

    // put forwarding tag in
    ptr[0] = (uint64_t)(new_ptr) + FORWARDING_TAG;

    for (int i = 0; i < size; i++) {
      heap = copy_if_needed(new_ptr + i + 3, heap);
    }
    *addr = (uint64_t)(new_ptr) + CLOSURE_TAG;
  } else if ((v & LOCK_TAG_MASK) == LOCK_TAG) {
    uint64_t *ptr = (uint64_t *)(v - CLOSURE_TAG);
    if ((ptr[0] & FORWARDING_TAG_MASK) == FORWARDING_TAG) {
      *addr = ptr[0] - FORWARDING_TAG + CLOSURE_TAG;
      return heap;
    }
    uint64_t *new_ptr = heap;
    heap += 2;
    new_ptr[0] = ptr[0]; // 0 or 1 LOL
    ptr[0] = (uint64_t)(new_ptr) + FORWARDING_TAG;

    *addr = (uint64_t)(new_ptr) + LOCK_TAG;
  } else if ((v & THREAD_TAG_MASK) == THREAD_TAG) {
    // no gc needed except to say that we have seen these buddies
    uint64_t tid = v - THREAD_TAG;
    set_insert(gc_state->seen_threads, tid);
  }

  return heap;
}

// gc a single tcb
uint64_t *gc_tcb(tcb_t *tcb, uint64_t *from_start, uint64_t *to_start,
                 uint64_t *to_end) {

  uint64_t *old_rsp = tcb->frame_top;
  uint64_t *top_rsp = tcb->frame_top;
  uint64_t *top_rbp = tcb->frame_bottom;
  do {
    for (uint64_t *cur_word = top_rsp; cur_word < top_rbp; cur_word++) {
      to_start = copy_if_needed(cur_word, to_start);
    }
    top_rsp = top_rbp + 2;
    old_rsp = top_rbp;
    top_rbp = (uint64_t *)*top_rbp;
  } while (old_rsp < tcb->stack_bottom);

  return to_start;
}
uint64_t *gc(uint64_t *from_heap, uint64_t *to_start, uint64_t *to_end) {
  uint64_t *heap_ptr = to_start;
  for (int i = 0; i < MAP_SIZE; i++) {
    kv_t *c = gc_state->map->table[i];
    while (c != NULL) {
      heap_ptr = gc_tcb(c->value, from_heap, heap_ptr, to_end);
      c = c->next;
    }
  }
  return heap_ptr;
}

uint64_t *reserve(uint64_t wanted, uint64_t *rsp, uint64_t *rbp) {

  state.current_context->frame_top = rsp;
  state.current_context->frame_bottom = rbp;

  gc_state->gc_ack++;
  if (pthread_mutex_lock(&gc_state->lock)) {
    perror("pthread_mutex_lock");
    exit(1);
  }
  // alloc [heap_ptr, heap_ptr + wanted)
  if (gc_state->heap_ptr + wanted > gc_state->heap_end) {
    // gc
    atomic_store(&gc_state->gc_flag, 1);

    // wait until all threads are in a safe state
    while (atomic_load(&gc_state->gc_ack) !=
           atomic_load(&gc_state->active_threads)) {
      usleep(10);
    }
    // all threads are waiting, go GC

    set_clear(gc_state->seen_threads);
    uint64_t *new_heap = mmap(NULL, gc_state->HEAP_SIZE, PROT_READ | PROT_WRITE,
                              MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    uint64_t *new_ptr =
        gc(gc_state->heap_start, new_heap, new_heap + gc_state->HEAP_SIZE);

    munmap(gc_state->heap_start, gc_state->HEAP_SIZE);

    // i am not going to both clearing everyone out
    if (new_ptr + wanted > (new_heap + gc_state->HEAP_SIZE)) {
      fprintf(stderr, "gc failed to allocate enough memory\n");
      exit(1);
    }
    // thread cleanup

    gc_state->heap_start = new_heap;
    gc_state->heap_ptr = new_ptr;
    gc_state->heap_end = new_heap + gc_state->HEAP_SIZE;

    gc_state->heap_ptr += wanted;

    atomic_store(&gc_state->gc_flag, 0);

    atomic_fetch_sub(&gc_state->gc_ack, 1);
    if (pthread_mutex_unlock(&gc_state->lock)) {
      perror("pthread_mutex_unlock");
      exit(1);
    }

    return new_ptr;
  }
  uint64_t *ret = gc_state->heap_ptr;
  gc_state->heap_ptr += wanted;

  atomic_fetch_sub(&gc_state->gc_ack, 1);
  if (pthread_mutex_unlock(&gc_state->lock)) {
    perror("pthread_mutex_unlock");
    exit(1);
  }
  return ret;
}
