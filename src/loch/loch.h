/*
 * loch.h
 * provides main facilities for scheduling. namely, yield()
 */

#pragma once
#ifndef LOCH_H
#define LOCH_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

// number of system threads to utilize (excluding main thread)
#define LOCH_THREADS 2

#include <stdint.h>
#include <ucontext.h>
typedef struct tcb tcb_t;

// each thread should have a _Thread_local of these
typedef struct thread_state {
  tcb_t *current_context;
  tcb_t *next_context;
  ucontext_t wait_ctx;
  uint64_t thread_id;

} thread_state_t;

// calls into threads_start_here function with the heap pointer,
// and this as args
void tcb_runner(uint32_t tcb_high, uint32_t tcb_low, uint32_t closure_high,
                uint32_t closure_low);

// sets the stack bottom of the tcb
// first thing called by thread_code_starts_here
uint64_t tcb_set_stack_bottom(uint64_t *stack_bottom);

// yield from inside the runtime (main yield)
void runtime_yield();


// the infinite loop of checking for work ()
void check_for_work();

// make sure you're being consistent here
// basically every time we can switch contexts, we must be ready to gc
uint64_t _loch_yield(uint64_t *rbp, uint64_t *rsp);

// creates a new thread that will call closure
uint64_t _loch_thread_create(uint64_t closure);

// takes in a untagged threadid. returns the value of the completed computation.
// we provide rbp and rsp as this blocks
uint64_t _loch_thread_get(uint64_t thread, uint64_t *rbp, uint64_t *rsp);

// starts a thread, although it may not start immediately
uint64_t _loch_thread_start(uint64_t thread);

// obvious
uint64_t _lock_set_stack(uint64_t *bottom);

#endif
