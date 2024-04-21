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

// yield must provide the gc parameters to the scheduler so
// we can set it
// this MUST be called from loch (obviously)
void loch_yield(uint64_t *rbp, uint64_t *rsp);

// yield from inside the runtime (main yield)
void runtime_yield();

// schedules the next thread - this may need some care lol
// called from C for various reasons (blocking sleep/get)
void schedule();

// spawns a closure
// returns the LOCH value
uint64_t spawn_thread(uint64_t closure);

// takes the loch_tcb to get the value
// returns the value
// if TCB is not ready, we block (reschedule)
uint64_t loch_get(uint64_t loch_tcb);

// the infinite loop of checking for work ()
void check_for_work();

void thread_func();

void init_threads();

#endif
