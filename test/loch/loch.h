/*
 * loch.h
 * provides main facilities for scheduling. namely, yield()
 */

#pragma once
#ifndef LOCH_H
#define LOCH_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include "sched.h"
#include "tcb.h"

// each thread should have a _Thread_local object here
typedef struct thread_state {
  tcb_t *current_context;
  tcb_t *next_context;
  ucontext_t wait_ctx;
  uint64_t thread_id;

} thread_state_t;

// yield must provide the gc parameters to the scheduler so
// we can set it
// this MUST be called from loch (obviously)
void yield(uint64_t *rbp, uint64_t *rsp);

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

void check_for_work();

#endif
