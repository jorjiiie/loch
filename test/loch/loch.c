#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE

#include "loch.h"
#include "sched.h"

// pass it as a compile flag -DDEBUG_LOG
#include "debug.h"

#include <unistd.h>

extern _Atomic uint64_t active_threads;
extern _Atomic uint64_t gc_ack;

_Thread_local thread_state_t state;
_Atomic uint64_t gc_flag = 0;

sched_t *scheduler;

_Atomic uint64_t thread_id;
// maybe have a big ass table of tcb_t (loch_int -> tcb_t)

// maybe this should return R15
void yield(uint64_t *rbp, uint64_t *rsp) {

  state.current_context->frame_bottom = rbp;
  state.current_context->frame_top = rsp;

  atomic_store(&state.current_context->state, NOT_RUNNING);

  // for thread to GC, we must have THREADS - 1 ack's
  // although this MUST work for the number of running threads!
  // check if GC is necessary
  atomic_fetch_add(&gc_ack, 1);
  while (atomic_load(&gc_flag) == 1) {
    usleep(10);
  }
  atomic_fetch_sub(&gc_ack, 1);
  state.next_context = sched_next(scheduler);

  // enter waiting state
  if (state.next_context == NULL) {
    // maybe not do this (depends on how you go in and out)
    // atomic_fetch_sub(&active_threads, 1);
    state.current_context = NULL;
    setcontext(&state.wait_ctx);
  }

  swapcontext(&state.current_context->ctx, &state.next_context->ctx);
  // we are now in state.next_context
  atomic_store(&state.current_context->state, NOT_RUNNING);
  atomic_store(&state.next_context->state, RUNNING);

  // AFTER we switch contexts!
  sched_enqueue(scheduler, state.current_context);

  state.current_context = state.next_context;
  state.next_context = NULL;
}

uint64_t spawn_thread(uint64_t closure) {
  uint64_t tid = atomic_fetch_add(&thread_id, 1);
  tcb_t *new_tcb = tcb_create(closure);
  // add tcb stuff around
  // TODO: fix this up for all the tracking stuff

  return tid;
}

uint64_t loch_get(uint64_t loch_tcb) {
  // TODO: get the TCB somehow. I don't want to throw pointers around.
  tcb_t *tcb;
  while (atomic_load(&tcb->state) != FINISHED) {
    schedule();
  }
  return tcb->result;
}

void check_for_work() {
  while (1) {
    tcb_t *tcb = sched_next(scheduler);
    if (tcb == NULL)
      usleep(10);
    else {
      atomic_store(&tcb->state, RUNNING);
      state.current_context = tcb;

      atomic_fetch_add(&active_threads, 1);
      swapcontext(&state.wait_ctx, &tcb->ctx);
      atomic_fetch_sub(&active_threads, 1);
    }
  }
}

void thread_func() {}

#endif
