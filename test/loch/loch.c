/*
 * loch.c
 * implementation of the threading runtime
 */
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include "loch.h"
#include "sched.h"
#include "tcb.h"

// pass it as a compile flag -DDEBUG_LOG
#include "debug.h"

#include <unistd.h>

/*
 * Runtime stuff
 */
extern uint64_t *heap_ptr;
extern uint64_t *heap_end;
extern uint64_t HEAP_SIZE;
extern _Atomic uint64_t gc_ack; // TODO: MAKE SURE THESE ARE INITIALIZED
extern _Atomic uint64_t gc_flag;

extern uint64_t
thread_code_starts_here(uint64_t *heap, uint64_t sz,
                        uint64_t closure) asm("thread_code_starts_here");

extern uint64_t do_something(uint64_t closure);

/*
 * threading stuff
 */
extern _Atomic uint64_t active_threads;

extern sched_t *scheduler;

extern _Atomic uint64_t thread_id;
extern _Thread_local thread_state_t state;
// table_t

void tcb_runner(uint32_t tcb_high, uint32_t tcb_low, uint32_t closure_high,
                uint32_t closure_low) {
  tcb_t *tcb = (tcb_t *)((uint64_t)tcb_high << 32 | tcb_low);
  uint64_t arg = (uint64_t)closure_high << 32 | closure_low;
  // we have this block because when we swap to a fresh context, it won't run
  // this important code! bookkeeping 101
  if (state.next_context != NULL) {
    // call from yield, must remember to reset the old stuffs
    atomic_store(&state.next_context->state, RUNNING);
    atomic_store(&state.current_context->state, NOT_RUNNING);

    sched_enqueue(scheduler, state.current_context);

    state.current_context = state.next_context;
    state.next_context = NULL;
  }
  printf("lol tcb? %p\n", tcb);
  // uint64_t ret = thread_code_starts_here(heap_ptr, HEAP_SIZE, arg);
  uint64_t ret = do_something(arg);
  tcb->result = ret;
  atomic_store(&tcb->state, FINISHED);
  swapcontext(&tcb->ctx, &state.wait_ctx);
}

uint64_t tcb_set_stack_bottom(uint64_t *stack_bottom) {
  assert(state.current_context != NULL);
  state.current_context->stack_bottom = stack_bottom;
  return 0;
}

// i think this all should be in loch

// maybe this should return R15 no. reserve is a C call that RESERVES and
// returns the pointer such [p,p+s) is reserved (reimplemented malloc)
void loch_yield(uint64_t *rbp, uint64_t *rsp) {
  printd("yield from loch! %p %p", rbp, rsp);

  state.current_context->frame_bottom = rbp;
  state.current_context->frame_top = rsp;

  // for thread to GC, we must have THREADS - 1 ack's
  // although this MUST work for the number of running threads!
  // check if GC is necessary
  // we do we yield in here? because why not?
  // it doesn't matter.
  atomic_fetch_add(&gc_ack, 1);
  while (atomic_load(&gc_flag) == 1) {
    usleep(10);
  }
  atomic_fetch_sub(&gc_ack, 1);
  runtime_yield();
}
void runtime_yield() {
  state.next_context = sched_next(scheduler);

  // no other threads, just continue
  if (state.next_context == NULL)
    return;

  printd("BEFORE STATE %p", &state);
  // swap to the other thread
  swapcontext(&state.current_context->ctx, &state.next_context->ctx);
  printd("AFTER STATE %p", &state);
  // we are now in state.next_context
  atomic_store(&state.current_context->state, NOT_RUNNING);
  atomic_store(&state.next_context->state, RUNNING);

  // AFTER we switch contexts! my face when multithreading
  // i think we should put this before the swap LOL
  // wait holy hell - needs to be a locked operation
  sched_enqueue(scheduler, state.current_context);

  state.current_context = state.next_context;
  state.next_context = NULL;
}

uint64_t spawn_thread(uint64_t closure) {
  uint64_t tid = atomic_fetch_add(&thread_id, 1);
  tcb_t *new_tcb = tcb_create(closure);
  // add tcb stuff around
  // TODO: fix this up for all the tracking stuff

  // tag with thread tag
  return tid << 4 | 0x13;
}

uint64_t loch_get(uint64_t loch_tcb) {
  // TODO: get the TCB somehow. I don't want to throw pointers around.
  tcb_t *tcb;
  while (atomic_load(&tcb->state) != FINISHED) {
    runtime_yield();
  }
  return tcb->result;
}

void check_for_work() {
  while (1) {
    tcb_t *tcb = sched_next(scheduler);
    if (tcb == NULL) {
      printd("no work to do!");
      usleep(100000);
    } else {
      atomic_store(&tcb->state, RUNNING);
      state.current_context = tcb;

      atomic_fetch_add(&active_threads, 1);
      swapcontext(&state.wait_ctx, &tcb->ctx);
      atomic_fetch_sub(&active_threads, 1);
    }
    if (atomic_load(&gc_flag)) {
      return;
    }
  }
}

void thread_func() {}
