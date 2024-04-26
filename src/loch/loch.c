/*
 * loch.c
 * implementation of the threading runtime
 */
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

#include "loch.h"
#include "gc.h"
#include "map.h"
#include "sched.h"
#include "tcb.h"

// pass it as a compile flag -DDEBUG_LOG
#include "debug.h"

#include <stdlib.h>
#include <unistd.h>

extern uint64_t
thread_code_starts_here(uint64_t closure) asm("thread_code_starts_here");
extern _Atomic uint8_t reschedule[];


/*
 * threading stuff
 */
extern gc_t *gc_state;
extern sched_t *scheduler;

extern _Atomic uint64_t tcb_id;
extern _Atomic uint8_t halt_flag;
extern _Thread_local thread_state_t state;

extern uint64_t LOCK_TAG;
// table_t

void tcb_runner(uint32_t tcb_high, uint32_t tcb_low, uint32_t closure_high,
                uint32_t closure_low) {
  tcb_t *tcb = (tcb_t *)((uint64_t)tcb_high << 32 | tcb_low);
  uint64_t arg = (uint64_t)closure_high << 32 | closure_low;
  // we have this block because when we swap to a fresh context, it won't run
  // this important code! bookkeeping 101
  printlog("in runner");
  if (state.next_context != NULL) {
    // call from yield, must remember to reset the old stuffs
    atomic_store(&state.next_context->state, RUNNING);
    atomic_store(&state.current_context->state, NOT_RUNNING);

    sched_enqueue(scheduler, state.current_context);

    state.current_context = state.next_context;
    state.next_context = NULL;
  }
  uint64_t ret = thread_code_starts_here(tcb->closure_ptr);
  // uint64_t ret = do_something(arg);
  tcb->result = ret;
  // tcb_destroy(state.current_context);

  // state.current_context = state.next_context = NULL;
  atomic_store(&tcb->state, FINISHED);
  setcontext(&state.wait_ctx);
}

uint64_t tcb_set_stack_bottom(uint64_t *stack_bottom) {
  assert(state.current_context != NULL);
  state.current_context->stack_bottom = stack_bottom;
  return 0;
}

void runtime_yield() {
  assert(state.current_context != NULL);
  assert(atomic_load(&state.current_context->state) == RUNNING);


  state.next_context = sched_next(scheduler);

  printlog("next context is %p", state.next_context);

  // no other threads, just continue
  if (state.next_context == NULL) {
    return;
  }

  /*
  printd_mt("be4 STATE %p %p %p", &state, state.current_context,
            state.next_context);
  */

  thread_state_t *t = &state;
  // swap to the other thread
  swapcontext(&state.current_context->ctx, &state.next_context->ctx);

  /*
  printd_mt("  STATE %p %p %p", &state, state.current_context,
            state.next_context);
  */

  // we either come from the above (swapcontext) or we have just resumed an old
  // thread from check_for_work()
  if (state.next_context == NULL) {
    // do NOT do the below
    return;
  }
  if (state.next_context == NULL) {
    // do NOT do the below
    return;
  }

  atomic_store(&state.current_context->state, NOT_RUNNING);
  atomic_store(&state.next_context->state, RUNNING);

  sched_enqueue(scheduler, state.current_context);

  state.current_context = state.next_context;
  state.next_context = NULL;
}

void check_for_work() {
  printlog("init");
  while (1) {
    tcb_t *tcb = sched_next(scheduler);
    if (tcb == NULL) {
      printd_mt("no work to do!");
      usleep(100000);
    } else {
      atomic_store(&tcb->state, RUNNING);
      state.current_context = tcb;

      atomic_fetch_add(&gc_state->active_threads, 1);
      swapcontext(&state.wait_ctx, &tcb->ctx);
      atomic_fetch_sub(&gc_state->active_threads, 1);
      
    }
    if (atomic_load(&halt_flag)) {
      printlog("im done? lol");
      return;
    }
  }
}

uint64_t _loch_yield(uint64_t *rbp, uint64_t *rsp) {
  printd("yield from loch! %p %p", rbp, rsp);

  state.current_context->frame_bottom = rbp;
  state.current_context->frame_top = rsp;
  // if (atomic_load(&reschedule[state.thread_id])) {
  //   atomic_store(&reschedule[state.thread_id], 0);
  // } else {
  //   // no need to reschedule
  //   return 0;
  // }
  // for thread to GC, we must have THREADS - 1 ack's
  // although this MUST work for the number of running threads!
  // check if GC is necessary
  // we do we yield in here? because why not?
  // it doesn't matter.
  atomic_fetch_add(&gc_state->gc_ack, 1);
  while (atomic_load(&gc_state->gc_flag) == 1) {
    usleep(100);
  }
  atomic_fetch_sub(&gc_state->gc_ack, 1);
  runtime_yield();
  return 0; // this does in fact mess up RAX!
}

uint64_t _loch_thread_create(uint64_t closure) {
  tcb_t *tcb = tcb_create(closure);
  printlog("what %llu %p %llu", closure, tcb, tcb_id);
  uint64_t t_id = atomic_fetch_add(&tcb_id, 1);
  map_put(gc_state->map, t_id, tcb);
  return (t_id << 4);
}

uint64_t _loch_thread_get(uint64_t thread, uint64_t *rbp, uint64_t *rsp) {
  // untag thread
  uint64_t t_id = thread >> 4;

  tcb_t *tcb = map_get(gc_state->map, t_id);
  if (tcb == NULL) {
    perror("invalid tcb!");
    exit(1);
  }
  tcb->frame_bottom = rbp;
  tcb->frame_top = rsp;
  printlog("trying to get?");

  // switch contexts
  while (atomic_load(&tcb->state) != FINISHED) {
    printlog("waiting to finish!");
    runtime_yield();
  }
  printlog("got??");
  return tcb->result;
}

uint64_t _loch_thread_start(uint64_t thread) {
  // untag thread
  uint64_t t_id = thread >> 4;
  tcb_t *tcb = map_get(gc_state->map, t_id);
  printlog("starting something! %llu %p", t_id, tcb);
  if (tcb == NULL) {
    perror("invalid tcb!");
    exit(1);
  }
  sched_enqueue(scheduler, tcb);
  return thread;
}

uint64_t _loch_set_stack(uint64_t *bottom) {
  printlog("at bottom %p", bottom);
  tcb_t *current = state.current_context;
  current->stack_bottom = bottom;
  return 0;
}
