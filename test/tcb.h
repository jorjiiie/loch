#pragma once
#ifndef TCB_H // classic ryan double include guard
#define TCB_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

#include <stdatomic.h>
#include <stdint.h>
#include <ucontext.h>

// defines tcb_t struct
// should hold enough information to switch contexts

// tcb should call a function that returns a uint64_t

#define LOCH_STACK_SIZE (1 << 16)

typedef enum { NOT_RUNNING, RUNNING, FINISHED } tcb_state_t;
typedef struct tcb {
  ucontext_t ctx; // these are BEEFY
  _Atomic uint64_t res;
  _Atomic tcb_state_t state;
  uint64_t closure; // we don't care for now
} tcb_t;

// entrypoint to all tcb
void tcb_run(tcb_t *tbc, uint64_t arg);
tcb_t *tcb_create(uint64_t arg);
void schedule();

// queue of jobs
#define NUM_TASKS 3
typedef struct job_queue {
  tcb_t *tasks[NUM_TASKS];
  _Atomic size_t cur_task;
} queue_t;

typedef struct kthread {
  ucontext_t *cur_ctx;

  // flag for whether or not it is safe to switch contexts
  // 0 = yes, 1 = no
  // 1 while interrupting or while GC'ing
  _Atomic int switch_flag;
} kthread_t;

// dummy function for now?
// should return something that's NOT RUNNING! maybe this should grab it before
// returning it
tcb_t *get_next_tcb(kthread_t *kt);

#endif
