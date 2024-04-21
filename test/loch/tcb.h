#pragma once
#ifndef TCB_H
#define TCB_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include <stdatomic.h>
#include <stdint.h>
#include <ucontext.h>

typedef enum { NOT_RUNNING, RUNNING, FINISHED } tcb_state_t;

#define LOCH_STACK_SIZE (1 << 20)

typedef struct tcb {
  // scheduler info
  ucontext_t ctx;
  _Atomic tcb_state_t state;

  // loch info (interfacing)
  uint64_t closure_ptr;
  uint64_t result;

  // gc info
  // rbp of thread_start_here
  uint64_t *stack_bottom;

  // rsp of highest stack frame
  uint64_t *frame_top;
  // rbp of highest stack frame
  uint64_t *frame_bottom;
} tcb_t;

// creates a new tcb with the closure pointer.
// starts with a call to tcb_runner(this, closure_ptr)
tcb_t *tcb_create(uint64_t closure_ptr);

void tcb_destroy(tcb_t *tcb);

#endif
