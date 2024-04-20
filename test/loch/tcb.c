#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

#include "tcb.h"

#include "debug.h"

// i think this should be in loch - we want to go to schedule after this
// yah
void tcb_runner(tcb_t *tcb, uint64_t arg) {
  uint64_t ret = thread_code_starts_here(heap_ptr, HEAP_SIZE, arg);
  tcb->result = ret;
  atomic_store(&tcb->state, FINISHED);
}

// i think this all should be in loch

tcb_t *tcb_create(uint64_t closure_ptr) {
  printd("making closure with %d", closure_ptr);
  tcb_t *tcb = (tcb_t *)malloc(sizeof(tcb_t));
  tcb->closure_ptr = closure_ptr;
  tcb->result = 0;
  atomic_init(&tcb->state, NOT_RUNNING);

  getcontext(&tcb->ctx);
  tcb->ctx.uc_stack.ss_sp = mmap(NULL, LOCH_STACK_SIZE, PROT_READ | PROT_WRITE,
                                 MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (tcb->ctx.uc_stack.ss_sp == MAP_FAILED) {
    perror("mmap");
    exit(1);
  }
  tcb->ctx.uc_stack.ss_size = LOCH_STACK_SIZE;
  tcb->ctx.uc_link = NULL;
  makecontext(&tcb->ctx, (void (*)(void))tcb_runner, 2, tcb, closure_ptr);

  return tcb;
}
#endif
