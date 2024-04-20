#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

#include "loch.h"
#include "tcb.h"

#include "debug.h"

tcb_t *tcb_create(uint64_t closure_ptr) {
  printd("making closure with %llu", closure_ptr);
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
  makecontext(&tcb->ctx,
              (void (*)(uint32_t, uint32_t, uint32_t, uint32_t))tcb_runner, 4,
              (uint64_t)tcb >> 32, (uint64_t)tcb & 0x00000000ffffffff,
              closure_ptr >> 32, closure_ptr & 0x00000000ffffffff);

  printf("created tcb %p\n", tcb);
  return tcb;
}
#endif
