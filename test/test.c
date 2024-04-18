#define _XOPEN_SOURCE 600

#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "tcb.h"

#define NUM_TASK 5
tcb_t *tbs[NUM_TASK];
ucontext_t ctxs[NUM_TASK];
int c = 0;
void function() {
  printf("hi! from %d\n", 1);
  fflush(stdout);
  if (c + 1 < NUM_TASK) {
    // setcontext(&tbs[++c]->ctx);
    setcontext(&ctxs[++c]);
  }
  printf("done!\n");
  fflush(stdout);
  return;
}

int main() {
  /*
  for (int i = 0; i < NUM_TASK; i++) {
    tbs[i] = malloc(sizeof(tcb_t));
    tcb_t *tcb = tbs[i];

    getcontext(&tcb->ctx);
    tcb->ctx.uc_stack.ss_sp = malloc(LOCH_STACK_SIZE);
    memset(tc->ctx.uc_stack.ss_sp, 0, LOCH_STACK_SIZE);
    tcb->ctx.uc_stack.ss_size = LOCH_STACK_SIZE;
    tcb->ctx.uc_stack.ss_flags = 0;

    makecontext(&tcb->ctx, function, 0);
  }
  */
  for (int i = 0; i < NUM_TASK; i++) {
    getcontext(&ctxs[i]);
    ctxs[i].uc_stack.ss_sp = malloc(4096);
    ctxs[i].uc_stack.ss_size = 4096;
    ctxs[i].uc_stack.ss_flags = 0;

    makecontext(&ctxs[i], function, 0);
  }
  setcontext(&ctxs[0]);
}
