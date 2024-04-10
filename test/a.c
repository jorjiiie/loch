#define _XOPEN_SOURCE

#include <errno.h>
// #include <pthread.h>
#include <stdint.h>

#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <ucontext.h>
#include <unistd.h>

static ucontext_t ctx[3];
#define STACK_SIZE 65536

typedef struct tcb {
  ucontext_t *ctx;
  _Atomic enum { NOT_STARTED, READY, RUNNING, BLOCKED, FINISHED } state;
  _Atomic uint64_t res;

} tcb_t;

void wrapper(uint64_t (*f)(), tcb_t *tcb) {

  uint64_t r = f();

  // i don't think this has to be atomic because it's
  // sequential ordering below

  atomic_store(&tcb->res, r);
  atomic_store(&tcb->state, FINISHED);

  if (munmap(tcb->ctx->uc_stack.ss_sp, STACK_SIZE)) {
    fprintf(stderr, "Failed to unmap the stack: %s\n", strerror(errno));
    exit(1);
  }
  free(tcb->ctx);
}

tcb_t init_tcb(uint64_t (*f)()) {
  tcb_t tcb;
  tcb.ctx = malloc(sizeof(ucontext_t));
  getcontext(tcb.ctx);
  tcb.ctx->uc_stack.ss_sp = mmap(NULL, STACK_SIZE, PROT_READ | PROT_WRITE,
                                 MAP_PRIVATE | MAP_ANON, -1, 0);
  tcb.ctx->uc_stack.ss_size = STACK_SIZE;
  tcb.ctx->uc_link = &ctx[0]; // should not uc link tbh

  makecontext(tcb.ctx, wrapper, 2, f, tcb);
  tcb.state = NOT_STARTED;
  tcb.res = 0;
  return tcb;
}

static void f1() {
  fprintf(stderr, "okay\n");
  printf("hi\n");
  swapcontext(&ctx[1], &ctx[2]);
  usleep(6942000);

  printf("hi111\n");
}

static void f2(void) {
  puts("start f2");
  swapcontext(&ctx[2], &ctx[1]);
  puts("finish f2");
}

int main(void) {
  // char st1[819v2 * 8];
  char *st2 = mmap(NULL, STACK_SIZE, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANON, -1, 0);
  char *st1 = mmap(NULL, STACK_SIZE, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANON, -1, 0);

  getcontext(&ctx[1]);
  ctx[1].uc_stack.ss_sp = st1;
  ctx[1].uc_stack.ss_size = STACK_SIZE;
  ctx[1].uc_link = &ctx[0];
  makecontext(&ctx[1], f1, 0);

  getcontext(&ctx[2]);
  ctx[2].uc_stack.ss_sp = st2;
  ctx[2].uc_stack.ss_size = STACK_SIZE;
  ctx[2].uc_link = &ctx[1];
  makecontext(&ctx[2], f2, 0);

  swapcontext(&ctx[0], &ctx[2]);
  return 0;
}
