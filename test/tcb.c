#define DEBUG_LOG 1
#include "tcb.h"
#include "debug.h"

#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

kthread_t ct;

// in loch, this will call the launchpad with the closure.
// along with the heap pointer and other args
// for now, we'll just... do nothing?
void tcb_run(tcb_t *tcb, uint64_t arg) {
  debug_print("starting closure %llu", arg);
  // in loch, this would be
  // uint64_t res = THREAD_CODE_STARTS_HERE(heap_start, closure);
  int i = 0; // i guess this is shared across all stacks
  atomic_store(&ct.switch_flag, 0);
  while (1) {
    printf("hi! from %llu %d\n", arg, ++i);
    poll(NULL, 0, 1000);
  }
  debug_print("GG!!!");
  munmap(tcb->ctx.uc_stack.ss_sp, LOCH_STACK_SIZE);
  atomic_store(&tcb->res, 0); // put the result in
  schedule();                 // so we don't actually exit
}
void tcb_init(tcb_t *tcb, uint64_t arg) {
  // memset(tcb, 0, sizeof(tcb_t));
  tcb->closure = arg;
  getcontext(&tcb->ctx);

  tcb->ctx.uc_stack.ss_sp = mmap(NULL, LOCH_STACK_SIZE, PROT_READ | PROT_WRITE,
                                 MAP_PRIVATE | MAP_ANON, -1, 0);
  memset(tcb->ctx.uc_stack.ss_sp, 0, LOCH_STACK_SIZE);
  tcb->ctx.uc_stack.ss_size = LOCH_STACK_SIZE;
  tcb->ctx.uc_stack.ss_flags = 0;

  if (sigemptyset(&tcb->ctx.uc_sigmask) < 0) {
    perror("sigemptyset");
    exit(1);
  }

  tcb->state = NOT_RUNNING;
  makecontext(&tcb->ctx, (void (*)(tcb_t *, uint64_t))tcb_run, 2, tcb, arg);

  printf("init tcb %p %llu\n", tcb, arg);
}
tcb_t *tcb_create(uint64_t arg) {
  tcb_t *tcb = malloc(sizeof(tcb_t));
  tcb_init(tcb, arg);
  return tcb;
}
tcb_t *c_ctx;

ucontext_t signal_ctx;
sigset_t set;              /* process wide signal mask */
ucontext_t signal_context; /* the interrupt context */
void *signal_stack;        /* global interrupt stack */

void schedule() {
  c_ctx->state = NOT_RUNNING;

  tcb_t *old = c_ctx;
  c_ctx = get_next_tcb(&ct);
  c_ctx->state = RUNNING;
  setcontext(&c_ctx->ctx);
  atomic_store(&ct.switch_flag, 0);
}

void timer_interrupt(int j, siginfo_t *si, void *old_ctx) {
  int zero = 0;
  if (atomic_compare_exchange_strong(&ct.switch_flag, &zero, 1) == 0)
    return;
  getcontext(&signal_context);

  signal_context.uc_stack.ss_sp = signal_stack;
  signal_context.uc_stack.ss_size = 4096;
  signal_context.uc_stack.ss_flags = 0;
  sigemptyset(&signal_context.uc_sigmask);
  makecontext(&signal_context, schedule, 0);

  swapcontext(&c_ctx->ctx, &signal_context); // saves state into here
  atomic_store(&ct.switch_flag, 0);
}

tcb_t *why;

tcb_t *q2[NUM_TASKS];
int cnter = 0;
tcb_t *get_next_tcb(kthread_t *kt) {
  cnter = (cnter + 1) % NUM_TASKS;
  return q2[cnter];
}

void setup_signals(void) {
  struct sigaction act;

  act.sa_sigaction = timer_interrupt;
  sigemptyset(&act.sa_mask);              // sets the mask to empty?
  act.sa_flags = SA_RESTART | SA_SIGINFO; // no idea what's going on
  // read into whats going on here

  sigemptyset(&set);
  sigaddset(&set, SIGALRM);

  if (sigaction(SIGALRM, &act, NULL) != 0) {
    perror("Signal handler");
  }
}

int main() {
  atomic_store(&ct.switch_flag, 1);
  signal_stack = malloc(4096);

  struct itimerval it;
  setup_signals();

  /* setup our timer */
  it.it_interval.tv_sec = 0;
  it.it_interval.tv_usec = 1000 * 100; // 10 ms timer
  it.it_value = it.it_interval;
  if (setitimer(ITIMER_REAL, &it, NULL))
    perror("setitiimer");
  debug_print("help");

  for (int i = 0; i < NUM_TASKS; i++) {
    q2[i] = tcb_create(i);
  }

  c_ctx = q2[0];

  atomic_store(&ct.switch_flag, 0);

  setcontext(&c_ctx->ctx);

  return 0;
}
