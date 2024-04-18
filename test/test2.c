#define _XOPEN_SOURCE 600
#include <poll.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <ucontext.h>
#include <unistd.h>

/* ucontext sample program

   by Jon Kaplan and Robert Spier (October 24th, 1999)
   Updated for 2000 and poll, Robert Spier
   sigprocmask gaff fixed by Ben Slusky
   ported to Linux by Eric Cronin
   $Id: context_demo.c 37 2006-10-12 22:16:59Z ecronin $

   Demonstrates swapping between multiple processor contexts (in a
   _stable_ way).  n-1 contexts do nothing.  1 context accepts input
   and outputs it.
*/

#define NUMCONTEXTS 10 /* how many contexts to make */
#define STACKSIZE 4096 /* stack size */
#define INTERVAL 1     /* timer interval in ms */

sigset_t set;              /* process wide signal mask */
ucontext_t signal_context; /* the interrupt context */
void *signal_stack;        /* global interrupt stack */

ucontext_t contexts[NUMCONTEXTS]; /* store our context info */
int curcontext = 0;               /* whats the current context? */
ucontext_t *cur_context;          /* a pointer to the current_context */

/* The scheduling algorithm; selects the next context to run, then starts
   it. */
void scheduler() {

  printf("coming from %d\n", curcontext);
  curcontext = (curcontext + 1) % NUMCONTEXTS; /* round robin */
  cur_context = &contexts[curcontext];
  printf("scheduling to %d\n", curcontext);

  setcontext(cur_context); /* go */
}

/* Thread bodies */
void thread1() {
  printf("what lol\n");
  while (1) {
    poll(NULL, 0, 100);
    scheduler();
  }; /* do nothing nicely */
}

/* helper function to create a context.
   initialize the context from the current context, setup the new
   stack, signal mask, and tell it which function to call.
*/
void mkcontext(ucontext_t *uc, void *function) {
  void *stack;

  getcontext(uc);

  stack = malloc(STACKSIZE);
  if (stack == NULL) {
    perror("malloc");
    exit(1);
  }
  /* we need to initialize the ucontext structure, give it a stack,
      flags, and a sigmask */
  uc->uc_stack.ss_sp = stack;
  uc->uc_stack.ss_size = STACKSIZE;
  uc->uc_stack.ss_flags = 0;

  /* setup the function we're going to, and n-1 arguments. */
  makecontext(uc, function, 1);

  printf("context is %p\n", uc);
}

int main() {
  int i;

  for (i = 0; i < NUMCONTEXTS; i++)
    mkcontext(&contexts[i], thread1);

  /* force a swap to the first context */
  cur_context = &contexts[0];
  setcontext(&contexts[0]);

  return 0; /* make gcc happy */
}
