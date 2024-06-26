#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

#include "loch.h"
#include "sched.h"
#include "tcb.h"

#include "debug.h"

#include <poll.h>
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

#define NUM_THREADS 2
pthread_t threads[NUM_THREADS];

uint64_t *heap_ptr = 0;
uint64_t *heap_end = 0;
uint64_t HEAP_SIZE = 0;
_Atomic uint64_t gc_ack = 0;
_Atomic uint64_t gc_flag = 0;

_Thread_local thread_state_t state;

uint64_t thread_code_starts_here(uint64_t *heap, uint64_t sz,
                                 uint64_t closure) {
  for (int i = 0; i < 10; i++) {
    printf("LOL! %d\n", i);
    usleep(100000); // 1s
  }
  return 0;
}


_Atomic uint64_t active_threads = 0;
sched_t *scheduler;

_Atomic uint64_t thread_id = 1;

void *loch_runner(void *x) {
  // initialize thread local stuff
  state.current_context = NULL;
  state.next_context = NULL;
  state.thread_id = atomic_load(&thread_id);
  check_for_work();
  return NULL;
}
/*
int main() {
  scheduler = sched_create();
  for (int i = 0; i < NUM_THREADS; i++) {
    pthread_create(&threads[i], NULL, loch_runner, NULL);
  }

  for (int i = 0; i < 10; i++) {
    tcb_t *tcb = tcb_create(0);
    sched_enqueue(scheduler, tcb);
    printd("enqueieng");
    usleep(3000);
  }
  while (sched_size(scheduler)) {
    usleep(1000);
  }
  atomic_store(&gc_flag, 1);

  for (int i = 0; i < NUM_THREADS; i++) {
    pthread_join(threads[i], NULL);
  }
}
*/
