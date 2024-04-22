#include "map.h"
#include "tcb.h"

#include "debug.h"

#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define NUM_TEST 4321

#define NUM_THREAD 8

map_t *m;
void *pthread_job(void *arg) {
  int id = (int)(intptr_t)arg;
  for (int i = 0; i < NUM_TEST; i++) {
    tcb_t *tcb = tcb_create(i);
    map_put(m, NUM_TEST * id + i, tcb);
  }
  return NULL;
}

void *pthread_remove(void *arg) {
  int id = (int)(intptr_t)arg;
  for (int i = 0; i < NUM_TEST; i++) {
    map_erase(m, NUM_TEST * id + i);
  }
  return NULL;
}


int main() {
  m = map_create();
  pthread_t threads[NUM_THREAD];
  for (int i = 0; i < NUM_THREAD; i++) {
    pthread_create(&threads[i], NULL, pthread_job, (void *)(intptr_t)i);
  }
  for (int i = 0; i < NUM_THREAD; i++) {
    pthread_join(threads[i], NULL);
  }
  for (int i = 0; i < NUM_TEST * NUM_THREAD; i++) {
    tcb_t *tcb = map_get(m, i);
    // printlog("key = %d, got %llu, expected %d", i, tcb->closure_ptr,
    //         i % NUM_TEST);
  }
  printlog("map size = %lu", m->size);
  for (int i = 0; i < NUM_THREAD; i++) {
    pthread_create(&threads[i], NULL, pthread_remove, (void *)(intptr_t)i);
  }
  for (int i = 0; i < NUM_THREAD; i++) {
    pthread_join(threads[i], NULL);
  }
  printlog("map size = %lu", m->size);

  for (int i = 0; i < MAP_SIZE; i++) {
    assert(m->table[i] == NULL);
  }
}

