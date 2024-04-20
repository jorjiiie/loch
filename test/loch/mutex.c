/*
 * mutex.c
 * impl of mutex with a dumb yield() call
 */
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include <stdlib.h>
#include <unistd.h>

#include "loch.h"
#include "mutex.h"

mutex_t *mutex_create() {
  mutex_t *m = (mutex_t *)malloc(sizeof(mutex_t));
  atomic_init(&m->lock, 0);
  return m;
}
void mutex_destroy(mutex_t *m) { free(m); }

void mutex_lock(mutex_t *m) {
  uint32_t exp = 0;
  while (atomic_compare_exchange_strong(&m->lock, &exp, 1)) {
    runtime_yield();
  }
  // yay we now own the lock!
  // proud parent of a 1 in the lock
}

// we are the ONLY person to have this!
void mutex_unlock(mutex_t *m) { atomic_store(&m->lock, 0); }

spinlock_t *spinlock_create() {
  spinlock_t *s = (spinlock_t *)malloc(sizeof(spinlock_t));
  atomic_init(&s->lock, 0);
  return s;
}

void spinlock_destroy(spinlock_t *s) { free(s); }

void spinlock_lock(spinlock_t *s) {
  uint32_t exp = 0;
  while (1) {
    if (atomic_compare_exchange_strong(&s->lock, &exp, 1)) {
      return;
    }
  }
}

void spinlock_unlock(spinlock_t *s) { atomic_store(&s->lock, 0); }
