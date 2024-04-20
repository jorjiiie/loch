/*
 * sched.c
 * fifo queue implementation
 */
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include <stdlib.h>

#include "mutex.h"
#include "sched.h"
#include "tcb.h"

tcb_t *sched_next(sched_t *sched) {
  // except lock this!
  spinlock_lock(sched->lock);
  node_t *node = sched->head;
  tcb_t *tcb = node->tcb;
  if (tcb == NULL) {
    spinlock_unlock(sched->lock);
    return NULL;
  }
  sched->head = node->next;
  if (sched->head == NULL)
    sched->tail = NULL;
  spinlock_unlock(sched->lock);
  return tcb;
}

void sched_enqueue(sched_t *sched, tcb_t *tcb) {
  spinlock_lock(sched->lock);
  if (sched->head == NULL) {
    sched->head = (node_t *)malloc(sizeof(node_t));
    sched->head->tcb = tcb;
    sched->head->next = NULL;
    sched->tail = sched->head;
    spinlock_unlock(sched->lock);
    return;
  }

  node_t *n = (node_t *)malloc(sizeof(node_t));
  n->next = NULL;
  n->tcb = tcb;
  sched->tail->next = n;

  spinlock_unlock(sched->lock);
}
