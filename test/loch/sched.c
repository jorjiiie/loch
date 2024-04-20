/*
 * sched.c
 * fifo queue implementation
 */
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include <stdlib.h>

#include "debug.h"
#include "mutex.h"
#include "sched.h"
#include "tcb.h"

tcb_t *sched_next(sched_t *sched) {
  // except lock this!
  spinlock_lock(sched->lock);
  node_t *node = sched->head;
  printd("oh noes %p %d", sched->head, sched->size);
  if (node == NULL) {
    spinlock_unlock(sched->lock);
    return NULL;
  }
  tcb_t *tcb = node->tcb;
  sched->size--;
  sched->head = node->next;
  printd("for some reason the next is %p %p", node, sched->head);
  if (sched->head == NULL)
    sched->tail = NULL;
  spinlock_unlock(sched->lock);
  return tcb;
}

void sched_enqueue(sched_t *sched, tcb_t *tcb) {
  spinlock_lock(sched->lock);
  printd("adding something! %p %p", sched->head, sched->tail);
  node_t *c = sched->head;
  while (c != NULL) {
    printd("going somewhere! %p %p", c, c->next);
    c = c->next;
  }

  if (sched->head == NULL) {
    sched->head = (node_t *)malloc(sizeof(node_t));
    sched->head->tcb = tcb;
    sched->head->next = NULL;
    sched->tail = sched->head;
    sched->size++;
    spinlock_unlock(sched->lock);
    return;
  }

  node_t *n = (node_t *)malloc(sizeof(node_t));
  n->next = NULL;
  n->tcb = tcb;
  sched->tail->next = n;
  sched->tail = n;
  sched->size++;

  spinlock_unlock(sched->lock);
}

sched_t *sched_create() {
  sched_t *sched = (sched_t *)malloc(sizeof(sched_t));
  sched->lock = spinlock_create();
  sched->head = sched->tail = NULL;
  sched->size = 0;
  return sched;
}
