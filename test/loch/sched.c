/*
 * sched.c
 * fifo queue implementation
 */
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

// TODO: asm level mutex will be implemented as
// loop:
//  mov rax, 1
//  xchg rax, [mutex]
//  cmp rax, 0
//  je acquired
//  call yield(rsp, rbp)
//  jmp loop
// acquired:
//
#include <stdlib.h>

#include "debug.h"
#include "sched.h"
#include "tcb.h"

tcb_t *sched_next(sched_t *sched) {
  if (pthread_mutex_lock(sched->mutex)) {
    printlog("pthread_mutex_lock failed");
    exit(1);
  }
  node_t *node = sched->head;
  if (node == NULL) {
    if (pthread_mutex_unlock(sched->mutex)) {
      printlog("pthread_mutex_unlock failed");
      exit(1);
    }
    return NULL;
  }
  tcb_t *tcb = node->tcb;
  sched->size--;
  sched->head = node->next;
  if (sched->head == NULL)
    sched->tail = NULL;

  if (pthread_mutex_unlock(sched->mutex)) {
    printlog("pthread_mutex_unlock failed");
    exit(1);
  }
  return tcb;
}

void sched_enqueue(sched_t *sched, tcb_t *tcb) {
  if (pthread_mutex_lock(sched->mutex)) {
    printlog("pthread_mutex_lock failed");
    exit(1);
  }

  if (sched->head == NULL) {
    sched->head = (node_t *)malloc(sizeof(node_t));
    sched->head->tcb = tcb;
    sched->head->next = NULL;
    sched->tail = sched->head;
    sched->size++;
    if (pthread_mutex_unlock(sched->mutex)) {
      printlog("pthread_mutex_unlock failed");
      exit(1);
    }
    return;
  }

  node_t *n = (node_t *)malloc(sizeof(node_t));
  n->next = NULL;
  n->tcb = tcb;
  sched->tail->next = n;
  sched->tail = n;
  sched->size++;

  if (pthread_mutex_unlock(sched->mutex)) {
    printlog("pthread_mutex_unlock failed");
    exit(1);
  }
}

sched_t *sched_create() {
  sched_t *sched = (sched_t *)malloc(sizeof(sched_t));
  sched->mutex = (pthread_mutex_t *)malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(sched->mutex, NULL);
  sched->head = sched->tail = NULL;
  sched->size = 0;
  return sched;
}
// hopefully it doesn't need to be locked but it could be...
size_t sched_size(sched_t *sched) { return atomic_load(&sched->size); }
