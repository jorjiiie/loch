#pragma once
#ifndef SCHED_H
#define SCHED_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include <pthread.h>
#include <stddef.h>

typedef struct tcb tcb_t;
// struct/class for scheduler!
// this is currently the dumbest one possible
typedef struct node {
  tcb_t *tcb;
  struct node *next;
} node_t;

typedef struct sched {
  pthread_mutex_t *mutex;
  node_t *head;
  node_t *tail;
  _Atomic size_t size;
} sched_t;

// get the next to schedule
// returned tcb_t MUST be not running, and is claimed
// must be thread safe.
tcb_t *sched_next(sched_t *sched);

// add a tcb_t to the scheduler
// tcb_t* passed in should be NOT_RUNNING,
// must be thread safe
void sched_enqueue(sched_t *sched, tcb_t *tcb);

size_t sched_size(sched_t *sched);

// initialize a scheduler
sched_t *sched_create();

#endif
