/*
 * mutex.h
 * mutexes for loch. used in both C runtime and provide the backing for
 * loch code (asm) mutexes - just expose this api
 *
 */
#pragma once
#ifndef MUTEX_H
#define MUTEX_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

// oh how i would love a namespace

#include <stdatomic.h>

// dumbest lock in existence
typedef struct mutex {
  _Atomic uint32_t lock;
} mutex_t;

// createa  mutex
// TODO: MAKE SURE ALL OUR ATOMICS ARE INITIALIZED PROPERLY
mutex_t *mutex_create();
void mutex_destroy(mutex_t *m);

// locks the mutex
// duh (blocks)
void mutex_lock(mutex_t *m);

// unlocks the mutex
// duh
void mutex_unlock(mutex_t *m);

// because sometimes we need the spinlock!
typedef struct spinlock {
  _Atomic uint32_t lock;
} spinlock_t;

spinlock_t *spinlock_create();
void spinlock_destroy(spinlock_t *m);
void spinlock_lock(spinlock_t *m);
void spinlock_unlock(spinlock_t *m);

#endif
