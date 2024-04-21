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
#include <stdint.h>

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

#endif
