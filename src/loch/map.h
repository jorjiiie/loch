/*
 * map.h
 * concurrent-safe map from uint64_t : tcb_t*
 * chaining + striped locks
 */
#pragma once
#ifndef MAP_H
#define MAP_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

#include "tcb.h"

#include <pthread.h>
#include <stddef.h>
#include <stdint.h>

typedef struct kv {
  uint64_t key;
  tcb_t *value;
  struct kv *next;
} kv_t;

#define MAP_SIZE 1024
#define NUM_LOCKS 10

// hash function is the brilliant identity
typedef struct map {
  kv_t *table[MAP_SIZE];
  _Atomic size_t size;

  pthread_rwlock_t locks[NUM_LOCKS];

} map_t;

// creates a map. duh! ready to be used
map_t *map_create();

// gets from map. null = not present.
tcb_t *map_get(map_t *map, uint64_t key);

// puts into a map. there shouldn't be any collisions.
void map_put(map_t *map, uint64_t key, tcb_t *value);

// erases from a map. should be present. otherwise it craps itself
void map_erase(map_t *map, uint64_t key);

#endif
