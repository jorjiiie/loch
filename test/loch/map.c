/*
 * map.c
 * impl
 */

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

// #define LOCH_RUNTIME

#include "map.h"
#include "debug.h"

#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

map_t *map_create() {
  map_t *mp = (map_t *)malloc(sizeof(map_t));
  mp->size = 0;
  for (int i = 0; i < NUM_LOCKS; i++) {
#ifdef LOCH_RUNTIME
    mp->locks[i] = mutex_create();
#else
    pthread_mutex_init(&mp->locks[i], NULL);
#endif
  }
  for (int i = 0; i < MAP_SIZE; i++) {
    mp->table[i] = NULL;
  }
  return mp;
}

tcb_t *map_get(map_t *map, uint64_t key) {
  size_t bucket = key & (MAP_SIZE - 1);
  size_t lock = key & (NUM_LOCKS - 1);

#ifdef LOCH_RUNTIME
  mutex_lock(map->locks[lock]);
#else
  if (pthread_mutex_lock(&map->locks[lock])) {
    perror("pthread_mutex_lock");
    exit(1);
  }
#endif

  kv_t *c = map->table[bucket];

  while (c != NULL) {
    if (c->key == key) {
#ifdef LOCH_RUNTIME
      mutex_unlock(map->locks[lock]);
#else
      if (pthread_mutex_unlock(&map->locks[lock])) {
        perror("pthread_mutex_unlock");
        exit(1);
      }
#endif
      return c->value;
    }
    c = c->next;
  }
#ifdef LOCH_RUNTIME
  mutex_unlock(map->locks[lock]);
#else
  if (pthread_mutex_unlock(&map->locks[lock])) {
    perror("pthread_mutex_unlock");
    exit(1);
  }
#endif
  return NULL;
}

void map_put(map_t *map, uint64_t key, tcb_t *value) {
  uint64_t bucket = key & (MAP_SIZE - 1);
  uint64_t lock = key & (NUM_LOCKS - 1);

#ifdef LOCH_RUNTIME
  mutex_lock(map->locks[lock]);
#else
  if (pthread_mutex_lock(&map->locks[lock])) {
    perror("pthread_mutex_lock");
    exit(1);
  }
#endif

  map->size++;

  // remember! NO collisions or we will be VERY sad
  kv_t *n = (kv_t *)malloc(sizeof(kv_t));

  n->key = key;
  n->value = value;
  n->next = map->table[bucket];

  map->table[bucket] = n;

#ifdef LOCH_RUNTIME
  mutex_unlock(map->locks[lock]);
#else
  if (pthread_mutex_unlock(&map->locks[lock])) {
    perror("pthread_mutex_unlock");
    exit(1);
  }
#endif
}

void map_erase(map_t *map, uint64_t key) {
  uint64_t bucket = key & (MAP_SIZE - 1);
  uint64_t lock = key & (NUM_LOCKS - 1);

#ifdef LOCH_RUNTIME
  mutex_lock(map->locks[lock]);
#else
  if (pthread_mutex_lock(&map->locks[lock])) {
    perror("pthread_mutex_lock");
    exit(1);
  }
#endif
  map->size--;

  kv_t *c = map->table[bucket];
  if (c == NULL)
    goto cleanup;

  if (c->key == key) {
    map->table[bucket] = c->next;
    goto cleanup;
  }

  kv_t *prev = c;
  c = c->next;
  while (c != NULL) {
    if (c->key == key) {
      prev->next = c->next;
      free(c);
      goto cleanup;
    }
    prev = c;
    c = c->next;
  }
  assert(0);

cleanup:
#ifdef LOCH_RUNTIME
  mutex_unlock(map->locks[lock]);
#else
  if (pthread_mutex_unlock(&map->locks[lock])) {
    perror("pthread_mutex_unlock");
    exit(1);
  }
#endif
}
