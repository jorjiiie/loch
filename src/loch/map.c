/*
 * map.c
 * impl
 */

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

// #define LOCH_RUNTIME

#include "map.h"
#include "debug.h"

#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

map_t *map_create() {
  map_t *mp = (map_t *)malloc(sizeof(map_t));
  mp->size = 0;
  for (int i = 0; i < NUM_LOCKS; i++) {
    if (pthread_rwlock_init(&mp->locks[i], NULL)) {
      printlog("no bueno init lock!");
      exit(1);
    }

  }
  for (int i = 0; i < MAP_SIZE; i++) {
    mp->table[i] = NULL;
  }
  return mp;
}

tcb_t *map_get(map_t *map, uint64_t key) {
  size_t bucket = key & (MAP_SIZE - 1);
  size_t lock = key & (NUM_LOCKS - 1);

  if (pthread_rwlock_rdlock(&map->locks[lock])) {
    perror("pthread_rwlock_rdlock");
    exit(1);
  }

  kv_t *c = map->table[bucket];

  while (c != NULL) {
    if (c->key == key) {

      if (pthread_rwlock_unlock(&map->locks[lock])) {
        perror("pthread_rwlock_unlock");
        exit(1);
      }
      return c->value;
    }
    c = c->next;
  }
  if (pthread_rwlock_unlock(&map->locks[lock])) {
    perror("pthread_rwlock_unlock");
    exit(1);
  }
  return NULL;
}

void map_put(map_t *map, uint64_t key, tcb_t *value) {
  uint64_t bucket = key & (MAP_SIZE - 1);
  uint64_t lock = key & (NUM_LOCKS - 1);

  printlog("what is going on %llu %llu %p", bucket, lock, &map->locks[lock]);
  int err = pthread_rwlock_wrlock(&map->locks[lock]);
  if (err) {
    printlog("HUH???? %d %d %d %d",err, EBUSY, EINVAL, EDEADLK);
    perror("pthread_rwlock_wrlock");
    exit(1);
  }

  map->size++;

  // remember! NO collisions or we will be VERY sad
  kv_t *n = (kv_t *)malloc(sizeof(kv_t));

  printlog("putting %llu %p", key, value);
  n->key = key;
  n->value = value;
  n->next = map->table[bucket];

  map->table[bucket] = n;

  if (pthread_rwlock_unlock(&map->locks[lock])) {
    perror("pthread_rwlock_unlock");
    exit(1);
  }
}

void map_erase(map_t *map, uint64_t key) {
  uint64_t bucket = key & (MAP_SIZE - 1);
  uint64_t lock = key & (NUM_LOCKS - 1);

  if (pthread_rwlock_wrlock(&map->locks[lock])) {
    perror("pthread_rwlock_wrlock");
    exit(1);
  }
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
  if (pthread_rwlock_unlock(&map->locks[lock])) {
    perror("pthread_rwlock_unlock");
    exit(1);
  }
}
