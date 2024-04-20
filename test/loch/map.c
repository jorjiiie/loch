/*
 * map.c
 * impl
 */

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#include <pthread.h>
#endif

#include "map.h"

#include <assert.h>
#include <stdlib.h>

map_t *map_create() {
  map_t *mp = (map_t *)malloc(sizeof(map_t));
  mp->size = 0;
  for (int i = 0; i < NUM_LOCKS; i++) {
    mp->locks[i] = mutex_create();
  }
  for (int i = 0; i < MAP_SIZE; i++) {
    mp->table[i] = NULL;
  }
  return mp;
}

tcb_t *map_get(map_t *map, uint64_t key) {
  size_t bucket = key & (MAP_SIZE - 1);
  size_t lock = key & (NUM_LOCKS - 1);

  mutex_lock(map->locks[lock]);

  kv_t *c = map->table[bucket];

  while (c != NULL) {
    if (c->key == key) {

      mutex_unlock(map->locks[lock]);
      return c->value;
    }
    c = c->next;
  }
  mutex_unlock(map->locks[lock]);
  return NULL;
}

void map_put(map_t *map, uint64_t key, tcb_t *value) {
  map->size++;
  uint64_t bucket = key & (MAP_SIZE - 1);
  uint64_t lock = key & (NUM_LOCKS - 1);

  mutex_lock(map->locks[lock]);

  // remember! NO collisions or we will be VERY sad
  kv_t *n = (kv_t *)malloc(sizeof(kv_t));

  n->key = key;
  n->value = value;
  n->next = map->table[bucket];

  map->table[bucket] = n;

  mutex_unlock(map->locks[lock]);
}

void map_erase(map_t *map, uint64_t key) {
  map->size--;
  uint64_t bucket = key & (MAP_SIZE - 1);
  uint64_t lock = key & (NUM_LOCKS - 1);

  mutex_lock(map->locks[lock]);

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
  mutex_unlock(map->locks[lock]);
}
