/*
 * set.c
 * impl. quadratic probing hash table!
 */

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#include "set.h"
#include "debug.h"

#include <assert.h>
#include <stdlib.h>

set_t *set_create() {
  set_t *st = (set_t *)malloc(sizeof(set_t));
  st->size = 0;
  return st;
}

void set_insert(set_t *set, uint64_t val) {
  size_t bucket = val & (SET_SIZE - 1);
  size_t i = 0;
  while (set->occupied[bucket]) {
      i++;
      bucket = (bucket + i) & (SET_SIZE - 1);
  }
  set->occupied[bucket] = 1;
  set->items[bucket] = val;
  set->size++;
  if (set->size == SET_SIZE) {
      fprintf(stderr, "max number of threads???\n");
    exit(1);
  }
}

int set_contains(set_t *set, uint64_t val) {
    size_t bucket = val & (SET_SIZE-1);
    size_t i = 0;
    while (set->occupied[bucket]) {
        if (set->items[bucket] == val) {
            return 1;
        }
        i++;
        bucket = (bucket + i) & (SET_SIZE - 1);
    }
    return 0;
}
