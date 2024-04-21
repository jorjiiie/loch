/*
 * set.h
 * single-threaded set implementation for integers
 * for devious flair this is open addressed
 */
#pragma once
#ifndef SET_H
#define SET_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif


#include <stddef.h>
#include <stdint.h>


// this is the max number of threads!
#define SET_SIZE 1024 * 4

// hash function is the brilliant identity
typedef struct set {
    uint64_t items[SET_SIZE];
    char occupied[SET_SIZE];
    size_t size;
} set_t;

// initializes a set
set_t *set_create();

// 0 = no, 1 = yes
int set_contains(set_t* set, uint64_t val);

// puts into a set. if collisions, whatever
void set_put(set_t *map, uint64_t key);

void set_clear(set_t *set);

// don't think i need set erase
#endif
