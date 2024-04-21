#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "set.h"

void test_simple() {
  set_t *s = set_create();
  assert(set_contains(s, 1) == 0);
  assert(set_contains(s, 0) == 0);

  set_insert(s, 1);
  assert(set_contains(s, 1) == 1);
  assert(set_contains(s, 0) == 0);

  set_insert(s, 2);
  assert(set_contains(s, 1) == 1);
  assert(set_contains(s, 2) == 1);
  assert(set_contains(s, 0) == 0);
}

void test_duplicates() {
  set_t *s = set_create();
  assert(set_contains(s, 1) == 0);
  set_insert(s, 1);
  assert(set_contains(s, 1) == 1);
  assert(set_contains(s, 0) == 0);
  set_insert(s, 1);
  assert(set_contains(s, 1) == 1);
  assert(set_contains(s, 0) == 0);
  set_insert(s, 1);
  assert(set_contains(s, 1) == 1);
  assert(set_contains(s, 0) == 0);
}

int main() {
  test_simple();
  test_duplicates();
}
