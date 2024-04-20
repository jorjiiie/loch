#include "gc.h"
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uint64_t SNAKEVAL;

void printHelp(FILE *out, SNAKEVAL val);

extern uint64_t NUM_TAG_MASK;
extern uint64_t CLOSURE_TAG_MASK;
extern uint64_t TUPLE_TAG_MASK;
extern uint64_t FORWARDING_TAG_MASK;
extern uint64_t CLOSURE_TAG;
extern uint64_t TUPLE_TAG;
extern uint64_t FORWARDING_TAG;
extern uint64_t NIL;
extern uint64_t tupleCounter;
extern uint64_t *STACK_BOTTOM;
extern uint64_t *FROM_S;
extern uint64_t *FROM_E;
extern uint64_t *TO_S;
extern uint64_t *TO_E;

SNAKEVAL dprintStack(SNAKEVAL val, uint64_t *rsp, uint64_t *rbp,
                     uint64_t args) {
  printf("RSP: %#018lx\t==>  ", (uint64_t)rsp);
  fflush(stdout);
  printHelp(stdout, *rsp);
  fflush(stdout);
  printf("\nRBP: %#018lx\t==>  ", (uint64_t)rbp);
  fflush(stdout);
  printHelp(stdout, *rbp);
  fflush(stdout);
  printf("\n(difference: %ld)\n", (uint64_t)(rsp - rbp));
  fflush(stdout);
  printf("Requested return val: %#018lx\t==> ", (uint64_t)val);
  fflush(stdout);
  printHelp(stdout, val);
  fflush(stdout);
  printf("\n");
  fflush(stdout);
  printf("Num args: %ld\n", args);

  uint64_t *origRsp = rsp;

  if (rsp > rbp) {
    printf("Error: RSP and RBP are not properly oriented\n");
    fflush(stdout);
  } else {
    for (uint64_t *cur = rsp; cur < STACK_BOTTOM + 3; cur++) {
      if (cur == STACK_BOTTOM) {
        printf("BOT %#018lx: %#018lx\t==>  old rbp\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else if (cur == rbp) {
        printf("RBP %#018lx: %#018lx\t==>  old rbp\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else if (cur == origRsp) {
        printf("    %#018lx: %#018lx\t==>  old rbp\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else if (cur == rbp + 1) {
        printf("    %#018lx: %#018lx\t==>  saved ret\n", (uint64_t)cur, *cur);
        fflush(stdout);
        rsp = rbp + 2;
        rbp = (uint64_t *)(*rbp);
      } else if (cur == STACK_BOTTOM + 2) {
        printf("    %#018lx: %#018lx\t==>  heap\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else {
        printf("    %#018lx: %#018lx\t==>  ", (uint64_t)cur, *cur);
        fflush(stdout);
        printHelp(stdout, *cur);
        fflush(stdout);
        printf("\n");
        fflush(stdout);
      }
    }
  }
  return val;
}

void naive_print_heap(uint64_t *heap, uint64_t *heap_end) {
  printf("In naive_print_heap from %p to %p\n", heap, heap_end);
  for (uint64_t i = 0; i < (uint64_t)(heap_end - heap); i += 1) {
    printf("  %ld/%p: %p (%ld)\n", i, (heap + i), (uint64_t *)(*(heap + i)),
           *(heap + i));
  }
}

// Implement the functions below

void smarter_print_heap(uint64_t *from_start, uint64_t *from_end,
                        uint64_t *to_start, uint64_t *to_end) {
  // Print out the entire heap (both semispaces), and
  // try to print values readably when possible
  fprintf(stderr, "---------heap #1----------\n");
  fprintf(stderr, "starting val %p %llu\n", from_start, *from_start);
  while (from_start < from_end) {
    if ((*from_start & TUPLE_TAG_MASK) == TUPLE_TAG)
      fprintf(stderr, " tuple?");
    else if ((*from_start & CLOSURE_TAG_MASK) == CLOSURE_TAG)
      fprintf(stderr, "closure?");
    fprintf(stderr, "[%p]: %" PRIx64 "\n", from_start, *from_start);
    from_start++;
  }
  fprintf(stderr, "---------heap #2----------\n");
  while (to_start < to_end) {
    if ((*to_start & TUPLE_TAG_MASK) == TUPLE_TAG)
      fprintf(stderr, " tuple?");
    else if ((*to_start & CLOSURE_TAG_MASK) == CLOSURE_TAG)
      fprintf(stderr, "closure?");

    fprintf(stderr, "[%p]: %" PRIx64 "\n", to_start, *to_start);

    to_start++;
  }
}

/*
  Copies a Loch value from the given address to the new heap,
  but only if the value is heap-allocated and needs copying.

  Arguments:
    Loch_val_addr: the *address* of some Loch value, which contains a Loch
  value, i.e. a tagged word. It may or may not be a pointer to a heap-allocated
  value... heap_top: the location at which to begin copying, if any copying is
  needed

  Return value:
    The new top of the heap, at which to continue allocations

  Side effects:
    If the data needed to be copied, then this replaces the value at its old
  location with a forwarding pointer to its new location
 */
uint64_t *copy_if_needed(uint64_t *Loch_val_addr, uint64_t *heap_top) {
  // no-op for now
  uint64_t v = *Loch_val_addr;
  if ((v & TUPLE_TAG_MASK) == TUPLE_TAG) {
    // heap alloc'd, realloc
    uint64_t *ptr = (uint64_t *)(v - TUPLE_TAG);
    if (((uint64_t)ptr | TUPLE_TAG) == NIL) {
      return heap_top;
    }
    if ((ptr[0] & FORWARDING_TAG_MASK) == FORWARDING_TAG) {
      *Loch_val_addr = ptr[0] - FORWARDING_TAG + TUPLE_TAG;
      return heap_top;
    }

    uint64_t size = *ptr / 2;
    uint64_t *new_ptr = heap_top;
    // need padding too
    heap_top += size + 1;
    if ((size + 1) & 1)
      heap_top++;
    // copy all the insides too.
    memcpy(new_ptr, ptr, (size + 1) * 8);
    ptr[0] = (uint64_t)(new_ptr) + FORWARDING_TAG;

    for (int i = 0; i < size; i++) {
      heap_top = copy_if_needed(new_ptr + i + 1, heap_top);
    }
    *Loch_val_addr = (uint64_t)new_ptr + TUPLE_TAG;
  } else if ((v & CLOSURE_TAG_MASK) == CLOSURE_TAG) {
    uint64_t *ptr = (uint64_t *)(v - CLOSURE_TAG);
    if ((ptr[0] & FORWARDING_TAG_MASK) == FORWARDING_TAG) {
      *Loch_val_addr = ptr[0] - FORWARDING_TAG + CLOSURE_TAG;
      return heap_top;
    }

    uint64_t size = ptr[2];
    uint64_t *new_ptr = heap_top;
    heap_top += size + 3;
    if ((size + 3) & 1)
      heap_top++;
    memcpy(new_ptr, ptr, (size + 3) * 8);

    // put forwarding tag in
    ptr[0] = (uint64_t)(new_ptr) + FORWARDING_TAG;

    for (int i = 0; i < size; i++) {
      heap_top = copy_if_needed(new_ptr + i + 3, heap_top);
    }
    *Loch_val_addr = (uint64_t)(new_ptr) + CLOSURE_TAG;
  }

  // forwarding pointer alternatively?
  return heap_top;
}

/*
  Implements Cheney's garbage collection algorithm.

  Arguments:
    bottom_frame: the base pointer of our_code_starts_here, i.e. the bottommost
  Loch frame top_frame: the base pointer of the topmost Loch stack frame
    top_stack: the current stack pointer of the topmost Loch stack frame
    from_start and from_end: bookend the from-space of memory that is being
  compacted to_start: the beginning of the to-space of memory

  Returns:
    The new location within to_start at which to allocate new data
 */
uint64_t *gc(uint64_t *bottom_frame, uint64_t *top_frame, uint64_t *top_stack,
             uint64_t *from_start, uint64_t *from_end, uint64_t *to_start) {
  uint64_t *old_start = to_start;
  uint64_t *old_top_frame = top_frame;
  uint64_t *old_top_stack = top_stack;
  /*
    fprintf(stderr, "PRE GC\n");
    smarter_print_heap(from_start, from_end, to_start,
                       to_start + (from_end - from_start));
    dprintStack(0, top_stack, bottom_frame, 0);
    */
  do {
    for (uint64_t *cur_word = top_stack /* maybe need a +1 here? (no) */;
         cur_word < top_frame; cur_word++) {

      to_start = copy_if_needed(cur_word, to_start);
    }
    /* Shift to next stack frame:
     * [top_frame] points to the saved RBP, which is the RBP of the next stack
     * frame, [top_frame + 8] is the return address, and [top_frame + 16] is
     * therefore the next frame's stack-top
     */
    top_stack = top_frame + 2;
    old_top_frame = top_frame;
    top_frame = (uint64_t *)(*top_frame);
  } while (old_top_frame < bottom_frame); // Use the old stack frame to decide
  // if there's more GC'ing to do
  /*
  fprintf(stderr, "POST GC\n");
  smarter_print_heap(from_start, from_end, old_start,

                     to_start);
  dprintStack(0, old_top_stack, bottom_frame, 0);
  */

  // after copying and GC'ing all the stack frames, return the new allocation
  // starting point
  return to_start;
}
