#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600
#endif

#include "gc.h"
#include "loch.h"
#include "sched.h"
#include "tcb.h"
#include "set.h"

#include "debug.h"

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

// number of threads to execute snake code with
#define NUM_THREADS 1

typedef uint64_t SNAKEVAL;

extern SNAKEVAL our_code_starts_here() asm("our_code_starts_here");
extern SNAKEVAL
thread_code_starts_here(SNAKEVAL closure) asm("thread_code_starts_here");
extern void error(uint64_t, uint64_t) asm("error");
extern SNAKEVAL
set_stack_bottom(uint64_t *stack_bottom) asm("set_stack_bottom");
extern SNAKEVAL print(SNAKEVAL val) asm("print");
extern SNAKEVAL input() asm("input");
extern SNAKEVAL printStack(SNAKEVAL val, uint64_t *STACK_BOTTOM, uint64_t *rsp,
                           uint64_t *rbp, uint64_t args) asm("print_stack");
extern SNAKEVAL equal(SNAKEVAL val1, SNAKEVAL val2) asm("equal");
extern uint64_t *try_gc(uint64_t *alloc_ptr, uint64_t amount_needed,
                        uint64_t *first_frame,
                        uint64_t *stack_top) asm("try_gc");
extern uint64_t *reserve(uint64_t size, uint64_t *rbp, uint64_t *rsp) asm("reserve");

extern uint64_t _loch_yield(uint64_t *rbp, uint64_t *rsp) asm("_loch_yield");

extern uint64_t
_loch_thread_create(uint64_t closure) asm("_loch_thread_create");

extern uint64_t _loch_thread_get(uint64_t thread, uint64_t *rbp,
                                 uint64_t *rsp) asm("_loch_thread_get");

extern uint64_t _loch_thread_start(uint64_t thread) asm("_loch_thread_start");

extern uint64_t _lock_set_stack(uint64_t *bottom) asm("_lock_set_stack");

const uint64_t NUM_TAG_MASK = 0x0000000000000001;
const uint64_t BOOL_TAG_MASK = 0x000000000000000f;
const uint64_t TUPLE_TAG_MASK = 0x000000000000000f;
const uint64_t CLOSURE_TAG_MASK = 0x000000000000000f;
const uint64_t FORWARDING_TAG_MASK = 0x000000000000000f;
const uint64_t THREAD_TAG_MASK = 0x000000000000000f;
const uint64_t LOCK_TAG_MASK = 0x000000000000000f;
const uint64_t NUM_TAG = 0x0000000000000000;
const uint64_t BOOL_TAG = 0x0000000000000007;
const uint64_t TUPLE_TAG = 0x0000000000000001;
const uint64_t CLOSURE_TAG = 0x0000000000000005;
const uint64_t FORWARDING_TAG = 0x0000000000000003;
const uint64_t THREAD_TAG = 0x0000000000000009; // these are not right lol
const uint64_t LOCK_TAG = 0x000000000000000b;

const uint64_t BOOL_TRUE = 0xFFFFFFFFFFFFFFFF;
const uint64_t BOOL_FALSE = 0x7FFFFFFFFFFFFFFF;
const uint64_t NIL = ((uint64_t)NULL | TUPLE_TAG);

typedef uint64_t error_code_t;
const error_code_t COMP_NOT_NUM = 1;
const error_code_t ARITH_NOT_NUM = 2;
const error_code_t LOGIC_NOT_BOOL = 3;
const error_code_t IF_NOT_BOOL = 4;
const error_code_t OVERFLOW = 5;
const error_code_t EXPECTED_TUPLE_GET = 6;
const error_code_t EXPECTED_INT_GET = 7;
const error_code_t EXPECTED_TUPLE_SET = 8;
const error_code_t EXPECTED_INT_SET = 9;
const error_code_t OUT_OF_MEMORY = 10;
const error_code_t INDEX_TOO_SMALL = 11;
const error_code_t INDEX_TOO_LARGE = 12;
const error_code_t WANT_TUPLE_GOT_NIL = 13;
const error_code_t ERR_CALL_NOT_CLOSURE = 14;
const error_code_t ERR_CALL_ARITY_ERR = 15;
const error_code_t EXPECTED_LOCK_LOCK = 16;
const error_code_t EXPECTED_LOCK_UNLOCK = 17;
const error_code_t EXPECTED_LOCK_SCOPED = 18;
const error_code_t EXPECTED_LAMBDA_SCOPED = 19;
const error_code_t EXPECTED_LAMBDA_THREAD = 20;
const error_code_t EXPECTED_THREAD_START = 21;
const error_code_t EXPECTED_THREAD_GET = 22;

// state stuff
gc_t *gc_state;
_Thread_local thread_state_t state;
_Atomic uint64_t thread_id = 1;
_Atomic uint8_t halt_flag = 0;
sched_t *scheduler;

// threads
pthread_t threads[NUM_THREADS + 1];

SNAKEVAL set_stack_bottom(uint64_t *stack_bottom) {
  return 0;
}
void naive_print_heap(uint64_t *heap, uint64_t *heap_end);

uint64_t *reserve(uint64_t wanted, uint64_t *rbp, uint64_t *rsp) {

  state.current_context->frame_top = rsp;
  state.current_context->frame_bottom = rbp;

  gc_state->gc_ack++;
  if (pthread_mutex_lock(&gc_state->lock)) {
    perror("pthread_mutex_lock");
    exit(1);
  }
  // alloc [heap_ptr, heap_ptr + wanted)
  if (gc_state->heap_ptr + wanted > gc_state->heap_end) {
    // gc
    atomic_store(&gc_state->gc_flag, 1);

    // wait until all threads are in a safe state
    while (atomic_load(&gc_state->gc_ack) !=
           atomic_load(&gc_state->active_threads)) {
      usleep(10);
    }
    // all threads are waiting, go GC

    set_clear(gc_state->seen_threads);
    uint64_t *new_heap = calloc(gc_state->HEAP_SIZE, 1);
    uint64_t *new_ptr =
        gc(gc_state->heap_start, new_heap, new_heap + gc_state->HEAP_SIZE);

    free(gc_state->heap_start);

    // i am not going to both clearing everyone out
    if (new_ptr + wanted > (new_heap + gc_state->HEAP_SIZE)) {
      fprintf(stderr, "gc failed to allocate enough memory\n");
      exit(1);
    }
    // thread cleanup

    gc_state->heap_start = new_heap;
    gc_state->heap_ptr = new_ptr;
    gc_state->heap_end = new_heap + gc_state->HEAP_SIZE;

    gc_state->heap_ptr += wanted;

    atomic_store(&gc_state->gc_flag, 0);

    atomic_fetch_sub(&gc_state->gc_ack, 1);
    if (pthread_mutex_unlock(&gc_state->lock)) {
      perror("pthread_mutex_unlock");
      exit(1);
    }

    return new_ptr;
  }
  uint64_t *ret = gc_state->heap_ptr;
  gc_state->heap_ptr += wanted;

  atomic_fetch_sub(&gc_state->gc_ack, 1);
  printlog("returning %p", ret);
  naive_print_heap(gc_state->heap_start, gc_state->heap_end);

  if (pthread_mutex_unlock(&gc_state->lock)) {
    perror("pthread_mutex_unlock");
    exit(1);
  }
  return ret;
}



SNAKEVAL equal(SNAKEVAL val1, SNAKEVAL val2) {
  if (val1 == val2) {
    return BOOL_TRUE;
  }
  if (val1 == NIL || val2 == NIL) {
    return BOOL_FALSE;
  }
  if ((val1 & TUPLE_TAG_MASK) == TUPLE_TAG &&
      (val2 & TUPLE_TAG_MASK) == TUPLE_TAG) {
    uint64_t *tup1 = (uint64_t *)(val1 - TUPLE_TAG);
    uint64_t *tup2 = (uint64_t *)(val2 - TUPLE_TAG);
    if (tup1[0] != tup2[0]) {
      return BOOL_FALSE;
    }
    for (uint64_t i = 1; i <= tup1[0] / 2; i++) {
      if (equal(tup1[i], tup2[i]) == BOOL_FALSE)
        return BOOL_FALSE;
    }
    return BOOL_TRUE;
  }
  return BOOL_FALSE;
}

uint64_t tupleCounter = 0;
void printHelp(FILE *out, SNAKEVAL val) {
  if (val == NIL) {
    fprintf(out, "nil");
  } else if ((val & NUM_TAG_MASK) == NUM_TAG) {
    fprintf(out, "%llu",
            ((int64_t)val) >> 1); // deliberately int64, so that it's signed
  } else if (val == BOOL_TRUE) {
    fprintf(out, "true");
  } else if (val == BOOL_FALSE) {
    fprintf(out, "false");
  } else if ((val & CLOSURE_TAG_MASK) == CLOSURE_TAG) {
    uint64_t *addr = (uint64_t *)(val - CLOSURE_TAG);
    fprintf(out, "[%p - 5] ==> <function arity %llu, closed %llu, fn-ptr %p>",
            (uint64_t *)val, addr[0] / 2, addr[1] / 2, (uint64_t *)addr[2]);
  } else if ((val & TUPLE_TAG_MASK) == TUPLE_TAG) {
    uint64_t *addr = (uint64_t *)(val - TUPLE_TAG);
    // Check whether we've visited this tuple already
    if ((*addr & 0x8000000000000000) != 0) {
      fprintf(out, "<cyclic tuple %d>", (int)(*addr & 0x7FFFFFFFFFFFFFFF));
      return;
    }
    uint64_t len = addr[0];
    if (len & 0x1) { // actually, it's a forwarding pointer
      fprintf(out, "forwarding to %p", (uint64_t *)(len - 1));
      return;
    } else {
      len /= 2; // length is encoded
    }
    *(addr) = 0x8000000000000000 | (++tupleCounter);
    fprintf(out, "(");
    for (uint64_t i = 1; i <= len; i++) {
      if (i > 1)
        fprintf(out, ", ");
      printHelp(out, addr[i]);
    }
    if (len == 1)
      fprintf(out, ", ");
    fprintf(out, ")");
    // Unmark this tuple: restore its length
    *(addr) = len * 2; // length is encoded
  } else {
    fprintf(out, "Unknown value: %#018llx", val);
  }
}

SNAKEVAL printStack(SNAKEVAL val, uint64_t *STACK_BOTTOM, uint64_t *rsp,
                    uint64_t *rbp, uint64_t args) {
  printf("RSP: %#018llx\t==>  ", (uint64_t)rsp);
  fflush(stdout);
  printHelp(stdout, *rsp);
  fflush(stdout);
  printf("\nRBP: %#018llx\t==>  ", (uint64_t)rbp);
  fflush(stdout);
  printHelp(stdout, *rbp);
  fflush(stdout);
  printf("\n(difference: %llu)\n", (uint64_t)(rsp - rbp));
  fflush(stdout);
  printf("Requested return val: %#018llx\t==> ", (uint64_t)val);
  fflush(stdout);
  printHelp(stdout, val);
  fflush(stdout);
  printf("\n");
  fflush(stdout);
  printf("Num args: %llu\n", args);

  uint64_t *origRsp = rsp;

  if (rsp > rbp) {
    printf("Error: RSP and RBP are not properly oriented\n");
    fflush(stdout);
  } else {
    for (uint64_t *cur = rsp; cur < STACK_BOTTOM + 3; cur++) {
      if (cur == STACK_BOTTOM) {
        printf("BOT %#018llx: %#018llx\t==>  old rbp\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else if (cur == rbp) {
        printf("RBP %#018llx: %#018llx\t==>  old rbp\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else if (cur == origRsp) {
        printf("    %#018llx: %#018llx\t==>  old rbp\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else if (cur == rbp + 1) {
        printf("    %#018llx: %#018llx\t==>  saved ret\n", (uint64_t)cur, *cur);
        fflush(stdout);
        rsp = rbp + 2;
        rbp = (uint64_t *)(*rbp);
      } else if (cur == STACK_BOTTOM + 2) {
        printf("    %#018llx: %#018llx\t==>  heap\n", (uint64_t)cur, *cur);
        fflush(stdout);
      } else {
        printf("    %#018llx: %#018llx\t==>  ", (uint64_t)cur, *cur);
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

SNAKEVAL input() {
  uint64_t ans;
  scanf("%llu", &ans);
  return ans << 1;
}

SNAKEVAL print(SNAKEVAL val) {
  printHelp(stdout, val);
  printf("\n");
  fflush(stdout);
  return val;
}

void naive_print_heap(uint64_t *heap, uint64_t *heap_end) {
  printf("In naive_print_heap from %p to %p\n", heap, heap_end);
  for (uint64_t i = 0; i < (uint64_t)(heap_end - heap); i += 1) {
    printf("  %llu/%p: %p (%llu)\n", i, (heap + i), (uint64_t *)(*(heap + i)),
           *(heap + i));
  }
}

void error(uint64_t code, SNAKEVAL val) {
  switch (code) {
  case COMP_NOT_NUM:
    fprintf(stderr, "Error: comparison expected a number, got ");
    break;
  case ARITH_NOT_NUM:
    fprintf(stderr, "Error: arithmetic expected a number, got ");
    break;
  case LOGIC_NOT_BOOL:
    fprintf(stderr, "Error: logic expected a boolean, got ");
    break;
  case IF_NOT_BOOL:
    fprintf(stderr, "Error: if expected a boolean, got ");
    break;
  case OVERFLOW:
    fprintf(stderr, "Error: integer overflow, got ");
    break;
  case EXPECTED_TUPLE_GET:
    fprintf(stderr, "Error: expected tuple, got something in tuple get ");
    break;
  case EXPECTED_INT_GET:
    fprintf(stderr, "Error: expected int, got something in int get ");
    break;
  case EXPECTED_TUPLE_SET:
    fprintf(stderr, "Error: expected tuple, got something in tuple set ");
    break;
  case EXPECTED_INT_SET:
    fprintf(stderr, "Error: expected int, got something in int set ");
    break;
  case OUT_OF_MEMORY:
    fprintf(stderr, "Error: out of memory\n");
    break;
  case INDEX_TOO_SMALL:
    fprintf(stderr, "Error: index too small, got ");
    break;
  case INDEX_TOO_LARGE:
    fprintf(stderr, "Error: index too large, got (don't know size of tuple) ");
    break;
  case WANT_TUPLE_GOT_NIL:
    fprintf(stderr, "Error: expected tuple, got nil: access component of nil ");
    break;
  case ERR_CALL_NOT_CLOSURE:
    fprintf(stderr, "Error: tried to call a non-closure value: ");
    break;
  case ERR_CALL_ARITY_ERR:
    fprintf(stderr, "Error: arity mismatch in call\n");
    goto skip_print;
  default:
    fprintf(stderr, "Error: Unknown error code: %llu, val: ", code);
  }
  fprintf(stderr, "\n%p ==> ", (uint64_t *)val);
  printHelp(stderr, val);
  fprintf(stderr, "\n");
  fflush(stderr);
  naive_print_heap(gc_state->heap_ptr,
                   gc_state->heap_end); // you really want to do getters and
                                        // setters for lock reasons
  fflush(stdout);
skip_print:
printlog("gc pointers are %p %p", gc_state->heap_start, gc_state->heap_end);

naive_print_heap(gc_state->heap_start, gc_state->heap_end);

  free(gc_state->heap_start);
  exit(code);
}

void *loch_runner(void *x) {
  // initialize thread local stuff
  state.current_context = NULL;
  state.next_context = NULL;
  state.thread_id = atomic_fetch_add(&thread_id, 1);
  check_for_work();
  return NULL;
}
_Atomic uint64_t gg = 0;
#define NUM_TASK 500
_Atomic(int) cnt[NUM_TASK];
uint64_t do_something(uint64_t arg) {
  for (int i = 0; i < 2; i++) {
    // printf("LOL! %d %llu\n", i, arg);

    cnt[arg]++;
    gg++;
    // usleep(50000); // 1s
    runtime_yield();
  }
  printd_mt("FINISHED_TASK!");
  return 0;
}

void loch_setup(void) {
  scheduler = sched_create();
  atomic_store(&halt_flag, 0);

  for (int i = 0; i < NUM_THREADS; i++) {
    pthread_create(&threads[i], NULL, loch_runner, NULL);
  }
  state.thread_id = 0; // main thread
}

void loch_teardown() {
  atomic_store(&halt_flag, 1);
  for (int i = 0; i < NUM_THREADS; i++) {
    pthread_join(threads[i], NULL);
  }
}

_Atomic uint64_t global_ans = 0;
void main_runner(void) {
    global_ans = our_code_starts_here();
    setcontext(&state.wait_ctx);
}
int main(int argc, char **argv) {
  uint64_t HEAP_SIZE = 100000;
  HEAP_SIZE = 100000;
  if (argc > 1) {
    HEAP_SIZE = atoi(argv[1]);
  }
  if (HEAP_SIZE < 0 || HEAP_SIZE > 1000000) {
    HEAP_SIZE = 0;
  }
  gc_state = gc_init(HEAP_SIZE);

  uint64_t *aligned = (uint64_t *)(((uint64_t)gc_state->heap_ptr + 15) & ~0xF);
  gc_state->heap_end = aligned + HEAP_SIZE;
  gc_state->heap_ptr = aligned;

  printlog("gc pointers are %p %p", gc_state->heap_start, gc_state->heap_end);


  loch_setup();

  tcb_t *main_tcb = tcb_create(0);
  makecontext(&main_tcb->ctx, main_runner,0);
  sched_enqueue(scheduler, main_tcb);




  while (sched_size(scheduler) || atomic_load(&gc_state->active_threads) != 0) {
    usleep(1000);
  }
  loch_teardown();

  uint64_t result = global_ans;
  printlog("gc pointers are %p %p", gc_state->heap_start, gc_state->heap_end);

  naive_print_heap(gc_state->heap_start, gc_state->heap_end);
  print(result);


  free(gc_state->heap_start);
  return 0;
}
