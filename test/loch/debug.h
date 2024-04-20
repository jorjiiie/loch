#pragma once

#ifndef DEBUG_H
#define DEBUG_H

#ifndef DEBUG
#define DEBUG_LOG 0
#else
#define DEBUG_LOG 1
#endif // !DEBUG

#include <assert.h>
#include <stdio.h>

// oh i love my debug macros
#define printd(fmt, ...)                                                       \
  do {                                                                         \
    if (DEBUG_LOG)                                                             \
      fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__, __LINE__, __func__,        \
              ##__VA_ARGS__);                                                  \
    fprintf(stderr, "\n");                                                     \
  } while (0)

#endif
