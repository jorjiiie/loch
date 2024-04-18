#pragma once

#ifndef DEBUG_H
#define DEBUG_H

// oh i love my debug macros
#define debug_print(fmt, ...)                                                  \
  do {                                                                         \
    if (DEBUG_LOG)                                                             \
      fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__, __LINE__, __func__,        \
              ##__VA_ARGS__);                                                  \
    fprintf(stderr, "\n");                                                     \
  } while (0)

#endif
