#define _XOPEN_SOURCE
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/syslimits.h>
#include <sys/time.h>
#include <ucontext.h>
#include <unistd.h>

volatile uint64_t i = 0;

void alarm_handler(int signum) { printf("what %d i: %lld\n", signum, i); }

int main() {
  if (signal(SIGALRM, alarm_handler) == SIG_ERR) {
    perror("Unable to catch SIGALRM");
    exit(1);
  }
  struct itimerval timer;

  // Time until first alarm
  timer.it_value.tv_sec = 0;       // Seconds
  timer.it_value.tv_usec = 150000; // Microseconds (15 milliseconds)

  // Time interval for subsequent alarms
  timer.it_interval.tv_sec = 0;       // Seconds
  timer.it_interval.tv_usec = 150000; // Microseconds (15 milliseconds)

  if (setitimer(ITIMER_REAL, &timer, NULL) == -1) {
    perror("error calling setitimer()");
    exit(1);
  }
  for (;;)
    i++;
}
