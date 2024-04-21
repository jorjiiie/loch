#include "set.h"

#include <stdlib.h>
#include <stdio.h>

int main() {
    set_t *set = set_create();
    for (int i=0;i<10;i++) {
        int x = rand();
        set_insert(set, x);
        printf("we have %d\n", x);
    }
    set_dump(set);
}
