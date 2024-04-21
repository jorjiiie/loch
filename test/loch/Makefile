TARGET = a.out
CC = clang
SOURCES = gc.c loch.c main.c map.c sched.c set.c tcb.c
CFLAGS = -Wshadow -g -DDEBUG -D_XOPEN_SOURCE=600
all: $(TARGET)


$(TARGET): $(SOURCES)
	$(CC) $(CFLAGS) $(SOURCES) -o $(TARGET)

clean:
	rm -f $(TARGET)

.PHONY: all clean
