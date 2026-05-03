CC ?= dcc
CFLAGS ?= -Wall -Wextra -Werror -std=c11 -g

TARGET ?= {{title}}
SRC := $(TARGET).c

.PHONY: all clean run test

all: $(TARGET)

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) -o $@ $<

run: $(TARGET)
	./$(TARGET)

test: $(TARGET)
	1521 autotest $(TARGET)

clean:
	rm -f $(TARGET) *.o core

