CC = clang
CFLAGS = -Wall -Wextra -Wpedantic -Werror -std=c18
LFLAGS = 

INCLUDES = 
LIBS = 

SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)
EXE = compiler

all: $(EXE)
	@echo Compiler has been compiled! Executable is named compiler.

debug: CFLAGS += -DDEBUG -g -O0
debug: $(EXE)

$(EXE): $(OBJS)
	$(CC) $(CFLAGS) $(INCLUDES) -o $(EXE) $(OBJS) $(LFLAGS) $(LIBS)

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -f *.o $(EXE)
