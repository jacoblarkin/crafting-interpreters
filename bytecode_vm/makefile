#CC = /opt/homebrew/bin/gcc-12
CC = clang
CFLAGS = -Wall -Wextra -Wpedantic -Wconversion -Wshadow -Werror -std=c2x -O3 -pg -g
LFLAGS = -L. -pg

INCLUDES =
LIBS = 

SRCDIR = .
SRCS = $(wildcard $(SRCDIR)/*.c)
OBJDIR = obj
OBJS = $(SRCS:$(SRCDIR)/%.c=$(OBJDIR)/%.o)
EXE = clox

TSTSRCDIR = test
TSTSRC = $(wildcard $(TSTDRCDIR)/*.c)
TSTOBJS = $(TSTSRC:$(TSTSRCDIR)/%.c=$(OBJDIR)/test/%.o)
TSTEXE =

all: $(EXE)
	@echo clox has been compiled! Executable is name $(EXE)

debug: CFLAGS += -DDEBUG -g -O0
debug: $(EXE)

test: $(TSTEXE)

$(EXE): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LFLAGS) $(LIBS)

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	@mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

$(TSTEXE): %: $(OBJDIR)/test/%.o
	$(CC) $(CFLAGS) $(INCLUDES) -o $@ $< $(LFLAGS) $(LIBS)

$(OBJDIR)/test/%.o: $(TSTSRCDIR)/%.c
	@mkdir -p $(OBJDIR)/test
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

clean:
	rm -f $(OBJDIR)/*.o $(OBJDIR)/test/*.o $(EXE) $(TSTEXE)
