CC = ghc
CFLAGS = -Wall
MODULES = PathFinding Test Util
MODULES_OBJS = $(MODULES:%=ZTile/%.o)
OBJS = $(MODULES_OBJS)

all: $(OBJS) ZTile.o

$(OBJS): %.o: %.lhs
	$(CC) $(CFLAGS) $<

clean:
	$(RM) $(OBJS) ZTile.o