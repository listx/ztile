CC = ghc
CFLAGS = -Wall
MODULE_MAIN_OBJ = ZTile.o
MODULES = PathFinding Test Util
MODULES_OBJS = $(MODULES:%=ZTile/%.o)
OBJS = $(MODULES_OBJS) $(MODULE_MAIN_OBJ)

all: $(OBJS) ZTile.o
	rm -f hlint-report.html
	hlint . --report=hlint-report.html --color --hint hlint

$(OBJS): %.o: %.lhs
	$(CC) $(CFLAGS) $<

clean:
	$(RM) $(OBJS) ZTile.o
