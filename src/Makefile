CC = ghc
CFLAGS = -Wall
EXEC = ztile-test
MODULE_MAIN_OBJ = ZTile.o
MODULES = PathFinding Test Util
MODULES_OBJS = $(MODULES:%=ZTile/%.o)
OBJS = $(MODULES_OBJS) $(MODULE_MAIN_OBJ) $(EXEC:%=%.o)

all: $(EXEC) hlint-report.html

$(OBJS): %.o: %.lhs
	$(CC) $(CFLAGS) $<

$(EXEC): $(OBJS)
	$(CC) $(CFLAGS) -O2 --make ztile-test
	./ztile-test

hlint-report.html: $(OBJS) $(EXEC)
	echo "No lint!" > hlint-report.html
	hlint . --report=hlint-report.html --color --hint hlint

clean:
	$(RM) $(OBJS) $(OBJS:%.o=%.hi)
