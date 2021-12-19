CC=ghc
SRC=Fib.hs
EXE=test.exe

.PHONY: all clean run

run: all
	@echo "Running..."
	./test.exe

all:
	$(CC) -c -O $(SRC)
	$(CC) --make -no-hs-main -optc-O test.c $(patsubst  %.hs,%,$(SRC)) -o $(EXE)

clean:
	rm -rf *.o *.hi $(EXE) $(patsubst  %.hs,%_stub.h,$(SRC))
