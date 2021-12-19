#include "/home/debian/.ghcup/ghc/8.10.4/lib/ghc-8.10.4/include/HsFFI.h"
#ifdef __GLASGOW_HASKELL__
#include "Fib_stub.h"
#endif
#include <stdio.h>

int main(int argc, char *argv[])
{
    int i;
    hs_init(&argc, &argv);

    i = fibonacci_hs(42);
    printf("Fibonacci: %d\n", i);

    hs_exit();
    return 0;
}