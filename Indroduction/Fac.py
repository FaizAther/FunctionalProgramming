def fac_1(n):
    if ( n <= 1 ):
        return 1
    else:
        return n * fac_1(n-1)


def fac_2(n):
    acc=1
    while(True):
        if (n<=1):
            return acc
        else:
            n=n-1
            acc=n*acc

for n in range(0,10):
    print "fac_1 of {} is... {}".format(n, fac_1(n))
    print "fac_2 of {} is... {}".format(n, fac_2(n))
