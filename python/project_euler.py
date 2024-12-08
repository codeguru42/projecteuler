def sieve(n: int):
    primes: list[int] = [True] * (n + 1)
    primes[0] = False
    primes[1] = False
    for i in range(1, n):
        if primes[i]:
            for j in range(i * 2, n + 1, i):
                primes[j] = False
    return primes
