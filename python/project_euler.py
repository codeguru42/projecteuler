def sieve(n: int):
    ps: list[int] = [True] * (n + 1)
    ps[0] = False
    ps[1] = False
    for i in range(1, n):
        if ps[i]:
            for j in range(i * 2, n + 1, i):
                ps[j] = False
    return ps


def primes(n: int):
    ps: list[int] = sieve(n)
    for i, is_prime in enumerate(ps):
        if is_prime:
            yield i