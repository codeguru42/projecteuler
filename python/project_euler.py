import itertools


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
            
def grouper(iterable, n, *, incomplete='fill', fillvalue=None):
    "Collect data into non-overlapping fixed-length chunks or blocks."
    # grouper('ABCDEFG', 3, fillvalue='x') → ABC DEF Gxx
    # grouper('ABCDEFG', 3, incomplete='strict') → ABC DEF ValueError
    # grouper('ABCDEFG', 3, incomplete='ignore') → ABC DEF
    iterators = [iter(iterable)] * n
    match incomplete:
        case 'fill':
            return itertools.zip_longest(*iterators, fillvalue=fillvalue)
        case 'strict':
            return zip(*iterators, strict=True)
        case 'ignore':
            return zip(*iterators)
        case _:
            raise ValueError('Expected fill, strict, or ignore')