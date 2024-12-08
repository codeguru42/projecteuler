import itertools

import project_euler


def prime_pairs(n: int):
    is_prime = project_euler.sieve(n*n)
    ps = project_euler.primes(n)
    for p1, p2 in itertools.combinations(ps, 2):
        x = int(str(p1) + str(p2))
        y = int(str(p2) + str(p1))
        if is_prime[x] and is_prime[y]:
            yield p1, p2

def main():
    for x, y in prime_pairs(10_000):
        print(x, y)


if __name__ == "__main__":
    main()
