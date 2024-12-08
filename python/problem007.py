def sieve(n: int):
    primes: list[int] = [True] * (n + 1)
    primes[0] = False
    primes[1] = False
    for i in range(1, n):
        if primes[i]:
            for j in range(i * 2, n + 1, i):
                primes[j] = False
    return primes

def main():
    primes = sieve(2_000_000)
    for i, is_prime in enumerate(primes):
        if is_prime:
            print(i)


if __name__ == "__main__":
    main()