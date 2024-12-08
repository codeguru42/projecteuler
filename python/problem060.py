import project_euler

is_prime = project_euler.sieve(400_000_000)


def is_prime_pair(p1: int, p2: int) -> bool:
    x = int(str(p1) + str(p2))
    y = int(str(p2) + str(p1))
    return is_prime[x] and is_prime[y]


def main():
    ps = project_euler.primes(200000)
    four = [3, 7, 109, 673]
    for p in ps:
        if p > 673:
            print(p)
            if all(is_prime_pair(p, x) for x in four):
                print(sum(four) + p)
                break


if __name__ == "__main__":
    main()
