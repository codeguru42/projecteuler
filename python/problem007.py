import project_euler


def main():
    primes = project_euler.sieve(2_000_000)
    count = 0
    for i, is_prime in enumerate(primes):
        if is_prime:
            count += 1
        if count == 10_001:
            print(i)
            break


if __name__ == "__main__":
    main()
