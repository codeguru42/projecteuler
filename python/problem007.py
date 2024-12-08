import project_euler


def main():
    primes = list(project_euler.primes(2_000_000))
    print(primes[10_000]) # Zero-based index


if __name__ == "__main__":
    main()
