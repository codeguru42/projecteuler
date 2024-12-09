def cubes(start=0):
    x = start
    while True:
        yield x*x*x
        x += 1

def main():
    for x in cubes():
        if x > 1_000_000_000:
            break
        print(x)


if __name__ == '__main__':
    main()