/*
 * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
 *
 * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 */

#include <iostream>

/*
 * Calculate the Greatest Common Divisor of two numbers using the Euclidean Algorithm.
 */
template <class T>
T gcd(T m, T n);

/*
 * Calculate the Least Common Multiple of two numbers.
 */
template <class T>
T lcm(T m, T n);

int main() {
  unsigned long l = 1;

  for (unsigned long i = 1; i <= 20; ++i) {
    l = lcm(i, l);

    std::cout << l << std::endl;
  }

  return 0;
}

template <class T>
T gcd(T m, T n) {
  if (n % m == 0) return m;

  return gcd(n % m, m);
}

template <class T>
T lcm(T m, T n) {
  return m / gcd(m, n) * n;
}
