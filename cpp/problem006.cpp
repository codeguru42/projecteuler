/*
The sum of the squares of the first ten natural numbers is,

1^2 + 2^2 + ... + 10^2 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)^2 = 55^2 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*/

#include <iostream>

template <class T>
T sumOfSquares(T n);

template <class T>
T squareOfSum(T n);

int main() {
  long n = 100;

  long sq = squareOfSum(n);
  long sum = sumOfSquares(n);

  std::cout << sq << " " << sum << " " << sq - sum << std::endl;

  return 0;
}

template <class T>
T sumOfSquares(T n) {
  return n * (n + 1) * (2 * n + 1) / 6;
}

template <class T>
T squareOfSum(T n) {
  int sum = n * (n + 1) / 2;
  return sum * sum;
}
