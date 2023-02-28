/*
 * Project Euler
 * Problem 28
 *
 * Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
 *
 * *21*  22   23   24  *25*
 *  20  * 7*   8  * 9*  10
 *  19    6  * 1*   2   11
 *  18  * 5*   4  * 3*  12
 * *17*  16   15   14  *13*
 * 
 * It can be verified that the sum of the numbers on the diagonals is 101.
 *
 * What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
 */

#include <iostream>

int spiralDiagonals(int n);
int negDiag(int n);
int posDiag(int n);
int sumN(int n);

int main() {
  int n = 5;

  std::cout << spiralDiagonals(n) << std::endl;

  return 0;
}

int spiralDiagonals(int n) {
  std::cout << negDiag(n) << " + " << posDiag(n) << std::endl;

  return negDiag(n) + posDiag(n) - 1;
}

int negDiag(int n) {
  std::cout << "negDiag(" << n << ")" << std::endl;

  if (n == 1) return 1;

  std::cout << 4 * sumN(n - 2) + 2 * n << std::endl;

  return negDiag(n - 2) + 4 * sumN(n - 2) + 2 * n;
}

int posDiag(int n) {
  std::cout << "posDiag(" << n << ")" << std::endl;

  if (n == 1) return 1;

  std::cout << 8 * sumN(n - 2) + 4 * n - 2 << std::endl;

  return posDiag(n - 2) + 8 * sumN(n - 2) + 4 * n - 2;
}

int sumN(int n) {
  return n * (n + 1) / 2;
}
