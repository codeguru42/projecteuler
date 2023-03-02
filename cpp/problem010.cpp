/*
 * Project Euler
 * Problem 10
 *
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 * 
 * Find the sum of all the primes below two million.
 */

#include <iostream>
#include <list>
#include <cmath>
#include <numeric>

/*
 * Prints any container which supports forward iterators.
 */
template <class T>
void print(T t, int n = 10);

/*
 * Generates a list of all primes less than n using the Sieve of Eratosthenes.
 */
template <class T>
std::list<T> primes(T n);

int main() {
  std::cout << "Generating primes..." << std::endl;
  const long n = 3000;
  std::list<long> pf = primes(n);
  print(pf);
  std::cout << "\n" << pf.size() << std::endl;

  std::cout << "Summing..." << std::endl;
  std::list<long> sum = std::list<long>(pf.size());
  std::partial_sum(pf.begin(), pf.end(), sum.begin());
  print(sum);
  std::cout << std::endl;


  std::cout << *(--sum.end()) << std::endl;

  return 0;
}

template <class T>
void print(T t, int n) {
  std::cout << "n:" << n << std::endl;

  typename T::iterator last = t.end();
  int i = 0;
  
  last--;
  std::cout << "[";

  for(typename T::iterator itr = t.begin(); itr != t.end(); ++itr) {
    std::cout << *itr << ", " ;

    if (i == n) {
      std::cout << std::endl << " ";
      i = 0;
    }

    ++i;
  }

  std::cout << *last << "]";
}

template <class T>
std::list<T> primes(T n) {
  std::list<T> nums;

  for (T i = 2; i <= n; ++i) {
    nums.push_back(i);
  }

  std::list<T> pr;

  T sqrtN = static_cast<T>(std::sqrt(n));
  T minPrime = *nums.begin();

  while (minPrime <= sqrtN) {
    pr.push_back(minPrime);
/*
    std::cout << "pr:";
    print(pr);
    std::cout << std::endl;
*/

    for (T i = minPrime; i <= n; i += minPrime) {
      typename std::list<T>::iterator itr = std::find(nums.begin(), nums.end(), i);

      if (itr != nums.end()) {
        nums.erase(itr);
      }
    }

    minPrime = *nums.begin();
  }

  pr.insert(pr.end(), nums.begin(), nums.end());

/*
  std::cout << "pr:";
  print(pr);
  std::cout << std::endl;
*/

  return pr;
}
