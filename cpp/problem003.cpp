/*
 * Project Euler
 * Problem 3
 *
 * The prime factors of 13195 are 5, 7, 13 and 29.
 *
 * What is the largest prime factor of the number 600851475143 ?
 */

#include <iostream>
#include <list>
#include <cmath>

/*
 * Prints any container which supports forward iterators.
 */
template <class T>
void print(T t);

/*
 * Generates a list of all primes less than n using the Sieve of Eratosthenes.
 */
template <class T>
std::list<T> primeFactors(T n);

template <class T>
std::list<T> primes(T n);

int main() {
  const long n = 73033; // 199 * 367
//  std::list<long> pf = primeFactors(n);
  std::list<int> pf = primeFactors(100);
  print(pf);

  return 0;
}

template <class T>
void print(T t) {
  typename T::iterator itr = t.begin();
  
  std::cout << "[";

  if (itr != t.end()) {
    std::cout << *itr;
    ++itr;
  }

  for(; itr != t.end(); ++itr) {
    std::cout << ", " << *itr;

  }

  std::cout << "]";
}

template <class T>
std::list<T> primeFactors(T n) {
  std::list<T> pr = primes(n);
  std::list<T> pf;

  for (typename std::list<T>::iterator itr = pr.begin(); itr != pr.end(); ++itr) {
    if (n % *itr == 0) {
      pf.push_back(*itr);
    }
  }

  return pf;
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
    std::cout << "pr:";
    print(pr);
    std::cout << std::endl;

    for (T i = minPrime; i <= n; i += minPrime) {
      typename std::list<T>::iterator itr = std::find(nums.begin(), nums.end(), i);

      if (itr != nums.end()) {
//        std::cout << "Removing " << *itr << std::endl;
        nums.erase(itr);
      }
    }

    minPrime = *nums.begin();
  }

  pr.insert(pr.end(), nums.begin(), nums.end());

  std::cout << "pr:";
  print(pr);
  std::cout << std::endl;

  return pr;
}
