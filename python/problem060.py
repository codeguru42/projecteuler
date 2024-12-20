import itertools

import networkx as nx

import project_euler

is_prime = project_euler.sieve(100_000_000)


def is_prime_pair(p1: int, p2: int) -> bool:
    x = int(str(p1) + str(p2))
    y = int(str(p2) + str(p1))
    return is_prime[x] and is_prime[y]


def main():
    g = nx.Graph()
    ps = project_euler.primes(10_000)
    for p_group in project_euler.grouper(ps, 100, incomplete='ignore'):
        # Check primes against those already in the graph
        for p1 in list(g.nodes.keys()):
            for p2 in p_group:
                if is_prime_pair(p1, p2):
                    g.add_edge(p1, p2)
        # Add primes from the next group
        for p1, p2 in itertools.combinations(p_group, 2):
            if is_prime_pair(p1, p2):
                g.add_edge(p1, p2)
                
        for clique in nx.clique.enumerate_all_cliques(g):
            if len(clique) == 5:
                print(clique)
                print(sum(clique))
                return


if __name__ == "__main__":
    main()
