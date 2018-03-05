# Uses python3
import sys

def find_pisano_period(m):
    previous = 0
    current = 1

    for i in range(m*m):
        previous, current = current, (previous + current) % m

        if previous == 0 and current == 1:
            return i+1

def get_fibonacci_huge_smart(n, m):
    pisano_period = find_pisano_period(m)
    # print pisano_period

    n %= pisano_period

    if n == 0:
        return 0

    previous = 0
    current  = 1

    for _ in range(n-1):
        previous, current = current, (previous + current) % m

    return current


def get_fibonacci_huge_naive(n, m):
    if n <= 1:
        return n

    previous = 0
    current  = 1

    for _ in range(n - 1):
        previous, current = (current) % m, (previous + current) % m

    return current % m

if __name__ == '__main__':
    input = sys.stdin.read();
    n, m = map(int, input.split())
    print(get_fibonacci_huge_smart(n, m))
