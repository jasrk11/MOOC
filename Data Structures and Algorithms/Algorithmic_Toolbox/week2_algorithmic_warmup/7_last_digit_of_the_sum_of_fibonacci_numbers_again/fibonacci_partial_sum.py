# Uses python3
import sys

def find_pisano_period(m):
    previous = 0
    current = 1


    sum_L = [None] * (m*m)
    sum_L[0] = 0
    sum_L[1] = 1

    for i in range(m*m):
        previous, current = current, (previous + current) % m

        if previous == 0 and current == 1:
            return i+1, sum_L[:i+2]

        sum_L[i+2] = (sum_L[i+1] + current) % m

def fibonacci_sum(from_, to):
    m=10
    pisano_period, sum_L = find_pisano_period(m)

    Q = (from_-1) // pisano_period
    R = (from_-1) % pisano_period

    sum_till_from_1 = int((sum_L[pisano_period] * Q + sum_L[R]) % m )

    Q = to // pisano_period
    R = to % pisano_period

    sum_till_to = int((sum_L[pisano_period] * Q + sum_L[R]) % m )

    diff = sum_till_to-sum_till_from_1
    if diff < 0:
        return 10+diff
    return diff

def fibonacci_partial_sum_naive(from_, to):
    sum = 0

    current = 0
    next  = 1

    for i in range(to + 1):
        if i >= from_:
            sum += current

        current, next = next, current + next

    return sum % 10


if __name__ == '__main__':
    input = sys.stdin.read();
    from_, to = map(int, input.split())
    print(fibonacci_sum(from_, to))