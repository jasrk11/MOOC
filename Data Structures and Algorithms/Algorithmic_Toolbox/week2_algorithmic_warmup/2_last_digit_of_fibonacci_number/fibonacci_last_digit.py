# Uses python3
import sys

def find_pisano_period(10):
    previous = 0
    current = 1

    L = [None] * (10*10)
    L[0] = 0
    L[1] = 1

    for i in range(10*10):
        previous, current = current, (previous + current) % 10

        if previous == 0 and current == 1:
            period = i+1
            break

        L[i+2] = L[i+1] + current

    period_sum = L[i-1]


def get_fibonacci_last_digit(n):
    if n <= 1:
        return n

    pisano_period = find_pisano_period(10)

    quot = n/pisano_period
    rem = n%pisano_period

    period_sum = 1
    rem_sum = 0

    previous = 0
    current  = 1

    # find sum of Fibonacci numbers for the period
    for i in range(1, pisano_period):
        previous, current = current, (previous + current) % 10

        if rem == i:
            rem_sum = period_sum

        period_sum += current


    return period_sum*quot + rem_sum

if __name__ == '__main__':
    input = sys.stdin.read()
    n = int(input)
    print(get_fibonacci_last_digit(n))
