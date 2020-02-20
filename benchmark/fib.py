
print("Iterative Fibonacci example")

def fib(n):
    prevprevn = 0
    prevn = 0
    curn = 1

    i = 2

    while i <=n:
        prevprevn = prevn

        prevn = curn

        curn = prevprevn + prevn

        i = i + 1
    
    return curn

print("Enter a number: ")
n = int(input())

print("Fibonacci of", str(n), "is", str(fib(n)))