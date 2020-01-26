#include <stdlib.h>
#include <stdio.h>

int fib(int n) {
    int prevprevn = 0;
    int prevn = 0;
    int curn = 1;

    int i = 2;

    while(i <= n) {
        prevprevn = prevn;

        prevn = curn;

        curn = prevprevn + prevn;

        i = i + 1;
    }

    return curn;
}

int main() {

    printf("Iterative Fibonacci example\n");

    printf("Enter a number: \n");
    int n;
    scanf("%d", &n);

    printf("Fibonacci of %d is %d\n", n, fib(n));

}