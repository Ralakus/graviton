
io.write("Iterative Fibonacci example\n")

function fib(n) 
    prevprevn = 0
    prevn = 0
    curn = 1

    i = 2

    while i <=n do
        prevprevn = prevn

        prevn = curn

        curn = prevprevn + prevn

        i = i + 1
    end
    
    return curn
end

io.write("Enter a number: \n")
n = io.read("*n")

io.write("Fibonacci of ", n, " is ", fib(n), '\n')