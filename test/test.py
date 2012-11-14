def b():
    x = 1
    def a():
        nonlocal x
        x = 2
        print(x)
    a()
    print(x)

print (b())

