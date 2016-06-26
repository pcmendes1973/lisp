# Solution for Problem 203 in Project Euler
# Paulo Mendes, 6-JAN-2016

squaredprimes = [4, 9, 25, 49, 121, 169, 289, 361, 529, 841, 961, 1369, 1681, 1849, 2209]

def squarefree (n):
    return not any ([n % i ==0 and n >= i for i in squaredprimes])
    
def candidates (n):
    triangle = [1]
    s = set()
    for i in range (0, n - 1):
        triangle = [j + k for j, k in zip ([0] + triangle, triangle + [0])]
        s = s | set (triangle [:len(triangle)//2])
    return s

print (sum ([i for i in candidates (51) if squarefree (i)]))
