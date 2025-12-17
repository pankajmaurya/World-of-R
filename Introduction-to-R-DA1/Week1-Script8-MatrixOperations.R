x <- matrix(sample(1:9), 3, 3)
y <- matrix(sample(1:9), 3, 3)
print(x)
print(y)
# Produce matrix multiplication
print(x %*% y)

# bind columns
print(cbind(x, y))

# bind rows
print(rbind(x, y))

print(x)
print(x[1:2, 1:3])

print(x[, -2])

# A matrix can also be treated as a vector with its columns combined as vectors. 
# If we use square-bracket indexing with a single index vector, this is the interpretation that is used.
print(x[2])
print(x[8])

b = matrix(sample(1:9, 3), 3, 1)
print(b)

print(sx <- solve(x, b))

# check sx * x = b
print(x %*% sx)
