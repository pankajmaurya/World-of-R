str(Titanic)
Titanic.adult <- Titanic[, , "Adult", ]
Titanic.adult
str(Titanic.adult)
str(Titanic.adult["3rd",,])
# The third dimension is survival status
print(apply(Titanic.adult, MARGIN = 3, sum))

# When summarized over multiple dimensions
print(apply(Titanic.adult, MARGIN = c(2, 3), sum))

aperm(Titanic.adult, c(2, 3, 1))

print(apply(Titanic.adult["3rd",,], MARGIN = c(2), sum))

print(apply(Titanic.adult, MARGIN = c(1, 3), sum))

# Simpsons Paradox in UCB data.
print(dimnames(UCBAdmissions))

aperm(UCBAdmissions, c(2, 3, 1))
str(UCBAdmissions)
print(apply(UCBAdmissions, c(1, 2), sum))

# prop.table converts counts into proportions (in this case, by column 2 of Gender). 
print(prop.table(apply(UCBAdmissions, c(1, 2), sum), margin = 2))

admittedPercent <- function(x) round(100 * prop.table(x, margin = 2)[1, ])
print(admittedPercent(apply(UCBAdmissions, c(1, 2), sum)))

print(admittedPercent(UCBAdmissions[,, 1]))
# Admitted percentages per department (male vs female)
print(apply(UCBAdmissions, 3, admittedPercent))

# Application counts per department
print(apply(UCBAdmissions, c(2, 3), sum))
