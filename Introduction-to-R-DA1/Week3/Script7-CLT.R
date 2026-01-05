nrep <- 1000
p_50_0.5 <- rbinom(nrep, size = 50, prob = 0.5) / 50
p_50_0.8 <- rbinom(nrep, size = 50, prob = 0.8) / 50
p_1000_0.5 <- rbinom(nrep, size = 1000, prob = 0.5) / 1000
p_1000_0.8 <- rbinom(nrep, size = 1000, prob = 0.8) / 1000

boxplot(list(p_50_0.5, p_50_0.8, p_1000_0.5, p_1000_0.8))


boxplot(list(p_50_0.5 = sqrt(50) * (p_50_0.5 - 0.5) / sqrt(0.5 * 0.5),
             p_50_0.8 = sqrt(50) * (p_50_0.8 - 0.8) / sqrt(0.8 * 0.2),
             p_1000_0.5 = sqrt(1000) * (p_1000_0.5 - 0.5) / sqrt(0.5 * 0.5),
             p_1000_0.8 = sqrt(1000) * (p_1000_0.8 - 0.8) / sqrt(0.8 * 0.2),
             Z = rnorm(nrep)))
