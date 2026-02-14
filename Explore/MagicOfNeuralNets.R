# https://blog.ephorie.de/understanding-the-magic-of-neural-networks
library(neuralnet)
library(NeuralNetTools)

qualities <- matrix (c(1, 1, 1, 0, 0, 0,
                       0, 1, 0, 1, 1, 0,
                       1, 0, 0, 1, 0, 1), byrow = TRUE, nrow = 3)

colnames(qualities) <- c("big_ears", "big_eyes", "big_teeth", "kindly", "wrinkled", "handsome")
rownames(qualities) <- c("wolf", "grannie", "woodcutter")
qualities
##            big_ears big_eyes big_teeth kindly wrinkled handsome
## wolf              1        1         1      0        0        0
## grannie           0        1         0      1        1        0
## woodcutter        1        0         0      1        0        1

actions <- matrix (c(1, 1, 1, 0, 0, 0, 0,
                     0, 0, 0, 1, 1, 1, 0,
                     0, 0, 0, 1, 0, 1, 1), byrow = TRUE, nrow = 3)
colnames(actions) <- c("run_away", "scream", "look_for_woodcutter", "talk_to", "approach", "offer_food", "is_rescued_by")
rownames(actions) <- rownames(qualities)
actions
##            run_away scream look_for_woodcutter talk_to approach offer_food
## wolf              1      1                   1       0        0          0
## grannie           0      0                   0       1        1          1
## woodcutter        0      0                   0       1        0          1
##            is_rescued_by
## wolf                   0
## grannie                0
## woodcutter             1
data <- cbind(qualities, actions)

# train the neural network (NN)
set.seed(123) # for reproducibility
neuralnetwork <- neuralnet(run_away + scream + look_for_woodcutter + talk_to + approach + 
                             offer_food + is_rescued_by ~ 
                             big_ears + big_eyes + big_teeth + kindly + wrinkled + handsome,
                           data = data, hidden = 3, exclude = c(1, 8, 15, 22, 26, 30, 34, 38, 42, 46), 
                           lifesign = "minimal", linear.output = FALSE)
## hidden: 3    thresh: 0.01    rep: 1/1    steps:      48  error: 0.01319  time: 0.01 secs

# plot the NN
par_bkp <- par(mar = c(0, 0, 0, 0)) # set different margin to minimize cutoff text
plotnet(neuralnetwork, bias = FALSE)


par(par_bkp)

# which actions were learned by the net?
round(neuralnetwork$net.result[[1]])
##            [,1] [,2] [,3] [,4] [,5] [,6] [,7]
## wolf          1    1    1    0    0    0    0
## grannie       0    0    0    1    1    1    0
## woodcutter    0    0    0    1    0    1    1

