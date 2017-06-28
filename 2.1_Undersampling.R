source("1_import.R")

library(unbalanced)

set.seed(123)

X = train[, -1]
Y = train$TARG_TOT

under = ubUnder(X, Y, perc = 50, method = "percPos")

newtrain = cbind(under$X, under$Y)
colnames(newtrain)[74] = "TARG_TOT"

dim(newtrain)

table(newtrain$TARG_TOT)
