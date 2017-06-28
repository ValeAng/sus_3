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


### POLISH ###

# COD_RES = Tipologia Residenza
newtrain$COD_RES = as.factor(newtrain$COD_RES)

# COD_STA_CIV = Stato civile
newtrain$COD_STA_CIV = as.factor(newtrain$COD_STA_CIV)

# NUM_FIGLI = Numero figli
newtrain$NUM_FIGLI = as.factor(newtrain$NUM_FIGLI)
levels(newtrain$NUM_FIGLI) = c("0", "1-3", ">3")

# FLG_SEX = Sesso
newtrain$FLG_SEX = as.factor(newtrain$FLG_SEX)

# PROF = Professione
newtrain$PROF = as.factor(newtrain$PROF)