library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)

###### INSERIRE QUI IL CODICE DELLA ROBA CHE VOLETE FARE

# Terminate parallel
stopCluster(cl)