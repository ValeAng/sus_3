source("2_EDA.R")

#BILANCIO DEL DATASET 50/50
library(unbalanced)

set.seed(123)

X = train[, -1]
Y = train$TARG_TOT

under = ubUnder(X, Y, perc = 50, method = "percPos")

newtrain = cbind(under$X, under$Y)
View(newtrain)
dim(newtrain)
colnames(newtrain)[84] = "TARG_TOT"

table(newtrain$TARG_TOT)


require(ElemStatLearn)
require(caret)

set.seed(123)

#=================================
# K-fold Cross-Validation settings
#=================================

K = 5
R = 1
KCV <- trainControl(method = "repeatedcv", 
                    repeats = R, 
                    number=K)

#========================
# Models
#========================

#Questa è una RF con le variabile nuove e in cui ho tolto quelle vecchie (mi daà 51.15% su Beeviva)
require(randomForest)


set.seed(123)
grid.mtry <- data.frame(.mtry = 6)
fit2 <- caret::train(TARG_TOT ~ . -Codice_Cliente -FIND_PPQ_C_NUM_PRA -ALTR_PPQ_C_NUM_PRA -NUM_PPQ_C -FIND_PPQ_C_IMP_RES
                     -ALTR_PPQ_C_IMP_RES -IMP_PPQ_C -FIND_NUM_MEN_RES -ALTR_PPQ_C_NUM_MEN_RES -FIND_PPQ_C_IMP_MEN
                     -ALTR_PPQ_C_IMP_MEN -PPQ_C_IMP_MEN -FIND_CC_C_NUM_PRA_GRA -FIND_CC_C_NUM_PRA_TAS -ALTR_CC_C_NUM_PRA -ALTR_CC_C_NUM_PRA
                     -CC_C_NUM -FIND_CC_C_IMP_RES_TAS -FIND_CC_C_IMP_RES_GRA -ALTR_CC_C_IMP_RES -CC_C_IMP_RES
                     -FIND_CC_C_IMP_MEN_TAS -FIND_CC_C_IMP_MEN_GRA -ALTR_CC_C_IMP_MEN -CC_C_IMP_MEN, 
              data = newtrain,
              method = "rf",
              trControl = KCV,
              tuneGrid = grid.mtry)
fit2 


test$IMP_RED = ifelse(is.na(test$IMP_RED), mean(test$IMP_RED,na.rm = T), test$IMP_RED )
test$IMP_FAM = ifelse(is.na(test$IMP_FAM), mean(test$IMP_FAM,na.rm = T), test$IMP_FAM )
test$FIND_NUM_MEN_RES = ifelse(is.na(test$FIND_NUM_MEN_RES), mean(test$FIND_NUM_MEN_RES,na.rm = T), test$FIND_NUM_MEN_RES )
test$PPQ_NUM_MEN_RES = ifelse(is.na(test$PPQ_NUM_MEN_RES), mean(test$PPQ_NUM_MEN_RES,na.rm = T), test$PPQ_NUM_MEN_RES )
test$FIND_PPQ18_IMP_FIN = ifelse(is.na(test$FIND_PPQ18_IMP_FIN), mean(test$FIND_PPQ18_IMP_FIN, na.rm=T), test$FIND_PPQ18_IMP_FIN)
test$PPQ_18_IMP_FIN = ifelse(is.na(test$PPQ_18_IMP_FIN), mean(test$PPQ_18_IMP_FIN, na.rm=T), test$PPQ_18_IMP_FIN)

yhats = predict(fit2, newdata=test)
yhats = sort(yhats, decreasing = TRUE)

test$yhats = predict(fit2, newdata = test, type="prob")
test$yhats2 = test$yhats[[2]]

oss_per_beeviva = test %>%
  arrange(desc(yhats2)) %>%
  top_n(10000) 

oss_per_beeviva = oss_per_beeviva %>%
  select(Codice_Cliente) %>%
  top_n(10000)
write_csv(oss_per_beeviva, path = "C:/Users/vale/Desktop/prova.txt", append = TRUE)
varImp(fit2)

#------------------
#------------------
#Questa è una RF con le variabili vecchie e non quelle nuove (su Beeviva da 49.75%)
require(randomForest)

set.seed(123)
grid.mtry <- data.frame(.mtry = 6)
fit3 <- caret::train(TARG_TOT ~ . -Codice_Cliente -FIND_PPQ_C_NUM_PRA2 -ALTR_PPQ_C_NUM_PRA2 -NUM_PPQ_C2
                     -FIND_CC_C_NUM_PRA_GRA2 -FIND_CC_C_NUM_PRA_TAS2 -ALTR_CC_C_NUM_PRA2 -CC_C_NUM2
                     -prestiti_totali -prestiti_totali2 -saldo_totale -rata_totale -saldo_log, 
                     data = newtrain,
                     method = "rf",
                     trControl = KCV,
                     tuneGrid = grid.mtry)
fit3

test$IMP_RED = ifelse(is.na(test$IMP_RED), mean(test$IMP_RED,na.rm = T), test$IMP_RED )
test$IMP_FAM = ifelse(is.na(test$IMP_FAM), mean(test$IMP_FAM,na.rm = T), test$IMP_FAM )
test$FIND_NUM_MEN_RES = ifelse(is.na(test$FIND_NUM_MEN_RES), mean(test$FIND_NUM_MEN_RES,na.rm = T), test$FIND_NUM_MEN_RES )
test$PPQ_NUM_MEN_RES = ifelse(is.na(test$PPQ_NUM_MEN_RES), mean(test$PPQ_NUM_MEN_RES,na.rm = T), test$PPQ_NUM_MEN_RES )
test$FIND_PPQ18_IMP_FIN = ifelse(is.na(test$FIND_PPQ18_IMP_FIN), mean(test$FIND_PPQ18_IMP_FIN, na.rm=T), test$FIND_PPQ18_IMP_FIN)
test$PPQ_18_IMP_FIN = ifelse(is.na(test$PPQ_18_IMP_FIN), mean(test$PPQ_18_IMP_FIN, na.rm=T), test$PPQ_18_IMP_FIN)

yhats = predict(fit3, newdata=test)
yhats = sort(yhats, decreasing = TRUE)

test$yhats = predict(fit3, newdata = test, type="prob")
test$yhats2 = test$yhats[[2]]

oss_per_beeviva = test %>%
  arrange(desc(yhats2)) %>%
  top_n(10000) 

oss_per_beeviva = oss_per_beeviva %>%
  select(Codice_Cliente) %>%
  top_n(10000)
write_csv(oss_per_beeviva, path = "C:/Users/vale/Desktop/prova2.txt", append = TRUE)
