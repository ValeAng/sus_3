#Variabile trasformata dalla famosa roba lunga orribile
posi = which(is.na(train$FIND_PPQ18SS_MONTH_DAT_MAX_FIN))
train[posi, "Richiesta_Prestito_18Mesi"] = 0
train[-posi, "Richiesta_Prestito_18Mesi"] = 1
train$Richiesta_Prestito_18Mesi = as.factor(train$Richiesta_Prestito_18Mesi)

#Variabile figli/no figli
posi = which(train$NUM_FIGLI=="0")
train[posi, "Figli"] = 0
train[-posi, "Figli"] = 1
train$Figli = as.factor(train$Figli)