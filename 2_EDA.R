source("1_import.R")

# Cambio il target
train$TARG_TOT = as.factor(train$TARG_TOT)

# COD_RES = Tipologia Residenza
train$COD_RES = as.factor(train$COD_RES)
test$COD_RES = as.factor(test$COD_RES)

# COD_STA_CIV = Stato civile
train$COD_STA_CIV = as.factor(train$COD_STA_CIV)
test$COD_STA_CIV = as.factor(test$COD_STA_CIV)

# NUM_FIGLI = Numero figli
train$NUM_FIGLI = as.factor(train$NUM_FIGLI)
levels(train$NUM_FIGLI) = c("0", "1-3", ">3")

test$NUM_FIGLI = as.factor(test$NUM_FIGLI)
levels(test$NUM_FIGLI) = c("0", "1-3", ">3")

# FLG_SEX = Sesso
train$FLG_SEX = as.factor(train$FLG_SEX)
test$FLG_SEX = as.factor(test$FLG_SEX)

# PROF = Professione
train$PROF = as.factor(train$PROF)
test$PROF = as.factor(test$PROF)

#################################
# Variabili del secondo blocco###
#################################

# prestiti personali in corso - numero pratiche
train$FIND_PPQ_C_NUM_PRA2 = as.factor(ifelse(train$FIND_PPQ_C_NUM_PRA==0,0,1))
test$FIND_PPQ_C_NUM_PRA2 = as.factor(ifelse(test$FIND_PPQ_C_NUM_PRA==0,0,1))

#altri prestiti in corso - numero pratiche
train$ALTR_PPQ_C_NUM_PRA2 = as.factor(ifelse(train$ALTR_PPQ_C_NUM_PRA ==0,0,1))
test$ALTR_PPQ_C_NUM_PRA2 = as.factor(ifelse(test$ALTR_PPQ_C_NUM_PRA ==0,0,1))

#totale prestiti in corso - numero pratiche
train$NUM_PPQ_C2 = as.factor(ifelse(train$NUM_PPQ_C==0,0,1))
test$NUM_PPQ_C2 = as.factor(ifelse(test$NUM_PPQ_C==0,0,1))

#numero mensilità residue al saldo; ho un NA che imputo
#summary(train$FIND_NUM_MEN_RES)
train[3663,"FIND_NUM_MEN_RES"] = 7
train[3663,"PPQ_NUM_MEN_RES"] = 7

#prestiti finalizzati gratuiti in corso - numero pratiche
train$FIND_CC_C_NUM_PRA_GRA2 = as.factor(ifelse(train$FIND_CC_C_NUM_PRA_GRA ==0,0,1))
test$FIND_CC_C_NUM_PRA_GRA2 = as.factor(ifelse(test$FIND_CC_C_NUM_PRA_GRA ==0,0,1))


#prestiti finalizzati a tasso in corso - numero pratiche
train$FIND_CC_C_NUM_PRA_TAS2 = as.factor(ifelse(train$FIND_CC_C_NUM_PRA_TAS==0,0,1))
test$FIND_CC_C_NUM_PRA_TAS2 = as.factor(ifelse(test$FIND_CC_C_NUM_PRA_TAS==0,0,1))

# altri prestiti finalizzati gratuiti in corso - numero pratiche
train$ALTR_CC_C_NUM_PRA2 = as.factor(ifelse(train$ALTR_CC_C_NUM_PRA ==0,0,1))
test$ALTR_CC_C_NUM_PRA2 = as.factor(ifelse(test$ALTR_CC_C_NUM_PRA ==0,0,1))

# totale prestiti finalizzati gratuiti in corso - numero pratiche
train$CC_C_NUM2 = as.factor(ifelse(train$CC_C_NUM ==0,0,1))
test$CC_C_NUM2 = as.factor(ifelse(test$CC_C_NUM ==0,0,1))

#cliente in possesso di carta
train$CRT_PRE_C_FLG_PRE = as.factor(train$CRT_PRE_C_FLG_PRE)
test$CRT_PRE_C_FLG_PRE = as.factor(test$CRT_PRE_C_FLG_PRE)


#######################
### Nuove variabili ###
#######################

#Creo una variabile che mi dica se ha almeno un prestito in corso
train$prestiti_totali = train$NUM_PPQ_C + train$CC_C_NUM
test$prestiti_totali = test$NUM_PPQ_C + test$CC_C_NUM

train$prestiti_totali2 = ifelse(train$prestiti_totali==0,0,1)
train$prestiti_totali2= as.factor(train$prestiti_totali2)
test$prestiti_totali2 = ifelse(test$prestiti_totali==0,0,1)
test$prestiti_totali2= as.factor(test$prestiti_totali2)

#Creo una variabile che mi dica qual è il totale del saldo 
train$saldo_totale = train$IMP_PPQ_C + train$CC_C_IMP_RES
test$saldo_totale = test$IMP_PPQ_C + test$CC_C_IMP_RES

#Creo una variabile che mi dica qual è l'importo rata totale
train$rata_totale = train$PPQ_C_IMP_MEN + train$CC_C_IMP_MEN
test$rata_totale = test$PPQ_C_IMP_MEN + test$CC_C_IMP_MEN

train$saldo_log = log(train$saldo_totale + 1)
test$saldo_log = log(test$saldo_totale + 1)

# Rimuovo la variabile con solo NA e la sostituisco 
# con una dummy creata ad hoc
# Tolgo anche quella con tanti missin

train <- train %>%
  mutate(Richiesta_prestito_18mesi = ifelse(is.na(FIND_PPQ18SS_MONTH_DAT_MAX_FIN),0,1)) %>%
  select(-FIND_PPQ18SS_MONTH_DAT_MAX_FIN, -ANZ_BAN)

train$Richiesta_prestito_18mesi <- as.factor(train$Richiesta_prestito_18mesi)

test <- test %>%
  mutate(Richiesta_prestito_18mesi = ifelse(is.na(FIND_PPQ18SS_MONTH_DAT_MAX_FIN),0,1)) %>%
  select(-FIND_PPQ18SS_MONTH_DAT_MAX_FIN, -ANZ_BAN)

test$Richiesta_prestito_18mesi <- as.factor(test$Richiesta_prestito_18mesi)


train <- train %>%
  mutate(Figli = ifelse(NUM_FIGLI == 0, 0, 1))

train$Figli <- as.factor(train$Figli)

test <- test %>%
  mutate(Figli = ifelse(NUM_FIGLI == 0, 0, 1))

test$Figli <- as.factor(test$Figli)


# Primo tentativo
train <- train %>%
  select(-ANZ_RES, -ANZ_PROF) %>%
  filter(!is.na(PPQ_18_IMP_FIN), !is.na(FIND_PPQ18_IMP_FIN))

test <- test %>%
  select(-ANZ_RES, -ANZ_PROF)

train$IMP_FAM[which(train$TARG_TOT == 0 & is.na(train$IMP_FAM))] <- 1735.259
train$IMP_FAM[which(train$TARG_TOT == 1 & is.na(train$IMP_FAM))] <- 1948.663

train$IMP_RED[which(train$TARG_TOT == 0 & is.na(train$IMP_RED))] <- 1328.747
train$IMP_RED[which(train$TARG_TOT == 1 & is.na(train$IMP_RED))] <- 1551.051

