source("1_import.R")

train$COD_RES = as.factor(train$COD_RES)
summary(train$COD_RES)

train$COD_STA_CIV = as.factor(train$COD_STA_CIV)
summary(train$COD_STA_CIV)

train$NUM_FIGLI = as.factor(train$NUM_FIGLI)
summary(train$NUM_FIGLI)

train$FLG_SEX = as.factor(train$FLG_SEX)
summary(train$FLG_SEX)

train$PROF = as.factor(train$PROF)
summary(train$PROF)


hist(train$FIND_PPQ_C_NUM_PRA) #prestiti personali in corso - numero pratiche
summary(train$FIND_PPQ_C_NUM_PRA)
str(train$FIND_PPQ_C_NUM_PRA)
train$FIND_PPQ_C_NUM_PRA2 = ifelse(train$FIND_PPQ_C_NUM_PRA==0,0,1)
train$FIND_PPQ_C_NUM_PRA2 = as.factor(train$FIND_PPQ_C_NUM_PRA2)
summary(train$FIND_PPQ_C_NUM_PRA2)

hist(train$ALTR_PPQ_C_NUM_PRA) #altri prestiti in corso - numero pratiche
summary(train$ALTR_PPQ_C_NUM_PRA)
train$ALTR_PPQ_C_NUM_PRA2 = ifelse(train$ALTR_PPQ_C_NUM_PRA ==0,0,1)
train$ALTR_PPQ_C_NUM_PRA2 = as.factor(train$ALTR_PPQ_C_NUM_PRA2)
summary(train$ALTR_PPQ_C_NUM_PRA2)

hist(train$NUM_PPQ_C) #totale prestiti in corso - numero pratiche
train$NUM_PPQ_C2 = ifelse(train$NUM_PPQ_C==0,0,1)
train$NUM_PPQ_C2 = as.factor(train$NUM_PPQ_C2)
summary(train$NUM_PPQ_C2)

hist(train$FIND_PPQ_C_IMP_RES) #prestiti personali in corso - residuo al saldo
hist(log(train$FIND_PPQ_C_IMP_RES+1)) #anche con log non sistemo molto
summary(train$FIND_PPQ_C_IMP_RES)

hist(train$ALTR_PPQ_C_IMP_RES) #altri prestiti personali in corso - residuo al saldo
hist(log(train$ALTR_PPQ_C_IMP_RES +1))
summary(train$ALTR_PPQ_C_IMP_RES)

hist(train$IMP_PPQ_C) # totale prestiti in corso
hist(log(train$IMP_PPQ_C +1))
summary(train$IMP_PPQ_C)

hist(train$FIND_NUM_MEN_RES) #numero mensilità residue al saldo; ho un NA
summary(train$FIND_NUM_MEN_RES)

hist(train$FIND_NUM_MEN_RES) #numero mensilità residue al saldo; ho un NA
summary(train$FIND_NUM_MEN_RES)

hist(train$ALTR_PPQ_C_NUM_MEN_RES) #ho un NA
summary(train$ALTR_PPQ_C_NUM_MEN_RES)

hist(train$PPQ_NUM_MEN_RES) #ho un NA
summary(train$PPQ_NUM_MEN_RES)

hist(train$FIND_PPQ_C_IMP_MEN) #prestiti personali in corso - importo rata
summary(train$FIND_PPQ_C_IMP_MEN)
hist(log(train$FIND_PPQ_C_IMP_MEN+1))

hist(train$ALTR_PPQ_C_IMP_MEN) #altri prestiti in corso - importo rata
summary(train$ALTR_PPQ_C_IMP_MEN)

hist(train$PPQ_C_IMP_MEN)#totale prestiti in corso - importo rata
summary(train$PPQ_C_IMP_MEN)

hist(train$FIND_CC_C_NUM_PRA_GRA) #prestiti finalizzati gratuiti in corso - numero pratiche
train$FIND_CC_C_NUM_PRA_GRA2 = ifelse(train$FIND_CC_C_NUM_PRA_GRA ==0,0,1)
train$FIND_CC_C_NUM_PRA_GRA2 = as.factor(train$FIND_CC_C_NUM_PRA_GRA2)
summary(train$FIND_CC_C_NUM_PRA_GRA2)

hist(train$FIND_CC_C_NUM_PRA_TAS) #prestiti finalizzati a tasso in corso - numero pratiche
train$FIND_CC_C_NUM_PRA_TAS2 = ifelse(train$FIND_CC_C_NUM_PRA_TAS==0,0,1)
train$FIND_CC_C_NUM_PRA_TAS2 = as.factor(train$FIND_CC_C_NUM_PRA_TAS2)
summary(train$FIND_CC_C_NUM_PRA_TAS2)

hist(train$ALTR_CC_C_NUM_PRA) # altri prestiti finalizzati gratuiti in corso - numero pratiche
train$ALTR_CC_C_NUM_PRA2 = ifelse(train$ALTR_CC_C_NUM_PRA ==0,0,1)
train$ALTR_CC_C_NUM_PRA2 = as.factor(train$ALTR_CC_C_NUM_PRA2)
summary(train$ALTR_CC_C_NUM_PRA2)

hist(train$CC_C_NUM) # totale prestiti finalizzati gratuiti in corso - numero pratiche
train$CC_C_NUM2 = ifelse(train$CC_C_NUM ==0,0,1)
train$CC_C_NUM2 = as.factor(train$CC_C_NUM2)
summary(train$CC_C_NUM2)

hist(train$FIND_CC_C_IMP_RES_TAS) #prestiti finalizzati a tasso in corso - importo residuo
summary(train$FIND_CC_C_IMP_RES_TAS)

hist(train$FIND_CC_C_IMP_RES_GRA) #prestiti finalizzati gratuiti in corso - importo residuo 
summary(train$FIND_CC_C_IMP_RES_GRA)

hist(train$ALTR_CC_C_IMP_RES) #altri prestiti finalizzati in corso - importo residuo
summary(train$ALTR_CC_C_IMP_RES)

hist(train$CC_C_IMP_RES) #totale prestiti finalizzati in corso - importo residuo
summary(train$CC_C_IMP_RES)


hist(train$FIND_CC_C_IMP_MEN_TAS) #prestiti finalizzati a tasso in corso - importo rata
summary(train$FIND_CC_C_IMP_MEN_TAS)

hist(train$FIND_CC_C_IMP_MEN_GRA) #prestiti finalizzati gratuiti in corso - importo rata
summary(train$FIND_CC_C_IMP_MEN_GRA)

hist(train$ALTR_CC_C_IMP_MEN) #altri prestiti finalizzati in corso - importo rata
summary(train$ALTR_CC_C_IMP_MEN)

hist(train$CC_C_IMP_MEN) #totale prestiti finalizzati in corso - importo residuo
summary(train$CC_C_IMP_MEN)

hist(train$CRT_PRE_C_FLG_PRE) #cliente in possesso di carta
train$CRT_PRE_C_FLG_PRE = as.factor(train$CRT_PRE_C_FLG_PRE)
summary(train$CRT_PRE_C_FLG_PRE)

hist(train$CRT_TODU_REV) #Esposizione carta di credito
summary(train$CRT_TODU_REV)

#------------
#------------

#Creo una variabile che mi dica se ha almeno un prestito in corso
train$prestiti_totali = train$NUM_PPQ_C + train$CC_C_NUM
summary(train$prestiti_totali)
train$prestiti_totali2 = ifelse(train$prestiti_totali==0,0,1)
train$prestiti_totali2= as.factor(train$prestiti_totali2)
summary(train$prestiti_totali2)

#Creo una variabile che mi dica qual è il totale del saldo 
train$saldo_totale = train$IMP_PPQ_C + train$CC_C_IMP_RES
summary(train$saldo_totale)

#Creo una variabile che mi dica qual è l'importo rata totale
train$rata_totale = train$PPQ_C_IMP_MEN + train$CC_C_IMP_MEN
summary(train$rata_totale)

#----------------
#----------------

saldo_nozero = c()
saldo_nozero = train$saldo_totale[which(train$saldo_totale != 0)]
saldo_nozero
length(saldo_nozero)
summary(saldo_nozero)
hist(saldo_nozero)
hist(log(saldo_nozero))

train$saldo_log = ifelse(train$saldo_totale==0,0,log(train$saldo_totale))
hist(train$saldo_log)
