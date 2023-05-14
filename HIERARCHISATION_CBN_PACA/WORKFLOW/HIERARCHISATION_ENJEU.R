# remotes::install_github("Rekyt/rtaxref")
# WEBSITE: https://rekyt.github.io/rtaxref/

# Chargement de HIERARCHISATION_ENJEU_RESEDA

HIERARCHISATION_ENJEU_RESEDA <- read.csv("~/Programmation/Github/ENJEU_FLORE/HIERARCHISATION_CBN_PACA/HIERARCHISATION_ENJEU_RESEDA.csv")

# Préparations des nouvelles colonnes

HIERARCHISATION_ENJEU_RESEDA$CD_NOM = "0"
HIERARCHISATION_ENJEU_RESEDA$NOM_VALIDE = "NULL"

# Chargement des packages

library(rtaxref)
library(tidyverse)

#Boucle de matching
for (i in 1:nrow(HIERARCHISATION_ENJEU_RESEDA)){
  cat(i," : ")
  t = rt_taxa_search(sciname = HIERARCHISATION_ENJEU_RESEDA$NOM_MATCHING[i],version = "16.0")
    if(ncol(t)>1){
      if(nrow(t)>1){
        if(str_detect(HIERARCHISATION_ENJEU_RESEDA$NOM_MATCHING[i],"subsp.")){
          t = t[t$rankId=="SSES",]
          cat("SOUS-ESP \n")
        } else{
          t = t[t$rankId=="ES",]
          cat("ESP \n")
        } 
      }
      if(nrow(t[t$id==t$referenceId,])>0){t = t[t$id==t$referenceId,]}
      if(nrow(t[!str_detect(t$scientificName," x "),])>0){t = t[!str_detect(t$scientificName," x "),]}
    HIERARCHISATION_ENJEU_RESEDA$CD_NOM[i] = t$referenceId[1]
    HIERARCHISATION_ENJEU_RESEDA$NOM_VALIDE[i] = t$scientificName[1]
    }else{
      HIERARCHISATION_ENJEU_RESEDA$CD_NOM[i] = "NOMATCH"
      HIERARCHISATION_ENJEU_RESEDA$NOM_VALIDE[i] = "NOMATCH"
    }
}

HIERARCHISATION_ENJEU_RESEDA %>% select(CD_NOM,NOM_VALIDE,NOM_CITE,ENJEU_CONSERVATION)
write.csv2(HIERARCHISATION_ENJEU_RESEDA,"HIERARCHISATION_ENJEU_RESEDA_TAXREFv16.csv",row.names = F,fileEncoding = "UTF-8")

