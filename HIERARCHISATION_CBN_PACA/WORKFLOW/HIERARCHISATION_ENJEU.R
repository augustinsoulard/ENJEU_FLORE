# remotes::install_github("Rekyt/rtaxref")
# WEBSITE: https://rekyt.github.io/rtaxref/

# Chargement de HIERARCHISATION_ENJEU_RESEDA

# HIERARCHISATION_ENJEU_RESEDA <- read.csv("~/Programmation/Github/ENJEU_FLORE/HIERARCHISATION_CBN_PACA/HIERARCHISATION_ENJEU_RESEDA.csv")
HIERARCHISATION_ENJEU_RESEDA <- read.csv("../../../ENJEU_FLORE/HIERARCHISATION_CBN_PACA/WORKFLOW/HIERARCHISATION_ENJEU_RESEDA.csv")

# Pr?parations des nouvelles colonnes

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


####### MISE A JOUR TAXREF
# Import de TAXREFv18
TAXREF_FLORE_FR_SYN<- read.csv("../../../BDD_FLORE_CONSTRUCT/TAXONOMIE/TAXREF/TAXREFv17_FLORE_FR_SYN.csv")
# Import de Hierarchisation première version de transformation = v16
HIERARCHISATION_ENJEU_RESEDA <- read.csv2("../../../ENJEU_FLORE/HIERARCHISATION_CBN_PACA/WORKFLOW/HIERARCHISATION_ENJEU_RESEDA_TAXREFv16.csv")
HIERARCHISATION_ENJEU_RESEDA$CD_NOM = as.integer(HIERARCHISATION_ENJEU_RESEDA$CD_NOM)

#Jointure
TAB_GEN_JOIN_BRUT = left_join(HIERARCHISATION_ENJEU_RESEDA,TAXREF_FLORE_FR_SYN,by=c("CD_NOM"="CD_NOM"))

# VERIFICATION !!!!!!MANUELLE!!!!!!!! des taxons dont la jointure a echouee
NO_MATCH = TAB_GEN_JOIN_BRUT[is.na(TAB_GEN_JOIN_BRUT$REGNE),]

#Tables finales
HIERARCHISATION_ENJEU_RESEDA = TAB_GEN_JOIN_BRUT %>% dplyr::select(CD_NOM = CD_REF,NOM_VALIDE=NOM_VALIDE.x,NOM_CITE,ENJEU_CONSERVATION)

#Enregistrement des tables en format excel et csv
write.csv2(NO_MATCH,"HIERARCHISATION_ENJEU_RESEDA_TAXREFv18_NO_MATCH.csv",row.names = F,fileEncoding = "UTF-8")
write.csv2(HIERARCHISATION_ENJEU_RESEDA,"HIERARCHISATION_ENJEU_RESEDA_TAXREFv18.csv",row.names = F,fileEncoding = "UTF-8")

