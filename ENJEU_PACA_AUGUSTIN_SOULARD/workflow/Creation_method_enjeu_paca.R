####################################
#MISE A JOUR TAXREF 16
####################################

# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

# Charger les bibliotheques necessaires
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("xlsx")){install.packages("xlsx")} ; library("xlsx")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")


# Import de TAXREFv16
TAXREFv16_FLORE_FR_SYN.csv <- read.csv2("../../../BDD_FLORE_CONSTRUCT/TAXONOMIE/TAXREF/TAXREFv16_FLORE_FR_SYN.csv")
TAXREFv16_FLORE_FR_SYN.csv$CD_NOM = as.character(TAXREFv16_FLORE_FR_SYN.csv$CD_NOM)

# Import de la table generale
Method_enjeu_PACAv1_5 <- read_excel("~/GitHub/ENJEU_FLORE/ENJEU_PACA_AUGUSTIN_SOULARD/Method_enjeu_PACAv1.5.xlsx", 
                                    sheet = "Tableau_general", col_types = c("text", 
                                                                             "text", "text", "text", "text", "text", 
                                                                             "text", "text", "text", "text", "text", 
                                                                             "text", "text", "text", "text", "text", 
                                                                             "text", "text", "text", "text", "text", 
                                                                             "text", "text", "text", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "numeric", "numeric", 
                                                                             "numeric", "text", "text"))
#Jointure
TAB_GEN_JOIN_BRUT = left_join(Method_enjeu_PACAv1_5,TAXREFv16_FLORE_FR_SYN.csv,by=c("CD_NOM"="CD_NOM"))

# VERIFICATION !!!!!!MANUELLE!!!!!!!! des taxons dont la jointure a echouee
NO_MATCH = TAB_GEN_JOIN_BRUT[is.na(TAB_GEN_JOIN_BRUT$REGNE),]


# Import de TAXREFv16 des TAXONS VALIDE
TAXREFv16_FLORE_FR.csv <- read.csv("../../../BDD_FLORE_CONSTRUCT/TAXONOMIE/TAXREF/TAXREFv16_FLORE_FR.csv")
TAXREFv16_FLORE_FR.csv$CD_NOM = as.character(TAXREFv16_FLORE_FR.csv$CD_NOM)


#Jointure des taxons nouveaux dans TAXREFv16
TAB_GEN_FULL_JOIN_BRUT = full_join(TAB_GEN_JOIN_BRUT,TAXREFv16_FLORE_FR.csv,by=c("CD_NOM"="CD_NOM"))

#Fonction de remplacement des colonnes à mettre à jour
remplacMethod = function(colTAXREF,colMETHOD){
  TAB_GEN_FULL_JOIN_BRUT[colTAXREF] = ifelse(TAB_GEN_FULL_JOIN_BRUT[paste0(colTAXREF,".x")] == "", TAB_GEN_FULL_JOIN_BRUT[paste0(colTAXREF,".y")], TAB_GEN_FULL_JOIN_BRUT[paste0(colTAXREF,".x")])
  TAB_GEN_FULL_JOIN_BRUT[colMETHOD] = ifelse(TAB_GEN_FULL_JOIN_BRUT[colTAXREF] == "", TAB_GEN_FULL_JOIN_BRUT[colMETHOD], TAB_GEN_FULL_JOIN_BRUT[colTAXREF])
  
}
remplacMethod("NOM_VALIDE","Nom scientifique")

# Selection des donnees du tableau
TAB_GEN_FULL_JOIN = TAB_GEN_FULL_JOIN_BRUT %>% select()

TAB_GEN_FULL_JOIN_BRUT[]
