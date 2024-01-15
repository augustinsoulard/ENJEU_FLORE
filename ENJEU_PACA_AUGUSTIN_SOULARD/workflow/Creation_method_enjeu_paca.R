####################################
#MISE A JOUR TAXREF
####################################

# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

# Charger les bibliotheques necessaires
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("xlsx")){install.packages("xlsx")} ; library("xlsx")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")


# Import de TAXREFv17
TAXREFv17_FLORE_FR_SYN.csv <- read.csv("../../../BDD_FLORE_CONSTRUCT/TAXONOMIE/TAXREF/TAXREFv17_FLORE_FR_SYN.csv")
TAXREFv17_FLORE_FR_SYN.csv$CD_NOM = as.character(TAXREFv17_FLORE_FR_SYN.csv$CD_NOM)

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
TAB_GEN_JOIN_BRUT = left_join(Method_enjeu_PACAv1_5,TAXREFv17_FLORE_FR_SYN.csv,by=c("CD_NOM"="CD_NOM"))

# VERIFICATION !!!!!!MANUELLE!!!!!!!! des taxons dont la jointure a echouee
NO_MATCH = TAB_GEN_JOIN_BRUT[is.na(TAB_GEN_JOIN_BRUT$REGNE),]


# Import de TAXREFv17 des TAXONS VALIDE
TAXREFv17_FLORE_FR.csv <- read.csv("../../../BDD_FLORE_CONSTRUCT/TAXONOMIE/TAXREF/TAXREFv17_FLORE_FR.csv")
TAXREFv17_FLORE_FR.csv$CD_NOM = as.character(TAXREFv17_FLORE_FR.csv$CD_NOM)


#Jointure des taxons nouveaux dans TAXREFv16
TAB_GEN_FULL_JOIN_BRUT = full_join(TAB_GEN_JOIN_BRUT,TAXREFv17_FLORE_FR.csv,by=c("CD_NOM"="CD_NOM"))



# Fusion des colonnes de la jointure et selection des colonnes à garder dans la méthode
TAB_GEN_FULL_JOIN = TAB_GEN_FULL_JOIN_BRUT %>%
  mutate(NOM_VALIDE = coalesce(NOM_VALIDE.x,NOM_VALIDE.y,`Nom scientifique`),
         NOM_VERN = coalesce(NOM_VERN.x,NOM_VERN.y,`Nom vernaculaire`),
         FAMILLE = coalesce(FAMILLE.x,FAMILLE.y),
         SOUS_FAMILLE = coalesce(SOUS_FAMILLE.x,SOUS_FAMILLE.y),
         INDIGENAT = coalesce(FR.x,FR.y,INDIGENAT),
         TRIGRAMME = coalesce(TRIGRAMME.x,TRIGRAMME.y)) %>%
  select(CD_NOM,TRIGRAMME,NOM_VALIDE,NOM_VERN,FAMILLE,SOUS_FAMILLE,INDIGENAT,EVEE,DH,PN,LRN,PR,`PR-Corr`,LRR,ZNIEFF,PD04,PD05,PD06,PD83,PD84,ENJEU_CBN,`Indicatrice ZH`,
         Barcelone,Berne,Mondiale,Europe)

# Reecriture des classes d'indigenat
CODEINDIGENAT = c("P","E","S","C","I","J","M","B","D","Q","A","W","X","Z","Y")
LBINDIGENAT = c("Présent (indigène ou indéterminé)","Endémique","Subendémique","Cryptogène","Introduit",
                "Introduit envahissant","Introduit non établi (dont cultivé / domestique)","Occasionnel","Douteux",
                "Mentionné par erreur","Absent","Disparu","Eteint","Endémique éteint","Introduit éteint")
Corresp_INDIGENAT <- data.frame(CODEINDIGENAT, LBINDIGENAT)

TAB_GEN_FULL_JOIN = left_join(TAB_GEN_FULL_JOIN,Corresp_INDIGENAT,by=c("INDIGENAT"="CODEINDIGENAT")) #Jointure

TAB_GEN_FULL_JOIN = TAB_GEN_FULL_JOIN %>%
  mutate(INDIGENAT = coalesce(LBINDIGENAT,INDIGENAT)) %>%
  select(-LBINDIGENAT)

#Enregistrement du tableau a integrer à la methode enjeu PACA
write.csv(TAB_GEN_FULL_JOIN,file = "TAB_GEN_FULL_JOIN.csv",row.names = F,fileEncoding = "UTF-8",na="-")
