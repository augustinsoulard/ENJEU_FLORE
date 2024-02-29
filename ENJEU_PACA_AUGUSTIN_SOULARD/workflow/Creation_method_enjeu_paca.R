####################################
####################################
#Creation de Tableau_general
####################################
####################################
# Voir mise à jour TAXREF plus bas ||
#                                  V

# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

# Charger les bibliotheques necessaires
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("xlsx")){install.packages("xlsx")} ; library("xlsx")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")


# URL du fichier ZIP contenant le Shapefile
url_zip <- "https://inpn.mnhn.fr/docs-web/docs/download/232324"

##########################Telechargement des fichiers non disponibles
# Télécharger le fichier ZIP
download.file(url_zip, destfile = "BaseConnaissance.zip", mode = "wb")
# Décompresser le fichier ZIP
unzip("BaseConnaissance.zip")

#Import de la base de connaissance
BDC_STATUTS_17 <- read.csv("BDC-Statuts-v17/BDC_STATUTS_17.csv")


# Import de TAXREFv17
TAXREFv17_FLORE_FR <- read.csv("../../../BDD_FLORE_CONSTRUCT/TAXONOMIE/TAXREF/TAXREFv17_FLORE_FR.csv")
TAXREFv17_FLORE_FR$CD_NOM = as.character(TAXREFv17_FLORE_FR$CD_NOM)

# Filtre de la base de connaissance pour la flore
BDC_STATUTS_17_FLORE = BDC_STATUTS_17 %>% 
  filter(BDC_STATUTS_17$CD_NOM %in% TAXREFv17_FLORE_FR$CD_REF) %>%
  filter(LB_ADM_TR %in% c("Monde","Europe") | CD_ISO3166_1 %in% c("FXX","FRA") | 
           CD_ISO3166_2 %in% c("FR-13","FR-04","FR-05","FR-06","FR-83","FR-84","FR-U"))
#Creer le futur nom des colonnes
BDC_STATUTS_17_FLORE$LB_STATUT_COL = make.names(paste0(BDC_STATUTS_17_FLORE$LB_TYPE_STATUT,"_",
                                                       BDC_STATUTS_17_FLORE$LB_ADM_TR))


# Traitement du tableau pour l'horizontaliser
TAXREFv17_FLORE_JOIN = TAXREFv17_FLORE_FR
for(i in 1:length(levels(as.factor(BDC_STATUTS_17_FLORE$LB_STATUT_COL)))){
  cat(i,"/",length(levels(as.factor(BDC_STATUTS_17_FLORE$LB_STATUT_COL))),"\n")
  LB_STATUT = levels(as.factor(BDC_STATUTS_17_FLORE$LB_STATUT_COL))[i]
  
  BDC_WORFLOW = BDC_STATUTS_17_FLORE %>%
    filter(BDC_STATUTS_17_FLORE$LB_STATUT_COL %in% LB_STATUT)
  
  BDC_TO_JOIN = BDC_WORFLOW %>%
    distinct(CD_NOM, .keep_all = TRUE) %>%
    select(CD_NOM, CODE_STATUT) %>%
    rename(!!LB_STATUT := CODE_STATUT)
  
  #Jointure avec le TAXREF
  TAXREFv17_FLORE_JOIN = left_join(TAXREFv17_FLORE_JOIN,BDC_TO_JOIN,by=c("CD_REF"="CD_NOM"))
  
}


#### Ajout des plantes indicatrices ZH et des EEEs
#Chargement flore ZH
FloreZH = read.csv("../../../01_MINI_OUTILS/ZONE_HUMIDE/RfloreZH/FloreZH.csv")
# FloreZH = read.csv("../../../ZONE_HUMIDE/RfloreZH/FloreZH.csv")
TAXREFv17_FLORE_JOIN = left_join(TAXREFv17_FLORE_JOIN,FloreZH,by=c("CD_REF"="CODE.FVF"))

# Chargement flore EEE
TAB_EVEE_PACA <- read_excel("../../../BDD_FLORE_CONSTRUCT/EEE/PACA/TAB_EVEE_PACA.xlsx")
TAB_EVEE_PACA = TAB_EVEE_PACA %>% select(CD_NOM,categorie_paca)

# Jointure flore EVEE
TAXREFv17_FLORE_JOIN = left_join(TAXREFv17_FLORE_JOIN,TAB_EVEE_PACA,by=c("CD_REF"="CD_NOM"))

# Chargement enjeu CBN
ENJEU_CBN <- read.csv2("../../HIERARCHISATION_CBN_PACA/WORKFLOW/HIERARCHISATION_ENJEU_RESEDA_TAXREFv17.csv")
ENJEU_CBN$CD_NOM = as.double(ENJEU_CBN$CD_NOM)
ENJEU_CBN = ENJEU_CBN %>% select(CD_NOM,ENJEU_CONSERVATION)
# Jointure enjeu CBN
TAXREFv17_FLORE_JOIN = left_join(TAXREFv17_FLORE_JOIN,ENJEU_CBN,by=c("CD_REF"="CD_NOM"))

#Selection des colonnes pour le tableau final
TAXREFv17_FLORE_JOIN = TAXREFv17_FLORE_JOIN %>% select(CD_NOM = CD_REF,
                                                       TRIGRAMME,
                                                       NOM_VALIDE,
                                                       NOM_VERN,
                                                       FAMILLE,
                                                       SOUS_FAMILLE,
                                                       INDIGENAT = FR, 
                                                       EVEE = categorie_paca,
                                                       DH = Directive.Habitat_France.métropolitaine,
                                                       PN = Protection.nationale_France.métropolitaine,
                                                       LRN = Liste.rouge.nationale_France.métropolitaine,
                                                       PR = Protection.régionale_Provence.Alpes.Côte.d.Azur,
                                                       PR_Corr = Protection.régionale_Provence.Alpes.Côte.d.Azur,
                                                       LRR = Liste.rouge.régionale_Provence.Alpes.Côte.d.Azur,
                                                       ZNIEFF = ZNIEFF.Déterminantes_Provence.Alpes.Côte.d.Azur,
                                                       PD04 = Protection.départementale_Alpes.de.Haute.Provence,
                                                       PD05 = Protection.départementale_Hautes.Alpes,
                                                       PD06 = Protection.départementale_Alpes.Maritimes,
                                                       PD83 = Protection.départementale_Var,
                                                       PD84 = Protection.départementale_Vaucluse,
                                                       ENJEU_CBN = ENJEU_CONSERVATION,
                                                       indicatrice_ZH = INDIC_ZH,
                                                       Barcelonne =Convention.de.Barcelone_France.métropolitaine,
                                                       Berne = Convention.de.Berne_France.métropolitaine,
                                                       Mondiale = Liste.rouge.mondiale_Monde,
                                                       Europe = Liste.rouge.européenne_Europe
)

## Mise en forme colonne ZNIEFF
TAXREFv17_FLORE_JOIN[!is.na(TAXREFv17_FLORE_JOIN$ZNIEFF),]$ZNIEFF = "D"

#Création de la colonne Protection AuRA
ProtectionPACA = function(x){
  valeur = NULL
  if(!is.na(x["PN"]) && x["PN"] != "-"){
    valeur = paste0(valeur," ","PN")
  }
  if(!is.na(x["PR"]) && x["PR"] != "-"){
    valeur = paste0(valeur," ","PR")
  }
  if(!is.na(x["PD04"]) && x["PD04"] != "-"){
    valeur = paste0(valeur," ","PD04")
  }
  if(!is.na(x["PD05"]) && x["PD05"] != "-"){
    valeur = paste0(valeur," ","PD05")
  }
  if(!is.na(x["PD06"]) && x["PD06"] != "-"){
    valeur = paste0(valeur," ","PD06")
  }
  if(!is.na(x["PD83"]) && x["PD83"] != "-"){
    valeur = paste0(valeur," ","PD83")
  }
  if(!is.na(x["PD84"]) && x["PD84"] != "-"){
    valeur = paste0(valeur," ","PD84")
  }
  if(is.null(valeur)){
    valeur = "-"
  }
  return(valeur)
}

TAXREFv17_FLORE_JOIN$PROTECTION_AURA = apply(TAXREFv17_FLORE_JOIN,1,ProtectionPACA)

#Enregistrement du tableau a integrer à la methode enjeu PACA
write.csv(TAXREFv17_FLORE_JOIN,file = "TAXREFv17_FLORE_JOIN.csv",row.names = F,fileEncoding = "UTF-8",na="-")

####################################
####################################
#MISE A JOUR TAXREF
####################################
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
