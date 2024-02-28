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

#Chargement flore EEE
EEE_AuRA <- read.csv("../../../BDD_FLORE_CONSTRUCT/EEE/AuRA/EEE_AuRA.csv",h=T)

for(i in 1:nrow(EEE_AuRA)){
  if(EEE_AuRA$LAVERGNE_AUVERGNE[i] == ""){
    EEE_AuRA$PointAUVERGNE[i] = 0
  } else if(EEE_AuRA$LAVERGNE_AUVERGNE[i] == "2+" | EEE_AuRA$LAVERGNE_AUVERGNE[i] == "2" | EEE_AuRA$LAVERGNE_AUVERGNE[i] == "2 et 2+"){
    EEE_AuRA$PointAUVERGNE[i] = 2
  } else if(EEE_AuRA$LAVERGNE_AUVERGNE[i] == "3" ){
    EEE_AuRA$PointAUVERGNE[i] = 3
  } else if(EEE_AuRA$LAVERGNE_AUVERGNE[i] == "4" ){
    EEE_AuRA$PointAUVERGNE[i] = 4
  }else if(EEE_AuRA$LAVERGNE_AUVERGNE[i] == "5" ){
    EEE_AuRA$PointAUVERGNE[i] = 5
  }else if(EEE_AuRA$LAVERGNE_AUVERGNE[i] == "A signaler" ){
    EEE_AuRA$PointAUVERGNE[i] = 5
  }
  if(EEE_AuRA$LAVERGNE_RHONE_ALPES[i] == ""){
    EEE_AuRA$PointRHONE_ALPES[i] = 0
  } else if(EEE_AuRA$LAVERGNE_RHONE_ALPES[i] == "1"){
    EEE_AuRA$PointRHONE_ALPES[i] = 1
  } else if(EEE_AuRA$LAVERGNE_RHONE_ALPES[i] == "2+" | EEE_AuRA$LAVERGNE_RHONE_ALPES[i] == "2" | EEE_AuRA$LAVERGNE_RHONE_ALPES[i] == "2 et 2+"){
    EEE_AuRA$PointRHONE_ALPES[i] = 2
  } else if(EEE_AuRA$LAVERGNE_RHONE_ALPES[i] == "3" ){
    EEE_AuRA$PointRHONE_ALPES[i] = 3
  } else if(EEE_AuRA$LAVERGNE_RHONE_ALPES[i] == "4" ){
    EEE_AuRA$PointRHONE_ALPES[i] = 4
  }else if(EEE_AuRA$LAVERGNE_RHONE_ALPES[i] == "5" ){
    EEE_AuRA$PointRHONE_ALPES[i] = 5
  }else if(EEE_AuRA$LAVERGNE_RHONE_ALPES[i] == "27" ){
    EEE_AuRA$PointRHONE_ALPES[i] = 4
  }
  
  if(EEE_AuRA$PointAUVERGNE[i] >= EEE_AuRA$PointRHONE_ALPES[i]){
    EEE_AuRA$LAVERGNE_AURA[i] = EEE_AuRA$LAVERGNE_AUVERGNE[i]
  } else (EEE_AuRA$LAVERGNE_AURA[i] = EEE_AuRA$LAVERGNE_RHONE_ALPES[i])
  
}

EEE_AuRA_JOIN = EEE_AuRA %>% 
  filter(CD_NOM != "NOMATCH" & CD_NOM != "") %>%
  select(CD_NOM,LAVERGNE_AURA)
EEE_AuRA_JOIN$CD_NOM = as.integer(EEE_AuRA_JOIN$CD_NOM)

TAXREFv17_FLORE_JOIN = left_join(TAXREFv17_FLORE_JOIN,EEE_AuRA_JOIN,by=c("CD_REF"="CD_NOM"))

#Selection des colonnes pour le tableau final
TAXREFv17_FLORE_JOIN = TAXREFv17_FLORE_JOIN %>% select(CD_NOM = CD_REF,
                                                       TRIGRAMME,
                                                       NOM_VALIDE,
                                                       NOM_VERN,INDIGENAT = FR, 
                                                       EVEE_LAVERGNE = LAVERGNE_AURA,
                                                       DH = Directive.Habitat_France.métropolitaine,
                                                       PN = Protection.nationale_France.métropolitaine,
                                                       LRN = Liste.rouge.nationale_France.métropolitaine,
                                                       PR = Protection.régionale_Rhône.Alpes,
                                                       PR_Corr = Protection.régionale_Rhône.Alpes,
                                                       LRR = Liste.rouge.régionale_Rhône.Alpes,
                                                       ZNIEFF = ZNIEFF.Déterminantes_Ain,# Les ZNIEFF départementale sont toutes les mêmes
                                                       #ZNIEFF03 = ZNIEFF.Déterminantes_Allier,
                                                       #ZNIEFF07 = ZNIEFF.Déterminantes_Ardèche,
                                                       #ZNIEFF15 = ZNIEFF.Déterminantes_Cantal,
                                                       #ZNIEFF26 = ZNIEFF.Déterminantes_Drôme,
                                                       #ZNIEFF38 = ZNIEFF.Déterminantes_Isère,
                                                       #ZNIEFF42 = ZNIEFF.Déterminantes_Loire,
                                                       #ZNIEFF43 = ZNIEFF.Déterminantes_Haute.Loire,
                                                       #ZNIEFF63 = ZNIEFF.Déterminantes_Puy.de.Dôme,
                                                       #ZNIEFF69 = ZNIEFF.Déterminantes_Rhône,
                                                       #ZNIEFF73 = ZNIEFF.Déterminantes_Savoie,
                                                       #ZNIEFF74 = ZNIEFF.Déterminantes_Haute.Savoie,
                                                       PD01 = Protection.départementale_Ain,
                                                       PD26 = Protection.départementale_Drôme,
                                                       PD38 = Protection.départementale_Isère,
                                                       PD42 = Protection.départementale_Loire,
                                                       PD74 = Protection.départementale_Haute.Savoie,
                                                       indicatrice_ZH = INDIC_ZH,
                                                       Barcelonne =Convention.de.Barcelone_France.métropolitaine,
                                                       Berne = Convention.de.Berne_France.métropolitaine,
                                                       Mondiale = Liste.rouge.mondiale_Monde,
                                                       Europe = Liste.rouge.européenne_Europe,
)

## Création de la colonne ZNIEFF AuRA
# ZNIEFFGlobale = function(x){
#   valeur = NULL
#   if(!is.na(x["ZNIEFF01"]) && x["ZNIEFF01"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF01")
#   }
#   if(!is.na(x["ZNIEFF03"]) && x["ZNIEFF03"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF03")
#   }
#   if(!is.na(x["ZNIEFF07"]) && x["ZNIEFF07"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF07")
#   }
#   if(!is.na(x["ZNIEFF15"]) && x["ZNIEFF15"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF15")
#   }
#   if(!is.na(x["ZNIEFF26"]) && x["ZNIEFF26"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF26")
#   }
#   if(!is.na(x["ZNIEFF38"]) && x["ZNIEFF38"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF38")
#   }
#   if(!is.na(x["ZNIEFF42"]) && x["ZNIEFF42"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF42")
#   }
#   if(!is.na(x["ZNIEFF43"]) && x["ZNIEFF43"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF43")
#   }
#   if(!is.na(x["ZNIEFF63"]) && x["ZNIEFF63"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF63")
#   }
#   if(!is.na(x["ZNIEFF69"]) && x["ZNIEFF69"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF69")
#   }
#   if(!is.na(x["ZNIEFF73"]) && x["ZNIEFF73"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF73")
#   }
#   if(!is.na(x["ZNIEFF74"]) && x["ZNIEFF74"] != "-"){
#     valeur = paste0(valeur," ","ZNIEFF74")
#   }
#   if(is.null(valeur)){
#     valeur = "-"
#   }
#   return(valeur)
# }

# TAXREFv17_FLORE_JOIN$ZNIEFF = apply(TAXREFv17_FLORE_JOIN,1,ZNIEFFGlobale)
# TAXREFv17_FLORE_JOIN[TAXREFv17_FLORE_JOIN$ZNIEFF != "-",]$ZNIEFF = "D" #Car ici c'ets une ZNIEFF régionale
TAXREFv17_FLORE_JOIN[!is.na(TAXREFv17_FLORE_JOIN$ZNIEFF),]$ZNIEFF = "D"

#Création de la colonne Protection AuRA
ProtectionAuRA = function(x){
  valeur = NULL
  if(!is.na(x["PN"]) && x["PN"] != "-"){
    valeur = paste0(valeur," ","PN")
  }
  if(!is.na(x["PR"]) && x["PR"] != "-"){
    valeur = paste0(valeur," ","PR")
  }
  if(!is.na(x["PD01"]) && x["PD01"] != "-"){
    valeur = paste0(valeur," ","PD01")
  }
  if(!is.na(x["PD26"]) && x["PD26"] != "-"){
    valeur = paste0(valeur," ","PD26")
  }
  if(!is.na(x["PD38"]) && x["PD38"] != "-"){
    valeur = paste0(valeur," ","PD38")
  }
  if(!is.na(x["PD42"]) && x["PD42"] != "-"){
    valeur = paste0(valeur," ","PD42")
  }
  if(!is.na(x["PD74"]) && x["PD74"] != "-"){
    valeur = paste0(valeur," ","PD74")
  }
  if(is.null(valeur)){
    valeur = "-"
  }
  return(valeur)
}

TAXREFv17_FLORE_JOIN$PROTECTION_AURA = apply(TAXREFv17_FLORE_JOIN,1,ProtectionAuRA)

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
