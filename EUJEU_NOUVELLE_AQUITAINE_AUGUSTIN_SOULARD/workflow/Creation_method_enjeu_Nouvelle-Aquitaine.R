# CREATION DE TABLEAU_GENERAL ##################################

## Definition du repertoire de fichiers  ####
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

## Fonctions ####

updatetaxa = function(liste_cd_nom){
  TAXREF_FLORE_FR_SYN <- read.csv("../../../BDD_FLORE_CONSTRUCT/TAXONOMIE/TAXREF/TAXREFv18_FLORE_FR_SYN.csv") ### MODIFIER LA VERSION TAXREF
  TAXREF_FLORE_FR_SYN$CD_NOM = as.character(TAXREF_FLORE_FR_SYN$CD_NOM)
  liste_cd_nom = as.character(liste_cd_nom)
  liste_cd_nom = data.frame("CD_NOM" = liste_cd_nom)
  liste_cd_nom_join = left_join(liste_cd_nom,TAXREF_FLORE_FR_SYN,by="CD_NOM") %>% select(CD_REF)
  CD_NOM_actuel = liste_cd_nom_join$CD_REF
  return(CD_NOM_actuel)
}

## Charger les bibliotheques necessaires ####
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")
if(!require("dplyr")){install.packages("dplyr")} ; library("dplyr")


# URL du fichier ZIP contenant le Shapefile
url_zip <- "https://inpn.mnhn.fr/docs-web/docs/download/232324"

## Telechargement des fichiers non disponibles #### 
# Télécharger le fichier ZIP
download.file(url_zip, destfile = "BaseConnaissance.zip", mode = "wb")
# Décompresser le fichier ZIP
unzip("BaseConnaissance.zip")

#Import de la base de connaissance
BDC_STATUTS <- read.csv("bdc_statuts_18.csv") ### MODIFIER LA VERSION


# Import de TAXREF
TAXREF_FLORE_FR <- read.csv("../../../BDD_FLORE_CONSTRUCT/TAXONOMIE/TAXREF/TAXREFv18_FLORE_FR.csv") ### MODIFIER LA VERSION TAXREF
TAXREF_FLORE_FR$CD_NOM = as.character(TAXREF_FLORE_FR$CD_NOM)### MODIFIER LA VERSION TAXREF

# Filtre de la base de connaissance pour la flore
BDC_STATUTS_FLORE = BDC_STATUTS %>% 
  filter(BDC_STATUTS$CD_NOM %in% TAXREF_FLORE_FR$CD_REF | BDC_STATUTS$CD_REF %in% TAXREF_FLORE_FR$CD_REF) %>%
  filter(LB_ADM_TR %in% c("Monde","Europe") |
           CD_ISO3166_1 %in% c("FXX","FRA") | 
           CD_ISO3166_2 %in% c("FR-16","FR-17","FR-19","FR-23","FR-24","FR-33","FR-40","FR-47",
                               "FR-64","FR-79","FR-86","FR-87","FR-T","FR-B","FR-L"))

# Creer le futur nom des colonnes
BDC_STATUTS_FLORE$LB_STATUT_COL = make.names(paste0(BDC_STATUTS_FLORE$LB_TYPE_STATUT,"_",
                                                       BDC_STATUTS_FLORE$LB_ADM_TR))


# Traitement du tableau pour l'horizontaliser

# Créer une table des statuts uniques pour chaque CD_REF
BDC_TO_JOIN <- BDC_STATUTS_FLORE %>%
  distinct(CD_REF, LB_STATUT_COL, CODE_STATUT) %>%
  rename(LB_STATUT = CODE_STATUT)

# Transformer les statuts en colonnes avec pivot_wider
BDC_TO_JOIN_WIDE <- BDC_TO_JOIN %>%
  pivot_wider(names_from = LB_STATUT_COL, values_from = LB_STATUT, values_fn = list(LB_STATUT = ~ paste(unique(.), collapse = ", ")))

# Jointure finale avec TAXREF_FLORE_FR
TAXREF_FLORE_JOIN <- TAXREF_FLORE_FR %>%
  left_join(BDC_TO_JOIN_WIDE, by = "CD_REF")


#### Ajout des plantes indicatrices ZH et des EEEs
#Chargement flore ZH
FloreZH = read.csv("../../../ZONE_HUMIDE/RfloreZH/FloreZH.csv")
FloreZH$CODE.FVF = updatetaxa(FloreZH$CODE.FVF)
# FloreZH = read.csv("../../../ZONE_HUMIDE/RfloreZH/FloreZH.csv")
TAXREF_FLORE_JOIN = left_join(TAXREF_FLORE_JOIN,FloreZH,by=c("CD_REF"="CODE.FVF"))

# Chargement flore EEE
TAB_EVEE <- read_excel("../../../BDD_FLORE_CONSTRUCT/EEE/Nouvelle-Aquitaine/CBNSA_2022-Liste_hierarchisee_PEE_NA_v1.0.xlsx", skip = 1)
TAB_EVEE$CD_NOM = updatetaxa(TAB_EVEE$`CD_NOM (TaxRef13)`)
TAB_EVEE = TAB_EVEE %>% select(CD_NOM,
                               LB_EEE = `Catégories Liste hiérarchisée des PEE de Nouvelle-Aquitaine` )

TAB_EVEE = TAB_EVEE %>%
  mutate(EEE = case_when(
    is.na(LB_EEE) ~ NA_character_,
    LB_EEE == "Insuffisament documentée" ~ "DD",
    LB_EEE == "Non envahissante actuellement" ~ "Non envahissante",
    LB_EEE == "PEE à impact modéré" ~ "Modéré",
    LB_EEE == "PEE à impact majeur" ~ "Majeur",
    LB_EEE == "Prévention" ~ "Prévention",
    TRUE ~ NA_character_  # pour toutes les autres valeurs non spécifiées
  ))
# Jointure flore EVEE
TAXREF_FLORE_JOIN = left_join(TAXREF_FLORE_JOIN,TAB_EVEE,by=c("CD_REF"="CD_NOM"))

#Selection des colonnes pour le tableau final
TAXREF_FLORE_JOIN = TAXREF_FLORE_JOIN %>% dplyr::select(CD_NOM = CD_NOM,
                                                       TRIGRAMME=TRIGRAMME,
                                                       NOM_VALIDE=NOM_VALIDE,
                                                       NOM_VERN=NOM_VERN,
                                                       FAMILLE=FAMILLE,
                                                       SOUS_FAMILLE=SOUS_FAMILLE,
                                                       INDIGENAT = FR, 
                                                       EVEE = EEE,
                                                       DH = Directive.Habitat_France.métropolitaine,
                                                       PN = Protection.nationale_France.métropolitaine,
                                                       LRN = Liste.rouge.nationale_France.métropolitaine,
                                                       PR_Aquitaine = Protection.régionale_Aquitaine,
                                                      PR_Limousin = Protection.régionale_Limousin,
                                                      PR_PC = Protection.régionale_Poitou.Charentes,
                                                        LRR_Aquitaine = Liste.rouge.régionale_Aquitaine,
                                                      LRR_Limousin = Liste.rouge.régionale_Limousin,
                                                      LRR_PC = Liste.rouge.régionale_Poitou.Charentes,
                                                      ZNIEFF_Aquitaine = ZNIEFF.Déterminantes_Aquitaine,
                                                      ZNIEFF_Limousin = ZNIEFF.Déterminantes_Limousin,
                                                      ZNIEFF_PC = ZNIEFF.Déterminantes_Poitou.Charentes,
                                                      PD19 = Protection.départementale_Corrèze,
                                                      PD23 = Protection.départementale_Creuse,
                                                      PD24 = Protection.départementale_Dordogne,
                                                      PD33 = Protection.départementale_Gironde,
                                                      PD40 = Protection.départementale_Landes,
                                                      PD47 = Protection.départementale_Lot.et.Garonne,
                                                       PD64 = Protection.départementale_Pyrénées.Atlantiques,
                                                       PD86 = Protection.départementale_Vienne,
                                                       PD87 = Protection.départementale_Haute.Vienne,
                                                       indicatrice_ZH = INDIC_ZH,
                                                       Barcelonne =Convention.de.Barcelone_France.métropolitaine,
                                                       Berne = Convention.de.Berne_France.métropolitaine,
                                                       Mondiale = Liste.rouge.mondiale_Monde,
                                                       Europe = Liste.rouge.européenne_Europe
)

## Mise en forme colonne ZNIEFF ####
TAXREF_FLORE_JOIN <- TAXREF_FLORE_JOIN %>%
  mutate(across(starts_with("ZNIEFF"), ~ ifelse(!is.na(.), "D", .)))

#Création de la colonne Protection AuRA
Protection = function(x){
  valeur = NULL
  if(!is.na(x["PN"]) && x["PN"] != "-"){
    valeur = paste0(valeur," ","PN")
  }
  if(!is.na(x["PR_Aquitaine"]) && x["PR_Aquitaine"] != "-"){
    valeur = paste0(valeur," ","PR_Aquitaine")
  }
  if(!is.na(x["PR_Limousin"]) && x["PR_Limousin"] != "-"){
    valeur = paste0(valeur," ","PR_Limousin")
  }
  if(!is.na(x["PR_PC"]) && x["PR_PC"] != "-"){
    valeur = paste0(valeur," ","PR_PC")
  }
  if(!is.na(x["PD19"]) && x["PD19"] != "-"){
    valeur = paste0(valeur," ","PD19")
  }
  if(!is.na(x["PD23"]) && x["PD23"] != "-"){
    valeur = paste0(valeur," ","PD23")
  }
  if(!is.na(x["PD24"]) && x["PD24"] != "-"){
    valeur = paste0(valeur," ","PD24")
  }
  if(!is.na(x["PD33"]) && x["PD33"] != "-"){
    valeur = paste0(valeur," ","PD33")
  }
  if(!is.na(x["PD40"]) && x["PD40"] != "-"){
    valeur = paste0(valeur," ","PD40")
  }
  if(!is.na(x["PD47"]) && x["PD47"] != "-"){
    valeur = paste0(valeur," ","PD47")
  }
  if(!is.na(x["PD64"]) && x["PD64"] != "-"){
    valeur = paste0(valeur," ","PD64")
  }
  if(!is.na(x["PD86"]) && x["PD86"] != "-"){
    valeur = paste0(valeur," ","PD86")
  }
  if(!is.na(x["PD87"]) && x["PD87"] != "-"){
    valeur = paste0(valeur," ","PD87")
  }
  if(is.null(valeur)){
    valeur = "-"
  }
  return(valeur)
}

TAXREF_FLORE_JOIN$PROTECTION_NOUVELLE_AQUITAINE = apply(TAXREF_FLORE_JOIN,1,Protection)

## Ajout de baseflor ####
baseflor_bryoTAXREF <- read.csv2("../../../BDD_FLORE_CONSTRUCT/TAXONOMIE/TAXREF-MATCH-BASEFLOR/baseflor_bryoTAXREFv17.csv")

baseflor_bryoTAXREF = baseflor_bryoTAXREF %>% select(CD_NOM=CD_NOM,
                                                           floraison=floraison,ecologie = CARACTERISATION_ECOLOGIQUE_.HABITAT_OPTIMAL.,
                                                           syntaxon = INDICATION_PHYTOSOCIOLOGIQUE_CARACTERISTIQUE)
baseflor_bryoTAXREF$CD_NOM = updatetaxa(baseflor_bryoTAXREF$CD_NOM)
### Vecteur de correspondance des chiffres aux mois ####
correspondance_mois <- c("Janvier", "Février", "Mars", "Avril", "Mai",
                         "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", 
                         "Décembre")

# Fonction pour remplacer un chiffre par un mois
remplacer_chiffre_par_mois <- function(chiffre) {
  chiffres <- as.numeric(unlist(strsplit(chiffre, "-")))
  mois <- paste(correspondance_mois[chiffres], collapse = "-")
  return(mois)
}

# Appliquer la fonction sur la colonne du dataframe
baseflor_bryoTAXREF$floraison <- sapply(baseflor_bryoTAXREF$floraison, remplacer_chiffre_par_mois)

# jointure avec le reste du tableau
baseflor_bryoTAXREF$CD_NOM = as.character(baseflor_bryoTAXREF$CD_NOM)
joinbaseflor = left_join(TAXREF_FLORE_JOIN,baseflor_bryoTAXREF,by="CD_NOM")

## Ajout de BDD_FICHE_FLORE ####
#Si pas de BDD_FLore :
joinbaseflor <- joinbaseflor %>% mutate(PATH_IMG = NA_character_,
         TEXTE_LEGEND_IMG = NA_character_)
join_BDD_IMG = joinbaseflor

# BDD_FICHE_FLORE <- read_excel("../../../BDD_FLORE_CONSTRUCT/BDD_FICHE_FLORE/BDD_FICHE_FLORE.xlsx", 
#                               sheet = "IMG")
# BDD_FICHE_FLORE = BDD_FICHE_FLORE %>% select(CD_NOM,PATH_IMG,TEXTE_LEGEND_IMG)
# join_BDD_IMG = left_join(joinbaseflor,BDD_FICHE_FLORE,by="CD_NOM")


#Retrait des duplicats
join_BDD_IMG = join_BDD_IMG[!duplicated(join_BDD_IMG$CD_NOM),]
TAB_TO_EXPORT=join_BDD_IMG




#Enregistrement du tableau a integrer à la methode enjeu PACA
TABLEAU_GENERAL = TAB_TO_EXPORT %>% dplyr::select(
  CD_NOM = CD_NOM,
  TRIGRAMME = TRIGRAMME,
  NOM_VALIDE = NOM_VALIDE,
  NOM_VERN = NOM_VERN,
  FAMILLE = FAMILLE,
  SOUS_FAMILLE = SOUS_FAMILLE,
  INDIGENAT = INDIGENAT,
  EVEE = EVEE,
  DH = DH,
  PN = PN,
  LRN = LRN,
  PR_Aquitaine = PR_Aquitaine,
  PR_Limousin = PR_Limousin,
  PR_PC = PR_PC,
  LRR_Aquitaine = LRR_Aquitaine,
  LRR_Limousin = LRR_Limousin,
  LRR_PC = LRR_PC,
  ZNIEFF_Aquitaine = ZNIEFF_Aquitaine,
  ZNIEFF_Limousin = ZNIEFF_Limousin,
  ZNIEFF_PC = ZNIEFF_PC,
  PD19 = PD19,
  PD23 = PD23,
  PD24 = PD24,
  PD33 = PD33,
  PD40 = PD40,
  PD47 = PD47,
  PD64 = PD64,
  PD86 = PD86,
  PD87 = PD87,
  indicatrice_ZH = indicatrice_ZH,
  Barcelonne = Barcelonne,
  Berne = Berne,
  Mondiale = Mondiale,
  Europe = Europe,
  Protection_Nouvelle_Aquitaine = PROTECTION_NOUVELLE_AQUITAINE,
  floraison = floraison,
  ecologie = ecologie,
  syntaxon = syntaxon,
  PATH_IMG = PATH_IMG,
  TEXTE_LEGEND_IMG = TEXTE_LEGEND_IMG
)

write.csv(TABLEAU_GENERAL,file = "TAB_GEN_METH_ENJEU_Nouvelle-Aquitaine.csv",row.names = F,fileEncoding = "UTF-8",na="-")
