
# CREATION DE TABLEAU_GENERAL ##################################

# Voir mise à jour TAXREF plus bas ||
#                                  V

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
           CD_ISO3166_2 %in% c("FR-09","FR-11","FR-12","FR-30","FR-31","FR-32","FR-34","FR-46",
                               "FR-48","FR-65","FR-66","FR-81","FR-82","FR-K","FR-N"))

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
TAB_EVEE <- read_excel("../../../BDD_FLORE_CONSTRUCT/EEE/Occitanie/Listes_PEE_biogeographiques_2021_Occitanie.xlsx")
TAB_EVEE$CD_NOM = updatetaxa(TAB_EVEE$CD_REF_12)
TAB_EVEE = TAB_EVEE %>% select(CD_NOM,EEEmed = `Catégorie MED`,EEEmc = `Catégorie MC`,
                               EEEso = `Catégorie SO`,EEEpyr =`Catégorie PYR`)

# Jointure flore EVEE
TAXREF_FLORE_JOIN = left_join(TAXREF_FLORE_JOIN,TAB_EVEE,by=c("CD_REF"="CD_NOM"))

# Chargement enjeu CBN
ENJEU_CBN <- read.csv2("../../HIERARCHISATION_CBN_PACA/WORKFLOW/HIERARCHISATION_ENJEU_RESEDA_TAXREFv18.csv")
ENJEU_CBN$CD_NOM = as.double(ENJEU_CBN$CD_NOM)
ENJEU_CBN = ENJEU_CBN %>% dplyr::select(CD_NOM,ENJEU_CONSERVATION=ENJEU_CONSERVATION)

# Jointure enjeu CBN
TAXREF_FLORE_JOIN$CD_NOM = as.double(TAXREF_FLORE_JOIN$CD_NOM)
TAXREF_FLORE_JOIN = left_join(TAXREF_FLORE_JOIN,ENJEU_CBN,by=c("CD_NOM"="CD_NOM"))

#Selection des colonnes pour le tableau final
TAXREF_FLORE_JOIN = TAXREF_FLORE_JOIN %>% dplyr::select(CD_NOM = CD_NOM,
                                                       TRIGRAMME=TRIGRAMME,
                                                       NOM_VALIDE=NOM_VALIDE,
                                                       NOM_VERN=NOM_VERN,
                                                       FAMILLE=FAMILLE,
                                                       SOUS_FAMILLE=SOUS_FAMILLE,
                                                       INDIGENAT = FR, 
                                                       EVEEmed = EEEmed,
                                                       EVEEmc = EEEmc,
                                                       EVEEso = EEEso,
                                                       EVEEpyr = EEEpyr,
                                                       DH = Directive.Habitat_France.métropolitaine,
                                                       PN = Protection.nationale_France.métropolitaine,
                                                       LRN = Liste.rouge.nationale_France.métropolitaine,
                                                       PR_LR = Protection.régionale_Languedoc.Roussillon,
                                                      PR_MP = Protection.régionale_Midi.Pyrénées,
                                                        LRR_MP = Liste.rouge.régionale_Midi.Pyrénées,
                                                       ZNIEFF_LR = ZNIEFF.Déterminantes_Languedoc.Roussillon,
                                                      ZNIEFF_09 = ZNIEFF.Déterminantes_Ariège,
                                                      ZNIEFF_12 = ZNIEFF.Déterminantes_Aveyron,
                                                      ZNIEFF_31 = ZNIEFF.Déterminantes_Haute.Garonne,
                                                      ZNIEFF_32 = ZNIEFF.Déterminantes_Gers,
                                                      ZNIEFF_46 = ZNIEFF.Déterminantes_Lot,
                                                      ZNIEFF_48 = ZNIEFF.Déterminantes_Lozère,
                                                      ZNIEFF_65 = ZNIEFF.Déterminantes_Hautes.Pyrénées,
                                                      ZNIEFF_82 = ZNIEFF.Déterminantes_Tarn.et.Garonne,
                                                      PD09 = Protection.départementale_Ariège,
                                                      PD12 = Protection.départementale_Aveyron,
                                                      PD31 = Protection.départementale_Haute.Garonne,
                                                      PD32 = Protection.départementale_Gers,
                                                      PD46 = Protection.départementale_Lot,
                                                      PD48 = Protection.départementale_Lozère,
                                                       PD65 = Protection.départementale_Hautes.Pyrénées,
                                                       PD81 = Protection.départementale_Tarn,
                                                       PD82 = Protection.départementale_Tarn.et.Garonne,
                                                       ENJEU_CBN = ENJEU_CONSERVATION,
                                                       indicatrice_ZH = INDIC_ZH,
                                                       Barcelonne =Convention.de.Barcelone_France.métropolitaine,
                                                       Berne = Convention.de.Berne_France.métropolitaine,
                                                       Mondiale = Liste.rouge.mondiale_Monde,
                                                       Europe = Liste.rouge.européenne_Europe
)
TAXREF_FLORE_JOIN$LRR_LR = "-"

## Mise en forme colonne ZNIEFF ####
TAXREF_FLORE_JOIN <- TAXREF_FLORE_JOIN %>%
  mutate(across(starts_with("ZNIEFF"), ~ ifelse(!is.na(.), "D", .)))

#Création de la colonne Protection AuRA
ProtectionOccitanie = function(x){
  valeur = NULL
  if(!is.na(x["PN"]) && x["PN"] != "-"){
    valeur = paste0(valeur," ","PN")
  }
  if(!is.na(x["PR_LR"]) && x["PR_LR"] != "-"){
    valeur = paste0(valeur," ","PR_LR")
  }
  if(!is.na(x["PR_MP"]) && x["PR_MP"] != "-"){
    valeur = paste0(valeur," ","PR_MP")
  }
  if(!is.na(x["PD09"]) && x["PD09"] != "-"){
    valeur = paste0(valeur," ","PD09")
  }
  if(!is.na(x["PD12"]) && x["PD12"] != "-"){
    valeur = paste0(valeur," ","PD12")
  }
  if(!is.na(x["PD31"]) && x["PD31"] != "-"){
    valeur = paste0(valeur," ","PD31")
  }
  if(!is.na(x["PD32"]) && x["PD32"] != "-"){
    valeur = paste0(valeur," ","PD32")
  }
  if(!is.na(x["PD46"]) && x["PD46"] != "-"){
    valeur = paste0(valeur," ","PD46")
  }
  if(!is.na(x["PD46"]) && x["PD46"] != "-"){
    valeur = paste0(valeur," ","PD46")
  }
  if(!is.na(x["PD48"]) && x["PD48"] != "-"){
    valeur = paste0(valeur," ","PD48")
  }
  if(!is.na(x["PD65"]) && x["PD65"] != "-"){
    valeur = paste0(valeur," ","PD65")
  }
  if(!is.na(x["PD81"]) && x["PD81"] != "-"){
    valeur = paste0(valeur," ","PD81")
  }
  if(!is.na(x["PD82"]) && x["PD82"] != "-"){
    valeur = paste0(valeur," ","PD82")
  }
  if(is.null(valeur)){
    valeur = "-"
  }
  return(valeur)
}

TAXREF_FLORE_JOIN$PROTECTION_PACA = apply(TAXREF_FLORE_JOIN,1,ProtectionPACA)

## Ajout de baseflor ####
baseflor_bryoTAXREF <- read.csv2("../../../BDD_FLORE_CONSTRUCT/TAXONOMIE/TAXREF-MATCH-BASEFLOR/baseflor_bryoTAXREFv17.csv")

baseflor_bryoTAXREF = baseflor_bryoTAXREF %>% select(CD_NOM=CD_NOM,
                                                           floraison=floraison,ecologie = CARACTERISATION_ECOLOGIQUE_.HABITAT_OPTIMAL.,
                                                           syntaxon = INDICATION_PHYTOSOCIOLOGIQUE_CARACTERISTIQUE)

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
baseflor_bryoTAXREF$CD_NOM = as.double(baseflor_bryoTAXREF$CD_NOM)
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
  EVEEmed = EVEEmed,
  EVEEmc = EVEEmc,
  EVEEso = EVEEso,
  EVEEpyr = EVEEpyr,
  DH = DH,
  PN = PN,
  LRN = LRN,
  PR_LR = PR_LR,
  PR_MP = PR_MP,
  LRR_LR = LRR_LR,
  LRR_MP = LRR_MP,
  ZNIEFF_LR = ZNIEFF_LR,
  ZNIEFF_09 = ZNIEFF_09,
  ZNIEFF_12 = ZNIEFF_12,
  ZNIEFF_31 = ZNIEFF_31,
  ZNIEFF_32 = ZNIEFF_32,
  ZNIEFF_46 = ZNIEFF_46,
  ZNIEFF_48 = ZNIEFF_48,
  ZNIEFF_65 = ZNIEFF_65,
  ZNIEFF_82 = ZNIEFF_82,
  PD09 = PD09,
  PD12 = PD12,
  PD31 = PD31,
  PD32 = PD32,
  PD46 = PD46,
  PD48 = PD48,
  PD65 = PD65,
  PD81 = PD81,
  PD82 = PD82,
    ENJEU_CBN = ENJEU_CBN,
  indicatrice_ZH = indicatrice_ZH,
  Barcelonne = Barcelonne,
  Berne = Berne,
  Mondiale = Mondiale,
  Europe = Europe,
  Protection_Occitanie = ProtectionOccitanie,
  floraison = floraison,
  ecologie = ecologie,
  syntaxon = syntaxon,
  PATH_IMG = PATH_IMG,
  TEXTE_LEGEND_IMG = TEXTE_LEGEND_IMG
)

write.csv(TABLEAU_GENERAL,file = "TAB_GEN_METH_ENJEU_Occitanie.csv",row.names = F,fileEncoding = "UTF-8",na="-")
