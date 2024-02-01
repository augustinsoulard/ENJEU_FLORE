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
  filter(BDC_STATUTS_17$CD_NOM %in% TAXREFv17_FLORE_FR$CD_REF)

#Creer le futur nom des colonnes
BDC_STATUTS_17_FLORE$LB_STATUT_COL = make.names(paste0(BDC_STATUTS_17_FLORE$LB_TYPE_STATUT,"_",
                                        BDC_STATUTS_17_FLORE$LB_ADM_TR))

# Selection des statuts à utiliser
BDC_STATUTS_17_FLORE = BDC_STATUTS_17_FLORE %>%
  filter(LB_STATUT_COL %in% 
           c("Convention.de.Barcelone_France.métropolitaine", "Convention.de.Berne_France.métropolitaine", 
             "Directive.Habitat_France.métropolitaine", "Interdiction.d.introduction_France.métropolitaine", 
             "Liste.rouge.européenne_Europe", "Liste.rouge.mondiale_Monde", 
             "Liste.rouge.nationale_France.métropolitaine",
             "Liste.rouge.régionale_Rhône.Alpes", "Lutte.contre.certaines.espèces_France.métropolitaine", 
             "Plan.national.terminé_France", 
             "Priorité.action.publique.nationale_France.métropolitaine", 
             "Protection.départementale_Ain", "Protection.départementale_Drôme", 
            "Protection.départementale_Haute.Savoie", 
            "Protection.départementale_Isère","Protection.départementale_Loire",
             "Protection.nationale_France.métropolitaine", "Protection.régionale_Rhône.Alpes",
             "Sensibilité.départementale_Ain", "Sensibilité.départementale_Ardèche",
            "Sensibilité.départementale_Cantal", 
             "Sensibilité.départementale_Drôme", "Sensibilité.départementale_Haute.Loire", 
             "Sensibilité.départementale_Haute.Savoie", 
             "Sensibilité.départementale_Isère", "Sensibilité.départementale_Rhône", 
             "Sensibilité.départementale_Savoie", "Sensibilité.nationale_France.métropolitaine", 
             "Sensibilité.régionale_Auvergne.Rhône.Alpes", 
             "ZNIEFF.Déterminantes_Ain", 
             "ZNIEFF.Déterminantes_Allier", 
             "ZNIEFF.Déterminantes_Ardèche","ZNIEFF.Déterminantes_Cantal", 
             "ZNIEFF.Déterminantes_Drôme", "ZNIEFF.Déterminantes_Haute.Saône", 
            "ZNIEFF.Déterminantes_Haute.Savoie", 
             "ZNIEFF.Déterminantes_Isère", 
             "ZNIEFF.Déterminantes_Loire", 
              "ZNIEFF.Déterminantes_Puy.de.Dôme", 
             "ZNIEFF.Déterminantes_Rhône", 
             "ZNIEFF.Déterminantes_Savoie"
           ))

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



#Selection des colonnes pour le tableau final
TAXREFv17_FLORE_JOIN %>% select(CD_NOM = CD_REF,TRIGRAMME,NOM_VALIDE,
                                NOM_VERN,INDIGENAT = FR, 
                                EVEE_FR = Lutte.contre.certaines.espèces_France.métropolitaine,
                                DH = Directive.Habitat_France.métropolitaine,
                                PN = Protection.nationale_France.métropolitaine,
                                LRN = Liste.rouge.nationale_France.métropolitaine,
                                PR =,
                                PR_Corr =,
                                LRR = ,
                                ZNIEFF = ,
                                PD01 = ,
                                PD03 = ,
                                PD07 = ,
                                PD15 =,
                                PD26 =,
                                PD38 = ,
                                PD42 = ,
                                PD43 = ,
                                PD63 = ,
                                PD69 = ,
                                PD73 = ,
                                PD74 = ,
                                Barcelonne =,
                                Berne = ,
                                Mondiale = ,
                                Europe =,
                                )


#Enregistrement du tableau a integrer à la methode enjeu PACA
write.csv(TAXREFv17_FLORE_JOIN,file = "TAXREFv17_FLORE_JOIN.csv",row.names = F,fileEncoding = "UTF-8",na="-")
