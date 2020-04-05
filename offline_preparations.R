
# Loading libraries

library(readxl)
library(readr)
library(ggpubr)
library(here)
library(scales)
library(RColorBrewer)
library(tidyverse)
library(shinydashboard)
library(plotly)
library(DT)
library(msaR)
library(Biostrings)
library(seqinr)
library(glue)
library(reticulate)

source(here("webPack", "Functions", "find.single.coverages.R"))

VIRUS = "nCoV"

REGIONS = c("World", "East Asia", "Northeast Asia", "South Asia", "Southeast Asia", "Southwest Asia", "Europe", 
            "East Africa", "West Africa", "Central Africa", "North Africa", "South Africa", 
            "West Indies", "North America", "South America", "Central America", "Oceania")


# Load RDS objects ####

refseq <- readRDS(here("webPack", "Data", "refseq"))

df_Bepitopes <- readRDS(here("webPack", "Data", paste0("df-Bepitopes-", VIRUS)))  

df_MHCepitopes <- readRDS(here("webPack", "Data", paste0("df-MHCepitopes-", VIRUS)))

df_Tepitopes <- readRDS(here("webPack", "Data", paste0("df-Tepitopes-", VIRUS)))

# Collecting T epitopes from both types of assays ######

df_onlyTepitopes <- df_Tepitopes %>% filter(!(IEDB %in% df_MHCepitopes$IEDB))

df_TMHCepitopes <- bind_rows(df_MHCepitopes, df_onlyTepitopes) 

df_TMHCepitopes <- df_TMHCepitopes %>% 
  mutate(Assay = if_else(condition = (df_TMHCepitopes$IEDB %in% df_Tepitopes$IEDB), 
                         true = "T cell", false = "MHC ligand"))

saveRDS(object = df_TMHCepitopes, file = here("webPack", "Data", paste0("df-TMHCepitopes-", VIRUS)))



# Preparing population coverages of single epitopes #####

df_TMHC_PopSCov <- df_TMHCepitopes %>% 
  mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", df_TMHCepitopes$IEDB, 
                       "\"target=\"_blank\">", df_TMHCepitopes$IEDB, "</a>")) %>% 
  mutate(`MHC Allele Class` = 
           str_split_fixed(string = df_TMHCepitopes$`MHC Allele Classes`, 
                           pattern = ",", n = 2)[,1]) %>% 
  select(IEDB, Epitope, Protein, `MHC Allele Class`, `MHC Allele Names`, nCoV)

df_TMHC_PopSCov$`MHC Allele Names` <- df_TMHC_PopSCov$`MHC Allele Names` %>% 
  str_replace_all(pattern = "/", replacement = ",")

region <- REGIONS

# setwd(here("Utils", "population_coverage"))
# use_condaenv(condaenv = "py27", required = TRUE)
# py_config()
# system("python configure.py")

df_single_coverages <- purrr::map2_dfr(rep(list(df_TMHC_PopSCov),length(region)), 
                                       as.list(region), find.single.coverages)

saveRDS(object = df_single_coverages, file = here("webPack", "Data", "df-single-coverages-ID70"))

setwd(here())
