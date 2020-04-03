
library(here)
library(seqinr)
library(msa)
library(msaR)
library(Biostrings)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(doParallel)
library(foreach)
library(readxl)
library(readr)
library(reticulate)
library(ggpubr)
library(fs)

ID_THRESH_MIN = 0.70
NUM_MAX_MISMATCH = 0
GAPratio = 0.15

if (file_exists(path = here::here("mafft-linux", "input"))) {
  file_delete(here::here("mafft-linux", "input"))
}
if (file_exists(path = here::here("mafft-linux", "output"))) {
  file_delete(here::here("mafft-linux", "output"))
}

source(file = here::here("Functions", "prepare.proteinMSA.R"))
source(file = here::here("Functions", "map.Tepitopes.R"))
source(file = here::here("Functions", "map.MHCepitopes.R"))
source(file = here::here("Functions", "map.Bepitopes.R"))
source(file = here::here("Functions", "map.REFepitopes.R"))

translation_indices <- readRDS(file = here::here("Data", "translation_indices"))

VIRUS = "nCoV"

PROTEINS <- translation_indices$gene

# Reference genome::
#   Virus name:	BetaCoV/Wuhan-Hu-1/2019
#   Accession ID:	EPI_ISL_402125
#   Full: BetaCoV/Wuhan-Hu-1/2019|EPI_ISL_402125

REF_genome_full_name <- "BetaCoV/Wuhan-Hu-1/2019|EPI_ISL_402125"

REF_genome_Acc_ID <- "EPI_ISL_402125"

# Analysis --------

dir_create(path = here::here("Data", "Filtered"))

raw_ls <- dir_ls(path = here::here("Data", "Raw"))

exclude_ls <- c("EPI_ISL_402121", "EPI_ISL_402126", "EPI_ISL_403928", "EPI_ISL_403931",
                "EPI_ISL_402120", "EPI_ISL_406959", "EPI_ISL_406960", "EPI_ISL_408068",
                "EPI_ISL_408975", "EPI_ISL_415787")

Incomplete_Genomes <- c("EPI_ISL_402126", "EPI_ISL_408068", "EPI_ISL_406959", "EPI_ISL_406960",
                        "EPI_ISL_408975")
Flagged_Genomes <- c("EPI_ISL_402120", "EPI_ISL_402121", "EPI_ISL_403928", "EPI_ISL_403931")

# remove(filtered_ls)
filtered_ls <- vector(mode = "character")

for (f in raw_ls) {
  if (sum(str_detect(string = f, pattern = exclude_ls)) == 0) {
    filtered_ls <- c(filtered_ls, f)
    temp <- str_split(string = f, pattern = "/")[[1]]
    file_copy(path = f, new_path = here::here("Data", "Filtered", temp[length(temp)]), overwrite = TRUE)
  }

}

Acc_IDs <- list.files(path = here::here("Data", "Filtered"))
Acc_IDs <- Acc_IDs %>% str_remove(pattern = ".fasta")

genomes <- list()
g_names <- list()

for (f in filtered_ls) {
  g <- read.fasta(file = f, seqtype = "DNA", whole.header = TRUE, as.string = TRUE)
  genomes <- c(genomes, as.character(g))
  g_name <- Acc_IDs[which(str_detect(string = f, pattern = Acc_IDs))]
  g_names <- c(g_names, g_name)
}

seqinr::write.fasta(sequences = genomes,
                    names = g_names,
                    as.string = FALSE,
                    file.out = here::here("mafft-linux", "input"))

setwd(here::here("mafft-linux"))
system("mafft input > output")

dir_create(path = here::here("Data", "MSAs"))

file_copy(path = here::here("mafft-linux", "output"),
          new_path = here::here("Data", "MSAs", "genome.fasta"), overwrite = TRUE)

setwd(here())

# Ensure alignment is along reference Wuhan-Hu-1 sequence ####

data_file <- here::here("Data", "MSAs", "genome.fasta")
align_MSA <- read.alignment(file = data_file, format = "fasta")

x <- seqinr::dist.alignment(x = align_MSA, matrix = "identity")
y <- as.matrix(x)

z <- y[REF_genome_Acc_ID,]

exclude_ls <- c(exclude_ls, which(z > 0.25) %>% names()) %>% unique()

dir_delete(path = here::here("Data", "Filtered"))

dir_create(path = here::here("Data", "Filtered"))

filtered_ls <- vector(mode = "character")

for (f in raw_ls) {
  if (sum(str_detect(string = f, pattern = exclude_ls)) == 0) {
    filtered_ls <- c(filtered_ls, f)
    temp <- str_split(string = f, pattern = "/")[[1]]
    file_copy(path = f, new_path = here::here("Data", "Filtered", temp[length(temp)]), overwrite = TRUE)
  }

}

Acc_IDs <- list.files(path = here::here("Data", "Filtered"))
Acc_IDs <- Acc_IDs %>% str_remove(pattern = ".fasta")

genome_Mat <- toupper(as.matrix.alignment(x = align_MSA))

ref_num <- genome_Mat %>% rownames() %>% str_which(REF_genome_full_name)

# genome_Mat <- genome_Mat[-ref_num,]

# subStart <- genome_Mat[ref_num,] %>% str_which("-", negate = TRUE) %>% min()
# subEnd <- genome_Mat[ref_num,] %>% str_which("-", negate = TRUE) %>% max()

# MSA_Mat <- genome_Mat[, seq.int(subStart, subEnd)]

MSA_Mat <- genome_Mat[, genome_Mat[ref_num,] %>% str_which("-", negate = TRUE)]

numSeq <- dim(MSA_Mat)[1]
numPos <- dim(MSA_Mat)[2]

MSA_list <- list()
for (j in seq.int(1,dim(MSA_Mat)[1])) {
  MSA_list[[j]] <- seqinr::c2s(MSA_Mat[j,])
}

seqinr::write.fasta(sequences = MSA_list,
                    names = as.list(rownames(genome_Mat)),
                    as.string = TRUE,
                    file.out = here::here("Data", "MSAs", "genome_referenced.fasta"))

data_file <- here::here("Data", "MSAs", "genome_referenced.fasta")
align_MSA_referenced <- read.alignment(file = data_file, format = "fasta")

DNA_Mat <- toupper(as.matrix.alignment(x = align_MSA_referenced))

for (l in seq.int(1,length(translation_indices$gene))) {
  prepare.proteinMSA(DNA_Mat = DNA_Mat,
                     PROTEIN = translation_indices$gene[l],
                     START = translation_indices$start[l],
                     END = translation_indices$end[l],
                     CODON_AMBIGUOUS = TRUE)
}

# Importing the positive-T cell assay based T cell epitopes data -----

ExEpiTcell_full <- as.data.frame(read_excel(here::here("Data", paste0("Tcell-", VIRUS, "-Results.xls")),
                                            sheet = "Results"))

ExEpiTcell_full %>% separate_rows(Host, `Assay Type Category`, `Assay Result`, `MHC Allele Name`,
                                  `MHC Allele Class`, Method, Measurement, sep = ",") -> ExEpiTcell_full

ExEpiTcell <- ExEpiTcell_full %>%
  filter(Host == "Human" &
           str_detect(string = `Assay Result`, pattern = "Positive") &
           str_detect(string = `Assay Type Category`, pattern = "T")) %>%
  # filter(!str_detect(string = `MHC Allele Name`, pattern = "N/A")) %>%
  # filter(!str_detect(string = `MHC Allele Name`, pattern = "class")) %>%
  select(`IEDB ID`, `Epitope Sequence`, `MHC Allele Name`, `MHC Allele Class`) %>%
  group_by(`IEDB ID`, `Epitope Sequence`) %>%
  distinct_at(vars(`MHC Allele Name`), .keep_all = TRUE) %>%
  summarise(`MHC Allele Names` = paste(`MHC Allele Name`, collapse = "/"),
            `MHC Allele Classes` = paste(`MHC Allele Class`, collapse = ",")) %>%
  distinct_at(vars(`IEDB ID`), .keep_all = TRUE) %>%
  ungroup()

colnames(ExEpiTcell) <- c("IEDB", "Epitope", "MHC Allele Names", "MHC Allele Classes")

write.fasta(
  sequences = as.list(ExEpiTcell$Epitope),
  names = ExEpiTcell$IEDB,
  file.out = here::here("Data", paste0("Tepitopes-", VIRUS, ".fasta")))

# Importing the positive-MHC assay based T cell epitopes data -----

ExEpiMHCTcell_full <- as.data.frame(read_excel(here::here("Data", paste0("MHC-", VIRUS, "-Results.xls")),
                                               sheet = "Results"))

ExEpiMHCTcell_full %>% separate_rows(Host, `Assay Type Category`, `Assay Result`, `MHC Allele Name`,
                                     `MHC Allele Class`, Method, Measurement, sep = ",") -> ExEpiMHCTcell_full

ExEpiMHCTcell <- ExEpiMHCTcell_full %>%
  filter(Host == "Human" &
           str_detect(string = `Assay Result`, pattern = "Positive")) %>%
  # filter(!str_detect(string = `MHC Allele Name`, pattern = "N/A")) %>%
  # filter(!str_detect(string = `MHC Allele Name`, pattern = "class")) %>%
  select(`IEDB ID`, `Epitope Sequence`, `MHC Allele Name`, `MHC Allele Class`) %>%
  group_by(`IEDB ID`, `Epitope Sequence`) %>%
  distinct_at(vars(`MHC Allele Name`), .keep_all = TRUE) %>%
  summarise(`MHC Allele Names` = paste(`MHC Allele Name`, collapse = "/"),
            `MHC Allele Classes` = paste(`MHC Allele Class`, collapse = ",")) %>%
  distinct_at(vars(`IEDB ID`), .keep_all = TRUE) %>%
  ungroup()

colnames(ExEpiMHCTcell) <- c("IEDB", "Epitope", "MHC Allele Names", "MHC Allele Classes")

write.fasta(
  sequences = as.list(ExEpiMHCTcell$Epitope),
  names = ExEpiMHCTcell$IEDB,
  file.out = here::here("Data", paste0("MHCepitopes-", VIRUS, ".fasta")))


# Importing B cell epitopes data -----

ExEpiBcell_full <- as.data.frame(read_excel(here::here("Data", paste0("Bcell-", VIRUS, "-Results.xls")),
                                            sheet = "Results"))

ExEpiBcell_full %>% separate_rows(Host, `Assay Type Category`, `Assay Result`, Method, Measurement, sep = ",") ->
  ExEpiBcell_full

ExEpiBcell <- ExEpiBcell_full %>%
  filter(Host == "Human" &
           str_detect(string = `Assay Result`, pattern = "Positive")) %>%
  select(`IEDB ID`, `Epitope Sequence`) %>%
  distinct_at(vars(`IEDB ID`), .keep_all = TRUE) %>%
  ungroup()

colnames(ExEpiBcell) <- c("IEDB", "Epitope")

write.fasta(
  sequences = as.list(ExEpiBcell$Epitope),
  names = ExEpiBcell$IEDB,
  file.out = here::here("Data", paste0("Bepitopes-", VIRUS, ".fasta")))


# Mapping ----------

source(file = here::here("Functions", "map.Tepitopes.R"))
source(file = here::here("Functions", "map.MHCepitopes.R"))
source(file = here::here("Functions", "map.Bepitopes.R"))
source(file = here::here("Functions", "map.REFepitopes.R"))

# Mapping positive-T cell assay based T cell epitopes ----

VIRUSES <- c(VIRUS)

PROTEINS <- translation_indices$gene

cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)

cases_PROTEINS <- c(rep(PROTEINS, times = length(VIRUSES)))
cases_VIRUSES <- c(rep(VIRUSES, each = length(PROTEINS)))

df_mapping_full <- foreach(i = seq_along(cases_VIRUSES), .combine = 'rbind.data.frame') %dopar% {
  map.Tepitopes(VIRUS = cases_VIRUSES[i], PROTEIN = cases_PROTEINS[i], NUM_MAX_MISMATCH, GAPratio)
}

parallel::stopCluster(cl)

df <- df_mapping_full %>% 
  filter(Id!=0) %>%
  select("Virus", "Protein", "IEDB", "Epitope", "Id", "length", "start", "end") %>% 
  spread(key = Virus, value = Id, fill = 0)

df <- left_join(df, ExEpiTcell, c("IEDB", "Epitope"))

saveRDS(object = df, file = here::here("Data", "Latest", paste0("df-allTepitopes-", VIRUS)))

df <- df %>% filter(nCoV >= ID_THRESH_MIN) 

df$Source <- "SARS" #VIRUS

saveRDS(object = df, file = here::here("Data", "Latest", paste0("df-Tepitopes-", VIRUS)))

# Mapping positive-MHC assay based T cell epitopes ----

VIRUSES <- c(VIRUS)

PROTEINS <- translation_indices$gene 

cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)

cases_PROTEINS <- c(rep(PROTEINS, times = length(VIRUSES)))
cases_VIRUSES <- c(rep(VIRUSES, each = length(PROTEINS)))

df_mapping_full <- foreach(i = seq_along(cases_VIRUSES), .combine = 'rbind.data.frame') %dopar% {
  map.MHCepitopes(VIRUS = cases_VIRUSES[i], PROTEIN = cases_PROTEINS[i], NUM_MAX_MISMATCH, GAPratio)
}

parallel::stopCluster(cl)

df <- df_mapping_full %>% 
  filter(Id!=0) %>%
  select("Virus", "Protein", "IEDB", "Epitope", "Id", "length", "start", "end") %>% distinct() %>% 
  spread(key = Virus, value = Id, fill = 0)

df <- left_join(df, ExEpiMHCTcell, c("IEDB", "Epitope"))

saveRDS(object = df, file = here::here("Data", "Latest", paste0("df-allMHCepitopes-", VIRUS)))

df <- df %>% filter(nCoV >= ID_THRESH_MIN) 

df$Source <- "SARS" #VIRUS

saveRDS(object = df, file = here::here("Data", "Latest", paste0("df-MHCepitopes-", VIRUS)))


# Mapping linear B cell epitopes ----

VIRUSES <- c(VIRUS)

PROTEINS <- translation_indices$gene 

cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)

cases_PROTEINS <- c(rep(PROTEINS, times = length(VIRUSES)))
cases_VIRUSES <- c(rep(VIRUSES, each = length(PROTEINS)))

df_mapping_full <- foreach(i = seq_along(cases_VIRUSES), .combine = 'rbind.data.frame') %dopar% {
  map.Bepitopes(VIRUS = cases_VIRUSES[i], PROTEIN = cases_PROTEINS[i], NUM_MAX_MISMATCH, GAPratio)
}

parallel::stopCluster(cl)

df <- df_mapping_full %>% 
  filter(Id!=0) %>% 
  select("Virus", "Protein", "IEDB", "Epitope", "Id", "length", "start", "end") %>% 
  spread(key = Virus, value = Id, fill = 0)

df <- left_join(df, ExEpiBcell, c("IEDB", "Epitope"))

saveRDS(object = df, file = here::here("Data", "Latest", paste0("df-allBepitopes-", VIRUS)))

df <- df %>% filter(nCoV >= ID_THRESH_MIN) 

df$Source <- "SARS" #VIRUS

saveRDS(object = df, file = here::here("Data", "Latest", paste0("df-Bepitopes-", VIRUS)))


# Date ######

UPDATE_DATE <- Sys.Date()
saveRDS(object = UPDATE_DATE, file = here::here("UPDATE_DATE"))

# Loading dfs ####

df_Bepitopes <- readRDS(here::here("Data", "Latest", paste0("df-Bepitopes-", VIRUS)))

df_MHCepitopes <- readRDS(here::here("Data", "Latest", paste0("df-MHCepitopes-", VIRUS)))

df_Tepitopes <- readRDS(here::here("Data", "Latest", paste0("df-Tepitopes-", VIRUS)))

# # Data from previous versions -----
# 
# 
# PRE_df_Tepitopes <- readRDS(here("Data", "Previous21Feb2020", paste0("df-Tepitopes-", VIRUS)))
# 
# PRE_df_MHCepitopes <- readRDS(here("Data", "Previous21Feb2020", paste0("df-MHCepitopes-", VIRUS)))
# 
# PRE_df_Bepitopes <- readRDS(here("Data", "Previous21Feb2020", paste0("df-Bepitopes-", VIRUS)))
# 
# PRE_df_Bepitopes %>% filter(IEDB %in% setdiff(x = PRE_df_Bepitopes$IEDB, y = df_Bepitopes$IEDB))
# 
# PRE_df_MHCepitopes %>% filter(IEDB %in% setdiff(x = PRE_df_MHCepitopes$IEDB, y = df_MHCepitopes$IEDB))
# 
# PRE_df_Tepitopes %>% filter(IEDB %in% setdiff(x = PRE_df_Tepitopes$IEDB, y = df_Tepitopes$IEDB))
