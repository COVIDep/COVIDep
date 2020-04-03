
# Global constants ####

WEBAPP = "WebApp"

VIRUS = "nCoV"

ID_THRESH = 0.95

# User interface #####

MSA_HEIGHT = 115#400
GB_PLOT_HEIGHT = 250
GB_HEIGHT = 300#250
S_LENGTH = 1273
E_LENGTH = 74
M_LENGTH = 222
N_LENGTH = 419
orf1a_LENGTH = 4401
orf1b_LENGTH = 2695
ORF3a_LENGTH = 275
ORF6_LENGTH = 61
ORF7a_LENGTH = 121
ORF7b_LENGTH = 43
ORF8_LENGTH = 121
ORF10_LENGTH = 38

# REGIONS = c("World", "China", "United States", "Australia", "Netherlands", "United Kingdom", "Switzerland", "Singapore",
            # "Japan", "Korea; South", "Hong Kong", "France", "Italy", "Iran")

# Load RDS objects ####

translation_indices <- readRDS(file = here::here("Data", "translation_indices"))

df_single_coverages_REF <- readRDS(file = here::here("Data", "df-single-coverages-ID70"))

# df_single_coverages_REF <- df_single_coverages_REF %>% replace_na(replace = list(Scov = "N/A"))

PROTEINS <- translation_indices$gene

refseq <- readRDS(here::here("Data", "refseq"))

df_Bepitopes_REF <- readRDS(here::here("Data", paste0("df-Bepitopes-", VIRUS)))  
names(df_Bepitopes_REF) <- c("Protein", "IEDB",    "Epitope", "Length",  "Start",   "End",     "nCoV",    "Source" )

df_MHCepitopes_REF <- readRDS(here::here("Data", paste0("df-MHCepitopes-", VIRUS)))
names(df_MHCepitopes_REF) <- c("Protein", "IEDB", "Epitope", "Length", "Start", "End", "nCoV", "MHC Allele Names", "MHC Allele Classes", "Source")

df_Tepitopes_REF <- readRDS(here::here("Data", paste0("df-Tepitopes-", VIRUS)))
names(df_Tepitopes_REF) <- c("Protein", "IEDB", "Epitope", "Length", "Start", "End", "nCoV", "MHC Allele Names", "MHC Allele Classes", "Source") 

# df_TMHCepitopes <- readRDS(here::here("Data", paste0("df-TMHCepitopes-", VIRUS)))

# Collecting T epitopes from both types of assays ######

df_onlyTepitopes <- df_Tepitopes_REF %>% filter(!(IEDB %in% df_MHCepitopes_REF$IEDB))

df_TMHCepitopes_REF <- bind_rows(df_MHCepitopes_REF, df_onlyTepitopes) 

df_TMHCepitopes_REF <- df_TMHCepitopes_REF %>% 
  mutate(Assay = if_else(condition = (df_TMHCepitopes_REF$IEDB %in% df_Tepitopes_REF$IEDB), 
                         true = "T cell", false = "MHC ligand"))

temp <- left_join(x = df_single_coverages_REF %>% filter(Epitope %in% df_TMHCepitopes_REF$Epitope) %>% 
                    select(-nCoV), y = df_TMHCepitopes_REF %>% select(Epitope, nCoV, Assay), by = "Epitope")

df_single_coverages_REF <- temp

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

# write.fasta(
#   sequences = as.list(ExEpiTcell$Epitope),
#   names = ExEpiTcell$IEDB,
#   file.out = here::here("Data", paste0("Tepitopes-", VIRUS, ".fasta")))


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

# write.fasta(
#   sequences = as.list(ExEpiMHCTcell$Epitope),
#   names = ExEpiMHCTcell$IEDB,
#   file.out = here::here("Data", paste0("MHCepitopes-", VIRUS, ".fasta")))


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

# write.fasta(
#   sequences = as.list(ExEpiBcell$Epitope),
#   names = ExEpiBcell$IEDB,
#   file.out = here::here("Data", paste0("Bepitopes-", VIRUS, ".fasta")))

# # Setting up dataframes #######

df_epitopes <- bind_rows(ExEpiTcell %>% mutate(Type = "T cell", Assay = "T-cell-assay"),
                         ExEpiMHCTcell %>% mutate(Type = "T cell", Assay = "MHC-assay"),
                         ExEpiBcell %>% mutate(Type = "B cell", Assay = "B-cell-assay"))

# saveRDS(object = df_epitopes, file = here::here("Data", "df_epitopes"))

# Preparing IEDB acknowledgement table

# ack_IEDBref <- read_csv(file = here::here("Data", "IEDB_reference_table.csv"), skip = 1, col_names = TRUE)  
