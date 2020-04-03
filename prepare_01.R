


# SARS-CoV-2 Sequence Accession IDs  ####

Acc_IDs <- list.files(path = here::here("Data", "Filtered"))

Acc_IDs <- Acc_IDs %>% str_remove(pattern = ".fasta")

NUM_GENOMES = length(Acc_IDs)

# Preparing home tab data ######

metadf <- as.data.frame(read_excel(here::here("Data", "Acknowledge.xls"), 
                                   sheet = "Acknowledgement Table", skip = 2, col_names = TRUE))

metadf <- na.omit(metadf)

metadf <- metadf %>% separate(col = Location, into = c("loc1", "loc2", "loc3", "loc4"), sep = "/", fill = "right", remove = FALSE) 

metadf$Country <- if_else(condition = metadf$loc1 %in% c("Asia ", "Europe ", "North America ", 
                                                         "Oceania ", "Africa ", "South America "), 
                          true = str_squish(metadf$loc2), 
                          false = str_squish(metadf$loc1)) 

metadf %<>% mutate(
  date = `Collection date` %>% lubridate::parse_date_time(orders = c("ymd", "ym", "y")) %>% lubridate::date(),
  year = `Collection date` %>% lubridate::parse_date_time(orders = c("ymd", "ym", "y")) %>% lubridate::year(),
  month = `Collection date` %>% lubridate::parse_date_time(orders = c("ymd", "ym")) %>% lubridate::month(),
  day = `Collection date` %>% lubridate::parse_date_time(orders = c("ymd")) %>% lubridate::day())

metadf <- metadf %>% filter(`Accession ID` %in% Acc_IDs)

# Latest collected sequence #####

LATEST <- metadf %>% 
        filter(date == metadf$date %>% max()) %>% 
        select(`Accession ID`, Country) %>% head(1) 

LATEST_DATE <- metadf %>% 
           filter(date == metadf$date %>% max()) %>% 
           pull(date) %>% unique()

# UPDATE_DATE <- Sys.Date()
UPDATE_DATE <- readRDS(file = here::here("UPDATE_DATE"))

# Preparing MSAs and diversity dfs ######

#S
{PROTEIN <- "S"
align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
                                    format = "fasta", forceToLower = FALSE) 
names(align_MSA$seq) <- align_MSA$nam

x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
y <- t(x$AA[,1:S_LENGTH])
z <- t(x$percentage[,1:S_LENGTH])
z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)

df_subs_S <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                      consAA = y[,1],  
                      numSubs = 0,
                      subsAA = "", stringsAsFactors = FALSE)

selected_positions <- vector()

for (i in seq.int(2, dim(y)[2])) {
  selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
}
selected_positions <- unique(selected_positions)
# selected_positions <- which(y[,2] %in% AA_STANDARD)

numResi <- dim(y)[2]

w <- z[,2:numResi]
# w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]

v <- y[,2:numResi]
# v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]

df_subs_S$numSubs[selected_positions] <- w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]

df_subs_S$subsAA[selected_positions] <- v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]

MSA_S <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
  # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
  }
#E
{PROTEIN <- "E"
  align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")),
                                      format = "fasta", forceToLower = FALSE) 
  names(align_MSA$seq) <- align_MSA$nam
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  y <- t(x$AA[,1:E_LENGTH])
  z <- t(x$percentage[,1:E_LENGTH])
  z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)
  
  df_subs_E <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                          consAA = y[,1], 
                          numSubs = 0,
                          subsAA = "", stringsAsFactors = FALSE)

  selected_positions <- vector()
    
  for (i in seq.int(2, dim(y)[2])) {
    selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
  }
  selected_positions <- unique(selected_positions)
  # selected_positions <- which(y[,2] %in% AA_STANDARD)
  
  numResi <- dim(y)[2]
  
  w <- z[,2:numResi]
  # w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  v <- y[,2:numResi]
  # v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_E$numSubs[selected_positions] <- w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_E$subsAA[selected_positions] <- v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  MSA_E <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
    # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
  }
#M
{PROTEIN <- "M"
  align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")),
                                      format = "fasta", forceToLower = FALSE) 
  names(align_MSA$seq) <- align_MSA$nam
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  y <- t(x$AA[,1:M_LENGTH])
  z <- t(x$percentage[,1:M_LENGTH])
  z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)
  
  df_subs_M <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                          consAA = y[,1], 
                          numSubs = 0,
                          subsAA = "", stringsAsFactors = FALSE)
  
  selected_positions <- vector()
  
  for (i in seq.int(2, dim(y)[2])) {
    selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
  }
  selected_positions <- unique(selected_positions)
  # selected_positions <- which(y[,2] %in% AA_STANDARD)
  
  numResi <- dim(y)[2]
  
  w <- z[,2:numResi]
  # w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  v <- y[,2:numResi]
  # v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_M$numSubs[selected_positions] <- w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_M$subsAA[selected_positions] <- v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  MSA_M <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
    # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
  }
#N
{PROTEIN <- "N"
  align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")),
                                      format = "fasta", forceToLower = FALSE) 
  names(align_MSA$seq) <- align_MSA$nam
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  y <- t(x$AA[,1:N_LENGTH])
  z <- t(x$percentage[,1:N_LENGTH])
  z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)
  
  df_subs_N <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                          consAA = y[,1], 
                          numSubs = 0,
                          subsAA = "", stringsAsFactors = FALSE)
  
  selected_positions <- vector()
  
  for (i in seq.int(2, dim(y)[2])) {
    selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
  }
  selected_positions <- unique(selected_positions)
  # selected_positions <- which(y[,2] %in% AA_STANDARD)
  
  numResi <- dim(y)[2]
  
  w <- z[,2:numResi]
  # w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  v <- y[,2:numResi]
  # v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_N$numSubs[selected_positions] <- w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_N$subsAA[selected_positions] <- v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  MSA_N <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
    # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
  }
#orf1a
{PROTEIN <- "orf1a"
  align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")),
                                      format = "fasta", forceToLower = FALSE) 
  names(align_MSA$seq) <- align_MSA$nam
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  
  y <- t(x$AA[,1:orf1a_LENGTH])
  z <- t(x$percentage[,1:orf1a_LENGTH])
  
  z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)
  
  df_subs_orf1a <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                              consAA = y[,1], 
                          numSubs = 0,
                          subsAA = "", stringsAsFactors = FALSE)
  
  selected_positions <- vector()
  
  for (i in seq.int(2, dim(y)[2])) {
    selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
  }
  selected_positions <- unique(selected_positions)
  # selected_positions <- which(y[,2] %in% AA_STANDARD)
  
  numResi <- dim(y)[2]
  
  w <- z[,2:numResi]
  # w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  v <- y[,2:numResi]
  # v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_orf1a$numSubs[selected_positions] <- w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_orf1a$subsAA[selected_positions] <- v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  MSA_orf1a <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
  # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
  }
#orf1b
{PROTEIN <- "orf1b"
  align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")),
                                      format = "fasta", forceToLower = FALSE) 
  names(align_MSA$seq) <- align_MSA$nam
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  
  y <- t(x$AA[,1:orf1b_LENGTH])
  z <- t(x$percentage[,1:orf1b_LENGTH])
  
  z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)
  
  df_subs_orf1b <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                              consAA = y[,1], 
                              numSubs = 0,
                              subsAA = "", stringsAsFactors = FALSE)
  
  selected_positions <- vector()
  
  for (i in seq.int(2, dim(y)[2])) {
    selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
  }
  selected_positions <- unique(selected_positions)
  # selected_positions <- which(y[,2] %in% AA_STANDARD)
  
  numResi <- dim(y)[2]
  # numResi <- max(dim(y)[1], dim(y)[2])
  
  w <- z[,2:numResi]
  # w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  v <- y[,2:numResi]
  # v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_orf1b$numSubs[selected_positions] <- w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_orf1b$subsAA[selected_positions] <- v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  MSA_orf1b <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
  # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
  }
#ORF3a
{PROTEIN <- "ORF3a"
  align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")),
                                      format = "fasta", forceToLower = FALSE) 
  names(align_MSA$seq) <- align_MSA$nam
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  
  y <- t(x$AA[,1:ORF3a_LENGTH])
  z <- t(x$percentage[,1:ORF3a_LENGTH])
  
  z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)
  
  df_subs_ORF3a <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                              consAA = y[,1], 
                              numSubs = 0,
                              subsAA = "", stringsAsFactors = FALSE)
  
  selected_positions <- vector()
  
  for (i in seq.int(2, dim(y)[2])) {
    selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
  }
  selected_positions <- unique(selected_positions)
  # selected_positions <- which(y[,2] %in% AA_STANDARD)
  
  numResi <- dim(y)[2]
  
  w <- z[,2:numResi]
  # w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  v <- y[,2:numResi]
  # v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_ORF3a$numSubs[selected_positions] <- w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_ORF3a$subsAA[selected_positions] <- v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  MSA_ORF3a <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
  # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
  }
#ORF6
{PROTEIN <- "ORF6"
  align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")),
                                      format = "fasta", forceToLower = FALSE) 
  names(align_MSA$seq) <- align_MSA$nam
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  
  y <- t(x$AA[,1:ORF6_LENGTH])
  z <- t(x$percentage[,1:ORF6_LENGTH])
  
  z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)
  
  if (dim(y)[1]==1) {
    
    df_subs_ORF6 <- data.frame(position = seq.int(from = 1, to = sum(y[1,] %in% AA_STANDARD)),
                               consAA = y[,1], 
                               numSubs = 0,
                               subsAA = "", stringsAsFactors = FALSE)
  }
  
  if (dim(y)[1]>1) {
  
    df_subs_ORF6 <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                               consAA = y[,1], 
                               numSubs = 0,
                               subsAA = "", stringsAsFactors = FALSE)
    
    selected_positions <- vector()
    
    for (i in seq.int(2, dim(y)[2])) {
      selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
    }
    selected_positions <- unique(selected_positions)
    # selected_positions <- which(y[,2] %in% AA_STANDARD)
    
    numResi <- dim(y)[2]
    
    w <- z[,2:numResi]
    # w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
    
    v <- y[,2:numResi]
    # v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
    
    df_subs_ORF6$numSubs[selected_positions] <- w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
    
    df_subs_ORF6$subsAA[selected_positions] <- v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  }
  
  
  MSA_ORF6 <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
  # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
  }
#ORF7a
{PROTEIN <- "ORF7a"
  align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")),
                                      format = "fasta", forceToLower = FALSE) 
  names(align_MSA$seq) <- align_MSA$nam
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  
  y <- t(x$AA[,1:ORF7a_LENGTH])
  z <- t(x$percentage[,1:ORF7a_LENGTH])
  
  z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)
  
    df_subs_ORF7a <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                                consAA = y[,1], 
                               numSubs = 0,
                               subsAA = "", stringsAsFactors = FALSE)
    
    selected_positions <- vector()
    
    for (i in seq.int(2, dim(y)[2])) {
      selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
    }
    selected_positions <- unique(selected_positions)
    # selected_positions <- which(y[,2] %in% AA_STANDARD)
    
    numResi <- dim(y)[2]
    
    w <- z[,2:numResi]
    # w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
    
    v <- y[,2:numResi]
    # v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
    
    df_subs_ORF7a$numSubs[selected_positions] <- 
      w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")|(y[,2:numResi]=="*")))]
    
    df_subs_ORF7a$subsAA[selected_positions] <- 
      v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")|(y[,2:numResi]=="*")))]
  
  MSA_ORF7a <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
  # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
  }
#ORF7b
{PROTEIN <- "ORF7b"
  align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")),
                                      format = "fasta", forceToLower = FALSE) 
  names(align_MSA$seq) <- align_MSA$nam
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  
  y <- t(x$AA[,1:ORF7b_LENGTH])
  z <- t(x$percentage[,1:ORF7b_LENGTH])
  
  z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)
  
  if (dim(y)[1]==1) {
    
    df_subs_ORF7b <- data.frame(position = seq.int(from = 1, to = sum(y[1,] %in% AA_STANDARD)),
                                consAA = y[,1], 
                               numSubs = 0,
                               subsAA = "", stringsAsFactors = FALSE)
  }
  
  
  if (dim(y)[1]>1){
  df_subs_ORF7b <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                              consAA = y[,1], 
                              numSubs = 0,
                              subsAA = "", stringsAsFactors = FALSE)
  
  selected_positions <- vector()
  
  for (i in seq.int(2, dim(y)[2])) {
    selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
  }
  selected_positions <- unique(selected_positions)
  # selected_positions <- which(y[,2] %in% AA_STANDARD)
  
  numResi <- dim(y)[2]
  
  w <- z[,2:numResi]
  # w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  v <- y[,2:numResi]
  # v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_ORF7b$numSubs[selected_positions] <- 
    w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")|(y[,2:numResi]=="*")))]
  
  df_subs_ORF7b$subsAA[selected_positions] <- 
    v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")|(y[,2:numResi]=="*")))]
  }
  
  MSA_ORF7b <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
    # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
    }
#ORF8
{PROTEIN <- "ORF8"
  align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")),
                                      format = "fasta", forceToLower = FALSE) 
  names(align_MSA$seq) <- align_MSA$nam
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  
  y <- t(x$AA[,1:ORF8_LENGTH])
  z <- t(x$percentage[,1:ORF8_LENGTH])
  
  z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)
  
  df_subs_ORF8 <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                             consAA = y[,1], 
                              numSubs = 0,
                              subsAA = "", stringsAsFactors = FALSE)
  
  selected_positions <- vector()
  
  for (i in seq.int(2, dim(y)[2])) {
    selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
  }
  selected_positions <- unique(selected_positions)
  # selected_positions <- which(y[,2] %in% AA_STANDARD)
  
  numResi <- dim(y)[2]
  
  w <- z[,2:numResi]
  # w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  v <- y[,2:numResi]
  # v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_ORF8$numSubs[selected_positions] <- 
    w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")|(y[,2:numResi]=="*")))]
  
  df_subs_ORF8$subsAA[selected_positions] <- 
    v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")|(y[,2:numResi]=="*")))]
  
  MSA_ORF8 <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
  # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
  }
#ORF10
{PROTEIN <- "ORF10"
  align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")),
                                      format = "fasta", forceToLower = FALSE) 
  names(align_MSA$seq) <- align_MSA$nam
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  
  y <- t(x$AA[,1:ORF10_LENGTH])
  z <- t(x$percentage[,1:ORF10_LENGTH])
  
  z <- round(matrix(data = as.numeric(z), ncol = dim(x$AA)[1]) * NUM_GENOMES/100)
  
  df_subs_ORF10 <- data.frame(position = seq.int(from = 1, to = sum(y[,1] %in% AA_STANDARD)),
                              consAA = y[,1], 
                             numSubs = 0,
                             subsAA = "", stringsAsFactors = FALSE)
  
  selected_positions <- vector()
  
  for (i in seq.int(2, dim(y)[2])) {
    selected_positions <- c(selected_positions, which(y[,i] %in% AA_STANDARD))
  }
  selected_positions <- unique(selected_positions)
  # selected_positions <- which(y[,2] %in% AA_STANDARD)
  
  numResi <- dim(y)[2]
  
  w <- z[,2:numResi]
  # w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  v <- y[,2:numResi]
  # v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")))]
  
  df_subs_ORF10$numSubs[selected_positions] <- 
    w[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")|(y[,2:numResi]=="*")))]
  
  df_subs_ORF10$subsAA[selected_positions] <- 
    v[which(!((y[,2:numResi]=="n")|(y[,2:numResi]=="X")|(y[,2:numResi]=="*")))]
  
  MSA_ORF10 <- Biostrings::AAMultipleAlignment(x = align_MSA$seq[align_MSA$nam == "EPI_ISL_402125"], use.names = TRUE)
  # Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
  }


# Reference for all proteins

REF_ID_MSA <- which(rownames(as.matrix(MSA_S)) == "EPI_ISL_402125")

# Popualtion coverage dfs #####


# Temporal plot related #####

df_tmp <- metadf %>% filter(!is.na(day)) %>% arrange(lubridate::as_date(date)) %>% mutate(index = row_number())

regionNOT1 <- df_tmp %>% filter(!(Country %in% c("Asia", "Europe", "Central America", "South America", "North America",
                                   "Africa"))) %>% group_by(Country) %>% summarise(count = n()) %>% filter(count > 1) %>% pull(Country) %>% as.character()

region1 <- df_tmp %>% filter(!(Country %in% c("Asia", "Europe", "Central America", "South America", "North America",
                                   "Africa"))) %>% group_by(Country) %>% summarise(count = n()) %>% filter(count == 1) %>% pull(Country) %>% as.character()

ls_region <- as.list(c(regionNOT1, "Others"))

names(ls_region) <- c(regionNOT1, "Others")

# ls_region$All <- c(regionNOT1, region1)


