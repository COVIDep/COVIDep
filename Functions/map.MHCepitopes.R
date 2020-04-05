map.MHCepitopes <- function(VIRUS, PROTEIN, NUM_MAX_MISMATCH, GAPratio, ...) {

  library(seqinr)
  library(tidyverse)
  library(here)
  library(readxl)
  library(Biostrings)
  
  # align_MSA <- read.alignment(file = here("Data", paste0(VIRUS, "_", PROTEIN, ".fasta")), 
  #                             format = "fasta", forceToLower = FALSE) 
  align_MSA <- read.alignment(file = here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
                              format = "fasta", forceToLower = FALSE) 
  align_MSA$seq <- toupper(align_MSA$seq)
  
  viprCons <- consensus(matali = align_MSA, method = "majority")
  
  x <- BALCONY::calculate_AA_variation(alignment = align_MSA)
  
  # T epitopes 
  {
    data_file <- here("Data", paste0("MHCepitopes-", VIRUS, ".fasta"))
    Tepitopes <- read.fasta(file = data_file, seqtype = "AA", as.string = "TRUE")
    temp <- toupper(as.character(Tepitopes))
    teiptope_dict <- AAStringSet(temp)
    
    y <- data.frame(start=rep(0,length(teiptope_dict)), end=rep(0,length(teiptope_dict)))
      temp <- matchPDict(pdict = teiptope_dict, subject = AAString(c2s(viprCons)), max.mismatch = NUM_MAX_MISMATCH)
      y$start <- as.double(IRanges::start(temp))
      y$end <- as.double(IRanges::end(temp))
    
    y$length <- y$end - y$start + 1
    
    tep_ind = vector()
    for (i in 1:length(y$length)) {
      if (!is.na(y[i,1])){
        tep_ind = union(tep_ind, seq.int(from = y$start[i], to = y$end[i]))}
    }

    tep_avg_cons = rep(NA, length(y$start))
    tep_min_cons = rep(NA, length(y$start))
    tep_med_cons = rep(NA, length(y$start))
    
    for (i in 1:length(y$start)) {
      if (!is.na(y[i,1])) {
        tep_avg_cons[i] <- mean(as.double(x$percentage[1, y[i,1]:y[i,2]]), na.rm = TRUE)
        tep_min_cons[i] <- min(as.double(x$percentage[1, y[i,1]:y[i,2]]), na.rm = TRUE)
        tep_med_cons[i] <- median(as.double(x$percentage[1, y[i,1]:y[i,2]]), na.rm = TRUE)
      }
    }
    
    temp1 <- vcountPDict(pdict = (teiptope_dict), 
                          subject = AAStringSet(align_MSA$seq), 
                          max.mismatch = NUM_MAX_MISMATCH)
    
    temp1 <- rowSums(temp1)
    
    ExEpi_df <- data.frame(Virus = VIRUS,
                           Protein = PROTEIN,
                           IEDB = names(Tepitopes),
                           Epitope = teiptope_dict %>% as.character(),
                           Id = temp1/align_MSA$nb, 
                           AverageEpCons = tep_avg_cons,
                           MinEpCons = tep_min_cons,
                           start = y$start,
                           end = y$end,
                           length = y$length, stringsAsFactors = FALSE)
    
  }
  
  ExEpi_df 
  
}