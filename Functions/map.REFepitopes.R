map.REFepitopes <- function(Tepitopes, REF, NUM_MAX_MISMATCH, ...) {

  library(seqinr)
  library(tidyverse)
  library(here)
  library(Biostrings)
  
  {
    temp <- toupper(as.character(Tepitopes))
    teiptope_dict <- AAStringSet(temp)
    
    y <- data.frame(start=rep(0,length(teiptope_dict)), end=rep(0,length(teiptope_dict)))
    
    temp <- Biostrings::matchPDict(pdict = teiptope_dict, 
                       subject = AAString(REF), 
                       max.mismatch = NUM_MAX_MISMATCH)
    
    y$start <- as.double(IRanges::start(temp))
    y$end <- as.double(IRanges::end(temp))
    
    y$length <- y$end - y$start + 1
    
    tep_ind = vector()
    for (i in 1:length(y$length)) {
      if (!is.na(y[i,1])){
        tep_ind = union(tep_ind, seq.int(from = y$start[i], to = y$end[i]))}
    }

    df <- data.frame(Epitope = teiptope_dict %>% as.character(),
                     start = y$start,
                     end = y$end,
                     length = y$length, stringsAsFactors = FALSE)
    
  }
  
  df 
  
}