prepare.proteinMSA <- function(DNA_Mat, PROTEIN, START, END, CODON_AMBIGUOUS, ...) {

  AA_list <- list()
  AA_Mat <- matrix(nrow = dim(DNA_Mat)[1], ncol = (END-START+1)/3)
  
  for (s in seq.int(1, dim(DNA_Mat)[1])) {
    DNA_Mat[s,seq.int(START, END)] -> nuc_seq
    if (CODON_AMBIGUOUS) {
      aa_seq <- ape::trans(x = ape::as.DNAbin(nuc_seq), code = 1, codonstart = 1) %>% as.character()
    }
    if (!CODON_AMBIGUOUS) {
      aa_seq <- seqinr::translate(seq = nuc_seq, NAstring = "X", ambiguous = FALSE)
    }
    # AA_list[[s]] <- seqinr::c2s(aa_seq)
    AA_Mat[s,] <- aa_seq
  }
  
  AACons <- consensus(matali = AA_Mat, method = "majority")
  
  AA_Mat <- AA_Mat[, which(AACons != "-")]
  AACons <- AACons[which(AACons != "-")]
  
  AA_Mat <- AA_Mat[, which(AACons != "x")]
  AACons <- AACons[which(AACons != "x")]
  
  numSeq <- dim(AA_Mat)[1]
  numPos <- dim(AA_Mat)[2]
  
  AA_list <- list()
  for (j in seq.int(1,dim(AA_Mat)[1])) {
    AA_list[[j]] <- seqinr::c2s(AA_Mat[j,])
  }
  
  seqinr::write.fasta(sequences = AA_list, 
                      names = as.list(rownames(DNA_Mat)), 
                      as.string = TRUE, 
                      file.out = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")))
  
}