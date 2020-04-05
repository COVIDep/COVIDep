find.single.coverages <- function(df, REGION_chr) {


  setwd(here("Utils", "population_coverage"))
  use_condaenv(condaenv = "py27", required = TRUE)
  py_config()
  system("python configure.py")

  print(REGION_chr)
  
  REGION <- glue::double_quote(REGION_chr)
  
  df <- df %>% 
    arrange(desc(nCoV))
  # %>% 
  #   distinct_at(vars(`MHC Allele Names`), .keep_all = TRUE) 
  
  df$`MHC Allele Names` %>% str_replace_all(pattern = "/", replacement = ",HLA-") -> df$`MHC Allele Names`
  
  df$Scov = vector(length = length(df$Epitope), mode = "numeric")
  
  df$Contry <- REGION_chr
  
  for (i in seq.int(1, length(df$Epitope))) {
    
    write_tsv(x = df[i,] %>% select(Epitope, `MHC Allele Names`), 
              col_names = FALSE, 
              path = "HLA_I_II")
    z <- system(glue::glue("python calculate_population_coverage.py -p ", REGION, " -c combined -f ./HLA_I_II"),
                intern = TRUE)
    x <- z[3] %>% stringr::str_extract_all(boundary("word"), 
                                           simplify = TRUE)
    df$Scov[i] <- as.double(x[REGION %>% stringr::str_count(boundary("word"))+1])
  }
  
  df
}