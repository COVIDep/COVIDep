find.immunogen <- function(df, REGION_chr) {


  setwd(here("Utils", "population_coverage"))
  use_condaenv(condaenv = "py27", required = TRUE)
  py_config()
  system("python configure.py")

  print(REGION_chr)
  
  REGION <- glue::double_quote(REGION_chr)
  
  df <- df %>% 
    arrange(desc(meanID)) %>% 
    distinct_at(vars(`MHC Allele Names`), .keep_all = TRUE) 
  
  df$`MHC Allele Names` %>% str_replace_all(pattern = "/", replacement = ",HLA-") -> df$`MHC Allele Names`
  
  single_cov = vector()
  
  for (i in seq.int(1, length(df$Epitope))) {
    
    write_tsv(x = df[i,] %>% select(Epitope, `MHC Allele Names`), 
              col_names = FALSE, 
              path = "HLA_I_II")
    
    z <- system(glue::glue("python calculate_population_coverage.py -p ", REGION, " -c combined -f ./HLA_I_II"),
                intern = TRUE)
    # z <- system(glue::glue("python calculate_population_coverage.py -p \"Brazil\" -c combined -f ./HLA_I_II"), 
    #             intern = TRUE)
    
    x <- z[3] %>% stringr::str_extract_all(boundary("word"), 
                                           simplify = TRUE)
    
    single_cov[i] <- as.double(x[REGION %>% stringr::str_count(boundary("word"))+1])
  }
  
  scov_df <- data.frame(Epitope = 
                          df$Epitope[which(!is.na(single_cov))], 
                        `MHC Allele Names` = 
                          df$`MHC Allele Names`[which(!is.na(single_cov))], stringsAsFactors = FALSE)
  
  scov_values <- single_cov[which(!is.na(single_cov))]
  
  scov_df <- bind_rows(scov_df[which(scov_values == max(scov_values)),],
                       scov_df[-which(scov_values == max(scov_values)),])
  
  best_covs <- vector(mode = "double")
  best_covs[1] <- max(scov_values)
  best_eps <- scov_df[1,]
  
  best_cov <- best_covs[1]
  best_ep <- best_eps[1,]
  
  selected <- vector()
  
  for (i in seq.int(1, length(scov_df$Epitope))) {
    print(paste0("i = ", i))
    for (j in seq.int(i+1, length(scov_df$Epitope))) {
      print(paste0("j = ", j))
      if(!(j %in% selected)){
        temp_ep <- bind_rows(best_eps, scov_df[j,]) 
        write_tsv(x = temp_ep, 
                  col_names = FALSE, path = "HLA_I_II")
        
        z <- system(glue::glue("python calculate_population_coverage.py -p ", REGION, " -c combined -f ./HLA_I_II"),
                    intern = TRUE)
        
        x <- z[3] %>% stringr::str_extract_all(boundary("word"), simplify = TRUE) 
        
        
        if(as.double(x[REGION %>% str_count(boundary("word"))+1]) > best_cov){
          best_cov <- as.double(x[REGION %>% stringr::str_count(boundary("word"))+1])
          best_ep <- temp_ep
          best_j <- j}
      }
    }
    
    best_covs <- c(best_covs, best_cov)
    best_eps <- best_ep
    selected <- c(selected, best_j)
    
  }
  
  
  immunogen <- data.frame(Epitope = best_eps$Epitope,
                          MHCs = best_eps$MHC.Allele.Names,
                          AccCov = best_covs[1:length(best_eps$Epitope)], 
                          stringsAsFactors = FALSE)
  
  Scov = vector()
  for (i in seq.int(1, length(immunogen$Epitope))) {
    
    write_tsv(x = immunogen[i,] %>% select(Epitope, MHCs), 
              col_names = FALSE, 
              path = "HLA_I_II")
    z <- system(glue::glue("python calculate_population_coverage.py -p ", REGION, " -c combined -f ./HLA_I_II"),
                intern = TRUE)
    x <- z[3] %>% stringr::str_extract_all(boundary("word"), 
                                           simplify = TRUE)
    Scov[i] <- as.double(x[REGION %>% stringr::str_count(boundary("word"))+1])
  }
  
  immunogen$SCov <- Scov
  
  immunogen <- left_join(x = immunogen, y = df %>% 
                           select(`MHC Allele Classes`, IEDB, `MHC Allele Names`,
                                  Epitope, meanID, Protein, Epitope), by = c("Epitope"))
  
  immunogen$order <- seq_along(immunogen$Epitope)
  
  write_csv(x = immunogen, path = here("Data", "pop_coverage.csv"), col_names = TRUE)

  write_tsv(x = immunogen %>% select(Epitope, MHCs),
            col_names = FALSE,
            path = here("Utils", "population_coverage", paste0("HLA_I_II_", REGION_chr)))

  write_tsv(x = immunogen %>% filter(`MHC Allele Classes` == "I") %>% select(Epitope, MHCs),
            col_names = FALSE,
            path = here("Utils", "population_coverage", paste0("HLA_I_", REGION_chr)))

  write_tsv(x = immunogen %>% filter(`MHC Allele Classes` == "II") %>% select(Epitope, MHCs),
            col_names = FALSE,
            path = here("Utils", "population_coverage", paste0("HLA_II_", REGION_chr)))
  
  immunogen$country <- REGION
  
  immunogen
  
}