
ep.map.vis <- function(df_ep_map) {
  
  df_ep_map <- df_ep_map %>% mutate(refStart = Start) %>%
    mutate(refEnd = End) %>%
    arrange(refStart, desc(refEnd)) %>% distinct_at(.vars = vars(IEDB), .keep_all = TRUE)
  
  WAIT = TRUE
  clear_i <- 1
  pre_clear_i <- 1
  
  if(dim(df_ep_map)[1]>0){
  df_ep_map$offset <- 0
  df_ep_map$y_pos <- 0
  df_ep_map$y_pos[1] <- 1
  }

  if(dim(df_ep_map)[1]==0){
    df_ep_map$offset <- double()
    df_ep_map$y_pos <- double()
  }
  
  if(dim(df_ep_map)[1]>1){
  for (i in 2:dim(df_ep_map)[1]) {
    
    if(clear_i > 1 & clear_i != pre_clear_i){
      offoffset <- 1
      for (k in seq.int(1, clear_i-1)) {
        if(df_ep_map$refStart[i] <= df_ep_map$refEnd[k]){
          ifelse(df_ep_map$offset[i] == 0,
                 yes = {df_ep_map$offset[i] <- offoffset
                 offoffset <- offoffset + 1},
                 no = {df_ep_map$offset[i] <- df_ep_map$offset[i] + 1})
        }
      }
      pre_clear_i <- clear_i
    }
    for (j in seq.int(clear_i, i-1)) {
      
      if((df_ep_map$refStart[i] <= df_ep_map$refEnd[j]) & WAIT){
        df_ep_map$y_pos[i] <- df_ep_map$y_pos[j]+1+df_ep_map$offset[i]
      }
      if(df_ep_map$refStart[i] > df_ep_map$refEnd[j]){
        clear_i <- i
        df_ep_map$y_pos[i] <- 1
        WAIT <- FALSE
      }
    }
    WAIT <- TRUE
  }
}
  df_ep_map
  
  }

