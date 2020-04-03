

# Server #####

server <- function(input, output) {
  
  # Global changes to inputs ####
  
  rdfs <- reactiveValues(
    ID_THRESH = ID_THRESH,
    df_Bepitopes = df_Bepitopes_REF %>% filter(nCoV >= ID_THRESH),
    df_TMHCepitopes = df_TMHCepitopes_REF %>% filter(nCoV >= ID_THRESH),
    df_single_coverages = df_single_coverages_REF %>% filter(nCoV >= ID_THRESH)
    # ,
    # REGION_temporal = ls_region["All"] %>% unlist() 
  )
  
  
  # observeEvent(input$IDthreshold, {
  #   rdfs$df_Bepitopes <- df_Bepitopes_REF %>% filter(nCoV >= {case_when(input$IDthreshold == 1 ~ 0.99, input$IDthreshold == 2 ~ 0.95) })
  #   rdfs$df_TMHCepitopes <- df_TMHCepitopes_REF %>% filter(nCoV >= {case_when(input$IDthreshold == 1 ~ 0.99, input$IDthreshold == 2 ~ 0.95) })
  #   rdfs$ID_THRESH <- {case_when(input$IDthreshold == 1 ~ 0.99, input$IDthreshold == 2 ~ 0.95) }
  #   rdfs$df_single_coverages  <- df_single_coverages_REF %>% filter(nCoV >= {case_when(input$IDthreshold == 1 ~ 0.99, input$IDthreshold == 2 ~ 0.95) })
  # })
  
  observeEvent(input$IDthreshold, {
    rdfs$df_Bepitopes <- df_Bepitopes_REF %>% filter(nCoV >= as.numeric(input$IDthreshold))
    rdfs$df_TMHCepitopes <- df_TMHCepitopes_REF %>% filter(nCoV >= as.numeric(input$IDthreshold))
    rdfs$ID_THRESH <- as.numeric(input$IDthreshold)
    rdfs$df_single_coverages  <- df_single_coverages_REF %>% filter(nCoV >= as.numeric(input$IDthreshold))
    # rdfs$REGION_temporal <- ls_region[input$TemporalRegion] %>% unlist()
  })

  
  # Home page plots ####
  
  # Country bar plot
  output$p_country <- renderPlotly({
    df <- metadf %>% filter(!is.na(day)) %>% arrange(lubridate::as_date(date)) %>% mutate(index = row_number())
    
    colourCount = length(unique(df$Country))
    getPalette = colorRampPalette(rev(brewer.pal(9, "Set1")))
    
    colorValues <- getPalette(colourCount)
    
    x_odd <- colorValues[seq_along(colorValues) %%2 != 0]
    x_even <- colorValues[seq_along(colorValues) %%2 == 0]
    x_even <- rev(x_even)
    
    colorValues <- c(rbind(x_odd, x_even))
    
    order <- df %>% group_by(Country) %>% summarize(n=n()) %>% arrange(n) %>% pull(Country)
    
    df2 <- df %>% group_by(Country) %>% summarize(Count=n()) %>% arrange(Count)
    df2 <- df2 %>% mutate(text = paste0(Country, " : ", Count))
    
    # g2 <- ggplot(data = df, mapping = aes(x = factor(Country, levels = order)))
    g2 <- ggplot(data = df2, mapping = aes(x = factor(Country, levels = order), y = Count))
    
    # g2 <- g2 + geom_bar(color = "white", , fill = brewer.pal(n = 3, name = "Set1")[2], size = 0.8, alpha = 0.85)
    # g2 <- g2 + geom_bar(color = "white", fill = getPalette(colourCount), size = 0.8, alpha = 0.85)
    g2 <- g2 + geom_col(color = "white", fill = colorValues, size = 0.8, alpha = 0.85)
    
    g2 <- g2 + scale_y_continuous(trans = "sqrt")
    
    g2 <- g2 + theme_minimal() + theme(legend.position = "none", axis.text.x = element_text(size = 12), panel.grid.major.y = element_blank())
    
    g2 <- g2 + xlab("") + ylab("Number of sequences")
    
    g2 <- g2 + coord_flip()
    
    plotly::ggplotly(g2, tooltip = 'Count') %>% 
      layout(hoverlabel = list(bgcolor = 'white', font = list(size = 16, color = 'black'))) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
    
    
  })
  
  # Date all histogram ####
  
  output$p_hist <- renderPlotly({
    
    df <- metadf %>% filter(!is.na(day)) %>% arrange(lubridate::as_date(date)) %>% mutate(index = row_number())
    
    order <- df %>% group_by(Country) %>% summarize(n=n()) %>% arrange(n) %>% pull(Country)
    
    df <- df %>% mutate(Country = factor(Country, levels = order))
    
    df <- df %>% mutate(`Cumulative number` = index)
    
    colourCount = length(unique(df$Country))
    getPalette = colorRampPalette(rev(brewer.pal(9, "Set1")))
    
    colorValues <- getPalette(colourCount)
    
    x_odd <- colorValues[seq_along(colorValues) %%2 != 0]
    x_even <- colorValues[seq_along(colorValues) %%2 == 0]
    x_even <- rev(x_even)
    
    colorValues <- c(rbind(x_odd, x_even))
    
    df <- df %>% mutate(i = 1)
    
    df <- df %>% mutate(text = paste0(Country))
    
    df <- df %>% filter(!(Country %in% c("Asia", "Europe", "Central America", "South America", "North America",
                                         "Africa")))
    
    df %>% mutate(Country = if_else(condition = (df$Country %in% region1),
                                    true = "Others",
                                    false = as.character(df$Country))) -> df

    df <- df %>% mutate(Country_fc = factor(Country, levels = c("Others", order)))
    
    plot_ly(df, 
            x = ~date, type = 'histogram', alpha = 0.75, text = ~text,
            color = ~Country_fc, bingroup = ~Country_fc, nbinsx = 45,
            colors = rev(colorValues),
            marker =list(line = list(color = "white")),
            # hoverinfo = 'none'
            #,
            hoverlabel = list(bgcolor = 'white', font = list(size = 16, color = 'black')),
            hovertemplate = if_else(condition = FALSE,
                                    true = {paste('<br>%{y}<br>', '%{x}<extra></extra>')},
                                    false = {paste('%{text}', '<br>%{y}<br>', '%{x}<extra></extra>')})
    ) %>% 
      layout(yaxis = list(title = 'Number of sequences'),
             xaxis = list(title = 'Collection date')) %>% 
      layout(
        legend = list(itemclick = FALSE, traceorder = 'reversed'),
        barmode="relative",
        bargap=0.1) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange = TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE)) %>%
      layout(showlegend = TRUE)
    
    
  })
  
  # Date individual histograms 
  
  output$p_date <- renderPlotly({
    df <- metadf %>% filter(!is.na(day)) %>% arrange(lubridate::as_date(date)) %>% mutate(index = row_number())
    
    order <- df %>% group_by(Country) %>% summarize(n=n()) %>% arrange(n) %>% pull(Country)
    
    df <- df %>% mutate(Country = factor(Country, levels = order))
    
    df <- df %>% mutate(`Cumulative number` = index)
    
    colourCount = length(unique(df$Country))
    getPalette = colorRampPalette(rev(brewer.pal(9, "Set1")))
    
    colorValues <- getPalette(colourCount)
    
    x_odd <- colorValues[seq_along(colorValues) %%2 != 0]
    x_even <- colorValues[seq_along(colorValues) %%2 == 0]
    x_even <- rev(x_even)
    
    colorValues <- c(rbind(x_odd, x_even))
    
    df <- df %>% mutate(i = 1)
    
    # BACKUP 
    
    # g1 <- ggplot(data = df, mapping = aes(x = date, color = Country, fill = Country))
    # 
    # g1 <- g1 + geom_histogram(binwidth = 14, position = "stack")
    # 
    # g1 <- g1 + scale_x_date(breaks = "14 days") 
    # 
    # g1 <- g1 + scale_color_manual(values = getPalette(colourCount), breaks = unique(df$Country))
    # 
    # g1 <- g1 + scale_fill_manual(values = getPalette(colourCount), breaks = unique(df$Country))
    # 
    # # g1 <- g1 + scale_y_continuous(trans = "log10") # breaks = seq.int(0, max(df$index), by = 200))
    # 
    # g1 <- g1 + theme_minimal() + theme(panel.grid.minor.y = element_blank(), axis.ticks.x = element_line(), 
    #                                    axis.text.x = element_text(size = 9, angle = 90, vjust= 0.5), 
    #                                    legend.position = "right", legend.title = element_blank())
    
    # plotly::ggplotly(g1, tooltip = "text") %>% config(displayModeBar = F) %>% 
    #   layout(legend = list(orientation = 'v', traceorder = 'reversed', 
    #                        itemclick = 'toggleothers'#, xanchor = 'left',
    #                        # yanchor = 'bottom'
    #   )) %>% 
    #   layout(xaxis=list(fixedrange = TRUE)) %>% 
    #   layout(yaxis=list(fixedrange=TRUE)) %>% 
    #   layout(legend=list(title=list(text='')))
    
    df <- df %>% mutate(text = paste0(Country))
    
    df <- df %>% filter(!(Country %in% c("Asia", "Europe", "Central America", "South America", "North America",
                                         "Africa")))
    
    df %>% mutate(Country = if_else(condition = (df$Country %in% region1),
                                    true = "Others",
                                    false = as.character(df$Country))) -> df
    
    df <- df %>% mutate(Country_fc = factor(Country, levels = c("Others", order)))
    
    plot_ly(df %>% filter(Country %in% ls_region[[input$TemporalRegion]]), 
                     x = ~date, type = 'histogram', alpha = 0.75, text = ~text,
                     color = ~Country_fc, bingroup = ~Country_fc, nbinsx = 45,
                     colors = rev(colorValues),
                     marker =list(line = list(color = "white")),
                     hoverinfo = 'none'
            # ,
            #          hoverlabel = list(bgcolor = 'white', font = list(size = 16, color = 'black')),
            #          hovertemplate = if_else(condition = TRUE, true = {paste('<br>%{y}<br>', '%{x}<extra></extra>')}, false = {paste('%{text}', '<br>%{y}<br>', '%{x}<extra></extra>')})
             ) %>% 
               layout(yaxis = list(title = 'Number of sequences'),
                      xaxis = list(title = 'Collection date')) %>% 
               layout(
                 legend = list(itemclick = 'toggleothers', traceorder = 'reversed'),
                 # barmode="stack",
                 bargap=0.1) %>% 
               config(displayModeBar = F) %>% 
               layout(xaxis=list(fixedrange = TRUE)) %>% 
               layout(yaxis=list(fixedrange=TRUE))
         
    
    # BACKUP
    
    # g1 <- ggplot(data = df, mapping = aes(x = date, y = index))
    # 
    # g1 <- g1 + geom_line(linetype = "dotted")
    # 
    # g1 <- g1 + geom_jitter(aes(group = seq_along(date), color = Country
    #                            # , 
    #                            # text = paste0("Country: ", Country, "\nCollection date: ", `Collection date`, "\nGISAID Accession ID: ", `Accession ID`)
    #                            # text = paste0("Country: ", Country, "\nCollection date: ", `Collection date`, "\nOriginating lab: ", `Originating lab`)
    #                            ), 
    #                        alpha = 0.75, size = 3.5)
    # 
    # g1 <- g1 + scale_x_date(minor_breaks = "1 day", limits = c(min(df$date), max(df$date)), 
    #                         breaks = seq.Date(min(df$date), max(df$date), by = 5), 
    #                         labels = as.character(seq.Date(min(df$date), max(df$date), by = 5))) 
    # 
    # g1 <- g1 + scale_color_manual(values = getPalette(colourCount), breaks = unique(df$Country))
    # 
    # g1 <- g1 + scale_y_continuous(trans = "log10") # breaks = seq.int(0, max(df$index), by = 200))
    # 
    # g1 <- g1 + theme_minimal() + theme(panel.grid.minor.y = element_blank(), axis.ticks.x = element_line(), 
    #                                    axis.text.x = element_text(size = 9, angle = 90, vjust= 0.5), 
    #                                    legend.position = "right", legend.title = element_blank())
    # 
    # plotly::ggplotly(g1, tooltip = "text") %>% config(displayModeBar = F) %>% 
    #   layout(legend = list(orientation = 'v', traceorder = 'reversed', 
    #                        itemclick = 'toggleothers'#, xanchor = 'left',
    #                        # yanchor = 'bottom'
    #   )) %>% 
    #   layout(xaxis=list(fixedrange = TRUE)) %>% 
    #   layout(yaxis=list(fixedrange=TRUE)) %>% 
    #   layout(legend=list(title=list(text='')))
    # 
    
    # plotly::ggplotly(g1, tooltip = "text", dynamicTicks = T) %>% config(displayModeBar = F) %>% 
    #   layout(legend = list(orientation = 'v', traceorder = 'reversed', 
    #                        itemclick = 'toggleothers'#, xanchor = 'left',
    #                        # yanchor = 'bottom'
    #   )) %>% 
    #   layout(xaxis=list(fixedrange = TRUE, 
    #                     rangeslider = list(type = "date", yaxis = list(ranngemode = "match")),
    #                     rangeselector = list(
    #                       buttons = list(
    #                         list(
    #                           count = 7,
    #                           label = "1 week",
    #                           step = "day",
    #                           stepmode = "backward"),
    #                         list(
    #                           count = 14,
    #                           label = "2 weeks",
    #                           step = "day",
    #                           stepmode = "backward"),
    #                         list(
    #                           count = 1,
    #                           label = "1 month",
    #                           step = "month",
    #                           stepmode = "backward"),
    #                         list(
    #                           count = 3,
    #                           label = "3 months",
    #                           step = "month",
    #                           stepmode = "todate"),
    #                         list(step = "all")
    #                       )
    #                     )
    #                     )) %>% 
    #   layout(yaxis=list(fixedrange=TRUE)) %>% 
    #   layout(legend=list(title=list(text='')))
    
    # plotly::ggplotly(g1, tooltip = c("Country", "Accession ID")) %>% config(displayModeBar = F)
    
    # p <- plot_ly(data = df, x = ~date, y = ~index, type = "scatter", mode = "markers",
                 # color = ~Country, colors = getPalette(colourCount), jitter = 0.5) %>% 
      # add_trace(y = ~index, type = "scatter", mode = "markers", ) %>% 
                # marker = list(color = ~Country, colors = getPalette(colourCount))) %>% 
      # add_trace(y = ~index, type = "scatter", mode = "line", line = list(dash = "dot")) %>% 
      # config(displayModeBar = F)
    
  })
  
  # Total number of sequences
  output$num_sequences <- renderValueBox({
    valueBox(
      subtitle = "Number of SARS-CoV-2 sequences analyzed", value = paste0(NUM_GENOMES), 
      # width = 3, 
      # icon = icon("list"), 
      color = "blue")
  })
  
  # Latest date of download of sequences
  output$latest_date <- renderValueBox({
    valueBox(value = paste0(lubridate::mday(UPDATE_DATE), " ", 
                            lubridate::month(UPDATE_DATE, label = TRUE), " ",
                            lubridate::year(UPDATE_DATE)),
             subtitle = "Date of last update", 
             # icon = icon("calendar-check"), 
             color = "blue")
  })
  
  # Identity threshold of epitopes among sequences
  output$id_threshold <- renderValueBox({
    
    valueBox(subtitle = "Minimum conservation of epitopes within SARS-CoV-2 sequences", value = rdfs$ID_THRESH, 
      # subtitle = "Minimum conservation within SARS-CoV-2\nsequences for epitope selection", value = scales::percent(rdfs$ID_THRESH), 
             # icon = icon("calendar-check"), 
             color = "blue")
  })
  
  # Distribution of epitopes in sunburst plot
  output$sunburst_epitopes <- renderPlotly({  
    
    
    b = df_epitopes %>% filter(Type == "B cell") %>% pull(IEDB) %>% length()
    c = df_epitopes %>% filter(Type == "T cell") %>% pull(IEDB) %>% length()
    
    d = df_epitopes %>% filter(Assay == "T-cell-assay") %>% pull(IEDB) %>% length()
    e = df_epitopes %>% filter(Assay == "MHC-assay") %>% pull(IEDB) %>% length()
    
    f = b-6
    g = 6
    
    h = rdfs$df_Bepitopes %>% pull(IEDB) %>% length()
    i = 0
    
    j = rdfs$df_TMHCepitopes %>% filter(Assay == "T cell") %>% pull(IEDB) %>% length()
    k = rdfs$df_TMHCepitopes %>% filter(Assay == "MHC ligand") %>% pull(IEDB) %>% length()
    
    # f = rdfs$df_Bepitopes_REF %>% pull(IEDB) %>% length()
    # g = 6
    # h = rdfs$df_TMHCepitopes_REF %>% filter(Assay == "T cell") %>% pull(IEDB) %>% length()
    # i = rdfs$df_TMHCepitopes_REF %>% filter(Assay == "MHC ligand") %>% pull(IEDB) %>% length()
    # 
    a <- b+c #+d+e+f+g+h+i+j+k
    
    # sector_colors <- c("white", 
    #                    brewer.pal(9, "YlOrRd")[2],  brewer.pal(9, "YlOrRd")[1], 
    #                    brewer.pal(9, "OrRd")[2], brewer.pal(9, "OrRd")[3], 
    #                    brewer.pal(9, "YlOrRd")[3], brewer.pal(9, "OrRd")[4], 
    #                    brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4])  
    
    # sector_colors <- c("white", 
    #                    "#9E9AC8",  "#DADAEB", 
    #                    "#FFCC99", "#FFB266", 
    #                    "#756BB1", "#FF9933",
    #                    brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4]) 
    
    sector_colors <- c("white", 
                       "#FDD0A2", "#FEE6CE",
                       "#9E9AC8", "#DADAEB",  
                        "#FDAE6B", "#756BB1", 
                       brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4]) 
    
    # sectext_colors <- c("black", "black", "black", "black", "black", "black", "black", 
    #                     brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4])
    
    sectext_colors <- c("black", 
                        "black", "black", 
                        "black", "black", 
                        "white", "white", 
                        brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4], brewer.pal(9, "Paired")[4])
    
    sectext_size <- c(rep(16, 7), rep(22,4))
    
    plot_ly(
      labels = c(glue::glue("SARS-CoV\nepitopes"), 
                 "Linear", "Discontinuous",
                 "Positive T cell", "Positive MHC binding",
                 "B cell", "T cell",
                 # "T cell", "B cell",
                 glue::glue(" "),
                 glue::glue("  "),
                 glue::glue("   "),
                 glue::glue("    ")
                 ),
      parents = c("",  
                  "B cell", "B cell",
                  "T cell", "T cell", 
                  glue::glue("SARS-CoV\nepitopes"), glue::glue("SARS-CoV\nepitopes"),
                  "Linear", "Discontinuous",
                  "Positive T cell", "Positive MHC binding"
      ),
      values = c(
        a,
        # {b+6+c+d+e+
        #   rdfs$df_Bepitopes %>% pull(IEDB) %>% length()+
        #   rdfs$df_TMHCepitopes %>% filter(Assay == "T cell") %>% pull(IEDB) %>% length()+
        #   rdfs$df_TMHCepitopes %>% filter(Assay == "MHC ligand") %>% pull(IEDB) %>% length()},
                 f, g,
                 d, e, 
                 b, c,
                 # c, b,
                 h, i,
                 j, k),
      type = 'sunburst',
      branchvalues = 'total',
      # maxdepth = 4,
      marker = list(colors = sector_colors),
      hoverinfo = "text",
      hovertext = c("", 
                    f, g,
                    d, e, 
                    b, c,
                    # c, b,
                    glue::glue("Identified epitopes: ", h), 
                    "",
                    glue::glue("Identified epitopes: ", j), 
                    glue::glue("Identified epitopes: ", k)
                    ),
      # domain = list(y = 0.95),
      hoverlabel = list(bgcolor = 'white', font = list(size = 16, color = 'black')),
      level = glue::glue("SARS-CoV\nepitopes"),
      # insidetextorientation = "tangential",
      insidetextfont = list(size = sectext_size, color = sectext_colors),
      outsidetextfont = list(size = 16)
    ) %>% config(displayModeBar = F) %>% 
      layout(margin = list(l = 0,
                           r = 0,
                           b = 0,
                           t = 0,
                           pad = 0))
  })
  
  # Number of B epitopes
  output$num_B_epitopes <- renderValueBox({
    
    valueBox(value = rdfs$df_Bepitopes %>% 
               pull(IEDB) %>% length(), 
             subtitle = strong("B cell epitopes identified as potential vaccine targets for SARS-CoV-2"),
             color = "green")
  })
  
  # Number of T.MHC epitopes
  output$num_TMHC_epitopes <- renderValueBox({
    
    valueBox(value = rdfs$df_TMHCepitopes %>% 
               filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                         true = "T cell", false = c("T cell", "MHC ligand"))) %>% 
               pull(Assay) %>% length(), 
             subtitle = strong("T cell epitopes identified as potential vaccine targets for SARS-CoV-2"),
             color = "green")
  })
  
  # B epitopes tables ######    
  # All proteins B cell epitopes
  output$DT_ALLBcell <- DT::renderDataTable(server = FALSE, {
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Protein, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none',
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE, 
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ALLBcell'))
                                 ), 
                  extensions = c('FixedHeader', 'Buttons'))
  }) 
  
  # S protein B cell epitopes
  output$DT_SBcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "S") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'SBcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # E protein B cell epitopes
  output$DT_EBcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "E") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'EBcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # M protein B cell epitopes
  output$DT_MBcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "M") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'MBcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # N protein B cell epitopes
  output$DT_NBcell <- DT::renderDataTable(server = FALSE, {
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "N") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE, 
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'NBcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # orf1a protein B cell epitopes
  output$DT_orf1aBcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "orf1a") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'orf1aBcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # orf1b protein B cell epitopes
  output$DT_orf1bBcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>%  
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "orf1b") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'orf1bBcell'))
                                 ),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF3a protein B cell epitopes
  output$DT_ORF3aBcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "ORF3a") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF3aBcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF6 protein B cell epitopes
  output$DT_ORF6Bcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "ORF6") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF6Bcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF7a protein B cell epitopes
  output$DT_ORF7aBcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "ORF7a") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF7aBcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF7b protein B cell epitopes
  output$DT_ORF7bBcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "ORF7b") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF7bBcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF8 protein B cell epitopes
  output$DT_ORF8Bcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "ORF8") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF8Bcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF10 protein B cell epitopes
  output$DT_ORF10Bcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_Bepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_Bepitopes$IEDB, "\"target=\"_blank\">", rdfs$df_Bepitopes$IEDB, "</a>")) %>% 
                    filter(Protein == "ORF10") %>% mutate(Conservation = round(nCoV, 2)) %>% 
                    select(IEDB, Epitope, Length, Start, End, Conservation), style = 'bootstrap4',
                  editable = FALSE,
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF10Bcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # All proteins T cell epitopes
  # T epitopes tables ###### 
  output$DT_ALLTcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Protein, `MHC allele class`, `MHC allele names`, Conservation), 
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ALLTcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # S protein T cell epitopes
  output$DT_STcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>%   
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                    filter(Protein == "S") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'STcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # E protein T cell epitopes
  output$DT_ETcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>%   
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                    filter(Protein == "E") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ETcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # M protein T cell epitopes
  output$DT_MTcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>%   
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                    filter(Protein == "M") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'MTcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # N protein T cell epitopes
  output$DT_NTcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>%   
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                    filter(Protein == "N") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'NTcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # orf1a protein T cell epitopes
  output$DT_orf1aTcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>%   
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                    filter(Protein == "orf1a") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'orf1aTcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # orf1b protein T cell epitopes
  output$DT_orf1bTcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>%   
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                    filter(Protein == "orf1b") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'orf1bTcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF3a protein T cell epitopes
  output$DT_ORF3aTcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>%   
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                    filter(Protein == "ORF3a") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF3aTcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF6 protein T cell epitopes
  output$DT_ORF6Tcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>%   
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                    filter(Protein == "ORF6") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF6Tcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF7a protein T cell epitopes
  output$DT_ORF7aTcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>%   
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                    filter(Protein == "ORF7a") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>%
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF7aTcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF7b protein T cell epitopes
  output$DT_ORF7bTcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>%  
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                    filter(Protein == "ORF7b") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF7bTcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF8 protein T cell epitopes
  output$DT_ORF8Tcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>%  
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                    filter(Protein == "ORF8") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF8Tcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # ORF10 protein T cell epitopes
  output$DT_ORF10Tcell <- DT::renderDataTable(server = FALSE, {
    
    
    
    
    
    
    DT::datatable(data = rdfs$df_TMHCepitopes %>% 
                    mutate(IEDB = paste0("<a href=\"http://www.iedb.org/epitope/", rdfs$df_TMHCepitopes$IEDB, 
                                         "\"target=\"_blank\">", rdfs$df_TMHCepitopes$IEDB, "</a>")) %>% 
                    mutate(`MHC Allele Class` = 
                             str_split_fixed(string = rdfs$df_TMHCepitopes$`MHC Allele Classes`, 
                                             pattern = ",", n = 2)[,1]) %>% 
                    filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                              true = "T cell", false = c("T cell", "MHC ligand"))) %>% 
                    filter(Protein == "ORF10") %>% 
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Length, Start, End, `MHC allele class`, `MHC allele names`, Conservation),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE,
                                 scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= 'ORF10Tcell'))),
                  extensions = c('FixedHeader', 'Buttons'))
  })
  
  # MSA widgets ######
  
  output$S_MSA <- msaR::renderMsaR({
    
    #alignmentHeight = MSA_HEIGHT, 
    msaR(msa = MSA_S, menu = FALSE, conservation = FALSE, markers = TRUE, #height = MSA_HEIGHT,
         seqlogo = FALSE,overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  output$E_MSA <- msaR::renderMsaR({
    
    msaR(msa = MSA_E, menu = FALSE, conservation = FALSE, markers = TRUE,
         seqlogo = FALSE, overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  output$M_MSA <- msaR::renderMsaR({
    
    # PROTEIN <- "M"
    # align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
    #                                     format = "fasta", forceToLower = FALSE) 
    # names(align_MSA$seq) <- align_MSA$nam
    # 
    # MSA <- Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
    
    msaR(msa = MSA_M, menu = FALSE, conservation = FALSE, markers = TRUE,
         seqlogo = FALSE, overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  output$N_MSA <- msaR::renderMsaR({
    
    # PROTEIN <- "N"
    # align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
    #                                     format = "fasta", forceToLower = FALSE) 
    # names(align_MSA$seq) <- align_MSA$nam
    # 
    # MSA <- Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
    
    msaR(msa = MSA_N, menu = FALSE, conservation = FALSE, markers = TRUE,
         seqlogo = FALSE, overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  output$orf1a_MSA <- msaR::renderMsaR({
    
    # PROTEIN <- "orf1a"
    # align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
    #                                     format = "fasta", forceToLower = FALSE) 
    # names(align_MSA$seq) <- align_MSA$nam
    # 
    # MSA <- Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
    
    msaR(msa = MSA_orf1a, menu = FALSE, conservation = FALSE, markers = TRUE,
         seqlogo = FALSE, overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  output$orf1b_MSA <- msaR::renderMsaR({
    
    # PROTEIN <- "orf1b"
    # align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
    #                                     format = "fasta", forceToLower = FALSE) 
    # names(align_MSA$seq) <- align_MSA$nam
    # 
    # MSA <- Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
    
    msaR(msa = MSA_orf1b, menu = FALSE, conservation = FALSE, markers = TRUE,
         seqlogo = FALSE, overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  output$ORF3a_MSA <- msaR::renderMsaR({
    
    # PROTEIN <- "ORF3a"
    # align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
    #                                     format = "fasta", forceToLower = FALSE) 
    # names(align_MSA$seq) <- align_MSA$nam
    # 
    # MSA <- Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
    
    msaR(msa = MSA_ORF3a, menu = FALSE, conservation = FALSE, markers = TRUE,
         seqlogo = FALSE, overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  output$ORF6_MSA <- msaR::renderMsaR({
    
    # PROTEIN <- "ORF6"
    # align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
    #                                     format = "fasta", forceToLower = FALSE) 
    # names(align_MSA$seq) <- align_MSA$nam
    # 
    # MSA <- Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
    
    msaR(msa = MSA_ORF6, menu = FALSE, conservation = FALSE, markers = TRUE,
         seqlogo = FALSE, overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  output$ORF7a_MSA <- msaR::renderMsaR({
    
    # PROTEIN <- "ORF7a"
    # align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
    #                                     format = "fasta", forceToLower = FALSE) 
    # names(align_MSA$seq) <- align_MSA$nam
    # 
    # MSA <- Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
    
    msaR(msa = MSA_ORF7a, menu = FALSE, conservation = FALSE, markers = TRUE,
         seqlogo = FALSE, overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  output$ORF7b_MSA <- msaR::renderMsaR({
    
    # PROTEIN <- "ORF7b"
    # align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
    #                                     format = "fasta", forceToLower = FALSE) 
    # names(align_MSA$seq) <- align_MSA$nam
    # 
    # MSA <- Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
    
    msaR(msa = MSA_ORF7b, menu = FALSE, conservation = FALSE, markers = TRUE,
         seqlogo = FALSE, overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  output$ORF8_MSA <- msaR::renderMsaR({
    
    # PROTEIN <- "ORF8"
    # align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
    #                                     format = "fasta", forceToLower = FALSE) 
    # names(align_MSA$seq) <- align_MSA$nam
    # 
    # MSA <- Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
    
    msaR(msa = MSA_ORF8, menu = FALSE, conservation = FALSE, markers = TRUE,
         seqlogo = FALSE, overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  output$ORF10_MSA <- msaR::renderMsaR({
    
    # PROTEIN <- "ORF10"
    # align_MSA <- seqinr::read.alignment(file = here::here("Data", "MSAs", paste0(PROTEIN, ".fasta")), 
    #                                     format = "fasta", forceToLower = FALSE) 
    # names(align_MSA$seq) <- align_MSA$nam
    # 
    # MSA <- Biostrings::AAMultipleAlignment(x = align_MSA$seq, use.names = TRUE)
    
    msaR(msa = MSA_ORF10, menu = FALSE, conservation = FALSE, markers = TRUE,
         seqlogo = FALSE, overviewbox = FALSE, colorscheme = "taylor", 
         labelname = FALSE, labels = FALSE, labelid = FALSE, labelNameLength = 200)
  })
  
  # Diversity widgets #####
  
  # S
  output$p_subs_S <- plotly::renderPlotly(expr = {
    
    df_subs_S <- df_subs_S %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, 
                                                    "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES)) 
    
    plot_ly(data=df_subs_S,
            # height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Consensus: %{text}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(50, S_LENGTH, 50))),
                          tickmode = "array",
                          autotick = FALSE, 
                          rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          # range = c(1,200)
                          range = c(1,S_LENGTH)
                          ),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          # dtick = 0.2*min(max(df_subs_S$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_S$numSubs/NUM_GENOMES), 1.0)),
                          tick0 = 0, 
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>%
      layout(yaxis=list(fixedrange=FALSE))
  })
  
  # E
  output$p_subs_E <- plotly::renderPlotly(expr = {
    
    df_subs_E <- df_subs_E %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES))
    
    plot_ly(data=df_subs_E,
            #height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 10,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, E_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          # rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          range = c(1,E_LENGTH)),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          # dtick = 0.2*min(max(df_subs_E$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_E$numSubs/NUM_GENOMES), 1.0)), 
                          tick0 = 0, 
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=FALSE))
  })  
  
  # M
  output$p_subs_M <- plotly::renderPlotly(expr = {
    
    df_subs_M <- df_subs_M %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES))
    
    plot_ly(data=df_subs_M,
            #height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 10,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(50, M_LENGTH, 50))),
                          tickmode = "array",
                          autotick = FALSE, 
                          # rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          range = c(1,M_LENGTH)),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          # dtick = 0.2*min(max(df_subs_M$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_M$numSubs/NUM_GENOMES), 1.0)), 
                          tick0 = 0, 
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=FALSE))
  })
  
  # N
  output$p_subs_N <- plotly::renderPlotly(expr = {
    
    df_subs_N <- df_subs_N %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES))
    
    plot_ly(data=df_subs_N,
            #height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(50, N_LENGTH, 50))),
                          tickmode = "array",
                          autotick = FALSE, 
                          rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          range = c(1,N_LENGTH)),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          # dtick = 0.2*min(max(df_subs_N$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_N$numSubs/NUM_GENOMES), 1.0)), 
                          tick0 = 0, 
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=FALSE))
  })
  
  # orf1a
  output$p_subs_orf1a <- plotly::renderPlotly(expr = {
    
    df_subs_orf1a <- df_subs_orf1a %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES))
    
    plot_ly(data=df_subs_orf1a,
            #height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(200, orf1a_LENGTH, 200))),
                          tickmode = "array",
                          autotick = FALSE, 
                          rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          range = c(1,orf1a_LENGTH)),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          # dtick = 0.2*min(max(df_subs_orf1a$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_orf1a$numSubs/NUM_GENOMES), 1.0)), 
                          tick0 = 0, 
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=FALSE))
  })
  
  # orf1b
  output$p_subs_orf1b <- plotly::renderPlotly(expr = {
    
    df_subs_orf1b <- df_subs_orf1b %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES))
    
    plot_ly(data=df_subs_orf1b,
            #height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(100, orf1b_LENGTH, 100))),
                          tickmode = "array",
                          autotick = FALSE, 
                          rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          range = c(1,orf1b_LENGTH)),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          # dtick = 0.2*min(max(df_subs_orf1b$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_orf1b$numSubs/NUM_GENOMES), 1.0)), 
                          tick0 = 0, 
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=FALSE))
  })
  
  # ORF3a
  output$p_subs_ORF3a <- plotly::renderPlotly(expr = {
    
    df_subs_ORF3a <- df_subs_ORF3a %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES))
    
    plot_ly(data=df_subs_ORF3a,
            #height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF3a_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          # rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          range = c(1,ORF3a_LENGTH)),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          # dtick = 0.2*min(max(df_subs_ORF3a$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_ORF3a$numSubs/NUM_GENOMES), 1.0)), 
                          tick0 = 0,
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=FALSE))
  })
  
  # ORF6
  output$p_subs_ORF6 <- plotly::renderPlotly(expr = {
    
    df_subs_ORF6 <- df_subs_ORF6 %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES))
    
    plot_ly(data=df_subs_ORF6,
            #height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF6_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          # rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          range = c(1,ORF6_LENGTH)),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          dtick = 0.2*min(max(df_subs_ORF6$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_ORF6$numSubs/NUM_GENOMES), 1.0)), 
                          tick0 = 0, 
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=FALSE))
  })
  
  # ORF7a
  output$p_subs_ORF7a <- plotly::renderPlotly(expr = {
    
    df_subs_ORF7a <- df_subs_ORF7a %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES))
    
    plot_ly(data=df_subs_ORF7a,
            #height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF7a_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          # rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          range = c(1,ORF7a_LENGTH)),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          dtick = 0.2*min(max(df_subs_ORF7a$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_ORF7a$numSubs/NUM_GENOMES), 1.0)), 
                          tick0 = 0, 
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=FALSE))
  })
  
  # ORF7b
  output$p_subs_ORF7b <- plotly::renderPlotly(expr = {
    
    df_subs_ORF7b <- df_subs_ORF7b %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES))
    
    plot_ly(data=df_subs_ORF7b,
            #height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF7b_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          # rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          range = c(1,ORF7b_LENGTH)),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          dtick = 0.2*min(max(df_subs_ORF7b$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_ORF7b$numSubs/NUM_GENOMES), 1.0)), 
                          tick0 = 0, 
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=FALSE))
  })
  
  # ORF8
  output$p_subs_ORF8 <- plotly::renderPlotly(expr = {
    
    df_subs_ORF8 <- df_subs_ORF8 %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES))
    
    plot_ly(data=df_subs_ORF8,
            #height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF8_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          # rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          range = c(1,ORF8_LENGTH)),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          # dtick = 0.2*min(max(df_subs_ORF8$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_ORF8$numSubs/NUM_GENOMES), 1.0)), 
                          tick0 = 0, 
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=FALSE))
  })
  
  # ORF10
  output$p_subs_ORF10 <- plotly::renderPlotly(expr = {
    
    df_subs_ORF10 <- df_subs_ORF10 %>% mutate(text = paste0("Position: ", position, "\nConsensus: ", consAA, "\nMutation(s): ", subsAA, "\nFraction: ", numSubs, "/", NUM_GENOMES))
    
    plot_ly(data=df_subs_ORF10,
            #height = GB_PLOT_HEIGHT,
            type = 'bar', 
            x = ~ position,
            y = ~ numSubs/NUM_GENOMES,
            text = ~ text,
            hoverinfo = 'text'
            # hovertemplate = paste('<br>Position: %{x}<br>', 'Mutation(s): %{text}<extra></extra>')
    ) %>% 
      layout(xaxis = list(title = paste0("Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF10_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          # rangeslider = list(type = "date", bgcolor = brewer.pal(n = 9, name = "Pastel1")[9]),
                          range = c(1,ORF10_LENGTH)),
             yaxis = list(title = "Fraction",titlefont = "Helvetica",
                          # dtick = 0.2*min(max(df_subs_ORF10$numSubs/NUM_GENOMES), 1.0) %>% round(digits = 3),
                          range = c(0,min(max(df_subs_ORF10$numSubs/NUM_GENOMES), 1.0)), 
                          tick0 = 0, 
                          nticks = 5,
                          tickmode = "auto",
                          autotick = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=FALSE))
  })
  # B Epitopes map widgets ######  
  
  #S
  output$p_map_SB <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "S"))
    
    plot_ly(data=df_ep_map_B,
            # height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(50, S_LENGTH, 50))),
                          tickmode = "array",
                          autotick = TRUE, 
                          range = c(1,S_LENGTH)
      ),  
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #E
  output$p_map_EB <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "E"))
    
    plot_ly(data=df_ep_map_B,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, E_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          range = c(1,E_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #M
  output$p_map_MB <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "M"))
    
    plot_ly(data=df_ep_map_B,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, M_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          range = c(1,M_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #N
  output$p_map_NB <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "N"))
    
    plot_ly(data=df_ep_map_B,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(50, N_LENGTH, 50))),
                          tickmode = "array",
                          autotick = FALSE, 
                          range = c(1,N_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #orf1a
  output$p_map_orf1aB <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "orf1a"))
    
    plot_ly(data=df_ep_map_B,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(200, orf1a_LENGTH, 200))),
                          tickmode = "array",
                          autotick = FALSE, 
                          range = c(1,orf1a_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #orf1b
  output$p_map_orf1bB <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "orf1b"))
    
    plot_ly(data=df_ep_map_B,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(100, orf1b_LENGTH, 100))),
                          tickmode = "array",
                          autotick = FALSE, 
                          range = c(1,orf1b_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })  
  
  #ORF3a
  output$p_map_ORF3aB <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "ORF3a"))
    
    plot_ly(data=df_ep_map_B,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF3a_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          range = c(1,ORF3a_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #ORF6
  output$p_map_ORF6B <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "ORF6"))
    
    plot_ly(data=df_ep_map_B,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF6_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          range = c(1,ORF6_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #ORF7a
  output$p_map_ORF7aB <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "ORF7a"))
    
    plot_ly(data=df_ep_map_B,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF7a_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          range = c(1,ORF7a_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #ORF7b
  output$p_map_ORF7bB <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "ORF7b"))
    
    plot_ly(data=df_ep_map_B,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF7b_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          range = c(1,ORF7b_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #ORF8
  output$p_map_ORF8B <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "ORF8"))
    
    plot_ly(data=df_ep_map_B,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF8_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          range = c(1,ORF8_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #ORF10
  output$p_map_ORF10B <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_B <- ep.map.vis(rdfs$df_Bepitopes %>% filter(Protein == "ORF10"))
    
    plot_ly(data=df_ep_map_B,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart, 
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF10_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE, 
                          range = c(1,ORF10_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  
  # T Epitopes map widgets ######  
  
  #S
  output$p_map_STMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "S"))
    
    plot_ly(data=df_ep_map_T,
            # height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(50, S_LENGTH, 50))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,S_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #E
  output$p_map_ETMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "E"))
    
    plot_ly(data=df_ep_map_T,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, E_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,E_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #M
  output$p_map_MTMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "M"))
    
    plot_ly(data=df_ep_map_T,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, M_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,M_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #N
  output$p_map_NTMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "N"))
    
    plot_ly(data=df_ep_map_T,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(50, N_LENGTH, 50))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,N_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #orf1a
  output$p_map_orf1aTMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "orf1a"))
    
    plot_ly(data=df_ep_map_T,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(200, orf1a_LENGTH, 200))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,orf1a_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #orf1b
  output$p_map_orf1bTMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "orf1b"))
    
    plot_ly(data=df_ep_map_T,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(100, orf1b_LENGTH, 100))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,orf1b_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #ORF3a
  output$p_map_ORF3aTMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "ORF3a"))
    
    plot_ly(data=df_ep_map_T,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF3a_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,ORF3a_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #ORF6
  output$p_map_ORF6TMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "ORF6"))
    
    plot_ly(data=df_ep_map_T,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF6_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,ORF6_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #ORF7a
  output$p_map_ORF7aTMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "ORF7a"))
    
    plot_ly(data=df_ep_map_T,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF7a_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,ORF7a_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #ORF7b
  output$p_map_ORF7bTMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "ORF7b"))
    
    plot_ly(data=df_ep_map_T,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF7b_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,ORF7b_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #ORF8
  output$p_map_ORF8TMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "ORF8"))
    
    plot_ly(data=df_ep_map_T,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF8_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,ORF8_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  #ORF10
  output$p_map_ORF10TMHC <- plotly::renderPlotly(expr = {
    
    
    
    
    
    
    df_ep_map_T <- ep.map.vis(rdfs$df_TMHCepitopes %>%   
                                filter(Assay %in% if_else(condition = c(input$Tassay, input$Tassay), 
                                                          true = "T cell", false = c("T cell", "MHC ligand"))) %>%
                                filter(Protein == "ORF10"))
    
    plot_ly(data=df_ep_map_T,
            #height = GB_PLOT_HEIGHT,
            type = "scatter",
            x = ~ refStart,
            y = ~ y_pos,
            # symbol = ~ `Source`,
            # color = ~ `Source`,
            text = ~ paste0(Epitope, " (Position: ", Start, "-", End, ")"),
            hoverinfo = 'text',
            mode = 'markers',
            marker = list(
              size = 10,
              opacity = 0.5,
              color = brewer.pal(n = 8, name = "Paired")[3],
              line = list(color = brewer.pal(n = 8, name = "Paired")[4],
                          width = 2),
              symbol = 'square')
    ) %>% 
      layout(xaxis = list(title = paste0(" Position"),titlefont = "Helvetica",
                          # dtick = 50,
                          # tick0 = 0, 
                          tickvals = as.list(c(1, seq.int(10, ORF10_LENGTH, 10))),
                          tickmode = "array",
                          autotick = FALSE,
                          range = c(1,ORF10_LENGTH)
      ),
      yaxis = list(title = "Number of epitopes",titlefont = "Helvetica", standoff = 0, visible = FALSE)) %>% 
      config(displayModeBar = F) %>% 
      layout(xaxis=list(fixedrange=FALSE)) %>% layout(yaxis=list(fixedrange=TRUE))
  })
  
  # Population coverage table ###### 

  
  output$selregion <- renderText({ 
    
    SelectedRegion <-
      case_when(input$PopCovRegion == 1 ~ "World",
                input$PopCovRegion == 2 ~ "East Asia",
                input$PopCovRegion == 3 ~ "Northeast Asia",
                input$PopCovRegion == 4 ~ "South Asia",
                input$PopCovRegion == 5 ~ "Southeast Asia",
                input$PopCovRegion == 6 ~ "Southwest Asia",
                input$PopCovRegion == 7 ~ "Europe",
                input$PopCovRegion == 8 ~ "East Africa", 
                input$PopCovRegion == 9 ~ "West Africa", 
                input$PopCovRegion == 10 ~ "Central Africa",  
                input$PopCovRegion == 11 ~ "North Africa",
                input$PopCovRegion == 12 ~ "South Africa",
                input$PopCovRegion == 13 ~ "West Indies",   
                input$PopCovRegion == 14 ~ "North America",
                input$PopCovRegion == 15 ~ "South America", 
                input$PopCovRegion == 16 ~ "Central America",
                input$PopCovRegion == 17 ~ "Oceania"
      )
    paste("Estimated population coverages of the identified T cell epitopes in region: ", SelectedRegion)
  })
  
  
    output$DT_PopCovALLTcell <- DT::renderDataTable(server = FALSE, {
    
    SelectedRegion <-
      case_when(input$PopCovRegion == 1 ~ "World",
              input$PopCovRegion == 2 ~ "East Asia",
              input$PopCovRegion == 3 ~ "Northeast Asia",
              input$PopCovRegion == 4 ~ "South Asia",
              input$PopCovRegion == 5 ~ "Southeast Asia",
              input$PopCovRegion == 6 ~ "Southwest Asia",
              input$PopCovRegion == 7 ~ "Europe",
              input$PopCovRegion == 8 ~ "East Africa", 
              input$PopCovRegion == 9 ~ "West Africa", 
              input$PopCovRegion == 10 ~ "Central Africa",  
              input$PopCovRegion == 11 ~ "North Africa",
              input$PopCovRegion == 12 ~ "South Africa",
              input$PopCovRegion == 13 ~ "West Indies",   
              input$PopCovRegion == 14 ~ "North America",
              input$PopCovRegion == 15 ~ "South America", 
              input$PopCovRegion == 16 ~ "Central America",
              input$PopCovRegion == 17 ~ "Oceania"
    )
    
    DT::datatable(data = rdfs$df_single_coverages %>% 
                    # filter(Epitope %in% rdfs$df_TMHCepitopes$Epitope) %>%
                    filter(`MHC Allele Class` %in% case_when(input$PopCovMHCclass == 1 ~ c("I", "II", "-N/A-"),
                                                             input$PopCovMHCclass == 2 ~ c("I"),
                                                             input$PopCovMHCclass == 3 ~ c("II"))) %>% 
                    filter(Contry == case_when(input$PopCovRegion == 1 ~ "World",
                                               input$PopCovRegion == 2 ~ "East Asia",
                                               input$PopCovRegion == 3 ~ "Northeast Asia",
                                               input$PopCovRegion == 4 ~ "South Asia",
                                               input$PopCovRegion == 5 ~ "Southeast Asia",
                                               input$PopCovRegion == 6 ~ "Southwest Asia",
                                               input$PopCovRegion == 7 ~ "Europe",
                                               input$PopCovRegion == 8 ~ "East Africa", 
                                               input$PopCovRegion == 9 ~ "West Africa", 
                                               input$PopCovRegion == 10 ~ "Central Africa",  
                                               input$PopCovRegion == 11 ~ "North Africa",
                                               input$PopCovRegion == 12 ~ "South Africa",
                                               input$PopCovRegion == 13 ~ "West Indies",   
                                               input$PopCovRegion == 14 ~ "North America",
                                               input$PopCovRegion == 15 ~ "South America", 
                                               input$PopCovRegion == 16 ~ "Central America",
                                               input$PopCovRegion == 17 ~ "Oceania"
                    )) %>% 
                    filter(Assay %in% case_when(input$PopCovTassay == 1 ~ c("T cell", "MHC ligand"),
                                                input$PopCovTassay == 2 ~ c("T cell"))
                             # if_else(condition = c(input$PopCovTassay, input$PopCovTassay), 
                             #                  true = "T cell", false = c("T cell", "MHC ligand"))
                             ) %>%
                    mutate(Region = Contry) %>%
                    mutate(Conservation = round(nCoV, 2)) %>% 
                    mutate(Coverage = round(Scov, 2)) %>%
                    mutate(`MHC allele class` = `MHC Allele Class`) %>% 
                    mutate(`MHC allele names` = `MHC Allele Names`) %>% 
                    select(IEDB, Epitope, Protein, 
                           # `MHC Allele Class`, 
                           `MHC allele names`, Conservation, Coverage), 
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lBfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE, pageLength = 10, 
                                 scrollX = FALSE, columnDefs = list(list(className = 'dt-center', targets="_all")),
                                 buttons = list(list(extend = c('csv'), text = ("Download csv"), filename= glue::glue("PopCov-", SelectedRegion)))
                                 ),
                  extensions = c('FixedHeader', 'Buttons')
                  )
  })
  # MSA downloads #####
  
  output$dlmsaS <- downloadHandler(
    filename = function() {
      paste("MSA-S", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "S.fasta"), to = file)
    }
  )
  
  output$dlmsaE <- downloadHandler(
    filename = function() {
      paste("MSA-E", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "E.fasta"), to = file)
    }
  )
  
  output$dlmsaM <- downloadHandler(
    filename = function() {
      paste("MSA-M", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "M.fasta"), to = file)
    }
  )
  
  output$dlmsaN <- downloadHandler(
    filename = function() {
      paste("MSA-N", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "N.fasta"), to = file)
    }
  )
  
  output$dlmsaorf1a <- downloadHandler(
    filename = function() {
      paste("MSA-orf1a", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "orf1a.fasta"), to = file)
    }
  )
  
  output$dlmsaorf1b <- downloadHandler(
    filename = function() {
      paste("MSA-orf1b", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "orf1b.fasta"), to = file)
    }
  )
  
  output$dlmsaORF3a <- downloadHandler(
    filename = function() {
      paste("MSA-ORF3a", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "ORF3a.fasta"), to = file)
    }
  )
  
  output$dlmsaORF6 <- downloadHandler(
    filename = function() {
      paste("MSA-ORF6", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "ORF6.fasta"), to = file)
    }
  )
  
  output$dlmsaORF7a <- downloadHandler(
    filename = function() {
      paste("MSA-ORF7a", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "ORF7a.fasta"), to = file)
    }
  )
  
  output$dlmsaORF7b <- downloadHandler(
    filename = function() {
      paste("MSA-ORF7b", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "ORF7b.fasta"), to = file)
    }
  )
  
  output$dlmsaORF8 <- downloadHandler(
    filename = function() {
      paste("MSA-ORF8", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "ORF8.fasta"), to = file)
    }
  )
  
  output$dlmsaORF10 <- downloadHandler(
    filename = function() {
      paste("MSA-ORF10", ".fasta", sep='')
    },
    content = function(file) {
      file.copy(from = here::here("Data", "MSAs", "ORF10.fasta"), to = file)
    }
  )
  
  
  # Acknowledgement tables ####
  
  # GISAID
  
  output$DT_ACK <- DT::renderDataTable(server = FALSE, {
    
    Raw_Acc_IDs <- list.files(path = here::here("Data", "Filtered"))
    
    Raw_Acc_IDs <- Raw_Acc_IDs %>% str_remove(pattern = ".fasta")
    
    ackdf <- as.data.frame(read_excel(here::here("Data", "Acknowledge.xls"), 
                                      sheet = "Acknowledgement Table", skip = 2, col_names = TRUE))
    
    # ackdf <- ackdf %>% filter(`Accession ID` %in% Raw_Acc_IDs) #metadf$`Accession ID`)
    
    DT::datatable(data = ackdf,
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE, pageLength = 10, 
                                 scrollX = FALSE, columnDefs = list(list(className = 'dt-center', targets="_all"))
                  ),
                  extensions = c('FixedHeader')
    )
  })
  
  
  # IEDB
  
  output$DT_IEDB <- DT::renderDataTable(server = FALSE, {
    
    ack_IEDBref <- read_csv(file = here::here("Data", "IEDB_reference_table.csv"), skip = 1, col_names = TRUE)  
    
    DT::datatable(data = ack_IEDBref %>% 
                    mutate(`IEDB Reference ID` = paste0("<a href=\"http://www.iedb.org/reference/",
                                                    ack_IEDBref$`Reference ID`,
                                                    "\"target=\"_blank\">", ack_IEDBref$`Reference ID`, "</a>")) %>%
                    mutate(`PubMed ID` = paste0("<a href=\"http://www.ncbi.nlm.nih.gov/pubmed/",
                                                    ack_IEDBref$`PubMed ID`,
                                                    "\"target=\"_blank\">", ack_IEDBref$`PubMed ID`, "</a>")) %>%
                    select(`IEDB Reference ID`, `PubMed ID`, Authors, Journal, Date, Title),
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = FALSE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE, pageLength = 10, 
                                 scrollX = FALSE, columnDefs = list(list(className = 'dt-center', targets="_all"))
                  ),
                  extensions = c('FixedHeader')
    )
  })  
  
  # R
  
  output$DT_R <- DT::renderDataTable(server = FALSE, {
   
    df <- as.data.frame(report::cite_packages(sessionInfo())) 
    df <- data.frame(References = df[-c(4, 23),])
    
    
    DT::datatable(data = df, 
                  editable = FALSE, style = 'bootstrap4',
                  filter = list(position = 'none', clear = 'TRUE'),
                  rownames = TRUE, escape = FALSE,
                  autoHideNavigation = TRUE, selection = 'none', 
                  options = list(dom = 'lfrtip',
                                 fixedHeader = FALSE,
                                 autowidth = TRUE, pageLength = 10, 
                                 scrollX = FALSE, columnDefs = list(list(className = 'dt-left', targets="_all"))
                  ),
                  extensions = c('FixedHeader')
    )
  })  
  
}

