body <- dashboardBody(
  
  tags$head(includeHTML(("google-analytics.html"))),
  
  ### changing theme
  # dashboardthemes::shinyDashboardThemes(
  #   theme = "blue_gradient"
  # ),
  
  # tags$style(HTML("
  # 
  #     .selectize-input {
  #       height: 70px;
  #       width: 600px;
  #       font-size: 24pt;
  #       padding-top: 5px;
  #     }
  # 
  #   ")),
  # 
  # tags$style(HTML("
  # 
  #     .checkbox {
  #       height: 70px;
  #       width: 600px;
  #       font-size: 24pt;
  #       padding-top: 5px;
  #     }
  # 
  #   ")),
  
  ### CSS
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css")
  # ),
  
  # font-family: "Helvetica Neue", Times, "Helvetica Neue", serif;

  tags$head(tags$style(
    HTML(
    '.main-header .logo {
        font-weight: bold;
        font-size: 24px;
      }
    .content-wrapper, .right-side{
    background-color: #ffffff;
    }'
  )
  )),
  
  tabItems(
    
    # Home tab content
    # Home tab content
    tabItem(tabName = "dashhome", 
            fluidRow(
              box(
                width = 12,
                # height = 490,
                # footer = tags$a(href="https://doi.org/10.3390/v12030254", 
                # This virus is also referred to as the 2019 novel coronavirus (2019-nCoV), 
                # or the 2019 human coronavirus (hCoV-19). 
                #                 "Details in published paper"), 
                status = "primary", solidHeader = TRUE,  collapsible = TRUE, collapsed = TRUE,
                title = tags$div(HTML('<font color="white"><b>Real-time reporting of vaccine target recommendations for the COVID-19 coronavirus (SARS-CoV-2)</b></font>')), 
                tags$p(strong("COVIDep"), " provides an up-to-date set of B-cell and T-cell epitopes that can serve as 
                         potential vaccine targets for Severe Acute Respiratory Syndrome Coronavirus 2 (SARS-CoV-2), the virus 
                         causing the COVID-19 pandemic. The identified epitopes are experimentally-derived from SARS-CoV (the virus that caused 
                         the 2003 SARS outbreak) and have a close genetic match with the available SARS-CoV-2 sequences."),
                tags$p("We thank all the authors, the originating and submitting laboratories for sharing SARS-CoV-2 genetic 
                         sequences and SARS-CoV epitopes through the ", tags$a("GISAID", href = "https://www.gisaid.org/CoV2020/", target="_blank"), " and ", 
                       tags$a("VIPR", href = "https://www.viprbrc.org/brc/home.spg?decorator=corona", target="_blank"), " databases, respectively. We also thank the ",
                       tags$a("IEDB", href = "http://www.iedb.org/", target="_blank"), " database for providing the population coverage estimation tool for T cell epitopes.")
              )
            ),
            fluidRow(
              valueBoxOutput("latest_date", width = 6),
              valueBoxOutput("num_sequences", width = 6)
              # ,
              # valueBoxOutput("id_threshold", width = 5)
              ),
            fluidRow(
              box(
                width = 12, 
                title = strong("Epitopes identified as potential SARS-CoV-2 vaccine targets"), 
                footer = "Click on the interactive figure to see distribution details. Proportion of SARS-CoV epitopes with high genetic match in SARS-CoV-2 sequences are indicated in green.",
                status = "primary",
                solidHeader = TRUE,
                withLoader(plotlyOutput(outputId = "sunburst_epitopes"), 
                           type="html", loader="dnaspin")
                # plotlyOutput(outputId = "sunburst_epitopes")
              )
            ),
            # fluidRow(
            #   box(
            #     width = 12, 
            #     # height = 490,
            #     # footer = tags$a(href="https://doi.org/10.3390/v12030254", 
            #     #                 "Details in published paper"), 
            #     status = "primary", solidHeader = TRUE,  collapsible = TRUE, collapsed = TRUE,
            #     title = tags$div(HTML('<font color="white"><b>COVIDep: Real-time identification of potential vaccine targets for the COVID-19 coronavirus (SARS-CoV-2)</b></font>')), 
            #     tags$p(strong("COVIDep"), " provides an up-to-date set of B-cell and T-cell epitopes that can serve as 
            #              potential vaccine targets for Severe Acute Respiratory Syndrome Coronavirus 2 (SARS-CoV-2), the virus 
            #              causing the COVID-19 pandemic. This virus is also referred to as the 2019 novel coronavirus (2019-nCoV), 
            #              or the 2019 human coronavirus (hCoV-19). The identified epitopes are derived from SARS-CoV (the virus 
            #              that caused the 2003 SARS outbreak) and are highly conserved in the available SARS-CoV-2 sequences."),
            #     tags$p("We thank all the authors, the originating and submitting laboratories for sharing SARS-CoV-2 genetic 
            #              sequences and SARS-CoV epitopes through the ", tags$a("GISAID", href = "https://www.gisaid.org/CoV2020/", target="_blank"), " and ", 
            #            tags$a("VIPR", href = "https://www.viprbrc.org/brc/home.spg?decorator=corona", target="_blank"), " databases, respectively. We also thank the ",
            #            tags$a("IEDB", href = "http://www.iedb.org/", target="_blank"), " database for providing the population coverage estimation tool for T cell epitopes.")
            #   )
            # ),
            fluidRow(
              box(title = strong("Summary of SARS-CoV-2 sequences"), width = 12,
                  status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              tabBox(title = "", id = "tabseqinfo", width = 12, 
                     # height = "750px",
                tabPanel(
                  title = strong("Geographical distribution"), 
                  withLoader(plotlyOutput(outputId = "p_country", height = "600px"), 
                             type="html", loader="loader1")
                ),
                tabPanel(
                  title = strong("Temporal distribution: Overall"), 
                  box(title = "", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 12,
                  footer = ("Sequences with incomplete date information are excluded from the plot. Locations with only a single sequence are grouped together under 'Others'."),
                    withLoader(plotlyOutput(outputId = "p_hist"
                                            # , 
                                            # height = "600px"
                                            ), 
                             type="html", loader="loader1"))
                  ),
                tabPanel(
                  title = strong("Temporal distribution: Specific location"), 
                  box(title = "", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 3,
                      selectizeInput("TemporalRegion", label = "Location", 
                                     choices = ls_region, 
                                     selected = "USA")
                  ),
                  box(title = "", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, width = 9,
                      footer = ("Sequences with incomplete date information are excluded from the plot. Locations with only a single sequence are grouped together under 'Others'."),
                      withLoader(plotlyOutput(outputId = "p_date"
                                              # , 
                                              # height = "600px"
                                              ), 
                                 type="html", loader="loader1"))
                )
              )
              )
              ),
            fluidRow(
              # box(width = 9, solidHeader = TRUE),
              box(solidHeader = TRUE, width = 12,
                # tags$p(img(src='LOGO.png', align = "center", width = "1%", height = "1%"),
                style="text-align:center", 
                # HTML('&emsp;'),
                span(h6("© 2020 The Hong Kong University of Science and Technology. All rights reserved."), style = "color:grey")
                )
                )
              # ,
              # box(width = 3, solidHeader = TRUE)
            # )
              # fluidRow(
              #   # Dynamic infoBoxes and sunburst
              #   box(status = "primary", solidHeader = TRUE,
              #     valueBoxOutput("num_sequences")),
              #   box(status = "primary", solidHeader = TRUE,
              #       valueBoxOutput("latest_date"))  
              # ),
            # fluidRow(
              # box(title = tags$div(HTML('<font color="white"><b>Number of sequences collected from different countries</b></font>')), 
              #     plotlyOutput(outputId = "p_country"), 
              #     status = "primary", solidHeader = TRUE),
              # box(title = tags$div(HTML('<font color="white"><b>Cumulative count of the sequences and their date of collection</b></font>')), 
              #     plotlyOutput(outputId = "p_date"), 
              #     status = "primary", solidHeader = TRUE, footer = "Sequences that are not annotated with exact date of collection are not shown here.")
            # )
    ),
    
    # SARS-CoV-2 tab content
    #, height = MSA_HEIGHT
    #, height = GB_HEIGHT
    tabItem(tabName = "Sdash",
            fluidRow(
              box(title = strong(paste0("Fraction of sequences with mutations in the S protein")), #", NUM_GENOMES, " sequences of 
                  footer = ("Site numbering is based on S protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724390.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_S"), 
                             type="html", loader="loader1"), 
                  width = 12, 
                  # height = GB_HEIGHT, 
                  solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the S protein")),
                title = strong(paste0("Reference sequence of the S protein")),
                  footer = ("NCBI accession: YP_009724390.1"),
                  withLoader(
                    msaR::msaROutput(outputId = "S_MSA", height = 50),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaS", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    tabItem(tabName = "Edash",
            fluidRow(
              box(title = strong(paste0("Fraction of sequences with mutations in the E protein")), 
                  footer = ("Site numbering is based on E protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724392.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_E"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the E protein")),
                title = strong(paste0("Reference sequence of the E protein")),
                  withLoader(
                    msaR::msaROutput(outputId = "E_MSA", height = 50),
                    type="html", loader="loader1"),
                  footer = ("NCBI accession: YP_009724392.1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaE", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    tabItem(tabName = "Mdash",
            fluidRow(
              box(title = strong(paste0("Fraction of sequences with mutations in the M protein")), 
                  footer = ("Site numbering is based on M protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724393.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_M"),  
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the M protein")),
                title = strong(paste0("Reference sequence of the M protein")),
                  footer = ("NCBI accession: YP_009724393.1"),
                  withLoader(
                    msaR::msaROutput(outputId = "M_MSA", height = 50),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaM", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    tabItem(tabName = "Ndash",
            fluidRow(
              box(title = strong(paste0("Fraction of sequences with mutations in the N protein")), 
                  footer = ("Site numbering is based on N protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724397.2)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_N"), 
                    type="html", loader="loader1"), 
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the N protein")),
                title = strong(paste0("Reference sequence of the N protein")),
                  footer = ("NCBI accession: YP_009724397.2"),
                  withLoader(
                    msaR::msaROutput(outputId = "N_MSA", height = 50),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaN", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    tabItem(tabName = "orf1adash",
            fluidRow(
              box(
                title = strong(paste0("Fraction of sequences with mutations in the orf1a protein")), 
                footer = ("Site numbering is based on orf1a protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009725295.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_orf1a"),
                    type="html", loader="loader1"), 
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the orf1a protein")),
                title = strong(paste0("Reference sequence of the orf1a protein")),
                  footer = ("NCBI accession: YP_009725295.1"),
                  withLoader(
                    msaR::msaROutput(outputId = "orf1a_MSA", height = 50),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaorf1a", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    tabItem(tabName = "orf1bdash",
            fluidRow(
              box(title = strong(paste0("Fraction of sequences with mutations in the orf1b protein")),
                  footer = ("Site numbering is based on orf1b protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724389.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_orf1b"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the orf1b protein")),
                title = strong(paste0("Reference sequence of the orf1b protein")),
                  footer = ("NCBI accession: YP_009724389.1"),
                  withLoader(
                    msaR::msaROutput(outputId = "orf1b_MSA", height = 50),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaorf1b", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    tabItem(tabName = "ORF3adash",
            fluidRow(
              box(title = strong(paste0("Fraction of sequences with mutations in the ORF3a protein")),
                  footer = ("Site numbering is based on ORF3a protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724391.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_ORF3a"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the ORF3a protein")),
                title = strong(paste0("Reference sequence of the ORF3a protein")),
                  footer = ("NCBI accession: YP_009724391.1"),
                  withLoader(
                    msaR::msaROutput(outputId = "ORF3a_MSA", height = 50),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaORF3a", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    tabItem(tabName = "ORF6dash",
            fluidRow(
              box(title = strong(paste0("Fraction of sequences with mutations in the ORF6 protein")),
                  footer = ("Site numbering is based on ORF6 protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724394.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_ORF6"),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the ORF6 protein")),
                title = strong(paste0("Reference sequence of the ORF6 protein")),
                  footer = ("NCBI accession: YP_009724394.1"),
                  withLoader(
                    msaR::msaROutput(outputId = "ORF6_MSA", height = 50),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaORF6", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    tabItem(tabName = "ORF7adash",
            fluidRow(
              box(title = strong(paste0("Fraction of sequences with mutations in the ORF7a protein")),
                  footer = ("Site numbering is based on ORF7a protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724395.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_ORF7a"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the ORF7a protein")),
                title = strong(paste0("Reference sequence of the ORF7a protein")),
                  footer = ("NCBI accession: YP_009724395.1"),
                  withLoader(
                    msaR::msaROutput(outputId = "ORF7a_MSA", height = 50),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaORF7a", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    tabItem(tabName = "ORF7bdash",
            fluidRow(
              box(title = strong(paste0("Fraction of sequences with mutations in the ORF7b protein")),
                  footer = ("Site numbering is based on ORF7b protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009725318.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_ORF7b"),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the ORF7b protein")),
                title = strong(paste0("Reference sequence of the ORF7b protein")),
                  footer = ("NCBI accession: YP_009725318.1"),
                  withLoader(
                    msaR::msaROutput(outputId = "ORF7b_MSA", height = 50),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaORF7b", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    tabItem(tabName = "ORF8dash",
            fluidRow(
              box(title = strong(paste0("Fraction of sequences with mutations in the ORF8 protein")),
                  footer = ("Site numbering is based on ORF8 protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724396.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_ORF8"),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the ORF8 protein")),
                title = strong(paste0("Reference sequence of the ORF8 protein")),
                  footer = ("NCBI accession: YP_009724396.1"),
                  withLoader(
                    msaR::msaROutput(outputId = "ORF8_MSA", height = 50),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaORF8", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    tabItem(tabName = "ORF10dash",
            fluidRow(
              box(title = strong(paste0("Fraction of sequences with mutations in the ORF10 protein")),
                  footer = ("Site numbering is based on ORF10 protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009725255.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_subs_ORF10"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            ,
            fluidRow(
              box(
                # title = strong(paste0("Aligned ", NUM_GENOMES, " sequences of the ORF10 protein")),
                title = strong(paste0("Reference sequence of the ORF10 protein")),
                  footer = ("NCBI accession: YP_009725255.1"),
                  withLoader(
                    msaR::msaROutput(outputId = "ORF10_MSA", height = 50),
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "primary")
            )
            # ,
            # fluidRow(
            #   box(downloadButton("dlmsaORF10", label = "Download MSA"), solidHeader = FALSE)
            # )
    ),
    # B cell tab content
    tabItem(tabName = "ALLdashbcell",
            fluidRow(
              valueBoxOutput("num_B_epitopes", width = 6)
            ),
            fluidRow(
              box(title = strong("Details of all the identified B cell epitopes"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ALLBcell"),  
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    
    tabItem(tabName = "Sdashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the S protein"),
                  footer = ("Site numbering is based on S protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724390.1)."),
                  plotly::plotlyOutput(outputId = "p_map_SB"), 
                  width = 12, 
                  # height = GB_HEIGHT, 
                  solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the S protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_SBcell"), 
                    type="html", loader="loader1"), 
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "Edashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the E protein"), 
                  footer = ("Site numbering is based on E protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724392.1)."),
                  plotly::plotlyOutput(outputId = "p_map_EB"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the E protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_EBcell"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "Mdashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the M protein"), 
                  footer = ("Site numbering is based on M protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724393.1)."),
                  plotly::plotlyOutput(outputId = "p_map_MB"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the M protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_MBcell"),  
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "Ndashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the N protein"), 
                  footer = ("Site numbering is based on N protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724397.2)."),
                  plotly::plotlyOutput(outputId = "p_map_NB"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the N protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_NBcell"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "orf1adashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the orf1a protein"), 
                  footer = ("Site numbering is based on orf1a protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009725295.1)."),
                  plotly::plotlyOutput(outputId = "p_map_orf1aB"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the orf1a protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_orf1aBcell"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "orf1bdashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the orf1b protein"), 
                  footer = ("Site numbering is based on orf1b protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724389.1)."),
                  plotly::plotlyOutput(outputId = "p_map_orf1bB"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the orf1b protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_orf1bBcell"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF3adashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the ORF3a protein"), 
                  footer = ("Site numbering is based on ORF3a protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724391.1)."),
                  plotly::plotlyOutput(outputId = "p_map_ORF3aB"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the ORF3a protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ORF3aBcell"),  
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF6dashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the ORF6 protein"), 
                  footer = ("Site numbering is based on ORF6 protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724394.1)."),
                  plotly::plotlyOutput(outputId = "p_map_ORF6B"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the ORF6 protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ORF6Bcell"),  
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF7adashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the ORF7a protein"), 
                  footer = ("Site numbering is based on ORF7a protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724395.1)."),
                  plotly::plotlyOutput(outputId = "p_map_ORF7aB"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the ORF7a protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ORF7aBcell"),  
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF7bdashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the ORF7b protein"), 
                  footer = ("Site numbering is based on ORF7b protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009725318.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_map_ORF7bB"),
                    type="html", loader="loader1"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the ORF7a protein"), 
                  DT::DTOutput(outputId = "DT_ORF7bBcell"), 
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF8dashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the ORF8 protein"), 
                  footer = ("Site numbering is based on ORF8 protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724396.1)."),
                  withLoader(
                    plotly::plotlyOutput(outputId = "p_map_ORF8B"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the ORF8 protein"), 
                  DT::DTOutput(outputId = "DT_ORF8Bcell"), 
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF10dashbcell",
            fluidRow(
              box(title = strong("Location of the identified B cell epitopes on the primary structure of the ORF10 protein"), 
                  footer = ("Site numbering is based on ORF10 protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009725255.1)."),
                  plotly::plotlyOutput(outputId = "p_map_ORF10B"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified B cell epitopes in the ORF10 protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ORF10Bcell"),  
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "DISCONTdashbcell",
            fluidRow(
              valueBox(value = 0, subtitle = "Identified discontinuous B cell epitopes",  
                       color = "maroon")
            )
    ),
    
    # T cell tab content
    tabItem(tabName = "ALLdashtcell",
            fluidRow(
              valueBoxOutput("num_TMHC_epitopes", width = 6)
              # valueBox(value = NUM_T_EPITOPES, 
              #          subtitle = strong("Identified T cell epitopes"),  
              #          color = "green",
              #          icon = icon("check-circle"))
            ),
            fluidRow(
              box(title = strong("Details of all the identified T cell epitopes"),
                  withLoader(
                    DT::DTOutput(outputId = "DT_ALLTcell"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
            ),
    tabItem(tabName = "Sdashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the S protein"),
                  footer = ("Site numbering is based on S protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724390.1)."),
              plotly::plotlyOutput(outputId = "p_map_STMHC"), 
              width = 12, 
              # height = GB_HEIGHT, 
              solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the S protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_STcell"),  
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "Edashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the E protein"),
                  footer = ("Site numbering is based on E protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724392.1)."),
                  plotly::plotlyOutput(outputId = "p_map_ETMHC"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the E protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ETcell"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "Mdashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the M protein"),
                  footer = ("Site numbering is based on M protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724393.1)."),
                  plotly::plotlyOutput(outputId = "p_map_MTMHC"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the M protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_MTcell"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "Ndashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the N protein"),
                  footer = ("Site numbering is based on N protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724397.2)."),
                  plotly::plotlyOutput(outputId = "p_map_NTMHC"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the N protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_NTcell"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "orf1adashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the orf1a protein"),
                  footer = ("Site numbering is based on orf1a protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009725295.1)."),
                  plotly::plotlyOutput(outputId = "p_map_orf1aTMHC"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the orf1a protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_orf1aTcell"),
                    type="html", loader="loader1"), 
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "orf1bdashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the orf1b protein"),
                  footer = ("Site numbering is based on orf1b protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724389.1)."),
                  plotly::plotlyOutput(outputId = "p_map_orf1bTMHC"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the orf1b protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_orf1bTcell"),  
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF3adashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the ORF3a protein"),
                  footer = ("Site numbering is based on ORF3a protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724391.1)."),
                  plotly::plotlyOutput(outputId = "p_map_ORF3aTMHC"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the ORF3a protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ORF3aTcell"),
                    type="html", loader="loader1"), 
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF6dashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the ORF6 protein"),
                  footer = ("Site numbering is based on ORF6 protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724394.1)."),
                  plotly::plotlyOutput(outputId = "p_map_ORF6TMHC"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the ORF6 protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ORF6Tcell"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF7adashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the ORF7a protein"),
                  footer = ("Site numbering is based on ORF7a protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724395.1)."),
                  plotly::plotlyOutput(outputId = "p_map_ORF7aTMHC"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the ORF7a protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ORF7aTcell"),  
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF7bdashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the ORF7b protein"),
                  footer = ("Site numbering is based on ORF7b protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009725318.1)."),
                  plotly::plotlyOutput(outputId = "p_map_ORF7bTMHC"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the ORF7b protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ORF7bTcell"),  
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF8dashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the ORF8 protein"),
                  footer = ("Site numbering is based on ORF8 protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009724396.1)."),
                  plotly::plotlyOutput(outputId = "p_map_ORF8TMHC"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the ORF8 protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ORF8Tcell"), 
                    type="html", loader="loader1"),
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    tabItem(tabName = "ORF10dashtcell",
            fluidRow(
              box(title = strong("Location of the identified T cell epitopes on the primary structure of the ORF10 protein"),
                  footer = ("Site numbering is based on ORF10 protein of the reference genome Wuhan-Hu-1/2019 (NCBI accession: YP_009725255.1)."),
                  plotly::plotlyOutput(outputId = "p_map_ORF10TMHC"), 
                  width = 12, solidHeader = TRUE, status = "success") 
            ),
            fluidRow(
              box(title = strong("Details of the identified T cell epitopes in the ORF10 protein"), 
                  withLoader(
                    DT::DTOutput(outputId = "DT_ORF10Tcell"), 
                    type="html", loader="loader1"), 
                  width = 12, solidHeader = TRUE, status = "success")
            )
    ),
    
    # Pop coverages of T cell tab content
    tabItem(tabName = "PopCovALLdashtcell",
            fluidRow(box(title = "Region", height = "120px",
                         solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = TRUE, width = 2,
            selectizeInput("PopCovRegion", label = "", 
                           choices = list("World" = 1,
                                          "East Asia" = 2,
                                          "Northeast Asia" = 3,
                                          "South Asia" = 4,
                                          "Southeast Asia" = 5,
                                          "Southwest Asia" = 6,
                                          "Europe" = 7,
                                          "East Africa" = 8, 
                                          "West Africa" = 9, 
                                          "Central Africa" = 10,  
                                          "North Africa" = 11,
                                          "South Africa" = 12,
                                          "West Indies" = 13,   
                                          "North America" = 14,
                                          "South America" = 15, 
                                          "Central America" = 16,
                                          "Oceania" = 17
                           ), 
                           selected = 1)),
            box(title = "MHC allele class", height = "120px",
                solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = TRUE, width = 3,
            radioButtons("PopCovMHCclass", label = "", 
                         choices = list("Both I & II" = 1, "I only" = 2, "II only" = 3), inline = TRUE,
                         selected = 1)),
            box(title = "Assay type", height = "120px",
                solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = TRUE, width = 4,
                radioButtons("PopCovTassay", label = "", 
                             choices = list("All" = 1, "T cell only" = 2), inline = TRUE,
                             selected = 1)
                # checkboxInput("PopCovTassay", label = "Epitopes determined using\npositive T cell assays", value = FALSE)
            )
            ),
            fluidRow(
              box(title = strong(textOutput("selregion")), 
                  footer = "Blank coverage values (if any) imply no data available.", 
                  # "Missing values for population coverage are due to unavailability of population-level MHC statistics for the specific MHC allele and region.",
                  withLoader(
                    DT::DTOutput(outputId = "DT_PopCovALLTcell"), 
                    type="html", loader="loader1"), 
                  width = 12, solidHeader = TRUE, status = "success")
            )
            ),    
    tabItem(tabName = "dashack",
              fluidRow(
                box(width = 12, status = "primary",
                tags$p("COVIDep is made possible by the open sharing of genome sequence data of SARS-CoV-2 sequences 
                by research groups from around the world through the GISAID platform, and the open sharing of 
                immunological data of experimentally determined SARS-CoV epitopes through IEDB and ViPR databases. We 
                gratefully acknowledge the contributions of all the researchers, scientists and technical staff involved."),
                tags$p("We would like to acknowledge the support extended by the Department of Electronic and Computer Engineering at the Hong Kong University of Science and Technology for providing the required resources and setting up the server for the web application."),
                tags$p("Special thanks to Dr. Raymond Louie, Dr. David Morales, Dr. Saqib Sohail, Neelkanth Kundu, Awais Shah, and Umer Abdullah for comments and suggestions that helped to improve the web application and its interface.")
                ),
                box(title = strong("Acknowledgement for all SARS-CoV-2 sequences downloaded from GISAID"), 
                    width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, 
                    withLoader(
                      DT::DTOutput(outputId = "DT_ACK"), 
                      type="html", loader="loader1"), 
                ),
                box(title = strong("Acknowledgement for all epitope data downloaded from ViPR/IEDB"), 
                    width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, 
                    withLoader(
                      DT::DTOutput(outputId = "DT_IEDB"), 
                      type="html", loader="loader1"), 
                ),
                box(title = strong("Acknowledgement for all the R/Shiny tools and packages used in the development"), 
                    width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, 
                    withLoader(
                      DT::DTOutput(outputId = "DT_R"), 
                      type="html", loader="loader1"), 
                )
              )
    ),
    tabItem(tabName = "dashhelp",
            fluidRow(
              # box(title = strong("Slider input labelled 'Minimum conservation of epitopes within SARS-CoV-2 sequences'"),
              #     width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
              #     tags$p(img(src='Fig_global_slider.png', align = "center", width = "100%", height = "100%"))
              # ),
              box(title = strong("Home"), width = 12, status = "primary", collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE,
                  box(title = strong("Description of the 'sunburst' plot"), 
                      width = 12, solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                      # tags$p(img(src='sunburst1.png', align = "center", width = "60%", height = "60%")),
                      tags$p(img(src='Fig_sunburst1.png', align = "center", width = "100%", height = "100%")),
                      tags$p(img(src='Epitopes_division_v1.png', align = "center", width = "100%", height = "100%")),
                      # tags$h4(strong("I. The SARS-CoV epitopes are divided as follows:")),
                      # tags$h4("Category 1: B cell"),
                      # tags$h5("         1.1: Linear"),
                      # tags$h5("         1.1.1: Identical epitopes"),
                      # tags$h5("         1.2: Discontinuous"),
                      # tags$h5("         1.2.1: Identical epitopes (Not displayed as no epitope satisfies the criteria)"),
                      # tags$h4("Category 2: T cell"),
                      # tags$h5("         2.1: Positive MHC binding (i.e., epitopes determined using positive MHC binding assays)<br>"),
                      # tags$h5("         2.1.1: Identical epitopes"),
                      # tags$h5("         2.2: Positive T cell (i.e., epitopes determined using positive T cell assays)"),
                      # tags$h5("         2.2.1: Identical epitopes"),
                      tags$h4(strong("I. Hovering mouse over the plot shows the specific number of the SARS-CoV epitopes with high genetic match in SARS-CoV-2 sequences.")),
                      tags$p(img(src='Fig_sunburst2.png', align = "center", width = "100%", height = "100%")),
                      tags$h4(strong("II. Clicking on a category level (e.g., T cell) in the sunburst plots focuses on the distribution of epitopes within that category. Clicking the same category again takes you back to the parent category.")),
                      tags$p(img(src='Fig_sunburst3.png', align = "center", width = "100%", height = "100%"))
                  ),
                  box(title = strong("'Geographical distribution' plot under 'Summary of SARS-CoV-2 sequences'"),
                      width = 12, solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                      tags$p(img(src='Fig_geographic_dist.png', align = "center", width = "100%", height = "100%"))
                  ),
                  box(title = strong("'Temporal distribution' plots under 'Summary of SARS-CoV-2 sequences'"), 
                      width = 12, solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                      tags$h4(strong("I. Temporal distribution: Overall")),
                      tags$p(img(src='Fig_temporal_dist_overall.png', align = "center", width = "100%", height = "100%")),
                      tags$h4(strong("II. Temporal distribution: Specific location")),
                      tags$p(img(src='Fig_temporal_dist_specific.png', align = "center", width = "100%", height = "100%"))
                      # ,
                      # tags$h4(strong("III. Legend selection to show for one country")),
                      # tags$p(img(src='Fig_temporal_dist_legend.png', align = "center", width = "100%", height = "100%"))
                  )
              ),
              box(title = strong("Slider input labelled 'Minimum conservation of epitopes within SARS-CoV-2 sequences'"),
                  width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                  box(
                    title = strong(""), 
                    width = 12, solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
                    tags$p(strong("Definition of conservation: "), "Conservation of an epitope within SARS-CoV-2 sequences represents the fraction of available SARS-CoV-2 sequences with the exact epitope sequence.")
                  ),
                  tags$p(img(src='Fig_global_slider.png', align = "center", width = "100%", height = "100%"))
              ),
              box(title = strong("Identified B cell/T cell epitopes"), width = 12, status = "primary", collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE,
              box(title = strong("Tables of epitopes"),
                  width = 12, solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                    tags$p(img(src='Fig_table_epitopes.png', align = "center", width = "100%", height = "100%"))              
              ),
              box(title = strong("Description of the 'identified epitopes location on the primary structure' plot"),
                  width = 12, solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                  tags$h5(strong("I. Plot details")),
                  tags$p(img(src='Fig_identified_epitopes_location_on_primary_structure.png', align = "center", width = "100%", height = "100%")),
                  tags$h5(strong("II. Zooming in and out on the primary structure")),
                  tags$p(img(src='Fig_identified_epitopes_location_on_primary_structure_zooming.png', align = "center", width = "100%", height = "100%"))
              )
              ),
              box(title = strong("Population coverage analysis"),
                  width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                  tags$p(img(src='Fig_pop_coverage.png', align = "center", width = "100%", height = "100%"))
              ),
              box(title = strong("Observed mutations in SARS-CoV-2 proteins"), width = 12, status = "primary", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
              # box(title = strong("'Fraction of sequences with mutations' plot"),
              #     width = 12, solidHeader = FALSE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                  tags$h5(strong("I. Plot details")),
                  tags$p(img(src='Fig_frac_mutations.png', align = "center", width = "100%", height = "100%")),
                  tags$h5(strong("II. Zooming in and clicking the bar displays the substitutions observed so far at a protein location")),
                  tags$p(img(src='Fig_frac_mutations_zoomed_in.png', align = "center", width = "100%", height = "100%")),
                  tags$h5(strong("III. 'Reference sequence' plot")),
                  tags$p(img(src='Fig_ref_seq.png', align = "center", width = "100%", height = "100%")),
              # )
              # ,
              # box(title = strong("Description of the 'aligned sequences of a protein' plot"),
              #     width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
              #     tags$p(img(src='Fig_MSA.png', align = "center", width = "100%", height = "100%"))
              # ),
              )
              # ,
              # box(title = strong("Population coverage analysis"),
              #     width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
              #     tags$p(img(src='Fig_pop_coverage.png', align = "center", width = "100%", height = "100%"))
              # )
    )
    ),
    tabItem(tabName = "dashabout",
              fluidRow(
                box(title = strong("COVIDep development"), width = 12, 
                    status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
                    # HTML('<center><img src="flowchart_01.jpeg"></center>')
                    tags$p("COVIDep is developed and maintained by Prof. Matthew McKay's Signal Processing & Computational Biology Lab (",
                           tags$a("SPCB", href = "https://www.mckayspcb.com/", target="_blank"), ") at the Hong Kong University of Science 
                           and Technology (HKUST)."
                           ),
                    tags$p("COVIDep aims to provide real-time potential vaccine targets for SARS-CoV-2. It screens the SARS-derived B cell and 
                           T cell epitopes (available at ", tags$a("VIPR", href = "https://www.viprbrc.org/brc/home.spg?decorator=corona", target="_blank"), "/", tags$a("IEDB", href = "http://www.iedb.org/", target="_blank"), ") and identifies those which are highly conserved within the available SARS-CoV-2 
                           sequences (continuing to be deposited at ", tags$a("GISAID", href = "https://www.gisaid.org/CoV2020/", target="_blank"), "."),
                    tags$p("The raw genome sequences from 'Human' hosts are downloaded from ", tags$a("GISAID", href = "https://www.gisaid.org/CoV2020/", target="_blank"), 
                           " and filtered to include only the full-length genomes. The sequences which are marked in the 'Comments' column on the ", tags$a("GISAID", href = "https://www.gisaid.org/CoV2020/", target="_blank"), 
                           " website as having issues are excluded. All remaining genomic sequences are aligned and translated to yield protein 
                           sequences according to the ", tags$a("reference genome", href = "https://www.ncbi.nlm.nih.gov/nuccore/NC_045512", target = "_blank"), 
                           " of SARS-CoV-2 obtained from NCBI. The minimum conservation for selecting epitopes is determined by the global “epitope screening” 
                           parameter, which can be set using the slider in the menu bar. For the identified T cell epitopes, an estimated population 
                           coverage is computed using the population coverage tool provided at the ", tags$a("IEDB Analysis Resource", href = "http://tools.iedb.org/population/", target="_blank"), "."),
                    tags$p("COVIDep is developed using the ", tags$a("Shiny", href = "http://shiny.rstudio.com/", 
                                                                      target="_blank"), 
                           " based web app development framework provided by RStudio. It is an open source web app and all the code scripts are available at the ",
                           tags$a("GitHub", href = "https://github.com/faraz107/COVIDep", target="_blank"), " repository. The web app display is optimized 
                           for viewing at 100% screen resolution on Chrome for PC/laptop. For any questions, comments 
                           or suggestions, please feel free to contact us at COVIDep@ust.hk." )
                ),
                box(title = strong("Method"), width = 12, status = "primary", solidHeader = TRUE,
                    # HTML('<center><img src="flowchart_01.jpeg"></center>')
                    img(src='Figure_Method.png', align = "center", width = "100%", height = "100%")
                ),
                box(title = strong("Related publications"), width = 12, status = "primary", solidHeader = TRUE,
                    tags$p("Ahmed, S. F., Quadeer, A. A. & McKay, M. R. Preliminary identification of potential vaccine targets for the COVID-19 coronavirus (SARS-CoV-2) based on SARS-CoV immunological studies. Viruses 12, 254 (2020) ", 
                           tags$a("doi.org/10.3390/v12030254", href = "https://doi.org/10.3390/v12030254", target="_blank")),
                    tags$p("Grifoni, A. et al. A sequence homology and bioinformatic approach can predict candidate targets for immune responses to SARS-CoV-2. Cell Host Microbe 27, 1–10 (2020)"), 
                           # tags$a("https://doi.org/10.3390/v12030254", href = "https://doi.org/10.3390/v12030254", target="_blank")),
                    tags$p("Zheng, M. & Song, L. Novel antibody epitopes dominate the antigenicity of spike glycoprotein in SARS-CoV-2 compared to SARS-CoV. Cell. Mol. Immunol. 2, 9–11 (2020)"), 
                           # tags$a("https://doi.org/10.3390/v12030254", href = "https://doi.org/10.3390/v12030254", target="_blank")),
                    tags$p("Poh, C. M. et al. Potent neutralizing antibodies in the sera of convalescent COVID-19 patients are directed against conserved linear epitopes on the SARS-CoV-2 spike protein. bioRxiv (2020)"), 
                           # tags$a("doi:10.1101/2020.03.30.015461", href = "https://doi:10.1101/2020.03.30.015461", target="_blank")),
                    tags$p("Wrapp, D. et al. Cryo-EM structure of the 2019-nCoV spike in the prefusion conformation. Science. 367, 1260–1263 (2020)"), 
                           # tags$a("https://doi.org/10.3390/v12030254", href = "https://doi.org/10.3390/v12030254", target="_blank")),
                    tags$p("Ou, X. et al. Characterization of spike glycoprotein of SARS-CoV-2 on virus entry and its immune cross-reactivity with SARS-CoV. Nat. Commun. 11, 1620 (2020)"), 
                           # tags$a("https://doi.org/10.3390/v12030254", href = "https://doi.org/10.3390/v12030254", target="_blank")),
                    tags$p("Zheng, Z. et al. Monoclonal antibodies for the S2 subunit of spike of SARS-CoV cross-react with the newly-emerged SARS-CoV-2. bioRxiv (2020)"), 
                           # tags$a("https://doi.org/10.3390/v12030254", href = "https://doi.org/10.3390/v12030254", target="_blank")),
                    tags$p("Walls, A. C. et al. Structure, function, and antigenicity of the SARS-CoV-2 spike glycoprotein. Cell 180, 1–12 (2020)")#, 
                           # tags$a("https://doi.org/10.3390/v12030254", href = "https://doi.org/10.3390/v12030254", target="_blank")
                )
                )
    )
  )
)

