sidebar <-  dashboardSidebar(
  
  tags$style(".left-side, .main-sidebar {padding-top: 120px}"),
  
  width = 250,
  sidebarMenu(
    id = "tabs",
    menuItem(strong("Home"), 
             tabName = "dashhome", icon = icon("th")),
    menuItem(text = ""),
    sliderInput("IDthreshold", 
                label = tags$p("Minimum conservation of epitopes\nwithin SARS-CoV-2 sequences"),
                # label = HTML("Minimum fraction of matching<br/>SARS-CoV-2 sequences<br/>for epitope selection"),
                min = 0.70, max = 1.00, value = ID_THRESH, step = 0.05
    ),
    menuItem(strong("Identified B cell epitopes"), 
             icon = icon("dna"), 
             menuItem(text = "Linear",
                      menuSubItem(tabName = "ALLdashbcell", text = "All proteins"),
                      menuSubItem(tabName = "Sdashbcell", text = "S ", selected = FALSE),
                      menuSubItem(tabName = "Edashbcell", text = "E ", selected = FALSE),
                      menuSubItem(tabName = "Mdashbcell", text = "M ", selected = FALSE),
                      menuSubItem(tabName = "Ndashbcell", text = "N ", selected = FALSE),
                      menuSubItem(tabName = "orf1adashbcell", text = "orf1a ", selected = FALSE),
                      menuSubItem(tabName = "orf1bdashbcell", text = "orf1b ", selected = FALSE),
                      menuSubItem(tabName = "ORF3adashbcell", text = "ORF3a ", selected = FALSE),
                      menuSubItem(tabName = "ORF6dashbcell", text = "ORF6 ", selected = FALSE),
                      menuSubItem(tabName = "ORF7adashbcell", text = "ORF7a ", selected = FALSE),
                      menuSubItem(tabName = "ORF7bdashbcell", text = "ORF7b ", selected = FALSE),
                      menuSubItem(tabName = "ORF8dashbcell", text = "ORF8 ", selected = FALSE),
                      menuSubItem(tabName = "ORF10dashbcell", text = "ORF10 ", selected = FALSE)
                      ),
             menuItem(tabName = "DISCONTdashbcell", text = "Discontinuous")
             ),
    menuItem(strong("Identified T cell epitopes"),
             icon = icon("dna"), 
             # checkboxInput("Tassay", label = "Only positive T cell assay", value = FALSE),
             menuSubItem(tabName = "ALLdashtcell", text = "All proteins"),
             menuSubItem(tabName = "Sdashtcell", text = "S ", selected = FALSE),
             menuSubItem(tabName = "Edashtcell", text = "E ", selected = FALSE),
             menuSubItem(tabName = "Mdashtcell", text = "M ", selected = FALSE),
             menuSubItem(tabName = "Ndashtcell", text = "N ", selected = FALSE),
             menuSubItem(tabName = "orf1adashtcell", text = "orf1a ", selected = FALSE),
             menuSubItem(tabName = "orf1bdashtcell", text = "orf1b ", selected = FALSE),
             menuSubItem(tabName = "ORF3adashtcell", text = "ORF3a ", selected = FALSE),
             menuSubItem(tabName = "ORF6dashtcell", text = "ORF6 ", selected = FALSE),
             menuSubItem(tabName = "ORF7adashtcell", text = "ORF7a ", selected = FALSE),
             menuSubItem(tabName = "ORF7bdashtcell", text = "ORF7b ", selected = FALSE),
             menuSubItem(tabName = "ORF8dashtcell", text = "ORF8 ", selected = FALSE),
             menuSubItem(tabName = "ORF10dashtcell", text = "ORF10 ", selected = FALSE),
             checkboxInput("Tassay", label = "Epitopes determined using\npositive T cell assays", value = FALSE)
             # checkboxInput("Tassay", label = "Show data only for epitopes\nstudied in positive T cell\nexperimental assays", value = FALSE)
             ),
    menuItem(
      text = strong("Population coverage analysis"),
      icon = icon("users"),
      tabName = "PopCovALLdashtcell"
             ), 
    menuItem(strong(HTML("Observed mutations in<br>SARS-CoV-2 proteins")),
             icon = icon("table"),
             menuSubItem(tabName = "Sdash", text = "S ", selected = FALSE),
             menuSubItem(tabName = "Edash", text = "E ", selected = FALSE),
             menuSubItem(tabName = "Mdash", text = "M ", selected = FALSE),
             menuSubItem(tabName = "Ndash", text = "N ", selected = FALSE),
             menuSubItem(tabName = "orf1adash", text = "orf1a ", selected = FALSE),
             menuSubItem(tabName = "orf1bdash", text = "orf1b ", selected = FALSE),
             menuSubItem(tabName = "ORF3adash", text = "ORF3a ", selected = FALSE),
             menuSubItem(tabName = "ORF6dash", text = "ORF6 ", selected = FALSE),
             menuSubItem(tabName = "ORF7adash", text = "ORF7a ", selected = FALSE),
             menuSubItem(tabName = "ORF7bdash", text = "ORF7b ", selected = FALSE),
             menuSubItem(tabName = "ORF8dash", text = "ORF8 ", selected = FALSE),
             menuSubItem(tabName = "ORF10dash", text = "ORF10 ", selected = FALSE)
            ),
    menuItem(strong("How to use COVIDep"), 
             tabName = "dashhelp", icon = icon("question-circle")
    ),
    menuItem(strong("About"), 
             tabName = "dashabout", icon = icon("info")
    ),
    menuItem(strong("Acknowledgements"), 
               tabName = "dashack", icon = icon("thumbs-up")
    )
  )
)