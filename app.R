#This app is designed to provide easy non-programmatic access to the
#R. Stephanie Huang Lab's reprocessed versions of the CTRPv2, GDSC1, GDSC2,
#and PRISM Repurposing high throughput cancer cell line drug screens.
#Written by Alexander L. Ling (alling@umn.edu)
#R. Stephanie Huang Lab, University of Minnesota
#6/18/2020

#Options
  options(stringsAsFactors = FALSE)

#Loading Libraries
  require(shiny)
  require(shinyhelper)
  require(shinyWidgets)
  require(readxl)
  require(plotly)
  require(ggplot2)

#Setting Seed
  set.seed(06182020)
  
#Working Directory
  setwd("D:/Huang Lab/simplicity app/Simplicity")
  
#Loading data for ui
  #Dataset Summaries
    Dataset_Summaries <- read.delim("./www/Dataset_Summaries/Dataset_Summaries.txt", sep = "\t")
    Dataset_ccls_per_cpd <- readRDS("./www/Dataset_Summaries/ccls_per_cpd.rds")
    Dataset_cpds_per_ccl <- readRDS("./www/Dataset_Summaries/cpds_per_ccl.rds")
    Dataset_rse <- readRDS("./www/Dataset_Summaries/dataset_rse.rds")
  #Compound and cell line availability
    compound_ccl_availability <- readRDS("./www/CCL_Availability_by_Compound.rds")
    ccl_compound_availability <- readRDS("./www/Compound_Availability_by_Cell_Line.rds")
    compound_ccl_availability_successful <- readRDS("./www/CCL_Availability_Successful_by_Compound.rds")
    ccl_compound_availability_successful <- readRDS("./www/Compound_Availability_Successful_by_Cell_Line.rds")
  #Cell line and compound information
    Cell_Line_Harm <- as.data.frame(read_xlsx("./www/Harmonized_CCL_Data_11_3_2020_manual_updates.xlsx", na = "NA"))
      Cell_Line_Harm$Dataset[grepl("CTRPv2", Cell_Line_Harm$Dataset)] <- "CTRPv2"
      Cell_Line_Harm$Dataset[grepl("GDSC1", Cell_Line_Harm$Dataset)] <- "GDSC1"
      Cell_Line_Harm$Dataset[grepl("GDSC2", Cell_Line_Harm$Dataset)] <- "GDSC2"
      Cell_Line_Harm$Dataset[grepl("PRISM_Repurposing", Cell_Line_Harm$Dataset)] <- "PRISM_Repurposing"
      Cell_Line_Harm$African_Ancestry <- as.numeric(Cell_Line_Harm$African_Ancestry)
      Cell_Line_Harm$Native_American_Ancestry <- as.numeric(Cell_Line_Harm$Native_American_Ancestry)
      Cell_Line_Harm$`East_Asian_(North)_Ancestry` <- as.numeric(Cell_Line_Harm$`East_Asian_(North)_Ancestry`)
      Cell_Line_Harm$`East_Asian_(South)_Ancestry` <- as.numeric(Cell_Line_Harm$`East_Asian_(South)_Ancestry`)
      Cell_Line_Harm$South_Asian_Ancestry <- as.numeric(Cell_Line_Harm$South_Asian_Ancestry)
      Cell_Line_Harm$`European_(North)_Ancestry` <- as.numeric(Cell_Line_Harm$`European_(North)_Ancestry`)
      Cell_Line_Harm$`European_(South)_Ancestry` <- as.numeric(Cell_Line_Harm$`European_(South)_Ancestry`)
    Compound_Harm <- as.data.frame(read_excel("./www/Harmonized_Compound_Data_11_1_2020_manual_updates.xlsx"))
      Compound_Harm$Dataset[grepl("CTRPv2", Compound_Harm$Dataset)] <- "CTRPv2"
      Compound_Harm$Dataset[grepl("GDSC1", Compound_Harm$Dataset)] <- "GDSC1"
      Compound_Harm$Dataset[grepl("GDSC2", Compound_Harm$Dataset)] <- "GDSC2"
      Compound_Harm$Dataset[grepl("PRISM_Repurposing", Compound_Harm$Dataset)] <- "PRISM_Repurposing"
  #Loading filename maps
    Compound_Filenames <- readRDS("./www/Compound_Filename_Master.rds")
    Cell_Line_Filenames <- readRDS("./www/Cell_Line_Filename_Master.rds")
  #Removing compound and cell line records when no files exist for those cell lines and compounds (i.e. no results)
    Cell_Line_Harm <- Cell_Line_Harm[Cell_Line_Harm$Harmonized_Cell_Line_ID %in% Cell_Line_Filenames$cell_lines,]
    Compound_Harm <- Compound_Harm[Compound_Harm$Harmonized_Compound_Name %in% Compound_Filenames$drugs,]
  #Making data frames of cell line and compound options/synonyms
    Compound_Option_df <- unique(Compound_Harm[,c("Harmonized_Compound_Name", "Compound_Synonyms")])
    for(i in 1:nrow(Compound_Option_df)){
      proper_name <- Compound_Option_df$Harmonized_Compound_Name[i]
      synonyms <- unlist(strsplit(Compound_Option_df$Compound_Synonyms[i], ":\\|:"))
      synonyms <- c(synonyms, gsub(" ", "", synonyms))
      synonyms <- c(synonyms, gsub("[[:punct:]]", "", synonyms))
      synonyms <- synonyms[! tolower(synonyms) %in% tolower(proper_name)]
      synonyms <- synonyms[! duplicated(tolower(synonyms))]
      if(length(synonyms) > 0){
        Compound_Option_df$Compound_Synonyms[i] <- paste0(proper_name, " [[", paste(synonyms, collapse = "; "), "]]")
      } else {
       Compound_Option_df$Compound_Synonyms[i] <- proper_name
      }
    }

    Cell_Line_Option_df <- unique(Cell_Line_Harm[,c("Harmonized_Cell_Line_ID", "Synonyms")])
    for(i in 1:nrow(Cell_Line_Option_df)){
      proper_name <- Cell_Line_Option_df$Harmonized_Cell_Line_ID[i]
      synonyms <- unlist(strsplit(Cell_Line_Option_df$Synonyms[i], ":\\|:"))
      synonyms <- c(synonyms, gsub(" ", "", synonyms))
      synonyms <- c(synonyms, gsub("[[:punct:]]", "", synonyms))
      synonyms <- synonyms[! tolower(synonyms) %in% tolower(proper_name)]
      synonyms <- synonyms[! duplicated(tolower(synonyms))]
      if(length(synonyms) > 0){
        Cell_Line_Option_df$Synonyms[i] <- paste0(proper_name, " [[", paste(synonyms, collapse = "; "), "]]")
      } else {
        Cell_Line_Option_df$Synonyms[i] <- proper_name
      }
    }

  #Making Simplified versions of compound and cell line harmonizers for sorting purposes
    Simple_Cell_Line_Harm <- unique(Cell_Line_Harm[,c("Harmonized_Cell_Line_ID", "Gender", "Numeric_Age_in_Years", "Diseases", "Simple_Cancer_Type", "African_Ancestry", "Native_American_Ancestry", "East_Asian_(North)_Ancestry", "East_Asian_(South)_Ancestry", "South_Asian_Ancestry", "European_(North)_Ancestry", "European_(South)_Ancestry")])
    Cell_Line_Diseases <- strsplit( Simple_Cell_Line_Harm$Diseases, ":\\|:")
      names(Cell_Line_Diseases) <-  Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID
    
    Simple_Compound_Harm <- unique(Compound_Harm[,c("Harmonized_Compound_Name", "Compound_Molecular_Targets", "Compound_MOA", "Compound_Clinical_Phase")])
    Compound_Molecular_Targets <- strsplit(Simple_Compound_Harm$Compound_Molecular_Targets, ":\\|:")
      names(Compound_Molecular_Targets) <- Simple_Compound_Harm$Harmonized_Compound_Name
    Unique_Compound_Molecular_Targets <- sort(unique(unlist(Compound_Molecular_Targets)))
    Compound_MOAs <- strsplit(Simple_Compound_Harm$Compound_MOA, ":\\|:")
      names(Compound_MOAs) <- Simple_Compound_Harm$Harmonized_Compound_Name
    Unique_Compound_MOAs <- sort(unique(unlist(Compound_MOAs)))
    
#Writing function for 4-parameter log-logistic curve
  #b = slope
  #c = lower asymptote
  #d = upper asymptote
  #e = EC50 (note, this is NOT log(EC50))
  #b_c_d_e should be input as a character vector with each element consisting of the string
  #"b_c_d_e" with the numeric values separated by an underscore in that order
  #x should be dose (not log(dose))
  ll.4 <- Vectorize(function(x, b_c_d_e){
    b_c_d_e <- as.data.frame(do.call(rbind, lapply(strsplit(b_c_d_e, "_"), as.numeric)))
    colnames(b_c_d_e) <- c("b", "c", "d", "e")
    return((b_c_d_e$c + (b_c_d_e$d - b_c_d_e$c)/(1 + exp(b_c_d_e$b*(log(x)-log(b_c_d_e$e))))))
  })
  
#Writing function to unique and remove NA, "NA", and "" values
  clean_vector <- function(x){
    return(unique(x[! x %in% c("", NA, "NA")]))
  }


#############################################################  
######################################################## UI #          
#############################################################
#Creating user interface function
  ui <- navbarPage(id = "tabs", title = "Simplicity",
                   
############################################################          
####################################################### UI #               
        tabPanel(value = "Welcome", title = "Welcome",
          
          #Welcome text for app
            h3("Welcome to the Cancer Drug Screen Portal!"),
            p("This app is designed to provide easy, non-programmatic access to the CTRPv2, GDSC1, GDSC2, 
            and PRISM Repurposing cancer cell line drug screening datasets. Please select the appropriate 
            tab at the top for the task you wish to complete with this data. You can also see a summary 
            of each of the datasets below."),
          #Selector for choosing which dataset to display a summary for
            sidebarLayout(sidebarPanel(
                selectInput(inputId = "Summary_Dataset", label = "Which dataset would you like to see a summary for?", choices = Dataset_Summaries$Dataset, selected = Dataset_Summaries$Dataset[1], multiple = FALSE, selectize = FALSE),
                uiOutput(outputId = "Dataset_Summary_Text")
              ), mainPanel(
                plotlyOutput(outputId = "n_Compounds_Per_Cell_Line"),
                plotlyOutput(outputId = "n_Cell_Lines_Per_Compound"),
                plotlyOutput(outputId = "Dataset_Residual_Standard_Error")
              )
            )
        ),

#############################################################          
######################################################## UI #        
        navbarMenu(title = "Data Explorer",
                   
######################################################## UI #                    
          tabPanel(value = "Explore Compounds", title = "Explore Compounds",
                sidebarLayout(sidebarPanel(
                  #Dropdown menu to select compound to explore
                    selectizeInput("compound_explorer", label = "Select a compound", choices = NULL, multiple = FALSE, options = list(
                        placeholder = 'Select compound',
                        # you can search the data based on these fields
                        searchField = c('Compound_Synonyms'),
                        # the label that will be shown once value is selected
                        labelField= 'Harmonized_Compound_Name',
                        # (each item is a row in data), which requires 'value' column (created by cbind at server side)
                         render = I("{
                                option: function(item, escape) {
                                  return '<div>' + escape(item.Harmonized_Compound_Name) +'</div>';
                                }
                              }")
                    )),
                    checkboxInput("Compound_Explorer_Show_Compound_Filters", label = "Show compound filters?", value = FALSE),
                    uiOutput(outputId = "Compound_Explorer_Compound_Filters"),
                  #Text display for information about this compound
                    uiOutput(outputId = "Compound_Explorer_Text"),
                  #Selection of cell lines to plot information for related to selected compound
                    uiOutput(outputId = "Compound_Explorer_Cell_Line_Menu"),
                    uiOutput(outputId = "Compound_Explorer_Cell_Line_Filters")
                ), mainPanel(
                  selectInput(inputId = "Compound_Explorer_to_Plot", "Select what values to plot", choices = c("AUC values for most commonly used concentration range", "AUC values for concentration range available for all cell lines", "IC50 values"), selected = "AUC values for most commonly used concentration range", multiple = FALSE, selectize = FALSE),
                  uiOutput(outputId = "Compound_Explorer_Plots")
                ))
          ),
          
######################################################## UI #           
          tabPanel(value = "Explore Cell Lines", title = "Explore Cell Lines",
                sidebarLayout(sidebarPanel(
                  #Dropdown menu to select Cell_Line to explore
                    selectizeInput("cell_line_explorer", label = paste0("Select a cell line"), choices = NULL, multiple = FALSE, options = list(
                        placeholder = 'Select cell line',
                        # you can search the data based on these fields
                        searchField = c('Synonyms'),
                        # the label that will be shown once value is selected
                        labelField= 'Harmonized_Cell_Line_ID',
                        # (each item is a row in data), which requires 'value' column (created by cbind at server side)
                         render = I("{
                                option: function(item, escape) {
                                  return '<div>' + escape(item.Harmonized_Cell_Line_ID) +'</div>';
                                }
                              }")
                    )),
                    checkboxInput("Cell_Line_Explorer_Show_Cell_Line_Filters", label = "Show cell line filters?", value = FALSE),
                    uiOutput(outputId = "Cell_Line_Explorer_Cell_Line_Filters"),
                  #Text display for information about this Cell_Line
                    uiOutput(outputId = "Cell_Line_Explorer_Text"),
                   #Selection of compounds to plot information for related to selected cell line
                    uiOutput(outputId = "Cell_Line_Explorer_Compound_Menu"),
                    uiOutput(outputId = "Cell_Line_Explorer_Compound_Filters")
                ), mainPanel(
                  selectInput(inputId = "Cell_Line_Explorer_to_Plot", "Select what values to plot", choices = c("AUC values for most commonly used concentration range", "AUC values for concentration range available for all cell lines", "IC50 values"), selected = "AUC values for most commonly used concentration range", multiple = FALSE, selectize = FALSE),
                  uiOutput(outputId = "Cell_Line_Explorer_Plots")
                ))
          ),
          
######################################################## UI #           
          tabPanel(value = "Plot Dose-Response Curves", title = "Plot Dose-Response Curves",
          #CSS formatting for button
            tags$head(
              tags$style(HTML('#Create_Compound_CCL_Plot{background-color:#6495ED;color:#FFFFFF;font-weight:bold;border-style:solid;border-color:black;border-width:medium}'))
            ),
          #Creating searchbars to select compound and cell line to plot
            wellPanel(
               fluidRow(
                column(width = 4, selectizeInput("plot_compound", paste0("Select a Compound (n = ", length(compound_ccl_availability), ")"), choices = names(compound_ccl_availability), selected = names(compound_ccl_availability)[1], multiple = FALSE, options = NULL)),
                column(width = 4, selectizeInput("plot_cell_line", paste0("Select a Cell Line (n = ", length(unique(unlist(compound_ccl_availability[[1]]))), ")"), choices = sort(unique(unlist(compound_ccl_availability[[1]]))), selected = NULL, multiple = FALSE, options = NULL))
              ),
            #Adding button to create plot
              fluidRow(
                column(width = 4, actionButton(inputId = "Create_Compound_CCL_Plot", label = "Plot Dose-Response Curves")),
                column(width = 8, checkboxInput(inputId = "Plot_Averages", label = "Plot Averaged Values (only uses experiments that passed QC)"))
              ) 
            ),
          #Display dose-response curve plot
            plotlyOutput(outputId = "Compound_CCL_Plot", height = "auto"),
          #Display table of experiments available for this dose-response curve
            DT::dataTableOutput(outputId = "Experiment_Table")
        )         
      ),

#############################################################          
######################################################## UI #           
        navbarMenu(title = "Calculate Custom Statistics",
                   
######################################################## UI #           
          tabPanel(value = "AUC Values", title = "AUC Values",
              wellPanel(
               fluidRow(
                column(width = 4, selectizeInput("cell_line_explorer", paste0("Select a Cell Line (n = ", length(ccl_compound_availability), ")"), choices = names(ccl_compound_availability), selected = names(ccl_compound_availability)[1], multiple = FALSE, options = NULL)),
              )),
              h1("PAGE UNDER CONSTRUCTION")
          ),

######################################################## UI #           
          tabPanel(value = "Viability Values", title = "Viability Values",
              wellPanel(
               fluidRow(
                column(width = 4, selectizeInput("cell_line_explorer", paste0("Select a Cell Line (n = ", length(ccl_compound_availability), ")"), choices = names(ccl_compound_availability), selected = names(ccl_compound_availability)[1], multiple = FALSE, options = NULL)),
              )),
              h1("PAGE UNDER CONSTRUCTION")
          )
        ),

#############################################################          
######################################################## UI #           
      tabPanel(value = "Download Data", title = "Download Data",
                 
          h1("PAGE UNDER CONSTRUCTION")
        
        )
        
        
  )
  
#############################################################  
#################################################### server #          
#############################################################  
#Creating server function
  server <- function(input, output, session){
    #Observing help menu objects
      observe_helpers()
    
    #Creating .5 sec timer for periodic updates
      half_sec_timer <- reactiveTimer(500)

    #Setting code to only run for each tab once that tab is selected
      observeEvent(input$tabs, {
        
#############################################################          
#################################################### server #          
        if(input$tabs == "Welcome"){
        #Code for "Welcome" tab
        
          #Summary text for dataset
            output$Dataset_Summary_Text <- renderUI({
              selected_data_summary <- Dataset_Summaries[Dataset_Summaries$Dataset %in% input$Summary_Dataset,]
              list(
                p(tags$b("Version: "), tags$em(selected_data_summary$Dataset_Version)),
                p(tags$b("Location: "), tags$em(selected_data_summary$Testing_Location)),
                p(tags$b("Dates: "), tags$em(selected_data_summary$Experiment_Dates)),
                p(tags$b("Assay Method: "), tags$em(selected_data_summary$Assay_Type)),
                p(tags$b("Plate Format: "), tags$em(selected_data_summary$Plate_Format)),
                p(tags$b("Treatment Duration: "), tags$em(selected_data_summary$Treatment_Duration)),
                p(tags$b("# of Compounds: "), tags$em(selected_data_summary$n_Compounds)),
                p(tags$b("# of Cell Lines: "), tags$em(selected_data_summary$n_Cell_Lines)),
                p(tags$b("# of Experiments: "), tags$em(selected_data_summary$n_Experiments)) 
              )
            })
          #Plot of # of compounds tested per cell line in dataset
            output$n_Compounds_Per_Cell_Line <- renderPlotly({
              selected_n_cpds_per_ccl <- Dataset_cpds_per_ccl[[input$Summary_Dataset]]
              plot_data <- as.data.frame.table(table(selected_n_cpds_per_ccl$x), stringsAsFactors = FALSE)
              plot_data$Var1 <- as.numeric(plot_data$Var1)
              if(nrow(plot_data) > 1){
                plot_range <- min(plot_data$Var1):max(plot_data$Var1)
              } else {
                plot_range <- (plot_data$Var1-5):(plot_data$Var1+5)
              }
              missing_values <-  plot_range[! plot_range %in% plot_data$Var1]
              extra_data <- cbind(missing_values, 0)
              colnames(extra_data) <- colnames(plot_data)
              plot_data <- rbind(plot_data, extra_data)
              plot_data <- plot_data[order(plot_data$Var1, decreasing = FALSE),]
              
              
              fig <- plot_ly(x = plot_data$Var1, y = plot_data$Freq, type = "bar")
              fig <- layout(fig,
                            xaxis = list(title = paste("# of Compounds Tested per Cell Line")),
                            yaxis = list(title = paste("Cell Line Count")))
              fig
            })
          #Plot of # of cell lines tested per compound
            output$n_Cell_Lines_Per_Compound <- renderPlotly({
              selected_n_ccls_per_cpd <- Dataset_ccls_per_cpd[[input$Summary_Dataset]]
              plot_data <- as.data.frame.table(table(selected_n_ccls_per_cpd$x), stringsAsFactors = FALSE)
              plot_data$Var1 <- as.numeric(plot_data$Var1)
              if(nrow(plot_data) > 1){
                plot_range <- min(plot_data$Var1):max(plot_data$Var1)
              } else {
                plot_range <- (plot_data$Var1-5):(plot_data$Var1+5)
              }
              missing_values <-  plot_range[! plot_range %in% plot_data$Var1]
              extra_data <- cbind(missing_values, 0)
              colnames(extra_data) <- colnames(plot_data)
              plot_data <- rbind(plot_data, extra_data)
              plot_data <- plot_data[order(plot_data$Var1, decreasing = FALSE),]
              
              
              fig <- plot_ly(x = plot_data$Var1, y = plot_data$Freq, type = "bar")
              fig <- layout(fig,
                            xaxis = list(title = paste("# of Cell Lines Tested per Compound")),
                            yaxis = list(title = paste("Compound Count")))
              fig
            })
          #Plot of residual standard errors
            output$Dataset_Residual_Standard_Error <- renderPlotly({
              RSE_plot_data <- data.frame(Dataset_rse[[input$Summary_Dataset]])
              colnames(RSE_plot_data) <- "RSE"
              p <- ggplot(RSE_plot_data, aes(x = RSE)) +
                          geom_density(color = "darkblue", fill = "lightblue") +
                          theme_light() +
                          labs(x = "Residual Standard Error for Dose-Response Curves", y = "Density")
              fig <- ggplotly(p)
              fig
            })
            
#############################################################          
#################################################### server #          
        }
        
        if(input$tabs == "Explore Compounds"){
          #Code for "Explore Compounds" Tab
          
            #Defining compound filter interface
              observeEvent(input$Compound_Explorer_Show_Compound_Filters, {
                output$Compound_Explorer_Compound_Filters <- renderUI({
                  if(input$Compound_Explorer_Show_Compound_Filters == TRUE){
                    return(list(
                      h4("Filter compounds by:") %>%
                          helper(type = "inline",
                            title = "Compound filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("These options can be used to filter the compound options displayed in the \"Select a compound\" menu. Selecting any filter options will limit the displayed compounds to those which meet all selected filtering options. Note that any compounds which are missing information for a selected filtering criteria will be excluded once any selection from that criteria has been made."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                      wellPanel(list(
                        pickerInput(inputId = "Compound_Explorer_Molecular_Target", label = "Filter compounds by molecular target", choices = Unique_Compound_Molecular_Targets, selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")),
                        pickerInput(inputId = "Compound_Explorer_MOA", label = "Filter compounds by free-text MOA", choices = Unique_Compound_MOAs, selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")),
                        pickerInput(inputId = "Compound_Explorer_Clinical_Phase", label = "Filter compounds by clinical phase", choices = sort(unique(Simple_Compound_Harm$Compound_Clinical_Phase)), selected = NULL, multiple = TRUE, options = list(`selected-Text-Format` = "count", `none-Selected-Text` = "Optional", `max-options` = 1))
                      ))
                    ))
                  } else {
                    return(list(p(" ")))
                  }
                })
              })
                
            #Updating compound filter interface
              #Defining currently available compounds based on compound filters
                Compound_Explorer_Currently_Available_Compounds <- reactive({
                  
                  Avail_compounds <- sort(unique(Simple_Compound_Harm$Harmonized_Compound_Name))
                  Filtered_compounds <- character(0)
                  flag <- 0
                  
                  
                  if(! is.null(input$Compound_Explorer_Molecular_Target)){
                    flag <- 1
                    Filtered_compounds <- Avail_compounds[Avail_compounds %in% names(Compound_Molecular_Targets)[sapply(Compound_Molecular_Targets, function(x,y){all(y %in% x)}, y = input$Compound_Explorer_Molecular_Target)]]
                  }
                  
                  if(! is.null(input$Compound_Explorer_MOA)){
                    if(flag == 0){
                      Filtered_compounds <- Avail_compounds[Avail_compounds %in% names(Compound_MOAs)[sapply(Compound_MOAs, function(x,y){all(y %in% x)}, y = input$Compound_Explorer_MOA)]]
                    } else if(flag == 1){
                      Filtered_compounds <- Filtered_compounds[Filtered_compounds %in% names(Compound_MOAs)[sapply(Compound_MOAs, function(x,y){all(y %in% x)}, y = input$Compound_Explorer_MOA)]]
                    }
                    flag <- 1
                  }
                  
                  if(! is.null(input$Compound_Explorer_Clinical_Phase)){
                    if(flag == 0){
                      Filtered_compounds <- Avail_compounds[Avail_compounds %in% Simple_Compound_Harm$Harmonized_Compound_Name[Simple_Compound_Harm$Compound_Clinical_Phase %in% input$Compound_Explorer_Clinical_Phase]]
                    } else if(flag == 1){
                      Filtered_compounds <- Filtered_compounds[Filtered_compounds %in% Simple_Compound_Harm$Harmonized_Compound_Name[Simple_Compound_Harm$Compound_Clinical_Phase %in% input$Compound_Explorer_Clinical_Phase]]
                    }
                    flag <- 1
                  }
                  
                  if(flag == 0){
                    return(Avail_compounds)
                  } else if(flag == 1){
                    return(Filtered_compounds)
                  }
                })
              
              #Updating compound filter input menus
                observeEvent(Compound_Explorer_Currently_Available_Compounds(), {
                  #Updating compound filter menus
                    #Molecular target menu
                      Compound_Explorer_Available_Molecular_Targets <- sort(unique(unlist(Compound_Molecular_Targets[names(Compound_Molecular_Targets) %in% Compound_Explorer_Currently_Available_Compounds()])))
                      previous_selection <- input$Compound_Explorer_Molecular_Target
                      isolate({
                        updatePickerInput(session, "Compound_Explorer_Molecular_Target", choices = Compound_Explorer_Available_Molecular_Targets, selected = previous_selection)
                      })
                    #MOA menu
                      Compound_Explorer_Available_MOAs <- sort(unique(unlist(Compound_MOAs[names(Compound_MOAs) %in% Compound_Explorer_Currently_Available_Compounds()])))
                      previous_selection <- input$Compound_Explorer_MOA
                      isolate({
                        updatePickerInput(session, "Compound_Explorer_MOA", choices = Compound_Explorer_Available_MOAs, selected = previous_selection)
                      })
                    #Clinical stage menu
                      Compound_Explorer_Available_Clinical_Phases <- sort(unique(Simple_Compound_Harm$Compound_Clinical_Phase[Simple_Compound_Harm$Harmonized_Compound_Name %in% Compound_Explorer_Currently_Available_Compounds()]))
                      previous_selection <- input$Compound_Explorer_Clinical_Phase
                      isolate({
                        updatePickerInput(session, "Compound_Explorer_Clinical_Phase", choices = Compound_Explorer_Available_Clinical_Phases, selected = previous_selection)
                      })
                })
                
              #Updating compound selection menu
                Selected_Compound_Option_df <- reactive({
                  filtered_Compound_Option_df <- Compound_Option_df[Compound_Option_df$Harmonized_Compound_Name %in% Compound_Explorer_Currently_Available_Compounds(),]
                  return(cbind(filtered_Compound_Option_df, value = filtered_Compound_Option_df$Harmonized_Compound_Name))
                })
                
                observeEvent(Selected_Compound_Option_df(),{
                  prev_selection <- input$compound_explorer
                  if(! length(prev_selection) == 0){
                    if(! prev_selection %in% Selected_Compound_Option_df()$Harmonized_Compound_Name){
                      prev_selection <- character(0)
                    }
                  }
                  updateSelectizeInput(session, "compound_explorer", label = paste0("Select a compound (n = ", nrow(Selected_Compound_Option_df()), ")"), choices = Selected_Compound_Option_df(), server = TRUE, selected = prev_selection)
                })
                
                observeEvent(input$compound_explorer, ignoreInit = TRUE, eventExpr =  {
                  if(! input$compound_explorer == ""){
                    #Loading data for selected compound
                      filename <- Compound_Filenames$drug_file_names[Compound_Filenames$drugs == input$compound_explorer]
                      data <- readRDS(paste0("./www/Results/", filename, ".rds"))
                      for(i in 1:length(data)){
                        data[[i]] <- data[[i]][! is.na(data[[i]]$b_c_d_e),]
                      }
                      cpdexp_data <- reactive({data})

                    #Defining cell lines available for selected compound
                      Compound_Explorer_Available_Cell_Lines_for_Compound <- reactive({
                        cell_lines <- character(0)
                        for(i in 1:length(cpdexp_data())){
                          cell_lines <- c(cell_lines, cpdexp_data()[[i]]$Cell_Line)
                        }
                        return(sort(unique(cell_lines)))
                      })

                    #Subsetting cell line info to cell lines available for this compound
                      Compound_Explorer_Simple_Cell_Line_Harm_for_Compound <- reactive({
                        Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% Compound_Explorer_Available_Cell_Lines_for_Compound(),]
                      })

                    #Getting age limits
                      Compound_Explorer_Age_Limits <- reactive({
                        if(! is.null(input$compound_explorer)){
                          Temp_CL_Harm_Data <- Compound_Explorer_Simple_Cell_Line_Harm_for_Compound()
                          temp_min <- suppressWarnings(min(Temp_CL_Harm_Data$Numeric_Age_in_Years, na.rm = TRUE))
                          if(temp_min == Inf){
                            temp_min <- 0
                          }
                          temp_max <- suppressWarnings(max(Temp_CL_Harm_Data$Numeric_Age_in_Years, na.rm = TRUE))
                          if(temp_max == -Inf){
                            temp_max <- 0
                          }
                          return(c(temp_min, temp_max))
                        } else {
                          return(c(0,0))
                        }
                      })

                  #Defining cell line interface if compound has been selected
                    if(input$compound_explorer %in% Simple_Compound_Harm$Harmonized_Compound_Name){

                      #Defining cell line selection interface
                        output$Compound_Explorer_Cell_Line_Menu <- renderUI({
                          Temp_CL_Harm_Data <- Compound_Explorer_Simple_Cell_Line_Harm_for_Compound()
                            temp_Cell_Line_Diseases <- strsplit(Temp_CL_Harm_Data$Diseases, ":\\|:")
                            names(temp_Cell_Line_Diseases) <- Temp_CL_Harm_Data$Harmonized_Cell_Line_ID

                          return_list <- list(
                            pickerInput("Compound_Explorer_Cell_Lines", label = "Select cell lines to plot data for", choices = sort(unique(Temp_CL_Harm_Data$Harmonized_Cell_Line_ID)), selected = sort(unique(Temp_CL_Harm_Data$Harmonized_Cell_Line_ID)), multiple = TRUE, options = list(
                              `actions-Box` = TRUE,
                              `live-Search-Style` = "contains" ,
                              `live-Search` = TRUE,
                              `live-Search-Normalize` = TRUE,
                              `selected-Text-Format` = "count"
                            )),
                            checkboxInput("Compound_Explorer_Show_Cell_Line_Filters", label = "Show cell line filters?", value = FALSE)
                          )

                          return(return_list)

                        })

                      #Defining cell line filter interface
                        output$Compound_Explorer_Cell_Line_Filters <- renderUI({
                          req(! length(input$Compound_Explorer_Show_Cell_Line_Filters) == 0)
                          Temp_CL_Harm_Data <- Compound_Explorer_Simple_Cell_Line_Harm_for_Compound()
                            Cell_Line_Diseases <- strsplit(Temp_CL_Harm_Data$Diseases, ":\\|:")
                            names(Cell_Line_Diseases) <- Temp_CL_Harm_Data$Harmonized_Cell_Line_ID
                          if(input$Compound_Explorer_Show_Cell_Line_Filters == TRUE){
                            return_list <- list(
                              h4("Filter cell lines by:") %>%
                              helper(type = "inline",
                                title = "Cell line filtering",
                                icon = "question-circle", colour = NULL,
                                content = c("These options can be used to filter the cell line options displayed in the \"Select cell lines to plot data for\" menu. Note that, once any options have been selected for a given filtering criteria, any cell lines that are missing information for that criteria will be excluded."),
                                size = "m",
                                buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                              ),
                              wellPanel(list(
                                pickerInput(inputId = "Compound_Explorer_Cancer_Type", label = "General cancer type", choices = sort(unique(Temp_CL_Harm_Data$Simple_Cancer_Type)), multiple = TRUE, options = list(`actions-Box` = TRUE, `live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                                helper(type = "inline",
                                  title = "General cancer type filtering",
                                  icon = "question-circle", colour = NULL,
                                  content = c("Selecting any cancer type options will limit displayed cell line options to only include cell lines from the selected cancer types. If any options have been selected from the \"Free-text disease name\" menu, cell lines will also be displayed that meet at least one of the disease name criteria selected in that menu."),
                                  size = "m",
                                  buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                ),
                                pickerInput(inputId = "Compound_Explorer_Disease_Name", label = "Free-text disease name", choices = sort(unique(unlist(Cell_Line_Diseases))), multiple = TRUE, options = list(`actions-Box` = TRUE, `live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                                helper(type = "inline",
                                  title = "Free-text disease name filtering",
                                  icon = "question-circle", colour = NULL,
                                  content = c("Selecting any disease name options will limit displayed cell line options to only include cell lines from the selected diseases. If any options have been selected from the \"General cancer type\" menu, cell lines will also be displayed that meet at least one of the cancer type criteria selected in that menu."),
                                  size = "m",
                                  buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                ),
                                pickerInput(inputId = "Compound_Explorer_Gender", label = "Gender", choices = sort(unique(Temp_CL_Harm_Data$Gender)), multiple = TRUE, options = list(`selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                                helper(type = "inline",
                                  title = "Gender filtering",
                                  icon = "question-circle", colour = NULL,
                                  content = c("Selecting any gender options will limit the displayed cell line options to only include cell lines of the selected gender(s)."),
                                  size = "m",
                                  buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                ),

                                sliderInput(inputId = "Compound_Explorer_Age", label = "Patient age (years)", min = Compound_Explorer_Age_Limits()[1], max = Compound_Explorer_Age_Limits()[2], value = Compound_Explorer_Age_Limits(), ticks = FALSE) %>%
                                helper(type = "inline",
                                  title = "Patient age filtering",
                                  icon = "question-circle", colour = NULL,
                                  content = c("Sets minimum and maximum age allowed for displayed cell line options. Note that setting the slider to anything other than its maximum range will exclude cell lines for which a numeric age could not be determined for the patient who the cell line was derived from at the time of sample collection. This both includes cases where the age is unspecified and cases where the specified age is ambiguous (i.e. such as \"Adult\"). You may download the cell line harmonization file in the \"Download Data\" tab for free-text descriptions of each cell line's patient age."),
                                  size = "m",
                                  buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                ),

                                h4("Ancestry") %>%
                                helper(type = "inline",
                                  title = "Patient age filtering",
                                  icon = "question-circle", colour = NULL,
                                  content = c("Each slider sets the minimum and maximum % ancestry for each ancestry group. Once any of the ancestry sliders has been changed from its maximum range, cell lines will be filtered to only include lines which meet the limits set on all of the ancestry sliders. Note that % ancestry adds to 100% across all ancestry groups for each individual cell line (i.e. a cell line cannot have 60% African ancestry and 60% Native American ancestry, because that would add to >100%). Ancestry information was obtained from the cellosaurus resource at https://www.expasy.org/."),
                                  size = "m",
                                  buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                ),
                                wellPanel(list(
                                  sliderInput(inputId = "Compound_Explorer_African", label = "% african ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                                  sliderInput(inputId = "Compound_Explorer_Native_American", label = "% native american ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                                  sliderInput(inputId = "Compound_Explorer_East_Asian_North", label = "% east asian (north) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                                  sliderInput(inputId = "Compound_Explorer_East_Asian_South", label = "% east asian (south) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                                  sliderInput(inputId = "Compound_Explorer_South_Asian", label = "% south asian ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                                  sliderInput(inputId = "Compound_Explorer_European_North", label = "% european (north) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                                  sliderInput(inputId = "Compound_Explorer_European_South", label = "% european (south) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE)
                                ))
                              ))
                            )
                            return(return_list)
                          } else {
                            return(list(" "))
                          }
                        })

                      #Updating cell line filter interface
                        #Defining currently available cell lines based on cell line filters
                          Compound_Explorer_Currently_Available_Cell_Lines <- reactive({

                            Temp_CL_Harm_Data <- Compound_Explorer_Simple_Cell_Line_Harm_for_Compound()
                              temp_Cell_Line_Diseases <- strsplit(Temp_CL_Harm_Data$Diseases, ":\\|:")
                              names(temp_Cell_Line_Diseases) <- Temp_CL_Harm_Data$Harmonized_Cell_Line_ID

                            Avail_ccls <- Compound_Explorer_Available_Cell_Lines_for_Compound()
                            Filtered_ccls <- character(0)
                            flag <- 0

                            if(! is.null(input$Compound_Explorer_Cancer_Type)){
                              flag <- 1
                              Filtered_ccls <- Compound_Explorer_Available_Cell_Lines_for_Compound()[Compound_Explorer_Available_Cell_Lines_for_Compound() %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Simple_Cancer_Type %in% input$Compound_Explorer_Cancer_Type]]
                            }

                            if(! is.null(input$Compound_Explorer_Disease_Name)){
                              flag <- 1
                              Filtered_ccls <- sort(unique(c(Filtered_ccls, Avail_ccls[Avail_ccls %in% names(temp_Cell_Line_Diseases)[sapply(temp_Cell_Line_Diseases, function(x,y){return(any(y %in% x))}, y = input$Compound_Explorer_Disease_Name)]])))
                            }

                            if(! is.null(input$Compound_Explorer_Gender)){
                              if(flag == 0){
                                Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Gender %in% input$Compound_Explorer_Gender]]))
                              } else if(flag == 1){
                                Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Gender %in% input$Compound_Explorer_Gender]]]))
                              }
                              flag <- 1
                            }

                            if(! all(input$Compound_Explorer_Age == Compound_Explorer_Age_Limits())){
                              if(flag == 0){
                                Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Numeric_Age_in_Years >= input$Compound_Explorer_Age[1] &
                                                                                                                     Temp_CL_Harm_Data$Numeric_Age_in_Years <= input$Compound_Explorer_Age[2]]]))
                              } else if(flag == 1){
                                Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Numeric_Age_in_Years >= input$Compound_Explorer_Age[1] &
                                                                                                                                                                  Temp_CL_Harm_Data$Numeric_Age_in_Years <= input$Compound_Explorer_Age[2]]]]))
                              }
                              flag <- 1
                            }

                            if(! all(input$Compound_Explorer_African == c(0,100)) |
                               ! all(input$Compound_Explorer_Native_American == c(0,100)) |
                               ! all(input$Compound_Explorer_East_Asian_North == c(0,100)) |
                               ! all(input$Compound_Explorer_East_Asian_South == c(0,100)) |
                               ! all(input$Compound_Explorer_South_Asian == c(0,100)) |
                               ! all(input$Compound_Explorer_European_North == c(0,100)) |
                               ! all(input$Compound_Explorer_European_South == c(0,100))){
                              if(flag == 0){
                                Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$African_Ancestry >= input$Compound_Explorer_African[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$African_Ancestry <= input$Compound_Explorer_African[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$Native_American_Ancestry >= input$Compound_Explorer_Native_American[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$Native_American_Ancestry <= input$Compound_Explorer_Native_American[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` >= input$Compound_Explorer_East_Asian_North[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` <= input$Compound_Explorer_East_Asian_North[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` >= input$Compound_Explorer_East_Asian_South[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` <= input$Compound_Explorer_East_Asian_South[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$South_Asian_Ancestry >= input$Compound_Explorer_South_Asian[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$South_Asian_Ancestry <= input$Compound_Explorer_South_Asian[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$`European_(North)_Ancestry` >= input$Compound_Explorer_European_North[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$`European_(North)_Ancestry` <= input$Compound_Explorer_European_North[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$`European_(South)_Ancestry` >= input$Compound_Explorer_European_South[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$`European_(South)_Ancestry` <= input$Compound_Explorer_European_South[2]/100]]))
                              } else if(flag == 1){
                                Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$African_Ancestry >= input$Compound_Explorer_African[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$African_Ancestry <= input$Compound_Explorer_African[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$Native_American_Ancestry >= input$Compound_Explorer_Native_American[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$Native_American_Ancestry <= input$Compound_Explorer_Native_American[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` >= input$Compound_Explorer_East_Asian_North[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` <= input$Compound_Explorer_East_Asian_North[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` >= input$Compound_Explorer_East_Asian_South[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` <= input$Compound_Explorer_East_Asian_South[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$South_Asian_Ancestry >= input$Compound_Explorer_South_Asian[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$South_Asian_Ancestry <= input$Compound_Explorer_South_Asian[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$`European_(North)_Ancestry` >= input$Compound_Explorer_European_North[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$`European_(North)_Ancestry` <= input$Compound_Explorer_European_North[2]/100 &
                                                                                                                     Temp_CL_Harm_Data$`European_(South)_Ancestry` >= input$Compound_Explorer_European_South[1]/100 &
                                                                                                                     Temp_CL_Harm_Data$`European_(South)_Ancestry` <= input$Compound_Explorer_European_South[2]/100]]]))
                              }
                              flag <- 1
                            }

                            if(flag == 0){
                              return(Avail_ccls)
                            } else if(flag == 1){
                              return(Filtered_ccls)
                            }
                          })

                        #Updating selection menus based on Compound_Explorer_Currently_Available_Cell_Lines()
                          observeEvent(Compound_Explorer_Currently_Available_Cell_Lines(), {
                            #Updating cell line selection menu
                              updatePickerInput(session, "Compound_Explorer_Cell_Lines", label = paste0("Select cell lines to plot data for (n = ", length(Compound_Explorer_Currently_Available_Cell_Lines()), ")"), choices = Compound_Explorer_Currently_Available_Cell_Lines(), selected = Compound_Explorer_Currently_Available_Cell_Lines())
                          })

                        #Determining which datasets contain data for the selected compound
                          cpdexplr_ccl_availability_data <- reactive({
                            temp_cpdexplr_ccl_availability_data <- lapply(cpdexp_data(), function(x){return(sort(unique(x$Cell_Line)))})
                            for(i in 1:length(temp_cpdexplr_ccl_availability_data)){
                              temp_cpdexplr_ccl_availability_data[[i]] <- temp_cpdexplr_ccl_availability_data[[i]][temp_cpdexplr_ccl_availability_data[[i]] %in% input$Compound_Explorer_Cell_Lines]
                            }
                            return(temp_cpdexplr_ccl_availability_data)
                          })

                          datasets_with_compound_data <- reactive({
                            temp_cpdexplr_ccl_availability_data <-  cpdexplr_ccl_availability_data()
                            return(names(temp_cpdexplr_ccl_availability_data)[sapply(temp_cpdexplr_ccl_availability_data, length) > 0])
                          })

                          Compound_Explorer_Data <- reactive({
                            temp_cpdexplr_data <- Compound_Harm[Compound_Harm$Harmonized_Compound_Name %in% input$compound_explorer,]
                            return(temp_cpdexplr_data[temp_cpdexplr_data$Dataset %in% datasets_with_compound_data(),])
                          })

                        #Displaying summary data for the selected compound
                          output$Compound_Explorer_Text <- renderUI({
                            req(Compound_Explorer_Data())
                            synonyms <- paste(clean_vector(unlist(strsplit(Compound_Explorer_Data()$Compound_Synonyms, ":\\|:"))), collapse = "; ")
                            related <- paste(clean_vector(unlist(strsplit(Compound_Explorer_Data()$Closely_Related_Compounds, ":\\|:"))), collapse = "; ")
                            CID <- paste(clean_vector(unlist(strsplit(Compound_Explorer_Data()$Pubchem_CID, ":\\|:"))), collapse = "; ")
                            Source <- paste(clean_vector(unlist(strsplit(Compound_Explorer_Data()$Compound_Source, ":\\|:"))), collapse = "; ")
                            Molecular_Targets <- paste(clean_vector(unlist(strsplit(Compound_Explorer_Data()$Compound_Molecular_Targets, ":\\|:"))), collapse = "; ")
                            MOA <- paste(clean_vector(unlist(strsplit(Compound_Explorer_Data()$Compound_MOA, ":\\|:"))), collapse = ", ")
                            clinical_phase <- paste(clean_vector(unlist(strsplit(Compound_Explorer_Data()$Compound_Clinical_Phase, ":\\|:"))), collapse = "; ")

                            list(
                              tableOutput(outputId = "Compound_Explorer_Name_in_Dataset"),
                              p(tags$b("Pubchem CID: "), p(tags$em(CID))),
                              p(tags$b("Synonyms: "), p(tags$em(synonyms))),
                              p(tags$b("Closely Related Compounds:"), p(tags$em(related))),
                              p(tags$b("Clinical Phase: "), p(tags$em(clinical_phase))),
                              p(tags$b("Sources: "), p(tags$em(Source))),
                              p(tags$b("Mechanisms of Action: "), p(tags$em(MOA))),
                              p(tags$b("Molecular Targets: "), p(tags$em(Molecular_Targets)))
                            )
                          })

                        output$Compound_Explorer_Name_in_Dataset <- renderTable({
                          #Creating table with the name used for this compound in each dataset
                            explorer_name_table <- Compound_Explorer_Data()[,c("Dataset", "Compound_Name_in_Dataset")]
                            colnames(explorer_name_table) <- c("Dataset", "Name in Dataset")
                            return(explorer_name_table)
                        })


                    } else { #END: if(input$compound_explorer %in% Simple_Compound_Harm$Harmonized_Compound_Name)
                      output$Compound_Explorer_Cell_Line_Menu <- renderUI({
                        list(p(" "))
                      })
                      output$Compound_Explorer_Cell_Line_Filters <- renderUI({
                        list(p(" "))
                      })
                    }
                      
                  #Generating plots
                    output$Compound_Explorer_Plots <- renderUI({
                      #Rendering plot UI for dataset with data for this compound
                        Compound_Explorer_Plot_UI <- vector(mode = "list")
                        if("CTRPv2" %in% datasets_with_compound_data()){
                          Compound_Explorer_Plot_UI <- c(Compound_Explorer_Plot_UI, plotlyOutput(outputId = "Compound_Explorer_CTRPv2_Plot"))
                        }
                        if("GDSC1" %in% datasets_with_compound_data()){
                          Compound_Explorer_Plot_UI <- c(Compound_Explorer_Plot_UI, plotlyOutput(outputId = "Compound_Explorer_GDSC1_Plot"))
                        }
                        if("GDSC2" %in% datasets_with_compound_data()){
                          Compound_Explorer_Plot_UI <- c(Compound_Explorer_Plot_UI, plotlyOutput(outputId = "Compound_Explorer_GDSC2_Plot"))
                        }
                        if("PRISM_Repurposing" %in% datasets_with_compound_data()){
                          Compound_Explorer_Plot_UI <- c(Compound_Explorer_Plot_UI, plotlyOutput(outputId = "Compound_Explorer_PRISM_Repurposing_Plot"))
                        }
                      Compound_Explorer_Plot_UI
                    })
    
                    output$Compound_Explorer_CTRPv2_Plot <- renderPlotly({
                      #Loading raw data for this compound and any datasets with data for this compound
                        if("CTRPv2" %in% datasets_with_compound_data()){
                          CTRPv2_Results <- cpdexp_data()$CTRPv2
                          CTRPv2_Results <- CTRPv2_Results[! is.na(CTRPv2_Results$b_c_d_e) & CTRPv2_Results$Cell_Line %in% cpdexplr_ccl_availability_data()$CTRPv2,]
                        } else {
                          CTRPv2_Results <- data.frame(NULL)
                        }
                    #Making compound explorer CTRPv2 plot
                      if(nrow(CTRPv2_Results) > 0){
                        if(input$Compound_Explorer_to_Plot == "AUC values for most commonly used concentration range"){
                          plot_data <- CTRPv2_Results[! is.na(CTRPv2_Results$AUC_mode_ccl_CTRPv2_conc),]
                          plot_data <- plot_data[order(plot_data$AUC_mode_ccl_CTRPv2_conc, decreasing = FALSE),]
                          plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_CTRPv2_conc] <- "Max Tested Concentration < AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                          ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_mode_ccl_CTRPv2_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_mode_ccl_CTRPv2_conc)), 3), " microMolar)")
                          fig <- plot_ly(x = plot_data$Cell_Line,
                                         y = plot_data$AUC_mode_ccl_CTRPv2_conc,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colorRampPalette(c("blue", "red"))(2),
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("CTRPv2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Compound_Explorer_to_Plot == "AUC values for concentration range available for all cell lines"){
                          plot_data <- CTRPv2_Results[! is.na(CTRPv2_Results$AUC_all_ccl_CTRPv2_conc),]
                          plot_data <- plot_data[order(plot_data$AUC_all_ccl_CTRPv2_conc, decreasing = FALSE),]
                          plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                          ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_all_ccl_CTRPv2_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_all_ccl_CTRPv2_conc)), 3), " microMolar)")
                          fig <- plot_ly(x = plot_data$Cell_Line,
                                         y = plot_data$AUC_all_ccl_CTRPv2_conc,
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("CTRPv2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Compound_Explorer_to_Plot == "IC50 values"){
                          plot_data <- CTRPv2_Results[! is.na(CTRPv2_Results$IC50),]
                          if(nrow(plot_data) > 0){
                            plot_data <- plot_data[order(plot_data$IC50, decreasing = FALSE),]
                            plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                            ylab <- "IC50 (microMolar)"
                            fig <- plot_ly(x = plot_data$Cell_Line,
                                           y = plot_data$IC50,
                                           type = "scatter",
                                           mode = "markers")
                            fig <- layout(fig,
                                          xaxis = list(paste0("CTRPv2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab, type = "log"))
                            fig
                          }
                        }
                      }
                    })
    
    
                    output$Compound_Explorer_GDSC1_Plot <- renderPlotly({
                      #Loading raw data for this compound and any datasets with data for this compound
                        if("GDSC1" %in% datasets_with_compound_data()){
                          GDSC1_Results <- cpdexp_data()$GDSC1
                          GDSC1_Results <- GDSC1_Results[! is.na(GDSC1_Results$b_c_d_e) & GDSC1_Results$Cell_Line %in% cpdexplr_ccl_availability_data()$GDSC1,]
                        } else {
                          GDSC1_Results <- data.frame(NULL)
                        }
                    #Making compound explorer GDSC1 plot
                      if(nrow(GDSC1_Results) > 0){
                        if(input$Compound_Explorer_to_Plot == "AUC values for most commonly used concentration range"){
                          plot_data <- GDSC1_Results[! is.na(GDSC1_Results$AUC_mode_ccl_GDSC1_conc),]
                          plot_data <- plot_data[order(plot_data$AUC_mode_ccl_GDSC1_conc, decreasing = FALSE),]
                          plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_GDSC1_conc] <- "Max Tested Concentration < AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                          ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_mode_ccl_GDSC1_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_mode_ccl_GDSC1_conc)), 3), " microMolar)")
                          fig <- plot_ly(x = plot_data$Cell_Line,
                                         y = plot_data$AUC_mode_ccl_GDSC1_conc,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colorRampPalette(c("blue", "red"))(2),
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("GDSC1 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Compound_Explorer_to_Plot == "AUC values for concentration range available for all cell lines"){
                          plot_data <- GDSC1_Results[! is.na(GDSC1_Results$AUC_all_ccl_GDSC1_conc),]
                          plot_data <- plot_data[order(plot_data$AUC_all_ccl_GDSC1_conc, decreasing = FALSE),]
                          plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                          ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_all_ccl_GDSC1_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_all_ccl_GDSC1_conc)), 3), " microMolar)")
                          fig <- plot_ly(x = plot_data$Cell_Line,
                                         y = plot_data$AUC_all_ccl_GDSC1_conc,
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("GDSC1 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Compound_Explorer_to_Plot == "IC50 values"){
                          plot_data <- GDSC1_Results[! is.na(GDSC1_Results$IC50),]
                          if(nrow(plot_data) > 0){
                            plot_data <- plot_data[order(plot_data$IC50, decreasing = FALSE),]
                            plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                            ylab <- "IC50 (microMolar)"
                            fig <- plot_ly(x = plot_data$Cell_Line,
                                           y = plot_data$IC50,
                                           type = "scatter",
                                           mode = "markers")
                            fig <- layout(fig,
                                          xaxis = list(title = paste0("GDSC1 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab, type = "log"))
                            fig
                          }
                        }
                      }
                    })
    
                    output$Compound_Explorer_GDSC2_Plot <- renderPlotly({
                      #Loading raw data for this compound and any datasets with data for this compound
                        if("GDSC2" %in% datasets_with_compound_data()){
                          GDSC2_Results <- cpdexp_data()$GDSC2
                          GDSC2_Results <- GDSC2_Results[! is.na(GDSC2_Results$b_c_d_e) & GDSC2_Results$Cell_Line %in% cpdexplr_ccl_availability_data()$GDSC2,]
                        } else {
                          GDSC2_Results <- data.frame(NULL)
                        }
                    #Making compound explorer GDSC2 plot
                      if(nrow(GDSC2_Results) > 0){
                        if(input$Compound_Explorer_to_Plot == "AUC values for most commonly used concentration range"){
                          plot_data <- GDSC2_Results[! is.na(GDSC2_Results$AUC_mode_ccl_GDSC2_conc),]
                          plot_data <- plot_data[order(plot_data$AUC_mode_ccl_GDSC2_conc, decreasing = FALSE),]
                          plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_GDSC2_conc] <- "Max Tested Concentration < AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                          ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_mode_ccl_GDSC2_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_mode_ccl_GDSC2_conc)), 3), " microMolar)")
                          fig <- plot_ly(x = plot_data$Cell_Line,
                                         y = plot_data$AUC_mode_ccl_GDSC2_conc,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colorRampPalette(c("blue", "red"))(2),
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("GDSC2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Compound_Explorer_to_Plot == "AUC values for concentration range available for all cell lines"){
                          plot_data <- GDSC2_Results[! is.na(GDSC2_Results$AUC_all_ccl_GDSC2_conc),]
                          plot_data <- plot_data[order(plot_data$AUC_all_ccl_GDSC2_conc, decreasing = FALSE),]
                          plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                          ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_all_ccl_GDSC2_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_all_ccl_GDSC2_conc)), 3), " microMolar)")
                          fig <- plot_ly(x = plot_data$Cell_Line,
                                         y = plot_data$AUC_all_ccl_GDSC2_conc,
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("GDSC2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Compound_Explorer_to_Plot == "IC50 values"){
                          plot_data <- GDSC2_Results[! is.na(GDSC2_Results$IC50),]
                          if(nrow(plot_data) > 0){
                            plot_data <- plot_data[order(plot_data$IC50, decreasing = FALSE),]
                            plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                            ylab <- "IC50 (microMolar)"
                            fig <- plot_ly(x = plot_data$Cell_Line,
                                           y = plot_data$IC50,
                                           type = "scatter",
                                           mode = "markers")
                            fig <- layout(fig,
                                          xaxis = list(title = paste0("GDSC2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab, type = "log"))
                            fig
                          }
                        }
                      }
                    })
    
                    output$Compound_Explorer_PRISM_Repurposing_Plot <- renderPlotly({
                      #Loading raw data for this compound and any datasets with data for this compound
                        if("PRISM_Repurposing" %in% datasets_with_compound_data()){
                          PRISM_Repurposing_Results <- cpdexp_data()$PRISM_Repurposing
                          PRISM_Repurposing_Results <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$b_c_d_e) & PRISM_Repurposing_Results$Cell_Line %in% cpdexplr_ccl_availability_data()$PRISM_Repurposing,]
                        } else {
                          PRISM_Repurposing_Results <- data.frame(NULL)
                        }
                    #Making compound explorer PRISM_Repurposing plot
                      if(nrow(PRISM_Repurposing_Results) > 0){
                        if(input$Compound_Explorer_to_Plot == "AUC values for most commonly used concentration range"){
                          plot_data <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$AUC_mode_ccl_PRISM_Repurposing_conc),]
                          plot_data <- plot_data[order(plot_data$AUC_mode_ccl_PRISM_Repurposing_conc, decreasing = FALSE),]
                          plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_PRISM_Repurposing_conc] <- "Max Tested Concentration < AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                          ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_mode_ccl_PRISM_Repurposing_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_mode_ccl_PRISM_Repurposing_conc)), 3), " microMolar)")
                          fig <- plot_ly(x = plot_data$Cell_Line,
                                         y = plot_data$AUC_mode_ccl_PRISM_Repurposing_conc,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colorRampPalette(c("blue", "red"))(2),
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("PRISM-Repurposing Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Compound_Explorer_to_Plot == "AUC values for concentration range available for all cell lines"){
                          plot_data <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$AUC_all_ccl_PRISM_Repurposing_conc),]
                          plot_data <- plot_data[order(plot_data$AUC_all_ccl_PRISM_Repurposing_conc, decreasing = FALSE),]
                          plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                          ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_all_ccl_PRISM_Repurposing_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_all_ccl_PRISM_Repurposing_conc)), 3), " microMolar)")
                          fig <- plot_ly(x = plot_data$Cell_Line,
                                         y = plot_data$AUC_all_ccl_PRISM_Repurposing_conc,
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("PRISM-Repurposing Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Compound_Explorer_to_Plot == "IC50 values"){
                          plot_data <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$IC50),]
                          if(nrow(plot_data) > 0){
                            plot_data <- plot_data[order(plot_data$IC50, decreasing = FALSE),]
                            plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                            ylab <- "IC50 (microMolar)"
                            fig <- plot_ly(x = plot_data$Cell_Line,
                                           y = plot_data$IC50,
                                           type = "scatter",
                                           mode = "markers")
                            fig <- layout(fig,
                                          xaxis = list(title = paste0("PRISM-Repurposing Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab, type = "log"))
                            fig
                          }
                        }
                      }
                    })
                }  else { #END: if(! input$compound_explorer == ""){
                  output$Compound_Explorer_Text <- renderUI({})
                  output$Compound_Explorer_Cell_Line_Menu <- renderUI({})
                  output$Compound_Explorer_Cell_Line_Filters <- renderUI({})
                  output$Compound_Explorer_Plots <- renderUI({})
                }
              }) #observeEvent(input$compound_explorer, ignoreInit = TRUE, eventExpr =  {
                  
                
#############################################################          
#################################################### server #          
        }
        
        if(input$tabs == "Explore Cell Lines"){
          #Code for "Explore Cell Lines" Tab
          
            #Defining cell line filter interface
              observeEvent(input$Cell_Line_Explorer_Show_Cell_Line_Filters, {
                output$Cell_Line_Explorer_Cell_Line_Filters <- renderUI({
                  if(input$Cell_Line_Explorer_Show_Cell_Line_Filters == TRUE){
                    Cell_Line_Explorer_Initial_Age_Limits <- c(min(Simple_Cell_Line_Harm$Numeric_Age_in_Years, na.rm = TRUE), max(Simple_Cell_Line_Harm$Numeric_Age_in_Years, na.rm = TRUE))
                    return_list <- list(
                      h4("Filter cell lines by:") %>%
                      helper(type = "inline",
                        title = "Cell line filtering",
                        icon = "question-circle", colour = NULL,
                        content = c("These options can be used to filter the cell line options displayed in the \"Select a cell line\" menu. Only cell lines which meet all of the selected filter criteria will be displayed. Note that, once any options have been selected for a given filtering criteria, any cell lines that are missing information for that criteria will be excluded."),
                        size = "m",
                        buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                      ),
                      wellPanel(list(
                        pickerInput(inputId = "Cell_Line_Explorer_Cancer_Type", label = "General cancer type", choices = sort(unique(Simple_Cell_Line_Harm$Simple_Cancer_Type)), multiple = TRUE, options = list(`actions-Box` = TRUE, `live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional", `max-options` = 1)) %>%
                        helper(type = "inline",
                          title = "General cancer type filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Filters cell lines by a generally broad cancer type classification."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
                        pickerInput(inputId = "Cell_Line_Explorer_Disease_Name", label = "Free-text disease name", choices = sort(unique(unlist(Cell_Line_Diseases))), multiple = TRUE, options = list(`actions-Box` = TRUE, `live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                        helper(type = "inline",
                          title = "Free-text disease name filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Filters cell lines by free-text descriptions of their disease name. If multiple options are selected, only cell lines that are annotated with all of the selected options will be shown."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
                        pickerInput(inputId = "Cell_Line_Explorer_Gender", label = "Gender", choices = sort(unique(Simple_Cell_Line_Harm$Gender)), multiple = TRUE, options = list(`selected-Text-Format` = "count", `none-Selected-Text` = "Optional", `max-options` = 1)) %>%
                        helper(type = "inline",
                          title = "Gender filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Filters cell lines by annotated gender."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
    
                        sliderInput(inputId = "Cell_Line_Explorer_Age", label = "Patient age (years)", min = Cell_Line_Explorer_Initial_Age_Limits[1], max = Cell_Line_Explorer_Initial_Age_Limits[2], value = Cell_Line_Explorer_Initial_Age_Limits, ticks = FALSE) %>%
                        helper(type = "inline",
                          title = "Patient age filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Sets minimum and maximum age allowed for displayed cell line options. Note that setting the slider to anything other than its maximum range will exclude cell lines for which a numeric age could not be determined for the patient who the cell line was derived from at the time of sample collection. This both includes cases where the age is unspecified and cases where the specified age is ambiguous (i.e. such as \"Adult\"). You may download the cell line harmonization file in the \"Download Data\" tab for free-text descriptions of each cell line's patient age."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
    
                        h4("Ancestry") %>%
                        helper(type = "inline",
                          title = "Patient age filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Each slider sets the minimum and maximum % ancestry for each ancestry group. Once any of the ancestry sliders has been changed from its maximum range, cell lines will be filtered to only include lines which meet the limits set on all of the ancestry sliders. Note that % ancestry adds to 100% across all ancestry groups for each individual cell line (i.e. a cell line cannot have 60% African ancestry and 60% Native American ancestry, because that would add to >100%). Ancestry information was obtained from the cellosaurus resource at https://www.expasy.org/."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
                        wellPanel(list(
                          sliderInput(inputId = "Cell_Line_Explorer_African", label = "% african ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                          sliderInput(inputId = "Cell_Line_Explorer_Native_American", label = "% native american ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                          sliderInput(inputId = "Cell_Line_Explorer_East_Asian_North", label = "% east asian (north) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                          sliderInput(inputId = "Cell_Line_Explorer_East_Asian_South", label = "% east asian (south) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                          sliderInput(inputId = "Cell_Line_Explorer_South_Asian", label = "% south asian ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                          sliderInput(inputId = "Cell_Line_Explorer_European_North", label = "% european (north) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                          sliderInput(inputId = "Cell_Line_Explorer_European_South", label = "% european (south) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE)
                        ))
                      ))
                    )
                    return(return_list)
                  } else {
                    return(list(" "))
                  }
                })
              })
          
          #Updating cell line filter interface
            #Defining currently available cell lines based on cell line filters
                Cell_Line_Explorer_Currently_Available_Cell_Lines <- reactive({

                  Temp_CL_Harm_Data <- Simple_Cell_Line_Harm
                    temp_Cell_Line_Diseases <- strsplit(Temp_CL_Harm_Data$Diseases, ":\\|:")
                    names(temp_Cell_Line_Diseases) <- Temp_CL_Harm_Data$Harmonized_Cell_Line_ID
  
                  Cell_Line_Explorer_Initial_Age_Limits <- c(min(Simple_Cell_Line_Harm$Numeric_Age_in_Years, na.rm = TRUE), max(Simple_Cell_Line_Harm$Numeric_Age_in_Years, na.rm = TRUE))
                    
                  Avail_ccls <- sort(unique(Temp_CL_Harm_Data$Harmonized_Cell_Line_ID))
                  Filtered_ccls <- character(0)
                  flag <- 0
  
                  if(! is.null(input$Cell_Line_Explorer_Cancer_Type)){
                    flag <- 1
                    Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Simple_Cancer_Type %in% input$Cell_Line_Explorer_Cancer_Type]]))
                  }
  
                  if(! is.null(input$Cell_Line_Explorer_Disease_Name)){
                    if(flag == 0){
                      Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% names(temp_Cell_Line_Diseases)[sapply(temp_Cell_Line_Diseases, function(x,y){return(any(y %in% x))}, y = input$Cell_Line_Explorer_Disease_Name)]]))
                    } else if(flag == 1){
                      Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% names(temp_Cell_Line_Diseases)[sapply(temp_Cell_Line_Diseases, function(x,y){return(any(y %in% x))}, y = input$Cell_Line_Explorer_Disease_Name)]]]))
                    }
                    flag <- 1
                  }
  
                  if(! is.null(input$Cell_Line_Explorer_Gender)){
                    if(flag == 0){
                      Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Gender %in% input$Cell_Line_Explorer_Gender]]))
                    } else if(flag == 1){
                      Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Gender %in% input$Cell_Line_Explorer_Gender]]]))
                    }
                    flag <- 1
                  }
  
                  if(! all(input$Cell_Line_Explorer_Age == Cell_Line_Explorer_Initial_Age_Limits)){
                    if(flag == 0){
                      Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Numeric_Age_in_Years >= input$Cell_Line_Explorer_Age[1] &
                                                                                                           Temp_CL_Harm_Data$Numeric_Age_in_Years <= input$Cell_Line_Explorer_Age[2]]]))
                    } else if(flag == 1){
                      Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Numeric_Age_in_Years >= input$Cell_Line_Explorer_Age[1] &
                                                                                                                                                        Temp_CL_Harm_Data$Numeric_Age_in_Years <= input$Cell_Line_Explorer_Age[2]]]]))
                    }
                    flag <- 1
                  }
  
                  if(! all(input$Cell_Line_Explorer_African == c(0,100)) |
                     ! all(input$Cell_Line_Explorer_Native_American == c(0,100)) |
                     ! all(input$Cell_Line_Explorer_East_Asian_North == c(0,100)) |
                     ! all(input$Cell_Line_Explorer_East_Asian_South == c(0,100)) |
                     ! all(input$Cell_Line_Explorer_South_Asian == c(0,100)) |
                     ! all(input$Cell_Line_Explorer_European_North == c(0,100)) |
                     ! all(input$Cell_Line_Explorer_European_South == c(0,100))){
                    if(flag == 0){
                      Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$African_Ancestry >= input$Cell_Line_Explorer_African[1]/100 &
                                                                                                           Temp_CL_Harm_Data$African_Ancestry <= input$Cell_Line_Explorer_African[2]/100 &
                                                                                                           Temp_CL_Harm_Data$Native_American_Ancestry >= input$Cell_Line_Explorer_Native_American[1]/100 &
                                                                                                           Temp_CL_Harm_Data$Native_American_Ancestry <= input$Cell_Line_Explorer_Native_American[2]/100 &
                                                                                                           Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` >= input$Cell_Line_Explorer_East_Asian_North[1]/100 &
                                                                                                           Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` <= input$Cell_Line_Explorer_East_Asian_North[2]/100 &
                                                                                                           Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` >= input$Cell_Line_Explorer_East_Asian_South[1]/100 &
                                                                                                           Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` <= input$Cell_Line_Explorer_East_Asian_South[2]/100 &
                                                                                                           Temp_CL_Harm_Data$South_Asian_Ancestry >= input$Cell_Line_Explorer_South_Asian[1]/100 &
                                                                                                           Temp_CL_Harm_Data$South_Asian_Ancestry <= input$Cell_Line_Explorer_South_Asian[2]/100 &
                                                                                                           Temp_CL_Harm_Data$`European_(North)_Ancestry` >= input$Cell_Line_Explorer_European_North[1]/100 &
                                                                                                           Temp_CL_Harm_Data$`European_(North)_Ancestry` <= input$Cell_Line_Explorer_European_North[2]/100 &
                                                                                                           Temp_CL_Harm_Data$`European_(South)_Ancestry` >= input$Cell_Line_Explorer_European_South[1]/100 &
                                                                                                           Temp_CL_Harm_Data$`European_(South)_Ancestry` <= input$Cell_Line_Explorer_European_South[2]/100]]))
                    } else if(flag == 1){
                      Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$African_Ancestry >= input$Cell_Line_Explorer_African[1]/100 &
                                                                                                           Temp_CL_Harm_Data$African_Ancestry <= input$Cell_Line_Explorer_African[2]/100 &
                                                                                                           Temp_CL_Harm_Data$Native_American_Ancestry >= input$Cell_Line_Explorer_Native_American[1]/100 &
                                                                                                           Temp_CL_Harm_Data$Native_American_Ancestry <= input$Cell_Line_Explorer_Native_American[2]/100 &
                                                                                                           Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` >= input$Cell_Line_Explorer_East_Asian_North[1]/100 &
                                                                                                           Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` <= input$Cell_Line_Explorer_East_Asian_North[2]/100 &
                                                                                                           Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` >= input$Cell_Line_Explorer_East_Asian_South[1]/100 &
                                                                                                           Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` <= input$Cell_Line_Explorer_East_Asian_South[2]/100 &
                                                                                                           Temp_CL_Harm_Data$South_Asian_Ancestry >= input$Cell_Line_Explorer_South_Asian[1]/100 &
                                                                                                           Temp_CL_Harm_Data$South_Asian_Ancestry <= input$Cell_Line_Explorer_South_Asian[2]/100 &
                                                                                                           Temp_CL_Harm_Data$`European_(North)_Ancestry` >= input$Cell_Line_Explorer_European_North[1]/100 &
                                                                                                           Temp_CL_Harm_Data$`European_(North)_Ancestry` <= input$Cell_Line_Explorer_European_North[2]/100 &
                                                                                                           Temp_CL_Harm_Data$`European_(South)_Ancestry` >= input$Cell_Line_Explorer_European_South[1]/100 &
                                                                                                           Temp_CL_Harm_Data$`European_(South)_Ancestry` <= input$Cell_Line_Explorer_European_South[2]/100]]]))
                    }
                    flag <- 1
                  }
  
                  if(flag == 0){
                    return(Avail_ccls)
                  } else if(flag == 1){
                    return(Filtered_ccls)
                  }
                })
              
              #Updating filter menus
                observeEvent(Cell_Line_Explorer_Currently_Available_Cell_Lines(), {
                  #Organizing data for available cell lines
                    Temp_CL_Harm_Data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% Cell_Line_Explorer_Currently_Available_Cell_Lines(),]
                    temp_Cell_Line_Diseases <- strsplit(Temp_CL_Harm_Data$Diseases, ":\\|:")
                      names(temp_Cell_Line_Diseases) <- Temp_CL_Harm_Data$Harmonized_Cell_Line_ID
                  #Cancer type menu
                    Cancer_Type_Options <- sort(unique(Temp_CL_Harm_Data$Simple_Cancer_Type))
                    previous_selection <- input$Cell_Line_Explorer_Cancer_Type
                    isolate({
                      updatePickerInput(session, "Cell_Line_Explorer_Cancer_Type", choices = Cancer_Type_Options, selected = previous_selection)
                    })
                  #Cancer type menu
                    Disease_Name_Options <- sort(unique(unlist(temp_Cell_Line_Diseases)))
                    previous_selection <- input$Cell_Line_Explorer_Disease_Name
                    isolate({
                      updatePickerInput(session, "Cell_Line_Explorer_Disease_Name", choices = Disease_Name_Options, selected = previous_selection)
                    })
                  #Gender menu
                    Gender_Options <- sort(unique(Temp_CL_Harm_Data$Gender))
                    previous_selection <- input$Cell_Line_Explorer_Gender
                    isolate({
                      updatePickerInput(session, "Cell_Line_Explorer_Gender", choices = Gender_Options, selected = previous_selection)
                    })
                  #Age slider: Decided not to update age slider because doing so removes cell lines with unknown age in years even if user has not manually changed the age
                  #slider. This problem could be overcome, but it seems like more work than it is worth to make it work intuitively.
                    # Age_Options <- c(min(Temp_CL_Harm_Data$Numeric_Age_in_Years, na.rm = TRUE), max(Temp_CL_Harm_Data$Numeric_Age_in_Years, na.rm = TRUE))
                    # previous_selection <- input$Cell_Line_Explorer_Age
                    # previous_selection <- c(max(Age_Options[1], previous_selection[1]), min(Age_Options[2], previous_selection[2]))
                    # isolate({
                    #   updateSliderInput(session, "Cell_Line_Explorer_Age", value = previous_selection)
                    # })
                })
              
              #Updating cell line selection menu
                Selected_Cell_Line_Option_df <- eventReactive(Cell_Line_Explorer_Currently_Available_Cell_Lines(), {
                  filtered_Cell_Line_Option_df <- Cell_Line_Option_df[Cell_Line_Option_df$Harmonized_Cell_Line_ID %in% Cell_Line_Explorer_Currently_Available_Cell_Lines(),]
                  return(cbind(filtered_Cell_Line_Option_df, value = filtered_Cell_Line_Option_df$Harmonized_Cell_Line_ID))
                })
                
                observeEvent(Selected_Cell_Line_Option_df(),{
                  prev_selection <- input$cell_line_explorer
                  if(! length(prev_selection) == 0){
                    if(! prev_selection %in% Selected_Cell_Line_Option_df()$Harmonized_Cell_Line_ID){
                      prev_selection <- character(0)
                    }
                  }
                  updateSelectizeInput(session, "cell_line_explorer", label = paste0("Select a cell line (n = ", nrow(Selected_Cell_Line_Option_df()), ")"), choices = Selected_Cell_Line_Option_df(), server = TRUE, selected = prev_selection)
                })
          
          
            observeEvent(input$cell_line_explorer, ignoreInit = TRUE, eventExpr = {
              
              if(! input$cell_line_explorer == ""){
                #Loading screening data for selected cell line
                  filename <- Cell_Line_Filenames$cell_line_file_names[Cell_Line_Filenames$cell_lines == input$cell_line_explorer]
                  data <- readRDS(paste0("./www/Results/", filename, ".rds"))
                  for(i in 1:length(data)){
                    data[[i]] <- data[[i]][! is.na(data[[i]]$b_c_d_e),]
                  }
                  Cell_Line_Explorer_Data <- reactive({data})

                #Defining compounds available for selected cell line
                  Cell_Line_Explorer_Available_Compounds_for_Cell_Line <- reactive({
                    compounds <- character(0)
                    for(i in 1:length(Cell_Line_Explorer_Data())){
                      compounds <- c(compounds, Cell_Line_Explorer_Data()[[i]]$Compound)
                    }
                    return(sort(unique(compounds)))
                  })

                #Subsetting compound info to compounds available for selected cell line
                  Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line <- reactive({
                    Simple_Compound_Harm[Simple_Compound_Harm$Harmonized_Compound_Name %in% Cell_Line_Explorer_Available_Compounds_for_Cell_Line(),]
                  })
                  
                #Getting molecular targets available for compounds available for selected cell line
                  Cell_Line_Explorer_Available_Molecular_Targets <- reactive({
                    temp_Compound_Molecular_Targets <- strsplit(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line()$Compound_Molecular_Targets, ":\\|:")
                    return(sort(unique(unlist(temp_Compound_Molecular_Targets))))
                  })
                
                #Getting mechanisms of action available for compounds available for selected cell line
                  Cell_Line_Explorer_Available_MOAs <- reactive({
                    temp_Compound_MOAs <- strsplit(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line()$Compound_MOA, ":\\|:")
                    return(sort(unique(unlist(temp_Compound_MOAs))))
                  })
    
                #Getting annotation information for the selected cell line
                  Cell_Line_Explorer_Annotation_Info <- reactive({
                    return(Cell_Line_Harm[Cell_Line_Harm$Harmonized_Cell_Line_ID %in% input$cell_line_explorer,])
                  })
                  
                #Summarizing selected cell line information for text output
                  output$Cell_Line_Explorer_Text <- renderUI({
                    synonyms <- paste(clean_vector(unlist(strsplit(Cell_Line_Explorer_Annotation_Info()$Synonyms, ":\\|:"))), collapse = "; ")
                    gender <- paste(clean_vector(unlist(strsplit(Cell_Line_Explorer_Annotation_Info()$Gender, ":\\|:"))), collapse = "; ")
                    age <- paste(clean_vector(unlist(strsplit(Cell_Line_Explorer_Annotation_Info()$Age, ":\\|:"))), collapse = "; ")
                    disease <- paste(clean_vector(unlist(strsplit(Cell_Line_Explorer_Annotation_Info()$Diseases, ":\\|:"))), collapse = "; ")
                    species <- paste(clean_vector(unlist(strsplit(Cell_Line_Explorer_Annotation_Info()$Species, ":\\|:"))), collapse = "; ")
  
                    list(
                      tableOutput(outputId = "Cell_Line_Explorer_Name_in_Dataset"),
                      p(tags$b("Synonyms: "), p(tags$em(synonyms))),
                      p(tags$b("Gender: "), p(tags$em(gender))),
                      p(tags$b("Age: "), p(tags$em(age))),
                      p(tags$b("Species: "), p(tags$em(species))),
                      p(tags$b("Disease: "), p(tags$em(disease))),
                      tableOutput(outputId = "Cell_Line_Explorer_Ethnicity")
                    )
                  })
  
                  output$Cell_Line_Explorer_Name_in_Dataset <- renderTable({
                    #Creating table with the name used for this cell_line in each dataset
                      explorer_name_table <- Cell_Line_Explorer_Annotation_Info()[,c("Dataset", "Cell_Line_Name_In_Dataset")]
                      colnames(explorer_name_table) <- c("Dataset", "Name in Dataset")
                      return(explorer_name_table)
                  })
  
                  output$Cell_Line_Explorer_Ethnicity <- renderTable({
                    #Creating table with the ethnicity information for this cell line
                      explorer_ethnicity_table <- t(Cell_Line_Explorer_Annotation_Info()[1,grepl("Ancestry", colnames(Cell_Line_Explorer_Annotation_Info()))])
                      explorer_ethnicity_table[,1] <- suppressWarnings(paste0(signif(as.numeric(explorer_ethnicity_table[,1]), 3)*100, "%"))
                      rownames(explorer_ethnicity_table) <- gsub("_", " ", rownames(explorer_ethnicity_table))
                      return(explorer_ethnicity_table)
                  }, rownames = TRUE, colnames = FALSE)
                  
                #Defining compound selection interface
                  output$Cell_Line_Explorer_Compound_Menu <- renderUI({
                    Temp_Compound_Harm_Data <- Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line()

                    return_list <- list(
                      pickerInput("Cell_Line_Explorer_Compounds", label = "Select compounds to plot data for", choices = sort(unique(Temp_Compound_Harm_Data$Harmonized_Compound_Name)), selected = sort(unique(Temp_Compound_Harm_Data$Harmonized_Compound_Name)), multiple = TRUE, options = list(
                        `actions-Box` = TRUE,
                        `live-Search-Style` = "contains" ,
                        `live-Search` = TRUE,
                        `live-Search-Normalize` = TRUE,
                        `selected-Text-Format` = "count"
                      )),
                      checkboxInput("Cell_Line_Explorer_Show_Compound_Filters", label = "Show compound filters?", value = FALSE)
                    )

                    return(return_list)

                  })
                  
                #Defining compound filter interface
                  output$Cell_Line_Explorer_Compound_Filters <- renderUI({
                    req(! length(input$Cell_Line_Explorer_Show_Compound_Filters) == 0)
                    if(input$Cell_Line_Explorer_Show_Compound_Filters == TRUE){
                      return(list(
                        h4("Filter compounds by:") %>%
                            helper(type = "inline",
                              title = "Compound filtering",
                              icon = "question-circle", colour = NULL,
                              content = c("These options can be used to filter the compound options displayed in the \"Select compounds to plot data for\" menu."),
                              size = "m",
                              buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                            ),
                        wellPanel(list(
                          pickerInput(inputId = "Cell_Line_Explorer_Molecular_Target", label = "Filter compounds by molecular target", choices = Cell_Line_Explorer_Available_Molecular_Targets(), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")),
                          pickerInput(inputId = "Cell_Line_Explorer_MOA", label = "Filter compounds by free-text MOA", choices = Cell_Line_Explorer_Available_MOAs(), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")),
                          pickerInput(inputId = "Cell_Line_Explorer_Clinical_Phase", label = "Filter compounds by clinical phase", choices = sort(unique(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line()$Compound_Clinical_Phase)), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional"))
                        ))
                      ))
                    } else {
                      return(list(p(" ")))
                    }
                  })
                  
                #Defining currently available compounds based on compound filters (meets any filter)
                  Cell_Line_Explorer_Currently_Available_Compounds <- reactive({
                    
                    Avail_compounds <- Cell_Line_Explorer_Available_Compounds_for_Cell_Line()
                    Filtered_compounds <- character(0)
                    flag <- 0
                    
                    
                    if(! is.null(input$Cell_Line_Explorer_Molecular_Target)){
                      flag <- 1
                      Filtered_compounds <- Avail_compounds[Avail_compounds %in% names(Compound_Molecular_Targets)[sapply(Compound_Molecular_Targets, function(x,y){any(y %in% x)}, y = input$Cell_Line_Explorer_Molecular_Target)]]
                    }
                    
                    if(! is.null(input$Cell_Line_Explorer_MOA)){
                      if(flag == 0){
                        Filtered_compounds <- Avail_compounds[Avail_compounds %in% names(Compound_MOAs)[sapply(Compound_MOAs, function(x,y){any(y %in% x)}, y = input$Cell_Line_Explorer_MOA)]]
                      } else if(flag == 1){
                        Filtered_compounds <- c(Filtered_compounds, Avail_compounds[Avail_compounds %in% names(Compound_MOAs)[sapply(Compound_MOAs, function(x,y){any(y %in% x)}, y = input$Cell_Line_Explorer_MOA)]])
                      }
                      flag <- 1
                    }
                    
                    if(! is.null(input$Cell_Line_Explorer_Clinical_Phase)){
                      if(flag == 0){
                        Filtered_compounds <- Avail_compounds[Avail_compounds %in% Simple_Compound_Harm$Harmonized_Compound_Name[Simple_Compound_Harm$Compound_Clinical_Phase %in% input$Cell_Line_Explorer_Clinical_Phase]]
                      } else if(flag == 1){
                        Filtered_compounds <- Filtered_compounds[Filtered_compounds %in% Simple_Compound_Harm$Harmonized_Compound_Name[Simple_Compound_Harm$Compound_Clinical_Phase %in% input$Cell_Line_Explorer_Clinical_Phase]]
                      }
                      flag <- 1
                    }
                    
                    if(flag == 0){
                      return(Avail_compounds)
                    } else if(flag == 1){
                      return(Filtered_compounds)
                    }
                  })
              
                #Updating compound input menu
                  observeEvent(Cell_Line_Explorer_Currently_Available_Compounds(), {
                    isolate({
                      updatePickerInput(session, "Cell_Line_Explorer_Compounds", label = paste0("Select compounds to plot data for (n = ", length(Cell_Line_Explorer_Currently_Available_Compounds()), ")"), choices = Cell_Line_Explorer_Currently_Available_Compounds(), selected = Cell_Line_Explorer_Currently_Available_Compounds())
                    })
                  })
                  
                #Creating filtered version of Cell_Line_Explorer_Data
                  Filtered_Cell_Line_Explorer_Data <- reactive({
                    data <- Cell_Line_Explorer_Data()
                    for(i in 1:length(data)){
                      data[[i]] <- data[[i]][data[[i]]$Compound %in% Cell_Line_Explorer_Currently_Available_Compounds(),]
                    }
                    return(data)
                  })
                  
                #Defining datasets with cell line data
                  datasets_with_cell_line_data <- reactive({
                    names(Filtered_Cell_Line_Explorer_Data())[sapply(Filtered_Cell_Line_Explorer_Data(), nrow) > 0]
                  })
                  
                #Rendering plot UI for datasets with data for this compound
                  output$Cell_Line_Explorer_Plots <- renderUI({
                        Cell_Line_Explorer_Plot_UI <- vector(mode = "list")
                        if("CTRPv2" %in% datasets_with_cell_line_data()){
                          Cell_Line_Explorer_Plot_UI <- c(Cell_Line_Explorer_Plot_UI, plotlyOutput(outputId = "Cell_Line_Explorer_CTRPv2_Plot"))
                        }
                        if("GDSC1" %in% datasets_with_cell_line_data()){
                          Cell_Line_Explorer_Plot_UI <- c(Cell_Line_Explorer_Plot_UI, plotlyOutput(outputId = "Cell_Line_Explorer_GDSC1_Plot"))
                        }
                        if("GDSC2" %in% datasets_with_cell_line_data()){
                          Cell_Line_Explorer_Plot_UI <- c(Cell_Line_Explorer_Plot_UI, plotlyOutput(outputId = "Cell_Line_Explorer_GDSC2_Plot"))
                        }
                        if("PRISM_Repurposing" %in% datasets_with_cell_line_data()){
                          Cell_Line_Explorer_Plot_UI <- c(Cell_Line_Explorer_Plot_UI, plotlyOutput(outputId = "Cell_Line_Explorer_PRISM_Repurposing_Plot"))
                        }
                      Cell_Line_Explorer_Plot_UI
                  })
                
                #Generating plots
                  output$Cell_Line_Explorer_CTRPv2_Plot <- renderPlotly({
                    #Loading raw data for this cell_line and any datasets with data for this cell_line
                      if("CTRPv2" %in% datasets_with_cell_line_data()){
                        CTRPv2_Results <- Filtered_Cell_Line_Explorer_Data()$CTRPv2
                        CTRPv2_Results <- CTRPv2_Results[! is.na(CTRPv2_Results$b_c_d_e),]
                      } else {
                        CTRPv2_Results <- data.frame(NULL)
                      }
                    #Making cell_line explorer CTRPv2 plot
                      if(nrow(CTRPv2_Results) > 0){
                        if(input$Cell_Line_Explorer_to_Plot == "AUC values for most commonly used concentration range"){
                          plot_data <- CTRPv2_Results[! is.na(CTRPv2_Results$Percentile_AUC_Mode_CCL),]
                          plot_data <- plot_data[order(plot_data$Percentile_AUC_Mode_CCL, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_CTRPv2_conc] <- "Max Tested Concentration < AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                          ylab <- "Percentile for AUC (most common range)"
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_Mode_CCL*100,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colorRampPalette(c("blue", "red"))(2),
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Cell_Line_Explorer_to_Plot == "AUC values for concentration range available for all cell lines"){
                          plot_data <- CTRPv2_Results[! is.na(CTRPv2_Results$Percentile_AUC_All_CCL),]
                          plot_data <- plot_data[order(plot_data$Percentile_AUC_All_CCL, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          ylab <- "Percentile for AUC (all cell line range)"
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_All_CCL*100,
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Cell_Line_Explorer_to_Plot == "IC50 values"){
                          plot_data <- CTRPv2_Results[! is.na(CTRPv2_Results$Percentile_IC50),]
                          if(nrow(plot_data) > 0){
                            plot_data <- plot_data[order(plot_data$Percentile_IC50, decreasing = FALSE),]
                            plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                            ylab <- "Percentile for IC50"
                            fig <- plot_ly(x = plot_data$Compound,
                                           y = plot_data$Percentile_IC50*100,
                                           type = "scatter",
                                           mode = "markers")
                            fig <- layout(fig,
                                          xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab))
                            fig
                          }
                        }
                      }
                    })
      
                  output$Cell_Line_Explorer_GDSC1_Plot <- renderPlotly({
                    #Loading raw data for this cell_line and any datasets with data for this cell_line
                      if("GDSC1" %in% datasets_with_cell_line_data()){
                        GDSC1_Results <- Filtered_Cell_Line_Explorer_Data()$GDSC1
                        GDSC1_Results <- GDSC1_Results[! is.na(GDSC1_Results$b_c_d_e),]
                      } else {
                        GDSC1_Results <- data.frame(NULL)
                      }
                    #Making cell_line explorer GDSC1 plot
                      if(nrow(GDSC1_Results) > 0){
                        if(input$Cell_Line_Explorer_to_Plot == "AUC values for most commonly used concentration range"){
                          plot_data <- GDSC1_Results[! is.na(GDSC1_Results$Percentile_AUC_Mode_CCL),]
                          plot_data <- plot_data[order(plot_data$Percentile_AUC_Mode_CCL, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_GDSC1_conc] <- "Max Tested Concentration < AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                          ylab <- "Percentile for AUC (most common range)"
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_Mode_CCL*100,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colorRampPalette(c("blue", "red"))(2),
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Cell_Line_Explorer_to_Plot == "AUC values for concentration range available for all cell lines"){
                          plot_data <- GDSC1_Results[! is.na(GDSC1_Results$Percentile_AUC_All_CCL),]
                          plot_data <- plot_data[order(plot_data$Percentile_AUC_All_CCL, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          ylab <- "Percentile for AUC (all cell line range)"
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_All_CCL*100,
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Cell_Line_Explorer_to_Plot == "IC50 values"){
                          plot_data <- GDSC1_Results[! is.na(GDSC1_Results$Percentile_IC50),]
                          if(nrow(plot_data) > 0){
                            plot_data <- plot_data[order(plot_data$Percentile_IC50, decreasing = FALSE),]
                            plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                            ylab <- "Percentile for IC50"
                            fig <- plot_ly(x = plot_data$Compound,
                                           y = plot_data$Percentile_IC50*100,
                                           type = "scatter",
                                           mode = "markers")
                            fig <- layout(fig,
                                          xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab))
                            fig
                          }
                        }
                      }
                    })
                  
                  output$Cell_Line_Explorer_GDSC2_Plot <- renderPlotly({
                    #Loading raw data for this cell_line and any datasets with data for this cell_line
                      if("GDSC2" %in% datasets_with_cell_line_data()){
                        GDSC2_Results <- Filtered_Cell_Line_Explorer_Data()$GDSC2
                        GDSC2_Results <- GDSC2_Results[! is.na(GDSC2_Results$b_c_d_e),]
                      } else {
                        GDSC2_Results <- data.frame(NULL)
                      }
                    #Making cell_line explorer GDSC2 plot
                      if(nrow(GDSC2_Results) > 0){
                        if(input$Cell_Line_Explorer_to_Plot == "AUC values for most commonly used concentration range"){
                          plot_data <- GDSC2_Results[! is.na(GDSC2_Results$Percentile_AUC_Mode_CCL),]
                          plot_data <- plot_data[order(plot_data$Percentile_AUC_Mode_CCL, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_GDSC2_conc] <- "Max Tested Concentration < AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                          ylab <- "Percentile for AUC (most common range)"
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_Mode_CCL*100,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colorRampPalette(c("blue", "red"))(2),
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Cell_Line_Explorer_to_Plot == "AUC values for concentration range available for all cell lines"){
                          plot_data <- GDSC2_Results[! is.na(GDSC2_Results$Percentile_AUC_All_CCL),]
                          plot_data <- plot_data[order(plot_data$Percentile_AUC_All_CCL, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          ylab <- "Percentile for AUC (all cell line range)"
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_All_CCL*100,
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Cell_Line_Explorer_to_Plot == "IC50 values"){
                          plot_data <- GDSC2_Results[! is.na(GDSC2_Results$Percentile_IC50),]
                          if(nrow(plot_data) > 0){
                            plot_data <- plot_data[order(plot_data$Percentile_IC50, decreasing = FALSE),]
                            plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                            ylab <- "Percentile for IC50"
                            fig <- plot_ly(x = plot_data$Compound,
                                           y = plot_data$Percentile_IC50*100,
                                           type = "scatter",
                                           mode = "markers")
                            fig <- layout(fig,
                                          xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab))
                            fig
                          }
                        }
                      }
                    })
                  
                  output$Cell_Line_Explorer_PRISM_Repurposing_Plot <- renderPlotly({
                    #Loading raw data for this cell_line and any datasets with data for this cell_line
                      if("PRISM_Repurposing" %in% datasets_with_cell_line_data()){
                        PRISM_Repurposing_Results <- Filtered_Cell_Line_Explorer_Data()$PRISM_Repurposing
                        PRISM_Repurposing_Results <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$b_c_d_e),]
                      } else {
                        PRISM_Repurposing_Results <- data.frame(NULL)
                      }
                    #Making cell_line explorer PRISM_Repurposing plot
                      if(nrow(PRISM_Repurposing_Results) > 0){
                        if(input$Cell_Line_Explorer_to_Plot == "AUC values for most commonly used concentration range"){
                          plot_data <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$Percentile_AUC_Mode_CCL),]
                          plot_data <- plot_data[order(plot_data$Percentile_AUC_Mode_CCL, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_PRISM_Repurposing_conc] <- "Max Tested Concentration < AUC Range"
                          plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                          ylab <- "Percentile for AUC (most common range)"
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_Mode_CCL*100,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colorRampPalette(c("blue", "red"))(2),
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("PRISM-Repurposing Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Cell_Line_Explorer_to_Plot == "AUC values for concentration range available for all cell lines"){
                          plot_data <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$Percentile_AUC_All_CCL),]
                          plot_data <- plot_data[order(plot_data$Percentile_AUC_All_CCL, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          ylab <- "Percentile for AUC (all cell line range)"
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_All_CCL*100,
                                         type = "scatter",
                                         mode = "markers")
                          fig <- layout(fig,
                                        xaxis = list(title = paste0("PRISM-Repurposing Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          fig
                        } else if(input$Cell_Line_Explorer_to_Plot == "IC50 values"){
                          plot_data <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$Percentile_IC50),]
                          if(nrow(plot_data) > 0){
                            plot_data <- plot_data[order(plot_data$Percentile_IC50, decreasing = FALSE),]
                            plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                            ylab <- "Percentile for IC50"
                            fig <- plot_ly(x = plot_data$Compound,
                                           y = plot_data$Percentile_IC50*100,
                                           type = "scatter",
                                           mode = "markers")
                            fig <- layout(fig,
                                          xaxis = list(title = paste0("PRISM-Repurposing Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab))
                            fig
                          }
                        }
                      }
                    })
                
              } else { #END: if(! input$cell_line_explorer == ""){
                output$Cell_Line_Explorer_Text <- renderUI({})
                output$Cell_Line_Explorer_Compound_Menu <- renderUI({})
                output$Cell_Line_Explorer_Compound_Filters <- renderUI({})
                output$Cell_Line_Explorer_Plots <- renderUI({})
              } 
            }) #END: observeEvent(input$cell_line_explorer, ignoreInit = TRUE, eventExpr = {
          

          
#############################################################          
#################################################### server #          
        }
        
        if(input$tabs == "Plot Dose-Response Curves"){
        #Code for "Plot Dose-Response Curves" tab
          
          #Updating label and choices for cell lines
            observe({
              updateSelectizeInput(session, "plot_cell_line", label = paste0("Select a Cell Line (n = ", length(unique(unlist(compound_ccl_availability[[input$plot_compound]]))), ")"), choices = sort(unique(unlist(compound_ccl_availability[[input$plot_compound]]))))
            })
          #Plotting selected compound and cell line
            observeEvent(input$Create_Compound_CCL_Plot, {
              #Loading data
                #Creating vectors of column names needed for raw plot data
                  raw_plot_columns <- c("Dataset", "Compound_Concentration_uM", "Viability", "Passed_Experiment_QC", "Experiment_ID", "Experiment_Date", "Experiment_Location", "Assay_Type", "Treatment_Duration", "Culture_Media", "Growth_Mode", "Cells_Per_Well", "Plate_Format", "Compound_Source")
                  curve_plot_columns <- c("Dataset", "b_c_d_e", "RSE", "IC50")
                #Checking which datasets have available data for this compound and cell line
                  selected_compound_ccl <- compound_ccl_availability[[input$plot_compound]]
                  in_dataset <- sapply(selected_compound_ccl, function(x){return(input$plot_cell_line %in% x)})
                #Determining Correct Filename for this compound
                  dose_response_filename <- Compound_Filenames$drug_file_names[Compound_Filenames$drugs == input$plot_compound]
                #Loading data for this compound
                  plot_data <- readRDS(paste0("./www/Raw_Data/", dose_response_filename, ".rds"))
                  curve_data <- readRDS(paste0("./www/Results/", dose_response_filename, ".rds"))
                #Loading raw data for this compound and any datasets with data for this pair
                  if(in_dataset["CTRPv2"] == TRUE){
                    CTRPv2_plot_data <- plot_data$CTRPv2
                    CTRPv2_plot_data <- CTRPv2_plot_data[[paste0(input$plot_cell_line, ":|:", input$plot_compound)]]
                    CTRPv2_plot_data$Dataset <- "CTRPv2"
                    CTRPv2_plot_data <- CTRPv2_plot_data[,raw_plot_columns]
                    CTRPv2_curve_data <- curve_data$CTRPv2
                    CTRPv2_curve_data <- CTRPv2_curve_data[CTRPv2_curve_data$Cell_Line %in% input$plot_cell_line,]
                    CTRPv2_curve_data$Dataset <- "CTRPv2"
                    CTRPv2_curve_data <- CTRPv2_curve_data[,curve_plot_columns]
                  } else {
                    CTRPv2_plot_data <- NULL
                    CTRPv2_curve_data <- NULL
                  }
                  if(in_dataset["GDSC1"] == TRUE){
                    GDSC1_plot_data <- plot_data$GDSC1
                    GDSC1_plot_data <- GDSC1_plot_data[[paste0(input$plot_cell_line, ":|:", input$plot_compound)]]
                    GDSC1_plot_data$Dataset <- "GDSC1"
                    GDSC1_plot_data <- GDSC1_plot_data[,raw_plot_columns]
                    GDSC1_curve_data <- curve_data$GDSC1
                    GDSC1_curve_data <- GDSC1_curve_data[GDSC1_curve_data$Cell_Line %in% input$plot_cell_line,]
                    GDSC1_curve_data$Dataset <- "GDSC1"
                    GDSC1_curve_data <- GDSC1_curve_data[,curve_plot_columns]
                  } else {
                    GDSC1_plot_data <- NULL
                    GDSC1_curve_data <- NULL
                  }
                  if(in_dataset["GDSC2"] == TRUE){
                    GDSC2_plot_data <-plot_data$GDSC2
                    GDSC2_plot_data <- GDSC2_plot_data[[paste0(input$plot_cell_line, ":|:", input$plot_compound)]]
                    GDSC2_plot_data$Dataset <- "GDSC2"
                    GDSC2_plot_data <- GDSC2_plot_data[,raw_plot_columns]
                    GDSC2_curve_data <- curve_data$GDSC2
                    GDSC2_curve_data <- GDSC2_curve_data[GDSC2_curve_data$Cell_Line %in% input$plot_cell_line,]
                    GDSC2_curve_data$Dataset <- "GDSC2"
                    GDSC2_curve_data <- GDSC2_curve_data[,curve_plot_columns]
                  } else {
                    GDSC2_plot_data <- NULL
                    GDSC2_curve_data <- NULL
                  }
                  if(in_dataset["PRISM_Repurposing"] == TRUE){
                    PRISM_Repurposing_plot_data <- plot_data$PRISM_Repurposing
                    PRISM_Repurposing_plot_data <- PRISM_Repurposing_plot_data[[paste0(input$plot_cell_line, ":|:", input$plot_compound)]]
                    PRISM_Repurposing_plot_data$Dataset <- "PRISM_Repurposing"
                    PRISM_Repurposing_plot_data <- PRISM_Repurposing_plot_data[,raw_plot_columns]
                    PRISM_Repurposing_curve_data <- curve_data$PRISM_Repurposing
                    PRISM_Repurposing_curve_data <- PRISM_Repurposing_curve_data[PRISM_Repurposing_curve_data$Cell_Line %in% input$plot_cell_line,]
                    PRISM_Repurposing_curve_data$Dataset <- "PRISM_Repurposing"
                    PRISM_Repurposing_curve_data <- PRISM_Repurposing_curve_data[,curve_plot_columns]
                  } else {
                    PRISM_Repurposing_plot_data <- NULL
                    PRISM_Repurposing_curve_data <- NULL
                  }
              #Collapsing Plot Data
                Plot_Data <- rbind(CTRPv2_plot_data, GDSC1_plot_data, GDSC2_plot_data, PRISM_Repurposing_plot_data)
                Plot_Data$Curve <- paste0(Plot_Data$Dataset, "_", Plot_Data$Experiment_ID)
                #Setting up colors for raw data
                  CTRPv2_Curve_IDs <- sort(unique(Plot_Data$Curve[Plot_Data$Dataset %in% "CTRPv2"]))
                  GDSC1_Curve_IDs <- sort(unique(Plot_Data$Curve[Plot_Data$Dataset %in% "GDSC1"]))
                  GDSC2_Curve_IDs <- sort(unique(Plot_Data$Curve[Plot_Data$Dataset %in% "GDSC2"]))
                  PRISM_Repurposing_Curve_IDs <- sort(unique(Plot_Data$Curve[Plot_Data$Dataset %in% "PRISM_Repurposing"]))
                  CTRPv2_colors <- colorRampPalette(c("cadetblue1", "cadetblue3"))(length(CTRPv2_Curve_IDs))
                  GDSC1_colors <- colorRampPalette(c("chartreuse", "chartreuse3"))(length(GDSC1_Curve_IDs))
                  GDSC2_colors <- colorRampPalette(c("deeppink", "deeppink3"))(length(GDSC2_Curve_IDs))
                  PRISM_Repurposing_colors <- colorRampPalette(c("orange", "orange3"))(length(PRISM_Repurposing_Curve_IDs))
                  color_df <- data.frame(c(CTRPv2_Curve_IDs, GDSC1_Curve_IDs, GDSC2_Curve_IDs, PRISM_Repurposing_Curve_IDs), c(CTRPv2_colors, GDSC1_colors, GDSC2_colors, PRISM_Repurposing_colors))
                  colnames(color_df) <- c("Curve", "Color")
                  Plot_Data <- merge(Plot_Data, color_df, by = "Curve")
                  Curves <- sort(unique(Plot_Data$Curve))
                #Handling symbols
                  Plot_Data$Symbols <- 19
                  Plot_Data$Symbols[Plot_Data$Passed_Experiment_QC == FALSE] <- 3
                #Creating copy of full plot data for Experiment_Table output
                  Experiment_Table <- unique(Plot_Data[,c("Dataset", "Experiment_ID", "Passed_Experiment_QC", "Experiment_Date", "Experiment_Location", "Assay_Type", "Treatment_Duration", "Culture_Media", "Growth_Mode", "Cells_Per_Well", "Plate_Format", "Compound_Source")])
                #Calculating averages per dataset if inputs$Plot_Averages == TRUE
                  if(input$Plot_Averages == TRUE){
                    datasets <- unique(Plot_Data$Dataset)
                    Averaged_Data <- data.frame(NULL)
                    for(i in 1:length(datasets)){
                      temp_data <- Plot_Data[Plot_Data$Dataset %in% datasets[i] & Plot_Data$Passed_Experiment_QC == TRUE,]
                      if(nrow(temp_data) > 0){
                        aggregated_data <- aggregate(temp_data$Viability, by = list(temp_data$Compound_Concentration_uM), mean)
                        colnames(aggregated_data) <- c("Compound_Concentration_uM", "Viability")
                        aggregated_data$Dataset <- datasets[i]
                        aggregated_data$Color <- c("cadetblue3", "chartreuse3", "deeppink3", "orange3")[c("CTRPv2", "GDSC1", "GDSC2", "PRISM_Repurposing") %in% aggregated_data$Dataset]
                        aggregated_data$Color <- colorRampPalette(aggregated_data$Color[1])(1)
                        aggregated_data$Symbols <- 19
                        aggregated_data$Curve <- paste0(datasets[i], " Average")
                        aggregated_data$Passed_Experiment_QC <- TRUE
                        Averaged_Data <- rbind(Averaged_Data, aggregated_data)
                      }
                    }
                    Plot_Data <- Averaged_Data
                  }
              #Collapsing curve data
                Curve_Data <- rbind(CTRPv2_curve_data, GDSC1_curve_data, GDSC2_curve_data, PRISM_Repurposing_curve_data)
                Curve_Data <- Curve_Data[! is.na(Curve_Data$b_c_d_e),]
                #Setting up colors for curve data
                  if(nrow(Curve_Data) > 0){
                    Curve_Data$Color <- c("cadetblue4", "chartreuse4", "deeppink4", "orange4")[c("CTRPv2", "GDSC1", "GDSC2", "PRISM_Repurposing") %in% Curve_Data$Dataset]
                    Curve_Data$Color <- colorRampPalette(Curve_Data$Color)(nrow(Curve_Data)) 
                  }
              #Plotting Data
                output$Compound_CCL_Plot <- renderPlotly({
                  if(nrow(Plot_Data) > 0){
                    #Plotting Raw Data
                      fig <- plot_ly(Plot_Data, x = ~Compound_Concentration_uM)
                      Curves <- unique(Plot_Data$Curve)
                      for(i in 1:length(Curves)){
                        temp_data <- Plot_Data[Plot_Data$Curve %in% Curves[i],]
                        fig <- add_trace(fig, x = temp_data$Compound_Concentration_uM, y = temp_data$Viability, name = Curves[i], type = "scatter", mode = "markers", marker = list(color = unique(temp_data$Color), symbol = unique(temp_data$Symbol)))
                      }
                    #Adding axis labels and setting x axis to log
                      fig <- layout(fig, 
                                    xaxis = list(type = "log",
                                                 title = paste("Concentration (microMolar)")), 
                                    yaxis = list(title = "Viability"))
                  } else {
                    fig <- plotly_empty(type = "scatter", mode = "markers")
                    fig <- layout(fig, title = list(text = "No experiments passed QC for this compound-cell line pair.",
                                               yref = "paper",
                                               y = 0.5))
                  }
                  #Plotting fitted dose-response curves
                    if(nrow(Curve_Data) > 0){
                      for(i in 1:nrow(Curve_Data)){
                        used_concentrations <- Plot_Data$Compound_Concentration_uM[Plot_Data$Dataset %in% Curve_Data$Dataset[i] & Plot_Data$Passed_Experiment_QC %in% TRUE]
                        x <- 10^seq(from = log10(min(used_concentrations)), to = log10(max(used_concentrations)), length.out = 100)                
                        y <- ll.4(x, b_c_d_e = Curve_Data$b_c_d_e[i])
                        fig <- add_trace(fig,
                                         line = list(color = Curve_Data$Color[i],
                                                     width = 4),
                                         mode = "lines",
                                         name = paste0(Curve_Data$Dataset[i], " Fit (RSE=", signif(Curve_Data$RSE[i], 2), "; IC50=", signif(Curve_Data$IC50[i], 2), "uM)"),
                                         type = "scatter",
                                         x = x,
                                         y = y,
                                         opacity = 0.6)
                      
                      } 
                    }
                  #Displaying plot
                    fig
                })
              #Creating data table to display information about each experiment in each dataset
                output$Experiment_Table <- DT::renderDataTable({
                  return(Experiment_Table)
                })
            })
            
#############################################################          
#################################################### server #          
        }
        
        if(input$tabs == "AUC Values"){
          #Code for "AUC Values" Tab
          
          
#############################################################          
#################################################### server #          
        }
        
        if(input$tabs == "Viability Values"){
          #Code for "Viability Values" Tab
          
          
#############################################################          
#################################################### server #          
        }
        
        if(input$tabs == "Download Data"){
          #Code for "Download Data" Tab
          
          
        }
      })
    
  }

#Assemble user interface and server
  shinyApp(ui = ui, server = server)  
  