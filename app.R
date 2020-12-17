#This app is designed to provide easy non-programmatic access to the
#R. Stephanie Huang Lab's reprocessed versions of the CTRPv2, GDSC1, GDSC2,
#and PRISM Repurposing high throughput cancer cell line drug screens.
#Written by Alexander L. Ling (alling@umn.edu)
#R. Stephanie Huang Lab, University of Minnesota
#6/18/2020

#Options
  options(stringsAsFactors = FALSE)

#Setting Seed
  set.seed(06182020)
  
#Working Directory
  #setwd("D:/Huang Lab/simplicity app/Simplicity")
 
#Loading global datasets, functions, and packages
  source("globaldata.R")

#############################################################  
######################################################## UI #          
#############################################################
#Creating user interface function
  ui <- tagList(
    
    tags$head(includeHTML(("google-analytics.html"))),
    
    navbarPage(id = "tabs", title = "Simplicity v1.0",
        
        
                   
############################################################          
####################################################### UI #               
        tabPanel(value = "Welcome", title = "Welcome",
          
          #Welcome text for app
            h3("Welcome to the Simplicity web app!"),
            p(HTML("This app was created to provide simple, non-programmatic access to explore and perform calculations with data from high-throughput cancer drug screens performed in cancer cell lines. Each of the included datasets (CTRPv2, GDSC1, GDSC2, and PRISM-Repurposing) have screened hundreds of compounds in hundreds of cell lines.")),
            p(HTML("You can watch <u>this video</u> for a quick tutorial on using Simplicity. Otherwise, please feel free to get started by selecting the appropriate tab at the top for the task you wish to complete. The basic functions of each tab are as follows:")),
            wellPanel(
              HTML("<b><u>Data Explorer</u></b>
                      <ul>
                        <li><b>Explore Datasets:</b> Provides a birds-eye overview of the compounds, cell lines, screening methodologies, and data quality associated with each of the datasets included in Simplicity.</li>
                        <li><b>Explore Compounds:</b> Easily visualize the effectiveness of a compound across a custom set of cell lines in each dataset. Also allows you to see the characteristics (i.e. age, gender, ethnicity) of the cell lines tested with each compound in each dataset.</li>
                        <li><b>Explore Cell Lines:</b> Easily visualize the response of a cell line to a custom set of compounds in each dataset.</li>
                        <li><b>Plot Dose-Respone Curves:</b> A detailed look at single compound-cell line pairs. View raw data, fitted dose-response curves, and experimental details from each dataset for the selected pair.</li>
                      </ul>
                      <br/>
                      <b><u>Calculate Custom Statistics</u></b>
                      <ul>
                        <li><b>AUC Values:</b> Calculate normalized area under the curve (AUC) values from each dataset using custom concentration ranges for each compound.</li>
                        <li><b>Viability Values:</b> Calculate viability values from each dataset using custom concentration ranges for each compound. Can be used to generate custom inputs for our IDACombo-shiny app.</li>
                      </ul>
                      <br/>
                      <b><u>About Simplicity</u></b>
                      <ul>
                        <li><b>Methods:</b> Details about how the data in Simplicity was generated.</li>
                        <li><b>Contact Us:</b> Who we are and how to get in touch with us.</li>
                        <li><b>Cite This Resource:</b> If you use this resource for your research, please cite us and the researchers who generated the data in Simplicity when you publish your work! This tab contains information on how to do that.</li>
                        <li><b>Usage License:</b> If you download any data from any tabs in Simplicity, please read this to learn about what you can and cannot do with that data.</li>
                      </ul>
                      <br/>
                      <b><u>Download Bulk Data:</u></b> Where to go if you want to download the data being used by this app.")
            )
        ),                  

#############################################################          
######################################################## UI #        
        navbarMenu(title = "Data Explorer",
          
####################################################### UI #               
        tabPanel(value = "Explore Datasets", title = "Explore Datasets",
          
          #Explore dataset page text
            h3("Dataset Explorer"),
            p("This page provides a birds-eye overview of each of the datasets included in Simplicity. You can change which dataset is being summarized using the drop-down menu below."),
          #Selector for choosing which dataset to display a summary for
            sidebarLayout(sidebarPanel(
                selectInput(inputId = "Summary_Dataset", label = "Which dataset would you like to see a summary for?", choices = Dataset_Summaries$Dataset, selected = Dataset_Summaries$Dataset[1], multiple = FALSE, selectize = FALSE),
                uiOutput(outputId = "Dataset_Summary_Text")
              ), mainPanel(
                plotlyOutput(outputId = "welcome_cancer_type") %>%
                            helper(type = "inline",
                              title = "Tested cell line cancer types",
                              icon = "question-circle", colour = NULL,
                              content = c("This stacked barplot shows the availability of cell lines by general cancer type and gender in the selected dataset. Cell lines can also be filtered by more detailed disease names in many of the tabs in this app. A detailed description of how cell line information was obtained is included in the \"About Simplicity/Methods\" tab. Full cell line annotation information can be downloaded in the \"Download Bulk Data\" tab."),
                              size = "m",
                              buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                            ),
                plotlyOutput(outputId = "welcome_ancestry") %>%
                            helper(type = "inline",
                              title = "Tested cell line ancestries",
                              icon = "question-circle", colour = NULL,
                              content = c("This stacked barplot shows the ancestry makeup of each of the tested cell lines in the selected dataset. Each bar summarizes the ancestry of a single cell line, with ancestry for each cell line being represented by the percentage of that cell line's ancestry which belongs to each of the 7 labeled ancestry groups. All ancestry groups for each cell line must sum to 100%. For example, if a cell line has 50% South Asian ancestry and 50% Native American Ancestry, it would be represented by a bar which is half purple (South Asian) and half orange (Native American). A detailed description of how cell line information was obtained is included in the \"About Simplicity/Methods\" tab. Full cell line annotation information can be downloaded in the \"Download Bulk Data\" tab."),
                              size = "m",
                              buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                            ),
                plotlyOutput(outputId = "welcome_age") %>%
                            helper(type = "inline",
                              title = "Tested cell line patient ages",
                              icon = "question-circle", colour = NULL,
                              content = c("This density plot shows the scaled distribution of patient ages at the time tissue was collected for cell line derivation. Note that this plot only includes information from cell lines for which a numeric age could be determined (i.e. it would omit cell lines annotated with the generic age of \"Adult\"). A detailed description of how cell line information was obtained is included in the \"About Simplicity/Methods\" tab. Full cell line annotation information can be downloaded in the \"Download Bulk Data\" tab."),
                              size = "m",
                              buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                            ),
                plotlyOutput(outputId = "welcome_clinical_phase") %>%
                            helper(type = "inline",
                              title = "Tested compound clinical phases",
                              icon = "question-circle", colour = NULL,
                              content = c("This barplot shows the number of compounds at each clinical stage of development in the selected dataset. A detailed description of how compound information was obtained is included in the \"About Simplicity/Methods\" tab. Full compound annotation information can be downloaded in the \"Download Bulk Data\" tab."),
                              size = "m",
                              buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                            ),
                plotlyOutput(outputId = "n_Compounds_Per_Cell_Line") %>%
                            helper(type = "inline",
                              title = "# of tested compounds per cell line",
                              icon = "question-circle", colour = NULL,
                              content = c("This barplot shows the number of compounds tested per cell line, calculated both using attempted tests and using only tests that passed Simplicity's QC metrics. A description of Simplicity's curve fitting and QC pipelines can be found in the \"About Simplicity/Methods\" tab."),
                              size = "m",
                              buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                            ),
                plotlyOutput(outputId = "n_Cell_Lines_Per_Compound") %>%
                            helper(type = "inline",
                              title = "# of tested cell lines per compound",
                              icon = "question-circle", colour = NULL,
                              content = c("This barplot shows the number of cell lines tested per compound, calculated both using attempted tests and using only tests that passed Simplicity's QC metrics. A description of Simplicity's curve fitting and QC pipelines can be found in the \"About Simplicity/Methods\" tab."),
                              size = "m",
                              buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                            ),
                plotlyOutput(outputId = "Dataset_Residual_Standard_Error") %>%
                            helper(type = "inline",
                              title = "Residual standard errors",
                              icon = "question-circle", colour = NULL,
                              content = c("This density plot shows the scaled distribution of residual standard errors (RSEs) for the fitted dose-response curves in the selected datset. Note that this includes RSEs from all curves from the dataset, regardless of whether or not they passed Simplicity's QC requirements. An RSE of 0 indicates a perfect curve fit, while an RSE of 1 indicates an RSE of 100% viability. Note that RSEs > 1 are possible with very noisy dose-response curves. For a description of how Simplicity dose-response curves were fit and the minimum RSE required to pass QC, please see the \"About Simplicity/Methods\" tab."),
                              size = "m",
                              buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                            )
              )
            )
        ),
                            
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
                  fluidRow(
                    column(width = 4,
                      selectInput(inputId = "Compound_Explorer_to_Plot", "Select what values to plot", choices = c("AUC values for most commonly used concentration range", "AUC values for concentration range available for all tested cell lines", "IC50 values", "Tested Cell Line Cancer Types & Genders", "Tested Cell Line Ancestries", "Tested Cell Line Ages"), selected = "AUC values for most commonly used concentration range", multiple = FALSE, selectize = FALSE) %>%
                        helper(type = "inline",
                          title = "Select which values to plot for the selected compound and cell lines",
                          icon = "question-circle", colour = NULL,
                          content = HTML("<ul>
                                          <li><b>AUC values for most commonly used concentration range:</b> Displays normalized area under the curve values calculated using concentration range most commonly tested for this compound in each dataset. Note that, since different concentration ranges for a comopund were sometimes tested in different cell lines, this concentration range may sometimes be broader than the range tested for some of the displayed cell lines. Such cell lines will be colored red.</li>
                                          <li><b>AUC values for concentration range available for all tested cell lines:</b> Displays normalized area under the curve values calculated using the broadest concentration range available for all cell lines tested with this comopund in each dataset. Note that, since different concentration ranges for a comopund were sometimes tested in different cell lines, this concentration range may sometimes be narrower than the range tested for some of the displayed cell lines.</li>
                                          <li><b>IC50 values:</b> Displays IC50 values (i.e. concentrations at which 50% viability is achieved). Note that IC50 values beyond the maximum tested concentrations are extrapolations and, as such, are prone to extreme error. Also note that some curves have lower assymptotes >50%, meaning that the IC50 value is never reached--such cases are colored gray.</li>
                                          <li><b>Tested Cell Line Cancer Types & Genders:</b> Displays stacked barplots showing the availability of the selected cell lines tested with the selected compound by general cancer type and gender in each dataset. A detailed description of how cell line information was obtained is included in the \"About Simplicity/Methods\" tab. Full cell line annotation information can be downloaded in the \"Download Bulk Data\" tab.</li>
                                          <li><b>Tested Cell Line Ancestries:</b> Displays stacked barplots showing the ancestry makeup of each of the selected cell lines cell lines tested with the selected compound in each dataset. Each bar summarizes the ancestry of a single cell line, with ancestry for each cell line being represented by the percentage of that cell line's ancestry which belongs to each of the 7 labeled ancestry groups. All ancestry groups for each cell line must sum to 100%. For example, if a cell line has 50% South Asian ancestry and 50% Native American Ancestry, it would be represented by a bar which is half purple (South Asian) and half orange (Native American). A detailed description of how cell line information was obtained is included in the \"About Simplicity/Methods\" tab. Full cell line annotation information can be downloaded in the \"Download Bulk Data\" tab.</li>
                                          <li><b>Tested Cell Line Age:</b> Displays density plots showing the scaled distribution of patient ages at the time tissue was collected for cell line derivation for the selected cell lines tested with the selected compound in each dataset. Note that this plot only includes information from cell lines for which a numeric age could be determined (i.e. it would omit cell lines annotated with the generic age of \"Adult\"). A detailed description of how cell line information was obtained is included in the \"About Simplicity/Methods\" tab. Full cell line annotation information can be downloaded in the \"Download Bulk Data\" tab.</li>
                                        </ul>"),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        )
                    )
                  ),
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
                  fluidRow(
                    column(width = 4,
                      selectInput(inputId = "Cell_Line_Explorer_to_Plot", "Select what values to plot", choices = c("AUC percentiles for most commonly used concentration range", "AUC percentiles for concentration range available for all tested cell lines", "IC50 percentiles"), selected = "AUC percentiles for most commonly used concentration range", multiple = FALSE, selectize = FALSE) %>%
                        helper(type = "inline",
                          title = "Select which values to plot for the selected cell line and compounds",
                          icon = "question-circle", colour = NULL,
                          content = HTML("<ul>
                                          <li><b>AUC percentiles for most commonly used concentration range:</b> Displays percentiles for this cell line's normalized area under the curve values when calculated using the most commonly tested concentration range for each compound in each dataset. The 100th percentile indicates that this cell line was the most sensitive (i.e. lowest AUC) cell line of those tested for a given compound, with the 0th percentile indicating this cell line was the least sensitive (i.e. highest AUC).</li>
                                          <li><b>AUC percentiles for concentration range available for all tested cell lines:</b> Displays percentiles for this cell line's normalized area under the curve values when calculated using the broadest concentration range available for all cell lines tested with a given compound in each dataset. The 100th percentile indicates that this cell line was the most sensitive (i.e. lowest AUC) cell line of those tested for a given compound, with the 0th percentile indicating this cell line was the least sensitive (i.e. highest AUC).</li>
                                          <li><b>IC50 percentiles:</b> Displays percentiles for this cell line's IC50 values (i.e. compound concentrations at which viability = 50%) for each of the selected compounds in each dataset. The 100th percentile indicates that this cell line was the most sensitive (i.e. lowest IC50) cell line of those tested for a given compound, with the 0th percentile indicating this cell line was the least sensitive (i.e. highest IC50).</li>
                                        </ul>"),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        )
                    )
                  ),
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
                column(width = 4,
                    selectizeInput("plot_compound", label = "Select a compound", choices = NULL, multiple = FALSE, options = list(
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
                    checkboxInput("Plot_Show_Compound_Filters", label = "Show compound filters?", value = FALSE),
                    uiOutput(outputId = "Plot_Compound_Filters")),
                column(width = 4,
                    uiOutput(outputId = "Plot_Cell_Line_Menu")
                )
              ),
            ),
          #Checkbox to indicate if average values should be plotted
            checkboxInput(inputId = "Plot_Averages", label = "Plot Averaged Values? (only uses experiments that passed QC)", width = "100%"),
          #Display dose-response curve and table of experiments
            uiOutput(outputId = "Plot_Info_Panel")
        )         
      ),

#############################################################          
######################################################## UI #           
        navbarMenu(title = "Calculate Custom Statistics",
                   
######################################################## UI #           
          tabPanel(value = "AUC Values", title = "AUC Values",
              list(
                wellPanel(
                  p(HTML("<b>By following the instructions below, this tab can be used to calculate normalized area under the curve (AUC) values (see methods tab) for custom concentration ranges using the fitted dose-response curves in Simplicity for each dataset.</b>"), style = "font-size:16px;"),
                  p(HTML("<b>Step 1:</b> Select a dataset to use for AUC calculations.")),
                  p(HTML("<b>Step 2 (optional):</b> Generate a template instruction file which can be downloaded and modified to specify the compounds and concentration ranges to be used for AUC calculations.")),
                  p(HTML("<b>Step 3:</b> Upload an instruction file specifying the compounds and concentration ranges to be used for AUC calculations.")),
                  p(HTML("<b>Step 4:</b> Select the cell lines for which AUC are to be calculated for the compounds/concentration ranges specified in the instruction file.")),
                  p(HTML("<b>Step 5:</b> Press \"Calculate AUC Values\" button to generate and download calculated AUC values."))
                ),
                # h4("Step 1: Select dataset"),
                pickerInput(inputId = "AUC_Dataset", label = "Step 1: Which dataset would you like to use to calculate AUC values?", choices = Dataset_Summaries$Dataset, selected = NULL, multiple = TRUE, width = "50%", options = list(`none-Selected-Text` = "Select dataset", `max-options` = 1)),
                uiOutput(outputId = "AUC_Interface")
              )
          ),

######################################################## UI #           
          tabPanel(value = "Viability Values", title = "Viability Values",
              list(
                wellPanel(
                  p(HTML("<b>By following the instructions below, this tab can be used to calculate cell line viability values (see methods tab) at custom compound concentrations using the fitted dose-response curves in Simplicity for each dataset.</b>"), style = "font-size:16px;"),
                  p(HTML("<b>Step 1:</b> Select a dataset to use for viability calculations.")),
                  p(HTML("<b>Step 2 (optional):</b> Generate a template instruction file which can be downloaded and modified to specify the compounds and concentrations to be used for viability calculations.")),
                  p(HTML("<b>Step 3:</b> Upload an instruction file specifying the compounds and concentration ranges to be used for Viability calculations.")),
                  p(HTML("<b>Step 4:</b> Select the cell lines for which viability values are to be calculated for the compounds/concentrations specified in the instruction file.")),
                  p(HTML("<b>Step 5:</b> Press \"Calculate Viability Values\" button to generate and download calculated viability values."))
                ),
                # h4("Step 1: Select dataset"),
                pickerInput(inputId = "Viability_Dataset", label = "Step 1: Which dataset would you like to use to calculate viability values?", choices = Dataset_Summaries$Dataset, selected = NULL, multiple = TRUE, width = "50%", options = list(`none-Selected-Text` = "Select dataset", `max-options` = 1)),
                uiOutput(outputId = "Viability_Interface")
              )
          )
        ),


#############################################################          
######################################################## UI #
        navbarMenu(title = "About Simplicity",

######################################################## UI #           
          tabPanel(value = "Methods", title = "Methods",
            list(
              h4("Once the methods section for the paper is written, it will be formatted nicely and put here. The main things that need to be covered are as follows:"),
              wellPanel(
                h5("1. How was raw data obtained for each dataset?"),
                h5("2. How was raw data pre-processed for each dataset (especially PRISM-Repurposing)?"),
                h5("3. How was curve fitting performed?"),
                h5("4. How were cell lines and compounds annotated/harmonized?"),
                h5("5. How are normalized AUC and Viability values calculated?"),
                h5("6. What makes the code tick (packages, environment, etc.) and where can the source code be accessed?")
              )
            )      
          ),

######################################################## UI #           
          tabPanel(value = "Contact Us", title = "Contact Us",
            list(
              h3("Please feel free to contact us!"),
              wellPanel(
                p(HTML("We would love to answer any questions you have about Simplicity. We also want to hear about any bugs you encounter 
                  when using the app or any features you would like us to add so we can keep improving Simplicity's usefullness to the field.")),
                br(),
                p(HTML("<b>Principal Investigator:</b>")),
                p(HTML("R. Stephanie Huang (<i>rshuang@umn.edu</i>)")),
                br(),
                p(HTML("<b>Project Creator:</b>")),
                p(HTML("Alexander Ling (<i>alling@umn.edu</i>)")),
                br(),
                p(HTML("<b>Project Maintainer:</b>")),
                p(HTML("John/Jane Doe (<i></i>)"))
              )
            )      
          ),

######################################################## UI #           
          tabPanel(value = "Citation", title = "Cite This Resource",
            list(
              h3("If you use this resource for your research, please cite us, along with the original creators of any datasets you use from Simplicity."),
              wellPanel(
              p(HTML("<b>Simplicity App:</b>")),
              p(HTML("1. We're working on a manuscript for the app, and we will provide a citation for that manuscript here once it is published."))
              ),
              wellPanel(
              p(HTML("<b>Compound and Cell Line Harmonization Tables and Csustained values:</b>")),
              p(HTML("1.	Ling, A. & Huang, R. S. Computationally predicting clinical drug combination efficacy with cancer cell line screens and independent drug action. <i>Nat. Commun.</i> <b>11</b>, 1–13 (2020)."))
              ),
              wellPanel(
              p(HTML("<b>CTRPv2:</b>")),
              p(HTML("1.	Basu, A. et al. An Interactive Resource to Identify Cancer Genetic and Lineage Dependencies Targeted by Small Molecules. <i>Cell</i> <b>154</b>, 1151–1161 (2013).")),
              p(HTML("2.	Seashore-Ludlow, B. et al. Harnessing Connectivity in a Large-Scale Small-Molecule Sensitivity Dataset. <i>Cancer Discov.</i> <b>5</b>, 1210–1223 (2015).")),
              p(HTML("3.	Rees, M. G. et al. Correlating chemical sensitivity and basal gene expression reveals mechanism of action. <i>Nat. Chem. Biol.</i> <b>12</b>, 109–116 (2016).")),
              p(HTML("Visit their website at <a href=\"https://portals.broadinstitute.org/ctrp/\">https://portals.broadinstitute.org/ctrp/</a>."))
              ),
              wellPanel(
              p(HTML("<b>GDSC1 and GDSC2:</b>")),
              p(HTML("1.	Iorio, F. et al. A Landscape of Pharmacogenomic Interactions in Cancer. <i>Cell</i> <b>166</b>, 740–754 (2016).")),
              p(HTML("2.	Yang, W. et al. Genomics of Drug Sensitivity in Cancer (GDSC): a resource for therapeutic biomarker discovery in cancer cells. <i>Nucleic Acids Res.</i> <b>41</b>, D955–D961 (2013).")),
              p(HTML("3.	Garnett, M. J. et al. Systematic identification of genomic markers of drug sensitivity in cancer cells. <i>Nature</i> <b>483</b>, 570–575 (2012).")),
              p(HTML("Visit their website at <a href=\"https://www.cancerrxgene.org/\">https://www.cancerrxgene.org/</a>."))
              ),
              wellPanel(
              p(HTML("<b>PRISM-Repurposing:</b>")),
              p(HTML("1.	Corsello, S. M. et al. Discovering the anti-cancer potential of non-oncology drugs by systematic viability profiling. <i>Nat. Cancer</i> <b>1</b>, 235–248 (2020).")),
              p(HTML("Visit their website at <a href=\"https://depmap.org/repurposing/\">https://depmap.org/repurposing/</a>."))
              )
            )      
          ),

######################################################## UI #           
          tabPanel(value = "Methods", title = "Usage License",
            list(
              h4("This will specify the license for using Simplicity and provide a link to the license terms...
                 once I bother to look it up. I believe it is GPL3 for the software itself. I don't know what
                 the licenses will be for the reprocessed datasets. I still need to hear back from the CTRPv2 and PRISM_Repurposing teams.")
            )      
          )
        ),

#############################################################                    
######################################################## UI # 
        tabPanel(value = "Download Data", title = "Download Bulk Data",
          list(
            h3("Download the bulk data used by Simplicity."),
            wellPanel(
              p(HTML("This page contains links to download the bulk data used by Simplicity. Please don't forget to visit the \"About Simplicity/Cite This Resource\" tab
              to find out how to cite Simplicity and the original creators of the data used by the app.")),
              p(HTML("By downloading this data, you agree to abide by the usage guidelines posted in the \"About Simplicity/Usage License\" tab."))
            ),
            wellPanel(
              p(HTML("<b>Harmonization Tables:</b>")),
              p(HTML("<a href=\"https://osf.io/9q4zs/download\">Harmonized_CCL_Data_v1.0.xlsx</a>: An excel spreadsheet containing information about each of the cell lines used by each dataset, along with harmonized identifiers for each cell line and the names originally used by each dataset.")),
              p(HTML("<a href=\"https://osf.io/w9h76/download\">Harmonized_Compound_Data_v1.0.xlsx</a>: An excel spreadsheet containing information about each of the compounds used by each dataset, along with harmonized identifiers for each compound and the names originally used by each dataset."))
            ),
            wellPanel(
              p(HTML("<b>Csustained Values:</b>")),
              p(HTML("<a href=\"https://osf.io/q5p8x/download\">Csustained_v1.0.xlsx</a>: An excel spreadsheet containing information about the clinically sustainable plasma concentrations (Csustained) for clinical compounds included in Simplicity. Please see the \"About Simplicity/Methods\" tab for details about how Csustained values are determined.")),
            ),
            wellPanel(
              p(HTML("<b>Dataset Summary Results:</b>")),
              p(HTML("<a href=\"https://osf.io/wm5jq/download\">CTRPv2_Results_v1.0.tsv</a>: A tab-separated value text file containing curve parameters, tested concentrations, AUC values, and IC50 values for the compound-cell line pairs tested in CTRPv2.")),
              p(HTML("<a href=\"https://osf.io/aub4p/download\">GDSC1_Results_v1.0.tsv</a>: A tab-separated value text file containing curve parameters, tested concentrations, AUC values, and IC50 values for the compound-cell line pairs tested in GDSC1.")),
              p(HTML("<a href=\"https://osf.io/tzkd2/download\">GDSC2_Results_v1.0.tsv</a>: A tab-separated value text file containing curve parameters, tested concentrations, AUC values, and IC50 values for the compound-cell line pairs tested in GDSC2.")),
              p(HTML("<a href=\"https://osf.io/awydb/download\">PRISM_Repurposing_Results_v1.0.tsv</a>: A tab-separated value text file containing curve parameters, tested concentrations, AUC values, and IC50 values for the compound-cell line pairs tested in PRISM_Repurposing."))
            ),
            wellPanel(
              p(HTML("<b>Dataset Raw Data:</b>")),
              p(HTML("<a href=\"https://osf.io/6ezmg/download\">CTRPv2_Results_v1.0.tsv.7z</a>: A 7-zip compressed, tab-separated value text file containing the raw data from CTRPv2 after compound and cell line name harmonization.")),
              p(HTML("<a href=\"https://osf.io/qmp7y/download\">GDSC1_Results_v1.0.tsv.7z</a>: A 7-zip compressed, tab-separated value text file containing the raw data from GDSC1 after compound and cell line name harmonization.")),
              p(HTML("<a href=\"https://osf.io/we7ry/download\">GDSC2_Results_v1.0.tsv.7z</a>: A 7-zip compressed, tab-separated value text file containing the raw data from GDSC2 after compound and cell line name harmonization.")),
              p(HTML("<a href=\"https://osf.io/nbjdu/download\">PRISM_Repurposing_Results_v1.0.tsv.7z</a>: A 7-zip compressed, tab-separated value text file containing the raw data from PRISM_Repurposing after compound and cell line name harmonization.")),
            ),
            wellPanel(
              p(HTML("<b>Dataset Full Curves:</b>")),
              p(HTML("The drc fit objects with full statistical information for each of the fitted curves in Simplicity can be downloaded from the <a href=\"https://osf.io/a9w5r/\">OSF repository for this project</a>."))
            )
            
          )      
        )
        
        
  ))
  
#############################################################  
#################################################### server #          
#############################################################  
#Creating server function
  server <- function(input, output, session){
    #Creating modal spinner waiting screen while app initializes
      show_modal_spinner(
        spin = "swapping-squares",
        color = "#112446",
        text = "Initializing App. Please Wait..."
      )
    #Observing help menu objects
      observe_helpers()
        
#############################################################          
#################################################### server #          
    #Code for "Explore Datasets" tab
    
      #Summary text for dataset
        output$Dataset_Summary_Text <- renderUI({
          selected_data_summary <- Dataset_Summaries[Dataset_Summaries$Dataset %in% input$Summary_Dataset,]
          list(
            p(tags$b("Full Name: "), tags$em(selected_data_summary$Dataset_Full_Name)),
            p(tags$b("Version: "), tags$em(selected_data_summary$Dataset_Version)),
            p(tags$b("Location: "), tags$em(selected_data_summary$Testing_Location)),
            p(tags$b("Dates: "), tags$em(selected_data_summary$Experiment_Dates)),
            p(tags$b("Assay Method: "), tags$em(selected_data_summary$Assay_Type)),
            p(tags$b("Plate Format: "), tags$em(selected_data_summary$Plate_Format)),
            p(tags$b("Treatment Duration: "), tags$em(selected_data_summary$Treatment_Duration)),
            p(tags$b("# of Compounds: "), tags$em(selected_data_summary$n_Compounds)),
            p(tags$b("# of Cell Lines: "), tags$em(selected_data_summary$n_Cell_Lines)),
            p(tags$b("# of Experiments: "), tags$em(selected_data_summary$n_Experiments)),
            p(tags$b("Dataset Website: "), tags$a(href = selected_data_summary$Dataset_Website, selected_data_summary$Dataset_Website))
          )
        })
      #Plot of cancer type distribution
        output$welcome_cancer_type <- renderPlotly({
          welcome_cancer_type_plots[[input$Summary_Dataset]]
        })
      #Plot of patient ages
        output$welcome_age <- renderPlotly({
          welcome_age_plots[[input$Summary_Dataset]]
        })
      #Plot of patient ancestries
        output$welcome_ancestry <- renderPlotly({
          welcome_ancestry_plots[[input$Summary_Dataset]]
        })
      #Plot of clinical phase information
        output$welcome_clinical_phase <- renderPlotly({
          welcome_clinical_phase_plots[[input$Summary_Dataset]]
        })
      #Plot of # of compounds tested per cell line in dataset
        output$n_Compounds_Per_Cell_Line <- renderPlotly({
          n_Compounds_Per_Cell_Line_Plots[[input$Summary_Dataset]]
        })
      #Plot of # of cell lines tested per compound
        output$n_Cell_Lines_Per_Compound <- renderPlotly({
          n_Cell_Lines_Per_Compound_Plots[[input$Summary_Dataset]]
        })
      #Plot of residual standard errors
        output$Dataset_Residual_Standard_Error <- renderPlotly({
          Dataset_Residual_Standard_Error_Plots[[input$Summary_Dataset]]
        })
            
#############################################################          
#################################################### server #          
      #Code for "Explore Compounds" Tab
      
        #Defining compound filter interface
          observeEvent(input$Compound_Explorer_Show_Compound_Filters, {
            isolate({
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
              isolate({
              #Updating compound filter menus
                #Molecular target menu
                  Compound_Explorer_Available_Molecular_Targets <- sort(unique(unlist(Compound_Molecular_Targets[names(Compound_Molecular_Targets) %in% Compound_Explorer_Currently_Available_Compounds()])))
                  previous_selection <- input$Compound_Explorer_Molecular_Target
                  updatePickerInput(session, "Compound_Explorer_Molecular_Target", choices = Compound_Explorer_Available_Molecular_Targets, selected = previous_selection)
                #MOA menu
                  Compound_Explorer_Available_MOAs <- sort(unique(unlist(Compound_MOAs[names(Compound_MOAs) %in% Compound_Explorer_Currently_Available_Compounds()])))
                  previous_selection <- input$Compound_Explorer_MOA
                  updatePickerInput(session, "Compound_Explorer_MOA", choices = Compound_Explorer_Available_MOAs, selected = previous_selection)
                #Clinical stage menu
                  Compound_Explorer_Available_Clinical_Phases <- sort(unique(Simple_Compound_Harm$Compound_Clinical_Phase[Simple_Compound_Harm$Harmonized_Compound_Name %in% Compound_Explorer_Currently_Available_Compounds()]))
                  previous_selection <- input$Compound_Explorer_Clinical_Phase
                  updatePickerInput(session, "Compound_Explorer_Clinical_Phase", choices = Compound_Explorer_Available_Clinical_Phases, selected = previous_selection)
                  })
            })
            
          #Updating compound selection menu
            Compound_Explorer_Selected_Compound_Option_df <- reactive({
              filtered_Compound_Option_df <- Compound_Option_df[Compound_Option_df$Harmonized_Compound_Name %in% Compound_Explorer_Currently_Available_Compounds(),]
              return(cbind(filtered_Compound_Option_df, value = filtered_Compound_Option_df$Harmonized_Compound_Name))
            })
            
            observeEvent(Compound_Explorer_Selected_Compound_Option_df(),{
              isolate({
                prev_selection <- input$compound_explorer
                if(! length(prev_selection) == 0){
                  if(! prev_selection %in% Compound_Explorer_Selected_Compound_Option_df()$Harmonized_Compound_Name){
                    prev_selection <- character(0)
                  }
                }
                updateSelectizeInput(session, "compound_explorer", label = paste0("Select a compound (n = ", nrow(Compound_Explorer_Selected_Compound_Option_df()), ")"), choices = Compound_Explorer_Selected_Compound_Option_df(), server = TRUE, selected = prev_selection)
              })
            })
            
          #Loading data for selected compound
            cpdexp_data <- eventReactive(input$compound_explorer, {
              req(input$compound_explorer)
              isolate({
                filename <- Compound_Filenames$drug_file_names[Compound_Filenames$drugs == input$compound_explorer]
                  data <- readRDS(paste0("./www/Results/", filename, ".rds"))
                  for(i in 1:length(data)){
                    data[[i]] <- data[[i]][! is.na(data[[i]]$b_c_d_e),]
                  }
                  return(data)
              })
            })
            
          #Defining cell lines available for selected compound
            Compound_Explorer_Available_Cell_Lines_for_Compound <- reactive({
              req(cpdexp_data())
  
              cell_lines <- character(0)
              for(i in 1:length(cpdexp_data())){
                cell_lines <- c(cell_lines, cpdexp_data()[[i]]$Cell_Line)
              }
              return(sort(unique(cell_lines)))
            })
            
          #Subsetting cell line info to cell lines available for this compound
            Compound_Explorer_Simple_Cell_Line_Harm_for_Compound <- reactive({
              req(Compound_Explorer_Available_Cell_Lines_for_Compound())
  
              Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% Compound_Explorer_Available_Cell_Lines_for_Compound(),]
            })
            
          #Getting age limits
            Compound_Explorer_Age_Limits <- reactive({
              req(Compound_Explorer_Simple_Cell_Line_Harm_for_Compound())
  
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
            })
            
          #Defining cell line selection interface if compound has been selected
            
            observeEvent(input$compound_explorer, {
            isolate({
              
              #Defining cell line selection interface
                output$Compound_Explorer_Cell_Line_Menu <- renderUI({
                  req(Compound_Explorer_Simple_Cell_Line_Harm_for_Compound())
  
                  Temp_CL_Harm_Data <- Compound_Explorer_Simple_Cell_Line_Harm_for_Compound()
                    temp_Cell_Line_Diseases <- strsplit(Temp_CL_Harm_Data$Diseases, ":\\|:")
                    names(temp_Cell_Line_Diseases) <- Temp_CL_Harm_Data$Harmonized_Cell_Line_ID
  
                  return_list <- list(
                    pickerInput("Compound_Explorer_Cell_Lines", label = "Select cell lines to plot data for", choices = NULL, selected = NULL, multiple = TRUE, options = list(
                      `actions-Box` = TRUE,
                      `live-Search-Style` = "contains" ,
                      `live-Search` = TRUE,
                      `live-Search-Normalize` = TRUE,
                      `selected-Text-Format` = "count"
                    )),
                    checkboxInput("Compound_Explorer_Highlight_Cell_Lines", label = "Highlight cell lines instead of filtering cell lines?", value = FALSE) %>%
                        helper(type = "inline",
                          title = "Highlight or Filter Cell Lines?",
                          icon = "question-circle", colour = NULL,
                          content = HTML("If this box is left unchecked, only selected cell lines will be shown in the plots. If this box is checked, all available cell lines will be shown in the plots, but selected cell lines will be highlighted.<br/><br/><b><u>NOTE: This options does not apply to plots of:</u></b><ul><li>Cancer Types and Genders</li><li>Cell Line Ancestries</li><li>Cell Line Ages</li></ul>These plots will show data for all available cell lines if this box is checked."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
                    checkboxInput("Compound_Explorer_Show_Cell_Line_Filters", label = "Show cell line filters?", value = FALSE)
                  )
  
                  return(return_list)
  
                })
  
              #Defining cell line filter interface
                output$Compound_Explorer_Cell_Line_Filters <- renderUI({
                  req(! length(input$Compound_Explorer_Show_Cell_Line_Filters) == 0)
                  req(Compound_Explorer_Age_Limits())
                  req(Compound_Explorer_Simple_Cell_Line_Harm_for_Compound())
                  
                  Temp_CL_Harm_Data <- Compound_Explorer_Simple_Cell_Line_Harm_for_Compound()
                    Cell_Line_Diseases <- strsplit(Temp_CL_Harm_Data$Diseases, ":\\|:")
                    names(Cell_Line_Diseases) <- Temp_CL_Harm_Data$Harmonized_Cell_Line_ID
                  if(input$Compound_Explorer_Show_Cell_Line_Filters == TRUE){
                    return_list <- list(
                      h4("Filter cell lines by:") %>%
                      helper(type = "inline",
                        title = "Cell line filtering",
                        icon = "question-circle", colour = NULL,
                        content = c("These options can be used to filter the cell line options displayed in the \"Select cell lines to plot data for\" or \"Select cell lines to highlight\" menu. Note that, once any options have been selected for a given filtering criteria, any cell lines that are missing information for that criteria will be excluded."),
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
                          content = c("Sets minimum and maximum age allowed for displayed cell line options. Note that setting the slider to anything other than its maximum range will exclude cell lines for which a numeric age could not be determined for the patient who the cell line was derived from at the time of sample collection. This both includes cases where the age is unspecified and cases where the specified age is ambiguous (i.e. such as \"Adult\"). You may download the cell line harmonization file in the \"Download Bulk Data\" tab for free-text descriptions of each cell line's patient age."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
  
                        h4("Ancestry") %>%
                        helper(type = "inline",
                          title = "Patient age filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Each slider sets the minimum and maximum % ancestry for each ancestry group. Once any of the ancestry sliders has been changed from its maximum range, cell lines will be filtered to only include lines which meet the limits set on all of the ancestry sliders. Note that % ancestry adds to 100% across all ancestry groups for each individual cell line (i.e. a cell line cannot have 60% African ancestry and 60% Native American ancestry, because that would add to >100%). Ancestry information was obtained from the cellosaurus resource at <a href=\"https://www.expasy.org/\">https://www.expasy.org/</a>."),
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
            }) #END: Isolate
            }) #END: observeEvent(input$compound_explorer, {
            
            
          #Defining currently available cell lines based on cell line filters
            Compound_Explorer_Currently_Available_Cell_Lines <- reactive({
              req(Compound_Explorer_Simple_Cell_Line_Harm_for_Compound())
              req(Compound_Explorer_Available_Cell_Lines_for_Compound())
              req(Compound_Explorer_Age_Limits())
  
              Temp_CL_Harm_Data <- Compound_Explorer_Simple_Cell_Line_Harm_for_Compound()
                temp_Cell_Line_Diseases <- strsplit(Temp_CL_Harm_Data$Diseases, ":\\|:")
                names(temp_Cell_Line_Diseases) <- Temp_CL_Harm_Data$Harmonized_Cell_Line_ID
  
              Avail_ccls <- Compound_Explorer_Available_Cell_Lines_for_Compound()
              Filtered_ccls <- character(0)
              flag <- 0
  
              if(! is.null(input$Compound_Explorer_Cancer_Type)){
                flag <- 1
                Filtered_ccls <- Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Simple_Cancer_Type %in% input$Compound_Explorer_Cancer_Type]]
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
            observeEvent(list(Compound_Explorer_Currently_Available_Cell_Lines(), input$Compound_Explorer_Highlight_Cell_Lines), {
              req(! length(input$Compound_Explorer_Highlight_Cell_Lines) == 0)
              req(Compound_Explorer_Currently_Available_Cell_Lines())
              #Updating cell line selection menu
                if(input$Compound_Explorer_Highlight_Cell_Lines == FALSE){
                  updatePickerInput(session, "Compound_Explorer_Cell_Lines", label = paste0("Select cell lines to plot data for (n = ", length(Compound_Explorer_Currently_Available_Cell_Lines()), ")"), choices = Compound_Explorer_Currently_Available_Cell_Lines(), selected = Compound_Explorer_Currently_Available_Cell_Lines())
                } else {
                  updatePickerInput(session, "Compound_Explorer_Cell_Lines", label = paste0("Select cell lines to highlight (n = ", length(Compound_Explorer_Currently_Available_Cell_Lines()), ")"), choices = Compound_Explorer_Currently_Available_Cell_Lines(), selected = Compound_Explorer_Currently_Available_Cell_Lines())
                }
            })
            
          #Determining which datasets contain data for the selected compound
            cpdexplr_ccl_availability_data <- reactive({
              req(cpdexp_data())
              req(! length(input$Compound_Explorer_Highlight_Cell_Lines) == 0)
              
              if(input$Compound_Explorer_Highlight_Cell_Lines == FALSE){
                temp_cpdexplr_ccl_availability_data <- lapply(cpdexp_data(), function(x){return(sort(unique(x$Cell_Line)))})
                for(i in 1:length(temp_cpdexplr_ccl_availability_data)){
                  temp_cpdexplr_ccl_availability_data[[i]] <- temp_cpdexplr_ccl_availability_data[[i]][temp_cpdexplr_ccl_availability_data[[i]] %in% input$Compound_Explorer_Cell_Lines]
                }
              } else {
                temp_cpdexplr_ccl_availability_data <- lapply(cpdexp_data(), function(x){return(sort(unique(x$Cell_Line)))})
              }
              return(temp_cpdexplr_ccl_availability_data)
            })
  
            Compound_Explorer_Datasets_with_Compound_Data <- reactive({
              req(cpdexplr_ccl_availability_data())
              
              temp_cpdexplr_ccl_availability_data <-  cpdexplr_ccl_availability_data()
              return(names(temp_cpdexplr_ccl_availability_data)[sapply(temp_cpdexplr_ccl_availability_data, length) > 0])
            })
  
            Compound_Explorer_Data <- reactive({
              req(input$compound_explorer)
              req(Compound_Explorer_Datasets_with_Compound_Data())
              
              temp_cpdexplr_data <- Compound_Harm[Compound_Harm$Harmonized_Compound_Name %in% input$compound_explorer,]
              return(temp_cpdexplr_data[temp_cpdexplr_data$Dataset %in% Compound_Explorer_Datasets_with_Compound_Data(),])
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
            req(Compound_Explorer_Data())
            
            #Creating table with the name used for this compound in each dataset
              explorer_name_table <- Compound_Explorer_Data()[,c("Dataset", "Compound_Name_in_Dataset")]
              colnames(explorer_name_table) <- c("Dataset", "Name in Dataset")
              return(explorer_name_table)
          })
    
                  
        #Generating plots
          observe({
            req(Compound_Explorer_Datasets_with_Compound_Data())
            req(cpdexp_data())
            req(cpdexplr_ccl_availability_data())
            req(! length(input$Compound_Explorer_Highlight_Cell_Lines) == 0)

            output$Compound_Explorer_Plots <- renderUI({
              #Rendering plot UI for dataset with data for this compound
                Compound_Explorer_Plot_UI <- vector(mode = "list")
                if("CTRPv2" %in% Compound_Explorer_Datasets_with_Compound_Data()){
                  Compound_Explorer_Plot_UI <- c(Compound_Explorer_Plot_UI, plotlyOutput(outputId = "Compound_Explorer_CTRPv2_Plot"), HTML("---"))
                }
                if("GDSC1" %in% Compound_Explorer_Datasets_with_Compound_Data()){
                  Compound_Explorer_Plot_UI <- c(Compound_Explorer_Plot_UI, plotlyOutput(outputId = "Compound_Explorer_GDSC1_Plot"), HTML("---"))
                }
                if("GDSC2" %in% Compound_Explorer_Datasets_with_Compound_Data()){
                  Compound_Explorer_Plot_UI <- c(Compound_Explorer_Plot_UI, plotlyOutput(outputId = "Compound_Explorer_GDSC2_Plot"), HTML("---"))
                }
                if("PRISM_Repurposing" %in% Compound_Explorer_Datasets_with_Compound_Data()){
                  Compound_Explorer_Plot_UI <- c(Compound_Explorer_Plot_UI, plotlyOutput(outputId = "Compound_Explorer_PRISM_Repurposing_Plot"))
                }
              Compound_Explorer_Plot_UI
            })
  
            output$Compound_Explorer_CTRPv2_Plot <- renderPlotly({
              #Loading raw data for this compound and any datasets with data for this compound
                if("CTRPv2" %in% Compound_Explorer_Datasets_with_Compound_Data()){
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
                    
                    colors <- setNames(rep(colorRampPalette(c("blue", "red"))(2), 3),
                                       c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range",
                                         "selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range",
                                         "unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                    
                    if(all(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration %in% "Max Tested Concentration Within AUC Range")){
                      colors <- colorRampPalette(c("blue"))(1)
                    } else if(all(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration %in% "Max Tested Concentration < AUC Range")){
                      colors <- colorRampPalette(c("red"))(1)
                    } else {
                      colors <- colorRampPalette(c("blue", "red"))(2)
                    }

                    if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                      plot_data$symbol <- ": not selected"
                      plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                      selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                      unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                      
                      if(nrow(selected_plot_data) > 0){
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "selected: Max Tested Concentration Within AUC Range"
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[selected_plot_data$max_dose_uM < selected_plot_data$max_mode_ccl_CTRPv2_conc] <- "selected: Max Tested Concentration < AUC Range"
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range"))
                        fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                   y = selected_plot_data$AUC_mode_ccl_CTRPv2_conc,
                                   color = selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   type = "scatter",
                                   mode = "markers")
                        if(nrow(unselected_plot_data) > 0){
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "unselected: Max Tested Concentration Within AUC Range"
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[unselected_plot_data$max_dose_uM < unselected_plot_data$max_mode_ccl_CTRPv2_conc] <- "unselected: Max Tested Concentration < AUC Range"
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                          fig <- add_trace(fig, 
                                    x = unselected_plot_data$Cell_Line,
                                    y = unselected_plot_data$AUC_mode_ccl_CTRPv2_conc,
                                    color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                    opacity = 0.2)
                        }
                      } else if(nrow(unselected_plot_data) > 0){
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "unselected: Max Tested Concentration Within AUC Range"
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[unselected_plot_data$max_dose_uM < unselected_plot_data$max_mode_ccl_CTRPv2_conc] <- "unselected: Max Tested Concentration < AUC Range"
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                        fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                   y = unselected_plot_data$AUC_mode_ccl_CTRPv2_conc,
                                   color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   opacity = 0.2,
                                   type = "scatter",
                                   mode = "markers")
                      }
                        
                      fig <- layout(fig,
                                    title = "CTRPv2 AUCs",
                                    xaxis = list(title = paste0("CTRPv2 Cell Lines (n = ", nrow(plot_data), "; ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    } else {

                      fig <- plot_ly(x = plot_data$Cell_Line,
                                   y = plot_data$AUC_mode_ccl_CTRPv2_conc,
                                   color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   type = "scatter",
                                   mode = "markers")
                      fig <- layout(fig,
                                    title = "CTRPv2 AUCs",
                                    xaxis = list(title = paste0("CTRPv2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    }

                    fig
                    
                  } else if(input$Compound_Explorer_to_Plot == "AUC values for concentration range available for all tested cell lines"){
                    plot_data <- CTRPv2_Results[! is.na(CTRPv2_Results$AUC_all_ccl_CTRPv2_conc),]
                    plot_data <- plot_data[order(plot_data$AUC_all_ccl_CTRPv2_conc, decreasing = FALSE),]
                    plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                    ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_all_ccl_CTRPv2_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_all_ccl_CTRPv2_conc)), 3), " microMolar)")
                    
                    if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                      plot_data$symbol <- ": not selected"
                      plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                      selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                      unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                      
                      if(nrow(selected_plot_data) > 0){
                        fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                     y = selected_plot_data$AUC_all_ccl_CTRPv2_conc,
                                     type = "scatter",
                                     color = factor(rep("A", nrow(selected_plot_data))),
                                     colors = colorRampPalette(c("blue"))(1),
                                     mode = "markers",
                                     name = "selected")
                        if(nrow(unselected_plot_data) > 0){
                          fig <- add_trace(fig, x = unselected_plot_data$Cell_Line,
                                       y = unselected_plot_data$AUC_all_ccl_CTRPv2_conc,
                                       color = factor(rep("A", nrow(unselected_plot_data))),
                                       name = "unselected",
                                       opacity = 0.2)
                        }
                      } else {
                        fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                     y = unselected_plot_data$AUC_all_ccl_CTRPv2_conc,
                                     type = "scatter",
                                     color = factor(rep("A", nrow(unselected_plot_data))),
                                     colors = colorRampPalette(c("blue"))(1),
                                     opacity = 0.2,
                                     mode = "markers",
                                     name = "selected")
                      }
                      
                      fig <- layout(fig,
                                    title = "CTRPv2 AUCs",
                                    xaxis = list(title = paste0("CTRPv2 Cell Lines (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                      
                    } else {
                      fig <- plot_ly(x = plot_data$Cell_Line,
                                   y = plot_data$AUC_all_ccl_CTRPv2_conc,
                                   color = factor(rep("A", nrow(plot_data))),
                                   colors = colorRampPalette(c("blue"))(1),
                                   type = "scatter",
                                   mode = "markers")
                      
                      fig <- layout(fig,
                                    title = "CTRPv2 AUCs",
                                    xaxis = list(title = paste0("CTRPv2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    }
                    
                    
                    fig
                    
                  } else if(input$Compound_Explorer_to_Plot == "IC50 values"){
                    plot_data <- CTRPv2_Results[! is.na(CTRPv2_Results$IC50),]
                    if(nrow(plot_data) > 0){
                      plot_data <- plot_data[order(plot_data$IC50, decreasing = FALSE),]
                      plot_data$Group <- "IC50 <= max tested concentration"
                      plot_data$Group[plot_data$IC50 > plot_data$max_dose_uM] <- "IC50 > max tested concentration"
                      plot_data$Group[plot_data$IC50 == Inf] <- "Infinite IC50"
                      if(any(plot_data$IC50 != Inf)){
                        plot_data$IC50[plot_data$IC50 == Inf] <- max(plot_data$IC50[! plot_data$IC50 == Inf])
                      } else {
                        plot_data$IC50[plot_data$IC50 == Inf] <- max(plot_data$max_dose_uM)+1
                      }
                      
                      colors <- setNames(rep(colorRampPalette(c("blue", "red", "lightgray"))(3), 3),
                                         c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50",
                                           "selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50",
                                           "unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))

                      plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                      ylab <- "IC50 (microMolar)"
                      
                      if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                        plot_data$symbol <- ": not selected"
                        plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                        selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                        unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                        
                        if(nrow(selected_plot_data) > 0){
                          selected_plot_data$Group <- paste0("selected: ", selected_plot_data$Group)
                          selected_plot_data$Group <- factor(selected_plot_data$Group, levels = c("selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50"))
                          fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                         y = selected_plot_data$IC50,
                                         color = selected_plot_data$Group,
                                         type = "scatter",
                                         mode = "markers",
                                         colors = colors)
                          if(nrow(unselected_plot_data) > 0){
                            unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                            unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                            fig <- add_trace(fig,
                                             x = unselected_plot_data$Cell_Line,
                                             y = unselected_plot_data$IC50,
                                             color = unselected_plot_data$Group,
                                             opacity = 0.2)
                          }
                        } else {
                          unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                          unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                          fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                         y = unselected_plot_data$IC50,
                                         type = "scatter",
                                         mode = "markers",
                                         color = unselected_plot_data$Group,
                                         colors = colors,
                                         opacity = 0.2)
                        }
                        
                        fig <- layout(fig,
                                      title = "CTRPv2 IC50s",
                                      xaxis = list(title = paste0("CTRPv2 Cell Lines (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab, type = "log"))
                        
                      } else {
                        plot_data$Group <- factor(plot_data$Group, levels = c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50"))
                        fig <- plot_ly(x = plot_data$Cell_Line,
                                     y = plot_data$IC50,
                                     type = "scatter",
                                     mode = "markers",
                                     color = plot_data$Group,
                                     colors = colors)
                        
                        fig <- layout(fig,
                                      title = "CTRPv2 IC50s",
                                      xaxis = list(title = paste0("CTRPv2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                      yaxis = list(title = ylab, type = "log"))
                      }
                      
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Cancer Types & Genders"){
                    ccls <- unique(CTRPv2_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      temp_dataset_ccl_data$Simple_Cancer_Type[temp_dataset_ccl_data$Simple_Cancer_Type == "unknown"] <- "unknown cancer type"
                      plot_data <- as.data.frame.table(table(temp_dataset_ccl_data$Simple_Cancer_Type))
                      plot_data <- plot_data[order(plot_data$Freq, decreasing = TRUE),]
                      plot_data <- rbind(plot_data[! plot_data$Var1 == "unknown cancer type",], plot_data[plot_data$Var1 == "unknown cancer type",])
                      
                      Gender_Unknown <- NA
                      Gender_Female <- NA
                      Gender_Male <- NA
                      for(j in 1:nrow(plot_data)){
                        Gender_Unknown[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Sex unspecified",])
                        Gender_Female[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Female",])
                        Gender_Male[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Male",])
                      }
                      
                      plot_data$Var1 <- factor(plot_data$Var1, levels = plot_data$Var1)
                      
                      fig <- plot_ly(x = plot_data$Var1, y = Gender_Unknown, type = "bar", name = "Unknown Gender", marker = list(color = "lightgray")) %>%
                              add_trace(y = Gender_Male, name = "Male", marker = list(color = rgb(65,105,225, maxColorValue = 255))) %>%
                              add_trace(y = Gender_Female, name = "Female", marker = list(color = rgb(186,85,211, maxColorValue = 255))) %>%
                              layout(title = 'CTRPv2 Cell Line Cancer Types/Genders', yaxis = list(title = "# of Cell Lines"), xaxis = list(tickangle = 45), barmode = "stack", margin = list(b = 150, l = 50))
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Ancestries"){
                    ccls <- unique(CTRPv2_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      completeness <- paste0("(data for ", nrow(temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$African_Ancestry),]), " of ", nrow(temp_dataset_ccl_data), " cell lines)")
                      temp_dataset_ccl_data <- temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$African_Ancestry),]
                      
                      plot_data <- temp_dataset_ccl_data[,grepl("_Ancestry", colnames(temp_dataset_ccl_data))]*100
                      rownames(plot_data) <- temp_dataset_ccl_data$Harmonized_Cell_Line_ID
                      
                      if(nrow(plot_data) > 2){
                        plot_data <- plot_data[hclust(dist(plot_data))$order,]
                      }
                      colnames(plot_data) <- gsub("_Ancestry", "", colnames(plot_data))
                      colnames(plot_data) <- gsub("_", " ", colnames(plot_data))
                      x <- factor(rownames(plot_data), levels = rownames(plot_data))
                      
                      if(nrow(plot_data) > 0){
                        plot_colors <- c("#6A3D9A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#A6CEE3", "#1F78B4")
                        fig <- plot_ly(x = x, y = plot_data[,1], name = colnames(plot_data)[1], type = "bar", marker = list(color = plot_colors[1])) %>%
                          add_trace(y = plot_data[,2], name = colnames(plot_data)[2], marker = list(color = plot_colors[2])) %>%
                          add_trace(y = plot_data[,3], name = colnames(plot_data)[3], marker = list(color = plot_colors[3])) %>%
                          add_trace(y = plot_data[,4], name = colnames(plot_data)[4], marker = list(color = plot_colors[4])) %>%
                          add_trace(y = plot_data[,5], name = colnames(plot_data)[5], marker = list(color = plot_colors[5])) %>%
                          add_trace(y = plot_data[,6], name = colnames(plot_data)[6], marker = list(color = plot_colors[6])) %>%
                          add_trace(y = plot_data[,7], name = colnames(plot_data)[7], marker = list(color = plot_colors[7])) %>%
                          layout(title = 'CTRPv2 Ethnicities', barmode = "stack", xaxis = list(title = paste("Tested Cell Lines", completeness), showticklabels = FALSE), yaxis = list(title = "% Ancestry Makeup"), bargap = 0, legend = list(traceorder = "normal"))
                      } else {
                        fig <- plot_ly(x = 1, y = 100, name = "No Values", type = "bar", marker = list(color = "white")) %>%
                        layout(title = 'CTRPv2 Cell Line Ethnicities', barmode = "stack", xaxis = list(title = paste("Tested Cell Lines", completeness), showticklabels = FALSE), yaxis = list(title = "% Ancestry Makeup"), bargap = 0, legend = list(traceorder = "normal"))
                      }
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Ages"){
                    ccls <- unique(CTRPv2_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      completeness <- paste0("(data for ", nrow(temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$Numeric_Age_in_Years),]), " of ", nrow(temp_dataset_ccl_data), " cell lines)")
                      temp_dataset_ccl_data <- temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$Numeric_Age_in_Years),]
                      
                      if(nrow(temp_dataset_ccl_data) > 1){
                        p <- ggplot(temp_dataset_ccl_data, aes(x = Numeric_Age_in_Years, y = after_stat(scaled))) +
                                    geom_density(color = "darkblue", fill = "lightblue") +
                                    theme_light() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "Scaled Density") +
                                    ggtitle("CTRPv2 Ages")
                      } else if(nrow(temp_dataset_ccl_data) == 1){
                        p <- ggplot(temp_dataset_ccl_data, aes(x = Numeric_Age_in_Years, y = Harmonized_Cell_Line_ID)) +
                                    geom_bar(stat="identity") +
                                    theme_light() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "") +
                                    ggtitle("CTRPv2 Ages")
                      } else {
                        p <- ggplot(data.frame(x = c(0,100), y = c(0,1)), aes(x = x, y = y)) +
                                    geom_blank() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "Scaled Density") +
                                    ggtitle("CTRPv2 Cell Line Patient Age Distribution")
                      }
                  
                      fig <- ggplotly(p)
                      fig
                    }
                  }
                }
            })
            
            output$Compound_Explorer_GDSC1_Plot <- renderPlotly({
              #Loading raw data for this compound and any datasets with data for this compound
                if("GDSC1" %in% Compound_Explorer_Datasets_with_Compound_Data()){
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
                    
                    colors <- setNames(rep(colorRampPalette(c("blue", "red"))(2), 3),
                                       c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range",
                                         "selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range",
                                         "unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                    
                    if(all(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration %in% "Max Tested Concentration Within AUC Range")){
                      colors <- colorRampPalette(c("blue"))(1)
                    } else if(all(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration %in% "Max Tested Concentration < AUC Range")){
                      colors <- colorRampPalette(c("red"))(1)
                    } else {
                      colors <- colorRampPalette(c("blue", "red"))(2)
                    }

                    if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                      plot_data$symbol <- ": not selected"
                      plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                      selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                      unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                      
                      if(nrow(selected_plot_data) > 0){
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "selected: Max Tested Concentration Within AUC Range"
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[selected_plot_data$max_dose_uM < selected_plot_data$max_mode_ccl_GDSC1_conc] <- "selected: Max Tested Concentration < AUC Range"
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range"))
                        fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                   y = selected_plot_data$AUC_mode_ccl_GDSC1_conc,
                                   color = selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   type = "scatter",
                                   mode = "markers")
                        if(nrow(unselected_plot_data) > 0){
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "unselected: Max Tested Concentration Within AUC Range"
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[unselected_plot_data$max_dose_uM < unselected_plot_data$max_mode_ccl_GDSC1_conc] <- "unselected: Max Tested Concentration < AUC Range"
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                          fig <- add_trace(fig, 
                                    x = unselected_plot_data$Cell_Line,
                                    y = unselected_plot_data$AUC_mode_ccl_GDSC1_conc,
                                    color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                    opacity = 0.2)
                        }
                      } else if(nrow(unselected_plot_data) > 0){
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "unselected: Max Tested Concentration Within AUC Range"
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[unselected_plot_data$max_dose_uM < unselected_plot_data$max_mode_ccl_GDSC1_conc] <- "unselected: Max Tested Concentration < AUC Range"
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                        fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                   y = unselected_plot_data$AUC_mode_ccl_GDSC1_conc,
                                   color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   opacity = 0.2,
                                   type = "scatter",
                                   mode = "markers")
                      }
                        
                      fig <- layout(fig,
                                    title = "GDSC1 AUCs",
                                    xaxis = list(title = paste0("GDSC1 Cell Lines (n = ", nrow(plot_data), "; ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    } else {

                      fig <- plot_ly(x = plot_data$Cell_Line,
                                   y = plot_data$AUC_mode_ccl_GDSC1_conc,
                                   color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   type = "scatter",
                                   mode = "markers")
                      fig <- layout(fig,
                                    title = "GDSC1 AUCs",
                                    xaxis = list(title = paste0("GDSC1 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    }

                    fig
                    
                  } else if(input$Compound_Explorer_to_Plot == "AUC values for concentration range available for all tested cell lines"){
                    plot_data <- GDSC1_Results[! is.na(GDSC1_Results$AUC_all_ccl_GDSC1_conc),]
                    plot_data <- plot_data[order(plot_data$AUC_all_ccl_GDSC1_conc, decreasing = FALSE),]
                    plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                    ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_all_ccl_GDSC1_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_all_ccl_GDSC1_conc)), 3), " microMolar)")
                    
                    if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                      plot_data$symbol <- ": not selected"
                      plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                      selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                      unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                      
                      if(nrow(selected_plot_data) > 0){
                        fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                     y = selected_plot_data$AUC_all_ccl_GDSC1_conc,
                                     type = "scatter",
                                     color = factor(rep("A", nrow(selected_plot_data))),
                                     colors = colorRampPalette(c("blue"))(1),
                                     mode = "markers",
                                     name = "selected")
                        if(nrow(unselected_plot_data) > 0){
                          fig <- add_trace(fig, x = unselected_plot_data$Cell_Line,
                                       y = unselected_plot_data$AUC_all_ccl_GDSC1_conc,
                                       color = factor(rep("A", nrow(unselected_plot_data))),
                                       name = "unselected",
                                       opacity = 0.2)
                        }
                      } else {
                        fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                     y = unselected_plot_data$AUC_all_ccl_GDSC1_conc,
                                     type = "scatter",
                                     color = factor(rep("A", nrow(unselected_plot_data))),
                                     colors = colorRampPalette(c("blue"))(1),
                                     opacity = 0.2,
                                     mode = "markers",
                                     name = "selected")
                      }
                      
                      fig <- layout(fig,
                                    title = "GDSC1 AUCs",
                                    xaxis = list(title = paste0("GDSC1 Cell Lines (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                      
                    } else {
                      fig <- plot_ly(x = plot_data$Cell_Line,
                                   y = plot_data$AUC_all_ccl_GDSC1_conc,
                                   color = factor(rep("A", nrow(plot_data))),
                                   colors = colorRampPalette(c("blue"))(1),
                                   type = "scatter",
                                   mode = "markers")
                      
                      fig <- layout(fig,
                                    title = "GDSC1 AUCs",
                                    xaxis = list(title = paste0("GDSC1 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    }
                    
                    
                    fig
                    
                  } else if(input$Compound_Explorer_to_Plot == "IC50 values"){
                    plot_data <- GDSC1_Results[! is.na(GDSC1_Results$IC50),]
                    if(nrow(plot_data) > 0){
                      plot_data <- plot_data[order(plot_data$IC50, decreasing = FALSE),]
                      plot_data$Group <- "IC50 <= max tested concentration"
                      plot_data$Group[plot_data$IC50 > plot_data$max_dose_uM] <- "IC50 > max tested concentration"
                      plot_data$Group[plot_data$IC50 == Inf] <- "Infinite IC50"
                      if(any(plot_data$IC50 != Inf)){
                        plot_data$IC50[plot_data$IC50 == Inf] <- max(plot_data$IC50[! plot_data$IC50 == Inf])
                      } else {
                        plot_data$IC50[plot_data$IC50 == Inf] <- max(plot_data$max_dose_uM)+1
                      }
                      
                      colors <- setNames(rep(colorRampPalette(c("blue", "red", "lightgray"))(3), 3),
                                         c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50",
                                           "selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50",
                                           "unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))

                      plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                      ylab <- "IC50 (microMolar)"
                      
                      if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                        plot_data$symbol <- ": not selected"
                        plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                        selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                        unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                        
                        if(nrow(selected_plot_data) > 0){
                          selected_plot_data$Group <- paste0("selected: ", selected_plot_data$Group)
                          selected_plot_data$Group <- factor(selected_plot_data$Group, levels = c("selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50"))
                          fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                         y = selected_plot_data$IC50,
                                         color = selected_plot_data$Group,
                                         type = "scatter",
                                         mode = "markers",
                                         colors = colors)
                          if(nrow(unselected_plot_data) > 0){
                            unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                            unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                            fig <- add_trace(fig,
                                             x = unselected_plot_data$Cell_Line,
                                             y = unselected_plot_data$IC50,
                                             color = unselected_plot_data$Group,
                                             opacity = 0.2)
                          }
                        } else {
                          unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                          unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                          fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                         y = unselected_plot_data$IC50,
                                         type = "scatter",
                                         mode = "markers",
                                         color = unselected_plot_data$Group,
                                         colors = colors,
                                         opacity = 0.2)
                        }
                        
                        fig <- layout(fig,
                                      title = "GDSC1 IC50s",
                                      xaxis = list(title = paste0("GDSC1 Cell Lines (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab, type = "log"))
                        
                      } else {
                        plot_data$Group <- factor(plot_data$Group, levels = c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50"))
                        fig <- plot_ly(x = plot_data$Cell_Line,
                                     y = plot_data$IC50,
                                     type = "scatter",
                                     mode = "markers",
                                     color = plot_data$Group,
                                     colors = colors)
                        
                        fig <- layout(fig,
                                      title = "GDSC1 IC50s",
                                      xaxis = list(title = paste0("GDSC1 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                      yaxis = list(title = ylab, type = "log"))
                      }
                      
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Cancer Types & Genders"){
                    ccls <- unique(GDSC1_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      temp_dataset_ccl_data$Simple_Cancer_Type[temp_dataset_ccl_data$Simple_Cancer_Type == "unknown"] <- "unknown cancer type"
                      plot_data <- as.data.frame.table(table(temp_dataset_ccl_data$Simple_Cancer_Type))
                      plot_data <- plot_data[order(plot_data$Freq, decreasing = TRUE),]
                      plot_data <- rbind(plot_data[! plot_data$Var1 == "unknown cancer type",], plot_data[plot_data$Var1 == "unknown cancer type",])
                      
                      Gender_Unknown <- NA
                      Gender_Female <- NA
                      Gender_Male <- NA
                      for(j in 1:nrow(plot_data)){
                        Gender_Unknown[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Sex unspecified",])
                        Gender_Female[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Female",])
                        Gender_Male[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Male",])
                      }
                      
                      plot_data$Var1 <- factor(plot_data$Var1, levels = plot_data$Var1)
                      
                      fig <- plot_ly(x = plot_data$Var1, y = Gender_Unknown, type = "bar", name = "Unknown Gender", marker = list(color = "lightgray")) %>%
                              add_trace(y = Gender_Male, name = "Male", marker = list(color = rgb(65,105,225, maxColorValue = 255))) %>%
                              add_trace(y = Gender_Female, name = "Female", marker = list(color = rgb(186,85,211, maxColorValue = 255))) %>%
                              layout(title = 'GDSC1 Cell Line Cancer Types/Genders', yaxis = list(title = "# of Cell Lines"), xaxis = list(tickangle = 45), barmode = "stack", margin = list(b = 150, l = 50))
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Ancestries"){
                    ccls <- unique(GDSC1_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      completeness <- paste0("(data for ", nrow(temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$African_Ancestry),]), " of ", nrow(temp_dataset_ccl_data), " cell lines)")
                      temp_dataset_ccl_data <- temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$African_Ancestry),]
                      
                      plot_data <- temp_dataset_ccl_data[,grepl("_Ancestry", colnames(temp_dataset_ccl_data))]*100
                      rownames(plot_data) <- temp_dataset_ccl_data$Harmonized_Cell_Line_ID
                      
                      if(nrow(plot_data) > 2){
                        plot_data <- plot_data[hclust(dist(plot_data))$order,]
                      }
                      colnames(plot_data) <- gsub("_Ancestry", "", colnames(plot_data))
                      colnames(plot_data) <- gsub("_", " ", colnames(plot_data))
                      x <- factor(rownames(plot_data), levels = rownames(plot_data))
                      
                      if(nrow(plot_data) > 0){
                        plot_colors <- c("#6A3D9A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#A6CEE3", "#1F78B4")
                        fig <- plot_ly(x = x, y = plot_data[,1], name = colnames(plot_data)[1], type = "bar", marker = list(color = plot_colors[1])) %>%
                          add_trace(y = plot_data[,2], name = colnames(plot_data)[2], marker = list(color = plot_colors[2])) %>%
                          add_trace(y = plot_data[,3], name = colnames(plot_data)[3], marker = list(color = plot_colors[3])) %>%
                          add_trace(y = plot_data[,4], name = colnames(plot_data)[4], marker = list(color = plot_colors[4])) %>%
                          add_trace(y = plot_data[,5], name = colnames(plot_data)[5], marker = list(color = plot_colors[5])) %>%
                          add_trace(y = plot_data[,6], name = colnames(plot_data)[6], marker = list(color = plot_colors[6])) %>%
                          add_trace(y = plot_data[,7], name = colnames(plot_data)[7], marker = list(color = plot_colors[7])) %>%
                          layout(title = 'GDSC1 Ethnicities', barmode = "stack", xaxis = list(title = paste("Tested Cell Lines", completeness), showticklabels = FALSE), yaxis = list(title = "% Ancestry Makeup"), bargap = 0, legend = list(traceorder = "normal"))
                      } else {
                        fig <- plot_ly(x = 1, y = 100, name = "No Values", type = "bar", marker = list(color = "white")) %>%
                        layout(title = 'GDSC1 Cell Line Ethnicities', barmode = "stack", xaxis = list(title = paste("Tested Cell Lines", completeness), showticklabels = FALSE), yaxis = list(title = "% Ancestry Makeup"), bargap = 0, legend = list(traceorder = "normal"))
                      }
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Ages"){
                    ccls <- unique(GDSC1_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      completeness <- paste0("(data for ", nrow(temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$Numeric_Age_in_Years),]), " of ", nrow(temp_dataset_ccl_data), " cell lines)")
                      temp_dataset_ccl_data <- temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$Numeric_Age_in_Years),]
                      
                      if(nrow(temp_dataset_ccl_data) > 1){
                        p <- ggplot(temp_dataset_ccl_data, aes(x = Numeric_Age_in_Years, y = after_stat(scaled))) +
                                    geom_density(color = "darkblue", fill = "lightblue") +
                                    theme_light() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "Scaled Density") +
                                    ggtitle("GDSC1 Ages")
                      } else if(nrow(temp_dataset_ccl_data) == 1){
                        p <- ggplot(temp_dataset_ccl_data, aes(x = Numeric_Age_in_Years, y = Harmonized_Cell_Line_ID)) +
                                    geom_bar(stat="identity") +
                                    theme_light() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "") +
                                    ggtitle("GDSC1 Ages")
                      } else {
                        p <- ggplot(data.frame(x = c(0,100), y = c(0,1)), aes(x = x, y = y)) +
                                    geom_blank() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "Scaled Density") +
                                    ggtitle("GDSC1 Cell Line Patient Age Distribution")
                      }
                  
                      fig <- ggplotly(p)
                      fig
                    }
                  }
                }
            })
            
            output$Compound_Explorer_GDSC2_Plot <- renderPlotly({
              #Loading raw data for this compound and any datasets with data for this compound
                if("GDSC2" %in% Compound_Explorer_Datasets_with_Compound_Data()){
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
                    
                    colors <- setNames(rep(colorRampPalette(c("blue", "red"))(2), 3),
                                       c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range",
                                         "selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range",
                                         "unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                    
                    if(all(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration %in% "Max Tested Concentration Within AUC Range")){
                      colors <- colorRampPalette(c("blue"))(1)
                    } else if(all(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration %in% "Max Tested Concentration < AUC Range")){
                      colors <- colorRampPalette(c("red"))(1)
                    } else {
                      colors <- colorRampPalette(c("blue", "red"))(2)
                    }

                    if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                      plot_data$symbol <- ": not selected"
                      plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                      selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                      unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                      
                      if(nrow(selected_plot_data) > 0){
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "selected: Max Tested Concentration Within AUC Range"
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[selected_plot_data$max_dose_uM < selected_plot_data$max_mode_ccl_GDSC2_conc] <- "selected: Max Tested Concentration < AUC Range"
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range"))
                        fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                   y = selected_plot_data$AUC_mode_ccl_GDSC2_conc,
                                   color = selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   type = "scatter",
                                   mode = "markers")
                        if(nrow(unselected_plot_data) > 0){
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "unselected: Max Tested Concentration Within AUC Range"
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[unselected_plot_data$max_dose_uM < unselected_plot_data$max_mode_ccl_GDSC2_conc] <- "unselected: Max Tested Concentration < AUC Range"
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                          fig <- add_trace(fig, 
                                    x = unselected_plot_data$Cell_Line,
                                    y = unselected_plot_data$AUC_mode_ccl_GDSC2_conc,
                                    color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                    opacity = 0.2)
                        }
                      } else if(nrow(unselected_plot_data) > 0){
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "unselected: Max Tested Concentration Within AUC Range"
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[unselected_plot_data$max_dose_uM < unselected_plot_data$max_mode_ccl_GDSC2_conc] <- "unselected: Max Tested Concentration < AUC Range"
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                        fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                   y = unselected_plot_data$AUC_mode_ccl_GDSC2_conc,
                                   color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   opacity = 0.2,
                                   type = "scatter",
                                   mode = "markers")
                      }
                        
                      fig <- layout(fig,
                                    title = "GDSC2 AUCs",
                                    xaxis = list(title = paste0("GDSC2 Cell Lines (n = ", nrow(plot_data), "; ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    } else {

                      fig <- plot_ly(x = plot_data$Cell_Line,
                                   y = plot_data$AUC_mode_ccl_GDSC2_conc,
                                   color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   type = "scatter",
                                   mode = "markers")
                      fig <- layout(fig,
                                    title = "GDSC2 AUCs",
                                    xaxis = list(title = paste0("GDSC2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    }

                    fig
                    
                  } else if(input$Compound_Explorer_to_Plot == "AUC values for concentration range available for all tested cell lines"){
                    plot_data <- GDSC2_Results[! is.na(GDSC2_Results$AUC_all_ccl_GDSC2_conc),]
                    plot_data <- plot_data[order(plot_data$AUC_all_ccl_GDSC2_conc, decreasing = FALSE),]
                    plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                    ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_all_ccl_GDSC2_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_all_ccl_GDSC2_conc)), 3), " microMolar)")
                    
                    if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                      plot_data$symbol <- ": not selected"
                      plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                      selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                      unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                      
                      if(nrow(selected_plot_data) > 0){
                        fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                     y = selected_plot_data$AUC_all_ccl_GDSC2_conc,
                                     type = "scatter",
                                     color = factor(rep("A", nrow(selected_plot_data))),
                                     colors = colorRampPalette(c("blue"))(1),
                                     mode = "markers",
                                     name = "selected")
                        if(nrow(unselected_plot_data) > 0){
                          fig <- add_trace(fig, x = unselected_plot_data$Cell_Line,
                                       y = unselected_plot_data$AUC_all_ccl_GDSC2_conc,
                                       color = factor(rep("A", nrow(unselected_plot_data))),
                                       name = "unselected",
                                       opacity = 0.2)
                        }
                      } else {
                        fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                     y = unselected_plot_data$AUC_all_ccl_GDSC2_conc,
                                     type = "scatter",
                                     color = factor(rep("A", nrow(unselected_plot_data))),
                                     colors = colorRampPalette(c("blue"))(1),
                                     opacity = 0.2,
                                     mode = "markers",
                                     name = "selected")
                      }
                      
                      fig <- layout(fig,
                                    title = "GDSC2 AUCs",
                                    xaxis = list(title = paste0("GDSC2 Cell Lines (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                      
                    } else {
                      fig <- plot_ly(x = plot_data$Cell_Line,
                                   y = plot_data$AUC_all_ccl_GDSC2_conc,
                                   color = factor(rep("A", nrow(plot_data))),
                                   colors = colorRampPalette(c("blue"))(1),
                                   type = "scatter",
                                   mode = "markers")
                      
                      fig <- layout(fig,
                                    title = "GDSC2 AUCs",
                                    xaxis = list(title = paste0("GDSC2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    }
                    
                    
                    fig
                    
                  } else if(input$Compound_Explorer_to_Plot == "IC50 values"){
                    plot_data <- GDSC2_Results[! is.na(GDSC2_Results$IC50),]
                    if(nrow(plot_data) > 0){
                      plot_data <- plot_data[order(plot_data$IC50, decreasing = FALSE),]
                      plot_data$Group <- "IC50 <= max tested concentration"
                      plot_data$Group[plot_data$IC50 > plot_data$max_dose_uM] <- "IC50 > max tested concentration"
                      plot_data$Group[plot_data$IC50 == Inf] <- "Infinite IC50"
                      if(any(plot_data$IC50 != Inf)){
                        plot_data$IC50[plot_data$IC50 == Inf] <- max(plot_data$IC50[! plot_data$IC50 == Inf])
                      } else {
                        plot_data$IC50[plot_data$IC50 == Inf] <- max(plot_data$max_dose_uM)+1
                      }
                      
                      colors <- setNames(rep(colorRampPalette(c("blue", "red", "lightgray"))(3), 3),
                                         c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50",
                                           "selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50",
                                           "unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))

                      plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                      ylab <- "IC50 (microMolar)"
                      
                      if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                        plot_data$symbol <- ": not selected"
                        plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                        selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                        unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                        
                        if(nrow(selected_plot_data) > 0){
                          selected_plot_data$Group <- paste0("selected: ", selected_plot_data$Group)
                          selected_plot_data$Group <- factor(selected_plot_data$Group, levels = c("selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50"))
                          fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                         y = selected_plot_data$IC50,
                                         color = selected_plot_data$Group,
                                         type = "scatter",
                                         mode = "markers",
                                         colors = colors)
                          if(nrow(unselected_plot_data) > 0){
                            unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                            unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                            fig <- add_trace(fig,
                                             x = unselected_plot_data$Cell_Line,
                                             y = unselected_plot_data$IC50,
                                             color = unselected_plot_data$Group,
                                             opacity = 0.2)
                          }
                        } else {
                          unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                          unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                          fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                         y = unselected_plot_data$IC50,
                                         type = "scatter",
                                         mode = "markers",
                                         color = unselected_plot_data$Group,
                                         colors = colors,
                                         opacity = 0.2)
                        }
                        
                        fig <- layout(fig,
                                      title = "GDSC2 IC50s",
                                      xaxis = list(title = paste0("GDSC2 Cell Lines (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab, type = "log"))
                        
                      } else {
                        plot_data$Group <- factor(plot_data$Group, levels = c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50"))
                        fig <- plot_ly(x = plot_data$Cell_Line,
                                     y = plot_data$IC50,
                                     type = "scatter",
                                     mode = "markers",
                                     color = plot_data$Group,
                                     colors = colors)
                        
                        fig <- layout(fig,
                                      title = "GDSC2 IC50s",
                                      xaxis = list(title = paste0("GDSC2 Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                      yaxis = list(title = ylab, type = "log"))
                      }
                      
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Cancer Types & Genders"){
                    ccls <- unique(GDSC2_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      temp_dataset_ccl_data$Simple_Cancer_Type[temp_dataset_ccl_data$Simple_Cancer_Type == "unknown"] <- "unknown cancer type"
                      plot_data <- as.data.frame.table(table(temp_dataset_ccl_data$Simple_Cancer_Type))
                      plot_data <- plot_data[order(plot_data$Freq, decreasing = TRUE),]
                      plot_data <- rbind(plot_data[! plot_data$Var1 == "unknown cancer type",], plot_data[plot_data$Var1 == "unknown cancer type",])
                      
                      Gender_Unknown <- NA
                      Gender_Female <- NA
                      Gender_Male <- NA
                      for(j in 1:nrow(plot_data)){
                        Gender_Unknown[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Sex unspecified",])
                        Gender_Female[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Female",])
                        Gender_Male[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Male",])
                      }
                      
                      plot_data$Var1 <- factor(plot_data$Var1, levels = plot_data$Var1)
                      
                      fig <- plot_ly(x = plot_data$Var1, y = Gender_Unknown, type = "bar", name = "Unknown Gender", marker = list(color = "lightgray")) %>%
                              add_trace(y = Gender_Male, name = "Male", marker = list(color = rgb(65,105,225, maxColorValue = 255))) %>%
                              add_trace(y = Gender_Female, name = "Female", marker = list(color = rgb(186,85,211, maxColorValue = 255))) %>%
                              layout(title = 'GDSC2 Cell Line Cancer Types/Genders', yaxis = list(title = "# of Cell Lines"), xaxis = list(tickangle = 45), barmode = "stack", margin = list(b = 150, l = 50))
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Ancestries"){
                    ccls <- unique(GDSC2_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      completeness <- paste0("(data for ", nrow(temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$African_Ancestry),]), " of ", nrow(temp_dataset_ccl_data), " cell lines)")
                      temp_dataset_ccl_data <- temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$African_Ancestry),]
                      
                      plot_data <- temp_dataset_ccl_data[,grepl("_Ancestry", colnames(temp_dataset_ccl_data))]*100
                      rownames(plot_data) <- temp_dataset_ccl_data$Harmonized_Cell_Line_ID
                      
                      if(nrow(plot_data) > 2){
                        plot_data <- plot_data[hclust(dist(plot_data))$order,]
                      }
                      colnames(plot_data) <- gsub("_Ancestry", "", colnames(plot_data))
                      colnames(plot_data) <- gsub("_", " ", colnames(plot_data))
                      x <- factor(rownames(plot_data), levels = rownames(plot_data))
                      
                      if(nrow(plot_data) > 0){
                        plot_colors <- c("#6A3D9A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#A6CEE3", "#1F78B4")
                        fig <- plot_ly(x = x, y = plot_data[,1], name = colnames(plot_data)[1], type = "bar", marker = list(color = plot_colors[1])) %>%
                          add_trace(y = plot_data[,2], name = colnames(plot_data)[2], marker = list(color = plot_colors[2])) %>%
                          add_trace(y = plot_data[,3], name = colnames(plot_data)[3], marker = list(color = plot_colors[3])) %>%
                          add_trace(y = plot_data[,4], name = colnames(plot_data)[4], marker = list(color = plot_colors[4])) %>%
                          add_trace(y = plot_data[,5], name = colnames(plot_data)[5], marker = list(color = plot_colors[5])) %>%
                          add_trace(y = plot_data[,6], name = colnames(plot_data)[6], marker = list(color = plot_colors[6])) %>%
                          add_trace(y = plot_data[,7], name = colnames(plot_data)[7], marker = list(color = plot_colors[7])) %>%
                          layout(title = 'GDSC2 Ethnicities', barmode = "stack", xaxis = list(title = paste("Tested Cell Lines", completeness), showticklabels = FALSE), yaxis = list(title = "% Ancestry Makeup"), bargap = 0, legend = list(traceorder = "normal"))
                      } else {
                        fig <- plot_ly(x = 1, y = 100, name = "No Values", type = "bar", marker = list(color = "white")) %>%
                        layout(title = 'GDSC2 Cell Line Ethnicities', barmode = "stack", xaxis = list(title = paste("Tested Cell Lines", completeness), showticklabels = FALSE), yaxis = list(title = "% Ancestry Makeup"), bargap = 0, legend = list(traceorder = "normal"))
                      }
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Ages"){
                    ccls <- unique(GDSC2_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      completeness <- paste0("(data for ", nrow(temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$Numeric_Age_in_Years),]), " of ", nrow(temp_dataset_ccl_data), " cell lines)")
                      temp_dataset_ccl_data <- temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$Numeric_Age_in_Years),]
                      
                      if(nrow(temp_dataset_ccl_data) > 1){
                        p <- ggplot(temp_dataset_ccl_data, aes(x = Numeric_Age_in_Years, y = after_stat(scaled))) +
                                    geom_density(color = "darkblue", fill = "lightblue") +
                                    theme_light() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "Scaled Density") +
                                    ggtitle("GDSC2 Ages")
                      } else if(nrow(temp_dataset_ccl_data) == 1){
                        p <- ggplot(temp_dataset_ccl_data, aes(x = Numeric_Age_in_Years, y = Harmonized_Cell_Line_ID)) +
                                    geom_bar(stat="identity") +
                                    theme_light() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "") +
                                    ggtitle("GDSC2 Ages")
                      } else {
                        p <- ggplot(data.frame(x = c(0,100), y = c(0,1)), aes(x = x, y = y)) +
                                    geom_blank() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "Scaled Density") +
                                    ggtitle("GDSC2 Cell Line Patient Age Distribution")
                      }
                  
                      fig <- ggplotly(p)
                      fig
                    }
                  }
                }
            })
  
            output$Compound_Explorer_PRISM_Repurposing_Plot <- renderPlotly({
              #Loading raw data for this compound and any datasets with data for this compound
                if("PRISM_Repurposing" %in% Compound_Explorer_Datasets_with_Compound_Data()){
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
                    
                    colors <- setNames(rep(colorRampPalette(c("blue", "red"))(2), 3),
                                       c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range",
                                         "selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range",
                                         "unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                    
                    if(all(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration %in% "Max Tested Concentration Within AUC Range")){
                      colors <- colorRampPalette(c("blue"))(1)
                    } else if(all(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration %in% "Max Tested Concentration < AUC Range")){
                      colors <- colorRampPalette(c("red"))(1)
                    } else {
                      colors <- colorRampPalette(c("blue", "red"))(2)
                    }

                    if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                      plot_data$symbol <- ": not selected"
                      plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                      selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                      unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                      
                      if(nrow(selected_plot_data) > 0){
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "selected: Max Tested Concentration Within AUC Range"
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[selected_plot_data$max_dose_uM < selected_plot_data$max_mode_ccl_PRISM_Repurposing_conc] <- "selected: Max Tested Concentration < AUC Range"
                        selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range"))
                        fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                   y = selected_plot_data$AUC_mode_ccl_PRISM_Repurposing_conc,
                                   color = selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   type = "scatter",
                                   mode = "markers")
                        if(nrow(unselected_plot_data) > 0){
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "unselected: Max Tested Concentration Within AUC Range"
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[unselected_plot_data$max_dose_uM < unselected_plot_data$max_mode_ccl_PRISM_Repurposing_conc] <- "unselected: Max Tested Concentration < AUC Range"
                          unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                          fig <- add_trace(fig, 
                                    x = unselected_plot_data$Cell_Line,
                                    y = unselected_plot_data$AUC_mode_ccl_PRISM_Repurposing_conc,
                                    color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                    opacity = 0.2)
                        }
                      } else if(nrow(unselected_plot_data) > 0){
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "unselected: Max Tested Concentration Within AUC Range"
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[unselected_plot_data$max_dose_uM < unselected_plot_data$max_mode_ccl_PRISM_Repurposing_conc] <- "unselected: Max Tested Concentration < AUC Range"
                        unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                        fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                   y = unselected_plot_data$AUC_mode_ccl_PRISM_Repurposing_conc,
                                   color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   opacity = 0.2,
                                   type = "scatter",
                                   mode = "markers")
                      }
                        
                      fig <- layout(fig,
                                    title = "PRISM_Repurposing AUCs",
                                    xaxis = list(title = paste0("PRISM_Repurposing Cell Lines (n = ", nrow(plot_data), "; ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    } else {

                      fig <- plot_ly(x = plot_data$Cell_Line,
                                   y = plot_data$AUC_mode_ccl_PRISM_Repurposing_conc,
                                   color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                   colors = colors,
                                   type = "scatter",
                                   mode = "markers")
                      fig <- layout(fig,
                                    title = "PRISM_Repurposing AUCs",
                                    xaxis = list(title = paste0("PRISM_Repurposing Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    }

                    fig
                    
                  } else if(input$Compound_Explorer_to_Plot == "AUC values for concentration range available for all tested cell lines"){
                    plot_data <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$AUC_all_ccl_PRISM_Repurposing_conc),]
                    plot_data <- plot_data[order(plot_data$AUC_all_ccl_PRISM_Repurposing_conc, decreasing = FALSE),]
                    plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                    ylab <- paste0("AUC (", signif(as.numeric(unique(plot_data$min_all_ccl_PRISM_Repurposing_conc)), 3), "-", signif(as.numeric(unique(plot_data$max_all_ccl_PRISM_Repurposing_conc)), 3), " microMolar)")
                    
                    if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                      plot_data$symbol <- ": not selected"
                      plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                      selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                      unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                      
                      if(nrow(selected_plot_data) > 0){
                        fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                     y = selected_plot_data$AUC_all_ccl_PRISM_Repurposing_conc,
                                     type = "scatter",
                                     color = factor(rep("A", nrow(selected_plot_data))),
                                     colors = colorRampPalette(c("blue"))(1),
                                     mode = "markers",
                                     name = "selected")
                        if(nrow(unselected_plot_data) > 0){
                          fig <- add_trace(fig, x = unselected_plot_data$Cell_Line,
                                       y = unselected_plot_data$AUC_all_ccl_PRISM_Repurposing_conc,
                                       color = factor(rep("A", nrow(unselected_plot_data))),
                                       name = "unselected",
                                       opacity = 0.2)
                        }
                      } else {
                        fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                     y = unselected_plot_data$AUC_all_ccl_PRISM_Repurposing_conc,
                                     type = "scatter",
                                     color = factor(rep("A", nrow(unselected_plot_data))),
                                     colors = colorRampPalette(c("blue"))(1),
                                     opacity = 0.2,
                                     mode = "markers",
                                     name = "selected")
                      }
                      
                      fig <- layout(fig,
                                    title = "PRISM_Repurposing AUCs",
                                    xaxis = list(title = paste0("PRISM_Repurposing Cell Lines (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                      
                    } else {
                      fig <- plot_ly(x = plot_data$Cell_Line,
                                   y = plot_data$AUC_all_ccl_PRISM_Repurposing_conc,
                                   color = factor(rep("A", nrow(plot_data))),
                                   colors = colorRampPalette(c("blue"))(1),
                                   type = "scatter",
                                   mode = "markers")
                      
                      fig <- layout(fig,
                                    title = "PRISM_Repurposing AUCs",
                                    xaxis = list(title = paste0("PRISM_Repurposing Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                    yaxis = list(title = ylab))
                    }
                    
                    
                    fig
                    
                  } else if(input$Compound_Explorer_to_Plot == "IC50 values"){
                    plot_data <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$IC50),]
                    if(nrow(plot_data) > 0){
                      plot_data <- plot_data[order(plot_data$IC50, decreasing = FALSE),]
                      plot_data$Group <- "IC50 <= max tested concentration"
                      plot_data$Group[plot_data$IC50 > plot_data$max_dose_uM] <- "IC50 > max tested concentration"
                      plot_data$Group[plot_data$IC50 == Inf] <- "Infinite IC50"
                      if(any(plot_data$IC50 != Inf)){
                        plot_data$IC50[plot_data$IC50 == Inf] <- max(plot_data$IC50[! plot_data$IC50 == Inf])
                      } else {
                        plot_data$IC50[plot_data$IC50 == Inf] <- max(plot_data$max_dose_uM)+1
                      }
                      
                      colors <- setNames(rep(colorRampPalette(c("blue", "red", "lightgray"))(3), 3),
                                         c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50",
                                           "selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50",
                                           "unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))

                      plot_data$Cell_Line <- factor(plot_data$Cell_Line, levels = plot_data$Cell_Line)
                      ylab <- "IC50 (microMolar)"
                      
                      if(input$Compound_Explorer_Highlight_Cell_Lines == TRUE){
                        plot_data$symbol <- ": not selected"
                        plot_data$symbol[plot_data$Cell_Line %in% input$Compound_Explorer_Cell_Lines] <- ": selected"
                        selected_plot_data <- plot_data[plot_data$symbol == ": selected",]
                        unselected_plot_data <- plot_data[plot_data$symbol == ": not selected",]
                        
                        if(nrow(selected_plot_data) > 0){
                          selected_plot_data$Group <- paste0("selected: ", selected_plot_data$Group)
                          selected_plot_data$Group <- factor(selected_plot_data$Group, levels = c("selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50"))
                          fig <- plot_ly(x = selected_plot_data$Cell_Line,
                                         y = selected_plot_data$IC50,
                                         color = selected_plot_data$Group,
                                         type = "scatter",
                                         mode = "markers",
                                         colors = colors)
                          if(nrow(unselected_plot_data) > 0){
                            unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                            unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                            fig <- add_trace(fig,
                                             x = unselected_plot_data$Cell_Line,
                                             y = unselected_plot_data$IC50,
                                             color = unselected_plot_data$Group,
                                             opacity = 0.2)
                          }
                        } else {
                          unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                          unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                          fig <- plot_ly(x = unselected_plot_data$Cell_Line,
                                         y = unselected_plot_data$IC50,
                                         type = "scatter",
                                         mode = "markers",
                                         color = unselected_plot_data$Group,
                                         colors = colors,
                                         opacity = 0.2)
                        }
                        
                        fig <- layout(fig,
                                      title = "PRISM_Repurposing IC50s",
                                      xaxis = list(title = paste0("PRISM_Repurposing Cell Lines (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab, type = "log"))
                        
                      } else {
                        plot_data$Group <- factor(plot_data$Group, levels = c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50"))
                        fig <- plot_ly(x = plot_data$Cell_Line,
                                     y = plot_data$IC50,
                                     type = "scatter",
                                     mode = "markers",
                                     color = plot_data$Group,
                                     colors = colors)
                        
                        fig <- layout(fig,
                                      title = "PRISM_Repurposing IC50s",
                                      xaxis = list(title = paste0("PRISM_Repurposing Cell Lines (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                      yaxis = list(title = ylab, type = "log"))
                      }
                      
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Cancer Types & Genders"){
                    ccls <- unique(PRISM_Repurposing_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      temp_dataset_ccl_data$Simple_Cancer_Type[temp_dataset_ccl_data$Simple_Cancer_Type == "unknown"] <- "unknown cancer type"
                      plot_data <- as.data.frame.table(table(temp_dataset_ccl_data$Simple_Cancer_Type))
                      plot_data <- plot_data[order(plot_data$Freq, decreasing = TRUE),]
                      plot_data <- rbind(plot_data[! plot_data$Var1 == "unknown cancer type",], plot_data[plot_data$Var1 == "unknown cancer type",])
                      
                      Gender_Unknown <- NA
                      Gender_Female <- NA
                      Gender_Male <- NA
                      for(j in 1:nrow(plot_data)){
                        Gender_Unknown[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Sex unspecified",])
                        Gender_Female[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Female",])
                        Gender_Male[j] <- nrow(temp_dataset_ccl_data[temp_dataset_ccl_data$Simple_Cancer_Type %in% plot_data$Var1[j] & temp_dataset_ccl_data$Gender == "Male",])
                      }
                      
                      plot_data$Var1 <- factor(plot_data$Var1, levels = plot_data$Var1)
                      
                      fig <- plot_ly(x = plot_data$Var1, y = Gender_Unknown, type = "bar", name = "Unknown Gender", marker = list(color = "lightgray")) %>%
                              add_trace(y = Gender_Male, name = "Male", marker = list(color = rgb(65,105,225, maxColorValue = 255))) %>%
                              add_trace(y = Gender_Female, name = "Female", marker = list(color = rgb(186,85,211, maxColorValue = 255))) %>%
                              layout(title = 'PRISM_Repurposing Cell Line Cancer Types/Genders', yaxis = list(title = "# of Cell Lines"), xaxis = list(tickangle = 45), barmode = "stack", margin = list(b = 150, l = 50))
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Ancestries"){
                    ccls <- unique(PRISM_Repurposing_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      completeness <- paste0("(data for ", nrow(temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$African_Ancestry),]), " of ", nrow(temp_dataset_ccl_data), " cell lines)")
                      temp_dataset_ccl_data <- temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$African_Ancestry),]
                      
                      plot_data <- temp_dataset_ccl_data[,grepl("_Ancestry", colnames(temp_dataset_ccl_data))]*100
                      rownames(plot_data) <- temp_dataset_ccl_data$Harmonized_Cell_Line_ID
                      
                      if(nrow(plot_data) > 2){
                        plot_data <- plot_data[hclust(dist(plot_data))$order,]
                      }
                      colnames(plot_data) <- gsub("_Ancestry", "", colnames(plot_data))
                      colnames(plot_data) <- gsub("_", " ", colnames(plot_data))
                      x <- factor(rownames(plot_data), levels = rownames(plot_data))
                      
                      if(nrow(plot_data) > 0){
                        plot_colors <- c("#6A3D9A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#A6CEE3", "#1F78B4")
                        fig <- plot_ly(x = x, y = plot_data[,1], name = colnames(plot_data)[1], type = "bar", marker = list(color = plot_colors[1])) %>%
                          add_trace(y = plot_data[,2], name = colnames(plot_data)[2], marker = list(color = plot_colors[2])) %>%
                          add_trace(y = plot_data[,3], name = colnames(plot_data)[3], marker = list(color = plot_colors[3])) %>%
                          add_trace(y = plot_data[,4], name = colnames(plot_data)[4], marker = list(color = plot_colors[4])) %>%
                          add_trace(y = plot_data[,5], name = colnames(plot_data)[5], marker = list(color = plot_colors[5])) %>%
                          add_trace(y = plot_data[,6], name = colnames(plot_data)[6], marker = list(color = plot_colors[6])) %>%
                          add_trace(y = plot_data[,7], name = colnames(plot_data)[7], marker = list(color = plot_colors[7])) %>%
                          layout(title = 'PRISM_Repurposing Ethnicities', barmode = "stack", xaxis = list(title = paste("Tested Cell Lines", completeness), showticklabels = FALSE), yaxis = list(title = "% Ancestry Makeup"), bargap = 0, legend = list(traceorder = "normal"))
                      } else {
                        fig <- plot_ly(x = 1, y = 100, name = "No Values", type = "bar", marker = list(color = "white")) %>%
                        layout(title = 'PRISM_Repurposing Cell Line Ethnicities', barmode = "stack", xaxis = list(title = paste("Tested Cell Lines", completeness), showticklabels = FALSE), yaxis = list(title = "% Ancestry Makeup"), bargap = 0, legend = list(traceorder = "normal"))
                      }
                      fig
                    }
                  } else if(input$Compound_Explorer_to_Plot == "Tested Cell Line Ages"){
                    ccls <- unique(PRISM_Repurposing_Results$Cell_Line)
                    if(length(ccls) > 0){
                      temp_dataset_ccl_data <- Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% ccls,]
                      completeness <- paste0("(data for ", nrow(temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$Numeric_Age_in_Years),]), " of ", nrow(temp_dataset_ccl_data), " cell lines)")
                      temp_dataset_ccl_data <- temp_dataset_ccl_data[! is.na(temp_dataset_ccl_data$Numeric_Age_in_Years),]
                      
                      if(nrow(temp_dataset_ccl_data) > 1){
                        p <- ggplot(temp_dataset_ccl_data, aes(x = Numeric_Age_in_Years, y = after_stat(scaled))) +
                                    geom_density(color = "darkblue", fill = "lightblue") +
                                    theme_light() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "Scaled Density") +
                                    ggtitle("PRISM_Repurposing Ages")
                      } else if(nrow(temp_dataset_ccl_data) == 1){
                        p <- ggplot(temp_dataset_ccl_data, aes(x = Numeric_Age_in_Years, y = Harmonized_Cell_Line_ID)) +
                                    geom_bar(stat="identity") +
                                    theme_light() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "") +
                                    ggtitle("PRISM_Repurposing Ages")
                      } else {
                        p <- ggplot(data.frame(x = c(0,100), y = c(0,1)), aes(x = x, y = y)) +
                                    geom_blank() +
                                    labs(x = paste("Patient Age (in Years) when Cell Line was Derived ", completeness), y = "Scaled Density") +
                                    ggtitle("PRISM_Repurposing Cell Line Patient Age Distribution")
                      }
                  
                      fig <- ggplotly(p)
                      fig
                    }
                  }
                }
            })
            
          }) #END: observe({
              
                  
                
#############################################################          
#################################################### server #          
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
                      content = c("Sets minimum and maximum age allowed for displayed cell line options. Note that setting the slider to anything other than its maximum range will exclude cell lines for which a numeric age could not be determined for the patient who the cell line was derived from at the time of sample collection. This both includes cases where the age is unspecified and cases where the specified age is ambiguous (i.e. such as \"Adult\"). You may download the cell line harmonization file in the \"Download Bulk Data\" tab for free-text descriptions of each cell line's patient age."),
                      size = "m",
                      buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                    ),

                    h4("Ancestry") %>%
                    helper(type = "inline",
                      title = "Patient age filtering",
                      icon = "question-circle", colour = NULL,
                      content = c("Each slider sets the minimum and maximum % ancestry for each ancestry group. Once any of the ancestry sliders has been changed from its maximum range, cell lines will be filtered to only include lines which meet the limits set on all of the ancestry sliders. Note that % ancestry adds to 100% across all ancestry groups for each individual cell line (i.e. a cell line cannot have 60% African ancestry and 60% Native American ancestry, because that would add to >100%). Ancestry information was obtained from the cellosaurus resource at <a href=\"https://www.expasy.org/\">https://www.expasy.org/</a>."),
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
            }) #END: Cell_Line_Explorer_Currently_Available_Cell_Lines <- reactive({

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
            Cell_Line_Explorer_Selected_Cell_Line_Option_df <- eventReactive(Cell_Line_Explorer_Currently_Available_Cell_Lines(), {
              filtered_Cell_Line_Option_df <- Cell_Line_Option_df[Cell_Line_Option_df$Harmonized_Cell_Line_ID %in% Cell_Line_Explorer_Currently_Available_Cell_Lines(),]
              return(cbind(filtered_Cell_Line_Option_df, value = filtered_Cell_Line_Option_df$Harmonized_Cell_Line_ID))
            })

            observeEvent(Cell_Line_Explorer_Selected_Cell_Line_Option_df(),{
              prev_selection <- input$cell_line_explorer
              if(! length(prev_selection) == 0){
                if(! prev_selection %in% Cell_Line_Explorer_Selected_Cell_Line_Option_df()$Harmonized_Cell_Line_ID){
                  prev_selection <- character(0)
                }
              }
              updateSelectizeInput(session, "cell_line_explorer", label = paste0("Select a cell line (n = ", nrow(Cell_Line_Explorer_Selected_Cell_Line_Option_df()), ")"), choices = Cell_Line_Explorer_Selected_Cell_Line_Option_df(), server = TRUE, selected = prev_selection)
            })

          #Loading screening data for selected cell line
            Cell_Line_Explorer_Data <- eventReactive(input$cell_line_explorer, {
              isolate({
                req(input$cell_line_explorer)

                filename <- Cell_Line_Filenames$cell_line_file_names[Cell_Line_Filenames$cell_lines == input$cell_line_explorer]
                data <- readRDS(paste0("./www/Results/", filename, ".rds"))
                for(i in 1:length(data)){
                  data[[i]] <- data[[i]][! is.na(data[[i]]$b_c_d_e),]
                }
                return(data)
              })
            })

          #Defining compounds available for selected cell line
            Cell_Line_Explorer_Available_Compounds_for_Cell_Line <- eventReactive(Cell_Line_Explorer_Data(), {
              isolate({
                req(Cell_Line_Explorer_Data())
                compounds <- character(0)
                for(i in 1:length(Cell_Line_Explorer_Data())){
                  compounds <- c(compounds, Cell_Line_Explorer_Data()[[i]]$Compound)
                }
                return(sort(unique(compounds)))
              })
            })

          #Subsetting compound info to compounds available for selected cell line
            Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line <- eventReactive(Cell_Line_Explorer_Available_Compounds_for_Cell_Line(), {
              isolate({
                req(Cell_Line_Explorer_Available_Compounds_for_Cell_Line())
                return(Simple_Compound_Harm[Simple_Compound_Harm$Harmonized_Compound_Name %in% Cell_Line_Explorer_Available_Compounds_for_Cell_Line(),])
              })
            })

          #Getting molecular targets available for compounds available for selected cell line
            Cell_Line_Explorer_Available_Molecular_Targets <- eventReactive(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line(), {
              isolate({
                req(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line())
                temp_Compound_Molecular_Targets <- strsplit(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line()$Compound_Molecular_Targets, ":\\|:")
                return(sort(unique(unlist(temp_Compound_Molecular_Targets))))
              })
            })

          #Getting mechanisms of action available for compounds available for selected cell line
            Cell_Line_Explorer_Available_MOAs <- eventReactive(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line(), {
              isolate({
                req(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line())
                temp_Compound_MOAs <- strsplit(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line()$Compound_MOA, ":\\|:")
                return(sort(unique(unlist(temp_Compound_MOAs))))
              })
            })

          #Getting annotation information for the selected cell line
            Cell_Line_Explorer_Annotation_Info <- reactive({
              #isolate({
                req(input$cell_line_explorer)
                return(Cell_Line_Harm[Cell_Line_Harm$Harmonized_Cell_Line_ID %in% input$cell_line_explorer,])
              #})
            })


          #Defining UI output for cell line summary
            observeEvent(Cell_Line_Explorer_Annotation_Info(), {
              isolate({
                req(Cell_Line_Explorer_Annotation_Info())
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

              }) #END: Isolate
            }) #END: observeEvent(Cell_Line_Explorer_Annotation_Info(), {


            #Defining compound selection menu
              observeEvent(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line(), {
                isolate({
                  req(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line())
                  #Defining compound selection interface
                    output$Cell_Line_Explorer_Compound_Menu <- renderUI({
                      Temp_Compound_Harm_Data <- Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line()

                      return_list <- list(
                        pickerInput("Cell_Line_Explorer_Compounds", label = "Select compounds to plot data for", choices = NULL, selected = NULL, multiple = TRUE, options = list(
                          `actions-Box` = TRUE,
                          `live-Search-Style` = "contains" ,
                          `live-Search` = TRUE,
                          `live-Search-Normalize` = TRUE,
                          `selected-Text-Format` = "count"
                        )),
                        checkboxInput("Cell_Line_Explorer_Highlight_Compounds", label = "Highlight compounds instead of filtering compounds?", value = FALSE) %>%
                          helper(type = "inline",
                            title = "Highlight or Filter Compounds?",
                            icon = "question-circle", colour = NULL,
                            content = HTML("If this box is left unchecked, only selected compounds will be shown in the plots. If this box is checked, all available compounds will be shown in the plots, but selected compounds will be highlighted."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                        checkboxInput("Cell_Line_Explorer_Show_Compound_Filters", label = "Show compound filters?", value = FALSE)
                      )

                      return(return_list)

                    })
                })
              })

            #Defining compound filter menu
              observeEvent(input$Cell_Line_Explorer_Show_Compound_Filters, {
                isolate({
                  #Defining compound filter interface
                  output$Cell_Line_Explorer_Compound_Filters <- renderUI({
                    req(! length(input$Cell_Line_Explorer_Show_Compound_Filters) == 0)
                    if(input$Cell_Line_Explorer_Show_Compound_Filters == TRUE){
                      return(list(
                        h4("Filter compounds by:") %>%
                            helper(type = "inline",
                              title = "Compound filtering",
                              icon = "question-circle", colour = NULL,
                              content = c("These options can be used to filter the compound options displayed in the \"Select compounds to plot data for\" menu. Note that selecting any options from a filter menu will omit all compounds which lack annotated information for that menu's filtering criteria."),
                              size = "m",
                              buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                            ),
                        wellPanel(list(
                          pickerInput(inputId = "Cell_Line_Explorer_Molecular_Target", label = "Filter compounds by molecular target", choices = Cell_Line_Explorer_Available_Molecular_Targets(), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                          helper(type = "inline",
                            title = "Molecular Target Filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Limits compounds displayed in the \"Select compounds to plot data for\" menu to compounds which target at least one of the selected molecular targets. Note that, if any selections have been made in the \"Filter compounds by free-text MOA\" menu, displayed compounds will also include compounds which have at least one of the selected MOAs."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                          pickerInput(inputId = "Cell_Line_Explorer_MOA", label = "Filter compounds by free-text MOA", choices = Cell_Line_Explorer_Available_MOAs(), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                          helper(type = "inline",
                            title = "Mechanism of Action (MOA) Filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Limits compounds displayed in the \"Select compounds to plot data for\" menu to compounds with at least one of the selected MOAs. Note that, if any selections have been made in the \"Filter compounds by molecular target\" menu, displayed compounds will also include compounds which target at least one of the selected molecular targets."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                          pickerInput(inputId = "Cell_Line_Explorer_Clinical_Phase", label = "Filter compounds by clinical phase", choices = sort(unique(Cell_Line_Explorer_Simple_Compound_Harm_for_Cell_Line()$Compound_Clinical_Phase)), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                          helper(type = "inline",
                            title = "Clinical Phase Filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Limits compounds displayed in the \"Select compounds to plot data for\" menu to compounds whose annotated highest reached clinical phase is one of the selected phases."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          )
                        ))
                      ))
                    } else {
                      return(list(p(" ")))
                    }
                  })
                }) #END: Isolate
              }) #END: observeEvent(input$Cell_Line_Explorer_Show_Compound_Filters, {


            #Defining currently available compounds based on compound filters (meets any filter)
              Cell_Line_Explorer_Currently_Available_Compounds <- reactive({
                req(Cell_Line_Explorer_Available_Compounds_for_Cell_Line())

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
              observeEvent(list(Cell_Line_Explorer_Currently_Available_Compounds(), input$Cell_Line_Explorer_Highlight_Compounds), {
                req(! length(input$Cell_Line_Explorer_Highlight_Compounds) == 0)
                isolate({
                  if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                    updatePickerInput(session, "Cell_Line_Explorer_Compounds", label = paste0("Select compounds to highlight (n = ", length(Cell_Line_Explorer_Currently_Available_Compounds()), ")"), choices = Cell_Line_Explorer_Currently_Available_Compounds(), selected = Cell_Line_Explorer_Currently_Available_Compounds())
                  } else {
                    updatePickerInput(session, "Cell_Line_Explorer_Compounds", label = paste0("Select compounds to plot data for (n = ", length(Cell_Line_Explorer_Currently_Available_Compounds()), ")"), choices = Cell_Line_Explorer_Currently_Available_Compounds(), selected = Cell_Line_Explorer_Currently_Available_Compounds())
                  }
                })
              })

            #Creating filtered version of Cell_Line_Explorer_Data
              Filtered_Cell_Line_Explorer_Data <- eventReactive(list(input$Cell_Line_Explorer_Compounds, input$Cell_Line_Explorer_Highlight_Compounds), {
                isolate({
                  req(Cell_Line_Explorer_Data())
                  req(! length(input$Cell_Line_Explorer_Highlight_Compounds) == 0)
                  data <- Cell_Line_Explorer_Data()
                  if(input$Cell_Line_Explorer_Highlight_Compounds == FALSE){
                    for(i in 1:length(data)){
                      data[[i]] <- data[[i]][data[[i]]$Compound %in% input$Cell_Line_Explorer_Compounds,]
                    }
                  }
                  return(data)
                })
              })

            #Defining datasets with cell line data
              Cell_Line_Explorer_datasets_with_cell_line_data <- eventReactive(Filtered_Cell_Line_Explorer_Data(), {
                names(Filtered_Cell_Line_Explorer_Data())[sapply(Filtered_Cell_Line_Explorer_Data(), nrow) > 0]
              })

          #Rendering plot UI for datasets with data for this compound
            observeEvent(Cell_Line_Explorer_datasets_with_cell_line_data(), {
              isolate({
                req(Cell_Line_Explorer_datasets_with_cell_line_data())
                req(Filtered_Cell_Line_Explorer_Data())

              #Defining Plot UI
                output$Cell_Line_Explorer_Plots <- renderUI({
                      Cell_Line_Explorer_Plot_UI <- vector(mode = "list")
                      if("CTRPv2" %in% Cell_Line_Explorer_datasets_with_cell_line_data()){
                        Cell_Line_Explorer_Plot_UI <- c(Cell_Line_Explorer_Plot_UI, plotlyOutput(outputId = "Cell_Line_Explorer_CTRPv2_Plot"), HTML("---"))
                      }
                      if("GDSC1" %in% Cell_Line_Explorer_datasets_with_cell_line_data()){
                        Cell_Line_Explorer_Plot_UI <- c(Cell_Line_Explorer_Plot_UI, plotlyOutput(outputId = "Cell_Line_Explorer_GDSC1_Plot"), HTML("---"))
                      }
                      if("GDSC2" %in% Cell_Line_Explorer_datasets_with_cell_line_data()){
                        Cell_Line_Explorer_Plot_UI <- c(Cell_Line_Explorer_Plot_UI, plotlyOutput(outputId = "Cell_Line_Explorer_GDSC2_Plot"), HTML("---"))
                      }
                      if("PRISM_Repurposing" %in% Cell_Line_Explorer_datasets_with_cell_line_data()){
                        Cell_Line_Explorer_Plot_UI <- c(Cell_Line_Explorer_Plot_UI, plotlyOutput(outputId = "Cell_Line_Explorer_PRISM_Repurposing_Plot"))
                      }
                    Cell_Line_Explorer_Plot_UI
                })

              #Generating plots
                output$Cell_Line_Explorer_CTRPv2_Plot <- renderPlotly({
                  req(! length(input$Cell_Line_Explorer_Highlight_Compounds) == 0)
                    #Loading raw data for this cell_line and any datasets with data for this cell_line
                      if("CTRPv2" %in% Cell_Line_Explorer_datasets_with_cell_line_data()){
                        CTRPv2_Results <- Filtered_Cell_Line_Explorer_Data()$CTRPv2
                        CTRPv2_Results <- CTRPv2_Results[! is.na(CTRPv2_Results$b_c_d_e),]
                      } else {
                        CTRPv2_Results <- data.frame(NULL)
                      }
                  #Making cell_line explorer CTRPv2 plot
                    if(nrow(CTRPv2_Results) > 0){
                      if(input$Cell_Line_Explorer_to_Plot == "AUC percentiles for most commonly used concentration range"){
                        plot_data <- CTRPv2_Results[! is.na(CTRPv2_Results$Percentile_AUC_Mode_CCL),]
                        plot_data <- plot_data[order(plot_data$Percentile_AUC_Mode_CCL, decreasing = FALSE),]
                        plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_CTRPv2_conc] <- "Max Tested Concentration < AUC Range"
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                        plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_AUC_Mode_CCL,3)*100, "%, AUC=", signif(plot_data$AUC_mode_ccl_CTRPv2_conc, 2), ")")
                        ylab <- "Percentile for AUC (most common range)"
                        
                        input$Cell_Line_Explorer_Compounds
                        colors <- setNames(rep(colorRampPalette(c("blue", "red"))(2), 3),
                                           c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range",
                                             "selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range",
                                             "unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range")
                                           )
                        if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                          plot_data$Group <- ": unselected"
                          plot_data$Group[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                          selected_plot_data <- plot_data[plot_data$Group == ": selected",]
                          unselected_plot_data <- plot_data[plot_data$Group == ": unselected",]
                          
                          if(nrow(selected_plot_data) > 0){
                            selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("selected: ", selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                            selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range"))
                            fig <- plot_ly(x = selected_plot_data$Compound,
                                       y = selected_plot_data$Percentile_AUC_Mode_CCL*100,
                                       color = selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                       colors = colors,
                                       type = "scatter",
                                       mode = "markers",
                                       text = selected_plot_data$hovertext,
                                       hoverinfo = "text")
                            if(nrow(unselected_plot_data) > 0){
                              unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("unselected: ", unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                              unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                              fig <- add_trace(fig,
                                               x = unselected_plot_data$Compound,
                                               y = unselected_plot_data$Percentile_AUC_Mode_CCL*100,
                                               color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                               text = unselected_plot_data$hovertext,
                                               hoverinfo = "text",
                                               opacity = 0.2)
                            }
                            
                            fig <- layout(fig,
                                        title = "CTRPv2 AUC Percentiles",
                                        xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                            
                          } else if(nrow(unselected_plot_data) > 0){
                            unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("unselected: ", unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                            unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                            fig <- plot_ly(x = unselected_plot_data$Compound,
                                       y = unselected_plot_data$Percentile_AUC_Mode_CCL*100,
                                       color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                       colors = colors,
                                       type = "scatter",
                                       mode = "markers",
                                       text = unselected_plot_data$hovertext,
                                       hoverinfo = "text",
                                       opacity = 0.2)
                            
                            fig <- layout(fig,
                                        title = "CTRPv2 AUC Percentiles",
                                        xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          }
                        } else {
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_Mode_CCL*100,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colors,
                                         type = "scatter",
                                         mode = "markers",
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                          
                          fig <- layout(fig,
                                        title = "CTRPv2 AUC Percentiles",
                                        xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                        }
                        
                        fig
                        
                      } else if(input$Cell_Line_Explorer_to_Plot == "AUC percentiles for concentration range available for all tested cell lines"){
                        plot_data <- CTRPv2_Results[! is.na(CTRPv2_Results$Percentile_AUC_All_CCL),]
                        plot_data <- plot_data[order(plot_data$Percentile_AUC_All_CCL, decreasing = FALSE),]
                        plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                        plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_AUC_All_CCL,3)*100, "%, AUC=", signif(plot_data$AUC_all_ccl_CTRPv2_conc, 2), ")")
                        ylab <- "Percentile for AUC (all cell line range)"
                        
                        if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                          plot_data$Group <- ": unselected"
                          plot_data$Group[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                          selected_plot_data <- plot_data[plot_data$Group == ": selected",]
                          unselected_plot_data <- plot_data[plot_data$Group == ": unselected",]
                          
                          if(nrow(selected_plot_data) > 0){
                            fig <- plot_ly(x = selected_plot_data$Compound,
                                           y = selected_plot_data$Percentile_AUC_All_CCL*100,
                                           type = "scatter",
                                           mode = "markers",
                                           color = factor(rep("A", nrow(selected_plot_data))),
                                           colors = colorRampPalette(c("blue"))(1),
                                           text = selected_plot_data$hovertext,
                                           hoverinfo = "text",
                                           name = "selected")
                            if(nrow(unselected_plot_data) > 0){
                              fig <- add_trace(fig,
                                               x = unselected_plot_data$Compound,
                                               y = unselected_plot_data$Percentile_AUC_All_CCL*100,
                                               text = unselected_plot_data$hovertext,
                                               color = factor(rep("A", nrow(unselected_plot_data))),
                                               hoverinfo = "text",
                                               name = "unselected",
                                               opacity = 0.2)
                            }
                            
                            fig <- layout(fig,
                                      title = "CTRPv2 AUC Percentiles",
                                      xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab))
                          } else {
                            fig <- plot_ly(x = unselected_plot_data$Compound,
                                           y = unselected_plot_data$Percentile_AUC_All_CCL*100,
                                           type = "scatter",
                                           mode = "markers",
                                           color = factor(rep("A", nrow(unselected_plot_data))),
                                           colors = colorRampPalette(c("blue"))(1),
                                           text = unselected_plot_data$hovertext,
                                           hoverinfo = "text",
                                           name = "unselected",
                                           opacity = 0.2)
                            fig <- layout(fig,
                                      title = "CTRPv2 AUC Percentiles",
                                      xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab))
                          }
                        } else {
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_All_CCL*100,
                                         type = "scatter",
                                         mode = "markers",
                                         color = factor(rep("A", nrow(plot_data))),
                                         colors = colorRampPalette(c("blue"))(1),
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                          fig <- layout(fig,
                                        title = "CTRPv2 AUC Percentiles",
                                        xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                        }
                        
                        fig
                        
                      } else if(input$Cell_Line_Explorer_to_Plot == "IC50 percentiles"){
                        plot_data <- CTRPv2_Results[! is.na(CTRPv2_Results$Percentile_IC50),]
                        if(nrow(plot_data) > 0){
                          plot_data <- plot_data[order(plot_data$Percentile_IC50, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_IC50,3)*100, "%, IC50=", signif(plot_data$IC50, 3), "uM)")
                          plot_data$Group <- "IC50 <= max tested concentration"
                          plot_data$Group[plot_data$IC50 > plot_data$max_dose_uM] <- "IC50 > max tested concentration"
                          plot_data$Group[plot_data$IC50 == Inf] <- "Infinite IC50"
                          ylab <- "Percentile for IC50"

                          colors <- setNames(rep(colorRampPalette(c("blue", "red", "lightgray"))(3), 3),
                                           c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50",
                                             "selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50",
                                             "unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50")
                                           )
                          
                          if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                            plot_data$Group_s <- ": unselected"
                            plot_data$Group_s[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                            selected_plot_data <- plot_data[plot_data$Group_s == ": selected",]
                            unselected_plot_data <- plot_data[plot_data$Group_s == ": unselected",]
                            
                            if(nrow(selected_plot_data) > 0){
                              selected_plot_data$Group <- paste0("selected: ", selected_plot_data$Group)
                              selected_plot_data$Group <- factor(selected_plot_data$Group, levels = c("selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50"))
                              fig <- plot_ly(x = selected_plot_data$Compound,
                                             y = selected_plot_data$Percentile_IC50*100,
                                             type = "scatter",
                                             mode = "markers",
                                             color = selected_plot_data$Group,
                                             colors = colors,
                                             text = selected_plot_data$hovertext,
                                             hoverinfo = "text")
                              if(nrow(unselected_plot_data) > 0){
                                unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                                unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                                fig <- add_trace(fig,
                                                 x = unselected_plot_data$Compound,
                                                 y = unselected_plot_data$Percentile_IC50*100,
                                                 color = unselected_plot_data$Group,
                                                 text = unselected_plot_data$hovertext,
                                                 hoverinfo = "text",
                                                 opacity = 0.2)
                              }
                              fig <- layout(fig,
                                            title = "CTRPv2 IC50 Percentiles",
                                            xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                            yaxis = list(title = ylab))
                            } else {
                              unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                              unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                              fig <- plot_ly(x = unselected_plot_data$Compound,
                                             y = unselected_plot_data$Percentile_IC50*100,
                                             type = "scatter",
                                             mode = "markers",
                                             color = unselected_plot_data$Group,
                                             colors = colors,
                                             text = unselected_plot_data$hovertext,
                                             hoverinfo = "text",
                                             opacity = 0.2)
                              
                              fig <- layout(fig,
                                            title = "CTRPv2 IC50 Percentiles",
                                            xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                            yaxis = list(title = ylab))
                            }
                            
                          } else {
                            fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_IC50*100,
                                         type = "scatter",
                                         mode = "markers",
                                         color = plot_data$Group,
                                         colors = colors,
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                            fig <- layout(fig,
                                          title = "CTRPv2 IC50 Percentiles",
                                          xaxis = list(title = paste0("CTRPv2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab))
                          }
                          
                          fig
                          
                        }
                      }
                    }
                  })

                output$Cell_Line_Explorer_GDSC1_Plot <- renderPlotly({
                  req(! length(input$Cell_Line_Explorer_Highlight_Compounds) == 0)
                    #Loading raw data for this cell_line and any datasets with data for this cell_line
                      if("GDSC1" %in% Cell_Line_Explorer_datasets_with_cell_line_data()){
                        GDSC1_Results <- Filtered_Cell_Line_Explorer_Data()$GDSC1
                        GDSC1_Results <- GDSC1_Results[! is.na(GDSC1_Results$b_c_d_e),]
                      } else {
                        GDSC1_Results <- data.frame(NULL)
                      }
                  #Making cell_line explorer GDSC1 plot
                    if(nrow(GDSC1_Results) > 0){
                      if(input$Cell_Line_Explorer_to_Plot == "AUC percentiles for most commonly used concentration range"){
                        plot_data <- GDSC1_Results[! is.na(GDSC1_Results$Percentile_AUC_Mode_CCL),]
                        plot_data <- plot_data[order(plot_data$Percentile_AUC_Mode_CCL, decreasing = FALSE),]
                        plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_GDSC1_conc] <- "Max Tested Concentration < AUC Range"
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                        plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_AUC_Mode_CCL,3)*100, "%, AUC=", signif(plot_data$AUC_mode_ccl_GDSC1_conc, 2), ")")
                        ylab <- "Percentile for AUC (most common range)"
                        
                        input$Cell_Line_Explorer_Compounds
                        colors <- setNames(rep(colorRampPalette(c("blue", "red"))(2), 3),
                                           c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range",
                                             "selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range",
                                             "unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range")
                                           )
                        if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                          plot_data$Group <- ": unselected"
                          plot_data$Group[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                          selected_plot_data <- plot_data[plot_data$Group == ": selected",]
                          unselected_plot_data <- plot_data[plot_data$Group == ": unselected",]
                          
                          if(nrow(selected_plot_data) > 0){
                            selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("selected: ", selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                            selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range"))
                            fig <- plot_ly(x = selected_plot_data$Compound,
                                       y = selected_plot_data$Percentile_AUC_Mode_CCL*100,
                                       color = selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                       colors = colors,
                                       type = "scatter",
                                       mode = "markers",
                                       text = selected_plot_data$hovertext,
                                       hoverinfo = "text")
                            if(nrow(unselected_plot_data) > 0){
                              unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("unselected: ", unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                              unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                              fig <- add_trace(fig,
                                               x = unselected_plot_data$Compound,
                                               y = unselected_plot_data$Percentile_AUC_Mode_CCL*100,
                                               color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                               text = unselected_plot_data$hovertext,
                                               hoverinfo = "text",
                                               opacity = 0.2)
                            }
                            
                            fig <- layout(fig,
                                        title = "GDSC1 AUC Percentiles",
                                        xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                            
                          } else if(nrow(unselected_plot_data) > 0){
                            unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("unselected: ", unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                            unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                            fig <- plot_ly(x = unselected_plot_data$Compound,
                                       y = unselected_plot_data$Percentile_AUC_Mode_CCL*100,
                                       color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                       colors = colors,
                                       type = "scatter",
                                       mode = "markers",
                                       text = unselected_plot_data$hovertext,
                                       hoverinfo = "text",
                                       opacity = 0.2)
                            
                            fig <- layout(fig,
                                        title = "GDSC1 AUC Percentiles",
                                        xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          }
                        } else {
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_Mode_CCL*100,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colors,
                                         type = "scatter",
                                         mode = "markers",
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                          
                          fig <- layout(fig,
                                        title = "GDSC1 AUC Percentiles",
                                        xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                        }
                        
                        fig
                        
                      } else if(input$Cell_Line_Explorer_to_Plot == "AUC percentiles for concentration range available for all tested cell lines"){
                        plot_data <- GDSC1_Results[! is.na(GDSC1_Results$Percentile_AUC_All_CCL),]
                        plot_data <- plot_data[order(plot_data$Percentile_AUC_All_CCL, decreasing = FALSE),]
                        plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                        plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_AUC_All_CCL,3)*100, "%, AUC=", signif(plot_data$AUC_all_ccl_GDSC1_conc, 2), ")")
                        ylab <- "Percentile for AUC (all cell line range)"
                        
                        if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                          plot_data$Group <- ": unselected"
                          plot_data$Group[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                          selected_plot_data <- plot_data[plot_data$Group == ": selected",]
                          unselected_plot_data <- plot_data[plot_data$Group == ": unselected",]
                          
                          if(nrow(selected_plot_data) > 0){
                            fig <- plot_ly(x = selected_plot_data$Compound,
                                           y = selected_plot_data$Percentile_AUC_All_CCL*100,
                                           type = "scatter",
                                           mode = "markers",
                                           color = factor(rep("A", nrow(selected_plot_data))),
                                           colors = colorRampPalette(c("blue"))(1),
                                           text = selected_plot_data$hovertext,
                                           hoverinfo = "text",
                                           name = "selected")
                            if(nrow(unselected_plot_data) > 0){
                              fig <- add_trace(fig,
                                               x = unselected_plot_data$Compound,
                                               y = unselected_plot_data$Percentile_AUC_All_CCL*100,
                                               text = unselected_plot_data$hovertext,
                                               color = factor(rep("A", nrow(unselected_plot_data))),
                                               hoverinfo = "text",
                                               name = "unselected",
                                               opacity = 0.2)
                            }
                            
                            fig <- layout(fig,
                                      title = "GDSC1 AUC Percentiles",
                                      xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab))
                          } else {
                            fig <- plot_ly(x = unselected_plot_data$Compound,
                                           y = unselected_plot_data$Percentile_AUC_All_CCL*100,
                                           type = "scatter",
                                           mode = "markers",
                                           color = factor(rep("A", nrow(unselected_plot_data))),
                                           colors = colorRampPalette(c("blue"))(1),
                                           text = unselected_plot_data$hovertext,
                                           hoverinfo = "text",
                                           name = "unselected",
                                           opacity = 0.2)
                            fig <- layout(fig,
                                      title = "GDSC1 AUC Percentiles",
                                      xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab))
                          }
                        } else {
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_All_CCL*100,
                                         type = "scatter",
                                         mode = "markers",
                                         color = factor(rep("A", nrow(plot_data))),
                                         colors = colorRampPalette(c("blue"))(1),
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                          fig <- layout(fig,
                                        title = "GDSC1 AUC Percentiles",
                                        xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                        }
                        
                        fig
                        
                      } else if(input$Cell_Line_Explorer_to_Plot == "IC50 percentiles"){
                        plot_data <- GDSC1_Results[! is.na(GDSC1_Results$Percentile_IC50),]
                        if(nrow(plot_data) > 0){
                          plot_data <- plot_data[order(plot_data$Percentile_IC50, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_IC50,3)*100, "%, IC50=", signif(plot_data$IC50, 3), "uM)")
                          plot_data$Group <- "IC50 <= max tested concentration"
                          plot_data$Group[plot_data$IC50 > plot_data$max_dose_uM] <- "IC50 > max tested concentration"
                          plot_data$Group[plot_data$IC50 == Inf] <- "Infinite IC50"
                          ylab <- "Percentile for IC50"

                          colors <- setNames(rep(colorRampPalette(c("blue", "red", "lightgray"))(3), 3),
                                           c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50",
                                             "selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50",
                                             "unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50")
                                           )
                          
                          if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                            plot_data$Group_s <- ": unselected"
                            plot_data$Group_s[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                            selected_plot_data <- plot_data[plot_data$Group_s == ": selected",]
                            unselected_plot_data <- plot_data[plot_data$Group_s == ": unselected",]
                            
                            if(nrow(selected_plot_data) > 0){
                              selected_plot_data$Group <- paste0("selected: ", selected_plot_data$Group)
                              selected_plot_data$Group <- factor(selected_plot_data$Group, levels = c("selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50"))
                              fig <- plot_ly(x = selected_plot_data$Compound,
                                             y = selected_plot_data$Percentile_IC50*100,
                                             type = "scatter",
                                             mode = "markers",
                                             color = selected_plot_data$Group,
                                             colors = colors,
                                             text = selected_plot_data$hovertext,
                                             hoverinfo = "text")
                              if(nrow(unselected_plot_data) > 0){
                                unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                                unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                                fig <- add_trace(fig,
                                                 x = unselected_plot_data$Compound,
                                                 y = unselected_plot_data$Percentile_IC50*100,
                                                 color = unselected_plot_data$Group,
                                                 text = unselected_plot_data$hovertext,
                                                 hoverinfo = "text",
                                                 opacity = 0.2)
                              }
                              fig <- layout(fig,
                                            title = "GDSC1 IC50 Percentiles",
                                            xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                            yaxis = list(title = ylab))
                            } else {
                              unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                              unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                              fig <- plot_ly(x = unselected_plot_data$Compound,
                                             y = unselected_plot_data$Percentile_IC50*100,
                                             type = "scatter",
                                             mode = "markers",
                                             color = unselected_plot_data$Group,
                                             colors = colors,
                                             text = unselected_plot_data$hovertext,
                                             hoverinfo = "text",
                                             opacity = 0.2)
                              
                              fig <- layout(fig,
                                            title = "GDSC1 IC50 Percentiles",
                                            xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                            yaxis = list(title = ylab))
                            }
                            
                          } else {
                            fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_IC50*100,
                                         type = "scatter",
                                         mode = "markers",
                                         color = plot_data$Group,
                                         colors = colors,
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                            fig <- layout(fig,
                                          title = "GDSC1 IC50 Percentiles",
                                          xaxis = list(title = paste0("GDSC1 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab))
                          }
                          
                          fig
                          
                        }
                      }
                    }
                  })
                
                output$Cell_Line_Explorer_GDSC2_Plot <- renderPlotly({
                  req(! length(input$Cell_Line_Explorer_Highlight_Compounds) == 0)
                    #Loading raw data for this cell_line and any datasets with data for this cell_line
                      if("GDSC2" %in% Cell_Line_Explorer_datasets_with_cell_line_data()){
                        GDSC2_Results <- Filtered_Cell_Line_Explorer_Data()$GDSC2
                        GDSC2_Results <- GDSC2_Results[! is.na(GDSC2_Results$b_c_d_e),]
                      } else {
                        GDSC2_Results <- data.frame(NULL)
                      }
                  #Making cell_line explorer GDSC2 plot
                    if(nrow(GDSC2_Results) > 0){
                      if(input$Cell_Line_Explorer_to_Plot == "AUC percentiles for most commonly used concentration range"){
                        plot_data <- GDSC2_Results[! is.na(GDSC2_Results$Percentile_AUC_Mode_CCL),]
                        plot_data <- plot_data[order(plot_data$Percentile_AUC_Mode_CCL, decreasing = FALSE),]
                        plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_GDSC2_conc] <- "Max Tested Concentration < AUC Range"
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                        plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_AUC_Mode_CCL,3)*100, "%, AUC=", signif(plot_data$AUC_mode_ccl_GDSC2_conc, 2), ")")
                        ylab <- "Percentile for AUC (most common range)"
                        
                        input$Cell_Line_Explorer_Compounds
                        colors <- setNames(rep(colorRampPalette(c("blue", "red"))(2), 3),
                                           c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range",
                                             "selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range",
                                             "unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range")
                                           )
                        if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                          plot_data$Group <- ": unselected"
                          plot_data$Group[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                          selected_plot_data <- plot_data[plot_data$Group == ": selected",]
                          unselected_plot_data <- plot_data[plot_data$Group == ": unselected",]
                          
                          if(nrow(selected_plot_data) > 0){
                            selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("selected: ", selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                            selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range"))
                            fig <- plot_ly(x = selected_plot_data$Compound,
                                       y = selected_plot_data$Percentile_AUC_Mode_CCL*100,
                                       color = selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                       colors = colors,
                                       type = "scatter",
                                       mode = "markers",
                                       text = selected_plot_data$hovertext,
                                       hoverinfo = "text")
                            if(nrow(unselected_plot_data) > 0){
                              unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("unselected: ", unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                              unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                              fig <- add_trace(fig,
                                               x = unselected_plot_data$Compound,
                                               y = unselected_plot_data$Percentile_AUC_Mode_CCL*100,
                                               color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                               text = unselected_plot_data$hovertext,
                                               hoverinfo = "text",
                                               opacity = 0.2)
                            }
                            
                            fig <- layout(fig,
                                        title = "GDSC2 AUC Percentiles",
                                        xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                            
                          } else if(nrow(unselected_plot_data) > 0){
                            unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("unselected: ", unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                            unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                            fig <- plot_ly(x = unselected_plot_data$Compound,
                                       y = unselected_plot_data$Percentile_AUC_Mode_CCL*100,
                                       color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                       colors = colors,
                                       type = "scatter",
                                       mode = "markers",
                                       text = unselected_plot_data$hovertext,
                                       hoverinfo = "text",
                                       opacity = 0.2)
                            
                            fig <- layout(fig,
                                        title = "GDSC2 AUC Percentiles",
                                        xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          }
                        } else {
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_Mode_CCL*100,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colors,
                                         type = "scatter",
                                         mode = "markers",
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                          
                          fig <- layout(fig,
                                        title = "GDSC2 AUC Percentiles",
                                        xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                        }
                        
                        fig
                        
                      } else if(input$Cell_Line_Explorer_to_Plot == "AUC percentiles for concentration range available for all tested cell lines"){
                        plot_data <- GDSC2_Results[! is.na(GDSC2_Results$Percentile_AUC_All_CCL),]
                        plot_data <- plot_data[order(plot_data$Percentile_AUC_All_CCL, decreasing = FALSE),]
                        plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                        plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_AUC_All_CCL,3)*100, "%, AUC=", signif(plot_data$AUC_all_ccl_GDSC2_conc, 2), ")")
                        ylab <- "Percentile for AUC (all cell line range)"
                        
                        if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                          plot_data$Group <- ": unselected"
                          plot_data$Group[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                          selected_plot_data <- plot_data[plot_data$Group == ": selected",]
                          unselected_plot_data <- plot_data[plot_data$Group == ": unselected",]
                          
                          if(nrow(selected_plot_data) > 0){
                            fig <- plot_ly(x = selected_plot_data$Compound,
                                           y = selected_plot_data$Percentile_AUC_All_CCL*100,
                                           type = "scatter",
                                           mode = "markers",
                                           color = factor(rep("A", nrow(selected_plot_data))),
                                           colors = colorRampPalette(c("blue"))(1),
                                           text = selected_plot_data$hovertext,
                                           hoverinfo = "text",
                                           name = "selected")
                            if(nrow(unselected_plot_data) > 0){
                              fig <- add_trace(fig,
                                               x = unselected_plot_data$Compound,
                                               y = unselected_plot_data$Percentile_AUC_All_CCL*100,
                                               text = unselected_plot_data$hovertext,
                                               color = factor(rep("A", nrow(unselected_plot_data))),
                                               hoverinfo = "text",
                                               name = "unselected",
                                               opacity = 0.2)
                            }
                            
                            fig <- layout(fig,
                                      title = "GDSC2 AUC Percentiles",
                                      xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab))
                          } else {
                            fig <- plot_ly(x = unselected_plot_data$Compound,
                                           y = unselected_plot_data$Percentile_AUC_All_CCL*100,
                                           type = "scatter",
                                           mode = "markers",
                                           color = factor(rep("A", nrow(unselected_plot_data))),
                                           colors = colorRampPalette(c("blue"))(1),
                                           text = unselected_plot_data$hovertext,
                                           hoverinfo = "text",
                                           name = "unselected",
                                           opacity = 0.2)
                            fig <- layout(fig,
                                      title = "GDSC2 AUC Percentiles",
                                      xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab))
                          }
                        } else {
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_All_CCL*100,
                                         type = "scatter",
                                         mode = "markers",
                                         color = factor(rep("A", nrow(plot_data))),
                                         colors = colorRampPalette(c("blue"))(1),
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                          fig <- layout(fig,
                                        title = "GDSC2 AUC Percentiles",
                                        xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                        }
                        
                        fig
                        
                      } else if(input$Cell_Line_Explorer_to_Plot == "IC50 percentiles"){
                        plot_data <- GDSC2_Results[! is.na(GDSC2_Results$Percentile_IC50),]
                        if(nrow(plot_data) > 0){
                          plot_data <- plot_data[order(plot_data$Percentile_IC50, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_IC50,3)*100, "%, IC50=", signif(plot_data$IC50, 3), "uM)")
                          plot_data$Group <- "IC50 <= max tested concentration"
                          plot_data$Group[plot_data$IC50 > plot_data$max_dose_uM] <- "IC50 > max tested concentration"
                          plot_data$Group[plot_data$IC50 == Inf] <- "Infinite IC50"
                          ylab <- "Percentile for IC50"

                          colors <- setNames(rep(colorRampPalette(c("blue", "red", "lightgray"))(3), 3),
                                           c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50",
                                             "selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50",
                                             "unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50")
                                           )
                          
                          if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                            plot_data$Group_s <- ": unselected"
                            plot_data$Group_s[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                            selected_plot_data <- plot_data[plot_data$Group_s == ": selected",]
                            unselected_plot_data <- plot_data[plot_data$Group_s == ": unselected",]
                            
                            if(nrow(selected_plot_data) > 0){
                              selected_plot_data$Group <- paste0("selected: ", selected_plot_data$Group)
                              selected_plot_data$Group <- factor(selected_plot_data$Group, levels = c("selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50"))
                              fig <- plot_ly(x = selected_plot_data$Compound,
                                             y = selected_plot_data$Percentile_IC50*100,
                                             type = "scatter",
                                             mode = "markers",
                                             color = selected_plot_data$Group,
                                             colors = colors,
                                             text = selected_plot_data$hovertext,
                                             hoverinfo = "text")
                              if(nrow(unselected_plot_data) > 0){
                                unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                                unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                                fig <- add_trace(fig,
                                                 x = unselected_plot_data$Compound,
                                                 y = unselected_plot_data$Percentile_IC50*100,
                                                 color = unselected_plot_data$Group,
                                                 text = unselected_plot_data$hovertext,
                                                 hoverinfo = "text",
                                                 opacity = 0.2)
                              }
                              fig <- layout(fig,
                                            title = "GDSC2 IC50 Percentiles",
                                            xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                            yaxis = list(title = ylab))
                            } else {
                              unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                              unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                              fig <- plot_ly(x = unselected_plot_data$Compound,
                                             y = unselected_plot_data$Percentile_IC50*100,
                                             type = "scatter",
                                             mode = "markers",
                                             color = unselected_plot_data$Group,
                                             colors = colors,
                                             text = unselected_plot_data$hovertext,
                                             hoverinfo = "text",
                                             opacity = 0.2)
                              
                              fig <- layout(fig,
                                            title = "GDSC2 IC50 Percentiles",
                                            xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                            yaxis = list(title = ylab))
                            }
                            
                          } else {
                            fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_IC50*100,
                                         type = "scatter",
                                         mode = "markers",
                                         color = plot_data$Group,
                                         colors = colors,
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                            fig <- layout(fig,
                                          title = "GDSC2 IC50 Percentiles",
                                          xaxis = list(title = paste0("GDSC2 Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab))
                          }
                          
                          fig
                          
                        }
                      }
                    }
                  })
                
                output$Cell_Line_Explorer_PRISM_Repurposing_Plot <- renderPlotly({
                  req(! length(input$Cell_Line_Explorer_Highlight_Compounds) == 0)
                    #Loading raw data for this cell_line and any datasets with data for this cell_line
                      if("PRISM_Repurposing" %in% Cell_Line_Explorer_datasets_with_cell_line_data()){
                        PRISM_Repurposing_Results <- Filtered_Cell_Line_Explorer_Data()$PRISM_Repurposing
                        PRISM_Repurposing_Results <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$b_c_d_e),]
                      } else {
                        PRISM_Repurposing_Results <- data.frame(NULL)
                      }
                  #Making cell_line explorer PRISM_Repurposing plot
                    if(nrow(PRISM_Repurposing_Results) > 0){
                      if(input$Cell_Line_Explorer_to_Plot == "AUC percentiles for most commonly used concentration range"){
                        plot_data <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$Percentile_AUC_Mode_CCL),]
                        plot_data <- plot_data[order(plot_data$Percentile_AUC_Mode_CCL, decreasing = FALSE),]
                        plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- "Max Tested Concentration Within AUC Range"
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration[plot_data$max_dose_uM < plot_data$max_mode_ccl_PRISM_Repurposing_conc] <- "Max Tested Concentration < AUC Range"
                        plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range"))
                        plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_AUC_Mode_CCL,3)*100, "%, AUC=", signif(plot_data$AUC_mode_ccl_PRISM_Repurposing_conc, 2), ")")
                        ylab <- "Percentile for AUC (most common range)"
                        
                        input$Cell_Line_Explorer_Compounds
                        colors <- setNames(rep(colorRampPalette(c("blue", "red"))(2), 3),
                                           c("Max Tested Concentration Within AUC Range", "Max Tested Concentration < AUC Range",
                                             "selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range",
                                             "unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range")
                                           )
                        if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                          plot_data$Group <- ": unselected"
                          plot_data$Group[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                          selected_plot_data <- plot_data[plot_data$Group == ": selected",]
                          unselected_plot_data <- plot_data[plot_data$Group == ": unselected",]
                          
                          if(nrow(selected_plot_data) > 0){
                            selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("selected: ", selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                            selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("selected: Max Tested Concentration Within AUC Range", "selected: Max Tested Concentration < AUC Range"))
                            fig <- plot_ly(x = selected_plot_data$Compound,
                                       y = selected_plot_data$Percentile_AUC_Mode_CCL*100,
                                       color = selected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                       colors = colors,
                                       type = "scatter",
                                       mode = "markers",
                                       text = selected_plot_data$hovertext,
                                       hoverinfo = "text")
                            if(nrow(unselected_plot_data) > 0){
                              unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("unselected: ", unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                              unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                              fig <- add_trace(fig,
                                               x = unselected_plot_data$Compound,
                                               y = unselected_plot_data$Percentile_AUC_Mode_CCL*100,
                                               color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                               text = unselected_plot_data$hovertext,
                                               hoverinfo = "text",
                                               opacity = 0.2)
                            }
                            
                            fig <- layout(fig,
                                        title = "PRISM_Repurposing AUC Percentiles",
                                        xaxis = list(title = paste0("PRISM_Repurposing Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                            
                          } else if(nrow(unselected_plot_data) > 0){
                            unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- paste0("unselected: ", unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration)
                            unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration <- factor(unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration, levels = c("unselected: Max Tested Concentration Within AUC Range", "unselected: Max Tested Concentration < AUC Range"))
                            fig <- plot_ly(x = unselected_plot_data$Compound,
                                       y = unselected_plot_data$Percentile_AUC_Mode_CCL*100,
                                       color = unselected_plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                       colors = colors,
                                       type = "scatter",
                                       mode = "markers",
                                       text = unselected_plot_data$hovertext,
                                       hoverinfo = "text",
                                       opacity = 0.2)
                            
                            fig <- layout(fig,
                                        title = "PRISM_Repurposing AUC Percentiles",
                                        xaxis = list(title = paste0("PRISM_Repurposing Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                          }
                        } else {
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_Mode_CCL*100,
                                         color = plot_data$Concentration_Range_Exceeds_Max_Tested_Concentration,
                                         colors = colors,
                                         type = "scatter",
                                         mode = "markers",
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                          
                          fig <- layout(fig,
                                        title = "PRISM_Repurposing AUC Percentiles",
                                        xaxis = list(title = paste0("PRISM_Repurposing Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                        }
                        
                        fig
                        
                      } else if(input$Cell_Line_Explorer_to_Plot == "AUC percentiles for concentration range available for all tested cell lines"){
                        plot_data <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$Percentile_AUC_All_CCL),]
                        plot_data <- plot_data[order(plot_data$Percentile_AUC_All_CCL, decreasing = FALSE),]
                        plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                        plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_AUC_All_CCL,3)*100, "%, AUC=", signif(plot_data$AUC_all_ccl_PRISM_Repurposing_conc, 2), ")")
                        ylab <- "Percentile for AUC (all cell line range)"
                        
                        if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                          plot_data$Group <- ": unselected"
                          plot_data$Group[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                          selected_plot_data <- plot_data[plot_data$Group == ": selected",]
                          unselected_plot_data <- plot_data[plot_data$Group == ": unselected",]
                          
                          if(nrow(selected_plot_data) > 0){
                            fig <- plot_ly(x = selected_plot_data$Compound,
                                           y = selected_plot_data$Percentile_AUC_All_CCL*100,
                                           type = "scatter",
                                           mode = "markers",
                                           color = factor(rep("A", nrow(selected_plot_data))),
                                           colors = colorRampPalette(c("blue"))(1),
                                           text = selected_plot_data$hovertext,
                                           hoverinfo = "text",
                                           name = "selected")
                            if(nrow(unselected_plot_data) > 0){
                              fig <- add_trace(fig,
                                               x = unselected_plot_data$Compound,
                                               y = unselected_plot_data$Percentile_AUC_All_CCL*100,
                                               text = unselected_plot_data$hovertext,
                                               color = factor(rep("A", nrow(unselected_plot_data))),
                                               hoverinfo = "text",
                                               name = "unselected",
                                               opacity = 0.2)
                            }
                            
                            fig <- layout(fig,
                                      title = "PRISM_Repurposing AUC Percentiles",
                                      xaxis = list(title = paste0("PRISM_Repurposing Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab))
                          } else {
                            fig <- plot_ly(x = unselected_plot_data$Compound,
                                           y = unselected_plot_data$Percentile_AUC_All_CCL*100,
                                           type = "scatter",
                                           mode = "markers",
                                           color = factor(rep("A", nrow(unselected_plot_data))),
                                           colors = colorRampPalette(c("blue"))(1),
                                           text = unselected_plot_data$hovertext,
                                           hoverinfo = "text",
                                           name = "unselected",
                                           opacity = 0.2)
                            fig <- layout(fig,
                                      title = "PRISM_Repurposing AUC Percentiles",
                                      xaxis = list(title = paste0("PRISM_Repurposing Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                      yaxis = list(title = ylab))
                          }
                        } else {
                          fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_AUC_All_CCL*100,
                                         type = "scatter",
                                         mode = "markers",
                                         color = factor(rep("A", nrow(plot_data))),
                                         colors = colorRampPalette(c("blue"))(1),
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                          fig <- layout(fig,
                                        title = "PRISM_Repurposing AUC Percentiles",
                                        xaxis = list(title = paste0("PRISM_Repurposing Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                        yaxis = list(title = ylab))
                        }
                        
                        fig
                        
                      } else if(input$Cell_Line_Explorer_to_Plot == "IC50 percentiles"){
                        plot_data <- PRISM_Repurposing_Results[! is.na(PRISM_Repurposing_Results$Percentile_IC50),]
                        if(nrow(plot_data) > 0){
                          plot_data <- plot_data[order(plot_data$Percentile_IC50, decreasing = FALSE),]
                          plot_data$Compound <- factor(plot_data$Compound, levels = plot_data$Compound)
                          plot_data$hovertext <- paste0("(", plot_data$Compound, ", ", signif(plot_data$Percentile_IC50,3)*100, "%, IC50=", signif(plot_data$IC50, 3), "uM)")
                          plot_data$Group <- "IC50 <= max tested concentration"
                          plot_data$Group[plot_data$IC50 > plot_data$max_dose_uM] <- "IC50 > max tested concentration"
                          plot_data$Group[plot_data$IC50 == Inf] <- "Infinite IC50"
                          ylab <- "Percentile for IC50"

                          colors <- setNames(rep(colorRampPalette(c("blue", "red", "lightgray"))(3), 3),
                                           c("IC50 <= max tested concentration", "IC50 > max tested concentration", "Infinite IC50",
                                             "selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50",
                                             "unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50")
                                           )
                          
                          if(input$Cell_Line_Explorer_Highlight_Compounds == TRUE){
                            plot_data$Group_s <- ": unselected"
                            plot_data$Group_s[plot_data$Compound %in% input$Cell_Line_Explorer_Compounds] <- ": selected"
                            selected_plot_data <- plot_data[plot_data$Group_s == ": selected",]
                            unselected_plot_data <- plot_data[plot_data$Group_s == ": unselected",]
                            
                            if(nrow(selected_plot_data) > 0){
                              selected_plot_data$Group <- paste0("selected: ", selected_plot_data$Group)
                              selected_plot_data$Group <- factor(selected_plot_data$Group, levels = c("selected: IC50 <= max tested concentration", "selected: IC50 > max tested concentration", "selected: Infinite IC50"))
                              fig <- plot_ly(x = selected_plot_data$Compound,
                                             y = selected_plot_data$Percentile_IC50*100,
                                             type = "scatter",
                                             mode = "markers",
                                             color = selected_plot_data$Group,
                                             colors = colors,
                                             text = selected_plot_data$hovertext,
                                             hoverinfo = "text")
                              if(nrow(unselected_plot_data) > 0){
                                unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                                unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                                fig <- add_trace(fig,
                                                 x = unselected_plot_data$Compound,
                                                 y = unselected_plot_data$Percentile_IC50*100,
                                                 color = unselected_plot_data$Group,
                                                 text = unselected_plot_data$hovertext,
                                                 hoverinfo = "text",
                                                 opacity = 0.2)
                              }
                              fig <- layout(fig,
                                            title = "PRISM_Repurposing IC50 Percentiles",
                                            xaxis = list(title = paste0("PRISM_Repurposing Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                            yaxis = list(title = ylab))
                            } else {
                              unselected_plot_data$Group <- paste0("unselected: ", unselected_plot_data$Group)
                              unselected_plot_data$Group <- factor(unselected_plot_data$Group, levels = c("unselected: IC50 <= max tested concentration", "unselected: IC50 > max tested concentration", "unselected: Infinite IC50"))
                              fig <- plot_ly(x = unselected_plot_data$Compound,
                                             y = unselected_plot_data$Percentile_IC50*100,
                                             type = "scatter",
                                             mode = "markers",
                                             color = unselected_plot_data$Group,
                                             colors = colors,
                                             text = unselected_plot_data$hovertext,
                                             hoverinfo = "text",
                                             opacity = 0.2)
                              
                              fig <- layout(fig,
                                            title = "PRISM_Repurposing IC50 Percentiles",
                                            xaxis = list(title = paste0("PRISM_Repurposing Compounds (n = ", nrow(plot_data), ", ", nrow(selected_plot_data), " selected)"), showticklabels = FALSE),
                                            yaxis = list(title = ylab))
                            }
                            
                          } else {
                            fig <- plot_ly(x = plot_data$Compound,
                                         y = plot_data$Percentile_IC50*100,
                                         type = "scatter",
                                         mode = "markers",
                                         color = plot_data$Group,
                                         colors = colors,
                                         text = plot_data$hovertext,
                                         hoverinfo = "text")
                            fig <- layout(fig,
                                          title = "PRISM_Repurposing IC50 Percentiles",
                                          xaxis = list(title = paste0("PRISM_Repurposing Compounds (n = ", nrow(plot_data), ")"), showticklabels = FALSE),
                                          yaxis = list(title = ylab))
                          }
                          
                          fig
                          
                        }
                      }
                    }
                  })
                
              }) #END: Isolate
            }) #END: observeEvent(Cell_Line_Explorer_datasets_with_cell_line_data(), {


          
#############################################################          
#################################################### server #          
      #Code for "Plot Dose-Response Curves" tab
        
        #Initializing compound choice menu
          updateSelectizeInput(session, "plot_compound", label = paste0("Select a compound (n = ", nrow(Compound_Option_df), ")"), choices = cbind(Compound_Option_df, value = Compound_Option_df$Harmonized_Compound_Name), server = TRUE)
      
        #Defining compound filter interface
          observeEvent(input$Plot_Show_Compound_Filters, {
            isolate({
              output$Plot_Compound_Filters <- renderUI({
                if(input$Plot_Show_Compound_Filters == TRUE){
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
                      pickerInput(inputId = "Plot_Molecular_Target", label = "Filter compounds by molecular target", choices = Unique_Compound_Molecular_Targets, selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")),
                      pickerInput(inputId = "Plot_MOA", label = "Filter compounds by free-text MOA", choices = Unique_Compound_MOAs, selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")),
                      pickerInput(inputId = "Plot_Clinical_Phase", label = "Filter compounds by clinical phase", choices = sort(unique(Simple_Compound_Harm$Compound_Clinical_Phase)), selected = NULL, multiple = TRUE, options = list(`selected-Text-Format` = "count", `none-Selected-Text` = "Optional", `max-options` = 1))
                    ))
                  ))
                } else {
                  return(list(p(" ")))
                }
              })
            })
          })
            
        #Updating compound filter interface
          #Defining currently available compounds based on compound filters
            Plot_Currently_Available_Compounds <- reactive({
              
              Avail_compounds <- sort(unique(Simple_Compound_Harm$Harmonized_Compound_Name))
              Filtered_compounds <- character(0)
              flag <- 0
              
              
              if(! is.null(input$Plot_Molecular_Target)){
                flag <- 1
                Filtered_compounds <- Avail_compounds[Avail_compounds %in% names(Compound_Molecular_Targets)[sapply(Compound_Molecular_Targets, function(x,y){all(y %in% x)}, y = input$Plot_Molecular_Target)]]
              }
              
              if(! is.null(input$Plot_MOA)){
                if(flag == 0){
                  Filtered_compounds <- Avail_compounds[Avail_compounds %in% names(Compound_MOAs)[sapply(Compound_MOAs, function(x,y){all(y %in% x)}, y = input$Plot_MOA)]]
                } else if(flag == 1){
                  Filtered_compounds <- Filtered_compounds[Filtered_compounds %in% names(Compound_MOAs)[sapply(Compound_MOAs, function(x,y){all(y %in% x)}, y = input$Plot_MOA)]]
                }
                flag <- 1
              }
              
              if(! is.null(input$Plot_Clinical_Phase)){
                if(flag == 0){
                  Filtered_compounds <- Avail_compounds[Avail_compounds %in% Simple_Compound_Harm$Harmonized_Compound_Name[Simple_Compound_Harm$Compound_Clinical_Phase %in% input$Plot_Clinical_Phase]]
                } else if(flag == 1){
                  Filtered_compounds <- Filtered_compounds[Filtered_compounds %in% Simple_Compound_Harm$Harmonized_Compound_Name[Simple_Compound_Harm$Compound_Clinical_Phase %in% input$Plot_Clinical_Phase]]
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
            observeEvent(Plot_Currently_Available_Compounds(), {
              isolate({
              #Updating compound filter menus
                #Molecular target menu
                  Plot_Available_Molecular_Targets <- sort(unique(unlist(Compound_Molecular_Targets[names(Compound_Molecular_Targets) %in% Plot_Currently_Available_Compounds()])))
                  previous_selection <- input$Plot_Molecular_Target
                  updatePickerInput(session, "Plot_Molecular_Target", choices = Plot_Available_Molecular_Targets, selected = previous_selection)
                #MOA menu
                  Plot_Available_MOAs <- sort(unique(unlist(Compound_MOAs[names(Compound_MOAs) %in% Plot_Currently_Available_Compounds()])))
                  previous_selection <- input$Plot_MOA
                  updatePickerInput(session, "Plot_MOA", choices = Plot_Available_MOAs, selected = previous_selection)
                #Clinical stage menu
                  Plot_Available_Clinical_Phases <- sort(unique(Simple_Compound_Harm$Compound_Clinical_Phase[Simple_Compound_Harm$Harmonized_Compound_Name %in% Plot_Currently_Available_Compounds()]))
                  previous_selection <- input$Plot_Clinical_Phase
                  updatePickerInput(session, "Plot_Clinical_Phase", choices = Plot_Available_Clinical_Phases, selected = previous_selection)
                  })
            })
            
          #Updating compound selection menu
            Plot_Selected_Compound_Option_df <- reactive({
              filtered_Compound_Option_df <- Compound_Option_df[Compound_Option_df$Harmonized_Compound_Name %in% Plot_Currently_Available_Compounds(),]
              return(cbind(filtered_Compound_Option_df, value = filtered_Compound_Option_df$Harmonized_Compound_Name))
            })
            
            observeEvent(Plot_Selected_Compound_Option_df(),{
              isolate({
                prev_selection <- input$plot_compound
                if(! length(prev_selection) == 0){
                  if(! prev_selection %in% Plot_Selected_Compound_Option_df()$Harmonized_Compound_Name){
                    prev_selection <- character(0)
                  }
                }
                updateSelectizeInput(session, "plot_compound", label = paste0("Select a compound (n = ", nrow(Plot_Selected_Compound_Option_df()), ")"), choices = Plot_Selected_Compound_Option_df(), server = TRUE, selected = prev_selection)
              })
            })
            
          #Loading data for selected compound
            DR_Plot_Compound_Data <- eventReactive(input$plot_compound, {
              isolate({
                req(input$plot_compound)
  
                filename <- Compound_Filenames$drug_file_names[Compound_Filenames$drugs == input$plot_compound]
                  data <- readRDS(paste0("./www/Results/", filename, ".rds"))
                  for(i in 1:length(data)){
                    data[[i]] <- data[[i]][! is.na(data[[i]]$b_c_d_e),]
                  }
                  return(data)
              })
            })
            
          #Defining cell lines available for selected compound
            Plot_Available_Cell_Lines_for_Compound <- reactive({
              req(DR_Plot_Compound_Data())
  
              cell_lines <- character(0)
              for(i in 1:length(DR_Plot_Compound_Data())){
                cell_lines <- c(cell_lines, DR_Plot_Compound_Data()[[i]]$Cell_Line)
              }
              return(sort(unique(cell_lines)))
            })
            
          #Subsetting cell line info to cell lines available for this compound
            Plot_Simple_Cell_Line_Harm_for_Compound <- reactive({
              req(Plot_Available_Cell_Lines_for_Compound())
  
              Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% Plot_Available_Cell_Lines_for_Compound(),]
            })
            
          #Getting age limits
            Plot_Age_Limits <- reactive({
              req(Plot_Simple_Cell_Line_Harm_for_Compound())
  
                Temp_CL_Harm_Data <- Plot_Simple_Cell_Line_Harm_for_Compound()
                temp_min <- suppressWarnings(min(Temp_CL_Harm_Data$Numeric_Age_in_Years, na.rm = TRUE))
                if(temp_min == Inf){
                  temp_min <- 0
                }
                temp_max <- suppressWarnings(max(Temp_CL_Harm_Data$Numeric_Age_in_Years, na.rm = TRUE))
                if(temp_max == -Inf){
                  temp_max <- 0
                }
                return(c(temp_min, temp_max))
            })
        
        #Defining cell line selection menu interface
          observeEvent(input$plot_compound, {
            output$Plot_Cell_Line_Menu <- renderUI({
              if(! input$plot_compound == ""){
                list(
                selectizeInput("plot_cell_line", label = paste0("Select a cell line"), choices = NULL, multiple = FALSE, options = list(
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
                checkboxInput("Plot_Show_Cell_Line_Filters", label = "Show cell line filters?", value = FALSE),
                uiOutput(outputId = "Plot_Cell_Line_Filters"))
              } else {
                list(NULL)
              }
            })
          })
          
                
        #Defining cell line filter interface
          observeEvent(input$Plot_Show_Cell_Line_Filters, {
            output$Plot_Cell_Line_Filters <- renderUI({
              if(input$Plot_Show_Cell_Line_Filters == TRUE){
                Plot_Initial_Age_Limits <- Plot_Age_Limits()
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
                    pickerInput(inputId = "Plot_Cancer_Type", label = "General cancer type", choices = sort(unique(Simple_Cell_Line_Harm$Simple_Cancer_Type)), multiple = TRUE, options = list(`actions-Box` = TRUE, `live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional", `max-options` = 1)) %>%
                    helper(type = "inline",
                      title = "General cancer type filtering",
                      icon = "question-circle", colour = NULL,
                      content = c("Filters cell lines by a generally broad cancer type classification."),
                      size = "m",
                      buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                    ),
                    pickerInput(inputId = "Plot_Disease_Name", label = "Free-text disease name", choices = sort(unique(unlist(Cell_Line_Diseases))), multiple = TRUE, options = list(`actions-Box` = TRUE, `live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                    helper(type = "inline",
                      title = "Free-text disease name filtering",
                      icon = "question-circle", colour = NULL,
                      content = c("Filters cell lines by free-text descriptions of their disease name. If multiple options are selected, only cell lines that are annotated with all of the selected options will be shown."),
                      size = "m",
                      buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                    ),
                    pickerInput(inputId = "Plot_Gender", label = "Gender", choices = sort(unique(Simple_Cell_Line_Harm$Gender)), multiple = TRUE, options = list(`selected-Text-Format` = "count", `none-Selected-Text` = "Optional", `max-options` = 1)) %>%
                    helper(type = "inline",
                      title = "Gender filtering",
                      icon = "question-circle", colour = NULL,
                      content = c("Filters cell lines by annotated gender."),
                      size = "m",
                      buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                    ),

                    sliderInput(inputId = "Plot_Age", label = "Patient age (years)", min = Plot_Initial_Age_Limits[1], max = Plot_Initial_Age_Limits[2], value = Plot_Initial_Age_Limits, ticks = FALSE) %>%
                    helper(type = "inline",
                      title = "Patient age filtering",
                      icon = "question-circle", colour = NULL,
                      content = c("Sets minimum and maximum age allowed for displayed cell line options. Note that setting the slider to anything other than its maximum range will exclude cell lines for which a numeric age could not be determined for the patient who the cell line was derived from at the time of sample collection. This both includes cases where the age is unspecified and cases where the specified age is ambiguous (i.e. such as \"Adult\"). You may download the cell line harmonization file in the \"Download Bulk Data\" tab for free-text descriptions of each cell line's patient age."),
                      size = "m",
                      buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                    ),

                    h4("Ancestry") %>%
                    helper(type = "inline",
                      title = "Patient age filtering",
                      icon = "question-circle", colour = NULL,
                      content = c("Each slider sets the minimum and maximum % ancestry for each ancestry group. Once any of the ancestry sliders has been changed from its maximum range, cell lines will be filtered to only include lines which meet the limits set on all of the ancestry sliders. Note that % ancestry adds to 100% across all ancestry groups for each individual cell line (i.e. a cell line cannot have 60% African ancestry and 60% Native American ancestry, because that would add to >100%). Ancestry information was obtained from the cellosaurus resource at <a href=\"https://www.expasy.org/\">https://www.expasy.org/</a>."),
                      size = "m",
                      buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                    ),
                    wellPanel(list(
                      sliderInput(inputId = "Plot_African", label = "% african ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                      sliderInput(inputId = "Plot_Native_American", label = "% native american ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                      sliderInput(inputId = "Plot_East_Asian_North", label = "% east asian (north) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                      sliderInput(inputId = "Plot_East_Asian_South", label = "% east asian (south) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                      sliderInput(inputId = "Plot_South_Asian", label = "% south asian ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                      sliderInput(inputId = "Plot_European_North", label = "% european (north) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                      sliderInput(inputId = "Plot_European_South", label = "% european (south) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE)
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
            Plot_Currently_Available_Cell_Lines <- reactive({

              Temp_CL_Harm_Data <- Plot_Simple_Cell_Line_Harm_for_Compound()
                temp_Cell_Line_Diseases <- strsplit(Temp_CL_Harm_Data$Diseases, ":\\|:")
                names(temp_Cell_Line_Diseases) <- Temp_CL_Harm_Data$Harmonized_Cell_Line_ID

              Plot_Initial_Age_Limits <- Plot_Age_Limits()

              Avail_ccls <- sort(unique(Temp_CL_Harm_Data$Harmonized_Cell_Line_ID))
              Filtered_ccls <- character(0)
              flag <- 0

              if(! is.null(input$Plot_Cancer_Type)){
                flag <- 1
                Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Simple_Cancer_Type %in% input$Plot_Cancer_Type]]))
              }

              if(! is.null(input$Plot_Disease_Name)){
                if(flag == 0){
                  Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% names(temp_Cell_Line_Diseases)[sapply(temp_Cell_Line_Diseases, function(x,y){return(any(y %in% x))}, y = input$Plot_Disease_Name)]]))
                } else if(flag == 1){
                  Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% names(temp_Cell_Line_Diseases)[sapply(temp_Cell_Line_Diseases, function(x,y){return(any(y %in% x))}, y = input$Plot_Disease_Name)]]]))
                }
                flag <- 1
              }

              if(! is.null(input$Plot_Gender)){
                if(flag == 0){
                  Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Gender %in% input$Plot_Gender]]))
                } else if(flag == 1){
                  Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Gender %in% input$Plot_Gender]]]))
                }
                flag <- 1
              }

              if(! all(input$Plot_Age == Plot_Initial_Age_Limits)){
                if(flag == 0){
                  Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Numeric_Age_in_Years >= input$Plot_Age[1] &
                                                                                                       Temp_CL_Harm_Data$Numeric_Age_in_Years <= input$Plot_Age[2]]]))
                } else if(flag == 1){
                  Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$Numeric_Age_in_Years >= input$Plot_Age[1] &
                                                                                                                                                    Temp_CL_Harm_Data$Numeric_Age_in_Years <= input$Plot_Age[2]]]]))
                }
                flag <- 1
              }

              if(! all(input$Plot_African == c(0,100)) |
                 ! all(input$Plot_Native_American == c(0,100)) |
                 ! all(input$Plot_East_Asian_North == c(0,100)) |
                 ! all(input$Plot_East_Asian_South == c(0,100)) |
                 ! all(input$Plot_South_Asian == c(0,100)) |
                 ! all(input$Plot_European_North == c(0,100)) |
                 ! all(input$Plot_European_South == c(0,100))){
                if(flag == 0){
                  Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$African_Ancestry >= input$Plot_African[1]/100 &
                                                                                                       Temp_CL_Harm_Data$African_Ancestry <= input$Plot_African[2]/100 &
                                                                                                       Temp_CL_Harm_Data$Native_American_Ancestry >= input$Plot_Native_American[1]/100 &
                                                                                                       Temp_CL_Harm_Data$Native_American_Ancestry <= input$Plot_Native_American[2]/100 &
                                                                                                       Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` >= input$Plot_East_Asian_North[1]/100 &
                                                                                                       Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` <= input$Plot_East_Asian_North[2]/100 &
                                                                                                       Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` >= input$Plot_East_Asian_South[1]/100 &
                                                                                                       Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` <= input$Plot_East_Asian_South[2]/100 &
                                                                                                       Temp_CL_Harm_Data$South_Asian_Ancestry >= input$Plot_South_Asian[1]/100 &
                                                                                                       Temp_CL_Harm_Data$South_Asian_Ancestry <= input$Plot_South_Asian[2]/100 &
                                                                                                       Temp_CL_Harm_Data$`European_(North)_Ancestry` >= input$Plot_European_North[1]/100 &
                                                                                                       Temp_CL_Harm_Data$`European_(North)_Ancestry` <= input$Plot_European_North[2]/100 &
                                                                                                       Temp_CL_Harm_Data$`European_(South)_Ancestry` >= input$Plot_European_South[1]/100 &
                                                                                                       Temp_CL_Harm_Data$`European_(South)_Ancestry` <= input$Plot_European_South[2]/100]]))
                } else if(flag == 1){
                  Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Temp_CL_Harm_Data$Harmonized_Cell_Line_ID[Temp_CL_Harm_Data$African_Ancestry >= input$Plot_African[1]/100 &
                                                                                                       Temp_CL_Harm_Data$African_Ancestry <= input$Plot_African[2]/100 &
                                                                                                       Temp_CL_Harm_Data$Native_American_Ancestry >= input$Plot_Native_American[1]/100 &
                                                                                                       Temp_CL_Harm_Data$Native_American_Ancestry <= input$Plot_Native_American[2]/100 &
                                                                                                       Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` >= input$Plot_East_Asian_North[1]/100 &
                                                                                                       Temp_CL_Harm_Data$`East_Asian_(North)_Ancestry` <= input$Plot_East_Asian_North[2]/100 &
                                                                                                       Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` >= input$Plot_East_Asian_South[1]/100 &
                                                                                                       Temp_CL_Harm_Data$`East_Asian_(South)_Ancestry` <= input$Plot_East_Asian_South[2]/100 &
                                                                                                       Temp_CL_Harm_Data$South_Asian_Ancestry >= input$Plot_South_Asian[1]/100 &
                                                                                                       Temp_CL_Harm_Data$South_Asian_Ancestry <= input$Plot_South_Asian[2]/100 &
                                                                                                       Temp_CL_Harm_Data$`European_(North)_Ancestry` >= input$Plot_European_North[1]/100 &
                                                                                                       Temp_CL_Harm_Data$`European_(North)_Ancestry` <= input$Plot_European_North[2]/100 &
                                                                                                       Temp_CL_Harm_Data$`European_(South)_Ancestry` >= input$Plot_European_South[1]/100 &
                                                                                                       Temp_CL_Harm_Data$`European_(South)_Ancestry` <= input$Plot_European_South[2]/100]]]))
                }
                flag <- 1
              }

              if(flag == 0){
                return(Avail_ccls)
              } else if(flag == 1){
                return(Filtered_ccls)
              }
            }) #END: Plot_Currently_Available_Cell_Lines <- reactive({

          #Updating filter menus
            observeEvent(Plot_Currently_Available_Cell_Lines(), {
              #Organizing data for available cell lines
                Temp_CL_Harm_Data <- Plot_Simple_Cell_Line_Harm_for_Compound()
                temp_Cell_Line_Diseases <- strsplit(Temp_CL_Harm_Data$Diseases, ":\\|:")
                  names(temp_Cell_Line_Diseases) <- Temp_CL_Harm_Data$Harmonized_Cell_Line_ID
              #Cancer type menu
                Cancer_Type_Options <- sort(unique(Temp_CL_Harm_Data$Simple_Cancer_Type))
                previous_selection <- input$Plot_Cancer_Type
                isolate({
                  updatePickerInput(session, "Plot_Cancer_Type", choices = Cancer_Type_Options, selected = previous_selection)
                })
              #Cancer type menu
                Disease_Name_Options <- sort(unique(unlist(temp_Cell_Line_Diseases)))
                previous_selection <- input$Plot_Disease_Name
                isolate({
                  updatePickerInput(session, "Plot_Disease_Name", choices = Disease_Name_Options, selected = previous_selection)
                })
              #Gender menu
                Gender_Options <- sort(unique(Temp_CL_Harm_Data$Gender))
                previous_selection <- input$Plot_Gender
                isolate({
                  updatePickerInput(session, "Plot_Gender", choices = Gender_Options, selected = previous_selection)
                })
              #Age slider: Decided not to update age slider because doing so removes cell lines with unknown age in years even if user has not manually changed the age
              #slider. This problem could be overcome, but it seems like more work than it is worth to make it work intuitively.
                # Age_Options <- c(min(Temp_CL_Harm_Data$Numeric_Age_in_Years, na.rm = TRUE), max(Temp_CL_Harm_Data$Numeric_Age_in_Years, na.rm = TRUE))
                # previous_selection <- input$Plot_Age
                # previous_selection <- c(max(Age_Options[1], previous_selection[1]), min(Age_Options[2], previous_selection[2]))
                # isolate({
                #   updateSliderInput(session, "Plot_Age", value = previous_selection)
                # })
            })

          #Updating cell line selection menu
            Plot_Selected_Cell_Line_Option_df <- eventReactive(Plot_Currently_Available_Cell_Lines(), {
              filtered_Cell_Line_Option_df <- Cell_Line_Option_df[Cell_Line_Option_df$Harmonized_Cell_Line_ID %in% Plot_Currently_Available_Cell_Lines(),]
              return(cbind(filtered_Cell_Line_Option_df, value = filtered_Cell_Line_Option_df$Harmonized_Cell_Line_ID))
            })

            observeEvent(Plot_Selected_Cell_Line_Option_df(),{
              prev_selection <- input$plot_cell_line
              if(! length(prev_selection) == 0){
                if(! prev_selection %in% Plot_Selected_Cell_Line_Option_df()$Harmonized_Cell_Line_ID){
                  prev_selection <- character(0)
                }
              }
              updateSelectizeInput(session, "plot_cell_line", label = paste0("Select a cell line (n = ", nrow(Plot_Selected_Cell_Line_Option_df()), ")"), choices = Plot_Selected_Cell_Line_Option_df(), server = TRUE, selected = prev_selection)
            })
          
        #Plotting selected compound and cell line
          observeEvent({input$plot_cell_line; input$plot_compound; input$Plot_Averages}, {
            req(length(input$plot_cell_line) > 0 & length(input$plot_compound) > 0 & length(input$Plot_Averages) > 0)
            isolate({
              #Rendering plot window
                output$Plot_Info_Panel <- renderUI({
                  if(! input$plot_cell_line == "" & ! input$plot_compound == ""){
                    list(
                      #Display dose-response curve plot
                        plotlyOutput(outputId = "Compound_CCL_Plot", height = "auto"),
                      #Display table of experiments available for this dose-response curve
                        DT::dataTableOutput(outputId = "Experiment_Table")
                    )
                  } else {
                    list(NULL)
                  }
                })
              
              if(! input$plot_cell_line == "" & ! input$plot_compound == ""){
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
              } #END: if(! input$plot_cell_line == "" & ! input$plot_compound == ""){
              
            }) #END: Isolate
          })
            
#############################################################          
#################################################### server #          
      #Code for "AUC Values" Tab
      
        #Generating AUC Interface
                isolate({output$AUC_Interface <- renderUI({
                  req(input$AUC_Dataset)
                  if(! input$AUC_Dataset == ""){
                    wellPanel(
                      fluidRow(
                        #First Column
                          column(width = 6,
                            wellPanel(
                              h4("Step 2 (optional): Generate template instruction file"),
                              #Generating compound selection menu
                                pickerInput("AUC_Compounds", label = "Select compounds to calculate AUC values for", choices = NULL, selected = NULL, multiple = TRUE, options = list(
                                  `actions-Box` = TRUE,
                                  `live-Search-Style` = "contains" ,
                                  `live-Search` = TRUE,
                                  `live-Search-Normalize` = TRUE,
                                  `selected-Text-Format` = "count"
                                )),
                              #Generating checkbox to toggle compound filter option display
                                checkboxInput("AUC_Show_Compound_Filters", label = "Show compound filters?", value = FALSE),
                              #Generating UI for compound filters
                                uiOutput("AUC_Compound_Filters"),
                              #Generating checkbox to toggle whether or not Csustained values should be used when available
                                checkboxInput("AUC_Use_Csustained", label = "Generate using Csustained when available?", value = TRUE) %>%
                                helper(type = "inline",
                                  title = "Using Csustained Concentrations",
                                  icon = "question-circle", colour = NULL,
                                  content = c("Selecting this option will cause the template instruction file to use Csustained concentrations for the Upper_Conc_Limit_uM column as long as the Csustained concentration is greater than the most common minimum concentration tested for that compound in the selected dataset. Csustained concentrations are the estimated maximum drug plasma concentrations in patients occurring at least 6 hours after drug administration, and are currently available for some, but not all, clinically advanced drugs. Details about these concentrations and how they were determined can be found by downloading the \"Csustained.xlsx\" table from the \"Download Bulk Data\" tab.", "", "If this option is not selected, the Upper_Conc_Limit_uM column will be populated with the most commonly used maximum concentration tested for each compound in the selected dataset."),
                                  size = "m",
                                  buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                ),
                              #Generating button to create and download template file
                                downloadButton(outputId = "AUC_Create_Template", label = "Download Template Instruction File") %>%
                                helper(type = "inline",
                                  title = "Create Template",
                                  icon = "question-circle", colour = NULL,
                                  content = c("Downloads a template Instruction file for the specified dataset and compounds for use in step 3. If at least one compound has been selected, the columns in the file will be as described below. Note that only the first three columns are necessary for the Instruction file, with the rest of the columns being provided for reference.", "",
                                              HTML("<b>Compound:</b>"), "Name of the compound for which normalized AUC values are to be calculated.", "",
                                              HTML("<b>Lower_Conc_Limit_uM:</b>"), "The lower concentration boundary for the integration area to be used when calculating AUC values (in microMolar). Defaults to Most_Commonly_Used_Min_Tested_Conc_uM.", "",
                                              HTML("<b>Upper_Conc_Limit_uM:</b>"), "The upper concentration boundary for the integration area to be used when calculating AUC values (in microMolar). Defaults to Most_Commonly_Used_Max_Tested_Conc_uM or to Csustained_uM if a Csustained concentration is available and the \"Generate using Csustained when available?\" checkbox is ticked.", "",
                                              HTML("<b>Min_Tested_Conc_uM:</b>"), "The minimum tested concentration (in microMolar) of this compound in the selected dataset in any cell line.", "",
                                              HTML("<b>Max_Tested_Conc_uM:</b>"), "The maximum tested concentration (in microMolar) of this compound in the selected dataset in any cell line.", "",
                                              HTML("<b>Most_Commonly_Used_Min_Tested_Conc_uM:</b>"), "The most commonly used minimum tested concentration (in microMolar) of this compound in the selected dataset.", "",
                                              HTML("<b>Most_Commonly_Used_Max_Tested_Conc_uM:</b>"), "The most commonly used maximum tested concentration (in microMolar) of this compound in the selected dataset.", "",
                                              HTML("<b>Csustained_uM:</b>"), "The maximum plasma concentation of this compound achieved at least 6 hours after drug administration in a patient at a clinically usable dose. These values were obtained from: Ling, A. & Huang, R. S. Computationally predicting clinical drug combination efficacy with cancer cell line screens and independent drug action. Nat. Commun. 11, 1–13 (2020). Details about these concentrations and how they were determined can be found by downloading the \"Csustained.xlsx\" table from the \"Download Bulk Data\" tab."),
                                  size = "m",
                                  buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                )
                            )
                          ),
                        #Second Column
                          column(width = 6,
                            wellPanel(
                              h4("Step 3: Upload instruction file"),
                              fileInput("AUC_Instruction", label = "Upload Instruction file", accept = ".xlsx") %>%
                                helper(type = "inline",
                                  title = "Upload Instruction File",
                                  icon = "question-circle", colour = NULL,
                                  content = c("Upload an instruction file in .xlsx format which specifies the compounds and concentration ranges to calculate AUC values for. The following columns are required, with additional columns being ignored.", "",
                                              HTML("<b>Compound:</b>"), "Name of the compound for which normalized AUC values are to be calculated.", "",
                                              HTML("<b>Lower_Conc_Limit_uM:</b>"), "The lower concentration boundary for the integration area to be used when calculating AUC values (in microMolar). Note that these values will be rounded to 3 significant figures.", "",
                                              HTML("<b>Upper_Conc_Limit_uM:</b>"), "The upper concentration boundary for the integration area to be used when calculating AUC values (in microMolar). Note that these values will be rounded to 3 significant figures."),
                                  size = "m",
                                  buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                ),
                              uiOutput("AUC_Error"),
                              #Generating UI for cell line selection
                                uiOutput("AUC_Cell_Line_Menu")
                            )
                          )
                      )
                    )
                  } else {
                    list(p(""))
                  }
                })})
            
              #Processing values that depend on AUC_Dataset
                
                #Determining which compounds are available for selected dataset
                  AUC_Available_Compounds <- reactive({
                    req(input$AUC_Dataset)
                    if(! input$AUC_Dataset == ""){
                      names(compound_ccl_availability_successful)[sapply(compound_ccl_availability_successful, function(x){return(length(x[[input$AUC_Dataset]]) > 0)})]
                    } else {
                      NULL
                    }
                  })
                  
                  AUC_Simple_Compound_Harm_for_Dataset <- reactive({
                    req(input$AUC_Dataset)
                    if(! input$AUC_Dataset == ""){
                      Simple_Compound_Harm[Simple_Compound_Harm$Harmonized_Compound_Name %in% AUC_Available_Compounds(),]
                    } else {
                      NULL
                    }
                  })
                  
                #Getting molecular targets available for compounds available for selected cell line
                  AUC_Available_Molecular_Targets <- reactive({
                    req(AUC_Simple_Compound_Harm_for_Dataset())
                    temp_Compound_Molecular_Targets <- strsplit(AUC_Simple_Compound_Harm_for_Dataset()$Compound_Molecular_Targets, ":\\|:")
                    return(sort(unique(unlist(temp_Compound_Molecular_Targets))))
                  })
                
                #Getting mechanisms of action available for compounds available for selected cell line
                  AUC_Available_MOAs <- reactive({
                    req(AUC_Simple_Compound_Harm_for_Dataset())
                    temp_Compound_MOAs <- strsplit(AUC_Simple_Compound_Harm_for_Dataset()$Compound_MOA, ":\\|:")
                    return(sort(unique(unlist(temp_Compound_MOAs))))
                  })
                  
              #Defining compound filter interface
                isolate({output$AUC_Compound_Filters <- renderUI({
                  req(input$AUC_Show_Compound_Filters)
                  req(AUC_Available_Molecular_Targets())
                  req(AUC_Available_MOAs())
                  req(AUC_Simple_Compound_Harm_for_Dataset())
                  
                  if(input$AUC_Show_Compound_Filters == TRUE){
                    return(list(
                      h4("Filter compounds by:") %>%
                          helper(type = "inline",
                            title = "Compound filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("These options can be used to filter the compound options displayed in the \"Select compounds to calculate AUC values for\" menu. Note that selecting any options from a filter menu will omit all compounds which lack annotated information for that menu's filtering criteria."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                      wellPanel(list(
                        pickerInput(inputId = "AUC_Molecular_Target", label = "Filter compounds by molecular target", choices = AUC_Available_Molecular_Targets(), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                        helper(type = "inline",
                          title = "Molecular Target Filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Limits compounds displayed in the \"Select compounds to calculate AUC values for\" menu to compounds which target at least one of the selected molecular targets. Note that, if any selections have been made in the \"Filter compounds by free-text MOA\" menu, displayed compounds will also include compounds which have at least one of the selected MOAs."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
                        pickerInput(inputId = "AUC_MOA", label = "Filter compounds by free-text MOA", choices = AUC_Available_MOAs(), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                        helper(type = "inline",
                          title = "Mechanism of Action (MOA) Filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Limits compounds displayed in the \"Select compounds to calculate AUC values for\" menu to compounds with at least one of the selected MOAs. Note that, if any selections have been made in the \"Filter compounds by molecular target\" menu, displayed compounds will also include compounds which target at least one of the selected molecular targets."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
                        pickerInput(inputId = "AUC_Clinical_Phase", label = "Filter compounds by clinical phase", choices = sort(unique(AUC_Simple_Compound_Harm_for_Dataset()$Compound_Clinical_Phase)), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                        helper(type = "inline",
                          title = "Clinical Phase Filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Limits compounds displayed in the \"Select compounds to calculate AUC values for\" menu to compounds whose annotated highest reached clinical phase is one of the selected phases."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        )
                      ))
                    ))
                  } else {
                    return(list(p("")))
                  }
                })})
                
              #Defining currently available compounds based on compound filters (meets any filter)
                AUC_Currently_Available_Compounds <- reactive({
                  req(AUC_Simple_Compound_Harm_for_Dataset())
  
                  Avail_compounds <- AUC_Simple_Compound_Harm_for_Dataset()$Harmonized_Compound_Name
                  Filtered_compounds <- character(0)
                  flag <- 0
                  
                  
                  if(! is.null(input$AUC_Molecular_Target)){
                    flag <- 1
                    Filtered_compounds <- Avail_compounds[Avail_compounds %in% names(Compound_Molecular_Targets)[sapply(Compound_Molecular_Targets, function(x,y){any(y %in% x)}, y = input$AUC_Molecular_Target)]]
                  }
                  
                  if(! is.null(input$AUC_MOA)){
                    if(flag == 0){
                      Filtered_compounds <- Avail_compounds[Avail_compounds %in% names(Compound_MOAs)[sapply(Compound_MOAs, function(x,y){any(y %in% x)}, y = input$AUC_MOA)]]
                    } else if(flag == 1){
                      Filtered_compounds <- c(Filtered_compounds, Avail_compounds[Avail_compounds %in% names(Compound_MOAs)[sapply(Compound_MOAs, function(x,y){any(y %in% x)}, y = input$AUC_MOA)]])
                    }
                    flag <- 1
                  }
                  
                  if(! is.null(input$AUC_Clinical_Phase)){
                    if(flag == 0){
                      Filtered_compounds <- Avail_compounds[Avail_compounds %in% Simple_Compound_Harm$Harmonized_Compound_Name[Simple_Compound_Harm$Compound_Clinical_Phase %in% input$AUC_Clinical_Phase]]
                    } else if(flag == 1){
                      Filtered_compounds <- Filtered_compounds[Filtered_compounds %in% Simple_Compound_Harm$Harmonized_Compound_Name[Simple_Compound_Harm$Compound_Clinical_Phase %in% input$AUC_Clinical_Phase]]
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
                observeEvent(AUC_Currently_Available_Compounds(), {
                  isolate({
                    updatePickerInput(session, "AUC_Compounds", label = paste0("Select compounds to calculate AUC values for (n = ", length(AUC_Currently_Available_Compounds()), ")"), choices = AUC_Currently_Available_Compounds(), selected = AUC_Currently_Available_Compounds())
                  })
                })
                
              #Generating Instruction template File
                AUC_Template <- reactive({
                  req(input$AUC_Dataset)
                  req(input$AUC_Compounds)
                  req(! length(input$AUC_Use_Csustained) == 0)
                  
                  if(length(input$AUC_Compounds) > 0){
                    temp_data <- Dataset_Tested_Concentrations[Dataset_Tested_Concentrations$Compound %in% input$AUC_Compounds, c("Compound", paste0("min_mode_ccl_", input$AUC_Dataset, "_conc"), paste0("max_mode_ccl_", input$AUC_Dataset, "_conc"), paste0("min_", input$AUC_Dataset, "_conc"), paste0("max_", input$AUC_Dataset, "_conc"), paste0("min_mode_ccl_", input$AUC_Dataset, "_conc"), paste0("max_mode_ccl_", input$AUC_Dataset, "_conc"))]
                    colnames(temp_data) <- c("Compound", "Lower_Conc_Limit_uM", "Upper_Conc_Limit_uM", "Min_Tested_Conc_uM", "Max_Tested_Conc_uM", "Most_Commonly_Used_Min_Tested_Conc_uM", "Most_Commonly_Used_Max_Tested_Conc_uM")
                    temp_data$Csustained_uM <- NA
                    temp_data$Csustained_uM <- signif(as.numeric(Csustained$`Csustained (uM)`[match(temp_data$Compound, Csustained$Compound)]), 3)
  
                    if(input$AUC_Use_Csustained == TRUE){
                      temp_data$Upper_Conc_Limit_uM[! is.na(temp_data$Csustained_uM) & ! (temp_data$Csustained_uM <=  temp_data$Lower_Conc_Limit_uM)] <- temp_data$Csustained_uM[! is.na(temp_data$Csustained_uM) & ! (temp_data$Csustained_uM <=  temp_data$Lower_Conc_Limit_uM)]
                    }
  
                    return(temp_data)
                  } else {
                    temp_colnames <- c("Compound", "Lower_Conc_Limit_uM", "Upper_Conc_Limit_uM")
                    temp_data <- as.data.frame(matrix(NA, nrow = 1, ncol = length(temp_colnames)))
                    colnames(temp_data) <- temp_colnames
                    return(temp_data)
                  }
                })
  
              #Allowing user to download Instruction template file
                observeEvent(input$AUC_Dataset, {
                  output$AUC_Create_Template <- downloadHandler(
                    filename = paste0(input$AUC_Dataset, "_AUC_Instruction_File_Template.xlsx"),
                    content = function(file){
                      write.xlsx(AUC_Template(), file, row.names = FALSE)
                    }
                  )
                })
                
              #Reading AUC instruction file
                AUC_Instruction <- eventReactive(input$AUC_Instruction, valueExpr = {
                  req(input$AUC_Dataset)
                  req(input$AUC_Instruction)
                  
                  #Determining which compounds are available for selected dataset (repeating because it gets saved in wrong environment in previous observeEvent block)
                    temp_available_compounds <- names(compound_ccl_availability_successful)[sapply(compound_ccl_availability_successful, function(x){return(length(x[[input$AUC_Dataset]]) > 0)})]
                  #Loading file and making sure it is correctly formatted
                    #Checking file extension
                      file <- input$AUC_Instruction
                      req(file)
                      ext <- tools::file_ext(file$datapath)
                    #Printing error message if file extension is incorrect
                      if(! ext == "xlsx"){
                          output$AUC_Error <- renderUI({
                            p(HTML("<b>Error: The uploaded file must be in xlsx format.</b>"), style = "color:red")
                          })
                          AUC_Instruction <- return("")
                      } else {
                        #Loading data
                          data <- try(as.data.frame(read_xlsx(file$datapath)))
                        #Printing error message if file could not be loaded
                          if(class(data) == "try-error"){
                            output$AUC_Error <- renderUI({
                              p(HTML("<b>Error: The selected file could not be loaded. Is it really an xlsx file?</b>"), style = "color:red")
                            })
                            AUC_Instruction <- return("")
                          } else {
                            #Checking that required columns are present and data has >0 rows
                              required_columns <- c("Compound", "Lower_Conc_Limit_uM", "Upper_Conc_Limit_uM")
                              if(! all(required_columns %in% colnames(data))){
                                output$AUC_Error <- renderUI({
                                  p(HTML("<b>Error: At least one required column is missing in uploaded file (Compound, Lower_Conc_Limit_uM, Upper_Conc_Limit_uM). You can download an Instruction file template in Step 2 to see the required file format.</b>"), style = "color:red")
                                })
                                AUC_Instruction <- return("")
                              } else if(! nrow(data) > 0){
                                output$AUC_Error <- renderUI({
                                  p(HTML("<b>Error: Uploaded file has zero rows of data.</b>"), style = "color:red")
                                })
                                AUC_Instruction <- return("")
                              } else {
                                #Organizing data to have only required columns and making sure concentration columns are numeric
                                  data <- data[,required_columns]
                                  data$Lower_Conc_Limit_uM <- as.numeric(data$Lower_Conc_Limit_uM)
                                  data$Upper_Conc_Limit_uM <- as.numeric(data$Upper_Conc_Limit_uM)
                                #Making vector of any drugs that are in the Instruction file but not in the selected dataset
                                  mismatched_drugs <- sort(unique(data$Compound[! data$Compound %in% temp_available_compounds]))
                                #Doing error handling for Instruction file upload
                                  #Checking that all listed compounds are present in the selected dataset
                                  if(length(mismatched_drugs) != 0){
                                    output$AUC_Error <- renderUI({list(
                                      p(HTML("<b>Error: Uploaded instruction file contains the following compounds that do not exist in selected dataset. Was it generated using this dataset?</b>"), style = "color:red"),
                                      p(paste(mismatched_drugs, collapse = "; "), style = "color:red")
                                    )})
                                    AUC_Instruction <- return("")
                                  #Checking that the Lower_Conc_Limit_uM column has no missing values
                                  } else if(any(is.na(data$Lower_Conc_Limit_uM))){
                                    output$AUC_Error <- renderUI({
                                      p(HTML("<b>Error: Missing or non-numeric values exist in Lower_Conc_Limit_uM column.</b>"), style = "color:red")
                                    })
                                    AUC_Instruction <- return("")
                                  #Checking that the Upper_Conc_Limit_uM column has no missing values
                                  } else if(any(is.na(data$Upper_Conc_Limit_uM))){
                                    output$AUC_Error <- renderUI({
                                      p(HTML("<b>Error: Missing or non-numeric values exist in Upper_Conc_Limit_uM column.</b>"), style = "color:red")
                                    })
                                    AUC_Instruction <- return("")
                                  #Checking that lower concentration limits are > 0
                                  } else if(! all(data$Lower_Conc_Limit_uM > 0)){
                                    output$AUC_Error <- renderUI({
                                      p(HTML("<b>Error: Lower_Conc_Limit_uM column contains values that are less than or equal to 0.</b>"), style = "color:red")
                                    })
                                    AUC_Instruction <- return("")
                                  #Checking that the upper conc limit is > the lower conc limit
                                  } else if(! all(data$Upper_Conc_Limit_uM > data$Lower_Conc_Limit_uM)){
                                    output$AUC_Error <- renderUI({
                                      p(HTML("<b>Error: Upper_Conc_Limit_uM column contains values that are less than or equal to the corresponding values in the Lower_Conc_Limit_uM column.</b>"), style = "color:red")
                                    })
                                    AUC_Instruction <- return("")
                                  #Defining AUC_Instruction() if uploaded Instruction file has passed all checks
                                  } else {
                                    output$AUC_Error <- renderUI({})
                                    AUC_Instruction <- return(data)
                                  }
                              }
                          }
                      }
                })
                
              #Initializing Cell Line Selection Menu if Instruction file upload complete and validated
                #Rendering cell line selection menu UI
                  isolate({output$AUC_Cell_Line_Menu <- renderUI({
                    req(input$AUC_Dataset)
                    if(is.data.frame(AUC_Instruction())){
                      return_list <- list(
                        h4("Step 4: Cell line selection"),
                        #Cell line selection menu
                          pickerInput("AUC_Cell_Lines", label = "Select cell lines to calculate normalized AUC values for", choices = NULL, selected = NULL, multiple = TRUE, options = list(
                            `actions-Box` = TRUE,
                            `live-Search-Style` = "contains" ,
                            `live-Search` = TRUE,
                            `live-Search-Normalize` = TRUE,
                            `selected-Text-Format` = "count"
                          )),
                        #Checkbox to show or hide cell line filters
                          checkboxInput("AUC_Show_Cell_Line_Filters", label = "Show cell line filters?", value = FALSE),
                        #Cell line filters
                          uiOutput("AUC_Cell_Line_Filters"),
                        #Checkbox to toggle whether or not output should be formatted for IDACombo shiny app
                          # checkboxInput("AUC_Format_For_IDACombo", label = "Format output for use with IDACombo shiny app?", value = FALSE) %>%
                          #         helper(type = "inline",
                          #           title = "Formatting for IDACombo shiny app",
                          #           icon = "question-circle", colour = NULL,
                          #           content = HTML("Selecting this option will format the output file so that it can be directly used as a custom input dataset for the IDACombo shiny app, an app developed by our group to use monotherapy drug screening data to predict drug combination efficacy. Note that we have not yet validated the use of AUC values with the IDACombo app, and suggest using viability values at clinical concentrations instead. The IDACombo app can be found at <a href=\"https://huanglab.shinyapps.io/idacombo-shiny-app/\">https://huanglab.shinyapps.io/idacombo-shiny-app/</a>"),
                          #           size = "m",
                          #           buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          #         ),
                        #Button to do AUC calculation
                          h4("Step 5: Calculate AUCs"),
                          actionButton(inputId = "AUC_Calc", label = "Calculate AUC Values"),
                        #Button to generate and download AUC values
                          uiOutput("AUC_Download_UI")
                      )
                    } else {
                      return_list <- list(p(""))
                    }
                    
                    return(return_list)
                  })})
                  
                #Determining which compounds have been selected
                  AUC_Instruction_Compounds <- reactive({
                    req(AUC_Instruction())
                    sort(unique(AUC_Instruction()$Compound))
                  })
                  
                #Determining which cell lines are available for the specified compounds in the selected dataset
                  AUC_Available_Cell_Lines <- reactive({
                    req(input$AUC_Dataset)
                    req(AUC_Instruction_Compounds())
                      sort(unique(unlist(lapply(compound_ccl_availability_successful[names(compound_ccl_availability_successful) %in% AUC_Instruction_Compounds()], function(x){return(x[input$AUC_Dataset])}))))
                  })
                
                #Subsetting cell line info to cell lines available for specified compounds and dataset
                  AUC_Simple_Cell_Line_Harm <- reactive({
                    req(AUC_Available_Cell_Lines())
                    Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% AUC_Available_Cell_Lines(),]
                  })
                  
                #Getting age limits
                  AUC_Age_Limits <- reactive({
                    req(AUC_Simple_Cell_Line_Harm())
                    
                    temp_min <- suppressWarnings(min(AUC_Simple_Cell_Line_Harm()$Numeric_Age_in_Years, na.rm = TRUE))
                    if(temp_min == Inf){
                      temp_min <- 0
                    }
                    temp_max <- suppressWarnings(max(AUC_Simple_Cell_Line_Harm()$Numeric_Age_in_Years, na.rm = TRUE))
                    if(temp_max == -Inf){
                      temp_max <- 0
                    }
                    return(c(temp_min, temp_max))
                  })
                  
                #Getting disease information
                  AUC_Cell_Line_Diseases <- reactive({
                    req(AUC_Simple_Cell_Line_Harm())
                    
                    temp_Cell_Line_Diseases <- strsplit(AUC_Simple_Cell_Line_Harm()$Diseases, ":\\|:")
                    names(temp_Cell_Line_Diseases) <- AUC_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID
                    return(temp_Cell_Line_Diseases)
                  })
                  
                #Defining cell line filter interface
                  isolate({output$AUC_Cell_Line_Filters <- renderUI({
                    req(! length(input$AUC_Show_Cell_Line_Filters) == 0)
                    req(AUC_Simple_Cell_Line_Harm())
                    req(AUC_Cell_Line_Diseases())
                    
                    if(input$AUC_Show_Cell_Line_Filters == TRUE){
                      return_list <- list(
                        h4("Filter cell lines by:") %>%
                        helper(type = "inline",
                          title = "Cell line filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("These options can be used to filter the cell line options displayed in the \"Select cell lines to calculate AUC values for\" menu. Note that, once any options have been selected for a given filtering criteria, any cell lines that are missing information for that criteria will be excluded."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
                        wellPanel(list(
                          pickerInput(inputId = "AUC_Cancer_Type", label = "General cancer type", choices = sort(unique(AUC_Simple_Cell_Line_Harm()$Simple_Cancer_Type)), multiple = TRUE, options = list(`actions-Box` = TRUE, `live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                          helper(type = "inline",
                            title = "General cancer type filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Selecting any cancer type options will limit displayed cell line options to only include cell lines from the selected cancer types. If any options have been selected from the \"Free-text disease name\" menu, cell lines will also be displayed that meet at least one of the disease name criteria selected in that menu."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                          pickerInput(inputId = "AUC_Disease_Name", label = "Free-text disease name", choices = sort(unique(unlist(AUC_Cell_Line_Diseases()))), multiple = TRUE, options = list(`actions-Box` = TRUE, `live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                          helper(type = "inline",
                            title = "Free-text disease name filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Selecting any disease name options will limit displayed cell line options to only include cell lines from the selected diseases. If any options have been selected from the \"General cancer type\" menu, cell lines will also be displayed that meet at least one of the cancer type criteria selected in that menu."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                          pickerInput(inputId = "AUC_Gender", label = "Gender", choices = sort(unique(AUC_Simple_Cell_Line_Harm()$Gender)), multiple = TRUE, options = list(`selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                          helper(type = "inline",
                            title = "Gender filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Selecting any gender options will limit the displayed cell line options to only include cell lines of the selected gender(s)."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
  
                          sliderInput(inputId = "AUC_Age", label = "Patient age (years)", min = AUC_Age_Limits()[1], max = AUC_Age_Limits()[2], value = AUC_Age_Limits(), ticks = FALSE) %>%
                          helper(type = "inline",
                            title = "Patient age filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Sets minimum and maximum age allowed for displayed cell line options. Note that setting the slider to anything other than its maximum range will exclude cell lines for which a numeric age could not be determined for the patient who the cell line was derived from at the time of sample collection. This both includes cases where the age is unspecified and cases where the specified age is ambiguous (i.e. such as \"Adult\"). You may download the cell line harmonization file in the \"Download Bulk Data\" tab for free-text descriptions of each cell line's patient age."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
  
                          h4("Ancestry") %>%
                          helper(type = "inline",
                            title = "Patient age filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Each slider sets the minimum and maximum % ancestry for each ancestry group. Once any of the ancestry sliders has been changed from its maximum range, cell lines will be filtered to only include lines which meet the limits set on all of the ancestry sliders. Note that % ancestry adds to 100% across all ancestry groups for each individual cell line (i.e. a cell line cannot have 60% African ancestry and 60% Native American ancestry, because that would add to >100%). Ancestry information was obtained from the cellosaurus resource at <a href=\"https://www.expasy.org/\">https://www.expasy.org/</a>."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                          wellPanel(list(
                            sliderInput(inputId = "AUC_African", label = "% african ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "AUC_Native_American", label = "% native american ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "AUC_East_Asian_North", label = "% east asian (north) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "AUC_East_Asian_South", label = "% east asian (south) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "AUC_South_Asian", label = "% south asian ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "AUC_European_North", label = "% european (north) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "AUC_European_South", label = "% european (south) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE)
                          ))
                        ))
                      )
                      return(return_list)
                    } else {
                      return(list(" "))
                    }
                  })})
                  
                #Updating cell line filter interface
                  #Defining currently available cell lines based on cell line filters
                    AUC_Currently_Available_Cell_Lines <- reactive({
                      req(AUC_Available_Cell_Lines())
                      req(AUC_Simple_Cell_Line_Harm())
                      req(AUC_Cell_Line_Diseases())
  
  
                      Avail_ccls <- AUC_Available_Cell_Lines()
                      Filtered_ccls <- character(0)
                      flag <- 0
  
                      if(! is.null(input$AUC_Cancer_Type)){
                        flag <- 1
                        Filtered_ccls <- Avail_ccls[Avail_ccls %in% AUC_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[AUC_Simple_Cell_Line_Harm()$Simple_Cancer_Type %in% input$AUC_Cancer_Type]]
                      }
  
                      if(! is.null(input$AUC_Disease_Name)){
                        flag <- 1
                        Filtered_ccls <- sort(unique(c(Filtered_ccls, Avail_ccls[Avail_ccls %in% names(AUC_Cell_Line_Diseases())[sapply(AUC_Cell_Line_Diseases(), function(x,y){return(any(y %in% x))}, y = input$AUC_Disease_Name)]])))
                      }
  
                      if(! is.null(input$AUC_Gender)){
                        if(flag == 0){
                          Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% AUC_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[AUC_Simple_Cell_Line_Harm()$Gender %in% input$AUC_Gender]]))
                        } else if(flag == 1){
                          Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% AUC_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[AUC_Simple_Cell_Line_Harm()$Gender %in% input$AUC_Gender]]]))
                        }
                        flag <- 1
                      }
  
                      if(! all(input$AUC_Age == AUC_Age_Limits())){
                        if(flag == 0){
                          Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% AUC_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[AUC_Simple_Cell_Line_Harm()$Numeric_Age_in_Years >= input$AUC_Age[1] &
                                                                                                               AUC_Simple_Cell_Line_Harm()$Numeric_Age_in_Years <= input$AUC_Age[2]]]))
                        } else if(flag == 1){
                          Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% AUC_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[AUC_Simple_Cell_Line_Harm()$Numeric_Age_in_Years >= input$AUC_Age[1] &
                                                                                                                                                            AUC_Simple_Cell_Line_Harm()$Numeric_Age_in_Years <= input$AUC_Age[2]]]]))
                        }
                        flag <- 1
                      }
  
                      if(! all(input$AUC_African == c(0,100)) |
                         ! all(input$AUC_Native_American == c(0,100)) |
                         ! all(input$AUC_East_Asian_North == c(0,100)) |
                         ! all(input$AUC_East_Asian_South == c(0,100)) |
                         ! all(input$AUC_South_Asian == c(0,100)) |
                         ! all(input$AUC_European_North == c(0,100)) |
                         ! all(input$AUC_European_South == c(0,100))){
                        if(flag == 0){
                          Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% AUC_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[AUC_Simple_Cell_Line_Harm()$African_Ancestry >= input$AUC_African[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$African_Ancestry <= input$AUC_African[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$Native_American_Ancestry >= input$AUC_Native_American[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$Native_American_Ancestry <= input$AUC_Native_American[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`East_Asian_(North)_Ancestry` >= input$AUC_East_Asian_North[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`East_Asian_(North)_Ancestry` <= input$AUC_East_Asian_North[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`East_Asian_(South)_Ancestry` >= input$AUC_East_Asian_South[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`East_Asian_(South)_Ancestry` <= input$AUC_East_Asian_South[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$South_Asian_Ancestry >= input$AUC_South_Asian[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$South_Asian_Ancestry <= input$AUC_South_Asian[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`European_(North)_Ancestry` >= input$AUC_European_North[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`European_(North)_Ancestry` <= input$AUC_European_North[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`European_(South)_Ancestry` >= input$AUC_European_South[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`European_(South)_Ancestry` <= input$AUC_European_South[2]/100]]))
                        } else if(flag == 1){
                          Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% AUC_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[AUC_Simple_Cell_Line_Harm()$African_Ancestry >= input$AUC_African[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$African_Ancestry <= input$AUC_African[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$Native_American_Ancestry >= input$AUC_Native_American[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$Native_American_Ancestry <= input$AUC_Native_American[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`East_Asian_(North)_Ancestry` >= input$AUC_East_Asian_North[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`East_Asian_(North)_Ancestry` <= input$AUC_East_Asian_North[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`East_Asian_(South)_Ancestry` >= input$AUC_East_Asian_South[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`East_Asian_(South)_Ancestry` <= input$AUC_East_Asian_South[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$South_Asian_Ancestry >= input$AUC_South_Asian[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$South_Asian_Ancestry <= input$AUC_South_Asian[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`European_(North)_Ancestry` >= input$AUC_European_North[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`European_(North)_Ancestry` <= input$AUC_European_North[2]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`European_(South)_Ancestry` >= input$AUC_European_South[1]/100 &
                                                                                                               AUC_Simple_Cell_Line_Harm()$`European_(South)_Ancestry` <= input$AUC_European_South[2]/100]]]))
                        }
                        flag <- 1
                      }
  
                      if(flag == 0){
                        return(Avail_ccls)
                      } else if(flag == 1){
                        return(Filtered_ccls)
                      }
                    })
  
                  #Updating selection menus based on AUC_Currently_Available_Cell_Lines()
                    observeEvent(AUC_Currently_Available_Cell_Lines(), {
                      #Updating cell line selection menu
                      isolate({
                        updatePickerInput(session, "AUC_Cell_Lines", label = paste0("Select cell lines to calculate AUC values for (n = ", length(AUC_Currently_Available_Cell_Lines()), ")"), choices = AUC_Currently_Available_Cell_Lines(), selected = AUC_Currently_Available_Cell_Lines())
                      })
                    })
  
              #Generating AUC Values
                AUC_Custom_Values <- eventReactive(input$AUC_Calc, ignoreInit = TRUE, valueExpr =  {
                    req(input$AUC_Calc)
                  isolate({
                    #Preloading data for coding ease
                      temp_Instruction <- AUC_Instruction()
                      temp_Cell_Lines <- input$AUC_Cell_Lines
                      temp_dataset <- input$AUC_Dataset
                      wd <- getwd()
                    #Loading fitted curves and calculating AUC values
                      show_modal_spinner(
                                spin = "atom",
                                color = "#112446",
                                text = "Calculating AUCs..."
                              )
                      progress <- AsyncProgress$new(session, min = 0, max = nrow(temp_Instruction), message = "Initializing Calculation...")
                      future_data <- future(
                        global = c("progress", "temp_Instruction", "temp_Cell_Lines", "temp_dataset", "wd", "Compound_Filenames", "AUC", "ll.4.AUC"),
                        packages = c("drc"),
                        expr = {
                      Calculated_AUCs <- vector(mode = "list", length = 0)
                        for(i in 1:nrow(temp_Instruction)){
                          if(i == 1){
                            progress$set(value = 0, message = paste0(0, " of ", nrow(temp_Instruction), " Rows Complete..."))
                          }
                          #Loading results for this cell line
                            filename <- Compound_Filenames$drug_file_names[Compound_Filenames$drugs %in% temp_Instruction$Compound[i]]
                            temp_results <- readRDS(paste0("./www/Results/", filename, ".rds"))[[temp_dataset]]
                          #Subsetting to selected cell lines
                            temp_results <- temp_results[temp_results$Cell_Line %in% temp_Cell_Lines & ! is.na(temp_results$b_c_d_e),]
                          if(nrow(temp_results) > 0){
                            #Calculating AUC Values
                              temp_AUCs <- AUC(temp_results$b_c_d_e, lower = temp_Instruction$Lower_Conc_Limit_uM[i], upper = temp_Instruction$Upper_Conc_Limit_uM[i])
                            #Constructing return values
                              temp_Return <- data.frame("Compound" = temp_results$Compound,
                                                        "Cell_Line" = temp_results$Cell_Line,
                                                        "normalized_AUC" = temp_AUCs,
                                                        "Lower_Conc_Limit_uM" = temp_Instruction$Lower_Conc_Limit_uM[i],
                                                        "Upper_Conc_Limit_uM" = temp_Instruction$Upper_Conc_Limit_uM[i],
                                                        "Min_Tested_Conc_uM" = temp_results$min_dose_uM,
                                                        "Max_Tested_Conc_uM" = temp_results$max_dose_uM,
                                                        stringsAsFactors = FALSE
                                                        )
                            #Storing result
                              Calculated_AUCs[[i]] <- temp_Return
                          } else {
                            #Constructing empty result data frame
                              temp_Return <- data.frame("Compound" = temp_results$Compound,
                                                          "Cell_Line" = character(0),
                                                          "normalized_AUC" = numeric(0),
                                                          "Lower_Conc_Limit_uM" = numeric(0),
                                                          "Upper_Conc_Limit_uM" = numeric(0),
                                                          "Min_Tested_Conc_uM" = numeric(0),
                                                          "Max_Tested_Conc_uM" = numeric(0),
                                                        )
                            #Storing empty data frame
                              Calculated_AUCs[[i]] <- temp_Return
                          }
                          progress$set(value = i, message = paste0(i, " of ", nrow(temp_Instruction), " Rows Complete..."))
                        }
                    #Organizing calculated AUC values
                      Returnable_AUCs <- as.data.frame(do.call(rbind, Calculated_AUCs))
                      progress$close()
                      return(Returnable_AUCs)
                    }) #END: future
                  }) #END: isolate
                  promise_race(future_data) %...>% {remove_modal_spinner()}
                  future_data
                })
                
              #Allowing user to download results
                observeEvent(AUC_Custom_Values(), {
                  req(AUC_Custom_Values())
                  req(input$AUC_Dataset)
                  promise_all(data = AUC_Custom_Values()) %...>% with({
                    isolate({
                    #Creating Download Button
                      output$AUC_Download_UI <- renderUI({
                        if(is.data.frame(data)){
                          downloadButton(outputId = "AUC_Download_AUC_Values", label = "Download Calculated AUC Values") %>%
                                  helper(type = "inline",
                                    title = "Download AUC Values",
                                    icon = "question-circle", colour = NULL,
                                    content = HTML("Pressing this button will download a tab separated value (.tsv) text file containing the calculated normalized AUC values, where each row reports a calculated AUC value for a single set of compound, cell line, and concentration range. This file will have the following columns:
                                                   <ol>
                                                    <li><b>Compound:</b> Compound name.</li>
                                                    <li><b>Cell_Line:</b> Cell line name.</li>
                                                    <li><b>normalized_AUC:</b> The area under the curve (AUC) for this compound-cell line pair from the Lower_Conc_Limit_uM to the Upper_Conc_Limit_uM, normalized by dividing the AUC by the concentration range used to calculate it. As such the normalized AUC value will range from ~0 to ~1, with a normalized AUC of 1 indicating 100% viability across the entire concentration range and a normalized AUC of 0 indicating 0% viability across the entire concentration range.</li>
                                                    <li><b>Lower_Conc_Limit_uM:</b> The lower concentration boundary, in microMolar, from which to calculate AUC values.</li>
                                                    <li><b>Upper_Conc_Limit_uM:</b> The upper concentration boundary, in microMolar, to which to calculate AUC values.</li>
                                                    <li><b>Min_Tested_Conc_uM:</b> The minimum tested concentration, in microMolar, for this compound with this cell line in this dataset.</li>
                                                    <li><b>Max_Tested_Conc_uM:</b> The maximum tested concentration, in microMolar, for this compound with this cell line in this dataset.</li>
                                                  </ol>"),
                                    size = "m",
                                    buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                  )
                        } else {
                          
                        }
                      })
                      
                    #Allowing user to download AUC_Custom_Values()
                      output$AUC_Download_AUC_Values <- downloadHandler(
                            filename = paste0(input$AUC_Dataset, "_Calculated_AUCs.tsv"),
                            content = function(file){
                              
                              show_modal_spinner(
                                spin = "atom",
                                color = "#112446",
                                text = "Preparing Results for Download..."
                              )
                              
                              success.check <- try(write_delim(data, file, delim = "\t"))
                              
                              if(class(success.check) == "try-error"){
                                warning("Download unsuccessful: ", Sys.time())
                                temp_class <- class(data)
                                warning("File class = ", temp_class)
                                if(temp_class == "data.frame"){
                                  temp_type <- NA
                                  for(i in 1:ncol(data)){
                                    temp_type[i] <- typeof(data[,i])
                                  }
                                  warning(paste0("Column Types: ", paste(temp_type, collapse = ", ")))
                                }
                              }
                              
                              remove_modal_spinner()
                            }
                          )
                    }) #END: Isolate
                  }) #END: promise_all(data = AUC_Custom_Values()) %...>% with({
                }) #END: observeEvent(AUC_Custom_Values(), {

          
#############################################################          
#################################################### server #          
      #Viability Value Tab
      
        #Generating Viability Interface
                isolate({output$Viability_Interface <- renderUI({
                  req(input$Viability_Dataset)
                  if(! input$Viability_Dataset == ""){
                    wellPanel(
                      fluidRow(
                        #First Column
                          column(width = 6,
                            wellPanel(
                              h4("Step 2 (optional): Generate template instruction file"),
                              #Generating compound selection menu
                                pickerInput("Viability_Compounds", label = "Select compounds to calculate viability values for", choices = NULL, selected = NULL, multiple = TRUE, options = list(
                                  `actions-Box` = TRUE,
                                  `live-Search-Style` = "contains" ,
                                  `live-Search` = TRUE,
                                  `live-Search-Normalize` = TRUE,
                                  `selected-Text-Format` = "count"
                                )),
                              #Generating checkbox to toggle compound filter option display
                                checkboxInput("Viability_Show_Compound_Filters", label = "Show compound filters?", value = FALSE),
                              #Generating UI for compound filters
                                uiOutput("Viability_Compound_Filters"),
                              #Generating checkbox to toggle whether or not Csustained values should be used when available
                                checkboxInput("Viability_Use_Csustained", label = "Generate using Csustained when available?", value = TRUE) %>%
                                helper(type = "inline",
                                  title = "Using Csustained Concentrations",
                                  icon = "question-circle", colour = NULL,
                                  content = c("Selecting this option will cause the template instruction file to use Csustained concentrations for the Upper_Conc_Limit_uM column whenever Csustained is available. Csustained concentrations are the estimated maximum drug plasma concentrations in patients occurring at least 6 hours after drug administration, and are currently available for some, but not all, clinically advanced drugs. Details about these concentrations and how they were determined can be found by downloading the \"Csustained.xlsx\" table from the \"Download Bulk Data\" tab.", "", "If this option is not selected, the Upper_Conc_Limit_uM column will be populated with the most commonly used maximum concentration tested for each compound in the selected dataset."),
                                  size = "m",
                                  buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                ),
                              #Generating numeric slider input to specify how many concentrations to use for each compound when calculating viabilities
                                sliderInput("Viability_n_Conc", label = "How many concentrations should viabilities be calculated at for each compound?", value = 11, min = 2, max = 20, step = 1),
                              #Generating button to create and download template file
                                downloadButton(outputId = "Viability_Create_Template", label = "Download Template Instruction File") %>%
                                helper(type = "inline",
                                  title = "Create Template",
                                  icon = "question-circle", colour = NULL,
                                  content = c("Downloads a template Instruction file for the specified dataset and compounds for use in step 3. If at least one compound has been selected, the columns in the file will be as described below. Note that only the first four columns are necessary for the Instruction file, with the rest of the columns being provided for reference.", "",
                                              HTML("<b>Compound:</b>"), "Name of the compound for which viability values are to be calculated.", "",
                                              HTML("<b>n_Concentrations:</b>"), "The number of concentrations between Lower_Conc_Limit_uM and Upper_Conc_Limit_uM at which viability values should be calculated. Must be >=2 and <=20.", "",
                                              HTML("<b>Lower_Conc_Limit_uM:</b>"), "The lowest concentration to be used when calculating viability values (in microMolar). Defaults to 0.", "",
                                              HTML("<b>Upper_Conc_Limit_uM:</b>"), "The highest concentration to be used when calculating viability values (in microMolar). Defaults to Most_Commonly_Used_Max_Tested_Conc_uM or to Csustained_uM if a Csustained concentration is available and the \"Generate using Csustained when available?\" checkbox is ticked.", "",
                                              HTML("<b>Min_Tested_Conc_uM:</b>"), "The minimum tested concentration (in microMolar) of this compound in the selected dataset in any cell line.", "",
                                              HTML("<b>Max_Tested_Conc_uM:</b>"), "The maximum tested concentration (in microMolar) of this compound in the selected dataset in any cell line.", "",
                                              HTML("<b>Most_Commonly_Used_Min_Tested_Conc_uM:</b>"), "The most commonly used minimum tested concentration (in microMolar) of this compound in the selected dataset.", "",
                                              HTML("<b>Most_Commonly_Used_Max_Tested_Conc_uM:</b>"), "The most commonly used maximum tested concentration (in microMolar) of this compound in the selected dataset.", "",
                                              HTML("<b>Csustained_uM:</b>"), "The maximum plasma concentation of this compound achieved at least 6 hours after drug administration in a patient at a clinically usable dose. These values were obtained from: Ling, A. & Huang, R. S. Computationally predicting clinical drug combination efficacy with cancer cell line screens and independent drug action. Nat. Commun. 11, 1–13 (2020). Details about these concentrations and how they were determined can be found by downloading the \"Csustained.xlsx\" table from the \"Download Bulk Data\" tab."),
                                  size = "m",
                                  buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                )
                            )
                          ),
                        #Second Column
                          column(width = 6,
                            wellPanel(
                              h4("Step 3: Upload instruction file"),
                              fileInput("Viability_Instruction", label = "Upload Instruction file", accept = ".xlsx") %>%
                                helper(type = "inline",
                                  title = "Upload Instruction File",
                                  icon = "question-circle", colour = NULL,
                                  content = c("Upload an instruction file in .xlsx format which specifies the compounds and concentration ranges to calculate viability values for. The following columns are required, with additional columns being ignored.", "",
                                              HTML("<b>Compound:</b>"), "Name of the compound for which viability values are to be calculated.", "",
                                              HTML("<b>n_Concentrations:</b>"), "The number of concentrations between Lower_Conc_Limit_uM and Upper_Conc_Limit_uM at which viability values should be calculated. Must be >=2 and <=20.", "",
                                              HTML("<b>Lower_Conc_Limit_uM:</b>"), "The lowest concentration to be used when calculating viability values (in microMolar). Note that these values will be rounded to 3 significant digits.", "",
                                              HTML("<b>Upper_Conc_Limit_uM:</b>"), "The highest concentration to be used when calculating viability values (in microMolar). Note that these values will be rounded to 3 signficant digits.", "",
                                              HTML("<b><u>Important Note</u>:</b> When multiple records exist for the same compound, the specified concentration ranges will be layered on top of each other in order of appearance in the Instruction file, with overlapping concentrations being ignored. For example, if two records exist for compound A, with the first record requesting 5 concentrations between 1 and 5 uM and the second record requesting 6 concentrations between 0 and 10 uM, all 5 concentrations from the first record would be included (1,2,3,4,5) but only the concentrations from the second record that did not overlap those requested from the first record would be included (0,6,8,10), such that the concentrations at which viabilities were reported for this comopund would be 0, 1, 2, 3, 4, 5, 6, 8, and 10 uM.")),
                                  size = "m",
                                  buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                ),
                              uiOutput("Viability_Error"),
                              #Generating UI for cell line selection
                                uiOutput("Viability_Cell_Line_Menu")
                            )
                          )
                      )
                    )
                  } else {
                    list(p(""))
                  }
                })})
            
              #Processing values that depend on Viability_Dataset
                
                #Determining which compounds are available for selected dataset
                  Viability_Available_Compounds <- reactive({
                    req(input$Viability_Dataset)
                    if(! input$Viability_Dataset == ""){
                      names(compound_ccl_availability_successful)[sapply(compound_ccl_availability_successful, function(x){return(length(x[[input$Viability_Dataset]]) > 0)})]
                    } else {
                      NULL
                    }
                  })
                  
                  Viability_Simple_Compound_Harm_for_Dataset <- reactive({
                    req(input$Viability_Dataset)
                    if(! input$Viability_Dataset == ""){
                      Simple_Compound_Harm[Simple_Compound_Harm$Harmonized_Compound_Name %in% Viability_Available_Compounds(),]
                    } else {
                      NULL
                    }
                  })
                  
                #Getting molecular targets available for compounds available for selected cell line
                  Viability_Available_Molecular_Targets <- reactive({
                    req(Viability_Simple_Compound_Harm_for_Dataset())
                    temp_Compound_Molecular_Targets <- strsplit(Viability_Simple_Compound_Harm_for_Dataset()$Compound_Molecular_Targets, ":\\|:")
                    return(sort(unique(unlist(temp_Compound_Molecular_Targets))))
                  })
                
                #Getting mechanisms of action available for compounds available for selected cell line
                  Viability_Available_MOAs <- reactive({
                    req(Viability_Simple_Compound_Harm_for_Dataset())
                    temp_Compound_MOAs <- strsplit(Viability_Simple_Compound_Harm_for_Dataset()$Compound_MOA, ":\\|:")
                    return(sort(unique(unlist(temp_Compound_MOAs))))
                  })
                  
              #Defining compound filter interface
                isolate({output$Viability_Compound_Filters <- renderUI({
                  req(input$Viability_Show_Compound_Filters)
                  req(Viability_Available_Molecular_Targets())
                  req(Viability_Available_MOAs())
                  req(Viability_Simple_Compound_Harm_for_Dataset())
                  
                  if(input$Viability_Show_Compound_Filters == TRUE){
                    return(list(
                      h4("Filter compounds by:") %>%
                          helper(type = "inline",
                            title = "Compound filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("These options can be used to filter the compound options displayed in the \"Select compounds to calculate Viability values for\" menu. Note that selecting any options from a filter menu will omit all compounds which lack annotated information for that menu's filtering criteria."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                      wellPanel(list(
                        pickerInput(inputId = "Viability_Molecular_Target", label = "Filter compounds by molecular target", choices = Viability_Available_Molecular_Targets(), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                        helper(type = "inline",
                          title = "Molecular Target Filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Limits compounds displayed in the \"Select compounds to calculate Viability values for\" menu to compounds which target at least one of the selected molecular targets. Note that, if any selections have been made in the \"Filter compounds by free-text MOA\" menu, displayed compounds will also include compounds which have at least one of the selected MOAs."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
                        pickerInput(inputId = "Viability_MOA", label = "Filter compounds by free-text MOA", choices = Viability_Available_MOAs(), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE,`live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                        helper(type = "inline",
                          title = "Mechanism of Action (MOA) Filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Limits compounds displayed in the \"Select compounds to calculate Viability values for\" menu to compounds with at least one of the selected MOAs. Note that, if any selections have been made in the \"Filter compounds by molecular target\" menu, displayed compounds will also include compounds which target at least one of the selected molecular targets."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
                        pickerInput(inputId = "Viability_Clinical_Phase", label = "Filter compounds by clinical phase", choices = sort(unique(Viability_Simple_Compound_Harm_for_Dataset()$Compound_Clinical_Phase)), selected = NULL, multiple = TRUE, options = list(`actions-Box` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                        helper(type = "inline",
                          title = "Clinical Phase Filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("Limits compounds displayed in the \"Select compounds to calculate Viability values for\" menu to compounds whose annotated highest reached clinical phase is one of the selected phases."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        )
                      ))
                    ))
                  } else {
                    return(list(p("")))
                  }
                })})
                
              #Defining currently available compounds based on compound filters (meets any filter)
                Viability_Currently_Available_Compounds <- reactive({
                  req(Viability_Simple_Compound_Harm_for_Dataset())
  
                  Avail_compounds <- Viability_Simple_Compound_Harm_for_Dataset()$Harmonized_Compound_Name
                  Filtered_compounds <- character(0)
                  flag <- 0
                  
                  
                  if(! is.null(input$Viability_Molecular_Target)){
                    flag <- 1
                    Filtered_compounds <- Avail_compounds[Avail_compounds %in% names(Compound_Molecular_Targets)[sapply(Compound_Molecular_Targets, function(x,y){any(y %in% x)}, y = input$Viability_Molecular_Target)]]
                  }
                  
                  if(! is.null(input$Viability_MOA)){
                    if(flag == 0){
                      Filtered_compounds <- Avail_compounds[Avail_compounds %in% names(Compound_MOAs)[sapply(Compound_MOAs, function(x,y){any(y %in% x)}, y = input$Viability_MOA)]]
                    } else if(flag == 1){
                      Filtered_compounds <- c(Filtered_compounds, Avail_compounds[Avail_compounds %in% names(Compound_MOAs)[sapply(Compound_MOAs, function(x,y){any(y %in% x)}, y = input$Viability_MOA)]])
                    }
                    flag <- 1
                  }
                  
                  if(! is.null(input$Viability_Clinical_Phase)){
                    if(flag == 0){
                      Filtered_compounds <- Avail_compounds[Avail_compounds %in% Simple_Compound_Harm$Harmonized_Compound_Name[Simple_Compound_Harm$Compound_Clinical_Phase %in% input$Viability_Clinical_Phase]]
                    } else if(flag == 1){
                      Filtered_compounds <- Filtered_compounds[Filtered_compounds %in% Simple_Compound_Harm$Harmonized_Compound_Name[Simple_Compound_Harm$Compound_Clinical_Phase %in% input$Viability_Clinical_Phase]]
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
                observeEvent(Viability_Currently_Available_Compounds(), {
                  isolate({
                    updatePickerInput(session, "Viability_Compounds", label = paste0("Select compounds to calculate Viability values for (n = ", length(Viability_Currently_Available_Compounds()), ")"), choices = Viability_Currently_Available_Compounds(), selected = Viability_Currently_Available_Compounds())
                  })
                })
                
              #Generating Instruction template File
                Viability_Template <- reactive({
                  req(input$Viability_Dataset)
                  req(input$Viability_Compounds)
                  req(input$Viability_n_Conc)
                  req(! length(input$Viability_Use_Csustained) == 0)
                  
                  if(length(input$Viability_Compounds) > 0){
                    temp_data <- Dataset_Tested_Concentrations[Dataset_Tested_Concentrations$Compound %in% input$Viability_Compounds, c("Compound", paste0("max_mode_ccl_", input$Viability_Dataset, "_conc"), paste0("min_", input$Viability_Dataset, "_conc"), paste0("max_", input$Viability_Dataset, "_conc"), paste0("min_mode_ccl_", input$Viability_Dataset, "_conc"), paste0("max_mode_ccl_", input$Viability_Dataset, "_conc"))]
                    colnames(temp_data) <- c("Compound", "Upper_Conc_Limit_uM", "Min_Tested_Conc_uM", "Max_Tested_Conc_uM", "Most_Commonly_Used_Min_Tested_Conc_uM", "Most_Commonly_Used_Max_Tested_Conc_uM")
                    temp_data$n_Concentrations <- input$Viability_n_Conc
                    temp_data$Lower_Conc_Limit_uM <- 0
                    temp_data$Csustained_uM <- NA
                    temp_data$Csustained_uM <- signif(as.numeric(Csustained$`Csustained (uM)`[match(temp_data$Compound, Csustained$Compound)]), 3)
  
                    if(input$Viability_Use_Csustained == TRUE){
                      temp_data$Upper_Conc_Limit_uM[! is.na(temp_data$Csustained_uM)] <- temp_data$Csustained_uM[! is.na(temp_data$Csustained_uM)]
                    }
                    
                    column_order <- c("Compound", "n_Concentrations", "Lower_Conc_Limit_uM", "Upper_Conc_Limit_uM", "Min_Tested_Conc_uM", "Max_Tested_Conc_uM", "Most_Commonly_Used_Min_Tested_Conc_uM", "Most_Commonly_Used_Max_Tested_Conc_uM", "Csustained_uM")
                    temp_data <- temp_data[,column_order]
  
                    return(temp_data)
                  } else {
                    temp_colnames <- c("Compound", "n_Concentrations", "Lower_Conc_Limit_uM", "Upper_Conc_Limit_uM")
                    temp_data <- as.data.frame(matrix(NA, nrow = 1, ncol = length(temp_colnames)))
                    colnames(temp_data) <- temp_colnames
                    return(temp_data)
                  }
                })
  
              #Allowing user to download Instruction template file
                observeEvent(input$Viability_Dataset, {
                  output$Viability_Create_Template <- downloadHandler(
                    filename = paste0(input$Viability_Dataset, "_Viability_Instruction_File_Template.xlsx"),
                    content = function(file){
                      write.xlsx(Viability_Template(), file, row.names = FALSE)
                    }
                  )
                })
                
              #Reading Viability instruction file
                Viability_Instruction <- eventReactive(input$Viability_Instruction, valueExpr = {
                  req(input$Viability_Dataset)
                  req(input$Viability_Instruction)
                  
                  #Determining which compounds are available for selected dataset (repeating because it gets saved in wrong environment in previous observeEvent block)
                    temp_available_compounds <- names(compound_ccl_availability_successful)[sapply(compound_ccl_availability_successful, function(x){return(length(x[[input$Viability_Dataset]]) > 0)})]
                  #Loading file and making sure it is correctly formatted
                    #Checking file extension
                      file <- input$Viability_Instruction
                      req(file)
                      ext <- tools::file_ext(file$datapath)
                    #Printing error message if file extension is incorrect
                      if(! ext == "xlsx"){
                          output$Viability_Error <- renderUI({
                            p(HTML("<b>Error: The uploaded file must be in xlsx format.</b>"), style = "color:red")
                          })
                          Viability_Instruction <- return("")
                      } else {
                        #Loading data
                          data <- try(as.data.frame(read_xlsx(file$datapath)))
                        #Printing error message if file could not be loaded
                          if(class(data) == "try-error"){
                            output$Viability_Error <- renderUI({
                              p(HTML("<b>Error: The selected file could not be loaded. Is it really an xlsx file?</b>"), style = "color:red")
                            })
                            Viability_Instruction <- return("")
                          } else {
                            #Checking that required columns are present
                              required_columns <- c("Compound", "n_Concentrations", "Lower_Conc_Limit_uM", "Upper_Conc_Limit_uM")
                              if(! all(required_columns %in% colnames(data))){
                                output$Viability_Error <- renderUI({
                                  p(HTML("<b>Error: At least one required column is missing in uploaded file (Compound, n_Concentrations, Lower_Conc_Limit_uM, and Upper_Conc_Limit_uM). You can download an Instruction file template in Step 2 to see the required file format.</b>"), style = "color:red")
                                })
                                Viability_Instruction <- return("")
                              #Checking that data has > 0 rows
                              } else if(! nrow(data) > 0){
                                output$Viability_Error <- renderUI({
                                  p(HTML("<b>Error: Uploaded file has zero rows of data.</b>"), style = "color:red")
                                })
                                Viability_Instruction <- return("")
                              } else {
                                #Organizing data to have only required columns and making sure concentration columns are numeric
                                  data <- data[,required_columns]
                                  data$n_Concentrations <- as.numeric(data$n_Concentrations)
                                  data$Lower_Conc_Limit_uM <- as.numeric(data$Lower_Conc_Limit_uM)
                                  data$Upper_Conc_Limit_uM <- as.numeric(data$Upper_Conc_Limit_uM)
                                #Making vector of any drugs that are in the Instruction file but not in the selected dataset
                                  mismatched_drugs <- sort(unique(data$Compound[! data$Compound %in% temp_available_compounds]))
                                #Doing error handling for Instruction file upload
                                  #Checking that all listed compounds are present in the selected dataset
                                  if(length(mismatched_drugs) != 0){
                                    output$Viability_Error <- renderUI({list(
                                      p(HTML("<b>Error: Uploaded instruction file contains the following compounds that do not exist in selected dataset. Was it generated using this dataset?</b>"), style = "color:red"),
                                      p(paste(mismatched_drugs, collapse = "; "), style = "color:red")
                                    )})
                                    Viability_Instruction <- return("")
                                  #Checking that there are no missing n_Concentrations values
                                  } else if(any(is.na(data$n_Concentrations))){
                                    output$Viability_Error <- renderUI({
                                      p(HTML("<b>Error: Missing or non-numeric values exist in n_Concentrations column.</b>"), style = "color:red")
                                    })
                                    Viability_Instruction <- return("")
                                  #Checking that all n_Concentrations values are integers that are >= 2 and <= 20
                                  } else if(any(data$n_Concentrations != floor(data$n_Concentrations)) | any(data$n_Concentrations < 2) | any(data$n_Concentrations > 20)){
                                    output$Viability_Error <- renderUI({
                                      p(HTML("<b>Error: Some n_Concentration values are not integers that are >= 2 and <= 20.</b>"), style = "color:red")
                                    })
                                    Viability_Instruction <- return("")
                                  #Checking that the Lower_Conc_Limit_uM column has no missing values
                                  } else if(any(is.na(data$Lower_Conc_Limit_uM))){
                                    output$Viability_Error <- renderUI({
                                      p(HTML("<b>Error: Missing or non-numeric values exist in Lower_Conc_Limit_uM column.</b>"), style = "color:red")
                                    })
                                    Viability_Instruction <- return("")
                                  #Checking that Lower_Conc_Limit_uM are all >= 0
                                  } else if(! all(data$Lower_Conc_Limit_uM >= 0)){
                                    output$Viability_Error <- renderUI({
                                      p(HTML("<b>Error: Lower_Conc_Limit_uM column contains values that are less than 0.</b>"), style = "color:red")
                                    })
                                    Viability_Instruction <- return("")
                                  #Checking that the Upper_Conc_Limit_uM column has no missing values
                                  } else if(any(is.na(data$Upper_Conc_Limit_uM))){
                                    output$Viability_Error <- renderUI({
                                      p(HTML("<b>Error: Missing or non-numeric values exist in Upper_Conc_Limit_uM column.</b>"), style = "color:red")
                                    })
                                    Viability_Instruction <- return("")
                                  #Checking that all Upper_Conc_Limit_uM values are greater than Lower_Conc_Limit_uM
                                  } else if(any(data$Upper_Conc_Limit_uM <= data$Lower_Conc_Limit_uM)){
                                    output$Viability_Error <- renderUI({
                                      p(HTML("<b>Error: Some Upper_Conc_Limit_uM values are less than or equal to their corresponding Lower_Conc_Limit_uM values.</b>"), style = "color:red")
                                    })
                                    Viability_Instruction <- return("")
                                  #Returning properly formatted data
                                  } else {
                                    output$Viability_Error <- renderUI({})
                                    Viability_Instruction <- return(data)
                                  }
                              }
                          }
                      }
                })
                
              #Initializing Cell Line Selection Menu if Instruction file upload complete and validated
                #Rendering cell line selection menu UI
                  isolate({output$Viability_Cell_Line_Menu <- renderUI({
                    req(input$Viability_Dataset)
                    
                    if(is.data.frame(Viability_Instruction())){
                      return_list <- list(
                        h4("Step 4: Cell line selection"),
                        #Cell line selection menu
                          pickerInput("Viability_Cell_Lines", label = "Select cell lines to calculate viability values for", choices = NULL, selected = NULL, multiple = TRUE, options = list(
                            `actions-Box` = TRUE,
                            `live-Search-Style` = "contains" ,
                            `live-Search` = TRUE,
                            `live-Search-Normalize` = TRUE,
                            `selected-Text-Format` = "count"
                          )),
                        #Checkbox to show or hide cell line filters
                          checkboxInput("Viability_Show_Cell_Line_Filters", label = "Show cell line filters?", value = FALSE),
                        #Cell line filters
                          uiOutput("Viability_Cell_Line_Filters"),
                        #Calculation interface
                          h4("Step 5: Calculate Viabilities"),
                        #Button to toggle whether or not uncertainties should be estimated
                          checkboxInput("Viability_Calculate_Uncertainty", label = "Calculate viability standard errors?", value = FALSE) %>%
                                  helper(type = "inline",
                                    title = "Calculating standard errors",
                                    icon = "question-circle", colour = NULL,
                                    content = c(HTML("Selecting this option will enable standard error estimates for calculated viability values (see Methods tab)."), "", HTML("<b>WARNING:</b> Estimating standard errors is <u>very slow</u> when estimating viabilites for a large number of compounds. Please only choose this option if you actually need standard error estimates. If using this option, we strongly recommend calculating viabilities in small batches of compounds at a time to minimize the risk of losing progress due to server disconnects.")),
                                    size = "m",
                                    buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                  ),
                        #Checkbox to toggle whether or not output should be formatted for IDACombo shiny app
                          checkboxInput("Viability_Format_For_IDACombo", label = "Format output for use with IDACombo shiny app?", value = FALSE) %>%
                                  helper(type = "inline",
                                    title = "Formatting for IDACombo shiny app",
                                    icon = "question-circle", colour = NULL,
                                    content = HTML("Selecting this option will format the output file so that it can be directly used as a custom input dataset for the IDACombo shiny app, an app developed by our group to use monotherapy drug screening data to predict drug combination efficacy. The IDACombo app can be found at <a href=\"https://huanglab.shinyapps.io/idacombo-shiny-app/\">https://huanglab.shinyapps.io/idacombo-shiny-app/</a>"),
                                    size = "m",
                                    buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                  ),
                        #Button to do Viability calculation
                          actionButton(inputId = "Viability_Calc", label = "Calculate Viability Values"),
                        #Button to generate and download Viability values
                          uiOutput("Viability_Download_UI")
                      )
                    } else {
                      return_list <- list(p(""))
                    }
                    
                    return(return_list)
                  })})
                  
                #Determining which compounds have been selected
                  Viability_Instruction_Compounds <- reactive({
                    req(Viability_Instruction())
                    sort(unique(Viability_Instruction()$Compound))
                  })
                  
                #Determining which cell lines are available for the specified compounds in the selected dataset
                  Viability_Available_Cell_Lines <- reactive({
                    req(input$Viability_Dataset)
                    req(Viability_Instruction_Compounds())
                      sort(unique(unlist(lapply(compound_ccl_availability_successful[names(compound_ccl_availability_successful) %in% Viability_Instruction_Compounds()], function(x){return(x[input$Viability_Dataset])}))))
                  })
                
                #Subsetting cell line info to cell lines available for specified compounds and dataset
                  Viability_Simple_Cell_Line_Harm <- reactive({
                    req(Viability_Available_Cell_Lines())
                    Simple_Cell_Line_Harm[Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID %in% Viability_Available_Cell_Lines(),]
                  })
                  
                #Getting age limits
                  Viability_Age_Limits <- reactive({
                    req(Viability_Simple_Cell_Line_Harm())
                    
                    temp_min <- suppressWarnings(min(Viability_Simple_Cell_Line_Harm()$Numeric_Age_in_Years, na.rm = TRUE))
                    if(temp_min == Inf){
                      temp_min <- 0
                    }
                    temp_max <- suppressWarnings(max(Viability_Simple_Cell_Line_Harm()$Numeric_Age_in_Years, na.rm = TRUE))
                    if(temp_max == -Inf){
                      temp_max <- 0
                    }
                    return(c(temp_min, temp_max))
                  })
                  
                #Getting disease information
                  Viability_Cell_Line_Diseases <- reactive({
                    req(Viability_Simple_Cell_Line_Harm())
                    
                    temp_Cell_Line_Diseases <- strsplit(Viability_Simple_Cell_Line_Harm()$Diseases, ":\\|:")
                    names(temp_Cell_Line_Diseases) <- Viability_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID
                    return(temp_Cell_Line_Diseases)
                  })
                  
                #Defining cell line filter interface
                  isolate({output$Viability_Cell_Line_Filters <- renderUI({
                    req(! length(input$Viability_Show_Cell_Line_Filters) == 0)
                    req(Viability_Simple_Cell_Line_Harm())
                    req(Viability_Cell_Line_Diseases())
                    
                    if(input$Viability_Show_Cell_Line_Filters == TRUE){
                      return_list <- list(
                        h4("Filter cell lines by:") %>%
                        helper(type = "inline",
                          title = "Cell line filtering",
                          icon = "question-circle", colour = NULL,
                          content = c("These options can be used to filter the cell line options displayed in the \"Select cell lines to calculate viability values for\" menu. Note that, once any options have been selected for a given filtering criteria, any cell lines that are missing information for that criteria will be excluded."),
                          size = "m",
                          buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                        ),
                        wellPanel(list(
                          pickerInput(inputId = "Viability_Cancer_Type", label = "General cancer type", choices = sort(unique(Viability_Simple_Cell_Line_Harm()$Simple_Cancer_Type)), multiple = TRUE, options = list(`actions-Box` = TRUE, `live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                          helper(type = "inline",
                            title = "General cancer type filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Selecting any cancer type options will limit displayed cell line options to only include cell lines from the selected cancer types. If any options have been selected from the \"Free-text disease name\" menu, cell lines will also be displayed that meet at least one of the disease name criteria selected in that menu."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                          pickerInput(inputId = "Viability_Disease_Name", label = "Free-text disease name", choices = sort(unique(unlist(Viability_Cell_Line_Diseases()))), multiple = TRUE, options = list(`actions-Box` = TRUE, `live-Search-Style` = "contains" , `live-Search` = TRUE, `live-Search-Normalize` = TRUE, `selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                          helper(type = "inline",
                            title = "Free-text disease name filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Selecting any disease name options will limit displayed cell line options to only include cell lines from the selected diseases. If any options have been selected from the \"General cancer type\" menu, cell lines will also be displayed that meet at least one of the cancer type criteria selected in that menu."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                          pickerInput(inputId = "Viability_Gender", label = "Gender", choices = sort(unique(Viability_Simple_Cell_Line_Harm()$Gender)), multiple = TRUE, options = list(`selected-Text-Format` = "count", `none-Selected-Text` = "Optional")) %>%
                          helper(type = "inline",
                            title = "Gender filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Selecting any gender options will limit the displayed cell line options to only include cell lines of the selected gender(s)."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
  
                          sliderInput(inputId = "Viability_Age", label = "Patient age (years)", min = Viability_Age_Limits()[1], max = Viability_Age_Limits()[2], value = Viability_Age_Limits(), ticks = FALSE) %>%
                          helper(type = "inline",
                            title = "Patient age filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Sets minimum and maximum age allowed for displayed cell line options. Note that setting the slider to anything other than its maximum range will exclude cell lines for which a numeric age could not be determined for the patient who the cell line was derived from at the time of sample collection. This both includes cases where the age is unspecified and cases where the specified age is ambiguous (i.e. such as \"Adult\"). You may download the cell line harmonization file in the \"Download Bulk Data\" tab for free-text descriptions of each cell line's patient age."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
  
                          h4("Ancestry") %>%
                          helper(type = "inline",
                            title = "Patient age filtering",
                            icon = "question-circle", colour = NULL,
                            content = c("Each slider sets the minimum and maximum % ancestry for each ancestry group. Once any of the ancestry sliders has been changed from its maximum range, cell lines will be filtered to only include lines which meet the limits set on all of the ancestry sliders. Note that % ancestry adds to 100% across all ancestry groups for each individual cell line (i.e. a cell line cannot have 60% African ancestry and 60% Native American ancestry, because that would add to >100%). Ancestry information was obtained from the cellosaurus resource at <a href=\"https://www.expasy.org/\">https://www.expasy.org/</a>."),
                            size = "m",
                            buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                          ),
                          wellPanel(list(
                            sliderInput(inputId = "Viability_African", label = "% african ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "Viability_Native_American", label = "% native american ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "Viability_East_Asian_North", label = "% east asian (north) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "Viability_East_Asian_South", label = "% east asian (south) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "Viability_South_Asian", label = "% south asian ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "Viability_European_North", label = "% european (north) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE),
                            sliderInput(inputId = "Viability_European_South", label = "% european (south) ancestry", min = 0, max = 100, value = c(0,100), ticks = FALSE)
                          ))
                        ))
                      )
                      return(return_list)
                    } else {
                      return(list(" "))
                    }
                  })})
                  
                #Updating cell line filter interface
                  #Defining currently available cell lines based on cell line filters
                    Viability_Currently_Available_Cell_Lines <- reactive({
                      req(Viability_Available_Cell_Lines())
                      req(Viability_Simple_Cell_Line_Harm())
                      req(Viability_Cell_Line_Diseases())
  
  
                      Avail_ccls <- Viability_Available_Cell_Lines()
                      Filtered_ccls <- character(0)
                      flag <- 0
  
                      if(! is.null(input$Viability_Cancer_Type)){
                        flag <- 1
                        Filtered_ccls <- Avail_ccls[Avail_ccls %in% Viability_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[Viability_Simple_Cell_Line_Harm()$Simple_Cancer_Type %in% input$Viability_Cancer_Type]]
                      }
  
                      if(! is.null(input$Viability_Disease_Name)){
                        flag <- 1
                        Filtered_ccls <- sort(unique(c(Filtered_ccls, Avail_ccls[Avail_ccls %in% names(Viability_Cell_Line_Diseases())[sapply(Viability_Cell_Line_Diseases(), function(x,y){return(any(y %in% x))}, y = input$Viability_Disease_Name)]])))
                      }
  
                      if(! is.null(input$Viability_Gender)){
                        if(flag == 0){
                          Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Viability_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[Viability_Simple_Cell_Line_Harm()$Gender %in% input$Viability_Gender]]))
                        } else if(flag == 1){
                          Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Viability_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[Viability_Simple_Cell_Line_Harm()$Gender %in% input$Viability_Gender]]]))
                        }
                        flag <- 1
                      }
  
                      if(! all(input$Viability_Age == Viability_Age_Limits())){
                        if(flag == 0){
                          Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Viability_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[Viability_Simple_Cell_Line_Harm()$Numeric_Age_in_Years >= input$Viability_Age[1] &
                                                                                                               Viability_Simple_Cell_Line_Harm()$Numeric_Age_in_Years <= input$Viability_Age[2]]]))
                        } else if(flag == 1){
                          Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Viability_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[Viability_Simple_Cell_Line_Harm()$Numeric_Age_in_Years >= input$Viability_Age[1] &
                                                                                                                                                            Viability_Simple_Cell_Line_Harm()$Numeric_Age_in_Years <= input$Viability_Age[2]]]]))
                        }
                        flag <- 1
                      }
  
                      if(! all(input$Viability_African == c(0,100)) |
                         ! all(input$Viability_Native_American == c(0,100)) |
                         ! all(input$Viability_East_Asian_North == c(0,100)) |
                         ! all(input$Viability_East_Asian_South == c(0,100)) |
                         ! all(input$Viability_South_Asian == c(0,100)) |
                         ! all(input$Viability_European_North == c(0,100)) |
                         ! all(input$Viability_European_South == c(0,100))){
                        if(flag == 0){
                          Filtered_ccls <- sort(unique(Avail_ccls[Avail_ccls %in% Viability_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[Viability_Simple_Cell_Line_Harm()$African_Ancestry >= input$Viability_African[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$African_Ancestry <= input$Viability_African[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$Native_American_Ancestry >= input$Viability_Native_American[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$Native_American_Ancestry <= input$Viability_Native_American[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`East_Asian_(North)_Ancestry` >= input$Viability_East_Asian_North[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`East_Asian_(North)_Ancestry` <= input$Viability_East_Asian_North[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`East_Asian_(South)_Ancestry` >= input$Viability_East_Asian_South[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`East_Asian_(South)_Ancestry` <= input$Viability_East_Asian_South[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$South_Asian_Ancestry >= input$Viability_South_Asian[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$South_Asian_Ancestry <= input$Viability_South_Asian[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`European_(North)_Ancestry` >= input$Viability_European_North[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`European_(North)_Ancestry` <= input$Viability_European_North[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`European_(South)_Ancestry` >= input$Viability_European_South[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`European_(South)_Ancestry` <= input$Viability_European_South[2]/100]]))
                        } else if(flag == 1){
                          Filtered_ccls <- sort(unique(Filtered_ccls[Filtered_ccls %in% Avail_ccls[Avail_ccls %in% Viability_Simple_Cell_Line_Harm()$Harmonized_Cell_Line_ID[Viability_Simple_Cell_Line_Harm()$African_Ancestry >= input$Viability_African[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$African_Ancestry <= input$Viability_African[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$Native_American_Ancestry >= input$Viability_Native_American[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$Native_American_Ancestry <= input$Viability_Native_American[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`East_Asian_(North)_Ancestry` >= input$Viability_East_Asian_North[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`East_Asian_(North)_Ancestry` <= input$Viability_East_Asian_North[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`East_Asian_(South)_Ancestry` >= input$Viability_East_Asian_South[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`East_Asian_(South)_Ancestry` <= input$Viability_East_Asian_South[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$South_Asian_Ancestry >= input$Viability_South_Asian[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$South_Asian_Ancestry <= input$Viability_South_Asian[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`European_(North)_Ancestry` >= input$Viability_European_North[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`European_(North)_Ancestry` <= input$Viability_European_North[2]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`European_(South)_Ancestry` >= input$Viability_European_South[1]/100 &
                                                                                                               Viability_Simple_Cell_Line_Harm()$`European_(South)_Ancestry` <= input$Viability_European_South[2]/100]]]))
                        }
                        flag <- 1
                      }
  
                      if(flag == 0){
                        return(Avail_ccls)
                      } else if(flag == 1){
                        return(Filtered_ccls)
                      }
                    })
  
                  #Updating selection menus based on Viability_Currently_Available_Cell_Lines()
                    observeEvent(Viability_Currently_Available_Cell_Lines(), {
                      #Updating cell line selection menu
                      isolate({
                        updatePickerInput(session, "Viability_Cell_Lines", label = paste0("Select cell lines to calculate viability values for (n = ", length(Viability_Currently_Available_Cell_Lines()), ")"), choices = Viability_Currently_Available_Cell_Lines(), selected = Viability_Currently_Available_Cell_Lines())
                      })
                    })
  
              #Generating Viability Values
                Viability_Custom_Values <- eventReactive(input$Viability_Calc, ignoreInit = TRUE, valueExpr =  {
                    req(input$Viability_Calc)
                  isolate({
                    #Preloading data for coding ease
                      temp_Instruction <- Viability_Instruction()
                      temp_Cell_Lines <- input$Viability_Cell_Lines
                      temp_dataset <- input$Viability_Dataset
                      temp_via_calc_uncertainty <- input$Viability_Calculate_Uncertainty
                      temp_via_format_IDACombo <- input$Viability_Format_For_IDACombo
                      wd <- getwd()
                    
                    #Generating requested concentrations for each compound
                      temp_compounds <- sort(unique(temp_Instruction$Compound))
                      temp_conc_list <- list(NULL)
                      for(i in 1:length(temp_compounds)){
                        temp_dat <- temp_Instruction[temp_Instruction$Compound %in% temp_compounds[i],]
                        to_return <- NULL
                        for(j in 1:nrow(temp_dat)){
                          if(j == 1){
                            to_return <- signif(seq(from = temp_dat$Lower_Conc_Limit_uM[j], to = temp_dat$Upper_Conc_Limit_uM[j], length.out = temp_dat$n_Concentrations[j]), 3)
                          } else {
                            temp_conc <- signif(seq(from = temp_dat$Lower_Conc_Limit_uM[j], to = temp_dat$Upper_Conc_Limit_uM[j], length.out = temp_dat$n_Concentrations[j]), 3)
                            to_return <- c(temp_conc[temp_conc < min(to_return)], to_return, temp_conc[temp_conc > max(to_return)])
                          }
                        }
                        temp_conc_list[[i]] <- to_return
                      }
                      names(temp_conc_list) <- temp_compounds
                       
                    #Processing asynchronously 
                      show_modal_spinner(
                                spin = "atom",
                                color = "#112446",
                                text = "Calculating Viabilities..."
                              )
                      progress <- AsyncProgress$new(session, min = 0, max = length(temp_conc_list), message = "Initializing Calculation...")
                      future_data <- future(
                        global = c("progress", "temp_conc_list", "Compound_Filenames", "temp_Instruction", "temp_Cell_Lines", "temp_dataset", "temp_via_calc_uncertainty", "temp_via_format_IDACombo", "wd", "ll.4", "predict.handle.errors.drc", "estfun.drc", "bread.drc", "meat.drc", "sandwich", "predict.drc", "Simple_Cell_Line_Harm"),
                        packages = c("drc"),
                        expr = {
                          #Setting working directory
                            setwd(wd)
                          #Calculating viabilities    
                          if(temp_via_calc_uncertainty == FALSE){
                            #Performing calculations when standard errors are not needed
                              #Loading fitted curves and calculating Viability values
                                Calculated_Viabilities <- vector(mode = "list", length = 0)
                                  for(i in 1:length(temp_conc_list)){
                                    if(i == 1){
                                      progress$set(value = 0, message = paste0(0, " of ", length(temp_conc_list), " Compounds Complete..."))
                                    }
                                    #Loading results for this cell line
                                      filename <- Compound_Filenames$drug_file_names[Compound_Filenames$drugs %in% names(temp_conc_list)[i]]
                                      temp_results <- readRDS(paste0("./www/Results/", filename, ".rds"))[[temp_dataset]]
                                    #Subsetting to selected cell lines
                                      temp_results <- temp_results[temp_results$Cell_Line %in% temp_Cell_Lines & ! is.na(temp_results$b_c_d_e),]
                                    if(nrow(temp_results) > 0){
                                      #Determining which concentrations to calculate viability values at
                                        temp_Concentrations <- temp_conc_list[[i]]
                                        temp_n_conc <- length(temp_Concentrations)
                                      #Calculating Viability Values
                                        temp_Viabilities <- ll.4(x = rep(temp_Concentrations, times = nrow(temp_results)), b_c_d_e = rep(temp_results$b_c_d_e, each = temp_n_conc))
                                      #Constructing return values
                                        temp_Return <- data.frame("Compound" = rep(temp_results$Compound, each = temp_n_conc),
                                                                  "Cell_Line" = rep(temp_results$Cell_Line, each = temp_n_conc),
                                                                  "Viability" = temp_Viabilities,
                                                                  "Concentration_uM" = rep(temp_Concentrations, times = nrow(temp_results)),
                                                                  "Min_Tested_Conc_uM" = rep(temp_results$min_dose_uM, each = temp_n_conc),
                                                                  "Max_Tested_Conc_uM" = rep(temp_results$max_dose_uM, each = temp_n_conc),
                                                                  stringsAsFactors = FALSE
                                                                  )
                                      #Storing result
                                        Calculated_Viabilities[[i]] <- temp_Return
                                    } else {
                                      #Constructing empty result data frame
                                        temp_Return <- data.frame("Compound" = character(0),
                                                                  "Cell_Line" = character(0),
                                                                  "Viability" = numeric(0),
                                                                  "Concentration_uM" = numeric(0),
                                                                  "Min_Tested_Conc_uM" = numeric(0),
                                                                  "Max_Tested_Conc_uM" = numeric(0),
                                                                  stringsAsFactors = FALSE
                                                                  )
                                      #Storing empty data frame
                                        Calculated_Viabilities[[i]] <- temp_Return
                                    }
                                    progress$set(value = i, message = paste0(i, " of ", length(temp_conc_list), " Compounds Complete..."))
                                  }
                              #Organizing calculated Viability values
                                Returnable_Viabilities <- as.data.frame(do.call(rbind, Calculated_Viabilities))
                              if(temp_via_format_IDACombo == TRUE){
                                 #If option is selected, formatting for IDACombo app
                                    Returnable_Viabilities <- Returnable_Viabilities[,c("Compound", "Cell_Line", "Viability", "Concentration_uM")]
                                  #Renaming columns
                                    colnames(Returnable_Viabilities) <- c("Drug", "Cell_Line", "Efficacy", "Drug_Dose")
                                  #Adding cell line cancer types
                                    Returnable_Viabilities$Cell_Line_Subgroup <- NA
                                    Returnable_Viabilities$Cell_Line_Subgroup <- Simple_Cell_Line_Harm$Simple_Cancer_Type[match(Returnable_Viabilities$Cell_Line, Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID)] 
                              }
                                progress$close()
                                return(Returnable_Viabilities)
                          } else if(temp_via_calc_uncertainty == TRUE){ #END: if(temp_via_calc_uncertainty == FALSE){
                            #Performing calculations when standard errors are needed
                              #Loading fitted curves and calculating Viability values
                                Calculated_Viabilities <- vector(mode = "list", length = 0)
                                  for(i in 1:length(temp_conc_list)){
                                    if(i == 1){
                                      progress$set(value = 0, message = paste0(0, " of ", length(temp_conc_list), " Compounds Complete..."))
                                    }
                                    #Loading results for this cell line
                                      filename <- Compound_Filenames$drug_file_names[Compound_Filenames$drugs %in% names(temp_conc_list)[i]]
                                      temp_results <- readRDS(paste0("./www/Curve_Fits/", temp_dataset, "/", filename, ".rds"))
                                    #Removing drug from result list names
                                      names(temp_results) <- gsub(":\\|:.*$", "", names(temp_results))
                                    #Subsetting to selected cell lines with successful fits
                                      temp_results <- temp_results[names(temp_results) %in% temp_Cell_Lines & ! sapply(temp_results, length) == 1]
                                    if(length(temp_results) > 0){
                                      #Determining which concentrations to calculate viability values at
                                        temp_Concentrations <- data.frame(dose = temp_conc_list[[i]])
                                      #Calculating Viability Values
                                        temp_Viabilities <- as.data.frame(do.call(rbind, lapply(temp_results, predict.handle.errors.drc, newdata = temp_Concentrations, se.fit = T, vcov. = sandwich)))
                                        temp_n_conc <- nrow(temp_Concentrations)
                                      #Constructing return values
                                        temp_Return <- data.frame("Compound" = names(temp_conc_list)[i],
                                                                  "Cell_Line" = rep(names(temp_results), each = temp_n_conc),
                                                                  "Viability" = temp_Viabilities$Prediction,
                                                                  "Viability_SE" = temp_Viabilities$SE,
                                                                  "Concentration_uM" = rep(temp_Concentrations[,1], times = length(temp_results)),
                                                                  "Min_Tested_Conc_uM" = rep(sapply(temp_results, function(x){return(min(x$dataList$dose))}), each = temp_n_conc),
                                                                  "Max_Tested_Conc_uM" = rep(sapply(temp_results, function(x){return(max(x$dataList$dose))}), each = temp_n_conc),
                                                                  stringsAsFactors = FALSE
                                                                  )
                                      #Repairing missing viability values at 0 concentration
                                        temp_Params <- sapply(temp_results[temp_Return$Cell_Line], function(x){return(paste(x$coefficients, collapse = "_"))})
                                        if(any(is.na(temp_Return$Viability))){
                                          temp_Return$Viability[is.na(temp_Return$Viability)] <- ll.4(x = temp_Return$Concentration_uM[is.na(temp_Return$Viability)], b_c_d_e = temp_Params[is.na(temp_Return$Viability)])
                                        }
                                      #If SE values could not be estimated (usually happens when slope == 0), assigning unknown SE as median SE from other cell lines at that concentration with this drug
                                        if(any(is.na(temp_Return$Viability_SE))){
                                          for(j in 1:nrow(temp_Concentrations)){
                                            temp_Return$Viability_SE[is.na(temp_Return$Viability_SE) & temp_Return$Concentration_uM == temp_Concentrations$dose[j]] <- median(temp_Return$Viability_SE[! is.na(temp_Return$Viability_SE) & temp_Return$Concentration_uM == temp_Concentrations$dose[j]])
                                          }
                                        }
                                      #Storing result
                                        Calculated_Viabilities[[i]] <- temp_Return
                                    } else {
                                      #Constructing empty result data frame
                                        temp_Return <- data.frame("Compound" = character(0),
                                                                  "Cell_Line" = character(0),
                                                                  "Viability" = numeric(0),
                                                                  "Viability_SE" = numeric(0),
                                                                  "Concentration_uM" = numeric(0),
                                                                  "Min_Tested_Conc_uM" = numeric(0),
                                                                  "Max_Tested_Conc_uM" = numeric(0),
                                                                  stringsAsFactors = FALSE
                                                                  )
                                      #Storing empty data frame
                                        Calculated_Viabilities[[i]] <- temp_Return
                                    }
                                    progress$set(value = i, message = paste0(i, " of ", length(temp_conc_list), " Compounds Complete..."))
                                  }
                              #Organizing calculated Viability values
                                Returnable_Viabilities <- as.data.frame(do.call(rbind, Calculated_Viabilities))
                              if(temp_via_format_IDACombo == TRUE){
                                 #If option is selected, formatting for IDACombo app
                                    Returnable_Viabilities <- Returnable_Viabilities[,c("Compound", "Cell_Line", "Viability", "Viability_SE", "Concentration_uM")]
                                  #Renaming columns
                                    colnames(Returnable_Viabilities) <- c("Drug", "Cell_Line", "Efficacy", "Efficacy_SE", "Drug_Dose")
                                  #Adding cell line cancer types
                                    Returnable_Viabilities$Cell_Line_Subgroup <- NA
                                    Returnable_Viabilities$Cell_Line_Subgroup <- Simple_Cell_Line_Harm$Simple_Cancer_Type[match(Returnable_Viabilities$Cell_Line, Simple_Cell_Line_Harm$Harmonized_Cell_Line_ID)]
                              }
                                progress$close()
                                return(Returnable_Viabilities)
                          } #END: else if(temp_via_calc_uncertainty == TRUE){
                        }) #END: future_data <- future(...
                  }) #END: isolate
                  promise_race(future_data) %...>% {remove_modal_spinner()}
                  future_data
                })
                
              #Allowing user to download results
                observeEvent(Viability_Custom_Values(), {
                  req(Viability_Custom_Values())
                  req(input$Viability_Dataset)
                  promise_all(data = Viability_Custom_Values()) %...>% with({
                    isolate({
                    #Creating Download Button
                      output$Viability_Download_UI <- renderUI({
                        if(is.data.frame(data)){
                          downloadButton(outputId = "Viability_Download_Viability_Values", label = "Download Calculated Viability Values") %>%
                                  helper(type = "inline",
                                    title = "Download Viabilities",
                                    icon = "question-circle", colour = NULL,
                                    content = HTML("Pressing this button will download a tab separated value (.tsv) text file containing the calculated viability values, where each row reports a calculated viability value for a single set of compound, cell line, and concentration. This file will have the following columns:
                                                   <ol>
                                                    <li><b>Compound:</b> Compound name.</li>
                                                    <li><b>Cell_Line:</b> Cell line name.</li>
                                                    <li><b>Viability:</b> The calculated viability value for this compound, cell line, and concentration. These values range from ~0 to ~1, with 0 indicating 0% viability and 1 indicating 100% viability. Viability is definined in the \"About Simplicity/Methods\" tab.</li>
                                                    <li><b>Viability_SE:</b> Reports estimated standard error values for the calculated viability values, and only appears if the \"Calculate viability standard errors?\" option was selected prior to calculating the viability values.</li>
                                                    <li><b>Concentration_uM:</b> The concentration, in microMolar, for which this viability is being reported.</li>
                                                    <li><b>Min_Tested_Conc_uM:</b> The minimum tested concentration, in microMolar, for this compound with this cell line in this dataset.</li>
                                                    <li><b>Max_Tested_Conc_uM:</b> The maximum tested concentration, in microMolar, for this compound with this cell line in this dataset.</li>
                                                  </ol>
                                                  <br/>
                                                   Note that, if the \"Format ouput for use with IDACombo shiny app?\" option was selected prior to calculating the viabilites, the Compound, Cell_Line, Viability, Viability_SE, and Concentration_uM columns will be named Drug, Cell_Line, Efficacy, Efficacy_SE, and Drug_Dose respectively, and a Cell_Line_Subgroup column will be added containing the general cancer type corresponding with each row's cell line."),
                                    size = "m",
                                    buttonLabel = "Okay", easyClose = TRUE, fade = FALSE
                                  )
                        } else {
                          
                        }
                      })
                      
                    #Allowing user to download data
                      output$Viability_Download_Viability_Values <- downloadHandler(
                            filename = paste0(input$Viability_Dataset, "_Calculated_Viabilities.tsv"),
                            content = function(file){
                              
                              show_modal_spinner(
                                spin = "atom",
                                color = "#112446",
                                text = "Preparing Results for Download..."
                              )
                              
                              success.check <- try(write_delim(data, file, delim = "\t"))
                              
                              if(class(success.check) == "try-error"){
                                warning("Download unsuccessful: ", Sys.time())
                                temp_class <- class(data)
                                warning("File class = ", temp_class)
                                if(temp_class == "data.frame"){
                                  temp_type <- NA
                                  for(i in 1:ncol(data)){
                                    temp_type[i] <- typeof(data[,i])
                                  }
                                  warning(paste0("Column Types: ", paste(temp_type, collapse = ", ")))
                                }
                              }
                              
                              remove_modal_spinner()
                            }
                          )
                    }) #END: isolate
                  }) #END: promise_all(data = Viability_Custom_Values()) %...>% with({
                }) #END: observeEvent(Viability_Custom_Values(), {
        
    remove_modal_spinner() #For app startup
  }

#Assemble user interface and server
  shinyApp(ui = ui, server = server)  
  