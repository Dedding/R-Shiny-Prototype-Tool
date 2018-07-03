library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(VIM)
library(shinydashboard)
library(Hmisc)
library(mlbench)
library(mice)

header <- dashboardHeader(
    title = "KD for Domain Experts"
)

#SIDE BAR
sidebar <- dashboardSidebar(
    sidebarMenu(
        id="selectionMenu",
        imageOutput("uulogo", inline = TRUE),
        tags$hr(),
        menuItem(
            "Data Preparation", 
            tabName = "dataPreparation", 
            icon = icon("cog"),
            selected=TRUE
        ),
        menuItem(
            "Guideline", 
            tabName = "guideline", 
            icon = icon("info-circle"),
            selected=FALSE
        )
    )
)

body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        tabItem(
            style="overflow-y: auto;",
            tabName = "dataPreparation",
            column(width = 4,
        #LOAD AND DESCRIBE DATA
                box(title = "Data Loading", width = NULL, collapsible = TRUE, class = "mainBox",
                    fileInput('datafile1', 'Choose First CSV file', accept=c('text/csv', 'text/comma-separated-values, text/plain')),
                    fileInput('datafile2', 'Choose Second CSV file', accept=c('text/csv', 'text/comma-separated-values, text/plain')),
                    tags$hr(),
                    box(width = NULL, collapsible = TRUE, title = "Loading Options", 
                        p(
                            class = "text-muted",
                            paste("Select the best way to load your files...")
                        ),
                        column(width = 4,
                            checkboxInput("header", "Header", TRUE),
                            checkboxInput("missingText", "Missing Strings as NA", TRUE)
                        ),
                        column(width = 4,
                            radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ";")
                        ),
                        column(width = 4,
                               radioButtons("quote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"')
                        )
                    )
                ),
        #INTEGRATE DATA
                box(title = "Data Integration", width = NULL, collapsible = TRUE, collapsed = TRUE, class = "mainBox",
                    p(
                        class = "text-muted",
                        paste("Select the file you want to work with...")
                    ),
                    uiOutput("existingfiles0_1"),
                    uiOutput("existingfiles0_2"),
                    box(title = "Merging Options", width = NULL, collapsible = TRUE, collapsed = TRUE,
                        column(width = 6,
                               uiOutput("selectionPanel1")
                        ),
                        column(width = 6,
                               uiOutput("selectionPanel2")
                        ),
                        column(width = 12,
                            selectInput("mergin_type", "Merging Type",
                               choices = c(
                                   "Inner Join" = "ij", 
                                   "Outer Join" = "oj", 
                                   "Left Join" = "lj", 
                                   "Right Join" = "rj"
                               ),
                               selected = 'Inner Join'
                            )
                        )
                    ),
                    tags$div(style="display:inline-block; width:100%;",
                             actionButton("mergeButton", "Integrate Files",style="float:right;")
                    ),
                    p(
                        id="errorMerge",
                        style="display:none",
                        class = "text-muted",
                        paste("Error: select two different files to be merged")
                    )
                ),
        #DATASET CONSTRUCTION
                box(title = "Data Construction", width = NULL, collapsible = TRUE, collapsed = TRUE, class = "mainBox",
                    p(
                        class = "text-muted",
                        paste("Select the file you want to work with...")
                    ),
                    uiOutput("existingfiles1"),
                    tags$hr(),
                #FORMAT DATA
                    box(width = NULL, collapsible = TRUE,title = "Format the Data", collapsed = TRUE,
                        checkboxInput("toLower", "Format all in lower case", TRUE),
                        tags$div(style="display:inline-block; width:100%;",
                                 actionButton("formatData", "Format Data",style="float:right;")
                        )
                    ),
                #ENGINEER FEATURES
                    box(width = NULL, collapsible = TRUE,title = "Engineer Features", collapsed = TRUE,
                        p(
                            class = "text-muted",
                            paste("Join Two Columns")
                        ),
                        column(width = 12,
                               textInput("newVariable", "Name for the new column (No Spaces)", placeholder = "New_Variable")
                        ),
                        column(width = 6,
                            uiOutput("engineer_data_variables_1")
                        ),
                        column(width = 6,
                            uiOutput("engineer_data_variables_2")
                        ),
                        tags$div(style="display:inline-block; width:100%;",
                                 actionButton("joinColumns", "Join Columns",style="float:right;")
                        )
                    ),
                #FEATURE SELECTION
                    box(width = NULL, collapsible = TRUE,title = "Feature Selection", collapsed = TRUE,
                        column(width = 6,
                               p(
                                   class = "text-muted",
                                   paste("Select Horizontally")
                               ),
                               uiOutput("data_construction_data_variables_h")
                        ),
                        column(width = 6,
                               p(
                                   class = "text-muted",
                                   paste("Select Vertically")
                               ),
                               uiOutput("data_construction_data_variables_v"),
                               selectInput("filter_condition", "Filter condition",
                                           choices = c(
                                               "Is equal to" = 'eq', 
                                               "Start with" = 'st', 
                                               "Has" = 'has'
                                           ),
                                           selected = 'Is equal to'
                               ),
                               textInput("text_condition", "Comparison Argument")
                        ),
                        tags$div(style="display:inline-block; width:100%;",
                                 actionButton("selectFeaturesH", "Select Features Horizontally",style="float: left;"),
                                 actionButton("selectFeaturesV", "Select Features Vertically",style="float:right;")
                        )
                    ),
                #HANDLE MISSING DATA AND DUPLICATES
                    box(width = NULL, collapsible = TRUE,title = "Data Cleaning", collapsed = TRUE,
                        p(
                            class = "text-muted",
                            paste("Features Selection")
                        ),
                        column(width = 6,
                               checkboxInput("removeNA", "Remove Missing Values", TRUE),
                               checkboxInput("removeDuplicates", "Remove Duplicates", TRUE)
                        ),
                        column(width = 6
                               
                        ),
                        tags$div(style="display:inline-block; width:100%;",
                                 actionButton("cleanData", "Clean Data",style="float:right;")
                        )
                    )
                ),
                box(title = "Export Datasets", width = NULL, collapsible = TRUE, collapsed = TRUE, class = "mainBox",
                    p(
                        class = "text-muted",
                        paste("Select dataset you want to export in a CSV format...")
                    ),
                    uiOutput("existingfilesToExport"),
                    textInput("newFileName", "Insert the File Name", placeholder = "new_dataset"),
                    tags$div(style="display:inline-block; width:100%;",
                                actionButton("exportButton", "Export Dataset",style="float:right;")
                    )
                )
            ),
            column(width = 8 ,
            #DATA DESCRIPTION BOX
               box(title = "Data Description", width = "100%", collapsible = TRUE, class = "mainBox",
                   p(
                       class = "text-muted",
                       paste("Select the file you want to work with...")
                   ),
                   uiOutput("existingfiles3"),
                   
                   #DATASET PREVIEW
                   box(title = "Dataset Preview", width = "100%",collapsible = TRUE, collapsed = TRUE,
                       radioButtons("disp", "Display", choices = c('First 100' = "head", 'First 1000' = "all"), selected = "head"),

                       div(style = 'overflow-x: scroll; max-height: 300px;', 
                           p(
                               class = "text-muted",
                               paste("First 100 Rows of 1st File")
                           ),
                           tableOutput("datasetTable")
                       )
                   ),
                   #DATASET SUMMARY
                   box(title = "Dataset Summary", width = "100%",collapsible = TRUE, collapsed = TRUE,
                       div(style = 'overflow-x: scroll', 
                           p(
                               class = "text-muted",
                               paste("Basic Information About 1st File")
                           ),
                           verbatimTextOutput("summaryFile")
                       )
                   )
               ),
            #PLOTS BOX
                box(title = "Data Visualization", width = "100%", collapsible = TRUE, collapsed = TRUE, class = "mainBox",
                    column(width = 3,
                           #which file to handle
                           uiOutput("existingfiles2"),
                           #which plot to build
                           selectInput("plotType", "Plot Type",
                                       choices = c(
                                           "Histogram" = 1,
                                           "Matrix Scatter Plot" = 2,
                                           "Proportion of Categorical Variables" = 3,
                                           "Missing Data Visualization" = 4,
                                           "Missing Data Matrix Scatter Plot" = 5
                                       )
                           ),
                           column(width = 12,
                                  #x-axis
                                  uiOutput("var_x_axis")
                           ),
                           tags$div(style="display:inline-block; width:100%;",
                                    actionButton("plotData", "Plot Data",style="float:right;")
                           )
                    ),
                    column(width = 9,
                           plotOutput("plot")
                    )
                ),    
           #DATA TABLES BOX
                box(title = "Output Files", width = "100%",collapsible = TRUE, collapsed = TRUE, class = "mainBox",
                   tabBox(
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "tabset1", width = "100%",
                        tabPanel(
                           "Merged", 
                           div(style = 'overflow-x: scroll;', verbatimTextOutput("summary_merged")),
                           tags$hr(),
                           div(style = 'overflow-x: scroll', tableOutput("filetable"))
                        ),
                        tabPanel(
                           "Cleaned", 
                           div(style = 'overflow-x: scroll;', verbatimTextOutput("summary_cleaned")),
                           tags$hr(),
                           div(style = 'overflow-x: scroll; max-height: 300px;',
                               tableOutput("filetable_cleaned")
                           )
                        )
                    )
                )
            )
        ),
    #GUIDELINE
        tabItem(
            tabName = "guideline",
            style = "overflow-y: auto;",
        #MAM - Overall View
            h1(
                paste("Meta-Algorithmic Model")
            ),
            p(
                class = "text-muted",
                paste("Knowledge Discovery Phases and Main Activities for Domain Experts")
            ),
            imageOutput("mam0", inline = TRUE),
            tags$hr(),
        #MAM - Data Environment Understanding
            h4(
                paste("Process Deliverable Diagram - PDD")
            ),
            imageOutput("mam1", inline = TRUE),
        #MAM - Data Collection
            p(
                class = "text-muted",
                paste("Indentify Merging Criteria")
            ),
            imageOutput("mam2", inline = TRUE),
            tags$hr(),
        #MAM - Data Integration
            p(
                class = "text-muted",
                paste("Execute Merging")
            ),
            imageOutput("mam3", inline = TRUE),
            tags$hr(),
        #MAM - Data Construction
            p(
                class = "text-muted",
                paste("Handle Missing Data")
            ),
            imageOutput("mam4", inline = TRUE)
        )
    )
)


dashboardPage(
    skin = "black",
    header,
    sidebar,
    body
)
