shinyServer(function(input, output, session) {

#COMMON FUNCTIONS
    findExistingFiles <- function(){
        filesList = list()
        df1 <- filedata1()
        df2 <- filedata2()
        if(!is.null(df1)) filesList[rv$fileNames$file1] <- TRUE
        if(!is.null(df2)) filesList[rv$fileNames$file2] <- TRUE
        if(!is.null(rv$final_df)) filesList$Merged_File <- TRUE
        if(!is.null(rv$cleaned_data)) filesList$Constructed_File <- TRUE
        
        return(filesList)
    }
    
    selectCorrectFile <- function(x) {
        if(is.null(x)) return(NULL)
        
        if(length(rv$fileNames$file1)>0){
            if(x == rv$fileNames$file1) dat <- rv$dfA
        }
        if(length(rv$fileNames$file2)>0){
            if(x == rv$fileNames$file2) dat <- rv$dfB
        }
        if(x == "Merged_File"){
            dat <- rv$final_df
        }
        if(x == "Constructed_File"){
            dat <- rv$cleaned_data
        }
        return(dat)
    }
    
#LOADING BOX
        #increasing the size of maximum file upload in Shiny
    options(shiny.maxRequestSize = 150*1024^2)
    
    rv <- reactiveValues()
    
    #load first file - CSV
    filedata1 <- reactive({
        infile <- input$datafile1
        rv$fileNames$file1 <- gsub("\\..*","",infile$name)
        if (!is.null(infile))
            if(input$missingText){
                rv$dfA <- read.csv(infile$datapath, 
                                   header = input$header, 
                                   sep = input$sep, 
                                   quote = input$quote, 
                                   fileEncoding = "latin1",
                                   na.strings=c("","NA"))
            }else{
                rv$dfA <- read.csv(infile$datapath, 
                                   header = input$header, 
                                   sep = input$sep, 
                                   quote = input$quote, 
                                   fileEncoding = "latin1")
            }
            
    })
    
    #load second file - CSV
    filedata2 <- reactive({
        infile <- input$datafile2
        rv$fileNames$file2 <- gsub("\\..*","",infile$name)
        if (!is.null(infile))
            if(input$missingText){
                rv$dfB <- read.csv(infile$datapath, 
                               header = input$header, 
                               sep = input$sep, 
                               quote = input$quote, 
                               fileEncoding = "latin1",
                               na.strings=c("","NA"))
            }else{
                rv$dfB <- read.csv(infile$datapath, 
                                   header = input$header, 
                                   sep = input$sep, 
                                   quote = input$quote, 
                                   fileEncoding = "latin1")
            }
    })

#MERGING BOX
    output$existingfiles0_1 <- renderUI({
        files <- findExistingFiles()
        if(length(files)==0) return(NULL)
        
        selectInput("mergingFile1", "Select File 1:", 
                    as.list(names(files))
        )
    })
    
    output$existingfiles0_2 <- renderUI({
        files <- findExistingFiles()
        if(length(files)==0) return(NULL)
        
        selectInput("mergingFile2", "Select File 2:", 
                    as.list(names(files))
        )
    })
    
    #when first file is uploaded it creates a list of radio buttons with its columns
    output$selectionPanel1 <- renderUI({
        df <- selectCorrectFile(input$mergingFile1)
        if (is.null(df)) return(NULL)
        
        items = c(seq(length(colnames(df))))
        names(items) = colnames(df)
        
        selectInput("mergingCriteria1", "Merging Criteria - File 1", as.list(names(items)))
    })
    
    #when second file is uploaded it creates a list of radio buttons with its columns
    output$selectionPanel2 <- renderUI({
        df <- selectCorrectFile(input$mergingFile2)
        if (is.null(df)) return(NULL)
        
        items = c(seq(length(colnames(df))))
        names(items) = colnames(df)
        
        selectInput("mergingCriteria2", "Merging Criteria - File 2", as.list(names(items)))
    })
    
    #event function for when button merged is pressed
    final_file <- observeEvent(input$mergeButton, {
        dfA <- selectCorrectFile(input$mergingFile1)
        dfB <- selectCorrectFile(input$mergingFile2)
        
        byx <- input$mergingCriteria1
        byy <- input$mergingCriteria2
        
        shinyjs::hide("errorMerge")
        switch(input$mergin_type,
               ij={
                   #inner join
                   rv$final_df <- merge(x = dfA, y = dfB, by.x = byx, by.y = byy)
               },
               lj={
                   #left join
                   rv$final_df <-merge(x = dfA, y = dfB, by.x = byx, by.y = byy, all.x = TRUE)
               },
               rj={
                   #right join
                   rv$final_df <-merge(x = dfA, y = dfB, by.x = byx, by.y = byy, all.y = TRUE)
               },
               oj={
                   #outer join
                   rv$final_df <-merge(x = dfA, y = dfB, by.x = byx, by.y = byy, all = TRUE)
               },
               {
                   #default
                   rv$final_df <-merge(x = dfA, y = dfB, by.x = byx, by.y = byy)
               }
        )
    })
    
#CONSTRUCTION BOX
    output$existingfiles1 <- renderUI({
        files <- findExistingFiles()
        if(length(files)==0) return(NULL)
        
        radioButtons("datasets", "Select Value:", 
                     choices = names(files),
                     selected = NULL)
    })
    
    #select features - variables
    output$data_construction_data_variables_h <- renderUI({
        dat <- selectCorrectFile(input$datasets)
        
        items = c(seq(length(colnames(dat))))
        names(items) = colnames(dat)
        
        checkboxGroupInput("columns_h", "Choose Variables to Remove:", 
                           choices  = colnames(dat),
                           selected = NULL)
    })
    
    #select features - records
    output$data_construction_data_variables_v <- renderUI({
        dat <- selectCorrectFile(input$datasets)
        
        items = c(seq(length(colnames(dat))))
        names(items) = colnames(dat)
        
        selectInput("columns_v", "Select Variable", 
                           choices  = colnames(dat))
    })
    
    #FORMAT DATA BUTTON
    format_data <- observeEvent(input$formatData, {
        rv$cleaned_data <- selectCorrectFile(input$datasets)
        if(is.null(rv$cleaned_data)) return(NULL)
        
        if(input$toLower)
            rv$cleaned_data <- mutate_all(rv$cleaned_data, .funs=tolower)
    })
    
    #FEATURE SELECTION BUTTON
    feature_selection <- observeEvent(input$selectFeaturesH, {
        rv$cleaned_data <- selectCorrectFile(input$datasets)
        if(is.null(rv$cleaned_data)) return(NULL)
        
        #remove columns selected
        if(length(input$columns_h)>0){
            cols <- input$columns_h
            rv$cleaned_data <- rv$cleaned_data[, !(names(rv$cleaned_data) %in% cols)] 
        }
    })
    
    feature_selection <- observeEvent(input$selectFeaturesV, {
        if(!is.null(input$columns_v) & !is.null(input$text_condition)){
            switch(input$filter_condition,
                   'eq'={
                       rv$cleaned_data <- filter(rv$cleaned_data, rv$cleaned_data[input$columns_v] == input$text_condition)
                   },
                   'st'={
                       rv$cleaned_data <-  rv$cleaned_data[grepl(paste0("^", input$text_condition, ".*"),
                                                                 rv$cleaned_data[[input$columns_v]]),]
                   },
                   'has'={
                       rv$cleaned_data <- rv$cleaned_data[grepl(paste0(".*", input$text_condition, ".*"),
                                                                rv$cleaned_data[[input$columns_v]]),]
                   }
            )
        }
    })
    
    #ENGINEER FEATURES
    output$engineer_data_variables_1 <- renderUI({
        dat <- selectCorrectFile(input$datasets)
        
        items = c(seq(length(colnames(dat))))
        names(items) = colnames(dat)
        
        selectInput("ef_column_1", "Column 1", as.list(names(items)))
    })
    
    output$engineer_data_variables_2 <- renderUI({
        dat <- selectCorrectFile(input$datasets)
        
        items = c(seq(length(colnames(dat))))
        names(items) = colnames(dat)
        
        selectInput("ef_column_2", "Column 2", as.list(names(items)))
    })
    
    joinColumns <- observeEvent(input$joinColumns, {
        rv$cleaned_data <- selectCorrectFile(input$datasets)
        if(is.null(rv$cleaned_data) | is.null(input$newVariable)) return(NULL)

        rv$cleaned_data[input$newVariable] <- paste(rv$cleaned_data[[input$ef_column_1]], rv$cleaned_data[[input$ef_column_2]], sep=" ")
    })
    
    #DATA CLEANING BUTTON
    data_problems <- observeEvent(input$cleanData, {
        rv$cleaned_data <- selectCorrectFile(input$datasets)
        if(is.null(rv$cleaned_data)) return(NULL)
        
        #remove NAs
        if(input$removeNA)
            rv$cleaned_data <- na.omit(rv$cleaned_data)
        #remove identical rows
        if(input$removeDuplicates)
            rv$cleaned_data <- unique(rv$cleaned_data)
        
    })
    
#EXPORT DATASETS BOX
    output$existingfilesToExport <- renderUI({
        files <- findExistingFiles()
        if(length(files)==0) return(NULL)
        
        radioButtons("datasetsToExport", "Select Value:", 
                     choices = names(files),
                     selected = NULL)
    })
    
    exportData <- observeEvent(input$exportButton, {
        export_dataset <- selectCorrectFile(input$datasetsToExport)
        if(is.null(export_dataset)) return(NULL)
        fileName <- paste(input$newFileName, ".csv", sep = "")
        
        write.csv(export_dataset, fileName)
    })
    
#DATA DESCRIPTION BOX    
    output$existingfiles3 <- renderUI({
        files <- findExistingFiles()
        if(length(files)==0) return(NULL)
        
        selectInput("dataToDescribe", "Select Dataset:", 
                    as.list(names(files))
        )
    })
    
    #display file 1 loaded
    output$datasetTable <- renderTable({
        df <- selectCorrectFile(input$dataToDescribe)
        if (is.null(df)) {
            return(NULL)
        }else{
            if(input$disp == "head") {
                return(head(df,100))
            }
            else {
                return(head(df,n=1000))
            }
        }
    })
    
    output$summaryFile <- renderPrint({
        df <- selectCorrectFile(input$dataToDescribe)
        if (is.null(df)) return("There is nothing to show you :(")
        summary(df)
    })
    
#DATA TABLE BOX
    #data description for file merged
    output$summary_merged <- renderPrint({
        if(is.null(rv$final_df)) return("There is nothing to show you :(")
        summary(rv$final_df)
    })
    
    #header of file merged
    output$filetable <- renderTable({
        if(is.null(rv$final_df)) return(NULL)
        head(rv$final_df)
    })
    
    #data description for file merged
    output$summary_cleaned <- renderPrint({
        if(is.null(rv$cleaned_data)) return("There is nothing to show you :(")
        summary(rv$cleaned_data)
    })
    
    #file merged
    output$filetable_cleaned <- renderTable({
        if(is.null(rv$cleaned_data)) return(NULL)
        head(rv$cleaned_data,1000)
    })
    
    #file aggregated
    output$filetable_aggregated <- renderTable({
        if(is.null(rv$aggregated_data)) return(NULL)
        head(rv$aggregated_data, 100)
    })
    
    
#PLOT BOX
    output$existingfiles2 <- renderUI({
        files <- findExistingFiles()
        if(length(files)==0) return(NULL)

        selectInput("dataToPlot", "Select Dataset:", 
                    as.list(names(files))
        )
    })
    
    output$var_x_axis <- renderUI({
        dat <- selectCorrectFile(input$dataToPlot)
        
        items = c(seq(length(colnames(dat))))
        names(items) = colnames(dat)
        
        checkboxGroupInput("x-axis-options", "Choose Variables to Use in the Graph", 
                           choices  = colnames(dat),
                           selected = NULL)
    })
    
    plot_data <- observeEvent(input$plotData, {
        dat <- selectCorrectFile(input$dataToPlot)
        if(is.null(dat)) return(NULL)
        rv$plot <- dat
    
    })
    
    output$plot <- renderPlot({
        if(!is.null(rv$plot)){
            if(input$plotType == 1) {
                hist(rv$plot[,c(input$`x-axis-options`)])
            }else if(input$plotType == 2) {
                trblack <-  rgb(0,0,0, alpha = 0.1)
                pairs(rv$plot[,c(input$`x-axis-options`)], col = trblack, panel = panel.smooth)
            }else if(input$plotType == 3){
                tab <- table(rv$plot[,c(input$`x-axis-options`)])
                par(mar = c(5.1, 20 , 4.1 ,2.1))
                barplot(tab, horiz = TRUE, las=2, col = "lightblue")
            }else if(input$plotType == 4) {
                aggr(rv$plot,numbers=TRUE, sortVars=TRUE, 
                     labels=names(rv$plot), cex.axis=.4, gap=2, 
                     ylab=c("Proportion of missingness","Missingness Pattern"))
            }else if(input$plotType == 5){
                if(!is.null(input$`x-axis-options`)){
                    scattmatrixMiss(rv$plot, interactive = F, plotvars = c(input$`x-axis-options`))
                }else{
                    scattmatrixMiss(rv$plot, interactive = F)
                }
            }
        }
        
    })
    
#GUIDELINE
    output$uulogo <- renderImage({
        list(src = "img/uulogo.png",
             contentType = 'image/png',
             width = 'auto',
             height = 'auto',
             alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$mam0 <- renderImage({
        list(src = "img/mam-0.png",
             contentType = 'image/png',
             width = 'auto',
             height = 'auto',
             alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$mam1 <- renderImage({
        list(src = "img/mam-1.png",
             contentType = 'image/png',
             width = 'auto',
             height = 'auto',
             alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$mam2 <- renderImage({
        list(src = "img/mam-2.png",
             contentType = 'image/png',
             width = 'auto',
             height = 'auto',
             alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$mam3 <- renderImage({
        list(src = "img/mam-3.png",
             contentType = 'image/png',
             width = 'auto',
             height = 'auto',
             alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$mam3_1 <- renderImage({
        list(src = "img/mam-3-1.png",
             contentType = 'image/png',
             width = 'auto',
             height = 'auto',
             alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$mam3_2 <- renderImage({
        list(src = "img/mam-3-2.png",
             contentType = 'image/png',
             width = 'auto',
             height = 'auto',
             alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$mam4 <- renderImage({
        list(src = "img/mam-4.png",
             contentType = 'image/png',
             width = 'auto',
             height = 'auto',
             alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$mam4_1 <- renderImage({
        list(src = "img/mam-4-1.png",
             contentType = 'image/png',
             width = 'auto',
             height = 'auto',
             alt = "This is alternate text"
        )
    }, deleteFile = FALSE)

})