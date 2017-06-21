#############################################################################################################################
# This version features:
# 1. uses rpivotTable package to do generate pivot table, slice data in the table and generate charts for the table
# 2. download summary data to csv file ( this NOT working on shiny.io error:cannot find x-path element!)
# 3. filter data by date columns
# 
# ----- sharing the app -----
# Github repo: 
# How to setup: https://help.github.com/articles/adding-an-existing-project-to-github-using-the-command-line/
# Quick Reference: https://readwrite.com/2013/09/30/understanding-github-a-journey-for-beginners-part-1/
# https://github.com/catelinn/a-simple-pivot-data-app
#
# for web user to download and test the app in their computer (requirements: R/RStudio IDE, shiny package)
# the file is saved on https://gist.github.com (my Github account)
# runGist("edaa916edf572b3a9db331ab6ac7ce74")  
#############################################################################################################################

# PENDING - validate input$daterange(i) - no dates can be selected outside of the range 
# PENDING - checkbox to choose show date columns data as date, month or year 
# PENDING - retest rvest() for rpivotTable data on Shiny.io when solution is found! (convert html table to data frame)
# PENDING - dashboard for the app! (read the javascript based widget tutorial for further study)


#############################################################
#  Key References
#############################################################
# shiny function reference
# https://shiny.rstudio.com/reference/shiny/latest/

# create loop for multiple inputs 
# (In this App, create multiple date range ui depending on number of date columns selected)
# https://stackoverflow.com/questions/42169380/shiny-renderui-with-multiple-inputs


# endoding issue for Chinese character used on Winrundows:
# https://github.com/vnijs/radiant/issues/25


# use rio package to do file import/export regardless of type:
# https://cran.r-project.org/web/packages/rio/README.html


# if problem with package versions:
# remove.packages() and then install.packages(p, dependencies = TRUE)

# verify if a column can be converted to date
# https://stackoverflow.com/questions/18178451/is-there-a-way-to-check-if-a-column-is-a-date-in-r

# CSS selector reference (how to remove uis under daterangescontrol div?)
# https://www.w3schools.com/cssref/css_selectors.asp

# for loop / lapply for observeEvent in Shiny:
# https://stackoverflow.com/questions/40038749/r-shiny-how-to-write-loop-for-observeevent

# how to delete delements in a list:
# https://stackoverflow.com/questions/652136/how-can-i-remove-an-element-from-a-list

# share the app and allow runURL, runGist, runGitHub:
# https://shiny.rstudio.com/reference/shiny/latest/runUrl.html
# https://shiny.rstudio.com/tutorial/lesson7/


library("shiny")
library("shinythemes") # choose shiny ui themes
library("DT") # Datatable 
library("rsconnect") # deploy to shinyapps.io
library("shinyjs") # use toggle button from shinyJS pacakage
library("readxl") # load excel file (http://readxl.tidyverse.org/) 
library("data.table") # setattr() to override data table
library("rpivotTable")
library("stats")
library("zoo") # to use as.Date() on numeric value

# the following is for probable success of running rvest functions on shiny.io, which turned out negative. 
# why rvest (to turn html table to data frame so I can download it to a file) cannot work properly on shiny.io, it's pending for resolution
library("rvest") 
library("magrittr")
library("htmlwidgets")
library("htmltools")

# set global options for shiny app
options(
  # show shiny error debug tool in RStudio
  shiny.error = browser,
  #print messages sent between the R server and the web browser client to the R console (for debugging)
  shiny.trace = TRUE,
  # show shiny reactivity logs by pressing Ctrl + F3:
  shiny.reactlog=TRUE
)


ui = fluidPage(
  
  theme = shinytheme("cerulean"),
  
  navbarPage("myApp",
             
             tabPanel("Data", 
                      
                      fluidPage(
                        fluidRow(
                          column(4,
                                 # file upload div
                                 fileInput("file", "Choose a file (text/csv or excel)",
                                           # limit the file extention to the following:
                                           accept=c(
                                             "text/csv", 
                                             "text/comma-separated-values,text/plain", 
                                             ".csv",
                                             ".xls",
                                             ".xlsx",
                                             "application/vnd.ms-excel"))
                          ),
                          column(4, 
                                 # show ui for upload file control
                                 uiOutput("ui")
                          ),
                          column(4,
                                 # no choices before a file uploaded
                                 uiOutput("columnscontrol")
                          )
                          
                        ),
                        
                        hr(),
                        
                        fluidRow(
                          column(4,
                                 uiOutput("datecolscontrol")),
                          
                          column(6,
                                 uiOutput("daterangescontrol"))
                        ),
                        
                        hr(),
                        
                        dataTableOutput("datatbl")
                        
                        # print console for debugging (delete after completion)
                        #verbatimTextOutput("print_con")
                        
                      )),
             
             
             
             tabPanel("Pivot Table",     
                      
                      #DOWNLOAD DOESN'T WORK RIGHT NOW ON SHINY.IO as rvest not running correctly 
                      downloadButton("downloadData", "Download"),
                      
                      rpivotTableOutput("pivot")
                      
                      
             )
             
             
             # tabPanel("Summary Data",
             #          dataTableOutput("SummaryTable") 
             #          )
             
             
  )# end of navbarPage
)  #end of fluidPage (ui)



# server
server = function(input, output, session) {
  
  
  #########################################################
  #1.  upload & datatable output
  #########################################################
  
  # create dataset reactive objects
  dt <- reactiveValues()
  
  # reset all uis depending on the uploaded file
  observeEvent(input$file, {
    
    # reset inputs
    
    # reset ui outputs
    # reset("ui")
    # reset("columns")
    
    
    # reset reactive values
    dt$data = NULL
    dt$df = NULL
    dt$cols = NULL
    dt$rows = NULL
    dt$summary = NULL
    dt$colchoices = NULL
    dt$datecols = NULL
    if (length(dt$range) > 0) { mapply(function(i) dt$range[[i]] <- c(NULL, NULL), 1:length(dt$range)) }
    if (length(dt$range_check) > 0) { mapply(function(i) dt$range_check[i] <- NULL, 1:length(dt$range_check))}
    
    removeUI(selector = "div#columns_div")
    removeUI(selector = "div#datecols_div")
    
    # remove all <div> elements indside <div>#daterangescontrol:
    removeUI(selector = "div#daterangescontrol *")
  })
  
  
  # https://groups.google.com/forum/#!topic/shiny-discuss/Mj2KFfECBhU
  chooseFile <- eventReactive(input$file, {
    
    # input$file will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    file <- input$file
    
    # if the file uploaded
    if (!is.null(file)) {
      
      # determine file format and assign it chooseFile$suf
      pattern <- "\\.[[:alnum:]]*$"
      ### regexpr() returns integer of the starting position of the first match or -1 if there is none
      ### regmatches() extract the matched substrings from match data obtained by regexpr
      suf <- tolower(regmatches(file$name, regexpr(pattern, file$name)))
      
      # return objects in the list of chooseFile
      return(list(path = file$datapath, suf = suf))
      
    } else{
      # if no file uploaded, NULL
      return(NULL)
    }
    
  })
  
  
  # generate upload file control ui once file uploaded
  observe ({
    
    # file will get all the columns (path, suf) in chooseFile()
    file <- chooseFile()
    req(file)
    
    # render ui control for excel file uploaded
    if (file$suf %in% c(".xls", ".xlsx")) {
      # excel file control
      file.rename(file$path, paste0(file$path,file$suf))
      sheets <- excel_sheets(paste0(file$path,file$suf))
      output$ui <- renderUI({
        tagList(
          selectInput("sheet", label = "Select a sheet:", choices = sheets),
          textInput("arg", label = "Additional arguments:", value = " "),
          br(),
          actionButton("readF", "Update"))
      })
    }
    
    # render ui control for text/csv file uploaded
    if (file$suf %in% c(".txt", '.csv') ) {
      # text/csv file control
      output$ui <- renderUI({
        tagList(
          p("Import first row of data as header"),
          textInput("sep", label = "Seperator:", value = ","),
          textInput("quote", label = "Quote:", value = "\""),
          textInput("arg", label = "Additional arguments:", value = " "),
          br(),
          actionButton("readF", "Update"))
      })
    }
    
  })
  
  
  # read data to dt$data with arguments set above when action button pressed
  observeEvent (input$readF, {
    
    file <- chooseFile()
    
    # equivalent to if(!is.null(file)) - make sure file uploaded before the rest of codes run
    req(file)
    
    # pop data from excel sheet
    if (file$suf %in% c(".xls", ".xlsx")) {
      sheet <- input$sheet
      req(sheet)
      req(input$arg)
      if(input$arg %in% c(' ', '')){
        f <- read_excel(paste0(file$path,file$suf), sheet)
      } else {
        expr <- paste('read_excel(paste0(file$path,file$suf), sheet,', input$arg, ')', sep = '')
        print(expr)
        f <- eval(parse(text = expr))
      }
      
      # dropdown filter is cut off if columns are not of class factor or numeric/integer
      # also it's easier to convert factor to character for as.Date() later in this App (align with csv upload)
      # so convert non-number columns to factor:
      factor_cols <- !(lapply(f, class) == "numeric" | lapply(f, class) == "integer" )
      f[,factor_cols] <- lapply(f[,factor_cols], as.factor)
      dt$data <- f
    }
    
    # pop data from txt/csv file
    if (file$suf %in% c(".txt", '.csv') ) {
      req(input$arg)
      if(input$arg %in% c(' ', '')){
        f <- read.csv(file$path, 
                      header = TRUE, 
                      sep = input$sep,  
                      quote = input$quote)
        dt$data <- f
      } else {
        # datapath of the file (change "\\" to "/")
        expr1 <- paste("", gsub("\\", "/", file$path, fixed = TRUE), "", sep = "")
        
        # prepare the strings of arguments for read.csv()
        expr2 <- paste(expr1,
                       paste("header = input$header"),
                       paste("sep=", paste("", input$sep, "", sep = "")),
                       paste("quote=", paste("", input$quote, "", sep = "")), 
                       input$arg, sep = "")
        print(expr2)
        
        # combine read.csv() with arguments
        expr <- paste("read.csv2(',expr2,')", sep = "")
        print(expr)
        
        # parse and run the expr and get the dataset
        f <- eval(parse(text = expr))
        dt$data <- f
      }
    }
    
    # get the col names of the dataset and assign them to a list
    dt$colchoices <- mapply(list, names(dt$data))
    
    
    # update datecols choices with those columns can be converted to Date only:
    dt$date_ok = sapply(dt$data, function(x) !all(is.na(as.Date(as.character(x), format = "%Y-%m-%d"))))
    dt$datecolchoices = colnames(dt$data[dt$date_ok])
    
    
    # render columnscontrol with empty data
    output$columnscontrol <-  renderUI({
      # render column group checkbox ui after loading the data
      # tags#div has the advantage that you can give it an id to make it easier to reference or remove it later on
      tags$div(id = "columns_div", 
               checkboxGroupInput("columns", "", choices = NULL, selected = NULL))
    })
    
    # render datecolscontrol with empty data
    output$datecolscontrol <- renderUI({
      tags$div(id = "datecols_div",
               selectInput("datecols", "Filter data by dates):", choices = NULL, multiple = TRUE, selected = NULL))
    })
    
    
  }) # end of observeEvent(update button pressed})
  
  
  # convert these columns to Date in the dataset (require dt$datecolchoices, dt$date_ok)
  # this is called whenever dt$data is loaded, in case the new uploaded dataset has same dt$cols, dt$datecolchoices as the previous loaded dataset
  observeEvent(dt$data, {
    req(dt$datecolchoices)
    req(dt$date_ok)
    dt$data[dt$date_ok] = lapply(dt$data[dt$date_ok], function(x) as.Date(as.character(x)))
  })
  
  
  # update columns choices when dt$choices is ready (so that it's automatically updated, not depending on input$readF)
  observeEvent(dt$colchoices, {
    updateCheckboxGroupInput(session, "columns", "Select Columns:", choices = dt$colchoices, selected = dt$colchoices)
  })
  
  
  # the other reactivity on dt$cols is input$file (when new file uploaded, dt$data and dt$cols set to NULL)
  # so that the following line set apart the reactivity of input$columns on dt$cols
  observeEvent(input$columns, { 
    dt$cols <- input$columns
    #dt$df <- dt$data[dt$cols]
  }, ignoreNULL = FALSE)
  
  
  
  # on change of columns selected
  observeEvent(dt$cols, {
    
    # update dt$df (table data)
    dt$df = dt$data[dt$cols]
    
    # update input$datecols choices with available date columns
    choices = dt$datecolchoices[dt$datecolchoices %in% dt$cols]
    updateSelectInput(session, "datecols", "Filter data by dates:", choices = choices, selected = NULL)
    
  })
  
  tbldata <- reactive({
    
    req(dt$df)
    
    # table data with date range filters or not
    if ( length(input$datecols) > 0 ) {
      
      # to avoid error when dt$range_check is not loaded yet (otherwise, dt$filtered_ind * NULL result in error)
      # so make sure first range_check is available before the loop over the rest of range_check
      req(dt$range_check)
      dt$filtered_ind = as.integer(dt$range_check[[input$datecols[1]]])
      
      # loop over range_check after the first one
      lapply( input$datecols[-1],
              FUN = function(x) {
                dt$filtered_ind = as.integer(dt$range_check[[input$datecols[1]]]) * dt$range_check[[x]]
              }
              
      ) # end of lapply()
      
      dt$df[as.logical(dt$filtered_ind),]
    } 
    else { dt$df }
    
  })
  
  # render output$datatbl 
  
  output$datatbl <- DT::renderDataTable({
    
    D = tbldata()
    
    # which columns not for search in datatable filter
    dt$notforsearch = sapply(which(names(D) %in% dt$datecolchoices), function(x) x-1)
    
    # datatable
    datatable(D, rownames = FALSE,
              # column filter on the top
              filter = 'top',
              # autoWidth
              options = list(autoWidth = TRUE, 
                             # disable date columns for filter - not working yet (awaiting stackoverflow reply)
                             columnDefs = list(list( targets = dt$notforsearch, searchable = FALSE)))
    )
  })
  
  # set apart input$datecols and dt$datecols, so that when a new file uploaded, dt$datecols can be set to NULL right away
  observeEvent(input$datecols, {
    dt$datecols = input$datecols
  }, ignoreNULL = FALSE)
  
  
  # for all new file uploaded, generate daterange uis per selected input$datecols
  observeEvent(dt$datecols, {
    
    # render daterange ui(s) per selected datecols
    output$daterangescontrol <- renderUI({
      
      # when input$datecols is NULL, no daterangecontrol ui and reset all dt$range[[x]] to original min/max
      if (is.null(dt$datecols)) {
        return(NULL)
      }
      
      # otherwise
      else {
        
        D = dt$df
        
        # whenever input$datecols change, start/end of daterange ui refresh to original state
        output = tagList()
        
        for (x in dt$datecols) {
          i = which(dt$datecols == x)
          output[[i]]= tagList()
          output[[i]][[1]] = tags$div(id = paste("dateranges_div", i, sep = "_"),
                                      dateRangeInput(paste0("daterange_", x),
                                                     paste("Date range of", x),
                                                     start = min(D[[x]]),
                                                     end = max(D[[x]]))
          )
        }
        # return output tagList() with ui elements
        output
      } # end of else{}
    }) # end of renderUI
  }, ignoreNULL = FALSE) # end of observeEvent
  
  
  # here goes the codes for assigning input$daterange data to filter corresponding columns
  
  observe({
    # loop observeEvent on input$daterange1, input$daterange2... 
    lapply( input$datecols,
            
            FUN = function(x) {
              
              # on change of input$daterange_x
              observeEvent(input[[paste0("daterange_", x)]], {
                
                # update reactive values to test whether this loop is working
                dt$range[[x]] = input[[paste0("daterange_", x)]]
                
                # filter dataset with the corresonding date column:
                D = dt$df
                dt$range_check[[x]] = D[[x]] >=  dt$range[[x]][[1]] & D[[x]] <= dt$range[[x]][[2]]
                
                ##################################################################
                #  Validation not working, no error message (check stackoverflow)
                ##################################################################
                # shiny::validate(
                #   need( dt$range[[x]][[1]] >= min(dt$data[[x]]), "The start date cannot be earlier than the oldest date!"),
                #   need( dt$range[[x]][[2]] <= max(dt$data[[x]]), "The end date cannot be later than the latest date!")
                # )
                
              }) # end of observeEvent
            }
    ) # end of lapply()
    
  })
  
  
  # rows displayed in input$datatbl (the rendered data table)
  observeEvent( input$datatbl_rows_all, { 
    dt$rows <- input$datatbl_rows_all
  })
  
  
  #########################################################
  #2.  pivot table output
  #########################################################
  
  pivotdata <- reactive({tbldata()[dt$rows,]})
  
  # Whenever the config is refreshed, call back with the content of the table
  output$pivot <- renderRpivotTable({
    
    validate(need(pivotdata(), "Please load data first. "))
    
    rpivotTable(
      
      pivotdata(),
      # this line will pass input$pivot innerHTML to input$myData 
      # (this is to generate HTML table and pass the HTML to rvest::html_table to convert to data frame, which now doesn't work on shiny.io)
      onRefresh =
        htmlwidgets::JS("function(config) {
                        Shiny.onInputChange('myData', document.getElementById('pivot').innerHTML);
  }")
    )
})
  
  
  # Clean the html of input$myData (rvest package) and store as reactive - NOT WORKING on shiny.io
  observeEvent(input$myData,{
    
    # there are two tables in an rpivotTable, we want the second
    dt$summary = input$myData %>%
      read_html %>%
      html_table(fill = TRUE) %>%
      .[[2]]
  })
  
  
  # show df as DT::datatable
  # output$SummaryTable <- DT::renderDataTable({
  #   datatable(dt$summary, rownames = FALSE)
  # })
  
  observeEvent("downloadData", {
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(dt$summary, file)
      }
    )
    
  })
  
  
  
  #########################################################
  # 3. print console
  #########################################################
  # output$print_con <- renderPrint({
  #   
  #   #req(input$daterange1)
  #   #d = dt$df[dt$rows, dt$cols]
  #   #date_ok = sapply(d, function(x) !all(is.na(as.Date(as.character(x), format = "%Y-%m-%d"))))
  #   #cells = unlist(d[dt$datecols[[1]]], use.names = FALSE)
  #   #d[date_ok] = lapply(d[date_ok], function(x) as.Date(as.character(x)))
  #   list(str(dt$data), 
  #        #str(dt$df[dt$rows, dt$cols]),
  #       #as.Date(as.character(cells))
  #       #sum(unlist(dt$date_ok)),
  #       #lapply(d[date_ok], function(x) as.Date(as.character(x)))),
  #       #summary(dt$df),
  #       str(dt$df),
  #       #dt$date_ok,
  #       #dt$datecolchoices,
  #       paste("dt$notforsearch: ", paste(dt$notforsearch, collapse = ",")),
  #       paste("dt$filtered_ind: ", paste(dt$filtered_ind, collapse = ",")),
  #       paste("dt$range: ", list(dt$range)),
  #       paste("dt$range_check: ", list(dt$range_check)),
  #       paste("length of dt$range: ", length(dt$range)),
  #       paste("input$daterange_visitdates: ", input$daterange_visitdates),
  #       paste("dt$range_check[[1]] * dt$range_check[[2]]:", paste(dt$range_check[["visitdates"]] * dt$range_check[["editdates"]], collapse = ",")),
  #       identical(dt$filtered_ind, dt$range_check[["visitdates"]] * dt$range_check[["editdates"]]),
  #       paste("dt$range_start[['visitdates']]:", dt$range_start[["visitdates"]]),
  #       paste("dt$datecolchoices: ", paste(dt$datecolchoices, collapse = ",")),
  #       paste("dt$datecols: ", paste(dt$datecols, collapse = ","))
  #       #str(dt$df)
  #       #min(dt$range[[1]][[1]]) >= min(dt$df[[dt$datecols[[1]]]]),
  #       #max(dt$range[[1]][[2]]) <= max(dt$df[[dt$datecols[[1]]]]),
  #       #html_structure(read_html(input$myData))
  #   )
  #})
  
  } # end of shiny server function

shinyApp(ui = ui, server = server, options = options)

