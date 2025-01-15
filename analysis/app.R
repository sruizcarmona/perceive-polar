#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(shinythemes)


######## PERCEIVE LIBRARIES
library(tidyr)
library(dplyr)
library(stringr)
library(xml2)
library(lutz)
library(lubridate)
library(openxlsx)

######## PERCEIVE FUNCTIONS
## read TCX format
source('src/readTCX.R')
# supporting functions
source('src/supporting.R')
# get list of files for each participant
source('src/getFilesZipByID.R')
# get activity information
source('src/getActivityInfo.R')
# process activity
source('src/processActivity.R')
# process results
source('src/summariseResults.R')
##############################


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),

    # Application title
    titlePanel("PERCEIVE x Polar"),
    fluidRow(
      column(6, offset = 0,
             p("This Shiny App is designed to help analyse the Polar sessions for the PERCEIVE study."),
             p("It requires 2 input files, a csv file with the IDs and maximum HR for the selected participants (follow structure from provided example CSV) 
               and a zipped folder with the corresponding TCX files."
             )
      )
    ),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("csvinput", "Choose CSV File",
                      multiple = F,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # add button to download sample csv
            downloadButton("example", "   Get example CSV",
                           icon = icon("file-export"),
                           style="color: #000000; background-color: #bce8ff; border-color: #98b8db"),
            tags$br(),
            tags$br(),
            fileInput("zipfile", "Choose ZIP File",
                      multiple = F,
                      accept = c(".zip")),
            tags$br(),
            tags$br(),
            checkboxInput("etrimp_addon", "Add eTRIMPs with 5-min increments", FALSE),
            actionButton("process", "   Process uploaded data",
                         icon = icon("play", class = "fa-fw"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            tags$br(),
            tags$br(),
            tags$hr(),
            uiOutput("downloadData")
            # downloadButton("download", "Download output",
            #              icon = icon("download"),
            #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        # icon = icon("spinner", class = 'fa-spin'))
    ),

        # Show a plot of the generated distribution
        mainPanel(
          # Output: Data file ----
          tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # increase upload limit to 300MB
  options(shiny.maxRequestSize=300*1024^2)
  
  # download example csv
  output$example <- downloadHandler(
    filename = function() {
      "example.csv"
    },
    content = function(file) {
      file.copy("data/sample.csv", file)
    },
    contentType = "text/csv"
  )
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$csvinput, input$zipfile)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        pop <- read.csv(input$csvinput$datapath, header = T, stringsAsFactors = F)
        # pop <- rbind(pop, data.frame(id = "test", maxhr = 100))
        files_in_zip <- get_list_files(zipfile = input$zipfile$datapath)
        files_by_id <- files_in_zip %>% separate(Name, into = c("p", "id", "file"), sep = "/") %>% count(id)
        df <- left_join(pop, files_by_id, by = 'id') %>% 
          # mutate_if(is.numeric, coalesce, 0) %>% 
          # mutate(across(where(is.numeric), coalesce, 0)) %>% 
          mutate(across(where(is.numeric), \(x) coalesce(x, 0))) %>% 
          mutate(`Participant ID` = id,
                 `TCX files in ZIP` = as.integer(n)) %>%
          select(`Participant ID`, `TCX files in ZIP`) %>% 
          arrange(`Participant ID`)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
      return(df)
  })

  ### MAIN RUN
  # define reactive value
  perceive_all <- reactiveValues(
    df = NULL
  )
  observeEvent(input$process, {
      csvinput <- input$csvinput$datapath
      zipfile <- input$zipfile$datapath
      etrimp_addon <- input$etrimp_addon
      input <- read.csv(csvinput, header=T, stringsAsFactors = F) %>% arrange(id)
      totalids <- nrow(input)
      # add progress bar
      withProgress(message = 'Processing participants...', value = 0, {
        # loop through athletes
        for (i in 1:totalids) {
          # move progress bar
          incProgress(1/totalids, detail = paste0(i, '/', totalids))
          # read id and maxhr
          id <- input$id[i]
          maxhr <- input$maxhr[i]
          # print(id)
          # print(maxhr)
          # read files
          myfiles <- get_files_id(id = id, zipfile = zipfile)
          # print(myfiles)
          # add file progress bar
          withProgress(message = '', value = 0, {
            for (file in myfiles) {
              # print(file)
              if(is.na(file)) {next}
              # print('file')
              incProgress(1/length(myfiles), detail = paste0('File ', which (myfiles == file), '/', length(myfiles)))
              polar_act <- process_polarfile(filename = file, zipfile = zipfile, id = id, maxhr = maxhr, etrimp_addon = etrimp_addon)
              # print(polar_act)
              perceive_all$df <- plyr::rbind.fill(perceive_all$df, polar_act)
              # perceive_all <- rbind(perceive_all, polar_act)
              # perceive_all <- do.call(rbind, list(perceive_all, polar_act))
            }
           })
          # print(dim(perceive_all$df))
          # print(perceive_all$df)
        }
      })
      #final perceive_all process!!!
      # perceive_info <<- perceive_all
      # process results
      results <- sum_results(perceive_all$df)
      # print(results)
      perceive_info <<- results$perceive_info
      all.activities <<- results$all.activities
      error.sessions <<- results$error.sessions
  })
  
  # make download button appear after uploading files and running process
  output$downloadData <- renderUI({
    req(input$csvinput, input$zipfile,
        input$process
        )
      downloadButton("download", "Download output",
                     icon = icon("download"),
                     style="color: #fff; background-color: #b71234; border-color: #b71234")
  })
  # download button
  output$download <- downloadHandler(
    filename = function() {
      paste0("perceive_results_", format(as.Date(Sys.Date()), format = "%y%m%d"),".xlsx")
      # paste("perceive_all", ".xlsx", sep = "")
    },
    content = function(file) {
      # save all in an XLSX file
      sheet_list <- list("participants" = perceive_info,
                         "activities" = all.activities,
                         "errors" = error.sessions
                         )
      openxlsx::write.xlsx(sheet_list,
                           keepNA = FALSE,
                           file = file)
    },
    contentType = "application/xlsx"
  )
  #############
  ## download button TEST
  # output$download <- downloadHandler(
  # 
  #   # This function returns a string which tells the client
  #   # browser what name to use when saving the file.
  #   filename = function() {
  #     paste('test', "rds", sep = ".") # example : iris.Rdata
  # 
  #   },
  # 
  #   # This function should write data to a file given to it by
  #   # the argument 'file'.
  #   content = function(file) {
  #     saveRDS(perceive_all$df, file)
  #   }
  # )
  #############
}

# Run the application 
shinyApp(ui = ui, server = server)
