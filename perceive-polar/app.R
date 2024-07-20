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
##############################


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),

    # Application title
    titlePanel("PERCEIVE x Polar"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("csvinput", "Choose CSV File",
                      multiple = F,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            fileInput("zipfile", "Choose ZIP File",
                      multiple = F,
                      accept = c(".zip")),
            actionButton("process", "   Process uploaded data",
                         icon = icon("play", class = "fa-fw"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
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
        files_in_zip <- unzip(input$zipfile$datapath, list = TRUE) %>% filter(Length > 0)
        files_by_id <- files_in_zip %>% separate(Name, into = c("id", "file"), sep = "/") %>% count(id)
        df <- left_join(pop, files_by_id, by = 'id') %>% 
          # mutate_if(is.numeric, coalesce, 0) %>% 
          # mutate(across(where(is.numeric), coalesce, 0)) %>% 
          mutate(across(where(is.numeric), \(x) coalesce(x, 0))) %>% 
          mutate(`Participant ID` = id,
                 `TCX files in ZIP` = as.integer(n)) %>%
          select(`Participant ID`, `TCX files in ZIP`)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
      return(df)
  })

  ### MAIN RUN
  observeEvent(input$process, {
      perceive_all <- NULL
      csvinput <- input$csvinput$datapath
      zipfile <- input$zipfile$datapath
      input <- read.csv(csvinput, header=T, stringsAsFactors = F)
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
          myfiles <- get_list_files(id = id, zipfile = zipfile)
          # print(myfiles)
          # add file progress bar
          withProgress(message = '', value = 0, {
            for (file in myfiles) {
              # print(file)
              incProgress(1/length(myfiles), detail = paste0('File ', which (myfiles == file), '/', length(myfiles)))
              polar_act <- process_polarfile(filename = file, zipfile = zipfile, id = id, maxhr = maxhr)
              # print(polar_act)
              perceive_all <- rbind(perceive_all, polar_act)
            }
          })
        }
      })
      #final perceive_all process!!!
      perceive <<- perceive_all
  })
  
  # make download button appear after uploading files and running process
  output$downloadData <- renderUI({
    req(input$csvinput, input$zipfile,
        input$process
        )
      downloadButton("download", "Download output",
                     icon = icon("download"),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  })
  # download button
  output$download <- downloadHandler(
    filename = function() {
      paste("perceive_all", ".xlsx", sep = "")
    },
    content = function(file) {
      # sheet_list <- list("participants" = id.info,
      #                    "activities" = all.activities,
      #                    "errors" = error.sessions,
      #                    "duplicates" = dup.sessions)
      sheet_list <- list("test" = perceive)
      openxlsx::write.xlsx(sheet_list,
                           keepNA = TRUE,
                           file = file)
    },
    contentType = "application/xlsx"
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
