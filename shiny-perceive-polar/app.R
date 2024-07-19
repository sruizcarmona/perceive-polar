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
            actionButton("process", "Process uploaded data"),
            tags$hr(),
            actionButton("download", "Download output")
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
        files_in_zip <- unzip(input$zipfile$datapath, list = TRUE) %>% filter(Length > 0)
        files_by_id <- files_in_zip %>% separate(Name, into = c("id", "file"), sep = "/") %>% count(id)
        df <- left_join(pop, files_by_id, by = 'id')
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
      return(df)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
