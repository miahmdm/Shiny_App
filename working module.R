library(readxl)
library(shiny)
library(bslib)
library(writexl)
library(DT)
library(shinyjs)

# module practice 08/27/24 ------------------------------------------------
mod_app_ui <- function(id) {
  ns <- NS(id) 
  
 tagList( 
   fluidPage(
   titlePanel("App-1"), 
   
   sidebarLayout(
     sidebarPanel(
       h3("File input", style = "font-weight: normal;"),
       helpText("Upload a .csv, .xls, or .xlsx file."),
       fileInput(ns("file"), label = NULL),
       actionButton(ns("submitFile"), "Submit"),
       br(),  
       selectInput(ns("x_var"), "X-axis variable", choices = NULL),
       selectInput(ns("y_var"), "Y-axis variable", choices = NULL),
       checkboxInput(ns("treat_x_as_categorical"), 
                     "Treat X-axis variable as categorical", 
                     value = FALSE),
       downloadLink(ns("downloadData"), "Download")
     ),
     
     mainPanel(
       tabsetPanel(
         tabPanel("Data",
                  DT::dataTableOutput(ns("dataTable")),
                  
                  uiOutput(ns("fileContents"))
                  
         ),
         tabPanel("Plot",
                  plotOutput(ns("plot")) 
         ),
         tabPanel("Summary",
                  tableOutput(ns("summary"))  
      )
     )       
    )
   )
  )
 ) 
}

# Server ------------------------------------------------------------------
mod_app_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store dataset
    dataset <- reactiveVal(NULL)
    
    observeEvent(input$submitFile, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      
      data <- switch(ext,
                     "csv" = read.csv(input$file$datapath, header = TRUE),
                     "xls" = read_excel(input$file$datapath),
                     "xlsx" = read_excel(input$file$datapath),
                     NULL)
      
      if (is.null(data)) {
        output$dataTable <- DT::renderDataTable({
          DT::datatable(data.frame(Message = 
"Unsupported file type. Please upload a .csv, .xls, or .xlsx file."),
                              options = list(pageLength = 10),
                              rownames = FALSE)
          })
        return()  
      }

# Naming Convention -------------------------------------------------------

      names(data) <- gsub("^PID$", "pid", names(data))
      dataset(data)
      
      updateSelectInput(session, "x_var", choices = names(data))
      updateSelectInput(session, "y_var", choices = names(data))
      
      output$dataTable <- DT::renderDataTable({
        req(dataset())
        DT::datatable(dataset(), options = list(pageLength = 10),
                      rownames = FALSE)
      })
    })  
    

# Plot Output -------------------------------------------------------------

    output$plot <- renderPlot({
      req(dataset())
      req(input$x_var, input$y_var)
      
      data <- dataset()
      is_x_numeric <- is.numeric(data[[input$x_var]])
      treat_as_categorical <- input$treat_x_as_categorical
      
      if (is_x_numeric && treat_as_categorical) {
        data[[input$x_var]] <- as.factor(data[[input$x_var]])
      }
      
      is_x_categorical <- is.factor(data[[input$x_var]]) || 
        is.character(data[[input$x_var]])
      
      if (is_x_categorical) {
        boxplot(data[[input$y_var]] ~ data[[input$x_var]],
                main = paste(input$y_var, "by", input$x_var),
                xlab = input$x_var,
                ylab = input$y_var,
                col = "green")
      } else {
        plot(data[[input$x_var]], data[[input$y_var]], 
             main = paste(input$y_var, "vs.", input$x_var),
             xlab = input$x_var,
             ylab = input$y_var,
             pch = 19, col = "blue")
      }
    })

# Summary Stats Output ----------------------------------------------------

    summaryStats <- reactive({
      req(dataset())
      req(input$x_var, input$y_var)
      
      data <- dataset()
      
      if (!(input$x_var %in% names(data)) ||
          !(input$y_var %in% names(data))) {
        return(NULL)
      }
      
      x_stats <- c(summary(data[[input$x_var]]), 
                   std = sd(data[[input$x_var]], na.rm = TRUE))
      y_stats <- c(summary(data[[input$y_var]]), 
                   std = sd(data[[input$y_var]], na.rm = TRUE))
      
      summary_df <- data.frame(
        Statistic = names(x_stats),
        X_Variable = as.numeric(x_stats),
        Y_Variable = as.numeric(y_stats)
      )
      
      return(summary_df)
    })
    
    output$summary <- renderTable({
      summaryStats()
    })

# Download Data Server ----------------------------------------------------

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("dataset", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(dataset(), file, row.names = FALSE)  
      }
    )
  })
}

# Define Shiny App
ui <- mod_app_ui("app")
server <- function(input, output, session) {
  mod_app_server("app")
}

# Run the app
shinyApp(ui = ui, server = server)


