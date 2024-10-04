library(readxl)
library(shiny)
library(bslib)
library(writexl)
library(DT)
library(shinyjs)
library(ggplot2)
library(plotly)

# shiny plots module --------------------------------------------------
mod_plot_ui <- function(id) {
  ns <- NS(id) 
  
  tagList( 
    fluidPage(
      titlePanel("App-1"), 
      theme = "paper",
      
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
          checkboxInput(ns("show_bar"), "Show Bar Graphs", value = FALSE),
          checkboxInput(ns("show_dist"), "Show Distribution Plots",
                        value = FALSE),
          numericInput(ns("density_points"), 
                       "Number of Points for Density Plot", 
                       value = 100),
          selectInput(ns("density_dist"), "Select Distribution Type",
                      choices = c("norm", "unif", "exp")),
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Data",
                     downloadLink(ns("downloadData"), "Download"),
                     DT::dataTableOutput(ns("dataTable"))
            ),
            
            tabPanel("Plot",
                     br(),
                     tableOutput(ns("info")),
                     plotlyOutput(ns("plot")),
                     br(),
                     plotlyOutput(ns("barPlot")), 
                     br(),
                     plotlyOutput(ns("densityPlot"))
            )
          )       
        )
      )
    )
  ) 
}

# Server ------------------------------------------------------------------
mod_plot_server <- function(id) {
  
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
          DT::datatable(data.frame(Message = "Unsupported file type. 
                                   Please upload a .csv, .xls, or .xlsx file."),
                        options = list(pageLength = 10),
                        rownames = FALSE)
        })
        return()  
      }
      
# Naming Convetion      
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

    # Main Plot Output
    output$plot <- renderPlotly({
      req(dataset())
      req(input$x_var, input$y_var)
      
      data <- dataset()
      
      # Treat X variable as categorical if selected
      if (is.numeric(data[[input$x_var]]) && input$treat_x_as_categorical) {
        data[[input$x_var]] <- as.factor(data[[input$x_var]])
      }
      
      # Check if X is categorical or numeric
      is_x_categorical <- is.factor(data[[input$x_var]]) || 
        is.character(data[[input$x_var]])
      is_x_numeric <- is.numeric(data[[input$x_var]])
      is_y_numeric <- is.numeric(data[[input$y_var]])
      
      # Create the Plotly plot
      if (is_x_categorical && is_y_numeric) {
        # Boxplot for categorical X variable
        fig <- plot_ly(data, 
                       x = ~get(input$x_var), 
                       y = ~get(input$y_var), 
                       type = 'box', 
                       marker = list(color = 'green')) %>%
          layout(title = paste(input$y_var, "by", input$x_var),
                 xaxis = list(title = input$x_var),
                 yaxis = list(title = input$y_var),
                 plot_bgcolor = '#e5ecf6')
      } else if (is_x_numeric && is_y_numeric) {
        # Scatter plot for numeric X and Y variables
        fig <- plot_ly(data, 
                       x = ~get(input$x_var), 
                       y = ~get(input$y_var), 
                       mode = 'markers', 
                       marker = list(color = 'blue', size = 5)) %>%
          layout(title = paste(input$y_var, "vs.", input$x_var),
                 xaxis = list(title = input$x_var),
                 yaxis = list(title = input$y_var),
                 plot_bgcolor = '#e5ecf6')
      } else {
        # If the conditions for plotting are not met, return NULL
        return(NULL)
      }
      
      fig
    })
    
    # Bar Plot
    output$barPlot <- renderPlotly({
      req(dataset())
      req(input$x_var)
      
      if (!input$show_bar) {
        return(NULL)  # Do not render if the checkbox is unchecked
      }
      
      data <- dataset()
      
      if (is.numeric(data[[input$x_var]])) {
        # For numeric x-axis, aggregate and plot
        bar_data <- aggregate(data[[input$y_var]], list(data[[input$x_var]]), 
                              FUN = length)
        colnames(bar_data) <- c(input$x_var, "Count")
      } else {
        # For categorical x-axis, use table function
        bar_data <- as.data.frame(table(data[[input$x_var]]))
        colnames(bar_data) <- c(input$x_var, "Count")
      }
      
      # Create Plotly bar plot
      fig <- plot_ly(data = bar_data, 
                     x = ~get(input$x_var), 
                     y = ~Count, 
                     type = 'bar', 
                     marker = list(color = 'purple')) %>%
        layout(title = paste("Bar Plot of", input$x_var),
               plot_bgcolor = '#e5ecf6', 
               xaxis = list( 
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = '#ffff'), 
               yaxis = list( 
                 zerolinecolor = '#ffff', 
                 zerolinewidth = 2, 
                 gridcolor = '#ffff'))
      fig
    })
    
    # Density Plot
    output$densityPlot <- renderPlotly({
      req(input$show_dist)  # Don't plot if checkbox is not checked
      req(dataset())  
      
      n <- input$density_points
      dist_type <- input$density_dist
      
      if (dist_type == "norm") {
        data <- rnorm(n)
      } else if (dist_type == "unif") {
        data <- runif(n)
      } else if (dist_type == "exp") {
        data <- rexp(n)
      }
      
      # Calculate the density
      d <- density(data)
      
      # Plot the density line
      fig <- plot_ly(x = d$x, y = d$y, type = 'scatter', mode = 'lines', 
                     fill = 'tozeroy', 
                     fillcolor = 'rgba(255, 192, 203, 0.5)', 
                     line = list(color = 'black', width = 2)) %>%
        layout(title = paste("Density Plot of", dist_type, "Distribution"),
               xaxis = list(title = "Value"),
               yaxis = list(title = "Density"),
               plot_bgcolor = '#e5ecf6',
               xaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, 
                            gridcolor = '#ffff'),
               yaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, 
                            gridcolor = '#ffff'))
      
      fig
    })
    
  })
}

# Define Shiny Plot

ui <- mod_plot_ui("app")
server <- function(input, output, session) {
  mod_plot_server("app")
}

# Run the app
shinyApp(ui = ui, server = server)
