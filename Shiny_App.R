library(readxl)
library(shiny)
library(bslib)
library(writexl)
library(DT)
library(shinyjs)
library(ggplot2)
library(plotly)

# ui ----------------------------------------------------------------------
ui <- page_sidebar(
  titlePanel("App-1"),
  theme = "paper",
  sidebar = sidebar(
    title = "File input",
    helpText("Upload a .csv, .xls, or .xlsx file."),
    fileInput("file", label = NULL),
    actionButton("submitFile", "Submit")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X-axis variable", choices = NULL),
      selectInput("y_var", "Y-axis variable", choices = NULL),
      br(),
      sliderInput("height", "Height", min = 100, max = 500, value = 300),
      sliderInput("width", "Width", min = 100, max = 500, value = 300),
      br(),
      checkboxInput("treat_x_as_categorical", 
                    "Treat X-axis variable as categorical", 
                    value = FALSE),
      checkboxInput("show_bar", "Show Bar Graphs", value = FALSE),
      checkboxInput("show_dist", "Show Distribution Plots", value = FALSE),
      checkboxInput("analyze", "Perform ANOVA"),
      
      # Conditional panel for density output  
      conditionalPanel(
        condition = "input.show_dist == true",
        numericInput("density_points", "Number of Data Points", 
                     value = 100, min = 1),
        selectInput("density_dist", "Choose Distribution", 
                    choices = c("Normal" = "norm",
                                "Uniform" = "unif", 
                                "Exponential" = "exp"))
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Data", DT::dataTableOutput("data")),
        
        tabPanel("Plot",
                 br(),
                 tableOutput("info"),
                 plotlyOutput("plot"), 
                 br(),
                 plotlyOutput("barPlot"), 
                 br(),
                 plotlyOutput("densityPlot")
        ),
        
        tabPanel("Summary",
                 br(),
                 downloadLink("downloadResults", "Download"),
                 br(),
                 tableOutput("summary"),
                 br(),
                 uiOutput("anovaResults")
        )
      )
    )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  
  observeEvent(input$submitFile, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    data <- switch(ext,
                   csv = read.csv(input$file$datapath, header = TRUE),
                   xls = readxl::read_excel(input$file$datapath),
                   xlsx = readxl::read_excel(input$file$datapath),
                   NULL)
    
    if (is.null(data)) {
      showModal(modalDialog(
        title = "Error",
        "Unsupported file type. Please upload a .csv, .xls, or .xlsx file.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()  
    }
    
    # Naming Convention
    names(data) <- gsub("^PID$", "pid", names(data))
    dataset(data)
    
    # Update select inputs
    updateSelectInput(session, "x_var", choices = names(data))
    updateSelectInput(session, "y_var", choices = names(data))
  })
  
  # Data Table Output
  output$data <- DT::renderDataTable({
    req(dataset())
    DT::datatable(dataset(), options = list(pageLength = 10), rownames = FALSE)
  })
  
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

  # ANOVA results
  anova_results <- reactive({
    req(dataset())
    req(input$x_var, input$y_var)
    req(input$analyze)
    
    data <- dataset()
    factor_var <- as.factor(data[[input$x_var]])
    response_var <- data[[input$y_var]]
    
    anova_model <- aov(response_var ~ factor_var, data = data)
    anova_summary <- summary(anova_model)
    
    list(
      model = anova_model,
      summary = anova_summary
    )
  })
  
  output$anovaResults <- renderUI({
    if (input$analyze) {
      req(anova_results())
      results <- anova_results()$summary
      if (length(results) > 0) {
        verbatimTextOutput("anova_summary")
      }
    } else {
      NULL
    }
  })
  
  output$anova_summary <- renderPrint({
    req(anova_results())
    anova_results()$summary
  })
  
  # Summary stats
  summaryStats <- reactive({
    req(dataset())
    req(input$x_var, input$y_var)
    
    data <- dataset()
    
    if (!(input$x_var %in% names(data)) || !(input$y_var %in% names(data))) {
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
  
  # Download handlers
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("summary", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(summaryStats(), file, row.names = FALSE)
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataset", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataset(), file, row.names = FALSE)  
    }
  )
}

shinyApp(ui, server)