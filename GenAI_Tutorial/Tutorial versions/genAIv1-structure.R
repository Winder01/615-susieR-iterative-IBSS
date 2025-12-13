library(shiny)
library(plotly)
library(DT)
library(susieR)
library(data.table)
library(matrixStats)
library(reshape2)

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  titlePanel("SuSiE Iterative Framework"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 # 1. Data Configuration
                 h4("1. Data Configuration"),
                 radioButtons("data_format", "Data Format:",
                              choices = c("Summary Statistics", "Individual Data"),
                              selected = "Individual Data"),
                 selectInput("data_source", "Data Source:",
                             choices = c("Simulated Data")),
                 selectInput("data_scenario", "Simulation Scenario:",
                             choices = c("Perfect Collinearity", "Moderate LD")),
                 
                 hr(),
                 
                 # 2. Execution
                 h4("2. Execution"),
                 actionButton("run_susie", "Run Analysis", 
                              class = "btn-primary btn-lg", width = "100%"),
                 br(), br(),
                 
                 # Viz Control Box
                 tags$div(
                   style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #ddd;",
                   h5("Visualization Control", style = "font-weight: bold;"),
                   sliderInput("iteration_step", "Iteration:",
                               min = 1, max = 1, value = 1, step = 1,
                               animate = animationOptions(interval = 600, loop = FALSE)),
                   actionButton("next_step", "Next Step", width = "100%", icon = icon("step-forward")),
                   br(), br(),
                   textOutput("iteration_info")
                 )
    ),
    
    mainPanel(width = 9,
              # Top Configuration Bar
              tags$div(
                style = "background-color: #e9ecef; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
                h4("Analysis Parameters"),
                fluidRow(
                  column(2, numericInput("param_L", "L (Effects)", value = 10, min = 1)),
                  column(2, numericInput("param_max_iter", "Max Iter", value = 100, min = 1)),
                  column(2, numericInput("param_tol", "Tol", value = 0.001)), # Replaced Lambda with Tol/standard param for clarity, or kept custom
                  column(3, numericInput("param_cs_cov", "CS Coverage", value = 0.95, step = 0.05)),
                  column(3, numericInput("param_prior_var", "Prior Var", value = 0.1, step = 0.05))
                )
              ),
              
              # Results Tabs
              tabsetPanel(
                tabPanel("PIP Plot", 
                         br(),
                         plotlyOutput("pip_plot", height = "500px")
                ),
                tabPanel("ELBO Convergence", 
                         br(),
                         plotOutput("elbo_plot")
                ),
                tabPanel("Credible Sets", 
                         br(),
                         verbatimTextOutput("cs_summary")
                ),
                tabPanel("Data Preview & Log", 
                         br(),
                         DTOutput("data_preview"),
                         verbatimTextOutput("log_output")
                )
              )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  # Reactive Values
  vals <- reactiveValues(
    susie_fit = NULL,        # The final susie object
    susie_trace = NULL,      # List of fits from track_fit=TRUE
    total_steps = 1,
    sim_data = NULL
  )
  
  # 1. Run Analysis Logic
  observeEvent(input$run_susie, {
    req(input$data_source == "Simulated Data")
    
    # Generate Dummy Data (N=100, P=50)
    set.seed(123)
    N <- 100
    P <- 50
    X <- matrix(rnorm(N * P), N, P)
    beta <- rep(0, P)
    beta[c(5, 15, 25)] <- 1 # True effects
    y <- X %*% beta + rnorm(N)
    
    vals$sim_data <- list(X=X, y=y)
    
    withProgress(message = 'Running SuSiE...', value = 0, {
      tryCatch({
        # Run SuSiE with track_fit = TRUE
        fit <- susie(X, y, 
                     L = input$param_L,
                     max_iter = input$param_max_iter,
                     estimate_residual_variance = TRUE, 
                     estimate_prior_variance = TRUE,
                     track_fit = TRUE)
        
        vals$susie_fit <- fit
        vals$susie_trace <- fit$trace
        vals$total_steps <- length(fit$trace)
        
        # Update Slider based on total iterations
        updateSliderInput(session, "iteration_step", 
                          max = vals$total_steps, 
                          value = vals$total_steps) 
        
        output$log_output <- renderText({
          paste("Analysis complete.", vals$total_steps, "iterations performed.")
        })
        
      }, error = function(e) {
        output$log_output <- renderText({ paste("Error:", e$message) })
      })
    })
  })
  
  # 2. Visualization Logic
  
  # Helper to increment slider
  observeEvent(input$next_step, {
    req(vals$total_steps)
    current <- input$iteration_step
    if(current < vals$total_steps) {
      updateSliderInput(session, "iteration_step", value = current + 1)
    }
  })
  
  # Iteration Info Text
  output$iteration_info <- renderText({
    req(vals$susie_trace)
    paste("Displaying Iteration:", input$iteration_step, "/", vals$total_steps)
  })
  
  # PIP Plot (Scatter Plot)
  output$pip_plot <- renderPlotly({
    req(vals$susie_trace, input$iteration_step)
    
    # Retrieve the specific iteration object
    iter_idx <- input$iteration_step
    # FIX: The trace objects are raw lists. We must manually treat them as susie objects.
    current_fit <- vals$susie_trace[[iter_idx]]
    class(current_fit) <- "susie" # Force class assignment
    
    # FIX: Manually calculate PIPs if the helper fails, or use helper cautiously
    # PIP formula: 1 - prod(1 - alpha) across L (columns)
    if(!is.null(current_fit$alpha)){
      pips <- 1 - apply(1 - current_fit$alpha, 2, prod) 
    } else {
      pips <- rep(0, ncol(vals$sim_data$X)) # Fallback
    }
    
    # FIX: Safely attempt to get CS, defaulting to empty if it fails
    # Trace objects often lack the data needed for full CS calculation (purity checks)
    cs_list <- list(cs = NULL)
    try({
      # We disable purity check to prevent errors since trace objects lack X/Y
      cs_list <- susie_get_cs(current_fit, coverage = input$param_cs_cov, check_purity = FALSE) 
    }, silent = TRUE)
    
    # Create Plot Data Frame
    plot_df <- data.frame(
      variable_index = 1:length(pips),
      pip = as.numeric(pips), # Ensure numeric
      is_cs = "No"
    )
    
    # Mark variables in CS
    if (!is.null(cs_list$cs) && length(cs_list$cs) > 0) {
      for (k in 1:length(cs_list$cs)) {
        vars_in_cs <- cs_list$cs[[k]]
        # Ensure we don't index out of bounds
        valid_vars <- vars_in_cs[vars_in_cs <= nrow(plot_df)]
        if(length(valid_vars) > 0){
          plot_df$is_cs[valid_vars] <- paste0("CS ", k)
        }
      }
    }
    
    # Colors logic
    colors <- c("No" = "lightgray")
    unique_cs <- unique(plot_df$is_cs[plot_df$is_cs != "No"])
    
    if(length(unique_cs) > 0) {
      # Dynamic color palette
      cs_colors <- RColorBrewer::brewer.pal(max(3, length(unique_cs)), "Set1")
      # Subset to actual number needed to avoid warning
      cs_colors <- cs_colors[1:length(unique_cs)]
      names(cs_colors) <- unique_cs
      colors <- c(colors, cs_colors)
    }
    
    # Plotly
    plot_ly(data = plot_df, 
            x = ~variable_index, 
            y = ~pip, 
            type = 'scatter', 
            mode = 'markers',
            color = ~is_cs,
            colors = colors,
            marker = list(
              size = ~pip * 15 + 5, 
              line = list(color = 'white', width = 1),
              opacity = 0.8
            ),
            text = ~paste("Var:", variable_index, "<br>PIP:", round(pip, 3), "<br>Set:", is_cs),
            hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Posterior Inclusion Probabilities (Iteration", iter_idx, ")"),
        xaxis = list(title = "Variable Index"),
        yaxis = list(title = "PIP", range = c(-0.05, 1.05)),
        legend = list(title = list(text = "Credible Sets"))
      )
  })
  
  # 3. ELBO Plot (Requires the FINAL fit object, not trace)
  output$elbo_plot <- renderPlot({
    req(vals$susie_fit)
    elbo_vals <- vals$susie_fit$elbo
    # Ensure elbo is numeric
    if(is.null(elbo_vals)) return(NULL)
    
    plot(elbo_vals, type = "b", pch = 19, col = "blue",
         xlab = "Iteration", ylab = "ELBO", 
         main = "ELBO Convergence")
    grid()
  })
  
  # 4. Credible Sets Text
  output$cs_summary <- renderPrint({
    req(vals$susie_fit)
    print(susie_get_cs(vals$susie_fit))
  })
  
  # 5. Data Preview
  output$data_preview <- renderDT({
    req(vals$sim_data)
    datatable(head(vals$sim_data$X), options = list(dom = 't'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)