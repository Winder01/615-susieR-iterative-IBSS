library(shiny)
library(plotly)
library(DT)
library(susieR)
library(data.table)
library(matrixStats)
library(reshape2)

# Load the simulation logic
source("simulation_v3.R")

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  titlePanel("SuSiE Iterative Framework"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("1. Data Configuration"),
                 radioButtons("data_format", "Data Format:",
                              choices = c("Summary Statistics", "Individual Data"),
                              selected = "Individual Data"),
                 selectInput("data_source", "Data Source:",
                             choices = c("Simulated Data")),
                 selectInput("data_scenario", "Simulation Scenario:",
                             choices = c("Perfect Collinearity", 
                                         "Moderate LD", 
                                         "Multiple LD Blocks",
                                         "Tricky Scenario")),
                 
                 hr(),
                 
                 h4("2. Execution"),
                 actionButton("run_susie", "Run Analysis", 
                              class = "btn-primary btn-lg", width = "100%"),
                 br(), br(),
                 
                 # Clean Visualization Control
                 div(
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
              # Clean Analysis Parameters
              div(
                style = "margin-bottom: 20px;",
                h4("Analysis Parameters"),
                fluidRow(
                  column(2, numericInput("param_L", "L (Effects)", value = 10, min = 1)),
                  column(2, numericInput("param_max_iter", "Max Iter", value = 100, min = 1)),
                  column(2, numericInput("param_tol", "Tol", value = 0.001)),
                  column(3, numericInput("param_cs_cov", "CS Coverage", value = 0.95, step = 0.05)),
                  column(3, numericInput("param_prior_var", "Prior Var", value = 0.1, step = 0.05))
                )
              ),
              
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
  
  vals <- reactiveValues(
    susie_fit = NULL,
    susie_trace = NULL,
    total_steps = 1,
    sim_data = NULL
  )
  
  # 1. Run Analysis
  observeEvent(input$run_susie, {
    req(input$data_source == "Simulated Data")
    
    withProgress(message = 'Simulating & Fitting...', value = 0, {
      
      # Select Scenario Function from simulation_v3.R
      sim_func <- switch(input$data_scenario,
                         "Perfect Collinearity" = simulate_perfect_collinearity,
                         "Moderate LD" = simulate_moderate_ld,
                         "Multiple LD Blocks" = simulate_multiple_ld_blocks,
                         "Tricky Scenario" = simulate_tricky_scenario)
      
      # Generate Data
      vals$sim_data <- sim_func()
      incProgress(0.3, detail = "Data Generated")
      
      tryCatch({
        fit <- susie(vals$sim_data$X, vals$sim_data$y, 
                     L = input$param_L,
                     max_iter = input$param_max_iter,
                     estimate_residual_variance = TRUE, 
                     estimate_prior_variance = TRUE,
                     track_fit = TRUE)
        
        vals$susie_fit <- fit
        vals$susie_trace <- fit$trace
        vals$total_steps <- length(fit$trace)
        
        updateSliderInput(session, "iteration_step", 
                          max = vals$total_steps, 
                          value = vals$total_steps) 
        
        output$log_output <- renderText({
          paste("Analysis complete.", vals$total_steps, "iterations performed.")
        })
        
      }, error = function(e) {
        output$log_output <- renderText({ paste("Error:", e$message) })
      })
      incProgress(1, detail = "Done")
    })
  })
  
  # 2. Visualization Logic
  observeEvent(input$next_step, {
    req(vals$total_steps)
    current <- input$iteration_step
    if(current < vals$total_steps) {
      updateSliderInput(session, "iteration_step", value = current + 1)
    }
  })
  
  output$iteration_info <- renderText({
    req(vals$susie_trace)
    paste("Displaying Iteration:", input$iteration_step, "/", vals$total_steps)
  })
  
  output$pip_plot <- renderPlotly({
    req(vals$susie_trace, input$iteration_step, vals$susie_fit)
    
    # Get the trace object for the current step
    fit_step <- vals$susie_trace[[input$iteration_step]]
    
    # Calculate PIPs (Robust Method)
    # Using colSums(alpha) approximates the PIP for visualization if L is large and sparse
    # The exact definition is 1 - prod(1-alpha), but if provided logic prefers simple sum or alpha access:
    if(!is.null(fit_step$alpha)) {
      # Use the standard SuSiE definition for PIP: 1 - prod(1 - alpha_lk)
      pip_val <- 1 - apply(1 - fit_step$alpha, 2, prod)
    } else {
      pip_val <- rep(0, ncol(vals$sim_data$X))
    }
    pip_val <- as.numeric(pip_val)
    
    # Initialize Color Vector (Default Gray)
    cols <- rep("lightgray", length(pip_val))
    
    # Color Mapping Logic using FINAL fit Credible Sets
    final_fit <- vals$susie_fit
    
    # Check if sets exist and are valid
    if (!is.null(final_fit$sets$cs) && length(final_fit$sets$cs) > 0) {
      colors_pal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd") # Tableau Colors
      
      for(i in 1:length(final_fit$sets$cs)) {
        idx <- final_fit$sets$cs[[i]]
        # Boundary check to prevent index out of bounds error
        idx <- idx[idx <= length(cols)]
        
        if(length(idx) > 0) {
          # Use modulo operator to cycle through colors if there are more sets than colors
          color_to_use <- colors_pal[(i - 1) %% length(colors_pal) + 1]
          cols[idx] <- color_to_use
        }
      }
    }
    
    # Create Plot Data
    plot_df <- data.frame(
      variable = 1:length(pip_val),
      pip = pip_val
    )
    
    # Plotly Call - DIRECT COLOR ASSIGNMENT
    plot_ly(data = plot_df,
            x = ~variable,
            y = ~pip,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              color = cols,               # Direct vector assignment
              size = pip_val * 15 + 5,    # Size scaling
              line = list(color = 'white', width = 1),
              opacity = 0.8
            ),
            text = ~paste("Var:", variable, "<br>PIP:", round(pip, 3)),
            hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Posterior Inclusion Probabilities (Iteration", input$iteration_step, ")"),
        xaxis = list(title = "Variable Index"),
        yaxis = list(title = "PIP", range = c(-0.05, 1.05)),
        showlegend = FALSE # Legend is less useful with direct coloring, can add manual legend if needed
      )
  })
  
  output$elbo_plot <- renderPlot({
    req(vals$susie_fit)
    elbo_vals <- vals$susie_fit$elbo
    if(is.null(elbo_vals)) return(NULL)
    plot(elbo_vals, type = "b", pch = 19, col = "#007bff", lwd = 2,
         xlab = "Iteration", ylab = "ELBO", 
         main = "ELBO Convergence", frame.plot = FALSE)
    grid()
  })
  
  output$cs_summary <- renderPrint({
    req(vals$susie_fit)
    print(susie_get_cs(vals$susie_fit))
  })
  
  output$data_preview <- renderDT({
    req(vals$sim_data)
    datatable(head(vals$sim_data$X), options = list(dom = 't'))
  })
}

shinyApp(ui = ui, server = server)