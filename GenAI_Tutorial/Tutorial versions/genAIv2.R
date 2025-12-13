library(shiny)
library(plotly)
library(DT)
library(susieR)
library(data.table)
library(matrixStats)
library(reshape2)

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
                 
                 # Clean Viz Control
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
              # Clean Parameters
              div(style = "margin-bottom: 20px;",
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
                tabPanel("PIP Plot", br(), plotlyOutput("pip_plot", height = "500px")),
                tabPanel("ELBO Convergence", br(), plotOutput("elbo_plot")),
                tabPanel("Credible Sets", br(), verbatimTextOutput("cs_summary")),
                tabPanel("Data Preview & Log", br(), DTOutput("data_preview"), verbatimTextOutput("log_output"))
              )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  vals <- reactiveValues(
    susie_fit = NULL, susie_trace = NULL, total_steps = 1, sim_data = NULL
  )
  
  # 1. Run Analysis
  observeEvent(input$run_susie, {
    req(input$data_source == "Simulated Data")
    
    withProgress(message = 'Simulating & Fitting...', value = 0, {
      
      # Select Scenario
      sim_func <- switch(input$data_scenario,
                         "Perfect Collinearity" = simulate_perfect_collinearity,
                         "Moderate LD" = simulate_moderate_ld,
                         "Multiple LD Blocks" = simulate_multiple_ld_blocks,
                         "Tricky Scenario" = simulate_tricky_scenario)
      
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
        
        updateSliderInput(session, "iteration_step", max = vals$total_steps, value = vals$total_steps) 
        output$log_output <- renderText({ paste("Analysis complete.", vals$total_steps, "iterations performed.") })
        
      }, error = function(e) {
        output$log_output <- renderText({ paste("Error:", e$message) })
      })
      incProgress(1, detail = "Done")
    })
  })
  
  # 2. Viz Logic
  observeEvent(input$next_step, {
    req(vals$total_steps)
    if(input$iteration_step < vals$total_steps) {
      updateSliderInput(session, "iteration_step", value = input$iteration_step + 1)
    }
  })
  
  output$iteration_info <- renderText({
    req(vals$susie_trace)
    paste("Displaying Iteration:", input$iteration_step, "/", vals$total_steps)
  })
  
  # 3. ULTIMATE FIX FOR PIP PLOT
  output$pip_plot <- renderPlotly({
    req(vals$susie_trace, input$iteration_step)
    
    # 3a. Prepare Data
    iter_idx <- input$iteration_step
    current_fit <- vals$susie_trace[[iter_idx]]
    class(current_fit) <- "susie"
    
    if(!is.null(current_fit$alpha)) pips <- 1 - apply(1 - current_fit$alpha, 2, prod) 
    else pips <- rep(0, ncol(vals$sim_data$X))
    
    cs_list <- list(cs = NULL)
    try({ cs_list <- susie_get_cs(current_fit, coverage = input$param_cs_cov, check_purity = FALSE) }, silent=T)
    
    plot_df <- data.frame(
      variable_index = 1:length(pips),
      pip = as.numeric(pips),
      cs_id = "No CS", # For label text
      color_code = "#D3D3D3", # Default Light Gray
      stringsAsFactors = FALSE
    )
    
    # 3b. Assign Hex Codes
    # Hex Palette for up to 5 CS
    cs_hex_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
    
    active_cs_indices <- list() # Store for dummy legend later
    
    if (!is.null(cs_list$cs) && length(cs_list$cs) > 0) {
      for (k in 1:length(cs_list$cs)) {
        vars_in_cs <- cs_list$cs[[k]]
        valid_vars <- vars_in_cs[vars_in_cs <= nrow(plot_df)]
        
        if(length(valid_vars) > 0){
          # Assign Label
          plot_df$cs_id[valid_vars] <- paste("CS", k)
          
          # Assign Color (Cycle palette if > 5)
          color_idx <- (k - 1) %% length(cs_hex_palette) + 1
          this_color <- cs_hex_palette[color_idx]
          plot_df$color_code[valid_vars] <- this_color
          
          # Store for legend
          active_cs_indices[[paste("CS", k)]] <- this_color
        }
      }
    }
    
    # 3c. Plotly with Direct Color Mapping
    p <- plot_ly() %>%
      add_trace(
        data = plot_df, 
        x = ~variable_index, 
        y = ~pip, 
        type = 'scatter', 
        mode = 'markers',
        marker = list(
          color = ~color_code,  # <--- DIRECT HEX MAPPING
          size = ~pip * 15 + 6,
          line = list(color = 'white', width = 1),
          opacity = 0.8
        ),
        text = ~paste("Var:", variable_index, "<br>PIP:", round(pip, 3), "<br>Set:", cs_id),
        hoverinfo = "text",
        showlegend = FALSE # Hide main trace from legend (it's mixed colors)
      )
    
    # 3d. Add Dummy Traces for Legend
    # Since we used direct color mapping, the legend won't automatically show "CS 1: Red".
    # We add invisible traces to build the legend manually.
    
    # Always add "No CS" (optional, usually gray is ignored in legend)
    
    if (length(active_cs_indices) > 0) {
      for (lbl in names(active_cs_indices)) {
        col <- active_cs_indices[[lbl]]
        p <- p %>% add_trace(
          x = c(NULL), y = c(NULL), # Empty data
          type = "scatter", mode = "markers",
          marker = list(color = col, size = 10),
          name = lbl,
          showlegend = TRUE
        )
      }
    }
    
    p %>% layout(
      title = paste("Posterior Inclusion Probabilities (Iteration", iter_idx, ")"),
      xaxis = list(title = "Variable Index"),
      yaxis = list(title = "PIP", range = c(-0.05, 1.05))
    )
  })
  
  output$elbo_plot <- renderPlot({
    req(vals$susie_fit)
    if(is.null(vals$susie_fit$elbo)) return(NULL)
    plot(vals$susie_fit$elbo, type = "b", pch = 19, col = "#007bff", lwd = 2,
         xlab = "Iteration", ylab = "ELBO", main = "ELBO Convergence", frame.plot = FALSE)
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