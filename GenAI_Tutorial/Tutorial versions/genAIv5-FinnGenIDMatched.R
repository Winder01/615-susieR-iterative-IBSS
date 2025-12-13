library(shiny)
library(plotly)
library(DT)
library(susieR)
library(data.table)
library(matrixStats)
library(reshape2)

# Load the simulation logic
source("simulation_v3.R")

# Increase file upload limit for genomic data
options(shiny.maxRequestSize = 100 * 1024^2) 

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  titlePanel("SuSiE Iterative Framework (FinnGen Support)"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("1. Data Configuration"),
                 radioButtons("data_format", "Data Format:",
                              choices = c("Summary Statistics", "Individual Data"),
                              selected = "Individual Data"),
                 
                 # Updated Data Source choices
                 selectInput("data_source", "Data Source:",
                             choices = c("Simulated Data", "Upload FinnGen")),
                 
                 # Conditional UI for Simulation
                 conditionalPanel(
                   condition = "input.data_source == 'Simulated Data'",
                   selectInput("data_scenario", "Simulation Scenario:",
                               choices = c("Perfect Collinearity", 
                                           "Moderate LD", 
                                           "Multiple LD Blocks",
                                           "Tricky Scenario"))
                 ),
                 
                 # Conditional UI for FinnGen Uploads
                 conditionalPanel(
                   condition = "input.data_source == 'Upload FinnGen'",
                   fileInput("file_sumstats", "Upload SumStats (.gz)", accept = ".gz"),
                   fileInput("file_ld", "Upload LD Matrix (.tsv.gz)", accept = ".gz"),
                   helpText("Format: SumStats needs #chrom/pos/ref/alt. LD needs variant1/variant2/r.")
                 ),
                 
                 hr(),
                 
                 h4("2. Execution"),
                 actionButton("run_susie", "Run Analysis", 
                              class = "btn-primary btn-lg", width = "100%"),
                 br(), br(),
                 
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
              div(
                style = "margin-bottom: 20px;",
                h4("Analysis Parameters"),
                fluidRow(
                  column(2, numericInput("param_L", "L (Effects)", value = 10, min = 1)),
                  column(2, numericInput("param_max_iter", "Max Iter", value = 100, min = 1)),
                  column(2, numericInput("param_lambda", "LD Reg (Lambda)", value = 0.001, step = 0.001)),
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
    sim_data = NULL,
    rss_data = NULL
  )
  
  # 1. Run Analysis Logic
  observeEvent(input$run_susie, {
    
    # --- SCENARIO A: SIMULATION ---
    if(input$data_source == "Simulated Data") {
      req(input$data_scenario)
      
      withProgress(message = 'Simulating & Fitting...', value = 0, {
        sim_func <- switch(input$data_scenario,
                           "Perfect Collinearity" = simulate_perfect_collinearity,
                           "Moderate LD" = simulate_moderate_ld,
                           "Multiple LD Blocks" = simulate_multiple_ld_blocks,
                           "Tricky Scenario" = simulate_tricky_scenario)
        
        vals$sim_data <- sim_func()
        vals$rss_data <- NULL 
        
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
          output$log_output <- renderText({ paste("Simulation Analysis complete.", vals$total_steps, "iterations.") })
          
        }, error = function(e) {
          output$log_output <- renderText({ paste("Error:", e$message) })
        })
        incProgress(1, detail = "Done")
      })
    }
    
    # --- SCENARIO B: FINNGEN UPLOAD (FIXED FOR CHR PREFIX) ---
    if(input$data_source == "Upload FinnGen") {
      req(input$file_sumstats, input$file_ld)
      
      withProgress(message = 'Processing FinnGen Data...', value = 0, {
        
        tryCatch({
          # 1. Load Data
          incProgress(0.1, detail = "Reading Files...")
          ss <- fread(input$file_sumstats$datapath)
          ld <- fread(input$file_ld$datapath)
          
          # 2. Clean Names (Fix #chrom issue)
          setnames(ss, tolower(names(ss)))
          names(ss)[names(ss) == "#chrom"] <- "chrom"
          
          setnames(ld, tolower(names(ld))) # Ensure ld variant1 is found
          
          # 3. Construct Match ID (Force 'chr' prefix)
          incProgress(0.3, detail = "Constructing IDs...")
          
          # Strip 'chr' first to be safe, then add it back consistently
          ss$chrom <- gsub("^chr", "", as.character(ss$chrom), ignore.case=TRUE)
          ss$match_id <- paste0("chr", ss$chrom, "_", ss$pos, "_", ss$ref, "_", ss$alt)
          
          # 4. Strict Intersection
          # Assume LD file uses 'variant1' as ID
          if(!"variant1" %in% names(ld)) stop("LD file must have 'variant1' column.")
          
          common_ids <- intersect(ss$match_id, ld$variant1)
          
          if(length(common_ids) == 0) {
            stop(paste("No matching IDs found. \nSS Example: ", ss$match_id[1], "\nLD Example: ", ld$variant1[1]))
          }
          
          ss_subset <- ss[match_id %in% common_ids]
          # Filter LD rows AND columns? Usually long format is rows.
          # We filter rows where both variants are in our set
          ld_subset <- ld[variant1 %in% common_ids & variant2 %in% common_ids]
          
          # 5. Calc Z and Deduplicate
          if(!"beta" %in% names(ss_subset) || !"sebeta" %in% names(ss_subset)) stop("SumStats need beta and sebeta columns.")
          ss_subset[, z := beta / sebeta]
          
          # Keep unique IDs with max Z
          ss_subset <- ss_subset[order(-abs(z))]
          ss_subset <- ss_subset[!duplicated(match_id)]
          
          # Align order strictly
          ss_subset <- ss_subset[order(match_id)]
          z_scores <- ss_subset$z
          names(z_scores) <- ss_subset$match_id
          
          # 6. Matrix Construction
          incProgress(0.5, detail = "Building LD Matrix...")
          
          # Pivot: variant1 (rows) x variant2 (cols)
          R_mat <- dcast(ld_subset, variant1 ~ variant2, value.var = "r", fill = 0)
          rownames(R_mat) <- R_mat$variant1
          R_mat <- as.matrix(R_mat[,-1])
          
          # Filter R to match Z exactly (Intersection again to be safe)
          final_ids <- intersect(names(z_scores), rownames(R_mat))
          
          z_scores <- z_scores[final_ids]
          R_mat <- R_mat[final_ids, final_ids]
          
          # 7. Robust Regularization
          diag(R_mat) <- 1
          R_mat <- (R_mat + t(R_mat)) / 2
          
          lambda <- input$param_lambda
          R_reg <- (1 - lambda) * R_mat + lambda * diag(nrow(R_mat))
          
          vals$rss_data <- list(z = z_scores, R = R_reg)
          vals$sim_data <- NULL
          
          # 8. Run SuSiE RSS
          incProgress(0.7, detail = "Running SuSiE RSS...")
          
          fit <- tryCatch({
            susie_rss(z = z_scores, R = R_reg,
                      L = input$param_L,
                      max_iter = input$param_max_iter,
                      check_prior = FALSE,
                      estimate_residual_variance = FALSE,
                      track_fit = TRUE)
          }, error = function(e) {
            warning("Standard run failed, attempting fallback...")
            susie_rss(z = z_scores, R = R_reg,
                      L = input$param_L,
                      max_iter = input$param_max_iter,
                      estimate_prior_variance = FALSE,
                      scaled_prior_variance = 0.1,
                      check_prior = FALSE,
                      estimate_residual_variance = FALSE,
                      track_fit = TRUE)
          })
          
          vals$susie_fit <- fit
          vals$susie_trace <- fit$trace
          vals$total_steps <- length(fit$trace)
          
          updateSliderInput(session, "iteration_step", max = vals$total_steps, value = vals$total_steps) 
          output$log_output <- renderText({ 
            paste("FinnGen Analysis complete.", vals$total_steps, "iterations.", 
                  "\nVariants analyzed:", length(z_scores)) 
          })
          
        }, error = function(e) {
          output$log_output <- renderText({ paste("Critical Error:", e$message) })
        })
        incProgress(1, detail = "Done")
      })
    }
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
    
    fit_step <- vals$susie_trace[[input$iteration_step]]
    
    # Calc PIPs
    if(!is.null(fit_step$alpha)) {
      pip_val <- 1 - apply(1 - fit_step$alpha, 2, prod)
    } else {
      if(!is.null(vals$sim_data)) n_vars <- ncol(vals$sim_data$X)
      else if(!is.null(vals$rss_data)) n_vars <- length(vals$rss_data$z)
      else n_vars <- 100
      pip_val <- rep(0, n_vars)
    }
    pip_val <- as.numeric(pip_val)
    
    # Colors
    cols <- rep("lightgray", length(pip_val))
    final_fit <- vals$susie_fit
    
    if (!is.null(final_fit$sets$cs) && length(final_fit$sets$cs) > 0) {
      colors_pal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
      for(i in 1:length(final_fit$sets$cs)) {
        idx <- final_fit$sets$cs[[i]]
        idx <- idx[idx <= length(cols)]
        if(length(idx) > 0) {
          cols[idx] <- colors_pal[(i - 1) %% length(colors_pal) + 1]
        }
      }
    }
    
    plot_df <- data.frame(
      variable = 1:length(pip_val),
      pip = pip_val
    )
    
    plot_ly(data = plot_df,
            x = ~variable,
            y = ~pip,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              color = cols,
              size = pip_val * 15 + 5,
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
        showlegend = FALSE
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
    if(!is.null(vals$sim_data)) {
      datatable(head(vals$sim_data$X), options = list(dom = 't'))
    } else if (!is.null(vals$rss_data)) {
      z_df <- data.frame(Variant = names(vals$rss_data$z)[1:10], 
                         Z_Score = vals$rss_data$z[1:10])
      datatable(z_df, options = list(dom = 't'))
    }
  })
}

shinyApp(ui = ui, server = server)