library(shiny)
library(plotly)
library(DT)
library(susieR)
library(data.table)
library(matrixStats)
library(reshape2)
library(Rfast) # Optimization library

# Load the simulation logic
source("simulation_v3.R")

# Increase file upload limit for genomic data
options(shiny.maxRequestSize = 200 * 1024^2) 

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  titlePanel("SuSiE Iterative Framework (FinnGen Support - Final)"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("1. Data Configuration"),
                 radioButtons("data_format", "Data Format:",
                              choices = c("Summary Statistics", "Individual Data"),
                              selected = "Individual Data"),
                 
                 selectInput("data_source", "Data Source:",
                             choices = c("Simulated Data", "Upload FinnGen")),
                 
                 # Simulation UI
                 conditionalPanel(
                   condition = "input.data_source == 'Simulated Data'",
                   selectInput("data_scenario", "Simulation Scenario:",
                               choices = c("Perfect Collinearity", 
                                           "Moderate LD", 
                                           "Multiple LD Blocks",
                                           "Tricky Scenario"))
                 ),
                 
                 # FinnGen UI
                 conditionalPanel(
                   condition = "input.data_source == 'Upload FinnGen'",
                   fileInput("file_sumstats", "Upload SumStats (.gz)", accept = ".gz"),
                   fileInput("file_ld", "Upload LD Matrix (.tsv.gz)", accept = ".gz"),
                   helpText("Ensure LD is in long format (variant1, variant2, r).")
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
              # Configuration Bar
              div(
                style = "background-color: #e9ecef; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
                h4("Analysis Parameters"),
                fluidRow(
                  column(2, numericInput("param_L", "L (Effects)", value = 10, min = 1)),
                  column(2, numericInput("param_max_iter", "Max Iter", value = 100, min = 1)),
                  # UPDATE 1: Default Lambda changed to 0.1
                  column(2, numericInput("param_lambda", "LD Regularization", value = 0.1, step = 0.05)),
                  column(3, numericInput("param_cs_cov", "CS Coverage", value = 0.95, step = 0.05)),
                  column(3, numericInput("param_prior_var", "Prior Var (Init)", value = 0.1, step = 0.05))
                ),
                
                # FinnGen Specifics
                conditionalPanel(
                  condition = "input.data_source == 'Upload FinnGen'",
                  hr(),
                  fluidRow(
                    column(4, numericInput("finngen_n", "Sample Size (N):", value = 520210)),
                    column(4, numericInput("top_n", "Top N Filter (by Z):", value = 1000))
                  )
                )
              ),
              
              # Results
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
                         # UPDATE 3: Changed to Data Table Output
                         DTOutput("cs_table")
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
    rss_data = NULL,      
    # Store auxiliary info for table/plot
    meta_data = NULL  
  )
  
  # 1. Run Analysis Logic
  observeEvent(input$run_susie, {
    
    # --------------------------------------------------------------------------
    # SCENARIO A: SIMULATION
    # --------------------------------------------------------------------------
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
        vals$meta_data <- NULL # No meta data for simple sims
        
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
    
    # --------------------------------------------------------------------------
    # SCENARIO B: FINNGEN OPTIMIZED
    # --------------------------------------------------------------------------
    if(input$data_source == "Upload FinnGen") {
      req(input$file_sumstats, input$file_ld, input$finngen_n)
      
      withProgress(message = 'Processing FinnGen Data...', value = 0, {
        
        tryCatch({
          # 1. Load Data
          incProgress(0.05, detail = "Reading Files...")
          ss <- fread(input$file_sumstats$datapath)
          ld <- fread(input$file_ld$datapath)
          
          # Clean colnames
          setnames(ss, tolower(names(ss)))
          names(ss)[names(ss) == "#chrom"] <- "chrom"
          names(ld) <- tolower(names(ld))
          
          # 2. Robust LD Renaming
          col_map <- names(ld)
          v1_candidate <- col_map[grep("variant1|var1|v1|rsid1", col_map)][1]
          if(is.na(v1_candidate)) v1_candidate <- col_map[1] 
          v2_candidate <- col_map[grep("variant2|var2|v2|rsid2", col_map)][1]
          if(is.na(v2_candidate)) v2_candidate <- col_map[2] 
          r_candidate <- col_map[grep("^r$|corr|correlation", col_map)][1]
          if(is.na(r_candidate)) r_candidate <- col_map[3] 
          
          setnames(ld, old = c(v1_candidate, v2_candidate, r_candidate), new = c("v1", "v2", "r"))
          
          # 3. Construct IDs & Z-Scores
          incProgress(0.1, detail = "Aligning IDs...")
          ss$chrom <- as.character(ss$chrom)
          ss$pos <- as.character(ss$pos)
          
          # Force 'chr' prefix
          ss$chrom_clean <- gsub("^chr", "", ss$chrom, ignore.case = TRUE)
          ss$match_id <- paste0("chr", ss$chrom_clean, "_", ss$pos, "_", ss$ref, "_", ss$alt)
          
          ss[, z := beta / sebeta]
          
          # 4. Aggressive Early Filtering
          ld_unique_ids <- unique(c(ld$v1, ld$v2))
          ss <- ss[match_id %in% ld_unique_ids]
          if(nrow(ss) == 0) stop("No matching IDs found between SumStats and LD.")
          
          ss <- ss[order(-abs(z))]
          ss <- ss[!duplicated(match_id)] 
          
          if(nrow(ss) > input$top_n) {
            ss <- ss[1:input$top_n, ]
          }
          
          target_ids <- ss$match_id
          
          # STORE META DATA (Needed for CS Table and Plotting)
          # We keep the subsetted dataframe to extract positions, betas, pvals later
          vals$meta_data <- ss[, c("match_id", "pos", "beta", "sebeta", "pval", "z"), with = FALSE]
          vals$meta_data$pos <- as.numeric(vals$meta_data$pos) # Ensure numeric pos
          
          # 5. Matrix Construction
          incProgress(0.3, detail = "Building Sparse Matrix...")
          ld_subset <- ld[v1 %in% target_ids & v2 %in% target_ids]
          R_mat <- dcast(ld_subset, v1 ~ v2, value.var = "r", fill = 0)
          rownames(R_mat) <- R_mat$v1
          R_mat <- as.matrix(R_mat[,-1])
          
          common_final <- intersect(target_ids, rownames(R_mat))
          if(length(common_final) < 2) stop("Not enough variants remained after filtering.")
          
          # Filter Meta Data to final set
          vals$meta_data <- vals$meta_data[match_id %in% common_final]
          # Sort Meta Data to match matrix order exactly
          vals$meta_data <- vals$meta_data[match(common_final, vals$meta_data$match_id), ]
          
          z_scores <- vals$meta_data$z
          names(z_scores) <- vals$meta_data$match_id
          R_mat <- R_mat[common_final, common_final]
          
          diag(R_mat) <- 1
          R_mat <- (R_mat + t(R_mat)) / 2
          
          lambda <- input$param_lambda
          R_reg <- (1 - lambda) * R_mat + lambda * diag(nrow(R_mat))
          
          vals$rss_data <- list(z = z_scores, R = R_reg)
          vals$sim_data <- NULL
          
          # 6. Run SuSiE RSS
          incProgress(0.6, detail = "Running SuSiE RSS...")
          
          fit <- susie_rss(z = z_scores, R = R_reg,
                           n = input$finngen_n,          
                           L = input$param_L,
                           max_iter = input$param_max_iter,
                           check_prior = FALSE,          
                           estimate_residual_variance = FALSE, 
                           track_fit = TRUE)
          
          vals$susie_fit <- fit
          vals$susie_trace <- fit$trace
          vals$total_steps <- length(fit$trace)
          
          updateSliderInput(session, "iteration_step", max = vals$total_steps, value = vals$total_steps) 
          output$log_output <- renderText({ 
            paste("FinnGen Analysis complete.", vals$total_steps, "iterations.", 
                  "\nFinal Model Size:", length(z_scores), "variants.") 
          })
          
        }, error = function(e) {
          output$log_output <- renderText({ paste("Error:", e$message) })
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
  
  # --- PIP PLOT (UPDATED X-AXIS) ---
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
    
    # Determine X-Axis Data
    if (!is.null(vals$meta_data)) {
      # Use Real Genomic Positions (Mb)
      x_vals <- vals$meta_data$pos / 1e6
      x_label <- "Chromosome Position (Mb)"
      hover_id <- vals$meta_data$match_id
    } else {
      # Use Index
      x_vals <- 1:length(pip_val)
      x_label <- "Variable Index"
      hover_id <- paste("Index", 1:length(pip_val))
    }
    
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
      x_pos = x_vals,
      pip = pip_val,
      h_id = hover_id
    )
    
    plot_ly(data = plot_df,
            x = ~x_pos,
            y = ~pip,
            type = 'scatter',
            mode = 'markers',
            marker = list(
              color = cols,
              size = pip_val * 15 + 5,
              line = list(color = 'white', width = 1),
              opacity = 0.8
            ),
            text = ~paste("ID:", h_id, "<br>PIP:", round(pip, 3), "<br>Pos:", x_pos),
            hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Posterior Inclusion Probabilities (Iteration", input$iteration_step, ")"),
        xaxis = list(title = x_label),
        yaxis = list(title = "PIP", range = c(-0.05, 1.05)),
        showlegend = FALSE
      )
  })
  
  # --- CREDIBLE SETS TABLE (UPDATED FORMAT) ---
  output$cs_table <- renderDT({
    req(vals$susie_fit)
    
    fit <- vals$susie_fit
    cs <- fit$sets$cs
    
    if(length(cs) == 0) return(datatable(data.frame(Message = "No Credible Sets identified.")))
    
    # Build Table
    table_rows <- list()
    
    for(i in seq_along(cs)) {
      cs_idx <- cs[[i]]
      
      # Extract info based on availability (Sim vs FinnGen)
      if(!is.null(vals$meta_data)) {
        # Real Data
        row_data <- vals$meta_data[cs_idx, ]
        row_df <- data.frame(
          CS = paste0("L", i),
          Variant = row_data$match_id,
          Pos = row_data$pos,
          PIP = round(fit$pip[cs_idx], 4),
          Beta = round(row_data$beta, 4),
          Z_Score = round(row_data$z, 4),
          P_Val = formatC(row_data$pval, format = "e", digits = 2)
        )
      } else {
        # Sim Data (Minimal Info)
        row_df <- data.frame(
          CS = paste0("L", i),
          Variant = paste("Var", cs_idx),
          Pos = NA,
          PIP = round(fit$pip[cs_idx], 4),
          Beta = NA,
          Z_Score = NA,
          P_Val = NA
        )
      }
      table_rows[[i]] <- row_df
    }
    
    final_df <- do.call(rbind, table_rows)
    datatable(final_df, options = list(pageLength = 10, dom = 'tip'))
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