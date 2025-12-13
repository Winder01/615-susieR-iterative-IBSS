library(shiny)
library(plotly)
library(matrixStats)
library(DT)
library(susieR)
library(data.table)
library(reshape2) # Required for dcast

# --- Auto-install R.utils for .gz support ---
if (!require("R.utils", quietly = TRUE)) {
  showNotification("Installing R.utils...", type="message")
  install.packages("R.utils")
}
library(R.utils)

# --- Performance Optimization (Optional) ---
if (!require("Rfast", quietly = TRUE)) {
  # install.packages("Rfast") 
} else {
  library(Rfast)
}

# Increase upload limit to 1GB
options(shiny.maxRequestSize = 1000 * 1024^2)

# Source the simulation file
if(file.exists("simulation_v3.R")) {
  source("simulation_v3.R")
} else {
  simulate_perfect_collinearity <- function() list(X=matrix(rnorm(5000),500,10), y=rnorm(500), p=10)
  simulate_moderate_ld <- function() list(X=matrix(rnorm(5000),500,10), y=rnorm(500), p=10)
  simulate_multiple_ld_blocks <- function() list(X=matrix(rnorm(5000),500,10), y=rnorm(500), p=10)
  simulate_tricky_scenario <- function() list(X=matrix(rnorm(5000),500,10), y=rnorm(500), p=10)
}

# ==============================================================================
# UI DEFINITION
# ==============================================================================
ui <- fluidPage(
  titlePanel("SuSiE Iterative Visualization"),
  
  sidebarLayout(
    # --- LEFT SIDEBAR: DATA SELECTION & ACTIONS ---
    sidebarPanel(
      width = 3, 
      
      h4("1. Data Configuration"),
      
      # 1. Global Data Format Toggle
      radioButtons("analysis_type", "Data Format:",
                   choices = c("Summary Statistics" = "summary",
                               "Individual Data" = "individual"),
                   selected = "summary", 
                   inline = TRUE),
      
      hr(),
      
      # 2. Data Source Selector (Renamed as requested)
      selectInput("input_type", "Data Source:",
                  choices = c("Simulated Data", 
                              "Upload Text", 
                              "Upload FinnGen")),
      
      # --- CONDITIONAL INPUTS BASED ON SOURCE ---
      
      # A. Simulated Data Inputs
      conditionalPanel(
        condition = "input.input_type == 'Simulated Data'",
        selectInput("data_scenario", "Choose Simulation:",
                    choices = c("Perfect Collinearity" = "perfect_collinearity",
                                "Moderate LD" = "moderate_ld",
                                "Multiple LD Blocks" = "multiple_ld_blocks",
                                "Tricky Scenario" = "tricky_scenario",
                                "Real Dataset (N3finemapping)" = "real_data"))
      ),
      
      # B. Standard Upload Inputs
      conditionalPanel(
        condition = "input.input_type == 'Upload Text'",
        helpText("Simple matrix upload (Space/Tab delimited)."),
        conditionalPanel(
          condition = "input.analysis_type == 'individual'",
          fileInput("X_file", "Upload Genotype (X)"),
          fileInput("y_file", "Upload Phenotype (y)")
        ),
        conditionalPanel(
          condition = "input.analysis_type == 'summary'",
          fileInput("R_file", "Upload LD (R)"),
          fileInput("z_file", "Upload Z-scores (z)"),
          numericInput("n_uploaded", "Sample Size (n):", value = 1000)
        )
      ),
      
      # C. FinnGen Inputs (Sample Size moved to Main Panel)
      conditionalPanel(
        condition = "input.input_type == 'Upload FinnGen'",
        helpText("Supports FinnGen R12 formats (.gz)."),
        fileInput("finngen_sumstats_file", "1. Upload SumStats (.gz/.txt)"),
        fileInput("finngen_ld_file", "2. Upload LD Matrix (Long Format)")
      ),
      
      hr(style = "border-top: 2px solid #ccc;"),
      
      # 3. ACTIONS
      h4("2. Execution"),
      actionButton("run_susie", "Run Analysis", class = "btn-primary btn-lg", width = "100%"),
      
      br(), br(),
      
      div(style="background: #f5f5f5; padding: 10px; border-radius: 5px;",
          h5("Visualization Control"),
          uiOutput("iteration_slider_ui"),
          actionButton("next_step", "Next Step", width = "100%"),
          div(style="text-align: center; margin-top: 5px;", textOutput("iteration_info"))
      )
    ),
    
    # --- RIGHT MAIN PANEL: PARAMETERS & RESULTS ---
    mainPanel(
      width = 9,
      
      # 1. PARAMETERS BAR (Expanded with Sample Size)
      div(style = "background-color: #e9ecef; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #ddd;",
          h4("Analysis Parameters", style="margin-top:0;"),
          
          # Row 1: General Parameters
          fluidRow(
            column(2, numericInput("L", "L (Effects):", value = 10, min = 1, width="100%")),
            column(2, numericInput("max_iter", "Max Iter:", value = 100, min = 1, width="100%")),
            column(2, numericInput("lambda", "Lambda (Reg):", value = 0.1, step = 0.001, width="100%")),
            column(2, numericInput("coverage", "CS Coverage:", value = 0.95, step = 0.01, width="100%")),
            column(2, numericInput("scaled_prior_variance", "Prior Var:", value = 0.1, step = 0.01, width="100%"))
          ),
          
          helpText("Note: Lambda = 0.1 is recommended for mismatched LD (FinnGen). For In-sample LD, use 0."),
          
          # Row 2: FinnGen Specific Parameters (Sample Size & Filter)
          conditionalPanel(
            condition = "input.input_type == 'Upload FinnGen'",
            hr(style="border-color: #ccc; margin-top: 5px; margin-bottom: 10px;"),
            h5("Data Specific Settings (FinnGen)", style="color: #666;"),
            fluidRow(
              column(3, numericInput("top_n_filter", "Top N Filter:", value = 1000, width="100%")),
              
              # Sample Size Logic moved here
              column(3, style = "padding-top: 25px;",
                     checkboxInput("auto_N", "Auto-estimate Sample Size", value = FALSE)
              ),
              column(3, 
                     conditionalPanel(
                       condition = "input.auto_N == false",
                       numericInput("finngen_n_manual", "Manual Sample Size (N):", value = 520210, width="100%")
                     )
              )
            ),
            helpText("Note: If the computation is slow or fail to load, try reduce sample size.")
          )
          
      ),
      
      # 2. ERROR DISPLAY
      uiOutput("error_display"),
      
      # 3. RESULTS TABS
      tabsetPanel(
        tabPanel("PIP Plot", 
                 br(),
                 plotlyOutput("pip_plot", height = "500px")
        ),
        tabPanel("ELBO Convergence", 
                 br(),
                 plotlyOutput("elbo_plot")
        ),
        tabPanel("Credible Sets", 
                 br(),
                 DT::dataTableOutput("cs_table")
        ),
        tabPanel("Data Preview & Log", 
                 br(),
                 div(class="alert alert-info", 
                     h4("Dataset Description"),
                     htmlOutput("dataset_description")
                 ),
                 hr(),
                 h4("Analysis Log"),
                 verbatimTextOutput("data_log"),
                 hr(),
                 h4("Data Head Preview"),
                 tableOutput("preview_table"),
                 hr(),
                 h4("LD Matrix Preview (Top-Left, Regularized)"),
                 tableOutput("ld_preview")
        )
      )
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  susie_fit <- reactiveVal(NULL)
  susie_trace <- reactiveVal(NULL)
  total_steps <- reactiveVal(0)
  
  plot_data_store <- reactiveVal(NULL)
  finngen_table_store <- reactiveVal(NULL)
  preview_data_store <- reactiveVal(NULL)
  ld_preview_store <- reactiveVal(NULL)
  
  run_log <- reactiveVal("Ready.")
  error_msg <- reactiveVal(NULL)
  
  log_message <- function(msg) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    run_log(paste0("[", timestamp, "] ", msg, "\n", run_log()))
  }
  
  output$data_log <- renderText({ run_log() })
  output$preview_table <- renderTable({ req(preview_data_store()); head(preview_data_store(), 10) })
  output$ld_preview <- renderTable({ req(ld_preview_store()); ld_preview_store() }, rownames = TRUE)
  
  output$error_display <- renderUI({
    if (!is.null(error_msg())) {
      div(style = "color: red; background-color: #fee; border: 1px solid red; padding: 10px; margin: 10px 0;",
          h4("Analysis Error:"), p(error_msg()))
    }
  })
  
  output$dataset_description <- renderUI({
    if (input$input_type == "Simulated Data") {
      req(input$data_scenario)
      desc <- switch(input$data_scenario,
                     "perfect_collinearity" = "Simulation: Two pairs of perfectly correlated variants (r=1).",
                     "moderate_ld" = "Simulation: Two separate blocks of moderate LD (r=0.6).",
                     "multiple_ld_blocks" = "Simulation: Blocks with high (0.9), mod (0.6), low (0.3) LD.",
                     "tricky_scenario" = "Simulation: High LD block (r=0.95) with weak signals.",
                     "real_data" = "N3finemapping: Real genotype matrix (Chr 19) with simulated phenotype.")
      HTML(desc)
    } else if (input$input_type == "Upload FinnGen") {
      HTML("<b>FinnGen Mode (Strict Matching):</b><br>Only variants present in BOTH SumStats and LD Matrix are kept.<br>Recommended Lambda: 0.001 - 0.1")
    } else {
      HTML("<b>Standard Upload:</b> Expects plain text matrices.")
    }
  })
  
  observeEvent(input$run_susie, {
    error_msg(NULL)
    run_log("Starting Workflow...")
    showNotification("Running Analysis...", type = "message")
    
    susie_fit(NULL); susie_trace(NULL); total_steps(0)
    plot_data_store(NULL); finngen_table_store(NULL); preview_data_store(NULL); ld_preview_store(NULL)
    data <- NULL
    
    tryCatch({
      
      # 1. SIMULATED DATA
      if (input$input_type == "Simulated Data") {
        log_message(paste("Generating:", input$data_scenario))
        sim_data <- switch(input$data_scenario,
                           "perfect_collinearity" = simulate_perfect_collinearity(),
                           "moderate_ld" = simulate_moderate_ld(),
                           "multiple_ld_blocks" = simulate_multiple_ld_blocks(),
                           "tricky_scenario" = simulate_tricky_scenario(),
                           "real_data" = {
                             data(N3finemapping)
                             list(X = N3finemapping$X, y = N3finemapping$Y[,1], true_coef = N3finemapping$true_coef[,1], p = ncol(N3finemapping$X))
                           })
        if (input$analysis_type == "individual") {
          data <- list(X = sim_data$X, y = sim_data$y, true_coef = sim_data$true_coef, pos = NULL)
          preview_data_store(as.data.frame(sim_data$X))
        } else {
          R_mat <- cor(sim_data$X)
          ss <- susieR::univariate_regression(sim_data$X, sim_data$y)
          z_vec <- ss$betahat / ss$sebetahat
          data <- list(R = R_mat, z = z_vec, n = nrow(sim_data$X), true_coef = sim_data$true_coef, pos = NULL)
          preview_data_store(as.data.frame(R_mat))
        }
      } 
      
      # 2. STANDARD UPLOAD (Updated Logic String)
      else if (input$input_type == "Upload Text") {
        log_message("Reading Standard Files...")
        if (input$analysis_type == "individual") {
          req(input$X_file, input$y_file)
          X <- data.matrix(read.table(input$X_file$datapath))
          y <- data.matrix(read.table(input$y_file$datapath))
          preview_data_store(as.data.frame(X))
          data <- list(X = X, y = y[,1], pos = NULL)
        } else {
          req(input$R_file, input$z_file)
          R <- data.matrix(read.table(input$R_file$datapath))
          z <- data.matrix(read.table(input$z_file$datapath))
          preview_data_store(as.data.frame(R))
          data <- list(R = R, z = z[,1], n = input$n_uploaded, pos = NULL)
        }
      }
      
      # 3. FINNGEN / REAL (Updated Logic String)
      else if (input$input_type == "Upload FinnGen") {
        req(input$finngen_sumstats_file, input$finngen_ld_file)
        log_message("Reading FinnGen files (Header=TRUE)...")
        
        ss_raw <- fread(input$finngen_sumstats_file$datapath, header = TRUE)
        names(ss_raw) <- tolower(names(ss_raw))
        if("#chrom" %in% names(ss_raw)) setnames(ss_raw, "#chrom", "chrom")
        
        ld_raw <- fread(input$finngen_ld_file$datapath, header = TRUE)
        names(ld_raw) <- tolower(names(ld_raw))
        
        preview_data_store(head(ss_raw, 20))
        
        col_beta <- intersect(names(ss_raw), c("beta", "b", "effect"))[1]
        col_se   <- intersect(names(ss_raw), c("sebeta", "se", "std_err"))[1]
        col_af   <- intersect(names(ss_raw), c("af_alt", "af", "maf"))[1]
        
        if(is.na(col_beta) || is.na(col_se)) stop("Could not identify Beta or SE columns.")
        
        # Ensure Numeric
        ss_raw[[col_beta]] <- as.numeric(ss_raw[[col_beta]])
        ss_raw[[col_se]]   <- as.numeric(ss_raw[[col_se]])
        if(!is.na(col_af)) ss_raw[[col_af]] <- as.numeric(ss_raw[[col_af]])
        
        log_message("Constructing IDs for strict matching...")
        has_chr_prefix <- grepl("chr", as.character(ss_raw$chrom[1]), ignore.case = TRUE)
        if(has_chr_prefix) {
          ss_raw$match_id <- paste(ss_raw$chrom, ss_raw$pos, ss_raw$ref, ss_raw$alt, sep = "_")
        } else {
          ss_raw$match_id <- paste0("chr", ss_raw$chrom, "_", ss_raw$pos, "_", ss_raw$ref, "_", ss_raw$alt)
        }
        
        # --- Strict ID Selection ---
        ld_variants <- unique(c(ld_raw$variant1, ld_raw$variant2))
        common_ids <- intersect(ss_raw$match_id, ld_variants)
        
        log_message(paste("SumStats rows:", nrow(ss_raw)))
        log_message(paste("LD Variants:", length(ld_variants)))
        log_message(paste("Strict Intersection:", length(common_ids)))
        
        if(length(common_ids) == 0) stop("No matching IDs found! Check ID format.")
        
        # --- FIX: N Logic (Default Manual) ---
        if(input$auto_N == TRUE && !is.na(col_af)) {
          log_message("Auto-estimating N...")
          maf_val <- ss_raw[[col_af]]
          maf_val <- ifelse(maf_val > 0.5, 1 - maf_val, maf_val)
          maf_val[maf_val == 0] <- 0.001
          n_est <- 1 / (2 * maf_val * (1-maf_val) * ss_raw[[col_se]]^2)
          final_n <- median(n_est, na.rm=TRUE)
          log_message(paste("Auto-estimated N:", round(final_n)))
        } else {
          final_n <- input$finngen_n_manual
          if(is.na(final_n) || final_n < 1) final_n <- 520210
          log_message(paste("Using Manual N:", final_n))
        }
        
        # --- Filter & Deduplicate ---
        ss_subset <- ss_raw[match_id %in% common_ids]
        ss_subset[, z_val := get(col_beta) / get(col_se)]
        ss_subset <- ss_subset[!is.na(z_val) & !is.infinite(z_val)]
        
        # Keep only max Z for duplicate IDs
        ss_subset <- ss_subset[order(abs(z_val), decreasing = TRUE)]
        ss_subset <- ss_subset[!duplicated(match_id)]
        
        # Top N Filter
        target_n <- input$top_n_filter
        if(nrow(ss_subset) > target_n) {
          ss_subset <- ss_subset[1:target_n]
        }
        log_message(paste("Variants after Top-N filter:", nrow(ss_subset)))
        
        # --- BUILD MATRIX ---
        final_target_ids <- ss_subset$match_id
        ld_sub <- ld_raw[variant1 %in% final_target_ids & variant2 %in% final_target_ids]
        
        R_wide <- dcast(ld_sub, variant1 ~ variant2, value.var = "r", fill = 0)
        
        mat_ids <- R_wide$variant1
        R_mat <- as.matrix(R_wide[, -1])
        rownames(R_mat) <- mat_ids
        
        # --- FIX: Matrix Integrity ---
        final_ids <- intersect(ss_subset$match_id, rownames(R_mat))
        
        if(length(final_ids) == 0) stop("No variants valid for matrix construction.")
        
        R_mat <- R_mat[final_ids, final_ids]
        
        # Ensure Diagonal is 1
        diag(R_mat) <- 1
        
        # Ensure symmetry
        R_mat[is.na(R_mat)] <- 0
        R_mat <- (R_mat + t(R_mat)) / 2
        
        ss_final <- ss_subset[match(final_ids, match_id)]
        z_vec <- ss_final$z_val
        names(z_vec) <- final_ids
        
        finngen_table_store(ss_final)
        
        data <- list(R = R_mat, z = z_vec, n = final_n, p = length(z_vec), pos = ss_final$pos)
      }
      
      # --- RUN SUSIE ---
      if (!is.null(data)) {
        if (!is.null(data$X)) {
          # Individual
          log_message("Running SuSiE (Individual)...")
          res <- susie(data$X, data$y, L=input$L, max_iter=input$max_iter, scaled_prior_variance=input$scaled_prior_variance, coverage=input$coverage, track_fit=TRUE)
        } else {
          # Summary Stats
          R_use <- data$R
          lambda <- input$lambda
          if (lambda > 0) {
            log_message(paste("Applying Regularization (Lambda =", lambda, ")..."))
            diag(R_use) <- 1 
            R_use <- (R_use + t(R_use)) / 2
            R_use <- (1 - lambda) * R_use
            diag(R_use) <- diag(R_use) + lambda
          }
          
          nr <- min(10, nrow(R_use)); nc <- min(10, ncol(R_use))
          ld_preview_store(as.data.frame(R_use[1:nr, 1:nc]))
          
          log_message("Running SuSiE RSS (Robust)...")
          
          res <- tryCatch({
            susie_rss(z = data$z, R = R_use, n = data$n, 
                      L = input$L, max_iter = input$max_iter,
                      scaled_prior_variance = input$scaled_prior_variance,
                      coverage = input$coverage, track_fit = TRUE,
                      estimate_residual_variance = FALSE, 
                      check_prior = FALSE)
          }, error = function(e) {
            log_message("Optimization failed. Fallback to Fixed Prior.")
            # Fallback
            susie_rss(z = data$z, R = R_use, n = data$n, 
                      L = input$L, max_iter = input$max_iter,
                      estimate_prior_variance = FALSE, 
                      scaled_prior_variance = 0.1,     
                      coverage = input$coverage, track_fit = TRUE,
                      estimate_residual_variance = FALSE, 
                      check_prior = FALSE)
          })
        }
        
        susie_fit(res)
        susie_trace(res$trace)
        total_steps(length(res$trace))
        data_to_store <- data
        if(!is.null(data$true_coef)) data_to_store$true_coef <- data$true_coef
        plot_data_store(data_to_store)
        log_message("Analysis Successful!")
        showNotification("Analysis Complete!", type = "message")
      }
      
    }, error = function(e) {
      log_message(paste("ERROR:", e$message))
      error_msg(e$message)
    })
  })
  
  # --- PLOTTING LOGIC ---
  output$iteration_slider_ui <- renderUI({ req(susie_trace()); sliderInput("iteration_step", "Iteration:", min=1, max=total_steps(), value=total_steps(), step=1, animate=TRUE) })
  observeEvent(input$next_step, { req(input$iteration_step); if(input$iteration_step < total_steps()) updateSliderInput(session, "iteration_step", value=input$iteration_step+1) })
  output$iteration_info <- renderText({ if(is.null(total_steps()) || total_steps() == 0) return("Waiting for analysis..."); paste("Displaying", input$iteration_step, "of", total_steps()) })
  
  output$pip_plot <- renderPlotly({
    req(susie_trace(), input$iteration_step, plot_data_store())
    step <- input$iteration_step; pip_val <- as.numeric(colSums(susie_trace()[[step]]$alpha)); data <- plot_data_store()
    if (!is.null(data$pos)) { x_val <- data$pos/1e6; x_lab <- "Chromosome Position (Mb)"; hover_txt <- paste("Pos:", data$pos, "<br>PIP:", round(pip_val, 3)) } else { x_val <- 1:length(pip_val); x_lab <- "Index"; hover_txt <- paste("Var:", x_val, "<br>PIP:", round(pip_val, 3)) }
    cols <- rep("gray", length(pip_val)); fit <- susie_fit()
    if (!is.null(fit$sets$cs)) { colors_pal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"); for(i in 1:length(fit$sets$cs)) { idx <- fit$sets$cs[[i]]; idx <- idx[idx <= length(cols)]; cols[idx] <- colors_pal[i %% length(colors_pal) + 1] } }
    plot_ly(x=x_val, y=pip_val, type='scatter', mode='markers', marker=list(color=cols, size=pip_val*15+3), text=hover_txt, hoverinfo='text') %>% layout(title=paste("Iteration", step), xaxis=list(title=x_lab), yaxis=list(title="PIP", range=c(0,1.05)))
  })
  output$elbo_plot <- renderPlotly({ req(susie_fit()); plot_ly(x=1:length(susie_fit()$elbo), y=susie_fit()$elbo, type='scatter', mode='lines+markers') %>% layout(title="ELBO Convergence") })
  output$cs_table <- DT::renderDataTable({
    req(susie_fit()); fit <- susie_fit()
    if (!is.null(finngen_table_store())) {
      df <- finngen_table_store(); if(is.null(fit$sets$cs) || length(fit$sets$cs)==0) return(datatable(data.frame(Message="No Credible Sets")))
      res_list <- list(); pip_vals <- fit$pip
      for (i in 1:length(fit$sets$cs)) {
        cs_indices <- fit$sets$cs[[i]]; cs_indices <- cs_indices[cs_indices <= nrow(df)]
        if(length(cs_indices) > 0) {
          subset_df <- df[cs_indices, ]; subset_pip <- pip_vals[cs_indices]
          col_beta <- intersect(names(subset_df), c("beta", "b", "effect"))[1]; col_pval <- intersect(names(subset_df), c("pval", "p", "p_value"))[1]; col_rsid <- intersect(names(subset_df), c("rsids", "rsid", "variant"))[1]
          temp_res <- data.frame(CS=paste0("L", i), Variant=if(!is.na(col_rsid)) subset_df[[col_rsid]] else subset_df$match_id, Pos=subset_df$pos, PIP=round(subset_pip, 4), Beta=round(as.numeric(if(!is.na(col_beta)) subset_df[[col_beta]] else NA), 4), Pval=formatC(as.numeric(if(!is.na(col_pval)) subset_df[[col_pval]] else NA), format="e", digits=2), Z=round(subset_df$z_val, 2))
          res_list[[i]] <- temp_res
        }
      }
      datatable(do.call(rbind, res_list), options=list(pageLength=10), rownames=FALSE)
    } else {
      if(is.null(fit$sets$cs) || length(fit$sets$cs)==0) return(NULL); cs_df <- data.frame(CS=names(fit$sets$cs), Size=sapply(fit$sets$cs, length), Purity=round(fit$sets$purity[,'min.abs.corr'],3)); datatable(cs_df, options=list(dom='t'))
    }
  })
}

shinyApp(ui, server)