# app_v20.R

library(shiny)
library(plotly)
library(matrixStats)
library(DT)
library(susieR)

# Source the simulation file
source("simulation_v3.R")

# Define UI
ui <- fluidPage(
  titlePanel("SuSiE IBSS Iteration Visualization"),

  sidebarLayout(
    sidebarPanel(
      h3("Controls"),
      selectInput("analysis_type", "Analysis Type:",
                  choices = c("Individual Data" = "individual",
                                "Summary Statistics" = "summary")),
      tabsetPanel(
        id = "input_type",
        tabPanel("Simulated Data",
                 selectInput("data_scenario", "Choose Simulation:",
                             choices = c("Perfect Collinearity" = "perfect_collinearity",
                                         "Moderate LD" = "moderate_ld",
                                         "Multiple LD Blocks" = "multiple_ld_blocks",
                                         "Tricky Scenario" = "tricky_scenario",
                                         "Real Dataset (N3finemapping)" = "real_data"))
        ),
        tabPanel("Upload Your Own Data",
                 conditionalPanel(
                   condition = "input.analysis_type == 'individual'",
                   fileInput("X_file", "Upload Genotype Matrix (X.txt)"),
                   fileInput("y_file", "Upload Phenotype Vector (y.txt)")
                 ),
                 conditionalPanel(
                   condition = "input.analysis_type == 'summary'",
                   fileInput("R_file", "Upload LD Matrix (R.txt)"),
                   fileInput("z_file", "Upload Z-scores (z.txt)"),
                   numericInput("n_uploaded", "Sample Size (n):", value = 1000)
                 )
        )
      ),
      hr(),
      numericInput("L", "Number of single effects (L):", value = 10, min = 1, max = 20),
      numericInput("max_iter", "Maximum Iterations:", value = 100, min = 1, max = 1000),
      numericInput("coverage", "Credible Set (CS) Coverage:", value = 0.95, min = 0, max = 1, step = 0.01),
      numericInput("scaled_prior_variance", "Scaled Prior Variance:", value = 0.1, min = 0, max = 1, step = 0.01),
      actionButton("run_susie", "Run SuSiE Analysis"),
      hr(),
      h4("Iteration Control"),
      uiOutput("iteration_slider_ui"),
      actionButton("next_step", "Next Step"),
      textOutput("iteration_info"),
      hr(),
      h4("Dataset Description"),
      htmlOutput("dataset_description")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("PIP Plot", plotlyOutput("pip_plot")),
        tabPanel("ELBO Convergence", plotlyOutput("elbo_plot")),
        tabPanel("Credible Sets", DT::dataTableOutput("cs_table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  susie_fit <- reactiveVal(NULL)
  susie_trace <- reactiveVal(NULL)
  total_steps <- reactiveVal(0)
  data_for_analysis <- reactiveVal(NULL)

  observeEvent(input$run_susie, {
    showNotification("Running SuSiE analysis...", type = "message", duration = 5)

    # Reset reactive values
    susie_fit(NULL)
    susie_trace(NULL)
    total_steps(0)
    data_for_analysis(NULL)
    
    data <- NULL
    
    if (input$input_type == "Simulated Data") {
      data <- switch(input$data_scenario,
                     "perfect_collinearity" = simulate_perfect_collinearity(),
                     "moderate_ld" = simulate_moderate_ld(),
                     "multiple_ld_blocks" = simulate_multiple_ld_blocks(),
                     "tricky_scenario" = simulate_tricky_scenario(),
                     "real_data" = {
                       data(N3finemapping)
                       list(X = N3finemapping$X, 
                            y = N3finemapping$Y[,1], 
                            true_coef = N3finemapping$true_coef[,1],
                            p = ncol(N3finemapping$X))
                     })
    } else { # Upload Data
      tryCatch({
        if (input$analysis_type == "individual") {
          req(input$X_file, input$y_file)
          X <- data.matrix(read.table(input$X_file$datapath))
          y <- data.matrix(read.table(input$y_file$datapath))
          if (nrow(X) != length(y)) {
            showNotification("Number of rows in X does not match length of y.", type = "error")
            return()
          }
          data <- list(X = X, y = y[,1], p = ncol(X), true_coef = NULL)
        } else { # summary
          req(input$R_file, input$z_file)
          R <- data.matrix(read.table(input$R_file$datapath))
          z <- data.matrix(read.table(input$z_file$datapath))
          if (nrow(R) != ncol(R) || nrow(R) != length(z)) {
            showNotification("Dimensions of R and z are not compatible.", type = "error")
            return()
          }
          R <- (R + t(R)) / 2 # Enforce symmetry
          data <- list(R = R, z = z[,1], p = length(z[,1]), true_coef = NULL)
        }
      }, error = function(e) {
        showNotification(paste("Error reading uploaded files:", e$message), type = "error")
        return()
      })
    }
    
    data_for_analysis(data)

    tryCatch({
      if (input$analysis_type == "individual") {
        res <- susieR::susie(data$X, data$y, L = input$L, max_iter = input$max_iter, 
                             scaled_prior_variance = input$scaled_prior_variance,
                             coverage = input$coverage,
                             track_fit = TRUE)
      } else { # Summary Statistics
        if (input$input_type == "Simulated Data") {
            R <- cor(data$X)
            ss <- susieR::univariate_regression(data$X, data$y)
            z <- ss$betahat / ss$sebetahat
            n_val <- nrow(data$X)
        } else {
            R <- data$R
            z <- data$z
            n_val <- input$n_uploaded
        }
        res <- susieR::susie_rss(z, R, n = n_val, L = input$L, max_iter = input$max_iter,
                                 scaled_prior_variance = input$scaled_prior_variance,
                                 coverage = input$coverage,
                                 track_fit = TRUE)
      }
      
      susie_fit(res)
      susie_trace(res$trace)
      total_steps(length(res$trace))
      
      showNotification("SuSiE analysis complete.", type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("An error occurred during SuSiE analysis:", e$message), type = "error")
    })
  })
  
  output$dataset_description <- renderUI({
    req(input$data_scenario)
    
    desc <- switch(input$data_scenario,
      "perfect_collinearity" = "A simulation with 500 samples and 1000 variables. Two causal variants (200 and 800) with perfect collinearity (r=1) with two other variants (400 and 600 respectively).",
      "moderate_ld" = "A simulation with 500 samples and 1000 variables. Two causal variants (100 and 200) in two separate blocks of moderate LD (r=0.6).",
      "multiple_ld_blocks" = "A simulation with 500 samples and 1000 variables. Three causal variants (50, 150, 250) in three separate blocks with high (r=0.9), moderate (r=0.6), and low (r=0.3) LD.",
      "tricky_scenario" = "A simulation with 500 samples and 1000 variables. Three weak signals in a 50-variable block of high LD (r=0.95).",
      "real_data" = "A real dataset from the `susieR` package. It contains genotype data for 574 samples and 1001 variables from chromosome 19. The phenotype is simulated. See `?N3finemapping` for more details."
    )
    HTML(desc)
  })

  output$iteration_slider_ui <- renderUI({
    if (!is.null(susie_trace())) {
      sliderInput("iteration_step", "IBSS Iteration:",
                  min = 1, max = total_steps(), value = total_steps(), step = 1,
                  animate = animationOptions(interval = 500, loop = FALSE))
    }
  })
  
  observeEvent(input$next_step, {
      req(input$iteration_step)
      current_val <- input$iteration_step
      if (current_val < total_steps()) {
          updateSliderInput(session, "iteration_step", value = current_val + 1)
      }
  })

  output$iteration_info <- renderText({
    if (!is.null(input$iteration_step)) {
      paste("Displaying Iteration:", input$iteration_step, "of", total_steps())
    } else {
      "Click 'Run SuSiE Analysis' to begin."
    }
  })

  output$pip_plot <- renderPlotly({
    trace <- susie_trace()
    step <- input$iteration_step
    fit <- susie_fit()
    data <- data_for_analysis()

    if (!is.null(trace) && !is.null(step) && step > 0 && step <= length(trace)) {
      current_alpha <- trace[[step]]$alpha
      pip_per_variable <- colSums(current_alpha)
      
      cs_colors <- rep("gray", data$p)
      if (!is.null(fit$sets$cs)) {
        colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
        for (i in 1:length(fit$sets$cs)) {
          cs_colors[fit$sets$cs[[i]]] <- colors[i %% length(colors) + 1]
        }
      }
      
      plot_data <- data.frame(
        variable = 1:length(pip_per_variable),
        pip = pip_per_variable,
        color = cs_colors,
        size = pip_per_variable * 20 + 2,
        text = paste("Variable:", 1:length(pip_per_variable), "<br>PIP:", round(pip_per_variable, 3))
      )
      
      p <- plot_ly(plot_data, x = ~variable, y = ~pip, type = 'scatter', mode = 'markers',
                   marker = list(color = ~color, size = ~size),
                   text = ~text, hoverinfo = 'text') %>%
        layout(title = paste("Iteration", step, "- PIP per Variable"),
               xaxis = list(title = "Variable Index"),
               yaxis = list(title = "Posterior Inclusion Probability", range = c(0, 1)),
               showlegend = FALSE)
      
      causal_indices <- if (!is.null(data$true_coef)) which(data$true_coef != 0) else NULL
      if (!is.null(causal_indices)) {
        p <- p %>% add_markers(x = causal_indices, y = 0.02,
                               marker = list(color = 'red', symbol = 'triangle-up', size = 10),
                               hoverinfo = 'text',
                               text = paste("True Causal Variant:", causal_indices),
                               name = "True Causal Variant")
      }
      p
    } else {
      plot_ly() %>% layout(title = "PIP Plot")
    }
  })
  
  output$elbo_plot <- renderPlotly({
    fit <- susie_fit()
    step <- input$iteration_step
    if (!is.null(fit) && !is.null(step)) {
      elbo_data <- data.frame(iteration = 1:length(fit$elbo), elbo = fit$elbo)
      plot_ly(elbo_data, x = ~iteration, y = ~elbo, type = 'scatter', mode = 'lines') %>%
        add_markers(x = step, y = fit$elbo[step], marker = list(color = 'red', size = 10), name = 'Current Iteration') %>%
        layout(title = "ELBO vs. Iteration",
               xaxis = list(title = "Iteration"),
               yaxis = list(title = "ELBO"))
    } else {
      plot_ly() %>% layout(title = "ELBO Convergence Plot")
    }
  })
  
  output$cs_table <- DT::renderDataTable({
    fit <- susie_fit()
    if (!is.null(fit) && !is.null(fit$sets$cs)) {
      sets <- fit$sets
      
      if (length(sets$cs) == 0) {
        return(datatable(data.frame(Message = "No credible sets found."), options = list(dom = 't')))
      }
      
      cs_info <- data.frame(
        CS_Index = sets$cs_index,
        Purity_Min_Abs_Corr = sets$purity[, "min.abs.corr"],
        Purity_Mean_Abs_Corr = sets$purity[, "mean.abs.corr"],
        Coverage = sets$coverage,
        Variables = sapply(sets$cs, function(x) paste(x, collapse = ", "))
      )
      cs_info <- cs_info[order(cs_info$CS_Index), ]
      
      colors <- c("#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5")
      
      datatable(cs_info, options = list(pageLength = 10, scrollX = TRUE)) %>%
        formatStyle(
          'CS_Index',
          target = 'row',
          backgroundColor = styleEqual(cs_info$CS_Index, colors[1:length(cs_info$CS_Index)])
        )
    }
  })
}

shinyApp(ui = ui, server = server)
