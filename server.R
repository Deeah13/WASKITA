# =============================================================================
# WASKITA Professional Dashboard - Server Logic
# Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik
# Professional Statistical Analysis Platform
# =============================================================================

source("global.R")

server <- function(input, output, session) {
  
  # =============================================================================
  # REACTIVE VALUES SYSTEM
  # =============================================================================
  
  values <- reactiveValues(
    # Core data storage
    original_data = NULL,
    processed_data = NULL,
    distance_data = NULL,
    
    # Analysis status flags
    data_loaded = FALSE,
    transformation_done = FALSE,
    categorization_done = FALSE,
    desc_generated = FALSE,
    viz_generated = FALSE,
    map_generated = FALSE,
    
    # Statistical analysis flags
    normality_done = FALSE,
    homogeneity_done = FALSE,
    ttest_done = FALSE,
    anova_done = FALSE,
    regression_done = FALSE,
    spatial_done = FALSE,
    
    # Analysis results storage
    current_desc_variable = NULL,
    transformation_result = NULL,
    categorization_result = NULL,
    desc_stats = NULL,
    desc_plot = NULL
  )
  
  # =============================================================================
  # DATA LOADING AND MANAGEMENT
  # =============================================================================
  
  observeEvent(input$load_data, {
    tryCatch({
      notify_success("Loading dataset...")
      
      # Load data using enhanced global function
      data_result <- load_waskita_data()
      
      if ("error" %in% names(data_result)) {
        notify_error(data_result$message)
        return()
      }
      
      # Store data in reactive values
      values$original_data <- data_result$sovi
      values$processed_data <- data_result$sovi
      values$distance_data <- data_result$distance
      values$data_loaded <- TRUE
      
      # Update all variable choices dynamically
      numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      categorical_vars <- names(values$processed_data)[sapply(values$processed_data, function(x) is.character(x) | is.factor(x))]
      all_vars <- names(values$processed_data)
      
      # Update selectInput choices for all relevant inputs
      updateSelectInput(session, "transform_variable", choices = numeric_vars)
      updateSelectInput(session, "categorize_variable", choices = numeric_vars)
      updateSelectInput(session, "desc_variable", choices = all_vars)
      
      notify_success(paste("Dataset loaded successfully!", data_result$message))
      
    }, error = function(e) {
      notify_error(paste("Failed to load data:", e$message))
    })
  })
  
  # Data loaded status output
  output$data_loaded <- reactive({
    values$data_loaded
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Enhanced data summary
  output$data_summary <- renderPrint({
    if (values$data_loaded) {
      data <- values$processed_data
      
      cat("=== WASKITA DATASET SUMMARY ===\n")
      cat("================================\n")
      cat("Observations:", nrow(data), "\n")
      cat("Variables:", ncol(data), "\n")
      cat("Missing Values:", sum(is.na(data)), 
          paste0("(", round(sum(is.na(data))/(nrow(data)*ncol(data))*100, 2), "%)"), "\n")
      cat("Numeric Variables:", sum(sapply(data, is.numeric)), "\n")
      cat("Categorical Variables:", sum(sapply(data, function(x) is.character(x) | is.factor(x))), "\n")
      cat("Memory Usage:", format(object.size(data), units = "MB"), "\n")
      cat("Data Source:", ifelse(exists("data_result"), data_result$source, "Unknown"), "\n")
      cat("================================\n")
      
      # Show first few variable names
      cat("Variables Preview:\n")
      var_names <- names(data)[1:min(8, ncol(data))]
      cat(paste(var_names, collapse = ", "))
      if (ncol(data) > 8) cat(", ...")
      cat("\n")
    }
  })
  
  # Enhanced data preview table
  output$data_preview <- DT::renderDataTable({
    if (values$data_loaded) {
      DT::datatable(
        values$processed_data,
        options = list(
          scrollX = TRUE,
          scrollY = "400px",
          pageLength = 15,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf'),
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        class = 'cell-border stripe hover',
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = sapply(values$processed_data, is.numeric), digits = 4)
    }
  })
  
  # Professional metadata table
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(
      sovi_metadata,
      options = list(
        pageLength = 17,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        ordering = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = c(2, 3, 4)),
          list(width = '200px', targets = 1),
          list(width = '100px', targets = c(2, 3, 4))
        )
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE
    )
  })
  
  # =============================================================================
  # DATA TRANSFORMATION
  # =============================================================================
  
  observeEvent(input$apply_transformation, {
    req(values$processed_data, input$transform_variable, input$transform_method)
    
    tryCatch({
      var_name <- input$transform_variable
      method <- input$transform_method
      original_data <- values$processed_data[[var_name]]
      
      # Enhanced validation
      if (!is.numeric(original_data)) {
        notify_warning("Transformation requires numeric variables only")
        return()
      }
      
      if (all(is.na(original_data))) {
        notify_error("Selected variable contains only missing values")
        return()
      }
      
      # Apply transformation with enhanced error handling
      transformed_data <- switch(method,
        "log" = {
          if (any(original_data <= 0, na.rm = TRUE)) {
            notify_error("Natural log transformation requires positive values")
            return()
          }
          log(original_data)
        },
        "log10" = {
          if (any(original_data <= 0, na.rm = TRUE)) {
            notify_error("Log10 transformation requires positive values")
            return()
          }
          log10(original_data)
        },
        "sqrt" = {
          if (any(original_data < 0, na.rm = TRUE)) {
            notify_error("Square root transformation requires non-negative values")
            return()
          }
          sqrt(original_data)
        },
        "square" = original_data^2,
        "boxcox" = {
          if (any(original_data <= 0, na.rm = TRUE)) {
            notify_error("Box-Cox transformation requires positive values")
            return()
          }
          if (requireNamespace("forecast", quietly = TRUE)) {
            lambda <- forecast::BoxCox.lambda(original_data)
            forecast::BoxCox(original_data, lambda = lambda)
          } else {
            notify_error("forecast package required for Box-Cox transformation")
            return()
          }
        },
        "zscore" = {
          (original_data - mean(original_data, na.rm = TRUE)) / sd(original_data, na.rm = TRUE)
        }
      )
      
      # Create new variable name and store
      new_col_name <- paste0(var_name, "_", method)
      values$processed_data[[new_col_name]] <- transformed_data
      values$transformation_done <- TRUE
      values$transformation_result <- list(
        original_var = var_name,
        new_var = new_col_name,
        method = method,
        original_stats = summary(original_data),
        transformed_stats = summary(transformed_data)
      )
      
      # Update variable choices
      numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      updateSelectInput(session, "transform_variable", choices = numeric_vars)
      updateSelectInput(session, "desc_variable", choices = names(values$processed_data))
      
      notify_success(paste("Transformation", method, "applied successfully to", var_name))
      
    }, error = function(e) {
      notify_error(paste("Transformation failed:", e$message))
    })
  })
  
  # Transformation status
  output$transformation_done <- reactive({
    values$transformation_done
  })
  outputOptions(output, "transformation_done", suspendWhenHidden = FALSE)
  
  # Enhanced transformation results
  output$transformation_result <- renderPrint({
    if (values$transformation_done && !is.null(values$transformation_result)) {
      result <- values$transformation_result
      
      cat("=== TRANSFORMATION RESULTS ===\n")
      cat("===============================\n")
      cat("Original Variable:", result$original_var, "\n")
      cat("New Variable:", result$new_var, "\n")
      cat("Method:", result$method, "\n\n")
      
      cat("ORIGINAL STATISTICS:\n")
      print(result$original_stats)
      
      cat("\nTRANSFORMED STATISTICS:\n")
      print(result$transformed_stats)
      
      cat("\nIMPROVEMENT METRICS:\n")
      orig_data <- values$original_data[[result$original_var]]
      trans_data <- values$processed_data[[result$new_var]]
      
      if (requireNamespace("moments", quietly = TRUE)) {
        orig_skew <- moments::skewness(orig_data, na.rm = TRUE)
        trans_skew <- moments::skewness(trans_data, na.rm = TRUE)
        cat("Skewness Reduction:", round(abs(orig_skew) - abs(trans_skew), 4), "\n")
        
        orig_kurt <- moments::kurtosis(orig_data, na.rm = TRUE)
        trans_kurt <- moments::kurtosis(trans_data, na.rm = TRUE)
        cat("Kurtosis Change:", round(orig_kurt - trans_kurt, 4), "\n")
      }
    }
  })
  
  # =============================================================================
  # DATA CATEGORIZATION
  # =============================================================================
  
  observeEvent(input$apply_categorization, {
    req(values$processed_data, input$categorize_variable, input$categorize_method)
    
    tryCatch({
      var_name <- input$categorize_variable
      var_data <- values$processed_data[[var_name]]
      
      if (!is.numeric(var_data)) {
        notify_warning("Categorization requires numeric variables only")
        return()
      }
      
      if (all(is.na(var_data))) {
        notify_error("Selected variable contains only missing values")
        return()
      }
      
      # Apply categorization method
      if (input$categorize_method == "quartile") {
        breaks <- quantile(var_data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        labels <- c("Q1 (Low)", "Q2 (Medium-Low)", "Q3 (Medium-High)", "Q4 (High)")
      } else if (input$categorize_method == "tertile") {
        breaks <- quantile(var_data, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
        labels <- c("Low", "Medium", "High")
      } else if (input$categorize_method == "quintile") {
        breaks <- quantile(var_data, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
        labels <- c("Very Low", "Low", "Medium", "High", "Very High")
      } else { # custom
        thresholds <- as.numeric(unlist(strsplit(input$custom_thresholds, ",")))
        if (any(is.na(thresholds))) {
          notify_error("Invalid threshold values. Please use numeric values separated by commas.")
          return()
        }
        breaks <- c(min(var_data, na.rm = TRUE), sort(thresholds), max(var_data, na.rm = TRUE))
        breaks <- sort(unique(breaks))
        labels <- paste0("Category_", 1:(length(breaks)-1))
      }
      
      # Create categorical variable
      categorized_var <- cut(var_data, breaks = breaks, labels = labels, include.lowest = TRUE)
      new_col_name <- paste0(var_name, "_cat")
      values$processed_data[[new_col_name]] <- categorized_var
      values$categorization_done <- TRUE
      
      # Store results
      values$categorization_result <- list(
        original_var = var_name,
        new_var = new_col_name,
        method = input$categorize_method,
        breaks = breaks,
        labels = labels,
        distribution = table(categorized_var, useNA = "ifany")
      )
      
      # Update variable choices
      categorical_vars <- names(values$processed_data)[sapply(values$processed_data, function(x) is.character(x) | is.factor(x))]
      updateSelectInput(session, "desc_variable", choices = names(values$processed_data))
      
      notify_success(paste("Categorization completed:", new_col_name, "created"))
      
    }, error = function(e) {
      notify_error(paste("Categorization failed:", e$message))
    })
  })
  
  # Categorization status
  output$categorization_done <- reactive({
    values$categorization_done
  })
  outputOptions(output, "categorization_done", suspendWhenHidden = FALSE)
  
  # Enhanced categorization results
  output$categorization_result <- renderPrint({
    if (values$categorization_done && !is.null(values$categorization_result)) {
      result <- values$categorization_result
      
      cat("=== CATEGORIZATION RESULTS ===\n")
      cat("===============================\n")
      cat("Original Variable:", result$original_var, "\n")
      cat("New Variable:", result$new_var, "\n")
      cat("Method:", result$method, "\n\n")
      
      cat("BREAKPOINTS:\n")
      cat(paste(round(result$breaks, 4), collapse = " | "), "\n\n")
      
      cat("CATEGORY LABELS:\n")
      cat(paste(result$labels, collapse = " | "), "\n\n")
      
      cat("DISTRIBUTION:\n")
      print(result$distribution)
      
      cat("\nPROPORTIONS (%):\n")
      props <- prop.table(result$distribution) * 100
      print(round(props, 2))
    }
  })
  
  # =============================================================================
  # DESCRIPTIVE STATISTICS
  # =============================================================================
  
  # Check if selected variable is numeric
  output$is_numeric_var <- reactive({
    if (values$data_loaded && !is.null(input$desc_variable)) {
      return(is.numeric(values$processed_data[[input$desc_variable]]))
    }
    return(FALSE)
  })
  outputOptions(output, "is_numeric_var", suspendWhenHidden = FALSE)
  
  # Generate descriptive analysis
  observeEvent(input$generate_desc, {
    req(values$processed_data, input$desc_variable)
    
    tryCatch({
      values$current_desc_variable <- input$desc_variable
      values$desc_generated <- TRUE
      
      # Generate statistics
      var_data <- values$processed_data[[input$desc_variable]]
      
      if (is.numeric(var_data)) {
        values$desc_stats <- list(
          n = sum(!is.na(var_data)),
          missing = sum(is.na(var_data)),
          mean = mean(var_data, na.rm = TRUE),
          median = median(var_data, na.rm = TRUE),
          sd = sd(var_data, na.rm = TRUE),
          min = min(var_data, na.rm = TRUE),
          max = max(var_data, na.rm = TRUE),
          q1 = quantile(var_data, 0.25, na.rm = TRUE),
          q3 = quantile(var_data, 0.75, na.rm = TRUE),
          iqr = IQR(var_data, na.rm = TRUE)
        )
        
        # Add advanced statistics if packages available
        if (requireNamespace("moments", quietly = TRUE)) {
          values$desc_stats$skewness <- moments::skewness(var_data, na.rm = TRUE)
          values$desc_stats$kurtosis <- moments::kurtosis(var_data, na.rm = TRUE)
        }
      } else {
        values$desc_stats <- table(var_data, useNA = "ifany")
      }
      
      notify_success("Descriptive analysis generated successfully")
      
    }, error = function(e) {
      notify_error(paste("Analysis generation failed:", e$message))
    })
  })
  
  # Descriptive analysis status
  output$desc_generated <- reactive({
    values$desc_generated
  })
  outputOptions(output, "desc_generated", suspendWhenHidden = FALSE)
  
  # Enhanced descriptive plot
  output$desc_plot <- renderPlotly({
    if (values$desc_generated && !is.null(values$current_desc_variable)) {
      
      tryCatch({
        var_data <- values$processed_data[[values$current_desc_variable]]
        var_name <- values$current_desc_variable
        
        if (is.numeric(var_data)) {
          # Numeric variable plots
          p <- switch(input$desc_chart_type,
            "histogram" = {
              ggplot(values$processed_data, aes_string(x = var_name)) +
                geom_histogram(bins = input$desc_bins, 
                               fill = waskita_colors$primary, 
                               alpha = 0.7, 
                               color = "white",
                               size = 0.3) +
                labs(title = paste("Histogram:", var_name),
                     x = var_name, 
                     y = "Frequency",
                     caption = paste("Bins:", input$desc_bins)) +
                theme_waskita_professional() +
                theme(plot.title = element_text(size = 16, face = "bold"))
            },
            "boxplot" = {
              ggplot(values$processed_data, aes_string(y = var_name)) +
                geom_boxplot(fill = waskita_colors$primary, 
                             alpha = 0.7, 
                             color = waskita_colors$secondary,
                             outlier.color = waskita_colors$error,
                             outlier.size = 2) +
                labs(title = paste("Box Plot:", var_name),
                     y = var_name) +
                theme_waskita_professional() +
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank())
            },
            "density" = {
              ggplot(values$processed_data, aes_string(x = var_name)) +
                geom_density(fill = waskita_colors$primary, 
                             alpha = 0.7, 
                             color = waskita_colors$secondary,
                             size = 1) +
                geom_vline(aes(xintercept = mean(var_data, na.rm = TRUE)),
                           color = waskita_colors$error, 
                           linetype = "dashed", 
                           size = 1) +
                labs(title = paste("Density Plot:", var_name),
                     x = var_name, 
                     y = "Density",
                     caption = "Dashed line: Mean") +
                theme_waskita_professional()
            },
            "qq" = {
              ggplot(values$processed_data, aes_string(sample = var_name)) +
                stat_qq(color = waskita_colors$primary, size = 2, alpha = 0.7) +
                stat_qq_line(color = waskita_colors$secondary, size = 1) +
                labs(title = paste("Q-Q Plot:", var_name),
                     x = "Theoretical Quantiles",
                     y = "Sample Quantiles") +
                theme_waskita_professional()
            },
            "violin" = {
              ggplot(values$processed_data, aes_string(x = '""', y = var_name)) +
                geom_violin(fill = waskita_colors$primary, 
                            alpha = 0.7, 
                            color = waskita_colors$secondary) +
                geom_boxplot(width = 0.1, 
                             fill = "white", 
                             alpha = 0.8,
                             outlier.color = waskita_colors$error) +
                labs(title = paste("Violin Plot:", var_name),
                     y = var_name) +
                theme_waskita_professional() +
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.title.x = element_blank())
            }
          )
        } else {
          # Categorical variable plots
          df <- as.data.frame(table(var_data))
          colnames(df) <- c("Category", "Frequency")
          
          p <- if (input$desc_chart_type == "piechart") {
            ggplot(df, aes(x = "", y = Frequency, fill = Category)) +
              geom_bar(stat = "identity", width = 1) +
              coord_polar("y", start = 0) +
              labs(title = paste("Pie Chart:", var_name)) +
              theme_waskita_professional() +
              theme(axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    axis.title = element_blank()) +
              scale_fill_manual(values = c(waskita_colors$chart_1, waskita_colors$chart_2, 
                                           waskita_colors$chart_3, waskita_colors$chart_4,
                                           waskita_colors$chart_5, waskita_colors$chart_6))
          } else {
            ggplot(df, aes(x = reorder(Category, -Frequency), y = Frequency, fill = Category)) +
              geom_bar(stat = "identity", alpha = 0.8) +
              geom_text(aes(label = Frequency), vjust = -0.5, fontface = "bold") +
              labs(title = paste("Bar Chart:", var_name),
                   x = var_name, 
                   y = "Frequency") +
              theme_waskita_professional() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.position = "none") +
              scale_fill_manual(values = c(waskita_colors$chart_1, waskita_colors$chart_2, 
                                           waskita_colors$chart_3, waskita_colors$chart_4,
                                           waskita_colors$chart_5, waskita_colors$chart_6))
          }
        }
        
        # Convert to plotly with enhanced interactivity
        ggplotly(p, tooltip = "all") %>%
          layout(
            font = list(family = "Inter", size = 12),
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)',
            hoverlabel = list(
              bgcolor = waskita_colors$primary,
              font = list(color = "white", family = "Inter")
            )
          ) %>%
          config(
            displayModeBar = TRUE,
            modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d"),
            displaylogo = FALSE
          )
        
      }, error = function(e) {
        notify_error(paste("Plot generation failed:", e$message))
        return(NULL)
      })
    }
  })
  
  # Enhanced descriptive statistics output
  output$desc_stats <- renderPrint({
    if (values$desc_generated && !is.null(values$desc_stats)) {
      var_name <- values$current_desc_variable
      
      cat("=== DESCRIPTIVE STATISTICS ===\n")
      cat("===============================\n")
      cat("Variable:", var_name, "\n")
      cat("Type:", class(values$processed_data[[var_name]])[1], "\n")
      cat("===============================\n")
      
      if (is.list(values$desc_stats)) {
        # Numeric variable statistics
        stats <- values$desc_stats
        cat("Sample Size (n):", stats$n, "\n")
        cat("Missing Values:", stats$missing, 
            paste0("(", round(stats$missing/(stats$n + stats$missing)*100, 2), "%)"), "\n\n")
        
        cat("CENTRAL TENDENCY:\n")
        cat("Mean:", round(stats$mean, 4), "\n")
        cat("Median:", round(stats$median, 4), "\n")
        cat("Difference:", round(stats$mean - stats$median, 4), "\n\n")
        
        cat("VARIABILITY:\n")
        cat("Standard Deviation:", round(stats$sd, 4), "\n")
        cat("Variance:", round(stats$sd^2, 4), "\n")
        cat("Coefficient of Variation:", round(stats$sd/abs(stats$mean)*100, 2), "%\n")
        cat("Range:", round(stats$max - stats$min, 4), "\n")
        cat("IQR:", round(stats$iqr, 4), "\n\n")
        
        cat("DISTRIBUTION:\n")
        cat("Minimum:", round(stats$min, 4), "\n")
        cat("Q1 (25th percentile):", round(stats$q1, 4), "\n")
        cat("Q2 (50th percentile):", round(stats$median, 4), "\n")
        cat("Q3 (75th percentile):", round(stats$q3, 4), "\n")
        cat("Maximum:", round(stats$max, 4), "\n\n")
        
        if (!is.null(stats$skewness)) {
          cat("SHAPE STATISTICS:\n")
          cat("Skewness:", round(stats$skewness, 4), "\n")
          cat("Kurtosis:", round(stats$kurtosis, 4), "\n")
        }
        
      } else {
        # Categorical variable statistics
        cat("FREQUENCY DISTRIBUTION:\n")
        print(values$desc_stats)
        
        cat("\nPROPORTIONS (%):\n")
        props <- prop.table(values$desc_stats) * 100
        print(round(props, 2))
        
        cat("\nSUMMARY:\n")
        cat("Categories:", length(values$desc_stats), "\n")
        cat("Most Frequent:", names(which.max(values$desc_stats)), 
            paste0("(", max(values$desc_stats), " cases, ", 
                   round(max(props), 2), "%)"), "\n")
        cat("Least Frequent:", names(which.min(values$desc_stats)), 
            paste0("(", min(values$desc_stats), " cases, ", 
                   round(min(props), 2), "%)"), "\n")
      }
    }
  })
  
  # Professional interpretation
  output$desc_interpretation <- renderPrint({
    if (values$desc_generated && !is.null(values$desc_stats)) {
      var_name <- values$current_desc_variable
      
      cat("=== PROFESSIONAL INTERPRETATION ===\n")
      cat("====================================\n")
      
      if (is.list(values$desc_stats)) {
        # Numeric variable interpretation
        stats <- values$desc_stats
        
        cat("1. DATA QUALITY ASSESSMENT:\n")
        missing_pct <- stats$missing/(stats$n + stats$missing)*100
        if (missing_pct == 0) {
          cat("   ✓ Complete data - no missing values\n")
        } else if (missing_pct < 5) {
          cat("   ⚠ Low missing data rate (", round(missing_pct, 1), "%)\n")
        } else if (missing_pct < 20) {
          cat("   ⚠ Moderate missing data rate (", round(missing_pct, 1), "%)\n")
        } else {
          cat("   ✗ High missing data rate (", round(missing_pct, 1), "%) - Consider imputation\n")
        }
        
        cat("\n2. CENTRAL TENDENCY:\n")
        mean_median_diff <- abs(stats$mean - stats$median) / stats$sd
        if (mean_median_diff < 0.1) {
          cat("   ✓ Distribution is approximately SYMMETRIC (mean ≈ median)\n")
        } else if (stats$mean > stats$median) {
          cat("   → Distribution is RIGHT-SKEWED (mean > median)\n")
          cat("   → Consider log transformation if appropriate\n")
        } else {
          cat("   → Distribution is LEFT-SKEWED (mean < median)\n")
          cat("   → Consider square transformation if appropriate\n")
        }
        
        cat("\n3. VARIABILITY ASSESSMENT:\n")
        cv <- stats$sd/abs(stats$mean)*100
        if (cv < 15) {
          cat("   ✓ LOW variability (CV = ", round(cv, 1), "%) - Homogeneous data\n")
        } else if (cv < 30) {
          cat("   → MODERATE variability (CV = ", round(cv, 1), "%) - Acceptable spread\n")
        } else {
          cat("   ⚠ HIGH variability (CV = ", round(cv, 1), "%) - Heterogeneous data\n")
        }
        
        cat("\n4. OUTLIER DETECTION:\n")
        Q1 <- stats$q1
        Q3 <- stats$q3
        IQR_val <- stats$iqr
        lower_fence <- Q1 - 1.5 * IQR_val
        upper_fence <- Q3 + 1.5 * IQR_val
        var_data <- values$processed_data[[var_name]]
        outliers <- sum(var_data < lower_fence | var_data > upper_fence, na.rm = TRUE)
        
        if (outliers == 0) {
          cat("   ✓ No outliers detected using IQR method\n")
        } else {
          outlier_pct <- outliers / stats$n * 100
          cat("   ⚠", outliers, "potential outliers detected (", round(outlier_pct, 1), "%)\n")
          cat("   → Values outside [", round(lower_fence, 2), ", ", round(upper_fence, 2), "]\n")
        }
        
        if (!is.null(stats$skewness)) {
          cat("\n5. DISTRIBUTION SHAPE:\n")
          skew <- abs(stats$skewness)
          if (skew < 0.5) {
            cat("   ✓ Approximately NORMAL distribution (|skewness| < 0.5)\n")
          } else if (skew < 1) {
            cat("   → MODERATELY skewed distribution (0.5 ≤ |skewness| < 1)\n")
          } else {
            cat("   ⚠ HIGHLY skewed distribution (|skewness| ≥ 1)\n")
            cat("   → Strong transformation recommended\n")
          }
          
          kurt <- stats$kurtosis
          if (kurt < 3) {
            cat("   → PLATYKURTIC: Flatter than normal (light tails)\n")
          } else if (kurt > 3) {
            cat("   → LEPTOKURTIC: More peaked than normal (heavy tails)\n")
          } else {
            cat("   ✓ MESOKURTIC: Similar to normal distribution\n")
          }
        }
        
        cat("\n6. RECOMMENDATIONS:\n")
        if (missing_pct > 5) {
          cat("   • Consider missing data imputation strategies\n")
        }
        if (mean_median_diff > 0.2) {
          cat("   • Apply appropriate transformation for normality\n")
        }
        if (outliers > 0) {
          cat("   • Investigate outliers - verify data quality\n")
        }
        if (cv > 50) {
          cat("   • Consider stratification or grouping analysis\n")
        }
        
      } else {
        # Categorical variable interpretation
        tbl <- values$desc_stats
        props <- prop.table(tbl) * 100
        
        cat("1. DISTRIBUTION ANALYSIS:\n")
        n_categories <- length(tbl)
        dominant_pct <- max(props)
        balance_ratio <- max(props) / min(props)
        
        cat("   Categories:", n_categories, "\n")
        cat("   Dominant category:", names(which.max(tbl)), 
            paste0("(", round(dominant_pct, 1), "%)"), "\n")
        
        if (balance_ratio < 2) {
          cat("   ✓ WELL-BALANCED distribution\n")
        } else if (balance_ratio < 5) {
          cat("   → MODERATELY imbalanced distribution\n")
        } else {
          cat("   ⚠ HIGHLY imbalanced distribution (ratio: ", round(balance_ratio, 1), ":1)\n")
        }
        
        cat("\n2. DATA QUALITY:\n")
        if (any(names(tbl) == "NA")) {
          missing_n <- tbl[names(tbl) == "NA"]
          missing_pct <- missing_n / sum(tbl) * 100
          cat("   ⚠ Missing values present:", missing_n, 
              paste0("(", round(missing_pct, 1), "%)"), "\n")
        } else {
          cat("   ✓ No missing values detected\n")
        }
        
        cat("\n3. STATISTICAL PROPERTIES:\n")
        entropy <- -sum(props/100 * log2(props/100), na.rm = TRUE)
        max_entropy <- log2(n_categories)
        normalized_entropy <- entropy / max_entropy
        
        cat("   Information content:", round(normalized_entropy * 100, 1), "%\n")
        if (normalized_entropy > 0.8) {
          cat("   ✓ HIGH diversity - categories well distributed\n")
        } else if (normalized_entropy > 0.5) {
          cat("   → MODERATE diversity\n")
        } else {
          cat("   ⚠ LOW diversity - dominated by few categories\n")
        }
        
        cat("\n4. RECOMMENDATIONS:\n")
        if (balance_ratio > 5) {
          cat("   • Consider grouping rare categories\n")
          cat("   • Use stratified sampling for analysis\n")
        }
        if (n_categories > 10) {
          cat("   • Consider category consolidation\n")
        }
        if (any(names(tbl) == "NA")) {
          cat("   • Address missing value treatment\n")
        }
      }
    }
  })
  
  # Download handlers would be implemented here...
  # (Continuing with remaining server logic...)
}
