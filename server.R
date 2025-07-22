# =============================================================================
# WASKITA Dashboard - Server Logic
# Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik
# =============================================================================

source("global.R")

server <- function(input, output, session) {
  
  # =============================================================================
  # REACTIVE VALUES
  # =============================================================================
  
  values <- reactiveValues(
    # Data storage
    original_data = NULL,
    processed_data = NULL,
    distance_data = NULL,
    
    # Status flags
    data_loaded = FALSE,
    transformation_done = FALSE,
    categorization_done = FALSE,
    desc_generated = FALSE,
    viz_generated = FALSE,
    map_generated = FALSE,
    normality_done = FALSE,
    homogeneity_done = FALSE,
    ttest_done = FALSE,
    prop_test_done = FALSE,
    var_test_done = FALSE,
    anova_done = FALSE,
    regression_done = FALSE,
    spatial_done = FALSE,
    
    # Analysis results
    current_desc_variable = NULL,
    current_viz_type = NULL,
    normality_result = NULL,
    homogeneity_result = NULL,
    ttest_result = NULL,
    prop_test_result = NULL,
    var_test_result = NULL,
    anova_result = NULL,
    regression_model = NULL,
    spatial_result = NULL,
    map_data = NULL
  )
  
  # =============================================================================
  # DATA LOADING
  # =============================================================================
  
  observeEvent(input$load_data, {
    tryCatch({
      # Load data using global function
      data_result <- load_waskita_data()
      
      if ("error" %in% names(data_result)) {
        safe_notification(data_result$error, "error")
        return()
      }
      
      values$original_data <- data_result$sovi
      values$processed_data <- data_result$sovi
      values$distance_data <- data_result$distance
      values$data_loaded <- TRUE
      
      # Update variable choices
      numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      categorical_vars <- names(values$processed_data)[sapply(values$processed_data, function(x) is.character(x) | is.factor(x))]
      all_vars <- names(values$processed_data)
      
      # Update all selectInput choices
      choices_list <- list(
        transform_variable = numeric_vars,
        categorize_variable = numeric_vars,
        desc_variable = all_vars,
        scatter_x = numeric_vars,
        scatter_y = numeric_vars,
        scatter_color = c("None" = "", categorical_vars),
        boxplot_numeric = numeric_vars,
        boxplot_group = categorical_vars,
        hist_variables = numeric_vars,
        map_variable = numeric_vars,
        normality_variable = numeric_vars,
        homogeneity_numeric = numeric_vars,
        homogeneity_group = categorical_vars,
        ttest_variable = numeric_vars,
        ttest_group = categorical_vars,
        ttest_paired_var = numeric_vars,
        prop_variable = categorical_vars,
        prop_group = categorical_vars,
        var_variable = numeric_vars,
        var_group = categorical_vars,
        anova_dependent = numeric_vars,
        anova_factor1 = categorical_vars,
        anova_factor2 = categorical_vars,
        reg_dependent = numeric_vars,
        reg_independent = numeric_vars,
        spatial_variable = numeric_vars
      )
      
      # Update choices for all inputs
      for (input_id in names(choices_list)) {
        if (input_id %in% names(input)) {
          updateSelectInput(session, input_id, choices = choices_list[[input_id]])
        }
      }
      
      safe_notification(paste("Data berhasil dimuat! Source:", data_result$source), "success")
      
    }, error = function(e) {
      safe_notification(paste("Error memuat data:", e$message), "error")
    })
  })
  
  # Data loaded status
  output$data_loaded <- reactive({
    values$data_loaded
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Data summary
  output$data_summary <- renderPrint({
    if (values$data_loaded) {
      cat("Dataset WASKITA\n")
      cat("================\n")
      cat("Baris:", nrow(values$processed_data), "\n")
      cat("Kolom:", ncol(values$processed_data), "\n")
      cat("Missing Values:", sum(is.na(values$processed_data)), "\n")
      cat("Numerik:", sum(sapply(values$processed_data, is.numeric)), "\n")
      cat("Kategorikal:", sum(sapply(values$processed_data, function(x) is.character(x) | is.factor(x))), "\n")
    }
  })
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    if (values$data_loaded) {
      DT::datatable(
        values$processed_data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = 'cell-border stripe'
      )
    }
  })
  
  # Metadata table
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(
      sovi_metadata,
      options = list(
        pageLength = 17,
        dom = 't',
        ordering = FALSE
      ),
      class = 'cell-border stripe'
    )
  })
  
  # =============================================================================
  # TRANSFORMASI DATA
  # =============================================================================
  
  observeEvent(input$apply_transformation, {
    req(values$processed_data, input$transform_variable, input$transform_method)
    
    tryCatch({
      var_name <- input$transform_variable
      method <- input$transform_method
      original_data <- values$processed_data[[var_name]]
      
      if (!is.numeric(original_data)) {
        safe_notification("Transformasi hanya untuk variabel numerik", "warning")
        return()
      }
      
      transformed_data <- switch(method,
                                 "log" = {
                                   if (any(original_data <= 0, na.rm = TRUE)) {
                                     safe_notification("Log transformasi memerlukan nilai positif", "error")
                                     return()
                                   }
                                   log(original_data)
                                 },
                                 "log10" = {
                                   if (any(original_data <= 0, na.rm = TRUE)) {
                                     safe_notification("Log10 transformasi memerlukan nilai positif", "error")
                                     return()
                                   }
                                   log10(original_data)
                                 },
                                 "sqrt" = {
                                   if (any(original_data < 0, na.rm = TRUE)) {
                                     safe_notification("Sqrt transformasi memerlukan nilai non-negatif", "error")
                                     return()
                                   }
                                   sqrt(original_data)
                                 },
                                 "square" = original_data^2,
                                 "boxcox" = {
                                   if (any(original_data <= 0, na.rm = TRUE)) {
                                     safe_notification("Box-Cox transformasi memerlukan nilai positif", "error")
                                     return()
                                   }
                                   lambda <- forecast::BoxCox.lambda(original_data)
                                   forecast::BoxCox(original_data, lambda = lambda)
                                 }
      )
      
      new_col_name <- paste0(var_name, "_", method)
      values$processed_data[[new_col_name]] <- transformed_data
      values$transformation_done <- TRUE
      
      # Update choices
      numeric_vars <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      updateSelectInput(session, "transform_variable", choices = numeric_vars)
      
      safe_notification(paste("Transformasi", method, "berhasil diterapkan"), "success")
      
    }, error = function(e) {
      safe_notification(paste("Error transformasi:", e$message), "error")
    })
  })
  
  output$transformation_done <- reactive({
    values$transformation_done
  })
  outputOptions(output, "transformation_done", suspendWhenHidden = FALSE)
  
  output$transformation_result <- renderPrint({
    if (values$transformation_done) {
      new_col_name <- paste0(input$transform_variable, "_", input$transform_method)
      if (new_col_name %in% names(values$processed_data)) {
        new_data <- values$processed_data[[new_col_name]]
        cat("Hasil Transformasi:", new_col_name, "\n")
        cat("====================\n")
        cat("Mean:", round(mean(new_data, na.rm = TRUE), 4), "\n")
        cat("Median:", round(median(new_data, na.rm = TRUE), 4), "\n")
        cat("SD:", round(sd(new_data, na.rm = TRUE), 4), "\n")
        cat("Min:", round(min(new_data, na.rm = TRUE), 4), "\n")
        cat("Max:", round(max(new_data, na.rm = TRUE), 4), "\n")
      }
    }
  })
  
  # =============================================================================
  # KATEGORISASI DATA
  # =============================================================================
  
  observeEvent(input$apply_categorization, {
    req(values$processed_data, input$categorize_variable, input$categorize_method)
    
    tryCatch({
      var_name <- input$categorize_variable
      var_data <- values$processed_data[[var_name]]
      
      if (!is.numeric(var_data)) {
        safe_notification("Kategorisasi hanya untuk variabel numerik", "warning")
        return()
      }
      
      if (input$categorize_method == "quartile") {
        breaks <- quantile(var_data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        labels <- c("Q1", "Q2", "Q3", "Q4")
      } else if (input$categorize_method == "tertile") {
        breaks <- quantile(var_data, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
        labels <- c("Low", "Medium", "High")
      } else { # custom
        thresholds <- as.numeric(unlist(strsplit(input$custom_thresholds, ",")))
        breaks <- c(min(var_data, na.rm = TRUE), thresholds, max(var_data, na.rm = TRUE))
        breaks <- sort(unique(breaks))
        labels <- paste0("Cat", 1:(length(breaks)-1))
      }
      
      categorized_var <- cut(var_data, breaks = breaks, labels = labels, include.lowest = TRUE)
      new_col_name <- paste0(var_name, "_cat")
      values$processed_data[[new_col_name]] <- categorized_var
      values$categorization_done <- TRUE
      
      # Update choices
      categorical_vars <- names(values$processed_data)[sapply(values$processed_data, function(x) is.character(x) | is.factor(x))]
      updateSelectInput(session, "homogeneity_group", choices = categorical_vars)
      updateSelectInput(session, "ttest_group", choices = categorical_vars)
      updateSelectInput(session, "prop_variable", choices = categorical_vars)
      updateSelectInput(session, "prop_group", choices = categorical_vars)
      updateSelectInput(session, "var_group", choices = categorical_vars)
      updateSelectInput(session, "anova_factor1", choices = categorical_vars)
      updateSelectInput(session, "anova_factor2", choices = categorical_vars)
      
      safe_notification(paste("Kategorisasi berhasil:", new_col_name), "success")
      
    }, error = function(e) {
      safe_notification(paste("Error kategorisasi:", e$message), "error")
    })
  })
  
  output$categorization_done <- reactive({
    values$categorization_done
  })
  outputOptions(output, "categorization_done", suspendWhenHidden = FALSE)
  
  output$categorization_result <- renderPrint({
    if (values$categorization_done) {
      new_col_name <- paste0(input$categorize_variable, "_cat")
      if (new_col_name %in% names(values$processed_data)) {
        cat("Hasil Kategorisasi:", new_col_name, "\n")
        cat("======================\n")
        print(table(values$processed_data[[new_col_name]], useNA = "ifany"))
      }
    }
  })
  
  # =============================================================================
  # ANALISIS DESKRIPTIF
  # =============================================================================
  
  # Check if variable is numeric
  output$is_numeric_var <- reactive({
    if (values$data_loaded && !is.null(input$desc_variable)) {
      return(is.numeric(values$processed_data[[input$desc_variable]]))
    }
    return(FALSE)
  })
  outputOptions(output, "is_numeric_var", suspendWhenHidden = FALSE)
  
  observeEvent(input$generate_desc, {
    req(values$processed_data, input$desc_variable)
    
    values$current_desc_variable <- input$desc_variable
    values$desc_generated <- TRUE
  })
  
  output$desc_generated <- reactive({
    values$desc_generated
  })
  outputOptions(output, "desc_generated", suspendWhenHidden = FALSE)
  
  output$desc_plot <- renderPlotly({
    if (values$desc_generated && !is.null(values$current_desc_variable)) {
      var_data <- values$processed_data[[values$current_desc_variable]]
      
      if (is.numeric(var_data)) {
        p <- switch(input$desc_chart_type,
                    "histogram" = {
                      ggplot(values$processed_data, aes_string(x = values$current_desc_variable)) +
                        geom_histogram(bins = input$desc_bins, fill = waskita_colors$steel, 
                                       alpha = 0.7, color = "white") +
                        labs(title = paste("Histogram", values$current_desc_variable),
                             x = values$current_desc_variable, y = "Frequency") +
                        theme_waskita()
                    },
                    "boxplot" = {
                      ggplot(values$processed_data, aes_string(y = values$current_desc_variable)) +
                        geom_boxplot(fill = waskita_colors$steel, alpha = 0.7, color = waskita_colors$navy) +
                        labs(title = paste("Boxplot", values$current_desc_variable),
                             y = values$current_desc_variable) +
                        theme_waskita()
                    },
                    "density" = {
                      ggplot(values$processed_data, aes_string(x = values$current_desc_variable)) +
                        geom_density(fill = waskita_colors$steel, alpha = 0.7, color = waskita_colors$navy) +
                        labs(title = paste("Density Plot", values$current_desc_variable),
                             x = values$current_desc_variable, y = "Density") +
                        theme_waskita()
                    },
                    "qq" = {
                      ggplot(values$processed_data, aes_string(sample = values$current_desc_variable)) +
                        stat_qq(color = waskita_colors$steel) +
                        stat_qq_line(color = waskita_colors$navy) +
                        labs(title = paste("Q-Q Plot", values$current_desc_variable)) +
                        theme_waskita()
                    }
        )
      } else {
        # Categorical variable
        df <- as.data.frame(table(var_data))
        colnames(df) <- c("Category", "Frequency")
        p <- ggplot(df, aes(x = reorder(Category, -Frequency), y = Frequency)) +
          geom_bar(stat = "identity", fill = waskita_colors$steel, alpha = 0.7) +
          labs(title = paste("Bar Chart", values$current_desc_variable),
               x = values$current_desc_variable, y = "Frequency") +
          theme_waskita() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      ggplotly(p) %>%
        layout(
          font = list(family = "Segoe UI", size = 12),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  output$desc_stats <- renderPrint({
    if (values$desc_generated && !is.null(values$current_desc_variable)) {
      var_data <- values$processed_data[[values$current_desc_variable]]
      
      cat("Statistik Deskriptif:", values$current_desc_variable, "\n")
      cat("=======================================\n")
      
      if (is.numeric(var_data)) {
        cat("N:", sum(!is.na(var_data)), "\n")
        cat("Missing:", sum(is.na(var_data)), "\n")
        cat("Mean:", round(mean(var_data, na.rm = TRUE), 4), "\n")
        cat("Median:", round(median(var_data, na.rm = TRUE), 4), "\n")
        cat("SD:", round(sd(var_data, na.rm = TRUE), 4), "\n")
        cat("Min:", round(min(var_data, na.rm = TRUE), 4), "\n")
        cat("Max:", round(max(var_data, na.rm = TRUE), 4), "\n")
        cat("Q1:", round(quantile(var_data, 0.25, na.rm = TRUE), 4), "\n")
        cat("Q3:", round(quantile(var_data, 0.75, na.rm = TRUE), 4), "\n")
        cat("IQR:", round(IQR(var_data, na.rm = TRUE), 4), "\n")
        
        if (requireNamespace("moments", quietly = TRUE)) {
          cat("Skewness:", round(moments::skewness(var_data, na.rm = TRUE), 4), "\n")
          cat("Kurtosis:", round(moments::kurtosis(var_data, na.rm = TRUE), 4), "\n")
        }
      } else {
        tbl <- table(var_data, useNA = "ifany")
        print(tbl)
        cat("\nProporsi:\n")
        print(round(prop.table(tbl) * 100, 2))
      }
    }
  })
  
  output$desc_interpretation <- renderPrint({
    if (values$desc_generated && !is.null(values$current_desc_variable)) {
      var_data <- values$processed_data[[values$current_desc_variable]]
      
      cat("INTERPRETASI STATISTIK\n")
      cat("======================\n")
      
      if (is.numeric(var_data)) {
        mean_val <- mean(var_data, na.rm = TRUE)
        median_val <- median(var_data, na.rm = TRUE)
        sd_val <- sd(var_data, na.rm = TRUE)
        cv <- sd_val / abs(mean_val) * 100
        
        cat("1. TENDENSI SENTRAL:\n")
        if (abs(mean_val - median_val) / sd_val < 0.1) {
          cat("   - Distribusi relatif SIMETRIS (mean ≈ median)\n")
        } else if (mean_val > median_val) {
          cat("   - Distribusi CONDONG KANAN (mean > median)\n")
        } else {
          cat("   - Distribusi CONDONG KIRI (mean < median)\n")
        }
        
        cat("\n2. VARIABILITAS:\n")
        cat("   - Coefficient of Variation:", round(cv, 2), "%\n")
        if (cv < 15) {
          cat("   - Variabilitas RENDAH (homogen)\n")
        } else if (cv < 30) {
          cat("   - Variabilitas SEDANG\n")
        } else {
          cat("   - Variabilitas TINGGI (heterogen)\n")
        }
        
        if (requireNamespace("moments", quietly = TRUE)) {
          skew_val <- moments::skewness(var_data, na.rm = TRUE)
          kurt_val <- moments::kurtosis(var_data, na.rm = TRUE)
          
          cat("\n3. BENTUK DISTRIBUSI:\n")
          if (abs(skew_val) < 0.5) {
            cat("   - Skewness: Hampir simetris\n")
          } else if (abs(skew_val) < 1) {
            cat("   - Skewness: Moderately skewed\n")
          } else {
            cat("   - Skewness: Highly skewed\n")
          }
          
          if (kurt_val < 3) {
            cat("   - Kurtosis: Platykurtic (datar)\n")
          } else if (kurt_val > 3) {
            cat("   - Kurtosis: Leptokurtic (runcing)\n")
          } else {
            cat("   - Kurtosis: Mesokurtic (normal)\n")
          }
        }
        
        # Deteksi outlier
        Q1 <- quantile(var_data, 0.25, na.rm = TRUE)
        Q3 <- quantile(var_data, 0.75, na.rm = TRUE)
        IQR_val <- Q3 - Q1
        lower_fence <- Q1 - 1.5 * IQR_val
        upper_fence <- Q3 + 1.5 * IQR_val
        outliers <- sum(var_data < lower_fence | var_data > upper_fence, na.rm = TRUE)
        
        cat("\n4. OUTLIER:\n")
        cat("   - Jumlah outlier:", outliers, "\n")
        if (outliers == 0) {
          cat("   - Tidak ada outlier terdeteksi\n")
        } else {
          pct_outliers <- round(outliers / length(var_data) * 100, 2)
          cat("   - Persentase outlier:", pct_outliers, "%\n")
        }
        
      } else {
        tbl <- table(var_data, useNA = "ifany")
        dominant_cat <- names(which.max(tbl))
        dominant_pct <- round(max(tbl)/sum(tbl) * 100, 2)
        
        cat("1. DISTRIBUSI KATEGORI:\n")
        cat("   - Kategori dominan:", dominant_cat, "(", dominant_pct, "%)\n")
        cat("   - Jumlah kategori:", length(tbl), "\n")
        
        if (max(tbl)/min(tbl) > 3) {
          cat("   - Distribusi: TIDAK SEIMBANG\n")
        } else {
          cat("   - Distribusi: RELATIF SEIMBANG\n")
        }
      }
    }
  })
  
  # =============================================================================
  # VISUALISASI MULTIVARIAT
  # =============================================================================
  
  observeEvent(input$generate_viz, {
    req(values$processed_data, input$viz_type)
    
    values$current_viz_type <- input$viz_type
    values$viz_generated <- TRUE
  })
  
  output$viz_generated <- reactive({
    values$viz_generated
  })
  outputOptions(output, "viz_generated", suspendWhenHidden = FALSE)
  
  output$viz_plot <- renderPlotly({
    if (values$viz_generated && !is.null(values$current_viz_type)) {
      
      p <- switch(values$current_viz_type,
                  "scatter" = {
                    if (!is.null(input$scatter_x) && !is.null(input$scatter_y)) {
                      base_plot <- ggplot(values$processed_data, aes_string(x = input$scatter_x, y = input$scatter_y))
                      
                      if (!is.null(input$scatter_color) && input$scatter_color != "") {
                        base_plot <- base_plot + aes_string(color = input$scatter_color)
                      }
                      
                      base_plot +
                        geom_point(alpha = 0.7, size = 2) +
                        geom_smooth(method = "lm", se = TRUE, color = waskita_colors$navy) +
                        labs(title = paste("Scatter Plot:", input$scatter_x, "vs", input$scatter_y)) +
                        theme_waskita() +
                        scale_color_manual(values = c(waskita_colors$steel, waskita_colors$warning, waskita_colors$success))
                    }
                  },
                  
                  "correlation" = {
                    numeric_data <- values$processed_data[sapply(values$processed_data, is.numeric)]
                    if (ncol(numeric_data) >= 2) {
                      cor_matrix <- cor(numeric_data, use = "complete.obs")
                      
                      # Convert to long format for ggplot
                      cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
                      cor_df$Correlation <- as.vector(cor_matrix)
                      
                      ggplot(cor_df, aes(Var1, Var2, fill = Correlation)) +
                        geom_tile() +
                        scale_fill_gradient2(low = waskita_colors$error, mid = "white", 
                                             high = waskita_colors$navy, midpoint = 0,
                                             limits = c(-1, 1)) +
                        labs(title = "Correlation Heatmap", x = "", y = "") +
                        theme_waskita() +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
                    }
                  },
                  
                  "boxplot_group" = {
                    if (!is.null(input$boxplot_numeric) && !is.null(input$boxplot_group)) {
                      ggplot(values$processed_data, aes_string(x = input$boxplot_group, y = input$boxplot_numeric, 
                                                               fill = input$boxplot_group)) +
                        geom_boxplot(alpha = 0.7) +
                        labs(title = paste("Boxplot:", input$boxplot_numeric, "by", input$boxplot_group)) +
                        theme_waskita() +
                        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
                        scale_fill_manual(values = rep(c(waskita_colors$steel, waskita_colors$warning, 
                                                         waskita_colors$success, waskita_colors$navy), 10))
                    }
                  },
                  
                  "histogram_overlay" = {
                    if (!is.null(input$hist_variables) && length(input$hist_variables) > 0) {
                      selected_vars <- head(input$hist_variables, 4)  # Max 4 variables
                      
                      # Reshape data to long format
                      plot_data <- values$processed_data[selected_vars]
                      plot_data_long <- tidyr::gather(plot_data, key = "Variable", value = "Value")
                      
                      ggplot(plot_data_long, aes(x = Value, fill = Variable)) +
                        geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
                        facet_wrap(~Variable, scales = "free") +
                        labs(title = "Histogram Overlay") +
                        theme_waskita() +
                        scale_fill_manual(values = c(waskita_colors$steel, waskita_colors$warning, 
                                                     waskita_colors$success, waskita_colors$navy))
                    }
                  }
      )
      
      if (!is.null(p)) {
        ggplotly(p) %>%
          layout(
            font = list(family = "Segoe UI", size = 12),
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)'
          )
      }
    }
  })
  
  output$viz_interpretation <- renderPrint({
    if (values$viz_generated && !is.null(values$current_viz_type)) {
      cat("INTERPRETASI VISUALISASI\n")
      cat("========================\n")
      
      switch(values$current_viz_type,
             "scatter" = {
               if (!is.null(input$scatter_x) && !is.null(input$scatter_y)) {
                 # Calculate correlation
                 x_data <- values$processed_data[[input$scatter_x]]
                 y_data <- values$processed_data[[input$scatter_y]]
                 cor_val <- cor(x_data, y_data, use = "complete.obs")
                 
                 cat("Scatter Plot Analysis:\n")
                 cat("- Korelasi:", round(cor_val, 4), "\n")
                 cat("- Kekuatan:", interpret_correlation(cor_val), "\n")
                 cat("- Arah:", if(cor_val > 0) "Positif" else "Negatif", "\n")
                 
                 if (abs(cor_val) > 0.7) {
                   cat("- Hubungan KUAT ditemukan\n")
                 } else if (abs(cor_val) > 0.3) {
                   cat("- Hubungan SEDANG ditemukan\n")
                 } else {
                   cat("- Hubungan LEMAH ditemukan\n")
                 }
               }
             },
             
             "correlation" = {
               numeric_data <- values$processed_data[sapply(values$processed_data, is.numeric)]
               cor_matrix <- cor(numeric_data, use = "complete.obs")
               
               # Find strong correlations
               strong_cors <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
               
               cat("Correlation Heatmap Analysis:\n")
               cat("- Total variabel numerik:", ncol(numeric_data), "\n")
               cat("- Korelasi kuat (|r| > 0.7):", nrow(strong_cors), "pasang\n")
               
               if (nrow(strong_cors) > 0) {
                 cat("\nKorelasi Kuat:\n")
                 for (i in 1:min(5, nrow(strong_cors))) {
                   row_idx <- strong_cors[i, 1]
                   col_idx <- strong_cors[i, 2]
                   var1 <- rownames(cor_matrix)[row_idx]
                   var2 <- colnames(cor_matrix)[col_idx]
                   cor_val <- cor_matrix[row_idx, col_idx]
                   cat("-", var1, "vs", var2, ":", round(cor_val, 3), "\n")
                 }
               }
             },
             
             "boxplot_group" = {
               if (!is.null(input$boxplot_numeric) && !is.null(input$boxplot_group)) {
                 # ANOVA test for group differences
                 formula <- as.formula(paste(input$boxplot_numeric, "~", input$boxplot_group))
                 anova_result <- aov(formula, data = values$processed_data)
                 anova_p <- summary(anova_result)[[1]]$`Pr(>F)`[1]
                 
                 cat("Boxplot Group Analysis:\n")
                 cat("- ANOVA p-value:", format_pvalue(anova_p), "\n")
                 cat("- Interpretasi:", interpret_significance(anova_p), "\n")
                 
                 if (anova_p <= 0.05) {
                   cat("- Ada perbedaan signifikan antar grup\n")
                 } else {
                   cat("- Tidak ada perbedaan signifikan antar grup\n")
                 }
               }
             },
             
             "histogram_overlay" = {
               if (!is.null(input$hist_variables) && length(input$hist_variables) > 0) {
                 selected_vars <- head(input$hist_variables, 4)
                 
                 cat("Histogram Overlay Analysis:\n")
                 cat("- Variabel yang dibandingkan:", length(selected_vars), "\n")
                 
                 for (var in selected_vars) {
                   var_data <- values$processed_data[[var]]
                   cat("- ", var, ": Mean =", round(mean(var_data, na.rm = TRUE), 3),
                       ", SD =", round(sd(var_data, na.rm = TRUE), 3), "\n")
                 }
                 
                 cat("\nPerbandingan distribusi membantu mengidentifikasi:\n")
                 cat("- Perbedaan skala dan lokasi\n")
                 cat("- Pola distribusi yang serupa/berbeda\n")
                 cat("- Outlier pada masing-masing variabel\n")
               }
             }
      )
    }
  })
  
  # =============================================================================
  # PETA SPASIAL
  # =============================================================================
  
  observeEvent(input$generate_map, {
    req(values$processed_data, input$map_variable)
    
    tryCatch({
      # Generate synthetic coordinates for demonstration
      n_obs <- nrow(values$processed_data)
      set.seed(123)
      
      # Create Indonesia-like coordinates
      lat_range <- c(-11, 6)  # Indonesia latitude range
      lon_range <- c(95, 141)  # Indonesia longitude range
      
      map_data <- data.frame(
        lat = runif(n_obs, lat_range[1], lat_range[2]),
        lon = runif(n_obs, lon_range[1], lon_range[2]),
        value = values$processed_data[[input$map_variable]],
        district = values$processed_data$DISTRICTCODE,
        stringsAsFactors = FALSE
      )
      
      values$map_data <- map_data
      values$map_generated <- TRUE
      
      safe_notification("Peta berhasil dibuat!", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error membuat peta:", e$message), "error")
    })
  })
  
  output$map_generated <- reactive({
    values$map_generated
  })
  outputOptions(output, "map_generated", suspendWhenHidden = FALSE)
  
  output$spatial_map <- renderLeaflet({
    if (values$map_generated && !is.null(values$map_data)) {
      map_data <- values$map_data
      
      # Create color palette
      pal <- colorNumeric(palette = input$color_palette, domain = map_data$value)
      
      # Create leaflet map
      m <- leaflet(map_data) %>%
        addTiles() %>%
        setView(lng = mean(map_data$lon), lat = mean(map_data$lat), zoom = 5)
      
      if (input$map_type == "point") {
        m <- m %>%
          addCircleMarkers(
            lng = ~lon, lat = ~lat,
            color = ~pal(value),
            radius = 6,
            fillOpacity = 0.8,
            stroke = TRUE,
            weight = 1,
            popup = ~paste("District:", district, "<br>",
                           input$map_variable, ":", round(value, 3))
          )
      } else if (input$map_type == "heat") {
        # Heat map style with varying sizes
        m <- m %>%
          addCircleMarkers(
            lng = ~lon, lat = ~lat,
            color = ~pal(value),
            radius = ~scales::rescale(value, to = c(3, 15)),
            fillOpacity = 0.6,
            stroke = TRUE,
            weight = 1,
            popup = ~paste("District:", district, "<br>",
                           input$map_variable, ":", round(value, 3))
          )
      } else { # choropleth
        m <- m %>%
          addCircleMarkers(
            lng = ~lon, lat = ~lat,
            fillColor = ~pal(value),
            color = "white",
            weight = 2,
            radius = 8,
            fillOpacity = 0.8,
            popup = ~paste("District:", district, "<br>",
                           input$map_variable, ":", round(value, 3))
          )
      }
      
      # Add legend
      m <- m %>%
        addLegend(
          pal = pal,
          values = ~value,
          title = input$map_variable,
          position = "bottomright"
        )
      
      return(m)
    }
  })
  
  output$spatial_stats <- renderPrint({
    if (values$map_generated && !is.null(values$map_data)) {
      map_data <- values$map_data
      
      cat("STATISTIK DISTRIBUSI SPASIAL\n")
      cat("=============================\n")
      cat("Variabel:", input$map_variable, "\n")
      cat("Jumlah lokasi:", nrow(map_data), "\n")
      cat("Range koordinat:\n")
      cat("- Latitude:", round(min(map_data$lat), 3), "to", round(max(map_data$lat), 3), "\n")
      cat("- Longitude:", round(min(map_data$lon), 3), "to", round(max(map_data$lon), 3), "\n")
      cat("\nDistribusi nilai:\n")
      cat("- Min:", round(min(map_data$value, na.rm = TRUE), 3), "\n")
      cat("- Max:", round(max(map_data$value, na.rm = TRUE), 3), "\n")
      cat("- Mean:", round(mean(map_data$value, na.rm = TRUE), 3), "\n")
      cat("- SD:", round(sd(map_data$value, na.rm = TRUE), 3), "\n")
    }
  })
  
  output$spatial_interpretation <- renderPrint({
    if (values$map_generated && !is.null(values$map_data)) {
      cat("INTERPRETASI POLA SPASIAL\n")
      cat("=========================\n")
      cat("Peta menunjukkan distribusi geografis dari", input$map_variable, "\n\n")
      
      cat("Pola yang dapat diamati:\n")
      cat("1. DISTRIBUSI GEOGRAFIS:\n")
      cat("   - Peta menampilkan sebaran nilai across Indonesia\n")
      cat("   - Warna menunjukkan intensitas/magnitude variabel\n")
      
      cat("\n2. KLASTERISASI:\n")
      cat("   - Perhatikan apakah ada pengelompokan nilai tinggi/rendah\n")
      cat("   - Area dengan warna serupa mengindikasikan pola lokal\n")
      
      cat("\n3. OUTLIER SPASIAL:\n")
      cat("   - Lokasi dengan warna berbeda dari sekitarnya\n")
      cat("   - Dapat mengindikasikan anomali atau karakteristik unik\n")
      
      cat("\n4. IMPLIKASI KEBIJAKAN:\n")
      cat("   - Identifikasi area prioritas berdasarkan pola spasial\n")
      cat("   - Pertimbangkan faktor geografis dalam intervensi\n")
    }
  })
  
  # =============================================================================
  # UJI NORMALITAS
  # =============================================================================
  
  observeEvent(input$run_normality, {
    req(values$processed_data, input$normality_variable)
    
    tryCatch({
      var_data <- values$processed_data[[input$normality_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      if (length(var_data) < 3) {
        safe_notification("Data tidak cukup untuk uji normalitas", "warning")
        return()
      }
      
      test_result <- switch(input$normality_test,
                            "shapiro" = {
                              if (length(var_data) > 5000) {
                                safe_notification("Shapiro-Wilk tidak recommended untuk n > 5000", "warning")
                              }
                              shapiro.test(var_data)
                            },
                            "ks" = {
                              ks.test(var_data, "pnorm", mean(var_data), sd(var_data))
                            },
                            "ad" = {
                              nortest::ad.test(var_data)
                            }
      )
      
      values$normality_result <- test_result
      values$normality_done <- TRUE
      
      safe_notification("Uji normalitas selesai", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error uji normalitas:", e$message), "error")
    })
  })
  
  output$normality_done <- reactive({
    values$normality_done
  })
  outputOptions(output, "normality_done", suspendWhenHidden = FALSE)
  
  output$normality_result <- renderPrint({
    if (values$normality_done && !is.null(values$normality_result)) {
      result <- values$normality_result
      
      cat("HASIL UJI NORMALITAS\n")
      cat("====================\n")
      cat("Variabel:", input$normality_variable, "\n")
      cat("Metode:", switch(input$normality_test,
                            "shapiro" = "Shapiro-Wilk Test",
                            "ks" = "Kolmogorov-Smirnov Test",
                            "ad" = "Anderson-Darling Test"), "\n\n")
      
      cat("HIPOTESIS:\n")
      cat("H₀: Data berdistribusi normal\n")
      cat("H₁: Data tidak berdistribusi normal\n")
      cat("α = 0.05\n\n")
      
      print(result)
      
      cat("\nINTERPRETASI:\n")
      p_value <- result$p.value
      cat("P-value:", format_pvalue(p_value), "\n")
      
      if (p_value > 0.05) {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Data dapat dianggap berdistribusi normal (α = 0.05)\n")
        cat("IMPLIKASI: Dapat menggunakan uji parametrik\n")
      } else {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Data tidak berdistribusi normal (α = 0.05)\n")
        cat("IMPLIKASI: Pertimbangkan transformasi atau uji non-parametrik\n")
      }
    }
  })
  
  # =============================================================================
  # UJI HOMOGENITAS
  # =============================================================================
  
  observeEvent(input$run_homogeneity, {
    req(values$processed_data, input$homogeneity_numeric, input$homogeneity_group)
    
    tryCatch({
      formula <- as.formula(paste(input$homogeneity_numeric, "~", input$homogeneity_group))
      
      test_result <- switch(input$homogeneity_test,
                            "levene" = {
                              car::leveneTest(formula, data = values$processed_data)
                            },
                            "bartlett" = {
                              bartlett.test(formula, data = values$processed_data)
                            },
                            "fligner" = {
                              fligner.test(formula, data = values$processed_data)
                            }
      )
      
      values$homogeneity_result <- test_result
      values$homogeneity_done <- TRUE
      
      safe_notification("Uji homogenitas selesai", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error uji homogenitas:", e$message), "error")
    })
  })
  
  output$homogeneity_done <- reactive({
    values$homogeneity_done
  })
  outputOptions(output, "homogeneity_done", suspendWhenHidden = FALSE)
  
  output$homogeneity_result <- renderPrint({
    if (values$homogeneity_done && !is.null(values$homogeneity_result)) {
      result <- values$homogeneity_result
      
      cat("HASIL UJI HOMOGENITAS VARIANS\n")
      cat("==============================\n")
      cat("Variabel numerik:", input$homogeneity_numeric, "\n")
      cat("Variabel grup:", input$homogeneity_group, "\n")
      cat("Metode:", switch(input$homogeneity_test,
                            "levene" = "Levene's Test",
                            "bartlett" = "Bartlett's Test",
                            "fligner" = "Fligner-Killeen Test"), "\n\n")
      
      cat("HIPOTESIS:\n")
      cat("H₀: Varians antar grup homogen\n")
      cat("H₁: Varians antar grup tidak homogen\n")
      cat("α = 0.05\n\n")
      
      print(result)
      
      cat("\nINTERPRETASI:\n")
      p_value <- if(input$homogeneity_test == "levene") result$`Pr(>F)`[1] else result$p.value
      cat("P-value:", format_pvalue(p_value), "\n")
      
      if (p_value > 0.05) {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Varians antar grup homogen (α = 0.05)\n")
        cat("IMPLIKASI: Asumsi homogenitas terpenuhi untuk ANOVA\n")
      } else {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Varians antar grup tidak homogen (α = 0.05)\n")
        cat("IMPLIKASI: Gunakan Welch's test atau transformasi data\n")
      }
    }
  })
  
  # =============================================================================
  # DIAGNOSTIC PLOTS
  # =============================================================================
  
  output$diagnostic_plots <- renderPlotly({
    if (values$normality_done || values$homogeneity_done) {
      plots <- list()
      
      if (values$normality_done) {
        var_data <- values$processed_data[[input$normality_variable]]
        
        # Q-Q Plot
        qq_plot <- ggplot(data.frame(sample = var_data), aes(sample = sample)) +
          stat_qq(color = waskita_colors$steel) +
          stat_qq_line(color = waskita_colors$navy) +
          labs(title = "Q-Q Plot", subtitle = "Test for Normality") +
          theme_waskita()
        
        ggplotly(qq_plot) %>%
          layout(
            font = list(family = "Segoe UI", size = 12),
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)'
          )
      } else if (values$homogeneity_done) {
        # Boxplot by group
        group_plot <- ggplot(values$processed_data, 
                             aes_string(x = input$homogeneity_group, 
                                        y = input$homogeneity_numeric,
                                        fill = input$homogeneity_group)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = "Boxplot by Group", subtitle = "Test for Homogeneity") +
          theme_waskita() +
          theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_manual(values = rep(c(waskita_colors$steel, waskita_colors$warning, 
                                           waskita_colors$success, waskita_colors$navy), 10))
        
        ggplotly(group_plot) %>%
          layout(
            font = list(family = "Segoe UI", size = 12),
            plot_bgcolor = 'rgba(0,0,0,0)',
            paper_bgcolor = 'rgba(0,0,0,0)'
          )
      }
    }
  })
  
  output$assumptions_interpretation <- renderPrint({
    if (values$normality_done || values$homogeneity_done) {
      cat("RINGKASAN UJI ASUMSI\n")
      cat("====================\n")
      
      if (values$normality_done && values$homogeneity_done) {
        norm_p <- values$normality_result$p.value
        homo_p <- if("Pr(>F)" %in% names(values$homogeneity_result)) {
          values$homogeneity_result$`Pr(>F)`[1]
        } else {
          values$homogeneity_result$p.value
        }
        
        cat("Normalitas:", if(norm_p > 0.05) "✓ TERPENUHI" else "✗ TIDAK TERPENUHI", "\n")
        cat("Homogenitas:", if(homo_p > 0.05) "✓ TERPENUHI" else "✗ TIDAK TERPENUHI", "\n\n")
        
        cat("REKOMENDASI ANALISIS:\n")
        if (norm_p > 0.05 && homo_p > 0.05) {
          cat("- Gunakan uji parametrik standar (t-test, ANOVA)\n")
          cat("- Semua asumsi terpenuhi\n")
        } else if (norm_p > 0.05 && homo_p <= 0.05) {
          cat("- Gunakan Welch's test (varians tidak sama)\n")
          cat("- Normalitas terpenuhi, homogenitas tidak\n")
        } else if (norm_p <= 0.05 && homo_p > 0.05) {
          cat("- Pertimbangkan transformasi data\n")
          cat("- Atau gunakan uji non-parametrik\n")
        } else {
          cat("- Gunakan uji non-parametrik\n")
          cat("- Kedua asumsi tidak terpenuhi\n")
        }
        
      } else if (values$normality_done) {
        cat("Hanya uji normalitas yang dilakukan.\n")
        cat("Lanjutkan dengan uji homogenitas untuk evaluasi lengkap.\n")
      } else if (values$homogeneity_done) {
        cat("Hanya uji homogenitas yang dilakukan.\n")
        cat("Lanjutkan dengan uji normalitas untuk evaluasi lengkap.\n")
      }
    }
  })
  
  # =============================================================================
  # UJI T-TEST
  # =============================================================================
  
  observeEvent(input$run_ttest, {
    req(values$processed_data, input$ttest_variable)
    
    tryCatch({
      var_data <- values$processed_data[[input$ttest_variable]]
      
      test_result <- switch(input$ttest_type,
                            "one_sample" = {
                              t.test(var_data, mu = input$test_value, 
                                     alternative = input$alternative,
                                     conf.level = input$confidence_level)
                            },
                            "independent" = {
                              if (is.null(input$ttest_group)) {
                                safe_notification("Pilih variabel grup untuk independent t-test", "warning")
                                return()
                              }
                              formula <- as.formula(paste(input$ttest_variable, "~", input$ttest_group))
                              t.test(formula, data = values$processed_data,
                                     var.equal = input$equal_variance,
                                     alternative = input$alternative,
                                     conf.level = input$confidence_level)
                            },
                            "paired" = {
                              if (is.null(input$ttest_paired_var)) {
                                safe_notification("Pilih variabel kedua untuk paired t-test", "warning")
                                return()
                              }
                              var2_data <- values$processed_data[[input$ttest_paired_var]]
                              t.test(var_data, var2_data, paired = TRUE,
                                     alternative = input$alternative,
                                     conf.level = input$confidence_level)
                            }
      )
      
      values$ttest_result <- test_result
      values$ttest_done <- TRUE
      
      safe_notification("Uji t selesai", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error uji t:", e$message), "error")
    })
  })
  
  output$ttest_done <- reactive({
    values$ttest_done
  })
  outputOptions(output, "ttest_done", suspendWhenHidden = FALSE)
  
  output$ttest_result <- renderPrint({
    if (values$ttest_done && !is.null(values$ttest_result)) {
      result <- values$ttest_result
      
      cat("HASIL UJI T\n")
      cat("============\n")
      cat("Jenis:", switch(input$ttest_type,
                           "one_sample" = "One Sample t-test",
                           "independent" = "Independent t-test",
                           "paired" = "Paired t-test"), "\n")
      cat("Variabel:", input$ttest_variable, "\n")
      cat("Alternative:", input$alternative, "\n")
      cat("Confidence level:", input$confidence_level * 100, "%\n\n")
      
      print(result)
      
      cat("\nINTERPRETASI:\n")
      p_value <- result$p.value
      cat("P-value:", format_pvalue(p_value), "\n")
      cat("t-statistic:", round(result$statistic, 4), "\n")
      cat("Degrees of freedom:", result$parameter, "\n")
      
      if (p_value <= 0.05) {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Ada perbedaan yang signifikan\n")
      } else {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Tidak ada perbedaan yang signifikan\n")
      }
      
      # Confidence interval
      ci <- result$conf.int
      cat("\nConfidence Interval (", input$confidence_level * 100, "%):\n", sep = "")
      cat("[", round(ci[1], 4), ", ", round(ci[2], 4), "]\n", sep = "")
    }
  })
  
  output$ttest_plot <- renderPlotly({
    if (values$ttest_done && !is.null(values$ttest_result)) {
      
      if (input$ttest_type == "one_sample") {
        # Histogram with test value line
        p <- ggplot(values$processed_data, aes_string(x = input$ttest_variable)) +
          geom_histogram(bins = 30, fill = waskita_colors$steel, alpha = 0.7) +
          geom_vline(xintercept = input$test_value, color = waskita_colors$error, 
                     linetype = "dashed", size = 1) +
          geom_vline(xintercept = mean(values$processed_data[[input$ttest_variable]], na.rm = TRUE),
                     color = waskita_colors$navy, size = 1) +
          labs(title = "Distribution with Test Value",
               subtitle = "Dashed line: Test value, Solid line: Sample mean") +
          theme_waskita()
        
      } else if (input$ttest_type == "independent") {
        # Boxplot comparison
        p <- ggplot(values$processed_data, 
                    aes_string(x = input$ttest_group, y = input$ttest_variable,
                               fill = input$ttest_group)) +
          geom_boxplot(alpha = 0.7) +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
          labs(title = "Group Comparison") +
          theme_waskita() +
          theme(legend.position = "none") +
          scale_fill_manual(values = c(waskita_colors$steel, waskita_colors$warning))
        
      } else { # paired
        # Scatter plot with diagonal line
        df_paired <- data.frame(
          var1 = values$processed_data[[input$ttest_variable]],
          var2 = values$processed_data[[input$ttest_paired_var]]
        )
        p <- ggplot(df_paired, aes(x = var1, y = var2)) +
          geom_point(alpha = 0.6, color = waskita_colors$steel) +
          geom_abline(intercept = 0, slope = 1, color = waskita_colors$error, linetype = "dashed") +
          labs(title = "Paired Data Comparison",
               x = input$ttest_variable,
               y = input$ttest_paired_var,
               subtitle = "Dashed line: y = x (no difference)") +
          theme_waskita()
      }
      
      ggplotly(p) %>%
        layout(
          font = list(family = "Segoe UI", size = 12),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  output$ttest_effect_size <- renderPrint({
    if (values$ttest_done && !is.null(values$ttest_result)) {
      result <- values$ttest_result
      
      cat("EFFECT SIZE & POWER ANALYSIS\n")
      cat("============================\n")
      
      # Calculate Cohen's d
      if (input$ttest_type == "one_sample") {
        var_data <- values$processed_data[[input$ttest_variable]]
        cohens_d <- (mean(var_data, na.rm = TRUE) - input$test_value) / sd(var_data, na.rm = TRUE)
      } else if (input$ttest_type == "independent") {
        # Two-sample Cohen's d
        formula <- as.formula(paste(input$ttest_variable, "~", input$ttest_group))
        groups <- split(values$processed_data[[input$ttest_variable]], 
                        values$processed_data[[input$ttest_group]])
        if (length(groups) == 2) {
          group1 <- groups[[1]]
          group2 <- groups[[2]]
          pooled_sd <- sqrt(((length(group1)-1)*var(group1, na.rm = TRUE) + 
                               (length(group2)-1)*var(group2, na.rm = TRUE)) / 
                              (length(group1) + length(group2) - 2))
          cohens_d <- (mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)) / pooled_sd
        } else {
          cohens_d <- NA
        }
      } else { # paired
        var1_data <- values$processed_data[[input$ttest_variable]]
        var2_data <- values$processed_data[[input$ttest_paired_var]]
        differences <- var1_data - var2_data
        cohens_d <- mean(differences, na.rm = TRUE) / sd(differences, na.rm = TRUE)
      }
      
      if (!is.na(cohens_d)) {
        cat("Cohen's d:", round(cohens_d, 4), "\n")
        cat("Interpretasi:", interpret_cohens_d(cohens_d), "effect size\n")
        
        cat("\nGuidelines Effect Size:\n")
        cat("- Small: |d| ≈ 0.2\n")
        cat("- Medium: |d| ≈ 0.5\n")
        cat("- Large: |d| ≈ 0.8\n")
      }
      
      # Practical vs Statistical significance
      cat("\nSIGNIFIKANSI:\n")
      p_value <- result$p.value
      if (p_value <= 0.05 && !is.na(cohens_d) && abs(cohens_d) >= 0.5) {
        cat("- Signifikan secara STATISTIK dan PRAKTIS\n")
      } else if (p_value <= 0.05) {
        cat("- Signifikan secara STATISTIK (effect size kecil)\n")
      } else {
        cat("- Tidak signifikan secara statistik\n")
      }
    }
  })
  
  output$ttest_interpretation <- renderPrint({
    if (values$ttest_done && !is.null(values$ttest_result)) {
      cat("INTERPRETASI PRAKTIS\n")
      cat("====================\n")
      
      result <- values$ttest_result
      p_value <- result$p.value
      
      cat("RINGKASAN TEMUAN:\n")
      if (p_value <= 0.001) {
        cat("- Perbedaan SANGAT SIGNIFIKAN (p < 0.001)\n")
      } else if (p_value <= 0.01) {
        cat("- Perbedaan SIGNIFIKAN (p < 0.01)\n")
      } else if (p_value <= 0.05) {
        cat("- Perbedaan SIGNIFIKAN (p < 0.05)\n")
      } else if (p_value <= 0.1) {
        cat("- Perbedaan MARGINALLY SIGNIFICANT (p < 0.1)\n")
      } else {
        cat("- Perbedaan TIDAK SIGNIFIKAN (p > 0.05)\n")
      }
      
      cat("\nREKOMENDASI:\n")
      if (p_value <= 0.05) {
        cat("1. Hasil dapat dilaporkan sebagai temuan signifikan\n")
        cat("2. Pertimbangkan replikasi untuk konfirmasi\n")
        cat("3. Evaluasi relevansi praktis dari perbedaan\n")
      } else {
        cat("1. Tidak ada bukti perbedaan yang signifikan\n")
        cat("2. Pertimbangkan power analysis\n")
        cat("3. Evaluasi kemungkinan Type II error\n")
      }
    }
  })
  
  # =============================================================================
  # UJI PROPORSI
  # =============================================================================
  
  observeEvent(input$run_prop_test, {
    req(values$processed_data, input$prop_variable)
    
    tryCatch({
      if (input$prop_test_type == "one_prop") {
        var_data <- values$processed_data[[input$prop_variable]]
        tbl <- table(var_data)
        success_count <- tbl[1]  # First category as "success"
        total_count <- sum(tbl)
        
        test_result <- prop.test(success_count, total_count, 
                                 p = input$prop_test_value,
                                 alternative = input$alternative)
      } else {
        # Two sample proportion test
        if (is.null(input$prop_group)) {
          safe_notification("Pilih variabel grup untuk two sample proportion test", "warning")
          return()
        }
        
        cont_table <- table(values$processed_data[[input$prop_variable]], 
                            values$processed_data[[input$prop_group]])
        success_counts <- cont_table[1, ]
        total_counts <- colSums(cont_table)
        
        test_result <- prop.test(success_counts, total_counts,
                                 alternative = input$alternative)
      }
      
      values$prop_test_result <- test_result
      values$prop_test_done <- TRUE
      
      safe_notification("Uji proporsi selesai", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error uji proporsi:", e$message), "error")
    })
  })
  
  output$prop_test_done <- reactive({
    values$prop_test_done
  })
  outputOptions(output, "prop_test_done", suspendWhenHidden = FALSE)
  
  output$prop_test_result <- renderPrint({
    if (values$prop_test_done && !is.null(values$prop_test_result)) {
      result <- values$prop_test_result
      
      cat("HASIL UJI PROPORSI\n")
      cat("==================\n")
      cat("Jenis:", if(input$prop_test_type == "one_prop") "One Sample Proportion" else "Two Sample Proportion", "\n")
      cat("Variabel:", input$prop_variable, "\n")
      if (input$prop_test_type == "one_prop") {
        cat("Proporsi uji (p₀):", input$prop_test_value, "\n")
      } else {
        cat("Variabel grup:", input$prop_group, "\n")
      }
      cat("Alternative:", input$alternative, "\n\n")
      
      print(result)
      
      cat("\nINTERPRETASI:\n")
      p_value <- result$p.value
      cat("P-value:", format_pvalue(p_value), "\n")
      cat("Chi-squared statistic:", round(result$statistic, 4), "\n")
      
      if (p_value <= 0.05) {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Ada perbedaan proporsi yang signifikan\n")
      } else {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Tidak ada perbedaan proporsi yang signifikan\n")
      }
      
      if (!is.null(result$conf.int)) {
        ci <- result$conf.int
        cat("\nConfidence Interval:\n")
        cat("[", round(ci[1], 4), ", ", round(ci[2], 4), "]\n", sep = "")
      }
    }
  })
  
  output$prop_test_plot <- renderPlotly({
    if (values$prop_test_done && !is.null(values$prop_test_result)) {
      
      if (input$prop_test_type == "one_prop") {
        # Bar chart for one sample proportion
        var_data <- values$processed_data[[input$prop_variable]]
        df <- as.data.frame(table(var_data))
        colnames(df) <- c("Category", "Frequency")
        df$Proportion <- df$Frequency / sum(df$Frequency)
        
        p <- ggplot(df, aes(x = Category, y = Frequency, fill = Category)) +
          geom_bar(stat = "identity", alpha = 0.7) +
          geom_text(aes(label = paste0(round(Proportion*100, 1), "%")), 
                    vjust = -0.5) +
          labs(title = "Observed Proportions") +
          theme_waskita() +
          theme(legend.position = "none") +
          scale_fill_manual(values = c(waskita_colors$steel, waskita_colors$warning))
        
      } else {
        # Grouped bar chart for two sample proportion
        cont_table <- table(values$processed_data[[input$prop_variable]], 
                            values$processed_data[[input$prop_group]])
        
        # Convert to data frame for plotting
        df <- as.data.frame(cont_table)
        colnames(df) <- c("Category", "Group", "Frequency")
        
        p <- ggplot(df, aes(x = Group, y = Frequency, fill = Category)) +
          geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
          labs(title = "Proportion Comparison Between Groups") +
          theme_waskita() +
          scale_fill_manual(values = c(waskita_colors$steel, waskita_colors$warning))
      }
      
      ggplotly(p) %>%
        layout(
          font = list(family = "Segoe UI", size = 12),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  # =============================================================================
  # UJI VARIANCE
  # =============================================================================
  
  observeEvent(input$run_var_test, {
    req(values$processed_data, input$var_variable)
    
    tryCatch({
      var_data <- values$processed_data[[input$var_variable]]
      var_data <- var_data[!is.na(var_data)]
      
      if (input$var_test_type == "one_var") {
        # One sample variance test (Chi-square test)
        n <- length(var_data)
        sample_var <- var(var_data)
        test_var <- input$var_test_value
        
        chi_sq_stat <- (n - 1) * sample_var / test_var
        
        if (input$alternative == "two.sided") {
          p_value <- 2 * min(pchisq(chi_sq_stat, n-1), 1 - pchisq(chi_sq_stat, n-1))
        } else if (input$alternative == "greater") {
          p_value <- 1 - pchisq(chi_sq_stat, n-1)
        } else {
          p_value <- pchisq(chi_sq_stat, n-1)
        }
        
        test_result <- list(
          statistic = chi_sq_stat,
          parameter = n-1,
          p.value = p_value,
          method = "One Sample Variance Test",
          data.name = input$var_variable
        )
        
      } else {
        # Two sample variance test (F-test)
        if (is.null(input$var_group)) {
          safe_notification("Pilih variabel grup untuk two sample variance test", "warning")
          return()
        }
        
        formula <- as.formula(paste(input$var_variable, "~", input$var_group))
        test_result <- var.test(formula, data = values$processed_data,
                                alternative = input$alternative)
      }
      
      values$var_test_result <- test_result
      values$var_test_done <- TRUE
      
      safe_notification("Uji variance selesai", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error uji variance:", e$message), "error")
    })
  })
  
  output$var_test_done <- reactive({
    values$var_test_done
  })
  outputOptions(output, "var_test_done", suspendWhenHidden = FALSE)
  
  output$var_test_result <- renderPrint({
    if (values$var_test_done && !is.null(values$var_test_result)) {
      result <- values$var_test_result
      
      cat("HASIL UJI VARIANCE\n")
      cat("==================\n")
      cat("Jenis:", if(input$var_test_type == "one_var") "One Sample Variance" else "Two Sample Variance (F-test)", "\n")
      cat("Variabel:", input$var_variable, "\n")
      if (input$var_test_type == "one_var") {
        cat("Variance uji (σ²₀):", input$var_test_value, "\n")
      } else {
        cat("Variabel grup:", input$var_group, "\n")
      }
      cat("Alternative:", input$alternative, "\n\n")
      
      print(result)
      
      cat("\nINTERPRETASI:\n")
      p_value <- result$p.value
      cat("P-value:", format_pvalue(p_value), "\n")
      
      if (input$var_test_type == "one_var") {
        cat("Chi-squared statistic:", round(result$statistic, 4), "\n")
        cat("Degrees of freedom:", result$parameter, "\n")
      } else {
        cat("F statistic:", round(result$statistic, 4), "\n")
      }
      
      if (p_value <= 0.05) {
        cat("KEPUTUSAN: Tolak H₀\n")
        cat("KESIMPULAN: Ada perbedaan variance yang signifikan\n")
      } else {
        cat("KEPUTUSAN: Gagal menolak H₀\n")
        cat("KESIMPULAN: Tidak ada perbedaan variance yang signifikan\n")
      }
    }
  })
  
  output$var_test_plot <- renderPlotly({
    if (values$var_test_done && !is.null(values$var_test_result)) {
      
      if (input$var_test_type == "one_var") {
        # Histogram with variance info
        var_data <- values$processed_data[[input$var_variable]]
        sample_var <- var(var_data, na.rm = TRUE)
        
        p <- ggplot(values$processed_data, aes_string(x = input$var_variable)) +
          geom_histogram(bins = 30, fill = waskita_colors$steel, alpha = 0.7) +
          labs(title = paste("Distribution (Sample Variance =", round(sample_var, 4), ")"),
               subtitle = paste("Test Variance =", input$var_test_value)) +
          theme_waskita()
        
      } else {
        # Boxplot for variance comparison
        p <- ggplot(values$processed_data, 
                    aes_string(x = input$var_group, y = input$var_variable,
                               fill = input$var_group)) +
          geom_boxplot(alpha = 0.7) +
          labs(title = "Variance Comparison Between Groups") +
          theme_waskita() +
          theme(legend.position = "none") +
          scale_fill_manual(values = c(waskita_colors$steel, waskita_colors$warning))
      }
      
      ggplotly(p) %>%
        layout(
          font = list(family = "Segoe UI", size = 12),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  output$prop_var_interpretation <- renderPrint({
    if (values$prop_test_done || values$var_test_done) {
      cat("RINGKASAN UJI PROPORSI & VARIANCE\n")
      cat("==================================\n")
      
      if (values$prop_test_done && !is.null(values$prop_test_result)) {
        prop_p <- values$prop_test_result$p.value
        cat("UJI PROPORSI:\n")
        cat("- P-value:", format_pvalue(prop_p), "\n")
        cat("- Status:", if(prop_p <= 0.05) "SIGNIFIKAN" else "TIDAK SIGNIFIKAN", "\n\n")
      }
      
      if (values$var_test_done && !is.null(values$var_test_result)) {
        var_p <- values$var_test_result$p.value
        cat("UJI VARIANCE:\n")
        cat("- P-value:", format_pvalue(var_p), "\n")
        cat("- Status:", if(var_p <= 0.05) "SIGNIFIKAN" else "TIDAK SIGNIFIKAN", "\n\n")
      }
      
      cat("CATATAN:\n")
      cat("- Uji proporsi berguna untuk membandingkan proporsi kategori\n")
      cat("- Uji variance berguna untuk membandingkan variabilitas data\n")
      cat("- Kedua uji ini melengkapi analisis deskriptif\n")
    }
  })
  
  # =============================================================================
  # ANOVA
  # =============================================================================
  
  # Helper function for effect size interpretation
  interpret_effect_size <- function(eta_sq) {
    if (is.na(eta_sq)) return("Tidak dapat dihitung")
    if (eta_sq < 0.01) return("Negligible")
    if (eta_sq < 0.06) return("Small")
    if (eta_sq < 0.14) return("Medium")
    return("Large")
  }
  
  observeEvent(input$run_anova, {
    req(values$processed_data, input$anova_dependent, input$anova_factor1)
    
    tryCatch({
      if (input$anova_type == "one_way") {
        formula <- as.formula(paste(input$anova_dependent, "~", input$anova_factor1))
        anova_model <- aov(formula, data = values$processed_data)
        anova_summary <- summary(anova_model)
        
        # Post-hoc test (Tukey HSD)
        posthoc <- TukeyHSD(anova_model)
        
        # Effect size (eta squared)
        ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
        ss_between <- anova_summary[[1]]$`Sum Sq`[1]
        eta_squared <- ss_between / ss_total
        
        values$anova_result <- list(
          model = anova_model,
          summary = anova_summary,
          posthoc = posthoc,
          eta_squared = eta_squared,
          type = "one_way"
        )
        
      } else {
        # Two-way ANOVA
        if (is.null(input$anova_factor2)) {
          safe_notification("Pilih faktor kedua untuk two-way ANOVA", "warning")
          return()
        }
        
        if (input$include_interaction) {
          formula <- as.formula(paste(input$anova_dependent, "~", 
                                      input$anova_factor1, "*", input$anova_factor2))
        } else {
          formula <- as.formula(paste(input$anova_dependent, "~", 
                                      input$anova_factor1, "+", input$anova_factor2))
        }
        
        anova_model <- aov(formula, data = values$processed_data)
        anova_summary <- summary(anova_model)
        
        # Effect sizes for each factor
        ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
        eta_squared_factor1 <- anova_summary[[1]]$`Sum Sq`[1] / ss_total
        eta_squared_factor2 <- anova_summary[[1]]$`Sum Sq`[2] / ss_total
        
        eta_squared_interaction <- NULL
        if (input$include_interaction && nrow(anova_summary[[1]]) > 3) {
          eta_squared_interaction <- anova_summary[[1]]$`Sum Sq`[3] / ss_total
        }
        
        values$anova_result <- list(
          model = anova_model,
          summary = anova_summary,
          eta_squared_factor1 = eta_squared_factor1,
          eta_squared_factor2 = eta_squared_factor2,
          eta_squared_interaction = eta_squared_interaction,
          type = "two_way"
        )
      }
      
      values$anova_done <- TRUE
      safe_notification("ANOVA selesai", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error ANOVA:", e$message), "error")
    })
  })
  
  output$anova_done <- reactive({
    values$anova_done
  })
  outputOptions(output, "anova_done", suspendWhenHidden = FALSE)
  
  output$anova_result <- renderPrint({
    if (values$anova_done && !is.null(values$anova_result)) {
      result <- values$anova_result
      
      cat("HASIL ANOVA\n")
      cat("===========\n")
      cat("Jenis:", if(result$type == "one_way") "One-Way ANOVA" else "Two-Way ANOVA", "\n")
      cat("Variabel dependen:", input$anova_dependent, "\n")
      cat("Faktor 1:", input$anova_factor1, "\n")
      if (result$type == "two_way") {
        cat("Faktor 2:", input$anova_factor2, "\n")
        cat("Interaksi:", if(input$include_interaction) "Ya" else "Tidak", "\n")
      }
      cat("Alpha level:", input$anova_alpha, "\n\n")
      
      print(result$summary)
      
      cat("\nINTERPRETASI:\n")
      anova_table <- result$summary[[1]]
      
      if (result$type == "one_way") {
        f_stat <- anova_table$`F value`[1]
        p_value <- anova_table$`Pr(>F)`[1]
        
        cat("F-statistic:", round(f_stat, 4), "\n")
        cat("P-value:", format_pvalue(p_value), "\n")
        
        if (p_value <= input$anova_alpha) {
          cat("KEPUTUSAN: Tolak H₀\n")
          cat("KESIMPULAN: Ada perbedaan rata-rata yang signifikan antar grup\n")
        } else {
          cat("KEPUTUSAN: Gagal menolak H₀\n")
          cat("KESIMPULAN: Tidak ada perbedaan rata-rata yang signifikan antar grup\n")
        }
        
      } else {
        # Two-way ANOVA interpretation
        for (i in 1:(nrow(anova_table)-1)) {
          effect_name <- rownames(anova_table)[i]
          f_stat <- anova_table$`F value`[i]
          p_value <- anova_table$`Pr(>F)`[i]
          
          cat("\nEFEK", effect_name, ":\n")
          cat("F-statistic:", round(f_stat, 4), "\n")
          cat("P-value:", format_pvalue(p_value), "\n")
          
          if (p_value <= input$anova_alpha) {
            cat("Status: SIGNIFIKAN\n")
          } else {
            cat("Status: TIDAK SIGNIFIKAN\n")
          }
        }
      }
    }
  })
  
  output$anova_plot <- renderPlotly({
    if (values$anova_done && !is.null(values$anova_result)) {
      result <- values$anova_result
      
      if (result$type == "one_way") {
        # Boxplot for one-way ANOVA
        p <- ggplot(values$processed_data, 
                    aes_string(x = input$anova_factor1, y = input$anova_dependent,
                               fill = input$anova_factor1)) +
          geom_boxplot(alpha = 0.7) +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
          labs(title = "One-Way ANOVA: Group Comparison") +
          theme_waskita() +
          theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_manual(values = rep(c(waskita_colors$steel, waskita_colors$warning, 
                                           waskita_colors$success, waskita_colors$navy), 10))
        
      } else {
        # Interaction plot for two-way ANOVA
        group_means <- values$processed_data %>%
          group_by(!!sym(input$anova_factor1), !!sym(input$anova_factor2)) %>%
          summarise(mean_val = mean(!!sym(input$anova_dependent), na.rm = TRUE),
                    .groups = 'drop')
        
        p <- ggplot(group_means, aes_string(x = input$anova_factor1, 
                                            y = "mean_val",
                                            color = input$anova_factor2,
                                            group = input$anova_factor2)) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          labs(title = "Two-Way ANOVA: Interaction Plot",
               x = input$anova_factor1,
               y = paste("Mean", input$anova_dependent),
               color = input$anova_factor2) +
          theme_waskita() +
          scale_color_manual(values = c(waskita_colors$steel, waskita_colors$warning, 
                                        waskita_colors$success, waskita_colors$navy))
      }
      
      ggplotly(p) %>%
        layout(
          font = list(family = "Segoe UI", size = 12),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  output$posthoc_result <- renderPrint({
    if (values$anova_done && !is.null(values$anova_result)) {
      result <- values$anova_result
      
      cat("POST-HOC ANALYSIS\n")
      cat("================\n")
      
      if (result$type == "one_way" && !is.null(result$posthoc)) {
        # Check if ANOVA was significant first
        anova_p <- result$summary[[1]]$`Pr(>F)`[1]
        
        if (anova_p <= input$anova_alpha) {
          cat("TUKEY HSD POST-HOC TEST:\n")
          cat("(Dilakukan karena ANOVA signifikan)\n\n")
          print(result$posthoc)
          
          cat("\nINTERPRETASI POST-HOC:\n")
          cat("Pasangan grup dengan p adj < 0.05 menunjukkan perbedaan signifikan.\n")
        } else {
          cat("Post-hoc test tidak dilakukan karena ANOVA tidak signifikan.\n")
        }
      } else {
        cat("Post-hoc analysis untuk two-way ANOVA memerlukan analisis terpisah.\n")
        cat("Fokus pada efek utama dan interaksi dari tabel ANOVA.\n")
      }
    }
  })
  
  output$effect_size_result <- renderPrint({
    if (values$anova_done && !is.null(values$anova_result)) {
      result <- values$anova_result
      
      cat("EFFECT SIZE ANALYSIS\n")
      cat("===================\n")
      
      if (result$type == "one_way") {
        eta_sq <- result$eta_squared
        cat("Eta Squared (η²):", round(eta_sq, 4), "\n")
        cat("Interpretasi:", interpret_effect_size(eta_sq), "effect size\n\n")
        
        cat("PANDUAN INTERPRETASI:\n")
        cat("η² < 0.01: Negligible effect\n")
        cat("η² 0.01-0.06: Small effect\n")
        cat("η² 0.06-0.14: Medium effect\n")
        cat("η² > 0.14: Large effect\n")
        
      } else {
        cat("EFFECT SIZES PER FAKTOR:\n")
        cat("Faktor 1 (", input$anova_factor1, "): η² =", 
            round(result$eta_squared_factor1, 4), "\n")
        cat("Faktor 2 (", input$anova_factor2, "): η² =", 
            round(result$eta_squared_factor2, 4), "\n")
        
        if (!is.null(result$eta_squared_interaction)) {
          cat("Interaksi: η² =", round(result$eta_squared_interaction, 4), "\n")
        }
      }
    }
  })
  
  output$anova_interpretation <- renderPrint({
    if (values$anova_done && !is.null(values$anova_result)) {
      cat("INTERPRETASI KOMPREHENSIF ANOVA\n")
      cat("===============================\n")
      
      result <- values$anova_result
      anova_table <- result$summary[[1]]
      
      if (result$type == "one_way") {
        p_value <- anova_table$`Pr(>F)`[1]
        eta_sq <- result$eta_squared
        
        cat("RINGKASAN TEMUAN:\n")
        if (p_value <= 0.001) {
          cat("- Perbedaan antar grup SANGAT SIGNIFIKAN (p < 0.001)\n")
        } else if (p_value <= 0.01) {
          cat("- Perbedaan antar grup SIGNIFIKAN (p < 0.01)\n")
        } else if (p_value <= 0.05) {
          cat("- Perbedaan antar grup SIGNIFIKAN (p < 0.05)\n")
        } else {
          cat("- Perbedaan antar grup TIDAK SIGNIFIKAN (p > 0.05)\n")
        }
        
        cat("- Effect size:", interpret_effect_size(eta_sq), "\n")
        cat("- Proporsi varians dijelaskan:", round(eta_sq * 100, 2), "%\n")
        
      } else {
        cat("RINGKASAN TWO-WAY ANOVA:\n")
        
        for (i in 1:(nrow(anova_table)-1)) {
          effect_name <- rownames(anova_table)[i]
          p_value <- anova_table$`Pr(>F)`[i]
          
          cat("\n", effect_name, ":\n")
          if (p_value <= 0.05) {
            cat("- SIGNIFIKAN: Ada efek dari", effect_name, "\n")
          } else {
            cat("- TIDAK SIGNIFIKAN: Tidak ada efek dari", effect_name, "\n")
          }
        }
      }
      
      cat("\nREKOMENDASI:\n")
      if (result$type == "one_way") {
        p_value <- anova_table$`Pr(>F)`[1]
        if (p_value <= 0.05) {
          cat("1. Lakukan post-hoc test untuk identifikasi grup yang berbeda\n")
          cat("2. Periksa asumsi ANOVA (normalitas, homogenitas)\n")
          cat("3. Pertimbangkan relevansi praktis dari perbedaan\n")
        } else {
          cat("1. Tidak ada bukti perbedaan antar grup\n")
          cat("2. Pertimbangkan power analysis\n")
          cat("3. Evaluasi kemungkinan Type II error\n")
        }
      } else {
        cat("1. Interpretasi efek utama harus mempertimbangkan interaksi\n")
        cat("2. Jika interaksi signifikan, fokus pada simple effects\n")
        cat("3. Gunakan plot interaksi untuk interpretasi visual\n")
      }
    }
  })
  
  # =============================================================================
  # REGRESI LINEAR BERGANDA
  # =============================================================================
  
  observeEvent(input$run_regression, {
    req(values$processed_data, input$reg_dependent, input$reg_independent)
    
    tryCatch({
      if (length(input$reg_independent) == 0) {
        safe_notification("Pilih minimal satu variabel independen", "warning")
        return()
      }
      
      # Build formula
      if (input$include_intercept) {
        formula <- as.formula(paste(input$reg_dependent, "~", 
                                    paste(input$reg_independent, collapse = "+")))
      } else {
        formula <- as.formula(paste(input$reg_dependent, "~ -1 +", 
                                    paste(input$reg_independent, collapse = "+")))
      }
      
      lm_model <- lm(formula, data = values$processed_data)
      values$regression_model <- lm_model
      values$regression_done <- TRUE
      
      safe_notification("Regresi selesai", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error regresi:", e$message), "error")
    })
  })
  
  output$regression_done <- reactive({
    values$regression_done
  })
  outputOptions(output, "regression_done", suspendWhenHidden = FALSE)
  
  output$regression_summary <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("RINGKASAN MODEL REGRESI\n")
      cat("=======================\n")
      cat("Variabel dependen:", input$reg_dependent, "\n")
      cat("Variabel independen:", paste(input$reg_independent, collapse = ", "), "\n")
      cat("Intercept:", if(input$include_intercept) "Included" else "Excluded", "\n\n")
      
      print(summary(model))
    }
  })
  
  output$model_fit_stats <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      model_summary <- summary(model)
      
      cat("STATISTIK KESESUAIAN MODEL\n")
      cat("==========================\n")
      cat("R-squared:", round(model_summary$r.squared, 4), "\n")
      cat("Adjusted R-squared:", round(model_summary$adj.r.squared, 4), "\n")
      cat("Residual Standard Error:", round(model_summary$sigma, 4), "\n")
      cat("F-statistic:", round(model_summary$fstatistic[1], 4), "\n")
      cat("P-value (F-test):", format_pvalue(pf(model_summary$fstatistic[1], 
                                                model_summary$fstatistic[2], 
                                                model_summary$fstatistic[3], 
                                                lower.tail = FALSE)), "\n")
      cat("AIC:", round(AIC(model), 4), "\n")
      cat("BIC:", round(BIC(model), 4), "\n")
      cat("Observations:", nobs(model), "\n")
      cat("Degrees of freedom:", model$df.residual, "\n")
    }
  })
  
  output$coefficients_table <- DT::renderDataTable({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      # Create coefficients table
      coeffs <- summary(model)$coefficients
      coeffs_df <- data.frame(
        Variable = rownames(coeffs),
        Estimate = round(coeffs[, 1], 4),
        Std_Error = round(coeffs[, 2], 4),
        t_value = round(coeffs[, 3], 4),
        p_value = round(coeffs[, 4], 4),
        Significance = ifelse(coeffs[, 4] <= 0.001, "***",
                              ifelse(coeffs[, 4] <= 0.01, "**",
                                     ifelse(coeffs[, 4] <= 0.05, "*",
                                            ifelse(coeffs[, 4] <= 0.1, ".", ""))))
      )
      
      DT::datatable(
        coeffs_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 't'
        ),
        class = 'cell-border stripe'
      ) %>%
        DT::formatStyle('p_value',
                        backgroundColor = DT::styleInterval(c(0.05), c('#ffcccc', '#ccffcc')))
    }
  })
  
  output$regression_diagnostics <- renderPlotly({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      # Create diagnostic plots
      df_resid <- data.frame(
        fitted = fitted(model),
        residuals = residuals(model),
        sqrt_abs_resid = sqrt(abs(residuals(model))),
        obs = 1:length(residuals(model))
      )
      
      # 1. Residuals vs Fitted
      p1 <- ggplot(df_resid, aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.6, color = waskita_colors$steel) +
        geom_hline(yintercept = 0, color = waskita_colors$error, linetype = "dashed") +
        geom_smooth(se = FALSE, color = waskita_colors$navy) +
        labs(title = "Residuals vs Fitted",
             x = "Fitted Values", y = "Residuals") +
        theme_waskita()
      
      # 2. Q-Q Plot of Residuals
      p2 <- ggplot(df_resid, aes(sample = residuals)) +
        stat_qq(color = waskita_colors$steel) +
        stat_qq_line(color = waskita_colors$navy) +
        labs(title = "Normal Q-Q Plot",
             x = "Theoretical Quantiles", y = "Sample Quantiles") +
        theme_waskita()
      
      # 3. Scale-Location Plot
      p3 <- ggplot(df_resid, aes(x = fitted, y = sqrt_abs_resid)) +
        geom_point(alpha = 0.6, color = waskita_colors$steel) +
        geom_smooth(se = FALSE, color = waskita_colors$navy) +
        labs(title = "Scale-Location Plot",
             x = "Fitted Values", y = "√|Residuals|") +
        theme_waskita()
      
      # 4. Cook's Distance
      cooks_dist <- cooks.distance(model)
      df_cook <- data.frame(
        obs = 1:length(cooks_dist),
        cooks_d = cooks_dist
      )
      
      p4 <- ggplot(df_cook, aes(x = obs, y = cooks_d)) +
        geom_col(fill = waskita_colors$steel, alpha = 0.7) +
        geom_hline(yintercept = 4/length(cooks_dist), color = waskita_colors$error, 
                   linetype = "dashed") +
        labs(title = "Cook's Distance",
             x = "Observation Number", y = "Cook's Distance") +
        theme_waskita()
      
      # Combine plots using subplot
      subplot(
        ggplotly(p1), ggplotly(p2),
        ggplotly(p3), ggplotly(p4),
        nrows = 2, shareX = FALSE, shareY = FALSE,
        titleX = TRUE, titleY = TRUE
      ) %>%
        layout(
          font = list(family = "Segoe UI", size = 10),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  output$residual_normality <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      residuals_data <- residuals(model)
      
      cat("UJI NORMALITAS RESIDUAL\n")
      cat("=======================\n")
      
      # Shapiro-Wilk test
      if (length(residuals_data) <= 5000) {
        shapiro_test <- shapiro.test(residuals_data)
        cat("Shapiro-Wilk Test:\n")
        cat("W =", round(shapiro_test$statistic, 4), "\n")
        cat("P-value =", format_pvalue(shapiro_test$p.value), "\n")
        
        if (shapiro_test$p.value > 0.05) {
          cat("KESIMPULAN: Residual berdistribusi normal\n")
        } else {
          cat("KESIMPULAN: Residual tidak berdistribusi normal\n")
        }
      } else {
        cat("Shapiro-Wilk tidak dapat dilakukan (n > 5000)\n")
        cat("Gunakan Q-Q plot untuk evaluasi visual\n")
      }
      
      # Jarque-Bera test
      if (requireNamespace("tseries", quietly = TRUE)) {
        jb_test <- tseries::jarque.bera.test(residuals_data)
        cat("\nJarque-Bera Test:\n")
        cat("JB =", round(jb_test$statistic, 4), "\n")
        cat("P-value =", format_pvalue(jb_test$p.value), "\n")
      }
    }
  })
  
  output$homoscedasticity_test <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("UJI HOMOSKEDASTISITAS\n")
      cat("=====================\n")
      
      # Breusch-Pagan test
      if (requireNamespace("lmtest", quietly = TRUE)) {
        bp_test <- lmtest::bptest(model)
        cat("Breusch-Pagan Test:\n")
        cat("BP =", round(bp_test$statistic, 4), "\n")
        cat("P-value =", format_pvalue(bp_test$p.value), "\n")
        
        if (bp_test$p.value > 0.05) {
          cat("KESIMPULAN: Homoskedastisitas terpenuhi\n")
        } else {
          cat("KESIMPULAN: Heteroskedastisitas terdeteksi\n")
        }
        
        # White test
        cat("\nWhite Test:\n")
        white_test <- lmtest::bptest(model, ~ fitted(model) + I(fitted(model)^2))
        cat("White =", round(white_test$statistic, 4), "\n")
        cat("P-value =", format_pvalue(white_test$p.value), "\n")
      }
    }
  })
  
  output$vif_results <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("VARIANCE INFLATION FACTOR (VIF)\n")
      cat("===============================\n")
      
      if (length(input$reg_independent) > 1) {
        if (requireNamespace("car", quietly = TRUE)) {
          vif_values <- car::vif(model)
          
          cat("VIF Values:\n")
          for (i in 1:length(vif_values)) {
            cat(names(vif_values)[i], ":", round(vif_values[i], 3), "\n")
          }
          
          cat("\nINTERPRETASI:\n")
          max_vif <- max(vif_values)
          if (max_vif < 5) {
            cat("- Tidak ada masalah multikolinearitas (VIF < 5)\n")
          } else if (max_vif < 10) {
            cat("- Multikolinearitas sedang (5 ≤ VIF < 10)\n")
          } else {
            cat("- Multikolinearitas tinggi (VIF ≥ 10)\n")
          }
          
          cat("\nPANDUAN:\n")
          cat("VIF < 5: Tidak ada masalah\n")
          cat("5 ≤ VIF < 10: Multikolinearitas sedang\n")
          cat("VIF ≥ 10: Multikolinearitas tinggi\n")
        }
      } else {
        cat("VIF tidak dapat dihitung (hanya 1 variabel independen)\n")
      }
    }
  })
  
  output$autocorrelation_test <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      
      cat("UJI AUTOKORELASI\n")
      cat("================\n")
      
      # Durbin-Watson test
      if (requireNamespace("lmtest", quietly = TRUE)) {
        dw_test <- lmtest::dwtest(model)
        cat("Durbin-Watson Test:\n")
        cat("DW =", round(dw_test$statistic, 4), "\n")
        cat("P-value =", format_pvalue(dw_test$p.value), "\n")
        
        dw_stat <- as.numeric(dw_test$statistic)
        if (dw_stat > 1.5 && dw_stat < 2.5) {
          cat("KESIMPULAN: Tidak ada autokorelasi\n")
        } else if (dw_stat < 1.5) {
          cat("KESIMPULAN: Autokorelasi positif terdeteksi\n")
        } else {
          cat("KESIMPULAN: Autokorelasi negatif terdeteksi\n")
        }
        
        cat("\nPANDUAN:\n")
        cat("DW ≈ 2: Tidak ada autokorelasi\n")
        cat("DW < 1.5: Autokorelasi positif\n")
        cat("DW > 2.5: Autokorelasi negatif\n")
      }
    }
  })
  
  output$regression_interpretation <- renderPrint({
    if (values$regression_done && !is.null(values$regression_model)) {
      model <- values$regression_model
      model_summary <- summary(model)
      
      cat("INTERPRETASI KOMPREHENSIF MODEL REGRESI\n")
      cat("=======================================\n")
      
      cat("1. KESESUAIAN MODEL:\n")
      r_squared <- model_summary$r.squared
      adj_r_squared <- model_summary$adj.r.squared
      
      cat("- R² =", round(r_squared, 4), "(", round(r_squared * 100, 2), "% varians dijelaskan)\n")
      cat("- Adjusted R² =", round(adj_r_squared, 4), "\n")
      
      if (r_squared < 0.3) {
        cat("- Model fit: LEMAH\n")
      } else if (r_squared < 0.7) {
        cat("- Model fit: SEDANG\n")
      } else {
        cat("- Model fit: KUAT\n")
      }
      
      cat("\n2. SIGNIFIKANSI MODEL:\n")
      f_stat <- model_summary$fstatistic[1]
      f_p_value <- pf(f_stat, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
      
      cat("- F-statistic:", round(f_stat, 4), "\n")
      cat("- P-value:", format_pvalue(f_p_value), "\n")
      
      if (f_p_value <= 0.05) {
        cat("- Model secara keseluruhan SIGNIFIKAN\n")
      } else {
        cat("- Model secara keseluruhan TIDAK SIGNIFIKAN\n")
      }
      
      cat("\n3. VARIABEL SIGNIFIKAN:\n")
      coeffs <- model_summary$coefficients
      sig_vars <- rownames(coeffs)[coeffs[, 4] <= 0.05]
      
      if (length(sig_vars) > 0) {
        for (var in sig_vars) {
          if (var != "(Intercept)") {
            coeff_val <- coeffs[var, 1]
            cat("- ", var, ": β =", round(coeff_val, 4))
            if (coeff_val > 0) {
              cat(" (pengaruh POSITIF)\n")
            } else {
              cat(" (pengaruh NEGATIF)\n")
            }
          }
        }
      } else {
        cat("- Tidak ada variabel independen yang signifikan\n")
      }
      
      cat("\n4. REKOMENDASI:\n")
      if (f_p_value <= 0.05 && r_squared >= 0.3) {
        cat("- Model dapat digunakan untuk prediksi\n")
        cat("- Periksa asumsi regresi (normalitas, homoskedastisitas)\n")
        cat("- Evaluasi multikolinearitas jika ada banyak prediktor\n")
      } else if (f_p_value <= 0.05 && r_squared < 0.3) {
        cat("- Model signifikan tapi daya prediksi lemah\n")
        cat("- Pertimbangkan menambah variabel prediktor\n")
        cat("- Evaluasi transformasi variabel\n")
      } else {
        cat("- Model tidak signifikan\n")
        cat("- Pertimbangkan model alternatif\n")
        cat("- Evaluasi pemilihan variabel\n")
      }
    }
  })
  
  # =============================================================================
  # ANALISIS SPASIAL
  # =============================================================================
  
  observeEvent(input$run_spatial_analysis, {
    req(values$processed_data, input$spatial_variable)
    
    tryCatch({
      # Get variable data
      var_data <- values$processed_data[[input$spatial_variable]]
      n_obs <- length(var_data)
      
      # Create distance matrix (synthetic for demonstration)
      set.seed(123)
      coords <- data.frame(
        x = runif(n_obs, 0, 100),
        y = runif(n_obs, 0, 100)
      )
      
      # Calculate distance matrix
      dist_matrix <- as.matrix(dist(coords))
      
      # Create weights matrix based on selected method
      weights_matrix <- switch(input$weight_type,
                               "distance_inverse" = {
                                 w <- 1 / (dist_matrix + 1)  # Add 1 to avoid division by zero
                                 diag(w) <- 0  # Set diagonal to 0
                                 w
                               },
                               "distance_exp" = {
                                 w <- exp(-dist_matrix / mean(dist_matrix))
                                 diag(w) <- 0
                                 w
                               },
                               "knn" = {
                                 k <- input$k_neighbors
                                 w <- matrix(0, n_obs, n_obs)
                                 for (i in 1:n_obs) {
                                   neighbors <- order(dist_matrix[i, ])[2:(k+1)]  # Exclude self
                                   w[i, neighbors] <- 1
                                 }
                                 w
                               }
      )
      
      # Calculate Moran's I
      morans_i <- calculate_morans_i(var_data, weights_matrix)
      
      # Calculate expected value and variance for significance test
      n <- length(var_data)
      expected_i <- -1 / (n - 1)
      
      # Simplified variance calculation
      s0 <- sum(weights_matrix)
      s1 <- 0.5 * sum((weights_matrix + t(weights_matrix))^2)
      s2 <- sum(apply(weights_matrix, 1, sum)^2)
      
      b2 <- n * sum((var_data - mean(var_data))^4) / (sum((var_data - mean(var_data))^2)^2)
      
      var_i <- ((n * ((n^2 - 3*n + 3) * s1 - n*s2 + 3*s0^2)) - 
                  (b2 * ((n^2 - n) * s1 - 2*n*s2 + 6*s0^2))) / 
        (((n-1) * (n-2) * (n-3) * s0^2))
      
      # Z-score and p-value
      z_score <- (morans_i - expected_i) / sqrt(var_i)
      p_value <- 2 * (1 - pnorm(abs(z_score)))
      
      values$spatial_result <- list(
        morans_i = morans_i,
        expected_i = expected_i,
        z_score = z_score,
        p_value = p_value,
        weights_matrix = weights_matrix,
        coords = coords
      )
      
      values$spatial_done <- TRUE
      safe_notification("Analisis spasial selesai", "success")
      
    }, error = function(e) {
      safe_notification(paste("Error analisis spasial:", e$message), "error")
    })
  })
  
  output$spatial_done <- reactive({
    values$spatial_done
  })
  outputOptions(output, "spatial_done", suspendWhenHidden = FALSE)
  
  output$spatial_autocorr_result <- renderPrint({
    if (values$spatial_done && !is.null(values$spatial_result)) {
      result <- values$spatial_result
      
      cat("HASIL AUTOKORELASI SPASIAL\n")
      cat("==========================\n")
      cat("Variabel:", input$spatial_variable, "\n")
      cat("Metode weights:", switch(input$weight_type,
                                    "distance_inverse" = "Distance-based (Inverse)",
                                    "distance_exp" = "Distance-based (Exponential)",
                                    "knn" = paste("K-Nearest Neighbors (k =", input$k_neighbors, ")")), "\n\n")
      
      cat("MORAN'S I STATISTICS:\n")
      cat("Observed I:", round(result$morans_i, 4), "\n")
      cat("Expected I:", round(result$expected_i, 4), "\n")
      cat("Z-score:", round(result$z_score, 4), "\n")
      cat("P-value:", format_pvalue(result$p_value), "\n\n")
      
      cat("INTERPRETASI:\n")
      if (result$p_value <= 0.05) {
        if (result$morans_i > result$expected_i) {
          cat("- AUTOKORELASI SPASIAL POSITIF SIGNIFIKAN\n")
          cat("- Nilai serupa cenderung berklaster secara spasial\n")
        } else {
          cat("- AUTOKORELASI SPASIAL NEGATIF SIGNIFIKAN\n")
          cat("- Nilai berbeda cenderung berdekatan secara spasial\n")
        }
      } else {
        cat("- TIDAK ADA AUTOKORELASI SPASIAL SIGNIFIKAN\n")
        cat("- Distribusi spasial acak\n")
      }
      
      cat("\nPANDUAN INTERPRETASI:\n")
      cat("I > E(I): Autokorelasi positif (clustering)\n")
      cat("I < E(I): Autokorelasi negatif (dispersi)\n")
      cat("I ≈ E(I): Distribusi acak\n")
    }
  })
  
  output$spatial_plot <- renderPlotly({
    if (values$spatial_done && !is.null(values$spatial_result)) {
      result <- values$spatial_result
      coords <- result$coords
      var_data <- values$processed_data[[input$spatial_variable]]
      
      # Create scatter plot of coordinates colored by variable value
      plot_data <- data.frame(
        x = coords$x,
        y = coords$y,
        value = var_data
      )
      
      p <- ggplot(plot_data, aes(x = x, y = y, color = value)) +
        geom_point(size = 3, alpha = 0.7) +
        scale_color_gradient2(low = waskita_colors$error, mid = "white", 
                              high = waskita_colors$navy, midpoint = median(var_data, na.rm = TRUE)) +
        labs(title = paste("Spatial Distribution of", input$spatial_variable),
             x = "X Coordinate", y = "Y Coordinate",
             color = input$spatial_variable) +
        theme_waskita() +
        theme(aspect.ratio = 1)
      
      ggplotly(p) %>%
        layout(
          font = list(family = "Segoe UI", size = 12),
          plot_bgcolor = 'rgba(0,0,0,0)',
          paper_bgcolor = 'rgba(0,0,0,0)'
        )
    }
  })
  
  output$distance_matrix_stats <- renderPrint({
    if (values$spatial_done && !is.null(values$spatial_result)) {
      result <- values$spatial_result
      weights_matrix <- result$weights_matrix
      
      cat("STATISTIK MATRIKS PENIMBANG\n")
      cat("===========================\n")
      cat("Dimensi:", nrow(weights_matrix), "x", ncol(weights_matrix), "\n")
      cat("Total weights:", round(sum(weights_matrix), 2), "\n")
      cat("Mean weight:", round(mean(weights_matrix[weights_matrix > 0]), 4), "\n")
      cat("Non-zero weights:", sum(weights_matrix > 0), "\n")
      cat("Sparsity:", round((1 - sum(weights_matrix > 0) / length(weights_matrix)) * 100, 2), "%\n")
      
      # Row sums statistics
      row_sums <- apply(weights_matrix, 1, sum)
      cat("\nROW SUMS (Connectivity):\n")
      cat("Min:", round(min(row_sums), 4), "\n")
      cat("Max:", round(max(row_sums), 4), "\n")
      cat("Mean:", round(mean(row_sums), 4), "\n")
      cat("SD:", round(sd(row_sums), 4), "\n")
    }
  })
  
  output$spatial_interpretation <- renderPrint({
    if (values$spatial_done && !is.null(values$spatial_result)) {
      result <- values$spatial_result
      
      cat("INTERPRETASI ANALISIS SPASIAL\n")
      cat("=============================\n")
      
      cat("1. AUTOKORELASI SPASIAL:\n")
      morans_i <- result$morans_i
      expected_i <- result$expected_i
      p_value <- result$p_value
      
      if (p_value <= 0.001) {
        cat("- Autokorelasi SANGAT SIGNIFIKAN (p < 0.001)\n")
      } else if (p_value <= 0.01) {
        cat("- Autokorelasi SIGNIFIKAN (p < 0.01)\n")
      } else if (p_value <= 0.05) {
        cat("- Autokorelasi SIGNIFIKAN (p < 0.05)\n")
      } else {
        cat("- Autokorelasi TIDAK SIGNIFIKAN (p > 0.05)\n")
      }
      
      cat("\n2. POLA SPASIAL:\n")
      if (p_value <= 0.05) {
        if (morans_i > expected_i) {
          cat("- CLUSTERING: Nilai serupa bergerombol\n")
          cat("- Hot spots dan cold spots teridentifikasi\n")
          cat("- Proses spasial yang menghasilkan dependensi\n")
        } else {
          cat("- DISPERSI: Nilai berbeda berdekatan\n")
          cat("- Pola checkerboard atau kompetisi spasial\n")
          cat("- Proses yang menghasilkan repulsi spasial\n")
        }
      } else {
        cat("- RANDOM: Distribusi acak secara spasial\n")
        cat("- Tidak ada pola spasial yang sistematis\n")
        cat("- Independensi spasial\n")
      }
      
      cat("\n3. IMPLIKASI METODOLOGIS:\n")
      if (p_value <= 0.05) {
        cat("- Gunakan model spasial (SAR, SEM, SDM)\n")
        cat("- Standard errors dalam OLS bias\n")
        cat("- Pertimbangkan spatial lag atau spatial error\n")
      } else {
        cat("- Model non-spasial (OLS) dapat digunakan\n")
        cat("- Asumsi independensi terpenuhi\n")
        cat("- Standard errors OLS valid\n")
      }
      
      cat("\n4. REKOMENDASI KEBIJAKAN:\n")
      if (p_value <= 0.05 && morans_i > expected_i) {
        cat("- Identifikasi cluster untuk intervensi targeted\n")
        cat("- Pertimbangkan spillover effects\n")
        cat("- Koordinasi kebijakan antar wilayah\n")
      } else {
        cat("- Kebijakan dapat diterapkan secara independen\n")
        cat("- Fokus pada karakteristik lokal\n")
        cat("- Minimal spillover effects\n")
      }
    }
  })
  
  # =============================================================================
  # DOWNLOAD HANDLERS
  # =============================================================================
  
  # Download original data
  output$download_original <- downloadHandler(
    filename = function() {
      create_download_filename("original_data", "csv")
    },
    content = function(file) {
      if (!is.null(values$original_data)) {
        write.csv(values$original_data, file, row.names = FALSE)
      }
    }
  )
  
  # Download processed data
  output$download_processed <- downloadHandler(
    filename = function() {
      create_download_filename("processed_data", "csv")
    },
    content = function(file) {
      if (!is.null(values$processed_data)) {
        write.csv(values$processed_data, file, row.names = FALSE)
      }
    }
  )
  
  # Download descriptive stats
  output$download_desc_stats <- downloadHandler(
    filename = function() {
      create_download_filename("descriptive_stats", "csv")
    },
    content = function(file) {
      if (values$desc_generated && !is.null(values$current_desc_variable)) {
        var_data <- values$processed_data[[values$current_desc_variable]]
        
        if (is.numeric(var_data)) {
          stats_df <- data.frame(
            Statistic = c("N", "Missing", "Mean", "Median", "SD", "Min", "Max", "Q1", "Q3", "IQR"),
            Value = c(
              sum(!is.na(var_data)),
              sum(is.na(var_data)),
              round(mean(var_data, na.rm = TRUE), 4),
              round(median(var_data, na.rm = TRUE), 4),
              round(sd(var_data, na.rm = TRUE), 4),
              round(min(var_data, na.rm = TRUE), 4),
              round(max(var_data, na.rm = TRUE), 4),
              round(quantile(var_data, 0.25, na.rm = TRUE), 4),
              round(quantile(var_data, 0.75, na.rm = TRUE), 4),
              round(IQR(var_data, na.rm = TRUE), 4)
            )
          )
        } else {
          tbl <- table(var_data, useNA = "ifany")
          stats_df <- data.frame(
            Category = names(tbl),
            Frequency = as.numeric(tbl),
            Proportion = round(as.numeric(prop.table(tbl)), 4)
          )
        }
        
        write.csv(stats_df, file, row.names = FALSE)
      }
    }
  )
  
  # Additional download handlers can be added here for other analyses...
  
}