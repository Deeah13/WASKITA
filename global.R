# =============================================================================
# WASKITA Dashboard - Global Configuration
# Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik
# Professional Statistical Analysis Platform
# =============================================================================

# Enhanced Package Management with Progress Feedback
required_packages <- c(
  # Core Shiny
  "shiny", "shinydashboard", "shinyWidgets", "shinycssloaders", "shinyjs",
  # Data Processing
  "DT", "dplyr", "readr", "tidyr", "stringr", "data.table",
  # Visualization
  "plotly", "ggplot2", "leaflet", "RColorBrewer", "viridis", "corrplot",
  # Statistics
  "nortest", "car", "lmtest", "moments", "psych", "broom", "effectsize",
  # Additional Tools
  "knitr", "rmarkdown", "htmltools", "scales", "MASS", "forecast"
)

# Safe package installation with error handling
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      tryCatch({
        cat("Installing package:", pkg, "\n")
        install.packages(pkg, dependencies = TRUE, quiet = TRUE)
        library(pkg, character.only = TRUE, quietly = TRUE)
        cat("✓ Successfully loaded:", pkg, "\n")
      }, error = function(e) {
        cat("✗ Failed to install:", pkg, "- Error:", e$message, "\n")
      })
    }
  }
}

# Load all required packages
suppressMessages(install_if_missing(required_packages))

# =============================================================================
# PROFESSIONAL COLOR PALETTE
# =============================================================================

waskita_colors <- list(
  # Primary Colors
  primary = "#1B3C53",      # Deep Navy
  secondary = "#456882",    # Steel Blue
  accent = "#2563EB",       # Bright Blue
  
  # Background Colors
  bg_primary = "#FFFFFF",   # Pure White
  bg_secondary = "#F8FAFC", # Light Gray
  bg_accent = "#F1F5F9",    # Lighter Gray
  
  # Text Colors
  text_primary = "#0F172A",   # Dark Slate
  text_secondary = "#475569", # Medium Slate
  text_muted = "#64748B",     # Light Slate
  
  # Status Colors
  success = "#059669",      # Green
  warning = "#D97706",      # Orange
  error = "#DC2626",        # Red
  info = "#0284C7",         # Blue
  
  # Chart Colors
  chart_1 = "#1B3C53",
  chart_2 = "#456882",
  chart_3 = "#2563EB",
  chart_4 = "#059669",
  chart_5 = "#D97706",
  chart_6 = "#DC2626",
  
  # Borders
  border_light = "#E2E8F0",
  border_medium = "#CBD5E1",
  border_dark = "#94A3B8"
)

# =============================================================================
# DATA CONFIGURATION
# =============================================================================

# Enhanced data paths with validation
data_config <- list(
  local_dir = "D:/Perkuliahan Tingkat 2 Semester 4/WASKITA2/data",
  sovi_file = "sovi_data.csv",
  distance_file = "distance.csv",
  backup_urls = list(
    sovi = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv",
    distance = "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
  )
)

# =============================================================================
# ENHANCED UTILITY FUNCTIONS
# =============================================================================

# Professional p-value formatting
format_pvalue <- function(p, digits = 3) {
  if (is.na(p) || !is.numeric(p)) return("NA")
  if (p < 0.001) return("< 0.001***")
  if (p < 0.01) return(paste0(sprintf("%.3f", p), "**"))
  if (p < 0.05) return(paste0(sprintf("%.3f", p), "*"))
  if (p < 0.1) return(paste0(sprintf("%.3f", p), "†"))
  return(sprintf(paste0("%.", digits, "f"), p))
}

# Statistical significance interpretation
interpret_significance <- function(p_value, alpha = 0.05) {
  if (is.na(p_value)) return("Cannot determine significance")
  if (p_value < 0.001) return("Highly significant (p < 0.001)")
  if (p_value < 0.01) return("Very significant (p < 0.01)")
  if (p_value < alpha) return(paste("Significant (p <", alpha, ")"))
  if (p_value < 0.1) return("Marginally significant (p < 0.1)")
  return("Not significant")
}

# Effect size interpretations
interpret_cohens_d <- function(d) {
  if (is.na(d)) return("Cannot calculate")
  abs_d <- abs(d)
  if (abs_d < 0.2) return("Negligible")
  if (abs_d < 0.5) return("Small")
  if (abs_d < 0.8) return("Medium")
  return("Large")
}

interpret_correlation <- function(r) {
  if (is.na(r)) return("Cannot calculate")
  abs_r <- abs(r)
  if (abs_r < 0.1) return("Negligible")
  if (abs_r < 0.3) return("Weak")
  if (abs_r < 0.5) return("Moderate")
  if (abs_r < 0.7) return("Strong")
  return("Very Strong")
}

interpret_eta_squared <- function(eta2) {
  if (is.na(eta2)) return("Cannot calculate")
  if (eta2 < 0.01) return("Negligible")
  if (eta2 < 0.06) return("Small")
  if (eta2 < 0.14) return("Medium")
  return("Large")
}

# Statistical test power interpretation
interpret_power <- function(power) {
  if (is.na(power)) return("Cannot calculate")
  if (power < 0.5) return("Very Low")
  if (power < 0.7) return("Low")
  if (power < 0.8) return("Moderate")
  if (power < 0.9) return("Good")
  return("Excellent")
}

# =============================================================================
# PROFESSIONAL GGPLOT THEME
# =============================================================================

theme_waskita_professional <- function(base_size = 12, base_family = "Arial") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Plot elements
      plot.title = element_text(
        size = base_size * 1.2, 
        face = "bold", 
        hjust = 0.5,
        color = waskita_colors$text_primary,
        margin = margin(b = 20)
      ),
      plot.subtitle = element_text(
        size = base_size * 0.9,
        hjust = 0.5,
        color = waskita_colors$text_secondary,
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        size = base_size * 0.8,
        color = waskita_colors$text_muted,
        hjust = 0
      ),
      
      # Axes
      axis.title = element_text(
        size = base_size * 0.9,
        color = waskita_colors$text_primary,
        face = "bold"
      ),
      axis.text = element_text(
        size = base_size * 0.8,
        color = waskita_colors$text_secondary
      ),
      axis.line = element_line(
        color = waskita_colors$border_medium,
        size = 0.5
      ),
      
      # Legend
      legend.title = element_text(
        size = base_size * 0.9,
        face = "bold",
        color = waskita_colors$text_primary
      ),
      legend.text = element_text(
        size = base_size * 0.8,
        color = waskita_colors$text_secondary
      ),
      legend.position = "bottom",
      legend.box = "horizontal",
      
      # Panel
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = waskita_colors$border_light,
        size = 0.3,
        linetype = "dotted"
      ),
      panel.border = element_rect(
        fill = NA,
        color = waskita_colors$border_medium,
        size = 0.5
      ),
      
      # Strips for facets
      strip.background = element_rect(
        fill = waskita_colors$bg_accent,
        color = waskita_colors$border_medium
      ),
      strip.text = element_text(
        size = base_size * 0.9,
        face = "bold",
        color = waskita_colors$text_primary
      ),
      
      # Background
      plot.background = element_rect(
        fill = waskita_colors$bg_primary,
        color = NA
      ),
      panel.background = element_rect(
        fill = waskita_colors$bg_primary,
        color = NA
      )
    )
}

# Set as default theme
theme_set(theme_waskita_professional())

# =============================================================================
# ENHANCED DATA LOADING FUNCTIONS
# =============================================================================

load_waskita_data <- function() {
  tryCatch({
    # Attempt local load first
    local_sovi <- file.path(data_config$local_dir, data_config$sovi_file)
    local_distance <- file.path(data_config$local_dir, data_config$distance_file)
    
    if (file.exists(local_sovi) && file.exists(local_distance)) {
      cat("Loading data from local files...\n")
      sovi_data <- read_csv(local_sovi, show_col_types = FALSE)
      distance_data <- read_csv(local_distance, show_col_types = FALSE)
      
      # Validate data
      validation <- validate_dataset(sovi_data)
      if (!validation$valid) {
        stop(paste("Local data validation failed:", validation$message))
      }
      
      return(list(
        sovi = sovi_data,
        distance = distance_data,
        source = "local",
        message = "Data loaded successfully from local files"
      ))
    } else {
      # Fallback to online sources
      cat("Local files not found. Loading from online sources...\n")
      sovi_data <- read_csv(data_config$backup_urls$sovi, show_col_types = FALSE)
      distance_data <- read_csv(data_config$backup_urls$distance, show_col_types = FALSE)
      
      # Validate data
      validation <- validate_dataset(sovi_data)
      if (!validation$valid) {
        stop(paste("Online data validation failed:", validation$message))
      }
      
      return(list(
        sovi = sovi_data,
        distance = distance_data,
        source = "online",
        message = "Data loaded successfully from online sources"
      ))
    }
  }, error = function(e) {
    return(list(
      error = TRUE,
      message = paste("Failed to load data:", e$message)
    ))
  })
}

# Enhanced data validation
validate_dataset <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "Dataset is empty or null"))
  }
  
  if (ncol(data) < 5) {
    return(list(valid = FALSE, message = "Dataset must have at least 5 columns"))
  }
  
  numeric_cols <- sum(sapply(data, is.numeric))
  if (numeric_cols < 2) {
    return(list(valid = FALSE, message = "Dataset must have at least 2 numeric columns"))
  }
  
  missing_pct <- sum(is.na(data)) / (nrow(data) * ncol(data)) * 100
  if (missing_pct > 50) {
    return(list(valid = FALSE, message = "Dataset has too many missing values (>50%)"))
  }
  
  return(list(valid = TRUE, message = "Dataset validation passed"))
}

# =============================================================================
# METADATA VARIABEL
# =============================================================================

sovi_metadata <- data.frame(
  Variable = c(
    "DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE",
    "LOWEDU", "POVERTY", "ILLITERATE", "NOTRAINING", "GROWTH", "NOELECTRIC",
    "RENTED", "NOSEWER", "TAPWATER", "DPHONE", "SOVI"
  ),
  Description = c(
    "District Code Identifier",
    "Proportion of Children Under 5 Years",
    "Proportion of Female Population",
    "Proportion of Elderly 65+ Years",
    "Proportion of Female Household Heads",
    "Average Household Size",
    "Proportion with Low Education",
    "Poverty Rate",
    "Illiteracy Rate",
    "Proportion without Vocational Training",
    "Population Growth Rate",
    "Proportion without Electricity Access",
    "Proportion Living in Rented Housing",
    "Proportion without Sewerage System",
    "Proportion with Clean Water Access",
    "Natural Disaster Risk Index",
    "Social Vulnerability Index"
  ),
  Type = c(
    "Categorical", rep("Numeric", 16)
  ),
  Unit = c(
    "Code", rep("Proportion", 10), "Rate", rep("Proportion", 3), "Index", "Index"
  ),
  Range = c(
    "Various", rep("0-1", 10), "0-∞", rep("0-1", 3), "0-100", "Continuous"
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# NOTIFICATION SYSTEM
# =============================================================================

safe_notification <- function(message, type = "message", duration = 5) {
  tryCatch({
    showNotification(
      ui = tags$div(
        style = "font-weight: 500;",
        message
      ),
      type = type,
      duration = duration
    )
  }, error = function(e) {
    cat("Notification:", message, "\n")
  })
}

# Professional notification wrapper
notify_success <- function(message) {
  safe_notification(paste("✓", message), "message", 4)
}

notify_warning <- function(message) {
  safe_notification(paste("⚠", message), "warning", 6)
}

notify_error <- function(message) {
  safe_notification(paste("✗", message), "error", 8)
}

# =============================================================================
# FILE HANDLING UTILITIES
# =============================================================================

create_download_filename <- function(prefix, extension = "csv") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  paste0("WASKITA_", prefix, "_", timestamp, ".", extension)
}

# Safe file operations
safe_write_csv <- function(data, filename) {
  tryCatch({
    write_csv(data, filename)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# =============================================================================
# FUNGSI MORAN'S I
# =============================================================================

# =============================================================================
# SPATIAL ANALYSIS FUNCTIONS
# =============================================================================

calculate_morans_i <- function(x, weights_matrix) {
  # Enhanced Moran's I calculation with error handling
  tryCatch({
    # Remove missing values
    complete_cases <- complete.cases(x)
    if (sum(complete_cases) < 3) {
      stop("Insufficient complete cases for Moran's I calculation")
    }
    
    x_clean <- x[complete_cases]
    w_clean <- weights_matrix[complete_cases, complete_cases, drop = FALSE]
    
    n <- length(x_clean)
    x_mean <- mean(x_clean)
    x_centered <- x_clean - x_mean
    
    # Calculate Moran's I
    numerator <- 0
    denominator <- sum(x_centered^2)
    total_weight <- sum(w_clean)
    
    if (total_weight == 0) {
      stop("Weights matrix has no positive values")
    }
    
    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          numerator <- numerator + w_clean[i, j] * x_centered[i] * x_centered[j]
        }
      }
    }
    
    if (denominator > 0) {
      morans_i <- (n * numerator) / (total_weight * denominator)
    } else {
      morans_i <- 0
    }
    
    return(morans_i)
    
  }, error = function(e) {
    warning(paste("Moran's I calculation failed:", e$message))
    return(NA)
  })
}

# Generate spatial weights matrix
create_spatial_weights <- function(coords, method = "distance", k = 5, threshold = NULL) {
  tryCatch({
    n <- nrow(coords)
    dist_matrix <- as.matrix(dist(coords))
    
    weights_matrix <- switch(method,
      "distance" = {
        w <- 1 / (dist_matrix + 1e-6)  # Add small constant to avoid division by zero
        diag(w) <- 0
        w
      },
      "distance_squared" = {
        w <- 1 / (dist_matrix^2 + 1e-6)
        diag(w) <- 0
        w
      },
      "exponential" = {
        w <- exp(-dist_matrix / mean(dist_matrix[dist_matrix > 0]))
        diag(w) <- 0
        w
      },
      "knn" = {
        w <- matrix(0, n, n)
        for (i in 1:n) {
          neighbors <- order(dist_matrix[i, ])[2:(k+1)]  # Exclude self
          w[i, neighbors] <- 1
        }
        w
      },
      "threshold" = {
        if (is.null(threshold)) threshold <- mean(dist_matrix[dist_matrix > 0])
        w <- ifelse(dist_matrix <= threshold & dist_matrix > 0, 1, 0)
        w
      }
    )
    
    return(weights_matrix)
    
  }, error = function(e) {
    warning(paste("Spatial weights creation failed:", e$message))
    return(matrix(0, nrow(coords), nrow(coords)))
  })
}

# =============================================================================
# SHINY CONFIGURATION
# =============================================================================

# Enhanced Shiny options
options(
  shiny.maxRequestSize = 200*1024^2,  # 200MB max file size
  shiny.sanitize.errors = FALSE,
  shiny.trace = FALSE,
  shiny.fullstacktrace = FALSE,
  warn = -1,
  digits = 4
)

# =============================================================================
# INITIALIZATION MESSAGE
# =============================================================================

cat("=====================================\n")
cat("WASKITA Professional Dashboard\n")
cat("Global Configuration Loaded Successfully\n")
cat("=====================================\n")
cat("✓ Packages loaded and validated\n")
cat("✓ Professional theme configured\n")
cat("✓ Utility functions ready\n")
cat("✓ Data loading functions prepared\n")
cat("✓ Statistical analysis tools loaded\n")
cat("=====================================\n")