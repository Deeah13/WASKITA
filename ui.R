# WASKITA Professional Dashboard - User Interface
# Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik
# Professional Statistical Analysis Platform

source("global.R")

# Professional CSS and UI components
professional_css <- tags$head(
  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap"),
  tags$style(HTML(paste0("
    body, .content-wrapper, .right-side {
      background: linear-gradient(135deg, ", waskita_colors$bg_secondary, " 0%, ", waskita_colors$bg_primary, " 100%) !important;
      font-family: \"Inter\", -apple-system, BlinkMacSystemFont, \"Segoe UI\", sans-serif !important;
      color: ", waskita_colors$text_primary, " !important;
      line-height: 1.6 !important;
    }
    
    .main-header .navbar {
      background: linear-gradient(135deg, ", waskita_colors$primary, " 0%, ", waskita_colors$secondary, " 100%) !important;
      border: none !important;
      box-shadow: 0 4px 20px rgba(27, 60, 83, 0.15) !important;
    }
    
    .main-sidebar {
      background: ", waskita_colors$primary, " !important;
      box-shadow: 4px 0 15px rgba(0, 0, 0, 0.1) !important;
    }
    
    .sidebar-menu > li > a {
      color: rgba(255, 255, 255, 0.9) !important;
      padding: 16px 24px !important;
      margin: 4px 12px !important;
      border-radius: 12px !important;
      transition: all 0.3s ease !important;
      font-weight: 500 !important;
    }
    
    .sidebar-menu > li:hover > a,
    .sidebar-menu > li.active > a {
      background: rgba(255, 255, 255, 0.12) !important;
      color: ", waskita_colors$bg_primary, " !important;
      transform: translateX(6px) !important;
      box-shadow: 0 4px 12px rgba(255, 255, 255, 0.1) !important;
    }
    
    .box {
      border: 1px solid ", waskita_colors$border_light, " !important;
      border-radius: 16px !important;
      box-shadow: 0 4px 20px rgba(0, 0, 0, 0.06) !important;
      background: ", waskita_colors$bg_primary, " !important;
      margin-bottom: 30px !important;
      transition: all 0.3s ease !important;
    }
    
    .box:hover {
      box-shadow: 0 8px 30px rgba(0, 0, 0, 0.12) !important;
      transform: translateY(-2px) !important;
    }
    
    .box-header {
      background: linear-gradient(135deg, ", waskita_colors$primary, " 0%, ", waskita_colors$secondary, " 100%) !important;
      color: ", waskita_colors$bg_primary, " !important;
      padding: 24px 30px !important;
      font-weight: 600 !important;
      font-size: 18px !important;
    }
    
    .btn {
      border-radius: 12px !important;
      padding: 14px 28px !important;
      font-weight: 600 !important;
      transition: all 0.3s ease !important;
      border: none !important;
    }
    
    .btn-primary {
      background: linear-gradient(135deg, ", waskita_colors$primary, " 0%, ", waskita_colors$accent, " 100%) !important;
      color: ", waskita_colors$bg_primary, " !important;
      box-shadow: 0 6px 20px rgba(27, 60, 83, 0.3) !important;
    }
    
    .btn-primary:hover {
      transform: translateY(-2px) !important;
      box-shadow: 0 8px 25px rgba(27, 60, 83, 0.4) !important;
    }
    
    .btn-success {
      background: linear-gradient(135deg, ", waskita_colors$success, " 0%, #047857 100%) !important;
      color: ", waskita_colors$bg_primary, " !important;
    }
    
    .btn-warning {
      background: linear-gradient(135deg, ", waskita_colors$warning, " 0%, #b45309 100%) !important;
      color: ", waskita_colors$bg_primary, " !important;
    }
    
    .form-control, .selectize-input {
      border-radius: 12px !important;
      border: 2px solid ", waskita_colors$border_light, " !important;
      padding: 14px 18px !important;
      transition: all 0.3s ease !important;
    }
    
    .form-control:focus, .selectize-input.focus {
      border-color: ", waskita_colors$accent, " !important;
      box-shadow: 0 0 0 4px rgba(37, 99, 235, 0.1) !important;
      outline: none !important;
    }
  ")))
)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$i(class = "fas fa-chart-network", style = "margin-right: 12px;"),
      "WASKITA Professional"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    professional_css,
    
    sidebarMenu(
      id = "sidebar_menu",
      
      menuItem("Dashboard", 
               tabName = "dashboard", 
               icon = icon("tachometer-alt")),
      
      menuItem("Data Management", 
               tabName = "data_management", 
               icon = icon("database")),
      
      menuItem("Descriptive Analysis", 
               tabName = "descriptive", 
               icon = icon("chart-bar")),
      
      menuItem("Data Visualization", 
               tabName = "visualization", 
               icon = icon("chart-line")),
      
      menuItem("Statistical Testing", 
               tabName = "statistical_testing", 
               icon = icon("calculator")),
      
      menuItem("T-Tests", 
               tabName = "t_tests", 
               icon = icon("vial")),
      
      menuItem("ANOVA", 
               tabName = "anova", 
               icon = icon("layer-group")),
      
      menuItem("Regression Analysis", 
               tabName = "regression", 
               icon = icon("trending-up")),
      
      menuItem("Spatial Analysis", 
               tabName = "spatial_analysis", 
               icon = icon("globe-americas"))
    )
  ),
  
  dashboardBody(
    professional_css,
    
    tabItems(
      
      # Dashboard Home
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          column(12,
                 div(style = paste0("
                   background: linear-gradient(135deg, ", waskita_colors$primary, " 0%, ", waskita_colors$secondary, " 50%, ", waskita_colors$accent, " 100%);
                   border-radius: 24px;
                   padding: 60px 50px;
                   margin: 30px 0 50px 0;
                   text-align: center;
                   color: ", waskita_colors$bg_primary, ";
                   box-shadow: 0 20px 40px rgba(27, 60, 83, 0.25);
                 "),
                     div(style = "display: inline-flex; align-items: center; background: rgba(255,255,255,0.25); padding: 12px 24px; border-radius: 30px; margin-bottom: 25px; backdrop-filter: blur(15px); font-weight: 600;",
                         tags$i(class = "fas fa-university", style = "margin-right: 10px;"),
                         "Professional Statistical Analysis Platform"
                     ),
                     h1("WASKITA", style = "font-size: 4rem; font-weight: 800; margin: 25px 0; letter-spacing: 2px; text-shadow: 0 4px 20px rgba(0,0,0,0.3);"),
                     h2("Advanced Social Vulnerability Index Analytics", style = "font-size: 1.9rem; font-weight: 400; margin-bottom: 30px; opacity: 0.95;"),
                     p("Comprehensive statistical analysis platform designed for professional researchers and data scientists. Featuring advanced spatial analytics, inferential statistics, and interactive visualizations for Social Vulnerability Index research with publication-ready outputs.", 
                       style = "font-size: 1.3rem; line-height: 1.8; max-width: 900px; margin: 0 auto; opacity: 0.9;")
                 )
          )
        ),
        
        div(style = "text-align: center; margin: 50px 0 40px 0;",
            h2(style = paste0("color: ", waskita_colors$primary, "; font-weight: 700; font-size: 2.5rem; margin-bottom: 15px;"),
               tags$i(class = "fas fa-rocket", style = paste0("margin-right: 18px; color: ", waskita_colors$accent, ";")),
               "Analytics Modules"),
            p(style = paste0("color: ", waskita_colors$text_secondary, "; font-size: 1.2rem; max-width: 700px; margin: 0 auto;"),
              "Professional-grade statistical analysis tools for comprehensive Social Vulnerability Index research")
        ),
        
        fluidRow(
          column(4,
                 div(class = "professional-card", onclick = "Shiny.setInputValue('sidebar_menu', 'data_management', {priority: 'event'});",
                     style = paste0("
                       background: ", waskita_colors$bg_primary, ";
                       border-radius: 20px;
                       padding: 32px 28px;
                       margin-bottom: 25px;
                       box-shadow: 0 8px 25px rgba(0,0,0,0.08);
                       border: 1px solid ", waskita_colors$border_light, ";
                       transition: all 0.4s ease;
                       cursor: pointer;
                       text-align: center;
                     "),
                     div(style = paste0("
                       width: 80px;
                       height: 80px;
                       background: linear-gradient(135deg, ", waskita_colors$primary, ", ", waskita_colors$accent, ");
                       border-radius: 20px;
                       display: flex;
                       align-items: center;
                       justify-content: center;
                       margin: 0 auto 24px auto;
                       transition: all 0.4s ease;
                     "),
                         tags$i(class = "fas fa-database", style = paste0("color: ", waskita_colors$bg_primary, "; font-size: 32px;"))
                     ),
                     h4("Data Management", style = paste0("font-size: 1.5rem; font-weight: 700; color: ", waskita_colors$primary, "; margin-bottom: 14px;")),
                     p("Advanced data preprocessing, transformation, and validation with professional-grade quality controls", 
                       style = paste0("color: ", waskita_colors$text_secondary, "; font-size: 1.1rem; line-height: 1.6; margin: 0;"))
                 )
          ),
          column(4,
                 div(class = "professional-card", onclick = "Shiny.setInputValue('sidebar_menu', 'descriptive', {priority: 'event'});",
                     style = paste0("
                       background: ", waskita_colors$bg_primary, ";
                       border-radius: 20px;
                       padding: 32px 28px;
                       margin-bottom: 25px;
                       box-shadow: 0 8px 25px rgba(0,0,0,0.08);
                       border: 1px solid ", waskita_colors$border_light, ";
                       transition: all 0.4s ease;
                       cursor: pointer;
                       text-align: center;
                     "),
                     div(style = paste0("
                       width: 80px;
                       height: 80px;
                       background: linear-gradient(135deg, ", waskita_colors$primary, ", ", waskita_colors$accent, ");
                       border-radius: 20px;
                       display: flex;
                       align-items: center;
                       justify-content: center;
                       margin: 0 auto 24px auto;
                       transition: all 0.4s ease;
                     "),
                         tags$i(class = "fas fa-chart-area", style = paste0("color: ", waskita_colors$bg_primary, "; font-size: 32px;"))
                     ),
                     h4("Exploratory Analysis", style = paste0("font-size: 1.5rem; font-weight: 700; color: ", waskita_colors$primary, "; margin-bottom: 14px;")),
                     p("Comprehensive descriptive statistics and advanced visualization techniques for data exploration", 
                       style = paste0("color: ", waskita_colors$text_secondary, "; font-size: 1.1rem; line-height: 1.6; margin: 0;"))
                 )
          ),
          column(4,
                 div(class = "professional-card", onclick = "Shiny.setInputValue('sidebar_menu', 'statistical_testing', {priority: 'event'});",
                     style = paste0("
                       background: ", waskita_colors$bg_primary, ";
                       border-radius: 20px;
                       padding: 32px 28px;
                       margin-bottom: 25px;
                       box-shadow: 0 8px 25px rgba(0,0,0,0.08);
                       border: 1px solid ", waskita_colors$border_light, ";
                       transition: all 0.4s ease;
                       cursor: pointer;
                       text-align: center;
                     "),
                     div(style = paste0("
                       width: 80px;
                       height: 80px;
                       background: linear-gradient(135deg, ", waskita_colors$primary, ", ", waskita_colors$accent, ");
                       border-radius: 20px;
                       display: flex;
                       align-items: center;
                       justify-content: center;
                       margin: 0 auto 24px auto;
                       transition: all 0.4s ease;
                     "),
                         tags$i(class = "fas fa-microscope", style = paste0("color: ", waskita_colors$bg_primary, "; font-size: 32px;"))
                     ),
                     h4("Statistical Testing", style = paste0("font-size: 1.5rem; font-weight: 700; color: ", waskita_colors$primary, "; margin-bottom: 14px;")),
                     p("Rigorous assumption testing including normality, homogeneity, and independence assessments", 
                       style = paste0("color: ", waskita_colors$text_secondary, "; font-size: 1.1rem; line-height: 1.6; margin: 0;"))
                 )
          )
        ),
        
        # Dataset Overview Section
        div(style = "text-align: center; margin: 60px 0 40px 0;",
            h2(style = paste0("color: ", waskita_colors$primary, "; font-weight: 700; font-size: 2.5rem; margin-bottom: 15px;"),
               tags$i(class = "fas fa-database", style = paste0("margin-right: 18px; color: ", waskita_colors$accent, ";")),
               "Dataset Overview"),
            p(style = paste0("color: ", waskita_colors$text_secondary, "; font-size: 1.2rem; max-width: 800px; margin: 0 auto;"),
              "Comprehensive Social Vulnerability Index dataset with 17 indicators across 511 Indonesian districts")
        ),
        
        fluidRow(
          column(4,
                 div(style = paste0("
                   background: ", waskita_colors$bg_primary, ";
                   border-radius: 20px;
                   padding: 35px 30px;
                   margin-bottom: 25px;
                   box-shadow: 0 8px 25px rgba(0,0,0,0.06);
                   border: 1px solid ", waskita_colors$border_light, ";
                   transition: all 0.3s ease;
                   text-align: center;
                 "),
                     div(style = paste0("
                       width: 70px;
                       height: 70px;
                       background: linear-gradient(135deg, ", waskita_colors$primary, ", ", waskita_colors$accent, ");
                       border-radius: 18px;
                       display: flex;
                       align-items: center;
                       justify-content: center;
                       margin: 0 auto 20px auto;
                     "),
                         tags$i(class = "fas fa-table", style = paste0("color: ", waskita_colors$bg_primary, "; font-size: 28px;"))
                     ),
                     h3("511", style = paste0("font-size: 3rem; font-weight: 800; margin: 15px 0; color: ", waskita_colors$primary, ";")),
                     h5("Districts", style = paste0("font-size: 1.2rem; font-weight: 600; color: ", waskita_colors$text_primary, "; margin-bottom: 10px; text-transform: uppercase; letter-spacing: 0.5px;")),
                     p("Indonesian administrative units", style = paste0("font-size: 1rem; color: ", waskita_colors$text_secondary, "; margin: 0;"))
                 )
          ),
          column(4,
                 div(style = paste0("
                   background: ", waskita_colors$bg_primary, ";
                   border-radius: 20px;
                   padding: 35px 30px;
                   margin-bottom: 25px;
                   box-shadow: 0 8px 25px rgba(0,0,0,0.06);
                   border: 1px solid ", waskita_colors$border_light, ";
                   transition: all 0.3s ease;
                   text-align: center;
                 "),
                     div(style = paste0("
                       width: 70px;
                       height: 70px;
                       background: linear-gradient(135deg, ", waskita_colors$success, ", #047857);
                       border-radius: 18px;
                       display: flex;
                       align-items: center;
                       justify-content: center;
                       margin: 0 auto 20px auto;
                     "),
                         tags$i(class = "fas fa-list-ol", style = paste0("color: ", waskita_colors$bg_primary, "; font-size: 28px;"))
                     ),
                     h3("17", style = paste0("font-size: 3rem; font-weight: 800; margin: 15px 0; color: ", waskita_colors$primary, ";")),
                     h5("SoVI Indicators", style = paste0("font-size: 1.2rem; font-weight: 600; color: ", waskita_colors$text_primary, "; margin-bottom: 10px; text-transform: uppercase; letter-spacing: 0.5px;")),
                     p("Vulnerability measurement variables", style = paste0("font-size: 1rem; color: ", waskita_colors$text_secondary, "; margin: 0;"))
                 )
          ),
          column(4,
                 div(style = paste0("
                   background: ", waskita_colors$bg_primary, ";
                   border-radius: 20px;
                   padding: 35px 30px;
                   margin-bottom: 25px;
                   box-shadow: 0 8px 25px rgba(0,0,0,0.06);
                   border: 1px solid ", waskita_colors$border_light, ";
                   transition: all 0.3s ease;
                   text-align: center;
                 "),
                     div(style = paste0("
                       width: 70px;
                       height: 70px;
                       background: linear-gradient(135deg, ", waskita_colors$warning, ", #b45309);
                       border-radius: 18px;
                       display: flex;
                       align-items: center;
                       justify-content: center;
                       margin: 0 auto 20px auto;
                     "),
                         tags$i(class = "fas fa-map-marked-alt", style = paste0("color: ", waskita_colors$bg_primary, "; font-size: 28px;"))
                     ),
                     h3("34", style = paste0("font-size: 3rem; font-weight: 800; margin: 15px 0; color: ", waskita_colors$primary, ";")),
                     h5("Provinces", style = paste0("font-size: 1.2rem; font-weight: 600; color: ", waskita_colors$text_primary, "; margin-bottom: 10px; text-transform: uppercase; letter-spacing: 0.5px;")),
                     p("Complete Indonesia coverage", style = paste0("font-size: 1rem; color: ", waskita_colors$text_secondary, "; margin: 0;"))
                 )
          )
        ),
        
        # Metadata Table
        box(
          title = tagList(icon("table"), "Variable Metadata & Documentation"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          DT::dataTableOutput("metadata_table")
        )
      ),
      
      # Data Management Tab
      tabItem(
        tabName = "data_management",
        
        box(
          title = tagList(icon("database"), "Professional Data Management Suite"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          fluidRow(
            column(4,
                   div(style = "background: #f8fafc; padding: 25px; border-radius: 12px; margin-bottom: 20px;",
                       h4("1. Data Loading", style = "margin-bottom: 20px; color: #1B3C53;"),
                       actionButton("load_data", "Load Dataset", 
                                    class = "btn btn-primary", 
                                    style = "width: 100%; margin-bottom: 20px;"),
                       
                       conditionalPanel(
                         condition = "output.data_loaded == true",
                         div(style = "background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%); border: 1px solid #0284c7; border-left: 4px solid #2563eb; padding: 25px; margin-top: 25px; border-radius: 12px; box-shadow: 0 4px 15px rgba(2, 132, 199, 0.1);",
                             h5("Dataset Information", style = "color: #1B3C53; font-weight: 700; margin-bottom: 15px; font-size: 18px;"),
                             verbatimTextOutput("data_summary")
                         )
                       )
                   )
            ),
            
            column(4,
                   div(style = "background: #f8fafc; padding: 25px; border-radius: 12px; margin-bottom: 20px;",
                       h4("2. Data Transformation", style = "margin-bottom: 20px; color: #1B3C53;"),
                       
                       conditionalPanel(
                         condition = "output.data_loaded == true",
                         selectInput("transform_variable", "Select Variable:",
                                     choices = NULL, width = "100%"),
                         
                         selectInput("transform_method", "Transformation Method:",
                                     choices = list(
                                       "Natural Log" = "log",
                                       "Log Base 10" = "log10",
                                       "Square Root" = "sqrt",
                                       "Square" = "square",
                                       "Box-Cox" = "boxcox",
                                       "Z-Score Standardization" = "zscore"
                                     ), width = "100%"),
                         
                         actionButton("apply_transformation", "Apply Transformation",
                                      class = "btn btn-success",
                                      style = "width: 100%; margin-bottom: 20px;"),
                         
                         conditionalPanel(
                           condition = "output.transformation_done == true",
                           div(style = "background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%); border: 1px solid #0284c7; border-left: 4px solid #2563eb; padding: 25px; margin-top: 25px; border-radius: 12px; box-shadow: 0 4px 15px rgba(2, 132, 199, 0.1);",
                               h5("Transformation Results", style = "color: #1B3C53; font-weight: 700; margin-bottom: 15px; font-size: 18px;"),
                               verbatimTextOutput("transformation_result")
                           )
                         )
                       )
                   )
            ),
            
            column(4,
                   div(style = "background: #f8fafc; padding: 25px; border-radius: 12px; margin-bottom: 20px;",
                       h4("3. Data Categorization", style = "margin-bottom: 20px; color: #1B3C53;"),
                       
                       conditionalPanel(
                         condition = "output.data_loaded == true",
                         selectInput("categorize_variable", "Select Variable:",
                                     choices = NULL, width = "100%"),
                         
                         selectInput("categorize_method", "Categorization Method:",
                                     choices = list(
                                       "Quartiles (4 groups)" = "quartile",
                                       "Tertiles (3 groups)" = "tertile",
                                       "Quintiles (5 groups)" = "quintile",
                                       "Custom Thresholds" = "custom"
                                     ), width = "100%"),
                         
                         conditionalPanel(
                           condition = "input.categorize_method == 'custom'",
                           textInput("custom_thresholds", "Custom Thresholds (comma-separated):",
                                     value = "25,50,75", width = "100%")
                         ),
                         
                         actionButton("apply_categorization", "Create Categories",
                                      class = "btn btn-success",
                                      style = "width: 100%; margin-bottom: 20px;"),
                         
                         conditionalPanel(
                           condition = "output.categorization_done == true",
                           div(style = "background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%); border: 1px solid #0284c7; border-left: 4px solid #2563eb; padding: 25px; margin-top: 25px; border-radius: 12px; box-shadow: 0 4px 15px rgba(2, 132, 199, 0.1);",
                               h5("Categorization Results", style = "color: #1B3C53; font-weight: 700; margin-bottom: 15px; font-size: 18px;"),
                               verbatimTextOutput("categorization_result")
                           )
                         )
                       )
                   )
            )
          ),
          
          conditionalPanel(
            condition = "output.data_loaded == true",
            hr(),
            h4("Data Preview & Export", style = "color: #1B3C53; margin-bottom: 20px;"),
            withSpinner(DT::dataTableOutput("data_preview")),
            
            div(style = "background: linear-gradient(135deg, #f1f5f9 0%, #ffffff 100%); padding: 25px; border-radius: 16px; margin-top: 30px; border: 1px solid #e2e8f0; box-shadow: 0 4px 15px rgba(0,0,0,0.04);",
                h5("Export Options", style = "color: #1B3C53; font-weight: 700; margin-bottom: 20px; font-size: 18px;"),
                fluidRow(
                  column(6,
                         downloadButton("download_original", "Download Original Data (CSV)",
                                        class = "btn btn-success", style = "width: 100%;")
                  ),
                  column(6,
                         downloadButton("download_processed", "Download Processed Data (CSV)",
                                        class = "btn btn-success", style = "width: 100%;")
                  )
                )
            )
          )
        )
      ),
      
      # Descriptive Analysis Tab
      tabItem(
        tabName = "descriptive",
        
        box(
          title = tagList(icon("chart-bar"), "Advanced Descriptive Statistics"),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          conditionalPanel(
            condition = "output.data_loaded == true",
            
            fluidRow(
              column(4,
                     div(style = "background: #f8fafc; padding: 25px; border-radius: 12px;",
                         h4("Analysis Configuration", style = "margin-bottom: 20px; color: #1B3C53;"),
                         
                         selectInput("desc_variable", "Select Variable:",
                                     choices = NULL, width = "100%"),
                         
                         conditionalPanel(
                           condition = "output.is_numeric_var == true",
                           selectInput("desc_chart_type", "Visualization Type:",
                                       choices = list(
                                         "Histogram" = "histogram",
                                         "Box Plot" = "boxplot", 
                                         "Density Plot" = "density",
                                         "Q-Q Plot" = "qq",
                                         "Violin Plot" = "violin"
                                       ), width = "100%"),
                           
                           conditionalPanel(
                             condition = "input.desc_chart_type == 'histogram'",
                             numericInput("desc_bins", "Number of Bins:",
                                          value = 30, min = 10, max = 100, width = "100%")
                           )
                         ),
                         
                         conditionalPanel(
                           condition = "output.is_numeric_var == false",
                           selectInput("desc_chart_type", "Visualization Type:",
                                       choices = list(
                                         "Bar Chart" = "barchart",
                                         "Pie Chart" = "piechart"
                                       ), width = "100%")
                         ),
                         
                         actionButton("generate_desc", "Generate Analysis",
                                      class = "btn btn-primary", style = "width: 100%;")
                     )
              ),
              
              column(8,
                     conditionalPanel(
                       condition = "output.desc_generated == true",
                       withSpinner(plotlyOutput("desc_plot", height = "500px"))
                     )
              )
            ),
            
            conditionalPanel(
              condition = "output.desc_generated == true",
              hr(),
              fluidRow(
                column(6,
                       div(style = "background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%); border: 1px solid #0284c7; border-left: 4px solid #2563eb; padding: 25px; margin-top: 25px; border-radius: 12px; box-shadow: 0 4px 15px rgba(2, 132, 199, 0.1);",
                           h5("Statistical Summary", style = "color: #1B3C53; font-weight: 700; margin-bottom: 15px; font-size: 18px;"),
                           verbatimTextOutput("desc_stats")
                       )
                ),
                column(6,
                       div(style = "background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%); border: 1px solid #0284c7; border-left: 4px solid #2563eb; padding: 25px; margin-top: 25px; border-radius: 12px; box-shadow: 0 4px 15px rgba(2, 132, 199, 0.1);",
                           h5("Professional Interpretation", style = "color: #1B3C53; font-weight: 700; margin-bottom: 15px; font-size: 18px;"),
                           verbatimTextOutput("desc_interpretation")
                       )
                )
              ),
              
              div(style = "background: linear-gradient(135deg, #f1f5f9 0%, #ffffff 100%); padding: 25px; border-radius: 16px; margin-top: 30px; border: 1px solid #e2e8f0; box-shadow: 0 4px 15px rgba(0,0,0,0.04);",
                  h5("Export Analysis", style = "color: #1B3C53; font-weight: 700; margin-bottom: 20px; font-size: 18px;"),
                  fluidRow(
                    column(4,
                           downloadButton("download_desc_plot", "Download Plot (PNG)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_desc_stats", "Download Statistics (CSV)",
                                          class = "btn btn-success", style = "width: 100%;")
                    ),
                    column(4,
                           downloadButton("download_desc_report", "Download Report (PDF)",
                                          class = "btn btn-warning", style = "width: 100%;")
                    )
                  )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.data_loaded == false",
            div(style = "text-align: center; padding: 80px 30px;",
                tags$i(class = "fas fa-database", style = "font-size: 4rem; margin-bottom: 20px; opacity: 0.5; color: #64748b;"),
                h4("Please load data first from the Data Management section", 
                   style = "color: #64748b; font-weight: 500; font-size: 20px; margin: 0;")
            )
          )
        )
      )
      
    )
  )
)
