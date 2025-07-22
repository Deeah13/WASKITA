# WASKITA Professional Dashboard

**Wawasan Spasial Kerentanan Interaktif & Terpadu Analitik**  
*Professional Statistical Analysis Platform*

## 🎯 Overview

WASKITA is a comprehensive, professional-grade statistical analysis dashboard built with R Shiny for Social Vulnerability Index (SoVI) research. Designed for professional statisticians, researchers, and data scientists, it provides advanced analytical capabilities with publication-ready outputs.

## ✨ Key Features

### 📊 **Professional Interface**
- Modern, responsive design with professional color scheme
- Interactive dashboard with smooth animations
- Professional typography and spacing
- Mobile-friendly responsive layout

### 🔧 **Data Management Suite**
- **Smart Data Loading**: Automatic local/online data source detection
- **Advanced Transformations**: Log, Box-Cox, Z-score standardization
- **Intelligent Categorization**: Quartiles, tertiles, quintiles, custom thresholds
- **Data Validation**: Comprehensive quality checks and validation

### 📈 **Descriptive Analytics**
- **Advanced Statistics**: Mean, median, skewness, kurtosis, CV
- **Professional Visualizations**: Histogram, boxplot, density, Q-Q, violin plots
- **Intelligent Interpretation**: Automated statistical interpretation
- **Quality Assessment**: Missing data analysis, outlier detection

### 🧪 **Statistical Testing Suite**
- **Assumption Testing**: Normality (Shapiro-Wilk, KS, Anderson-Darling)
- **Homogeneity Tests**: Levene, Bartlett, Fligner-Killeen
- **Inferential Statistics**: T-tests, ANOVA, Chi-square tests
- **Effect Size Calculations**: Cohen's d, eta-squared, power analysis

### 📊 **Advanced Analytics**
- **Regression Analysis**: Multiple linear regression with diagnostics
- **Spatial Analysis**: Moran's I, spatial autocorrelation
- **Interactive Visualizations**: Plotly-powered interactive charts
- **Professional Reports**: PDF export capabilities

## 🛠 Technical Architecture

### **File Structure**
```
├── app.R              # Main application launcher
├── global.R           # Global configuration and utilities
├── ui.R              # Professional user interface
├── server.R          # Enhanced server logic
└── README.md         # Documentation
```

### **Core Components**

#### **global.R**
- **Enhanced Package Management**: Automatic installation and loading
- **Professional Color Palette**: Consistent branding throughout
- **Utility Functions**: Statistical interpretations and formatting
- **Data Loading Functions**: Robust data handling with validation
- **Spatial Analysis Tools**: Moran's I and weights matrix functions

#### **ui.R**
- **Professional CSS**: Custom styling with modern design principles
- **Responsive Layout**: Mobile-first design approach
- **Interactive Components**: Enhanced user interface elements
- **Modular Structure**: Organized tab-based navigation

#### **server.R**
- **Reactive System**: Efficient state management
- **Error Handling**: Comprehensive try-catch mechanisms
- **Professional Notifications**: User-friendly feedback system
- **Advanced Analytics**: Statistical computations and interpretations

## 📊 Dataset Information

### **Social Vulnerability Index (SoVI) Dataset**
- **Observations**: 511 Indonesian districts
- **Variables**: 17 vulnerability indicators
- **Coverage**: All 34 provinces of Indonesia
- **Source**: Scientific publication (Data in Brief, Elsevier)

### **Variables Included**
| Variable | Description | Type |
|----------|-------------|------|
| DISTRICTCODE | District identifier | Categorical |
| CHILDREN | Proportion under 5 years | Numeric (0-1) |
| FEMALE | Female population proportion | Numeric (0-1) |
| ELDERLY | Proportion 65+ years | Numeric (0-1) |
| FHEAD | Female household heads | Numeric (0-1) |
| FAMILYSIZE | Average household size | Numeric |
| LOWEDU | Low education proportion | Numeric (0-1) |
| POVERTY | Poverty rate | Numeric (0-1) |
| ILLITERATE | Illiteracy rate | Numeric (0-1) |
| NOTRAINING | No vocational training | Numeric (0-1) |
| GROWTH | Population growth rate | Numeric |
| NOELECTRIC | No electricity access | Numeric (0-1) |
| RENTED | Rented housing proportion | Numeric (0-1) |
| NOSEWER | No sewerage system | Numeric (0-1) |
| TAPWATER | Clean water access | Numeric (0-1) |
| DPHONE | Disaster risk index | Numeric (0-100) |
| SOVI | Social Vulnerability Index | Numeric |

## 🚀 Getting Started

### **Prerequisites**
```r
# Required R packages (auto-installed by dashboard)
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinycssloaders", "shinyjs",
  "DT", "dplyr", "readr", "tidyr", "stringr", "data.table",
  "plotly", "ggplot2", "leaflet", "RColorBrewer", "viridis", "corrplot",
  "nortest", "car", "lmtest", "moments", "psych", "broom", "effectsize",
  "knitr", "rmarkdown", "htmltools", "scales", "MASS", "forecast"
)
```

### **Quick Start**
1. **Clone/Download** the dashboard files
2. **Set working directory** to the dashboard folder
3. **Run the application**:
   ```r
   shiny::runApp()
   ```
   or
   ```r
   source("app.R")
   ```

### **Data Setup**
The dashboard supports both local and online data sources:

- **Local**: Place CSV files in `D:/Perkuliahan Tingkat 2 Semester 4/WASKITA2/data/`
- **Online**: Automatic fallback to GitHub repository
- **Format**: CSV files with proper column headers

## 📱 User Interface

### **Navigation Structure**
1. **Dashboard** - Overview and dataset information
2. **Data Management** - Loading, transformation, categorization
3. **Descriptive Analysis** - Statistical summaries and visualizations
4. **Data Visualization** - Advanced plotting capabilities
5. **Statistical Testing** - Assumption validation
6. **T-Tests** - Hypothesis testing
7. **ANOVA** - Analysis of variance
8. **Regression Analysis** - Multiple regression modeling
9. **Spatial Analysis** - Geographic and autocorrelation analysis

### **Key Interface Features**
- **Professional Cards**: Interactive navigation elements
- **Real-time Feedback**: Progress indicators and notifications
- **Responsive Design**: Adapts to different screen sizes
- **Export Options**: Download results in multiple formats
- **Interactive Plots**: Plotly-powered visualizations

## 🔬 Statistical Capabilities

### **Descriptive Statistics**
- Central tendency measures (mean, median, mode)
- Variability measures (SD, variance, CV, IQR)
- Distribution shape (skewness, kurtosis)
- Outlier detection and analysis
- Missing data assessment

### **Statistical Tests**
- **Normality Tests**: Shapiro-Wilk, Kolmogorov-Smirnov, Anderson-Darling
- **Homogeneity Tests**: Levene's test, Bartlett's test, Fligner-Killeen
- **T-Tests**: One-sample, independent, paired
- **ANOVA**: One-way, two-way with interactions
- **Effect Sizes**: Cohen's d, eta-squared, power calculations

### **Advanced Analytics**
- **Regression Analysis**: Multiple linear regression
- **Model Diagnostics**: Residual analysis, VIF, Cook's distance
- **Spatial Analysis**: Moran's I, spatial weights matrices
- **Data Transformations**: Log, Box-Cox, standardization

## 📊 Professional Features

### **Quality Assurance**
- Comprehensive error handling
- Input validation and sanitization
- Professional statistical interpretations
- Automated quality checks

### **Export Capabilities**
- **Plots**: High-resolution PNG exports
- **Data**: CSV format with processed variables
- **Reports**: Professional PDF reports
- **Statistics**: Formatted statistical summaries

### **Professional Interpretations**
- Automated statistical interpretation
- Effect size classifications
- Practical significance assessment
- Recommendations for further analysis

## 🎨 Design Philosophy

### **Professional Standards**
- Clean, modern interface design
- Consistent color scheme and typography
- Intuitive navigation and workflow
- Professional statistical reporting

### **User Experience**
- Minimal learning curve
- Clear visual feedback
- Progressive disclosure of complexity
- Mobile-responsive design

### **Technical Excellence**
- Robust error handling
- Efficient reactive programming
- Scalable code architecture
- Professional documentation

## 📚 Academic Context

**Course**: Computational Statistics  
**Institution**: STIS Polytechnic  
**Program**: D-IV Computational Statistics  
**Semester**: Even Semester 2024/2025  
**Assessment**: Final Exam Project  

### **Learning Objectives**
- Advanced R Shiny development
- Professional statistical analysis
- Interactive data visualization
- Spatial data analysis
- Publication-ready reporting

## 🤝 Support & Documentation

### **Getting Help**
- Check the built-in help tooltips
- Review the professional interpretations
- Consult the metadata documentation
- Reference the statistical methodology

### **Best Practices**
1. Always load and validate data first
2. Check assumptions before statistical tests
3. Review interpretations carefully
4. Export results for reproducibility
5. Document your analysis workflow

## 📄 License & Citation

This dashboard is developed for academic purposes as part of the Computational Statistics course. The underlying SoVI dataset is from published scientific research.

**Data Source**: Social Vulnerability Index dataset  
**Publisher**: Elsevier - Data in Brief  
**DOI**: 10.1016/j.dib.2021.107618

---

**WASKITA Professional Dashboard** - *Advanced Statistical Analysis Made Professional*

*Developed with ❤️ for the statistical community*
