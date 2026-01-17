#' The application User-Interface
#' @import shiny
#' @import bs4Dash
#' @export
app_ui <- function(request) {
  # Custom CSS (Original Lines 31-39)
  custom_theme <- "
    .main-header .logo { background-color: #006400 !important; color: #FFD700 !important; font-weight: bold; }
    .main-header .navbar { background-color: #006400 !important; }
    .sidebar-dark-primary { background-color: #004d00 !important; }
    .btn-success { background-color: #006400 !important; border-color: #FFD700 !important; color: #FFD700 !important; }
    .btn-info { background-color: #004d00 !important; border-color: #FFD700 !important; }
    .nav-pills .nav-link.active, .nav-pills .show > .nav-link { background-color: #006400 !important; color: #FFD700 !important; }
  "

  tagList(
    tags$head(tags$style(HTML(custom_theme))),
    dashboardPage(
      header = dashboardHeader(title = "HawaSpatial Pro"),
      sidebar = dashboardSidebar(
        sidebarMenu(
          menuItem("1. Introduction", tabName = "intro", icon = icon("info-circle")),
          menuItem("2. Data Setup", tabName = "setup", icon = icon("database")),
          menuItem("3. 16 Spatial Methods", tabName = "esda", icon = icon("map-marked-alt")),
          menuItem("4. Bivariate Spatial", tabName = "bivariate", icon = icon("layer-group")),
          menuItem("5. Advanced Spatial", tabName = "advanced", icon = icon("magic")),
          menuItem("6. Bayesian Multilevel", tabName = "bayes_ml", icon = icon("brain")),
          menuItem("7. Bayesian Spatial", tabName = "bayes_spatial", icon = icon("map-marker-alt")),
          menuItem("8. Reporting", tabName = "reporting", icon = icon("file-export"))
        )
      ),
      body = dashboardBody(
        tags$head(tags$style(HTML(custom_theme))),
        tabItems(
          tabItem(tabName = "intro",
                  jumbotron(
                    title = "HAWA Spatial Intelligence",
                    lead = "Health & Areal Weighted Analysis (HAWA) for Global Development",
                    status = "success",
                    "HawaSpatial Pro is a professional-grade, multi-sectoral platform designed to bridge the gap
                between complex Bayesian spatial modeling and actionable policy insights. By integrating
                advanced computational statistics with geographic information systems (GIS), it provides
                a robust framework for sub-national monitoring of development indicators."
                  ),
                  fluidRow(
                    box(title = "Universal SDG Alignment", width = 12, status = "success", solidHeader = TRUE,
                        p("HawaSpatial Pro is engineered to support the monitoring and achievement of all 17 United Nations Sustainable Development Goals (SDGs).
                      By identifying geographic 'Hot Spots' and 'Cold Spots', the platform enables precision targeting for:"),
                        column(4,
                               tags$ul(
                                 tags$li(strong("SDG 1-6:"), " Poverty, Hunger, Health, Education, Gender, & Water"),
                                 tags$li(strong("SDG 7-9:"), " Energy, Decent Work, & Infrastructure")
                               )),
                        column(4,
                               tags$ul(
                                 tags$li(strong("SDG 10-12:"), " Reduced Inequalities, Sustainable Cities, & Consumption"),
                                 tags$li(strong("SDG 13-15:"), " Climate Action, Life Below Water, & Life on Land")
                               )),
                        column(4,
                               tags$ul(
                                 tags$li(strong("SDG 16-17:"), " Peace, Justice, & Global Partnerships for the Goals")
                               )),
                        p(em("The platform's ability to handle Binary, Count, Continuous, Ordinal, Multinomial, and Fractional outcomes makes it universally applicable to any SDG indicator."))
                    )
                  ),
                  fluidRow(
                    box(title = "Project Overview & Data Sources", width = 6, status = "primary", solidHeader = TRUE,
                        p("The platform is optimized for large-scale, complex household surveys including:"),
                        tags$ul(
                          tags$li(strong("DHS:"), " Demographic and Health Surveys (USAID)"),
                          tags$li(strong("MICS:"), " Multiple Indicator Cluster Surveys (UNICEF)"),
                          tags$li(strong("IHBS:"), " Integrated Household Budget Surveys (World Bank/National Agencies)"),
                          tags$li(strong("SPA/MIS:"), " Service Provision Assessment & Malaria Indicator Surveys")
                        ),
                        p("It features a built-in 'Spatial Equalizer' to resolve naming inconsistencies between survey datasets and administrative shapefiles (Admin1/Admin2).")
                    ),
                    box(title = "Technical Engine & R Packages", width = 6, status = "info", solidHeader = TRUE,
                        p("HawaSpatial Pro leverages a sophisticated ecosystem of R libraries:"),
                        tags$ul(
                          tags$li(strong("INLA:"), " Integrated Nested Laplace Approximations for fast Bayesian inference."),
                          tags$li(strong("sf & spdep:"), " Simple Features and Spatial Dependence for geometry processing and weight matrices."),
                          tags$li(strong("biscale & viridis:"), " Advanced bivariate mapping and color-blind friendly visualizations."),
                          tags$li(strong("gstat & spatialreg:"), " Geostatistical interpolation (Kriging) and Spatial Autoregressive models."),
                          tags$li(strong("shiny & bs4Dash:"), " High-performance reactive UI framework for real-time analysis.")
                        )
                    )
                  )
          ),
          tabItem(tabName = "setup",
                  fluidRow(
                    box(title = "1. Data Ingestion", width = 4, status = "primary",
                        fileInput("data_file", "Upload Survey Data (.dta, .sav, .csv)"),
                        fileInput("shp_file", "Upload Shapefile Components", multiple = TRUE),
                        selectInput("outcome_type", "Outcome Type:",
                                    choices = c("Binary", "Count", "Continuous", "Ordinal", "Multinomial", "Fractional")),
                        checkboxInput("is_dhs_wgt", "Apply DHS Weight (v005/1,000,000)", TRUE),
                        actionButton("process_data", "Initialize & Equalize Names", class = "btn-success btn-block")
                    ),
                    box(title = "2. Core Mapping Variables", width = 8, status = "primary",
                        fluidRow(
                          column(6, selectInput("v_dv", "Outcome (dv):", choices = NULL),
                                 selectInput("v_reg", "Region Variable:", choices = NULL)),
                          column(6, selectInput("v_cluster", "Cluster ID (V001):", choices = NULL),
                                 selectInput("v_wgt", "Weight (V005):", choices = NULL))
                        ),
                        hr(),
                        fluidRow(
                          column(6, selectInput("v_indiv_cont", "Individual Continuous (L1):", choices = NULL, multiple = TRUE),
                                 selectInput("v_indiv_fact", "Individual Factors (L1):", choices = NULL, multiple = TRUE)),
                          column(6, selectInput("v_comm_cont", "Community Continuous (L2):", choices = NULL, multiple = TRUE),
                                 selectInput("v_comm_fact", "Community Factors (L2):", choices = NULL, multiple = TRUE))
                        ),
                        hr(),
                        selectInput("v_wealth", "Bivariate X Variable (e.g. Wealth):", choices = NULL),
                        uiOutput("sparsity_alert"))
                  )
          ),

          tabItem(tabName = "esda",
                  fluidRow(
                    box(title = "ESDA Diagnostic Selector", width = 3, status = "success",
                        selectInput("esda_method", "Select Method:",
                                    choices = c("1. Connectivity Graph", "2. Basic Choropleth", "3. EBS Smoothing",
                                                "4. Quantile Map", "5. Global Moran's I", "6. Moran Scatterplot",
                                                "7. LISA Cluster Map", "8. Getis-Ord Gi*", "9. Join Count Analysis",
                                                "10. Hinge Map", "11. Spatial Correlogram", "12. Regime Density",
                                                "13. Spatial Lag Map", "14. SDG Target Gap", "15. Local Geary's C",
                                                "16. Conditional Plot"))
                    ),
                    box(title = "Spatial Diagnostic Output", width = 9, plotOutput("diag_plot", height = "600px"))
                  )
          ),

          tabItem(tabName = "bivariate",
                  fluidRow(
                    box(title = "Bivariate Controls", width = 4, status = "warning",
                        actionButton("run_bi", "Generate Bivariate Map", class = "btn-success")
                    ),
                    box(title = "Bivariate Spatial Relationship", width = 8, plotOutput("bi_plot", height = "600px"))
                  )
          ),

          tabItem(tabName = "advanced",
                  fluidRow(
                    box(title = "Ordinary Kriging (Surface Interpolation)", width = 6, plotOutput("krig_plot")),
                    box(title = "Spatial Lag Model Residuals", width = 6, plotOutput("resid_plot"))
                  )
          ),

          tabItem(tabName = "bayes_ml",
                  fluidRow(
                    box(width = 3, status = "info",
                        actionButton("run_bayes", "Run 4-Stage INLA Model", class = "btn-info btn-block"),
                        hr(),
                        helpText("Stage 0: Null Model"),
                        helpText("Stage 1: Individual Level"),
                        helpText("Stage 2: Community Level"),
                        helpText("Stage 3: Full Model")
                    ),
                    box(title = "Posterior Means (Full Model Fixed Effects)", width = 9, plotOutput("bayes_plot"))
                  ),
                  fluidRow(
                    box(title = "Bayesian Model Comparison (DIC, WAIC, MLIK, ICC)", width = 12, plotOutput("bayes_metrics_plot"))
                  ),
                  fluidRow(
                    box(title = "Full Model Summary Table", width = 12, tableOutput("bayes_table"))
                  )
          ),
          tabItem(tabName = "bayes_spatial",
                  fluidRow(
                    box(title = "Unexplained Spatial Variation (Random Effects)", width = 12, status = "info",
                        helpText("This map shows the cluster-level random effects aggregated to the regional level.
                       Positive values (red) indicate areas where the outcome is higher than predicted by your variables,
                       while negative values (blue) indicate areas where it is lower."),
                        plotOutput("bayes_map", height = "700px"))
                  )
          ),
          tabItem(tabName = "reporting",
                  fluidRow(
                    box(title = "Export Publication Report", width = 6, status = "success",
                        textInput("rep_title", "Report Title:", "HAWA Spatial Analysis"),
                        radioButtons("format", "Format:", choices = c("Word" = "docx", "PDF" = "pdf", "HTML" = "html")),
                        downloadButton("dl_report", "Download Full Report")
                    )
                  )
          )
        )
      )
    )
  )
}
