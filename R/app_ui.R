app_ui <- function(request) {
  tagList(
    # Leave the golem_add_external_resources() call here
    golem_add_external_resources(),

    # PASTE YOUR dashboardPage HERE:
    bs4Dash::dashboardPage(
      dark = NULL,
      header = dashboardHeader(title = "HawaSpatial Pro"),
      sidebar = dashboardSidebar(
        sidebarMenu(
          menuItem("1. Introduction", tabName = "intro", icon = icon("info-circle")),
          menuItem("2. Data Setup & Equalizer", tabName = "setup", icon = icon("database")),
          menuItem("3. 17 Spatial Methods", tabName = "esda", icon = icon("map-marked-alt")),
          menuItem("4. Bivariate Spatial", tabName = "bivariate", icon = icon("layer-group")),
          menuItem("5. Advanced Spatial", tabName = "advanced", icon = icon("magic")),
          menuItem("6. Multilevel Models", tabName = "me_ml", icon = icon("brain")),
          menuItem("7. Spatial Random Effects", tabName = "me_spatial", icon = icon("map-marker-alt")),
          menuItem("8. Small Area Estimation", tabName = "sae_tab", icon = icon("chart-area")),
          menuItem("9. Spatial Econometrics", tabName = "spatial_econ", icon = icon("calculator")),
          menuItem("10. Spatial Inequality", tabName = "inequality", icon = icon("balance-scale")),
          menuItem("11. Reporting", tabName = "reporting", icon = icon("file-export")),
          menuItem("12. Spatiotemporal Dynamics", tabName = "st_dynamics", icon = icon("history")),
          menuItem("13. Decomposition", tabName = "decomp", icon = icon("scissors"))
        )
      ),
      body = dashboardBody(
        # Add your custom theme here
        tags$head(tags$style(HTML("
          .main-header .logo { background-color: #006400 !important; color: #FFD700 !important; font-weight: bold; }
          .main-header .navbar { background-color: #006400 !important; }
          .sidebar-dark-primary { background-color: #004d00 !important; }
          .btn-success { background-color: #006400 !important; border-color: #FFD700 !important; color: #FFD700 !important; }
          .btn-info { background-color: #004d00 !important; border-color: #FFD700 !important; }
          .nav-pills .nav-link.active, .nav-pills .show > .nav-link { background-color: #006400 !important; color: #FFD700 !important; }
        "))),
        tabItems(
          tabItem(tabName = "intro",
                  jumbotron(
                    title = "HAWA Spatial Intelligence",
                    lead = "Holistic & Areal Weighted Analysis (HAWA) for Global Development",
                    status = "success",
                    "HawaSpatial Pro is a professional-grade, multi-sectoral platform designed to bridge the gap
                between complex spatial modeling and actionable policy insights. It integrates
                advanced computational statistics with Geographic Information Systems (GIS) to provide
                a robust framework for sub-national monitoring of development indicators. The platform
                supports comprehensive analysis across seven domains: **Exploratory Spatial Data Analysis**,
                **Bivariate Spatial Association**, **Spatiotemporal Dynamics**, **Small Area Estimation (SAE)**,
                **Spatial Econometrics**, **Spatial Inequality Metrics**, and **Hierarchical Multilevel Modeling**."
                  ),
                  fluidRow(
                    box(title = "Core Analytical Capabilities", width = 12, status = "primary", solidHeader = TRUE,
                        p("HawaSpatial Pro provides deep insights into geographic disparities and underlying risk factors:"),
                        column(4,
                               tags$ul(
                                 tags$li(strong("Spatial & Hotspot Analysis:"), " 17 methods including Global/Local Moran's I, LISA Cluster Maps, Getis-Ord Gi*, and Spatial Correlograms to identify clustering."),
                                 tags$li(strong("Small Area Estimation (SAE):"), " Implements Fay-Herriot area-level models to produce stabilized sub-national estimates (EBLUPs). It accounts for complex survey design effects and leverages regional covariates to reduce variance in data-sparse regions."),
                                 tags$li(strong("Spatial Econometrics:"), " Features Spatial Autoregressive (SAR) and Spatial Error Models (SEM) to quantify geographic spillover effects and neighborhood influences.")
                               )),
                        column(4,
                               tags$ul(
                                 tags$li(strong("Multilevel Modeling (GLMM):"), " Runs 4-Stage Hierarchical Models (Null, Individual, Community, Full) to partition variance between clusters and regions."),
                                 tags$li(strong("Model Diagnostics:"), " Calculates ICC (Intraclass Correlation) and VPR (Variance Partitioning Ratio) to quantify the explanatory power of fixed effects."),
                                 tags$li(strong("Spatial Inequality (SDG 10):"), " Calculates the Gini Coefficient and Theil Index with Lorenz Curve visualization to measure geographic concentration and disparity.")
                               )),
                        column(4,
                               tags$ul(
                                 tags$li(strong("Spatiotemporal Dynamics:"), " Analyzes trends across multiple survey waves, including Global Moran's I trend, Rate of Change maps, and LISA Cluster Transition analysis."),
                                 tags$li(strong("Bivariate Analysis:"), " Measures spatial covariance between the outcome and a key predictor (e.g., wealth) using Bivariate Moran's I and specialized maps."),
                                 tags$li(strong("Data Integration:"), " Features a 'Spatial Equalizer' for seamless merging of complex survey data (DHS/MICS) with administrative shapefiles.")
                               ))
                    )
                  ),
                  fluidRow(
                    box(title = "Universal SDG Alignment", width = 12, status = "success", solidHeader = TRUE,
                        p("HawaSpatial Pro is engineered to support the monitoring and achievement of all 17 United Nations Sustainable Development Goals (SDGs).
                      By identifying geographic 'Hot Spots' and 'Inequality Gaps', the platform enables precision targeting for:"),
                        column(4,
                               tags$ul(
                                 tags$li(strong("SDG 1-5:"), " Poverty, Hunger, Health, Education, & Gender Equality"),
                                 tags$li(strong("SDG 10:"), " Reduced Inequalities (Geographic Gini & Theil Indices)")
                               )),
                        column(4,
                               tags$ul(
                                 tags$li(strong("SDG 6-9:"), " Water, Energy, Decent Work, & Infrastructure"),
                                 tags$li(strong("SDG 11-15:"), " Sustainable Cities, Climate Action, & Life on Land")
                               )),
                        column(4,
                               tags$ul(
                                 tags$li(strong("SDG 16-17:"), " Peace, Justice, & Global Partnerships for the Goals"),
                                 tags$li(strong("Outcome Flexibility:"), " Supports Binary, Count, Continuous, Ordinal, Multinomial, and Fractional outcomes.")
                               ))
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
                          tags$li(strong("glmmTMB & survey:"), " used for Hierarchical Models and Design-Based Inference."),
                          tags$li(strong("sae & spatialreg:"), " used for Small Area Estimation (Fay-Herriot) and Spatial Econometrics (SAR/SEM)."),
                          tags$li(strong("ineq:"), " used for Gini and Theil inequality metrics."),
                          tags$li(strong("sf & spdep:"), " Simple Features and Spatial Dependence for geometry processing and weight matrices.")
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
                                 selectInput("v_wgt", "Weight (V005):", choices = NULL)),
                          selectInput("v_strata", "Strata Variable (V023):", choices = NULL)
                        ),
                        hr(),
                        fluidRow(
                          column(6, selectInput("v_indiv_cont", "Individual Continuous (L1):", choices = NULL, multiple = TRUE),
                                 selectInput("v_indiv_fact", "Individual Factors (L1):", choices = NULL, multiple = TRUE)),
                          column(6, selectInput("v_comm_cont", "Community Continuous (L2):", choices = NULL, multiple = TRUE),
                                 selectInput("v_comm_fact", "Community Factors (L2):", choices = NULL, multiple = TRUE))
                        ),
                        hr(),
                        # --- NEW: TIME VARIABLE INPUT (Optional) ---
                        selectInput("v_time", "Time/Wave Variable (for Spatiotemporal):", choices = NULL),
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
                                                "16. Conditional Plot", "17. Global Geary's C"))
                    ),
                    box(title = "Spatial Diagnostic Output", width = 9, plotOutput("diag_plot", height = "600px"))
                  ),
                  fluidRow(
                    box(title = "Table 1: Characteristics of Respondents (Weighted)", width = 12, status = "primary", solidHeader = TRUE,
                        helpText("Displays Weighted Frequency and % for Categorical variables, and Weighted Mean (SD) for Continuous variables."),
                        tableOutput("desc_stats_table")
                    )
                  )
                  # --------------------------------------
          ),

          tabItem(tabName = "bivariate",
                  fluidRow(
                    box(title = "Bivariate Controls", width = 4, status = "warning",
                        actionButton("run_bi", "Generate Bivariate Map", class = "btn-success"),
                        hr(),
                        h4("Bivariate Global Moran's I Test"),
                        verbatimTextOutput("bi_moran_test") # New output for Bivariate Moran
                    ),
                    box(title = "Bivariate Spatial Relationship", width = 8, plotOutput("bi_plot", height = "600px"))
                  ),

                  fluidRow(
                    box(title = "Multicollinearity Check: Covariate Correlation Matrix", width = 12, status = "primary", solidHeader = TRUE,
                        plotOutput("cov_corr_plot", height = "600px"),
                        helpText("Visualizes correlations between all selected Individual and Community level independent variables. Darker colors indicate stronger correlation."))
                  )
                  # ------------------------
          ),

          tabItem(tabName = "advanced",
                  fluidRow(
                    box(title = "Ordinary Kriging (Surface Interpolation)", width = 6, plotOutput("krig_plot")),
                    box(title = "Spatial Lag Model Residuals", width = 6, plotOutput("resid_plot"))
                  )
          ),

          tabItem(tabName = "me_ml",
                  fluidRow(
                    box(width = 3, title = "Multilevel Engine", status = "info",
                        actionButton("run_freq", "Run 4-Stage Hierarchical Model", class = "btn-info btn-block"),
                        hr(),
                        helpText("Generates comparative models M0-M3 for publication.")
                    ),
                    box(title = "Full Model: Fixed Effect Estimates", width = 9, plotOutput("freq_forest_plot"))
                  ),
                  fluidRow(
                    box(title = "Formal Publication Table: Side-by-Side Model Comparison", width = 12,
                        status = "success", solidHeader = TRUE,
                        helpText("Includes Models 0, 1, 2, and 3. Estimates are reported as Adjusted Odds Ratios (AOR) for Binary/Count outcomes."),
                        tableOutput("ml_comp_table"))
                  ),
                  fluidRow(
                    box(title = "Model Fitness & Variance Metrics", width = 12, plotOutput("freq_metrics_plot"))
                  )
          ),
          tabItem(tabName = "me_spatial",
                  fluidRow(
                    box(title = "Unexplained Spatial Variation (Random Effects)", width = 12, status = "info",
                        plotOutput("freq_map", height = "700px"))
                  )
          ),
          tabItem(tabName = "sae_tab",
                  fluidRow(
                    box(title = "Small Area Estimation (Fay-Herriot Model)", width = 4, status = "primary",
                        actionButton("run_sae", "Run SAE Model", class = "btn-success btn-block"),
                        hr(),
                        helpText("This module uses the Fay-Herriot area-level model to stabilize regional estimates. It combines direct survey estimates with a regression model (using your Bivariate X variable) to reduce variance in regions with small sample sizes.")
                    ),
                    box(title = "SAE Stabilized Estimates (EBLUPs)", width = 8, plotOutput("sae_map", height = "600px"))
                  ),
                  fluidRow(
                    box(title = "SAE Model Comparison: Direct vs. EBLUP", width = 12,
                        status = "info", solidHeader = TRUE,
                        tableOutput("sae_table"))
                  )
          ),
          tabItem(tabName = "inequality",
                  fluidRow(
                    box(title = "SDG 10: Spatial Inequality Metrics", width = 4, status = "danger",
                        infoBoxOutput("gini_box", width = 12),
                        infoBoxOutput("theil_box", width = 12),
                        hr(),
                        helpText("The Gini Coefficient (0 to 1) measures geographic concentration. 1 indicates total inequality. The Theil Index decomposes inequality and is sensitive to outliers.")
                    ),
                    box(title = "Lorenz Curve (Geographic Distribution)", width = 8,
                        status = "primary", solidHeader = TRUE,
                        plotOutput("lorenz_plot", height = "500px"),
                        helpText("The Lorenz Curve visualizes the cumulative distribution of the outcome across regions. The further the curve is from the diagonal line, the higher the geographic inequality."))
                  )
          ),
          tabItem(tabName = "spatial_econ",
                  fluidRow(
                    box(title = "1. Model Selection (Lagrange Multiplier Tests)", width = 12, status = "warning",
                        verbatimTextOutput("econ_lm_tests"),
                        helpText("LM tests help choose between SAR and SEM. If LM-Lag is significant, use SAR. If LM-Error is significant, use SEM."))
                  ),
                  fluidRow(
                    box(title = "2. Multivariate Spatial Regression", width = 4, status = "primary",
                        selectInput("econ_nb_type", "Spatial Weights Matrix:",
                                    choices = c("Queen's Contiguity", "K-Nearest Neighbors (K=3)", "K-Nearest Neighbors (K=5)")),
                        selectInput("econ_model_type", "Select Model Type:",
                                    choices = c("Spatial Lag Model (SAR)", "Spatial Error Model (SEM)")),
                        actionButton("run_econ", "Run Multivariate Regression", class = "btn-success btn-block"),
                        hr(),
                        helpText(strong("Note on Interpretation:"), "This is an aggregate-level model. Results for variables like 'Education' may differ from individual-level models due to the Ecological Paradox (e.g., individual protection vs. regional backlash).")
                    ),
                    box(title = "Model Coefficients", width = 8, status = "info", tableOutput("econ_coef_table"))
                  ),
                  fluidRow(
                    box(title = "3. Spillover Impacts (Direct, Indirect, Total)", width = 12, status = "success",
                        tableOutput("econ_impacts_table"))
                  )
          ),
          tabItem(tabName = "reporting",
                  fluidRow(
                    box(title = "Export Publication Report", width = 6, status = "success",
                        textInput("rep_title", "Report Title:", "HAWA Spatial Intelligence Publication Report"),
                        radioButtons("format", "Format:", choices = c("Word" = "docx", "PDF" = "pdf", "HTML" = "html")),
                        downloadButton("dl_report", "Download Full Report")
                    )
                  )
          ),
          # --- NEW: SPATIOTEMPORAL DYNAMICS TAB ---
          tabItem(tabName = "decomp",
                  fluidRow(
                    box(title = "Decomposition Controls", width = 4, status = "primary",
                        helpText("Compare two time periods (Waves) to see what drove the change in the outcome."),
                        uiOutput("wave_selector_1"),
                        uiOutput("wave_selector_2"),
                        actionButton("run_decomp", "Run Multivariate Decomposition", class = "btn-success btn-block")
                    ),
                    box(title = "Total Change Summary", width = 8, status = "info",
                        plotOutput("decomp_summary_plot", height = "300px"),
                        tableOutput("decomp_summary_table"))
                  ),
                  fluidRow(
                    box(title = "Detailed Decomposition (Variable Contribution)", width = 12,
                        helpText("Positive values indicate that the variable contributed to the increase in the outcome."),
                        tableOutput("decomp_detail_table"))
                  )
          ),
          tabItem(tabName = "st_dynamics",
                  fluidRow(
                    box(title = "4.1 Global Spatial Trend Over Time", width = 12, status = "primary",
                        uiOutput("st_moran_trend_ui")) # Use UI output for conditional rendering
                  ),
                  fluidRow(
                    box(title = "4.3 Outcome Rate of Change (Min to Max Time)", width = 6, status = "info",
                        uiOutput("st_rate_change_ui")),
                    box(title = "4.4 LISA Cluster Transition Map", width = 6, status = "info",
                        uiOutput("st_lisa_transition_ui"))
                  ),
                  fluidRow(
                    box(title = "4.2 Outcome Rate by Time Period (Small Multiples)", width = 12, status = "info",
                        plotOutput("st_small_multiples", height = "800px"))
                  )
          )
          # --- END NEW TAB ---
        )
      )
    )
  )
}
#' Add external Resources to the Application
#' @noRd
golem_add_external_resources <- function() {
  # Add golem:: prefix here
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    # Add golem:: prefix here
    golem::favicon(),
    # Add golem:: prefix here
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "HawaSpatial"
    )
  )
}
