#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom INLA inla f
#' @import tidyverse
#' @importFrom magrittr %>%
#' @importFrom sf st_read st_make_valid st_transform st_centroid st_as_sf st_filter st_make_grid st_geometry
#' @importFrom spdep poly2nb nb2listw card knn2nb knearneigh lag.listw localmoran localG localC EBlocal
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_distiller theme_void labs geom_sf_text scale_fill_brewer scale_fill_manual scale_fill_gradient2 geom_density theme_minimal geom_boxplot geom_jitter
#' @importFrom stats as.formula weighted.mean dnorm pnorm quantile var residuals
#' @importFrom rlang sym
#' @importFrom cowplot plot_grid ggdraw draw_plot
#' @importFrom biscale bi_class bi_scale_fill bi_legend
#' @importFrom gstat variogram fit.variogram vgm krige
#' @importFrom spatialreg lagsarlm
#' @importFrom tibble rownames_to_column
#' @export
app_server <- function(input, output, session) {

  # Set global options for the session
  sf::sf_use_s2(FALSE)
  options(shiny.maxRequestSize = 900 * 1024^2)
  raw_data <- reactive({
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$name)

    if(ext == "dta") {
      return(haven::read_dta(input$data_file$datapath)) # Added haven::
    } else if(ext == "sav") {
      return(haven::read_sav(input$data_file$datapath)) # Added haven::
    } else {
      return(readr::read_csv(input$data_file$datapath)) # Added readr::
    }
  })
  observe({
    df <- raw_data(); vars <- names(df)
    updateSelectInput(session, "v_dv", choices = vars)
    updateSelectInput(session, "v_reg", choices = vars)
    updateSelectInput(session, "v_cluster", choices = vars)
    updateSelectInput(session, "v_wgt", choices = vars)
    updateSelectInput(session, "v_indiv_cont", choices = vars)
    updateSelectInput(session, "v_indiv_fact", choices = vars)
    updateSelectInput(session, "v_comm_cont", choices = vars)
    updateSelectInput(session, "v_comm_fact", choices = vars)
    updateSelectInput(session, "v_wealth", choices = vars)
  })

  processed_df <- reactive({
    req(input$v_dv)
    df <- raw_data()

    if (!is.null(input$v_reg)) {
      df[[input$v_reg]] <- as.character(haven::as_factor(df[[input$v_reg]]))
    }

    # Use dplyr:: prefix for pipe-related functions
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~if(inherits(., "haven_labelled")) haven::zap_labels(.) else .))

    y <- as.numeric(as.character(df[[input$v_dv]]))

    if(input$outcome_type == "Binary") {
      if (all(y %in% c(1, 2), na.rm = TRUE)) { y <- y - 1 }
    } else if(input$outcome_type == "Fractional") {
      y <- (y * (length(y) - 1) + 0.5) / length(y)
    } else if(input$outcome_type %in% c("Ordinal", "Multinomial")) {
      y <- as.numeric(as.factor(y))
    }

    df$dv_final <- y
    for(v in c(input$v_indiv_fact, input$v_comm_fact)) {
      df[[v]] <- haven::as_factor(df[[v]])
    }
    return(df)
  })
  # --- FIXED: Spatial Join Logic with Smart Matching ---
  adal_spatial <- eventReactive(input$process_data, {
    req(input$shp_file)

    # 1. Create a temporary directory to hold shapefile components
    temp_dir <- file.path(tempdir(), as.numeric(Sys.time()))
    dir.create(temp_dir)

    # 2. Copy all uploaded files (.shp, .dbf, .shx, .prj) to the temp directory
    purrr::walk2(input$shp_file$datapath, input$shp_file$name, ~file.copy(.x, file.path(temp_dir, .y)))

    # 3. Find and read the .shp file (THIS IS THE MISSING LINE)
    shp_path <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]

    # Check if shp_path exists
    validate(need(length(shp_path) > 0, "No .shp file found in the upload."))

    shp <- sf::st_read(shp_path) |>
      sf::st_make_valid() |>
      sf::st_transform(32638)

    # 4. Process the Survey Data Stats
    stats <- processed_df() |>
      dplyr::mutate(
        dv_num = dv_final,
        wgt = if(input$is_dhs_wgt) !!rlang::sym(input$v_wgt)/1000000 else as.numeric(!!rlang::sym(input$v_wgt))
      ) |>
      dplyr::group_by(!!rlang::sym(input$v_reg)) |>
      dplyr::summarise(
        outcome = stats::weighted.mean(dv_num, wgt, na.rm = TRUE),
        bi_var = mean(as.numeric(!!rlang::sym(input$v_wealth)), na.rm = TRUE),
        cases = sum(dv_num, na.rm = TRUE),
        n_obs = dplyr::n()
      ) |>
      dplyr::mutate(
        Match_Key = stringr::str_to_upper(stringr::str_trim(as.character(!!rlang::sym(input$v_reg))))
      )

    # 5. Smart Match: Find which column in the shapefile matches our data names best
    shp_char_cols <- names(shp)[sapply(shp, is.character)]
    best_col <- shp_char_cols[1]
    max_matches <- 0

    for(col in shp_char_cols) {
      current_matches <- sum(stringr::str_to_upper(stringr::str_trim(shp[[col]])) %in% stats$Match_Key)
      if(current_matches > max_matches) {
        max_matches <- current_matches
        best_col <- col
      }
    }

    # 6. Final Join
    shp <- shp |>
      dplyr::mutate(Match_Key = stringr::str_to_upper(stringr::str_trim(as.character(!!rlang::sym(best_col)))))

    joined <- shp |>
      dplyr::left_join(stats, by = "Match_Key") |>
      dplyr::filter(!is.na(outcome))

    validate(
      need(nrow(joined) > 0, "No matching regions found. Check your Region variable names.")
    )

    return(joined)
  })

  output$sparsity_alert <- renderUI({
    # Use req() to make sure the spatial data exists before running
    req(adal_spatial())

    df <- adal_spatial()
    low_info <- sum(df$n_obs < 5, na.rm = TRUE)
    percent_low <- (low_info / nrow(df)) * 100

    if(percent_low > 25) {
      # Use standard Shiny tags if bs4Dash::callout is causing the [object Object]
      shiny::div(
        style = "padding: 15px; background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; border-radius: 4px;",
        shiny::strong("Data Sparsity Warning: "),
        paste0(round(percent_low, 1), "% of regions have low data (n < 5).")
      )
    } else {
      shiny::div(
        style = "padding: 15px; background-color: #d4edda; color: #155724; border: 1px solid #c3e6cb; border-radius: 4px;",
        shiny::strong("Data Density Check: "),
        "Data distribution is sufficient for analysis."
      )
    }
  })

  output$diag_plot <- renderPlot({
    df <- adal_spatial()
    generate_esda_plot(df, input$esda_method)
  })

  output$bi_plot <- renderPlot({
    req(input$run_bi)
    df <- adal_spatial()
    bi_data <- bi_class(df, x = bi_var, y = outcome, style = "quantile", dim = 3)
    map <- ggplot(bi_data) + geom_sf(aes(fill = bi_class), show.legend = FALSE) +
      bi_scale_fill(pal = "GrPink", dim = 3) + theme_void()
    legend <- bi_legend(pal = "GrPink", dim = 3, xlab = "Higher X ", ylab = "Higher Outcome ")
    ggdraw() + draw_plot(map, 0, 0, 1, 1) + draw_plot(legend, 0.05, 0.05, 0.25, 0.25)
  })

  output$krig_plot <- renderPlot({
    df <- adal_spatial(); pts <- st_centroid(df)
    grid <- st_make_grid(df, n = c(50, 50), what = "centers") %>% st_as_sf() %>% st_filter(df)
    v_fit <- variogram(outcome ~ 1, pts)
    v_mod <- fit.variogram(v_fit, model = vgm(psill=var(pts$outcome, na.rm=T), "Exp", range=500000, nugget=0.01))
    krig_res <- krige(outcome ~ 1, pts, grid, model = v_mod)
    ggplot(krig_res) + geom_sf(aes(color=var1.pred), size=1) + scale_color_viridis_c() + theme_void()
  })

  output$resid_plot <- renderPlot({
    df <- adal_spatial(); nb <- poly2nb(df, queen = TRUE); lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    m_sar <- lagsarlm(outcome ~ 1, data = df, lw, zero.policy = TRUE)
    df$resids <- residuals(m_sar)
    ggplot(df) + geom_sf(aes(fill=resids)) + scale_fill_gradient2() + theme_void()
  })

  # --- BAYESIAN 4-STAGE MODELS (INLA) ---

  # --- 1. Robust ICC Calculation ---
  calc_inla_icc <- function(model, type) {
    tryCatch({
      hypers <- model$summary.hyperpar
      # Find the row for the Cluster random effect precision
      prec_u_row <- grep("Precision for cluster_idx", rownames(hypers))
      if(length(prec_u_row) == 0) return(0)

      prec_u <- hypers[prec_u_row[1], "mean"]
      var_u <- 1 / prec_u

      var_e <- switch(type,
                      "Binary" = pi^2 / 3,
                      "Ordinal" = pi^2 / 3,
                      "Multinomial" = pi^2 / 3,
                      "Count" = 1,
                      "Fractional" = 1,
                      "Continuous" = {
                        prec_e_row <- grep("Precision for the Gaussian observations", rownames(hypers))
                        if(length(prec_e_row) > 0) 1 / hypers[prec_e_row, "mean"] else 0.001
                      })
      return(var_u / (var_u + var_e))
    }, error = function(e) return(0))
  }

  # --- 2. Robust Bayesian Reactive ---
  bayes_res <- eventReactive(input$run_bayes, {
    # Validation: Ensure outcome and cluster are selected
    validate(
      need(input$v_dv != "", "Please select an Outcome variable in Setup."),
      need(input$v_cluster != "", "Please select a Cluster ID (V001) in Setup."),
      need(length(c(input$v_indiv_cont, input$v_indiv_fact)) > 0, "Please select at least one Individual Level predictor.")
    )

    # Prepare Predictors
    preds_indiv <- c(input$v_indiv_cont, input$v_indiv_fact)
    preds_comm <- c(input$v_comm_cont, input$v_comm_fact)
    all_preds <- unique(c(preds_indiv, preds_comm))

    # Clean Data: Remove NAs from selected variables only
    df_clean <- processed_df() %>%
      drop_na(dv_final, !!sym(input$v_cluster), all_of(all_preds))

    # IMPORTANT: Create a numeric index for the Cluster (INLA requirement)
    df_clean$cluster_idx <- as.numeric(as.factor(df_clean[[input$v_cluster]]))

    # Map INLA Family
    inla_fam <- switch(input$outcome_type,
                       "Binary" = "binomial",
                       "Count" = "poisson",
                       "Continuous" = "gaussian",
                       "Ordinal" = "pom",
                       "Multinomial" = "pom",
                       "Fractional" = "beta")

    # Helper to build formula safely (prevents "dv ~ + f()" errors)
    build_form <- function(preds) {
      rhs <- "1 + f(cluster_idx, model='iid')"
      if (length(preds) > 0) {
        rhs <- paste(paste(preds, collapse = " + "), "+", rhs)
      }
      as.formula(paste("dv_final ~", rhs))
    }

    # Run Models with Progress Bar
    withProgress(message = 'Computing Bayesian Stages...', value = 0, {
      incProgress(0.2, detail = "Stage 0: Null")
      b0 <- inla(build_form(NULL), data = df_clean, family = inla_fam,
                 control.compute = list(dic = TRUE, waic = TRUE, mlik = TRUE))

      incProgress(0.2, detail = "Stage 1: Individual")
      b1 <- inla(build_form(preds_indiv), data = df_clean, family = inla_fam,
                 control.compute = list(dic = TRUE, waic = TRUE, mlik = TRUE))

      incProgress(0.2, detail = "Stage 2: Community")
      b2 <- inla(build_form(preds_comm), data = df_clean, family = inla_fam,
                 control.compute = list(dic = TRUE, waic = TRUE, mlik = TRUE))

      incProgress(0.2, detail = "Stage 3: Full")
      b3 <- inla(build_form(all_preds), data = df_clean, family = inla_fam,
                 control.compute = list(dic = TRUE, waic = TRUE, mlik = TRUE))
    })

    list(b0=b0, b1=b1, b2=b2, b3=b3)
  })

  # --- 3. Updated Outputs with NULL checks ---
  output$bayes_plot <- renderPlot({
    req(bayes_res())
    model <- bayes_res()$b3
    res_df <- as.data.frame(model$summary.fixed) %>%
      rownames_to_column("term") %>%
      filter(term != "(Intercept)")

    validate(need(nrow(res_df) > 0, "No fixed effects to plot (check your predictors)."))

    ggplot(res_df, aes(x=mean, y=term)) +
      geom_point(size=4, color="#006400") +
      geom_errorbarh(aes(xmin=`0.025quant`, xmax=`0.975quant`), color="#FFD700", height=0.2) +
      geom_vline(xintercept = 0, linetype="dashed") +
      theme_minimal() + labs(title="Full Model: Posterior Fixed Effects (95% Credible Intervals)")
  })

  output$bayes_metrics_plot <- renderPlot({
    req(bayes_res())
    res <- bayes_res()

    metrics_df <- data.frame(
      Model = factor(c("Null", "Indiv", "Comm", "Full"), levels=c("Null", "Indiv", "Comm", "Full")),
      DIC = c(res$b0$dic$dic, res$b1$dic$dic, res$b2$dic$dic, res$b3$dic$dic),
      WAIC = c(res$b0$waic$waic, res$b1$waic$waic, res$b2$waic$waic, res$b3$waic$waic),
      MLIK = c(res$b0$mlik[1,1], res$b1$mlik[1,1], res$b2$mlik[1,1], res$b3$mlik[1,1]),
      ICC = c(calc_inla_icc(res$b0, input$outcome_type),
              calc_inla_icc(res$b1, input$outcome_type),
              calc_inla_icc(res$b2, input$outcome_type),
              calc_inla_icc(res$b3, input$outcome_type))
    )

    p1 <- ggplot(metrics_df, aes(x=Model, y=DIC, fill=Model)) + geom_bar(stat="identity") + theme_minimal() + scale_fill_viridis_d()
    p2 <- ggplot(metrics_df, aes(x=Model, y=WAIC, fill=Model)) + geom_bar(stat="identity") + theme_minimal() + scale_fill_viridis_d()
    p3 <- ggplot(metrics_df, aes(x=Model, y=MLIK, fill=Model)) + geom_bar(stat="identity") + theme_minimal() + scale_fill_viridis_d()
    p4 <- ggplot(metrics_df, aes(x=Model, y=ICC, fill=Model)) + geom_bar(stat="identity") + theme_minimal() + scale_fill_viridis_d() + ylim(0,1)

    plot_grid(p1, p2, p3, p4, ncol = 2, labels = c("DIC", "WAIC", "Log-MLIK", "ICC"))
  })

  output$bayes_table <- renderTable({
    req(bayes_res())
    model <- bayes_res()$b3
    res <- as.data.frame(model$summary.fixed) %>% rownames_to_column("Variable")

    if(input$outcome_type %in% c("Binary", "Count", "Ordinal", "Multinomial")) {
      res <- res %>% mutate(across(c(mean, `0.025quant`, `0.975quant`), exp))
      colnames(res)[2] <- "Odds/Rate Ratio"
    }
    res
  })
  output$bayes_map <- renderPlot({
    # 1. Ensure the Bayesian model and Spatial data are ready
    req(bayes_res())
    req(adal_spatial())

    res <- bayes_res()

    # 2. Extract Random Effects from the Full Model (b3)
    # This is the cluster-level unexplained variation
    re <- res$b3$summary.random$cluster_idx

    # 3. Create a mapping between Cluster Index and Region Name
    # We need this to know which cluster belongs to which region
    df_clean <- processed_df()
    df_clean$cluster_idx <- as.numeric(as.factor(df_clean[[input$v_cluster]]))

    cluster_region_map <- df_clean |>
      dplyr::group_by(cluster_idx) |>
      dplyr::summarise(Region_Name = dplyr::first(!!rlang::sym(input$v_reg)))

    # 4. Join Random Effects to Region Names and Aggregate to Regional Level
    re_mapped <- re |>
      dplyr::left_join(cluster_region_map, by = c("ID" = "cluster_idx")) |>
      dplyr::group_by(Region_Name) |>
      dplyr::summarise(mean_random_effect = mean(mean, na.rm = TRUE)) |>
      dplyr::mutate(
        # Use stringr:: to clean names for the final spatial join
        Match_Key = stringr::str_to_upper(stringr::str_trim(as.character(Region_Name)))
      )

    # 5. Join the aggregated random effects to the Shapefile
    shp <- adal_spatial()
    map_df <- shp |> dplyr::left_join(re_mapped, by = "Match_Key")

    # 6. Render the Map
    ggplot2::ggplot(map_df) +
      ggplot2::geom_sf(ggplot2::aes(fill = mean_random_effect), color = "white", size = 0.2) +
      ggplot2::scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        name = "Random Effect"
      ) +
      ggplot2::theme_void() +
      ggplot2::labs(
        title = "Bayesian Random Effects Map",
        subtitle = "Unexplained variation after controlling for all predictors"
      ) +
      ggplot2::theme(legend.position = "right")
  })
  output$dl_report <- downloadHandler(
    filename = function() {
      paste0("HawaSpatial_Full_Report_", Sys.Date(), ".", input$format)
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "hawa_report.Rmd")

      # 1. Capture state
      report_title_val   <- input$rep_title
      report_format      <- input$format
      spatial_data_val   <- adal_spatial()
      outcome_type_val   <- input$outcome_type
      v_reg_val          <- input$v_reg
      v_cluster_val      <- input$v_cluster
      clean_df_val       <- processed_df() # Needed to link clusters to regions

      all_esda_methods <- c(
        "1. Connectivity Graph", "2. Basic Choropleth", "3. EBS Smoothing",
        "4. Quantile Map", "5. Global Moran's I", "6. Moran Scatterplot",
        "7. LISA Cluster Map", "8. Getis-Ord Gi*", "9. Join Count Analysis",
        "10. Hinge Map", "11. Spatial Correlogram", "12. Regime Density",
        "13. Spatial Lag Map", "14. SDG Target Gap", "15. Local Geary's C",
        "16. Conditional Plot"
      )

      bi_run_val <- !is.null(input$run_bi) && input$run_bi > 0
      bayes_run_val <- !is.null(input$run_bayes) && input$run_bayes > 0
      b_res_val <- if(bayes_run_val) bayes_res() else NULL

      # 2. Prepare Rmd content
      report_content <- c(
        "---",
        paste0("title: '", report_title_val, "'"),
        "author: 'HawaSpatial Pro Intelligence'",
        "date: '`r Sys.Date()`'",
        "params:",
        "  spatial_data: !r NULL",
        "  all_methods: !r NULL",
        "  bi_was_run: !r NULL",
        "  bayes_was_run: !r NULL",
        "  bayes_results: !r NULL",
        "  outcome_type: !r NULL",
        "  clean_df: !r NULL",
        "  v_reg: !r NULL",
        "  v_cluster: !r NULL",
        paste0("output: ", ifelse(report_format == "docx", "word_document",
                                  ifelse(report_format == "pdf", "pdf_document", "html_document"))),
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=7)",
        "library(tidyverse); library(sf); library(cowplot); library(biscale); library(viridis)",
        "library(spdep); library(spatialreg); library(gstat); library(scales); library(classInt)",
        "generate_esda_plot_internal <- ", paste(deparse(generate_esda_plot), collapse = "\n"),
        "calc_inla_icc_internal <- ", paste(deparse(calc_inla_icc), collapse = "\n"),
        "```",
        "",
        "# 1. Exploratory Spatial Data Analysis (ESDA)",
        "```{r, results='asis'}",
        "df <- params$spatial_data",
        "for (m in params$all_methods) {",
        "  cat('\\n\\n## ', m, '\\n')",
        "  if (m == '1. Connectivity Graph') { generate_esda_plot_internal(df, m) }",
        "  else { p <- generate_esda_plot_internal(df, m); if(!is.null(p)) print(p) }",
        "  cat('\\n\\n')",
        "}",
        "```",
        "",
        "# 2. Bivariate Spatial Analysis",
        "```{r}",
        "if(params$bi_was_run){",
        "  df_bi <- params$spatial_data",
        "  bi_data <- bi_class(df_bi, x = bi_var, y = outcome, style = 'quantile', dim = 3)",
        "  map <- ggplot(bi_data) + geom_sf(aes(fill = bi_class), show.legend = FALSE) +",
        "    bi_scale_fill(pal = 'GrPink', dim = 3) + theme_void()",
        "  legend <- bi_legend(pal = 'GrPink', dim = 3, xlab = 'Higher X ', ylab = 'Higher Outcome ')",
        "  ggdraw() + draw_plot(map, 0, 0, 1, 1) + draw_plot(legend, 0.05, 0.05, 0.25, 0.25)",
        "}",
        "```",
        "",
        "# 3. Advanced Spatial Diagnostics",
        "### Ordinary Kriging (Surface Interpolation)",
        "```{r}",
        "df_k <- params$spatial_data; pts <- st_centroid(df_k)",
        "grid <- st_make_grid(df_k, n = c(50, 50), what = 'centers') %>% st_as_sf() %>% st_filter(df_k)",
        "v_fit <- variogram(outcome ~ 1, pts)",
        "v_mod <- fit.variogram(v_fit, model = vgm(psill=var(pts$outcome, na.rm=T), 'Exp', range=500000, nugget=0.01))",
        "krig_res <- krige(outcome ~ 1, pts, grid, model = v_mod)",
        "ggplot(krig_res) + geom_sf(aes(color=var1.pred), size=1) + scale_color_viridis_c() + theme_void()",
        "```",
        "",
        "### Spatial Lag Model Residuals",
        "```{r}",
        "df_s <- params$spatial_data; nb <- poly2nb(df_s, queen = TRUE); lw <- nb2listw(nb, style = 'W', zero.policy = TRUE)",
        "m_sar <- lagsarlm(outcome ~ 1, data = df_s, lw, zero.policy = TRUE)",
        "df_s$resids <- residuals(m_sar)",
        "ggplot(df_s) + geom_sf(aes(fill=resids)) + scale_fill_gradient2() + theme_void()",
        "```",
        "",
        "# 3. Bayesian Model Comparison",
        "```{r, fig.height=10}",
        "if(params$bayes_was_run){",
        "  res <- params$bayes_results",
        "  metrics_df <- data.frame(",
        "    Model = factor(c('Null', 'Indiv', 'Comm', 'Full'), levels=c('Null', 'Indiv', 'Comm', 'Full')),",
        "    DIC = c(res$b0$dic$dic, res$b1$dic$dic, res$b2$dic$dic, res$b3$dic$dic),",
        "    WAIC = c(res$b0$waic$waic, res$b1$waic$waic, res$b2$waic$waic, res$b3$waic$waic),",
        "    MLIK = c(res$b0$mlik[1,1], res$b1$mlik[1,1], res$b2$mlik[1,1], res$b3$mlik[1,1]),",
        "    ICC = c(calc_inla_icc_internal(res$b0, params$outcome_type), ",
        "            calc_inla_icc_internal(res$b1, params$outcome_type), ",
        "            calc_inla_icc_internal(res$b2, params$outcome_type), ",
        "            calc_inla_icc_internal(res$b3, params$outcome_type))",
        "  )",
        "  p1 <- ggplot(metrics_df, aes(x=Model, y=DIC, fill=Model)) + geom_bar(stat='identity') + theme_minimal() + scale_fill_viridis_d()",
        "  p2 <- ggplot(metrics_df, aes(x=Model, y=WAIC, fill=Model)) + geom_bar(stat='identity') + theme_minimal() + scale_fill_viridis_d()",
        "  p3 <- ggplot(metrics_df, aes(x=Model, y=MLIK, fill=Model)) + geom_bar(stat='identity') + theme_minimal() + scale_fill_viridis_d()",
        "  p4 <- ggplot(metrics_df, aes(x=Model, y=ICC, fill=Model)) + geom_bar(stat='identity') + theme_minimal() + scale_fill_viridis_d() + ylim(0,1)",
        "  plot_grid(p1, p2, p3, p4, ncol = 2, labels = c('DIC', 'WAIC', 'Log-MLIK', 'ICC'))",
        "}",
        "```",
        "",
        "# 4. Bayesian Random Effects Map",
        "```{r}",
        "if(params$bayes_was_run){",
        "  re <- params$bayes_results$b3$summary.random$cluster_idx",
        "  # Link cluster ID to Region",
        "  cluster_region_map <- params$clean_df %>%",
        "    mutate(cluster_idx = as.numeric(as.factor(!!sym(params$v_cluster)))) %>%",
        "    group_by(cluster_idx) %>%",
        "    summarise(Region_Name = first(!!sym(params$v_reg)))",
        "  ",
        "  re_mapped <- re %>%",
        "    left_join(cluster_region_map, by = c('ID' = 'cluster_idx')) %>%",
        "    group_by(Region_Name) %>%",
        "    summarise(mean_random_effect = mean(mean, na.rm=TRUE)) %>%",
        "    mutate(Match_Key = str_to_upper(str_trim(as.character(Region_Name))))",
        "  ",
        "  map_re <- params$spatial_data %>% left_join(re_mapped, by = 'Match_Key')",
        "  ggplot(map_re) + geom_sf(aes(fill=mean_random_effect)) + ",
        "    scale_fill_viridis_c(option='plasma') + theme_void() +",
        "    labs(title='Map of Unexplained Cluster-Level Variation (Random Effects)',",
        "         subtitle='Aggregated to Regional Level')",
        "}",
        "```",
        "",
        "# 5. Bayesian Fixed Effects & Estimates",
        "```{r}",
        "if(params$bayes_was_run){",
        "  res <- params$bayes_results; model <- res$b3",
        "  res_df <- as.data.frame(model$summary.fixed) %>% rownames_to_column('term') %>% filter(term != '(Intercept)')",
        "  ggplot(res_df, aes(x=mean, y=term)) + ",
        "    geom_point(size=4, color='#006400') + ",
        "    geom_errorbarh(aes(xmin=`0.025quant`, xmax=`0.975quant`), color='#FFD700', height=0.2) + ",
        "    geom_vline(xintercept = 0, linetype='dashed') + theme_minimal() + labs(title='Posterior Fixed Effects')",
        "}",
        "```",
        "",
        "### Full Model Parameter Estimates",
        "```{r}",
        "if(params$bayes_was_run){ knitr::kable(as.data.frame(params$bayes_results$b3$summary.fixed), digits = 3) }",
        "```"
      )

      writeLines(report_content, tempReport)

      # 3. Render
      rmarkdown::render(tempReport, output_file = file,
                        params = list(
                          spatial_data = spatial_data_val,
                          all_methods = all_esda_methods,
                          bi_was_run = bi_run_val,
                          bayes_was_run = bayes_run_val,
                          bayes_results = b_res_val,
                          outcome_type = outcome_type_val,
                          clean_df = clean_df_val,
                          v_reg = v_reg_val,
                          v_cluster = v_cluster_val
                        ),
                        envir = new.env(parent = globalenv()))
    }
  )
}
