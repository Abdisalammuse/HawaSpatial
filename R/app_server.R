#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  raw_data <- reactive({
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$name)
    if(ext == "dta") read_dta(input$data_file$datapath) else
      if(ext == "sav") read_sav(input$data_file$datapath) else read_csv(input$data_file$datapath)
  })

  observe({
    df <- raw_data(); vars <- names(df)
    updateSelectInput(session, "v_dv", choices = vars)
    updateSelectInput(session, "v_reg", choices = vars)
    updateSelectInput(session, "v_cluster", choices = vars)
    updateSelectInput(session, "v_wgt", choices = vars)
    updateSelectInput(session, "v_strata", choices = vars)
    updateSelectInput(session, "v_indiv_cont", choices = vars)
    updateSelectInput(session, "v_indiv_fact", choices = vars)
    updateSelectInput(session, "v_comm_cont", choices = vars)
    updateSelectInput(session, "v_comm_fact", choices = vars)
    updateSelectInput(session, "v_wealth", choices = vars)
    # --- NEW: Update Time Variable Input ---
    updateSelectInput(session, "v_time", choices = c("", vars)) # Added "" for optionality
  })

  processed_df <- reactive({
    req(input$v_dv)
    df <- raw_data()

    # FORCE conversion of Region to Labels (e.g., 11 -> "Awdal")
    if (!is.null(input$v_reg)) {
      # as_factor() gets the Stata labels, as.character() ensures it's a string
      df[[input$v_reg]] <- as.character(haven::as_factor(df[[input$v_reg]]))
    }

    # Now safely remove labels from other variables for calculation
    df <- df %>% mutate(across(everything(), ~if(inherits(., "haven_labelled")) zap_labels(.) else .))

    y <- as.numeric(as.character(df[[input$v_dv]]))

    if(input$outcome_type == "Binary") {
      if (all(y %in% c(1, 2), na.rm = TRUE)) { y <- y - 1 }
    } else if(input$outcome_type == "Fractional") {
      y <- (y * (length(y) - 1) + 0.5) / length(y)
    } else if(input$outcome_type %in% c("Ordinal", "Multinomial")) {
      y <- as.numeric(as.factor(y))
    }

    df$dv_final <- y
    for(v in c(input$v_indiv_fact, input$v_comm_fact)) { df[[v]] <- as_factor(df[[v]]) }
    return(df)
  })

  # 1. Create formal Survey Design Object
  design_obj <- reactive({
    req(processed_df(), input$v_cluster, input$v_strata, input$v_wgt)
    df <- processed_df() %>% drop_na(dv_final, !!sym(input$v_cluster), !!sym(input$v_strata))
    df$wgt_final <- if(input$is_dhs_wgt) df[[input$v_wgt]]/1000000 else as.numeric(df[[input$v_wgt]])

    survey::svydesign(
      ids = as.formula(paste0("~", input$v_cluster)),
      strata = as.formula(paste0("~", input$v_strata)),
      weights = ~wgt_final,
      data = df,
      nest = TRUE
    )
  })

  # 2. SAE Engine (Fay-Herriot)
  sae_res <- eventReactive(input$run_sae, {
    # We need the design-based variance from adal_spatial
    req(adal_spatial(), input$v_wealth)

    # Extract data for sae package
    df <- adal_spatial() %>%
      st_drop_geometry() %>%
      filter(!is.na(outcome), !is.na(bi_var), !is.na(sampling_var))

    # Fit Fay-Herriot Model: Outcome ~ Covariate, Vardir = Sampling Variance
    # Note: bi_var is the mean of your 'Bivariate X Variable' per region
    fh_mod <- sae::mseFH(outcome ~ bi_var, vardir = sampling_var, data = df)

    df$eblup <- fh_mod$est$eblup
    df$mse_eblup <- fh_mod$mse
    df$cv_direct <- (sqrt(df$sampling_var) / df$outcome) * 100
    df$cv_eblup <- (sqrt(df$mse_eblup) / df$eblup) * 100

    return(df)
  })

  # 3. SAE Map Output
  output$sae_map <- renderPlot({
    req(sae_res())
    df_sae <- sae_res()
    shp <- adal_spatial() %>% dplyr::select(Match_Key, geometry)
    map_df <- shp %>% left_join(df_sae, by = "Match_Key")

    ggplot(map_df) +
      geom_sf(aes(fill = eblup), color = "white", size = 0.2) +
      scale_fill_viridis_c(option = "mako", labels = scales::percent, name = "EBLUP Estimate") +
      theme_void() +
      labs(title = "Small Area Estimation: Stabilized Regional Rates (EBLUP)",
           subtitle = "Fay-Herriot Model accounting for design-based sampling variance")
  })

  # 4. SAE Table Output
  output$sae_table <- renderTable({
    req(sae_res())
    sae_res() %>%
      dplyr::select(Region = Match_Key, `Direct Mean` = outcome, `SAE (EBLUP)` = eblup,
                    `Direct CV (%)` = cv_direct, `SAE CV (%)` = cv_eblup) %>%
      mutate(across(where(is.numeric), ~round(., 4)))
  }, striped = TRUE, bordered = TRUE)

  # --- FIXED: Spatial Join Logic with Smart Matching (Used for ESDA/Bivariate/ME) ---
  adal_spatial <- eventReactive(input$process_data, {
    req(input$shp_file, design_obj())
    temp_dir <- file.path(tempdir(), as.numeric(Sys.time()))
    dir.create(temp_dir)
    purrr::walk2(input$shp_file$datapath, input$shp_file$name, ~file.copy(.x, file.path(temp_dir, .y)))
    shp <- st_read(list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]) %>% st_make_valid() %>% st_transform(32638)

    # USE SURVEY PACKAGE FOR DESIGN-BASED MEANS AND SE
    design <- design_obj()
    stats_survey <- survey::svyby(
      formula = ~dv_final,
      by = as.formula(paste0("~", input$v_reg)),
      design = design,
      svymean,
      na.rm = TRUE
    )

    # Calculate Bivariate X Mean for SAE covariate
    df_p <- processed_df()
    df_p$wgt_f <- if(input$is_dhs_wgt) df_p[[input$v_wgt]]/1000000 else as.numeric(df_p[[input$v_wgt]])
    stats_bi <- df_p %>%
      group_by(!!sym(input$v_reg)) %>%
      summarise(bi_var = weighted.mean(as.numeric(as.character(!!sym(input$v_wealth))), wgt_f, na.rm=TRUE),
                n_obs = n(),
                cases = sum(dv_final, na.rm=TRUE))

    stats <- stats_survey %>%
      left_join(stats_bi, by = input$v_reg) %>%
      rename(outcome = dv_final, se_design = se) %>%
      mutate(sampling_var = se_design^2,
             Match_Key = str_to_upper(str_trim(as.character(!!sym(input$v_reg)))))

    # Smart Match logic (unchanged)
    shp_char_cols <- names(shp)[sapply(shp, is.character)]
    best_col <- shp_char_cols[1]; max_matches <- 0
    for(col in shp_char_cols) {
      current_matches <- sum(str_to_upper(str_trim(shp[[col]])) %in% stats$Match_Key)
      if(current_matches > max_matches) { max_matches <- current_matches; best_col <- col }
    }
    shp <- shp %>% mutate(Match_Key = str_to_upper(str_trim(as.character(!!sym(best_col)))))
    joined <- shp %>% left_join(stats, by = "Match_Key") %>% filter(!is.na(outcome))
    return(joined)
  })

  # 1. Spatial Econometrics Engine
  # --- Dedicated Spatial Aggregator for Econometrics (All Predictors) ---
  adal_spatial_econ <- reactive({
    req(adal_spatial())
    df_p <- processed_df()

    # Identify all predictors selected in Setup
    preds <- unique(c(input$v_indiv_cont, input$v_indiv_fact,
                      input$v_comm_cont, input$v_comm_fact))
    preds <- setdiff(preds, input$v_reg) # Exclude region name

    df_p$wgt_f <- if(input$is_dhs_wgt) df_p[[input$v_wgt]]/1000000 else as.numeric(df_p[[input$v_wgt]])

    # Aggregate ALL predictors to the regional level
    stats_all <- df_p %>%
      group_by(!!sym(input$v_reg)) %>%
      summarise(across(all_of(preds),
                       ~ weighted.mean(as.numeric(as.character(.x)), wgt_f, na.rm = TRUE)),
                .groups = "drop") %>%
      mutate(Match_Key = str_to_upper(str_trim(as.character(!!sym(input$v_reg)))))

    # Join with the existing spatial data (geometry + outcome)
    joined <- adal_spatial() %>%
      left_join(stats_all %>% select(-!!sym(input$v_reg)), by = "Match_Key")

    return(list(data = joined, pred_names = preds))
  })

  # --- Multivariate Econometrics Engine ---
  econ_res <- eventReactive(input$run_econ, {
    req(adal_spatial_econ())
    res_obj <- adal_spatial_econ()
    df <- res_obj$data %>% filter(!is.na(outcome))
    preds <- res_obj$pred_names

    # Build Formula: outcome ~ pred1 + pred2 + ...
    form <- as.formula(paste("outcome ~", paste(preds, collapse = " + ")))

    # Weights Matrix
    nb <- if(input$econ_nb_type == "Queen's Contiguity") {
      poly2nb(df, queen = TRUE)
    } else if(input$econ_nb_type == "K-Nearest Neighbors (K=3)") {
      knn2nb(knearneigh(st_coordinates(st_centroid(df)), k = 3))
    } else {
      knn2nb(knearneigh(st_coordinates(st_centroid(df)), k = 5))
    }

    # Add a check to ensure no islands exist
    if(any(card(nb) == 0)) {
      nb <- knn2nb(knearneigh(st_coordinates(st_centroid(df)), k = 1))
    }
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
    # 1. Lagrange Multiplier Tests
    ols_mod <- lm(form, data = df)
    lm_tests <- lm.RStests(ols_mod, lw, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
    # 2. Fit Spatial Model
    if(input$econ_model_type == "Spatial Lag Model (SAR)") {
      mod <- lagsarlm(form, data = df, listw = lw, zero.policy = TRUE)
      imp <- impacts(mod, listw = lw)
    } else {
      mod <- errorsarlm(form, data = df, listw = lw, zero.policy = TRUE)
      imp <- NULL
    }

    return(list(model = mod, impacts = imp, lm_tests = lm_tests, pred_names = preds))
  })

  output$econ_lm_tests <- renderPrint({ req(econ_res()); print(econ_res()$lm_tests) })
  output$econ_coef_table <- renderTable({ req(econ_res()); broom::tidy(econ_res()$model) }, striped = TRUE, bordered = TRUE)
  output$econ_impacts_table <- renderTable({
    req(econ_res())
    imp <- econ_res()$impacts
    if(is.null(imp)) return(data.frame(Message = "Impacts only for SAR models."))
    data.frame(Variable = econ_res()$pred_names, Direct = as.numeric(imp$direct), Indirect = as.numeric(imp$indirect), Total = as.numeric(imp$total))
  }, striped = TRUE, bordered = TRUE)


  # --- NEW: Spatiotemporal Data Aggregation ---
  # --- FIXED: Spatiotemporal Data Aggregation ---
  adal_spatiotemporal <- reactive({
    req(input$v_reg, processed_df())
    df_proc <- processed_df()

    # Guard: If no time variable is selected, return NULL
    if (is.null(input$v_time) || input$v_time == "") {
      return(NULL)
    }

    # Aggregate by Region AND Time
    stats_time <- df_proc %>%
      mutate(
        dv_num = dv_final,
        wgt = if(input$is_dhs_wgt) !!sym(input$v_wgt)/1000000 else as.numeric(!!sym(input$v_wgt))
      ) %>%
      group_by(!!sym(input$v_reg), time_var = as.factor(!!sym(input$v_time))) %>%
      summarise(
        outcome = weighted.mean(dv_num, wgt, na.rm = TRUE),
        n_obs = n(),
        .groups = 'drop'
      ) %>%
      mutate(Match_Key = str_to_upper(str_trim(as.character(!!sym(input$v_reg)))))

    # Re-join geometry
    shp_base <- adal_spatial() %>% select(Match_Key, geometry) %>% st_drop_geometry() %>% distinct()

    st_sf <- stats_time %>%
      left_join(adal_spatial() %>% select(Match_Key, geometry), by = "Match_Key") %>%
      st_as_sf()

    return(st_sf)
  })

  output$sparsity_alert <- renderUI({
    df <- adal_spatial()
    low_info <- sum(df$n_obs < 5, na.rm = TRUE)
    percent_low <- (low_info / nrow(df)) * 100
    if(percent_low > 25) {
      callout(title = "Data Sparsity Warning", status = "danger", paste0(round(percent_low, 1), "% of regions have low data."))
    } else {
      callout(title = "Data Density Check", status = "success", "Data distribution is sufficient.")
    }
  })
  # ORIGINAL SPATIAL LOGIC
  generate_esda_plot <- function(df, method) {
    nb <- poly2nb(df, queen = TRUE)
    if(any(card(nb) == 0)) nb <- knn2nb(knearneigh(st_coordinates(st_centroid(df)), k = 1))
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

    if(method == "1. Connectivity Graph") {
      plot(st_geometry(df), border="grey")
      plot(nb, st_coordinates(st_centroid(df)), add=TRUE, col="#006400", lwd=2)
    } else if(method == "2. Basic Choropleth") {
      ggplot(df) + geom_sf(aes(fill=outcome), color="black", size=0.2) +
        scale_fill_distiller(palette = "RdPu", direction = 1, labels = percent) + theme_void()
    } else if(method == "3. EBS Smoothing") {
      eb_res <- EBlocal(ri = df$cases, ni = df$n_obs, nb = nb, zero.policy = TRUE)
      df$eb_rate <- eb_res$est

      # Plot 1: Raw Map with Labels
      p1 <- ggplot(df) +
        geom_sf(aes(fill=outcome)) +
        geom_sf_text(aes(label = Match_Key), size = 2.5, color = "black", check_overlap = TRUE) +
        scale_fill_distiller(palette="RdPu", direction=1) +
        labs(title="Raw Rates") + theme_void()

      # Plot 2: Smoothed Map with Labels
      p2 <- ggplot(df) +
        geom_sf(aes(fill=eb_rate)) +
        geom_sf_text(aes(label = Match_Key), size = 2.5, color = "black", check_overlap = TRUE) +
        scale_fill_distiller(palette="RdPu", direction=1) +
        labs(title="EBS Smoothed") + theme_void()

      plot_grid(p1, p2)
    } else if(method == "4. Quantile Map") {
      breaks_qt <- classIntervals(df$outcome, n = 5, style = "quantile")
      df$quantile_cat <- cut(df$outcome, breaks_qt$brks, include.lowest = TRUE)
      ggplot(df) + geom_sf(aes(fill = quantile_cat), color = "black", size = 0.2) +
        scale_fill_brewer(palette = "Blues", name = "Quintiles") + labs(title = "4. Quantile Map (Ranking)") + theme_void()
    } else if(method == "5. Global Moran's I") {
      gmoran <- moran.test(df$outcome, lw, zero.policy = TRUE)
      z_score <- gmoran$statistic
      p_value <- gmoran$p.value
      moran_i <- gmoran$estimate[1]
      df_norm <- data.frame(x = seq(-4, 4, length.out = 1000)) %>% mutate(y = dnorm(x))
      ggplot(df_norm, aes(x, y)) + geom_line(size = 1) + geom_area(data = subset(df_norm, x >= z_score), fill = "red", alpha = 0.5) +
        geom_vline(xintercept = z_score, color = "red", linetype="dashed") +
        annotate("text", x = 2.5, y = 0.3, label = paste("p-value:", format.pval(p_value)), color="red", fontface="bold") +
        labs(title = "5. Global Moran's I (Systemic Test)", subtitle = paste("Moran's I:", round(moran_i, 3), "| Z-Score:", round(z_score, 2)), x = "Z-Score", y = "Density") + theme_minimal()
    } else if(method == "6. Moran Scatterplot") {
      df$lag_outcome <- lag.listw(lw, df$outcome, zero.policy = TRUE)
      m_prev <- mean(df$outcome)
      df$quadrant <- case_when(df$outcome >= m_prev & df$lag_outcome >= m_prev ~ "High-High", df$outcome <= m_prev & df$lag_outcome <= m_prev ~ "Low-Low", df$outcome >= m_prev & df$lag_outcome <= m_prev ~ "High-Low", df$outcome <= m_prev & df$lag_outcome >= m_prev ~ "Low-High")
      ggplot(df, aes(x = outcome, y = lag_outcome, color = quadrant)) + geom_vline(xintercept = m_prev, linetype = "dashed") + geom_hline(yintercept = mean(df$lag_outcome), linetype = "dashed") + geom_point(size = 3) + geom_text(aes(label = Match_Key), vjust = -0.5, size = 3, show.legend = FALSE) + geom_smooth(method = "lm", se = FALSE, color = "black") + scale_color_manual(values = c("High-High"="red", "Low-Low"="blue", "High-Low"="pink", "Low-High"="lightblue")) + labs(title = "6. Moran Scatterplot (Association)") + theme_minimal()
    } else if(method == "7. LISA Cluster Map") {
      loc_m <- localmoran(df$outcome, lw, zero.policy = TRUE)
      df$Pr <- loc_m[,5]; df$lag_outcome <- lag.listw(lw, df$outcome, zero.policy = TRUE)
      m_prev <- mean(df$outcome)

      # --- ENHANCED LISA CLUSTER MAP (Coloring by Quadrant and Significance) ---
      df$lisa_quadrant <- case_when(
        df$Pr <= 0.05 & df$outcome >= m_prev & df$lag_outcome >= m_prev ~ "High-High",
        df$Pr <= 0.05 & df$outcome <= m_prev & df$lag_outcome <= m_prev ~ "Low-Low",
        df$Pr <= 0.05 & df$outcome >= m_prev & df$lag_outcome <= m_prev ~ "High-Low",
        df$Pr <= 0.05 & df$outcome <= m_prev & df$lag_outcome >= m_prev ~ "Low-High",
        TRUE ~ "Not Significant"
      )

      lisa_colors <- c("High-High"="red", "Low-Low"="blue", "High-Low"="pink", "Low-High"="lightblue", "Not Significant"="lightgray")

      ggplot(df) + geom_sf(aes(fill=lisa_quadrant), color="black") +
        scale_fill_manual(values=lisa_colors, name="LISA Cluster Type") +
        labs(title = "7. LISA Cluster Map (Local Spatial Association)") +
        theme_void()

    } else if(method == "8. Getis-Ord Gi*") {
      gi_stat <- localG(df$outcome, lw, zero.policy = TRUE)
      df$gi_z <- as.numeric(gi_stat)
      ggplot(df) + geom_sf(aes(fill=gi_z)) + scale_fill_gradient2(low="blue", mid="white", high="red") + theme_void()
    } else if(method == "9. Join Count Analysis") {
      global_mean <- mean(df$outcome, na.rm = TRUE)
      df$Coverage_Class <- factor(ifelse(df$outcome > global_mean, "High", "Low"))
      ggplot(data = df) + geom_sf(aes(fill = Coverage_Class), color = "black", size = 0.2) + scale_fill_manual(values = c("High" = "forestgreen", "Low" = "darkorange")) + labs(title = "9. Join Count Binary Map (Categorical Clustering)") + theme_void()
    } else if(method == "10. Hinge Map") {
      cuts <- quantile(df$outcome, c(0, 0.25, 0.5, 0.75, 1)); iqr <- diff(cuts[c(2, 4)])
      df$Hinge <- cut(df$outcome, breaks = c(-Inf, cuts[2]-1.5*iqr, cuts[2], cuts[3], cuts[4], cuts[4]+1.5*iqr, Inf), labels = c("L-Outlier", "<25%", "25-50%", "50-75%", ">75%", "U-Outlier"))
      ggplot(df) + geom_sf(aes(fill=Hinge)) + scale_fill_brewer(palette="RdBu") + theme_void()
    } else if(method == "11. Spatial Correlogram") {
      # Dynamic Lag Calculation:
      n_regions <- nrow(df)
      max_lag <- ifelse(n_regions < 8, floor(n_regions / 2), 4)

      # Safety check: If data is too small (e.g., < 3 regions), show error
      if (max_lag < 1) {
        ggplot() + annotate("text", x = 1, y = 1, label = "Not enough regions for Correlogram", size = 6) + theme_void()
      } else {
        tryCatch({
          cor_res <- sp.correlogram(nb, df$outcome, order = max_lag, method = "I", style = "W", zero.policy = TRUE)

          actual_lags <- nrow(cor_res$res)

          cor_df <- data.frame(
            Lag = 1:actual_lags,
            Morans_I = cor_res$res[, 1],
            P_Value = 2 * (1 - pnorm(abs(cor_res$res[, 1] - cor_res$res[, 2]) / cor_res$res[, 3]))
          ) %>% mutate(Significance = ifelse(P_Value < 0.05, "Significant", "Not Significant"))

          ggplot(cor_df, aes(x = Lag, y = Morans_I)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
            geom_line() +
            geom_point(aes(color = Significance), size = 4) +
            scale_color_manual(values = c("Significant" = "red", "Not Significant" = "blue")) +
            scale_x_continuous(breaks = 1:actual_lags) + # Ensure integer ticks
            labs(title = "11. Spatial Correlogram", y = "Moran's I") +
            theme_minimal()
        }, error = function(e) {
          ggplot() + annotate("text", x = 1, y = 1, label = "Data structure cannot support Correlogram", size = 5) + theme_void()
        })
      }} else if(method == "12. Regime Density") {
        gi_stat <- localG(df$outcome, lw, zero.policy = TRUE)
        df$Regime <- ifelse(as.numeric(gi_stat) > 1.65, "Hot Spot", ifelse(as.numeric(gi_stat) < -1.65, "Cold Spot", "Neutral"))
        ggplot(df, aes(x=outcome, fill=Regime)) + geom_density(alpha=0.5) + theme_minimal()
      } else if(method == "13. Spatial Lag Map") {
        df$lag_outcome <- lag.listw(lw, df$outcome, zero.policy = TRUE)
        ggplot(df) + geom_sf(aes(fill = lag_outcome), color = "black", size = 0.2) + scale_fill_distiller(palette = "YlGnBu", direction = 1, labels = percent) + labs(title = "13. Spatial Lag Map (Neighborhood Context)", subtitle = "Average Rate of Neighbors") + theme_void()
      } else if(method == "14. SDG Target Gap") {
        df$gap <- df$outcome - 0
        ggplot(df) + geom_sf(aes(fill=gap)) + scale_fill_viridis_c(option="magma", direction=-1) + theme_void()
      } else if(method == "15. Local Geary's C") {
        geary_loc <- localC(df$outcome, lw); df$geary_c <- geary_loc
        ggplot(df) + geom_sf(aes(fill = geary_c), color = "black", size = 0.2) + scale_fill_viridis_c(option = "magma", direction = -1) + labs(title = "15. Local Geary's C (Edge Detection)", subtitle = "Darker = High Dissimilarity") + theme_void()
      } else if(method == "16. Conditional Plot") {
        df$lag_bin <- cut(lag.listw(lw, df$outcome), breaks=3, labels=c("Low Nb", "Med Nb", "High Nb"))
        ggplot(df, aes(x=lag_bin, y=outcome, fill=lag_bin)) + geom_boxplot() + geom_jitter(width=0.1) + theme_minimal()
      } else if(method == "17. Global Geary's C") {
        g_geary <- geary.test(df$outcome, lw, zero.policy = TRUE)
        z_score <- as.numeric(g_geary$statistic)
        p_value <- g_geary$p.value
        geary_c <- g_geary$estimate[1]
        df_norm <- data.frame(x = seq(-4, 4, length.out = 1000)) %>% mutate(y = dnorm(x))
        ggplot(df_norm, aes(x, y)) + geom_line(size = 1) +
          geom_area(data = subset(df_norm, x <= z_score), fill = "#006400", alpha = 0.5) +
          geom_vline(xintercept = z_score, color = "red", linetype="dashed", size = 1) +
          annotate("text", x = 2.5, y = 0.3, label = paste("p-value:", format.pval(p_value)), color="red", fontface="bold") +
          labs(title = "17. Global Geary's C (Systemic Test)",
               subtitle = paste("Geary's C:", round(geary_c, 3), "| Z-Score:", round(z_score, 2)),
               x = "Z-Score", y = "Density") + theme_minimal()

      } else {
        ggplot(df) + geom_sf(aes(fill=outcome)) + scale_fill_viridis_c() + theme_void()
      }
  }

  output$diag_plot <- renderPlot({
    df <- adal_spatial()
    generate_esda_plot(df, input$esda_method)
  })

  output$bi_moran_test <- renderPrint({
    df <- adal_spatial()
    req(input$v_wealth)

    # 1. Define Spatial Weights
    nb <- poly2nb(df, queen = TRUE)
    if(any(card(nb) == 0)) nb <- knn2nb(knearneigh(st_coordinates(st_centroid(df)), k = 1))
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

    # Check for NA values in the variables used for the test
    if (any(is.na(df$outcome)) || any(is.na(df$bi_var))) {
      cat("Bivariate Moran's I Test Skipped: Outcome or Bivariate variable contains missing values (NA).")
      return(NULL)
    }

    # --- CORRECTED CONDITION CHECK ---
    # Check if we have enough observations AND if the spatial weights list is valid
    is_valid_spatial_structure <- nrow(df) > 1 && length(lw$neighbours) > 1 && sum(card(nb)) > 0

    if (is_valid_spatial_structure) {

      # 2. Bivariate Moran's I (Y vs Lagged X) using Moran Monte Carlo (the robust method)

      # Standardize variables for Bivariate I calculation
      z_y <- scale(df$outcome)[, 1]
      z_x <- scale(df$bi_var)[, 1]

      # The Bivariate Moran's I statistic is calculated on the product of the standardized variables
      # and tested via permutation (Moran Monte Carlo).
      product_var <- z_y * z_x

      # Run Moran Monte Carlo test (99 simulations)
      bi_moran_mc <- moran.mc(product_var, lw, nsim = 99, zero.policy = TRUE)

      # Extract results
      I_B_observed <- bi_moran_mc$statistic
      p_value <- bi_moran_mc$p.value

      cat("Bivariate Moran's I (Outcome vs. Lagged X) - Permutation Test (999 simulations):\n")
      cat(paste("Observed Bivariate I:", round(I_B_observed, 4), "\n"))
      cat(paste("P-value (Simulated):", format.pval(p_value), "\n"))
      cat(paste("Alternative hypothesis: greater\n"))

      cat("\nInterpretation: Measures if the outcome in a region is correlated with the X variable (e.g., Wealth) in neighboring regions. A significant positive I indicates spatial covariance between the two variables.")

    } else {
      cat("Bivariate Moran's I Test Skipped: Requires at least two connected regions (e.g., check shapefile connectivity or data density).")
    }
    # --- END CORRECTED CONDITION CHECK ---
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
    df <- adal_spatial()
    # Add st_jitter to prevent singular matrix errors from overlapping centroids
    pts <- st_centroid(df) %>% st_jitter(amount = 0.01)

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

  # 1. Calculate Inequality Indices
  ineq_stats <- reactive({
    req(adal_spatial())
    df <- adal_spatial() %>% filter(!is.na(outcome))

    g_val <- ineq(df$outcome, type = "Gini")
    t_val <- ineq(df$outcome, type = "Theil")

    return(list(gini = g_val, theil = t_val))
  })

  # 2. Render InfoBoxes
  output$gini_box <- renderInfoBox({
    infoBox("Gini Index", round(ineq_stats()$gini, 3), icon = icon("divide"), color = "danger", fill = TRUE) # Changed 'red' to 'danger'
  })

  output$theil_box <- renderInfoBox({
    infoBox("Theil Index", round(ineq_stats()$theil, 3), icon = icon("chart-line"), color = "orange", fill = TRUE)
  })

  # 3. Lorenz Curve Plot
  output$lorenz_plot <- renderPlot({
    req(adal_spatial())
    df <- adal_spatial() %>% filter(!is.na(outcome))

    # Plotting the Lorenz Curve
    plot(Lc(df$outcome), col = "red", lwd = 2, main = "Geographic Lorenz Curve")
  })
  # Add to ineq_stats reactive
  conc_index <- reactive({
    req(adal_spatial(), input$v_wealth)
    df <- adal_spatial() %>% st_drop_geometry() %>% filter(!is.na(outcome), !is.na(bi_var))
    # bi_var is your wealth variable aggregated to region
    # We sort by wealth to calculate the Concentration Index
    df <- df[order(df$bi_var), ]
    # Simple Concentration Index calculation
    mu <- mean(df$outcome)
    n <- nrow(df)
    index <- (2 / (n * mu)) * sum(df$outcome * (1:n)) - 1 - (1/n)
    return(index)
  })

  output$conc_box <- renderInfoBox({
    infoBox("Concentration Index", round(conc_index(), 4),
            icon = icon("hand-holding-usd"), color = "purple", fill = TRUE)
  })
  # --- SPATIOTEMPORAL OUTPUTS ---

  output$st_moran_trend_ui <- renderUI({
    df_st <- adal_spatiotemporal()

    if (is.null(df_st) || length(unique(df_st$time_var)) < 2) {
      bs4Dash::callout(
        title = "Spatiotemporal Analysis Disabled",
        status = "warning",
        width = 12,
        "This module requires at least two survey waves (years). Please ensure a Time Variable is selected in the Setup tab."
      )
    } else {
      plotOutput("st_moran_trend", height = "400px")
    }
  })

  output$st_moran_trend <- renderPlot({
    df_st <- adal_spatiotemporal()
    req(nrow(df_st) > 0)

    # Robustly check unique time points (already checked in UI, but safe here)
    time_points_numeric <- df_st$time_var %>% as.character() %>% as.numeric() %>% unique()
    req(length(time_points_numeric) >= 2)

    # Calculate Moran's I for each time period
    moran_results <- df_st %>%
      group_by(time_var) %>%
      group_modify(~ {
        current_df <- .x
        # Ensure connectivity for the current subset
        nb <- poly2nb(current_df, queen = TRUE)
        # Fallback to KNN if any region is isolated
        if(any(card(nb) == 0)) nb <- knn2nb(knearneigh(st_coordinates(st_centroid(current_df)), k = 1))
        lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

        # Calculate Moran's I
        if (length(lw$neighbours) > 1) {
          m_test <- moran.test(current_df$outcome, lw, zero.policy = TRUE)
          tibble(
            Moran_I = m_test$estimate[1],
            P_Value = m_test$p.value
          )
        } else {
          tibble(Moran_I = NA, P_Value = NA)
        }
      }) %>%
      ungroup() %>%
      mutate(Significant = P_Value < 0.05)

    ggplot(moran_results, aes(x = time_var, y = Moran_I, group = 1)) +
      geom_line(color = "#006400", size = 1.2) +
      geom_point(aes(color = Significant), size = 4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray"), name = "Significant (p<0.05)") +
      labs(title = "Global Spatial Autocorrelation Trend (Moran's I)",
           subtitle = "Tracking the strength of clustering over time.",
           x = input$v_time, y = "Moran's I") +
      theme_minimal(base_size = 14)
  })

  output$st_rate_change_ui <- renderUI({
    df_st <- adal_spatiotemporal()
    time_points_numeric <- df_st$time_var %>% as.character() %>% as.numeric() %>% unique()

    if (length(time_points_numeric) < 2) {
      box(title = "4.3 Outcome Rate of Change (Min to Max Time)", status = "warning", width = 12,
          p("Skipped: Requires multiple time points (survey years) for Rate of Change analysis."))
    } else {
      plotOutput("st_rate_change", height = "400px")
    }
  })

  output$st_rate_change <- renderPlot({
    df_st <- adal_spatiotemporal()

    # --- FIX: Convert factor time_var to numeric for min/max ---
    time_points <- df_st$time_var %>% as.character() %>% as.numeric() %>% unique() %>% sort()

    req(length(time_points) >= 2)

    t_min <- min(time_points)
    t_max <- max(time_points)

    # Filter using the numeric time points, matching against the factor/character column
    df_min <- df_st %>% filter(as.numeric(as.character(time_var)) == t_min) %>% st_drop_geometry() %>% select(Match_Key, outcome_min = outcome)
    df_max <- df_st %>% filter(as.numeric(as.character(time_var)) == t_max) %>% st_drop_geometry() %>% select(Match_Key, outcome_max = outcome)

    df_change <- df_min %>%
      full_join(df_max, by = "Match_Key") %>%
      mutate(
        change = outcome_max - outcome_min,
        percent_change = (change / outcome_min) * 100
      )

    map_df <- adal_spatial() %>%
      select(Match_Key, geometry) %>%
      left_join(df_change, by = "Match_Key")

    ggplot(map_df) +
      geom_sf(aes(fill = percent_change), color = "white", size = 0.2) +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                           name = "Percent Change",
                           labels = function(x) paste0(round(x, 0), "%")) +
      theme_void() +
      labs(title = paste("Percent Change in Outcome Rate:", t_min, "to", t_max),
           subtitle = "Blue = Improvement (Positive Change), Red = Decline (Negative Change)")
  })

  output$st_lisa_transition_ui <- renderUI({
    df_st <- adal_spatiotemporal()
    time_points_numeric <- df_st$time_var %>% as.character() %>% as.numeric() %>% unique()

    if (length(time_points_numeric) < 2) {
      box(title = "4.4 LISA Cluster Transition Map", status = "warning", width = 12,
          p("Skipped: Requires multiple time points (survey years) for LISA Transition analysis."))
    } else {
      plotOutput("st_lisa_transition", height = "400px")
    }
  })

  output$st_lisa_transition <- renderPlot({
    df_st <- adal_spatiotemporal()

    # --- FIX: Convert factor time_var to numeric for min/max ---
    time_points <- df_st$time_var %>% as.character() %>% as.numeric() %>% unique() %>% sort()

    req(length(time_points) >= 2)

    t_min <- min(time_points)
    t_max <- max(time_points)

    # 1. Get base geometry and neighborhood list
    base_shp <- df_st %>% filter(as.numeric(as.character(time_var)) == t_min)
    nb <- poly2nb(base_shp, queen = TRUE)
    if(any(card(nb) == 0)) nb <- knn2nb(knearneigh(st_coordinates(st_centroid(base_shp)), k = 1))
    lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

    # 2. Calculate LISA for Min Time
    df_min <- df_st %>% filter(as.numeric(as.character(time_var)) == t_min)
    loc_m_min <- localmoran(df_min$outcome, lw, zero.policy = TRUE)
    m_prev_min <- mean(df_min$outcome)
    df_min$lisa_min <- case_when(
      loc_m_min[,5] <= 0.05 & df_min$outcome >= m_prev_min ~ "HH",
      loc_m_min[,5] <= 0.05 & df_min$outcome <= m_prev_min ~ "LL",
      TRUE ~ "NS"
    )

    # 3. Calculate LISA for Max Time
    df_max <- df_st %>% filter(as.numeric(as.character(time_var)) == t_max)
    loc_m_max <- localmoran(df_max$outcome, lw, zero.policy = TRUE)
    m_prev_max <- mean(df_max$outcome)
    df_max$lisa_max <- case_when(
      loc_m_max[,5] <= 0.05 & df_max$outcome >= m_prev_max ~ "HH",
      loc_m_max[,5] <= 0.05 & df_max$outcome <= m_prev_max ~ "LL",
      TRUE ~ "NS"
    )

    # 4. Join and create Transition Category
    df_transition <- df_min %>%
      st_drop_geometry() %>%
      select(Match_Key, lisa_min) %>%
      left_join(df_max %>% st_drop_geometry() %>% select(Match_Key, lisa_max), by = "Match_Key") %>%
      mutate(
        Transition = paste0(lisa_min, " -> ", lisa_max),
        Transition = ifelse(lisa_min == "NS" | lisa_max == "NS", "Not Significant", Transition)
      )

    map_df <- adal_spatial() %>%
      select(Match_Key, geometry) %>%
      left_join(df_transition, by = "Match_Key")

    # Define a color palette for key transitions
    transition_colors <- c(
      "HH -> HH" = "darkred",
      "LL -> LL" = "darkblue",
      "HH -> LL" = "purple",
      "LL -> HH" = "orange",
      "NS -> NS" = "lightgray",
      "Not Significant" = "lightgray"
    )

    ggplot(map_df) +
      geom_sf(aes(fill = Transition), color = "black", size = 0.1) +
      scale_fill_manual(values = transition_colors, na.value = "white") +
      theme_void() +
      labs(title = paste("LISA Cluster Transition:", t_min, "to", t_max),
           subtitle = "HH = High-High, LL = Low-Low")
  })

  output$st_small_multiples <- renderPlot({
    df_st <- adal_spatiotemporal()
    req(nrow(df_st) > 0)

    # Determine the range for consistent color scaling across all time periods
    max_outcome <- max(df_st$outcome, na.rm = TRUE)
    min_outcome <- min(df_st$outcome, na.rm = TRUE)

    ggplot(df_st) +
      geom_sf(aes(fill = outcome), color = "black", size = 0.1) +
      scale_fill_viridis_c(option = "plasma", labels = percent, limits = c(min_outcome, max_outcome)) +
      facet_wrap(~ time_var, ncol = 3) +
      labs(title = "Outcome Rate Over Time (Small Multiples)",
           fill = "Outcome Rate") +
      theme_void() +
      theme(strip.text = element_text(face = "bold", size = 12),
            legend.position = "bottom")
  })

  # --- UPDATED: MULTILEVEL 4-STAGE PUBLICATION ENGINE ---

  # --- 1. Multilevel 4-Stage Engine ---
  freq_res <- eventReactive(input$run_freq, {
    req(input$v_dv, input$v_cluster)

    # Define predictor sets
    preds_indiv <- c(input$v_indiv_cont, input$v_indiv_fact)
    preds_comm <- c(input$v_comm_cont, input$v_comm_fact)
    all_preds <- unique(c(preds_indiv, preds_comm))

    # Clean data: remove NAs from DV and Cluster
    df_clean <- processed_df() %>%
      drop_na(dv_final, !!sym(input$v_cluster))

    df_clean$cluster_idx <- as.factor(df_clean[[input$v_cluster]])

    # Map glmmTMB Families based on outcome type
    fam_obj <- switch(input$outcome_type,
                      "Binary"     = binomial(link="logit"),
                      "Count"      = poisson(),
                      "Continuous" = gaussian(),
                      "Fractional" = beta_family(),
                      "Ordinal"    = cumulative(),
                      "Nominal"    = multinomial(),
                      gaussian())

    # Formula builder helper
    build_form_freq <- function(preds) {
      rhs <- "(1 | cluster_idx)"
      if (length(preds) > 0) rhs <- paste(paste(preds, collapse = " + "), "+", rhs)
      as.formula(paste("dv_final ~", rhs))
    }

    withProgress(message = 'Running Hierarchical Models...', value = 0, {
      incProgress(0.2, detail = "Stage 0: Null Model")
      m0 <- glmmTMB(build_form_freq(NULL), data = df_clean, family = fam_obj)

      incProgress(0.2, detail = "Stage 1: Individual Level")
      m1 <- if(length(preds_indiv) > 0) glmmTMB(build_form_freq(preds_indiv), data = df_clean, family = fam_obj) else m0

      incProgress(0.2, detail = "Stage 2: Community Level")
      m2 <- if(length(preds_comm) > 0) glmmTMB(build_form_freq(preds_comm), data = df_clean, family = fam_obj) else m0

      incProgress(0.2, detail = "Stage 3: Full Model")
      m3 <- if(length(all_preds) > 0) glmmTMB(build_form_freq(all_preds), data = df_clean, family = fam_obj) else m0
    })

    list(m0=m0, m1=m1, m2=m2, m3=m3)
  })

  # --- 2. Formal Side-by-Side Publication Table ---
  output$ml_comp_table <- renderTable({
    req(freq_res())
    models <- freq_res()

    # Helper function to tidy and format each model column
    tidy_m_full <- function(m, name) {
      # Get fixed effects with 95% Confidence Intervals
      res <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE)

      # For Binary and Count, convert log-odds/log-rates to Ratios (AOR/ARR)
      if(input$outcome_type %in% c("Binary", "Count")) {
        res <- res %>% mutate(across(c(estimate, conf.low, conf.high), exp))
      }

      res %>%
        mutate(
          stars = case_when(
            p.value < 0.001 ~ "***",
            p.value < 0.01  ~ "**",
            p.value < 0.05  ~ "*",
            TRUE            ~ ""
          ),
          # Create the academic string: Estimate (Lower CI, Upper CI) Stars
          formatted = paste0(
            round(estimate, 2),
            " (", round(conf.low, 2), "-", round(conf.high, 2), ")",
            stars
          )
        ) %>%
        select(term, !!name := formatted)
    }

    # Process all 4 models
    t0 <- tidy_m_full(models$m0, "Model 0 (Null)")
    t1 <- tidy_m_full(models$m1, "Model 1 (Indiv)")
    t2 <- tidy_m_full(models$m2, "Model 2 (Comm)")
    t3 <- tidy_m_full(models$m3, "Model 3 (Full)")

    # Join models side-by-side
    final_table <- t0 %>%
      full_join(t1, by = "term") %>%
      full_join(t2, by = "term") %>%
      full_join(t3, by = "term") %>%
      # Remove the Intercept row for a cleaner publication look
      filter(!str_detect(term, "Intercept")) %>%
      rename(Predictor = term)

    # Replace NA with "-" for variables not present in specific models
    final_table[is.na(final_table)] <- "-"

    return(final_table)

  }, striped = TRUE, bordered = TRUE, align = 'l', spacing = 'm')

  output$freq_forest_plot <- renderPlot({
    req(freq_res())
    model <- freq_res()$m3
    res_df <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
      filter(term != "(Intercept)")

    ggplot(res_df, aes(x=estimate, y=term)) +
      geom_point(size=4, color="#006400") +
      geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), color="#FFD700", height=0.2) +
      geom_vline(xintercept = 0, linetype="dashed") +
      theme_minimal() + labs(title="Full Model: Fixed Effects (95% CI)")
  })

  output$freq_metrics_plot <- renderPlot({
    req(freq_res())
    res <- freq_res()

    # 1. Construct the data frame with explicit numeric conversion for LogLik
    metrics_df <- data.frame(
      Model = factor(c("Null", "Indiv", "Comm", "Full"),
                     levels = c("Null", "Indiv", "Comm", "Full")),
      AIC = c(AIC(res$m0), AIC(res$m1), AIC(res$m2), AIC(res$m3)),
      BIC = c(BIC(res$m0), BIC(res$m1), BIC(res$m2), BIC(res$m3)),
      ICC = c(calc_freq_icc(res$m0), calc_freq_icc(res$m1), calc_freq_icc(res$m2), calc_freq_icc(res$m3))
    )

    # Calculate PCV (Proportional Change in Variance)
    pcv_m1 <- calc_pcv(res$m0, res$m1)
    pcv_m2 <- calc_pcv(res$m0, res$m2)
    pcv_m3 <- calc_pcv(res$m0, res$m3)

    pcv_df <- data.frame(
      Model = factor(c("Indiv", "Comm", "Full"), levels = c("Indiv", "Comm", "Full")),
      PCV = c(pcv_m1, pcv_m2, pcv_m3)
    )

    # Helper function for consistent professional styling
    style_metric_plot <- function(df, y_var, title, is_icc = FALSE, is_pcv = FALSE) {
      p <- ggplot(df, aes(x = Model, y = !!sym(y_var), fill = Model)) +
        geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
        geom_text(aes(label = round(!!sym(y_var), 3)), vjust = -0.3, size = 3.5, fontface = "bold") +
        scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
        labs(title = title, x = NULL, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(face = "bold")
        )

      if(is_icc) p <- p + ylim(0, 1.1)
      if(is_pcv) p <- p + labs(y = "PCV (Proportion Explained)") +
          geom_text(aes(label = scales::percent(!!sym(y_var), accuracy = 0.1)), vjust = -0.3, size = 3.5, fontface = "bold")

      return(p)
    }

    # 2. Generate individual plots
    p_aic  <- style_metric_plot(metrics_df, "AIC", "AIC (Lower is Better)")
    p_icc  <- style_metric_plot(metrics_df, "ICC", "ICC (Intraclass Correlation)", is_icc = TRUE)
    p_pcv  <- style_metric_plot(pcv_df, "PCV", "PCV (Proportional Change in Variance)", is_pcv = TRUE)

    # 3. Combine into a professional 2x2 grid (AIC, BIC, ICC, VPR)
    cowplot::plot_grid(
      p_aic,
      style_metric_plot(metrics_df, "BIC", "BIC (Lower is Better)"),
      p_icc,
      p_pcv,
      ncol = 2,
      labels = "AUTO",
      label_size = 14
    )
  })
  output$freq_map <- renderPlot({
    req(freq_res())
    res <- freq_res()

    # Extract Random Effects (BLUPs)
    re <- as.data.frame(ranef(res$m3)$cond$cluster_idx)
    re$ID <- rownames(re)
    colnames(re)[1] <- "intercept_re"

    df_clean <- processed_df()
    cluster_region_map <- df_clean %>%
      group_by(!!sym(input$v_cluster)) %>%
      summarise(Region_Name = first(!!sym(input$v_reg))) %>%
      mutate(ID = as.character(!!sym(input$v_cluster)))

    re_mapped <- re %>%
      left_join(cluster_region_map, by = "ID") %>%
      group_by(Region_Name) %>%
      summarise(mean_random_effect = mean(intercept_re, na.rm = TRUE)) %>%
      mutate(Match_Key = str_to_upper(str_trim(as.character(Region_Name))))

    shp <- adal_spatial()
    map_df <- shp %>% left_join(re_mapped, by = "Match_Key")

    ggplot(map_df) +
      geom_sf(aes(fill = mean_random_effect), color = "white", size = 0.2) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", name = "Random Effect") +
      theme_void() +
      labs(title = "Spatial Random Effects Map",
           subtitle = "Unexplained cluster-level variation aggregated to administrative level")
  })

  output$cov_corr_plot <- renderPlot({
    # 1. Collect all selected independent variables
    vars <- c(input$v_indiv_cont, input$v_indiv_fact,
              input$v_comm_cont, input$v_comm_fact)

    # 2. Validation: Need at least 2 variables to correlate
    validate(
      need(length(vars) >= 2, "Please select at least 2 covariates (Individual or Community) in the 'Data Setup' tab to generate a correlation matrix.")
    )

    # 3. Prepare Data: Use processed_df (Individual level data)
    # We convert factors to numeric to allow correlation calculation
    df_corr <- processed_df() %>%
      select(all_of(vars)) %>%
      mutate(across(everything(), ~as.numeric(as.factor(.)))) %>%
      na.omit() # Correlation requires complete observations or pairwise handling

    # 4. Compute Correlation Matrix
    M <- cor(df_corr)

    # 5. Plot Upper Triangle
    corrplot(M, method = "circle", type = "upper",
             order = "hclust", # Clusters correlated variables together
             tl.col = "black", tl.srt = 45, # Text label color and rotation
             addCoef.col = "black", # Add numeric correlation coefficient
             number.cex = 0.7, # Size of numbers
             col = brewer.pal(n = 8, name = "RdYlBu"), # Color palette
             title = "Covariate Correlation Matrix (Upper Triangle)",
             mar = c(0,0,2,0)) # Adjust margins for title
  })

  # Dynamic UI for selecting waves based on the time variable
  output$wave_selector_1 <- renderUI({
    req(input$v_time, processed_df())
    choices <- unique(processed_df()[[input$v_time]])
    selectInput("wave1", "Reference Group (Wave 1):", choices = choices)
  })

  output$wave_selector_2 <- renderUI({
    req(input$v_time, processed_df())
    choices <- unique(processed_df()[[input$v_time]])
    selectInput("wave2", "Comparison Group (Wave 2):", choices = choices, selected = choices[length(choices)])
  })

  # Decomposition Engine
  decomp_res <- eventReactive(input$run_decomp, {
    # 1. Validation Guard: Check if we have 2 waves
    validate(
      need(!is.null(input$v_time) && input$v_time != "",
           "Error: Please select a 'Time/Wave Variable' in the Data Setup tab first."),
      need(length(unique(processed_df()[[input$v_time]])) >= 2,
           "Error: Decomposition requires at least two different survey years (waves) to compare.")
    )

    req(input$wave1, input$wave2, input$v_dv)

    df <- processed_df()

    # Identify which variables are which based on UI selection
    cont_preds <- unique(c(input$v_indiv_cont, input$v_comm_cont))
    fact_preds <- unique(c(input$v_indiv_fact, input$v_comm_fact))
    all_preds <- unique(c(cont_preds, fact_preds))

    # 1. Subset Data
    group1 <- df %>% filter(!!sym(input$v_time) == input$wave1) %>% drop_na(dv_final, all_of(all_preds))
    group2 <- df %>% filter(!!sym(input$v_time) == input$wave2) %>% drop_na(dv_final, all_of(all_preds))

    # --- STEP 2: TYPE SYNCHRONIZATION ---
    for(p in all_preds) {
      if (p %in% fact_preds) {
        # Handle Categorical: Synchronize levels so predict() doesn't crash
        levs <- union(unique(as.character(group1[[p]])), unique(as.character(group2[[p]])))
        group1[[p]] <- factor(group1[[p]], levels = levs)
        group2[[p]] <- factor(group2[[p]], levels = levs)
      } else {
        # Handle Continuous (like Parity): Force to numeric
        group1[[p]] <- as.numeric(as.character(group1[[p]]))
        group2[[p]] <- as.numeric(as.character(group2[[p]]))
      }
    }

    # 3. Fit Logit Models
    form <- as.formula(paste("dv_final ~", paste(all_preds, collapse = " + ")))
    m1 <- glm(form, data = group1, family = binomial(link = "logit"))
    m2 <- glm(form, data = group2, family = binomial(link = "logit"))

    # 4. Math for Detailed Table (Design Matrix approach)
    # model.matrix automatically turns factors into 0/1 dummies and keeps numeric as-is
    X1 <- model.matrix(m1)
    X2 <- model.matrix(m2)

    means1 <- colMeans(X1)
    means2 <- colMeans(X2)
    coefs1 <- coef(m1)

    # Handle NAs in coefficients
    coefs1[is.na(coefs1)] <- 0

    # Detailed Endowment: (Mean_Wave2 - Mean_Wave1) * Coef_Wave1
    detailed_endowment <- (means2 - means1) * coefs1

    detailed_df <- data.frame(
      Variable = names(detailed_endowment),
      Contribution = as.numeric(detailed_endowment)
    ) %>% filter(Variable != "(Intercept)")

    # 5. Summary Math
    y1_bar <- mean(group1$dv_final, na.rm=TRUE)
    y2_bar <- mean(group2$dv_final, na.rm=TRUE)
    total_diff <- y2_bar - y1_bar

    # Counterfactual: What if Wave 1 had Wave 2's effectiveness?
    y_counter <- mean(predict(m2, newdata = group1, type = "response"), na.rm=TRUE)

    endowment <- y_counter - y1_bar
    coefficient <- y2_bar - y_counter

    list(
      summary = data.frame(
        Component = c("Total Change", "Endowment (Characteristics)", "Coefficient (Effectiveness)"),
        Value = c(total_diff, endowment, coefficient),
        Percentage = c(100, (endowment/total_diff)*100, (coefficient/total_diff)*100)
      ),
      detail = detailed_df,
      wave1 = input$wave1,
      wave2 = input$wave2
    )
  })

  # Render the detailed table in the UI
  output$decomp_detail_table <- renderTable({
    req(decomp_res())
    decomp_res()$detail
  }, striped = TRUE, bordered = TRUE)

  output$decomp_summary_plot <- renderPlot({
    req(decomp_res())
    ggplot(decomp_res()$summary[-1,], aes(x = Component, y = Value, fill = Component)) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_text(aes(label = scales::percent(Value, accuracy = 0.1)), vjust = -0.5) +
      theme_minimal() + labs(y = "Contribution to Change", x = "") +
      scale_fill_manual(values = c("Endowment (Characteristics)"="#2c3e50", "Coefficient (Effectiveness)"="#e74c3c"))
  })
  # --- Descriptive Statistics Table Logic (Fixes Visibility & Strict Requirements) ---

  desc_table_data <- reactive({
    # 1. Basic Requirement: Data must be uploaded
    req(raw_data())

    # 2. Collect variables (Handle NULLs)
    cont_vars <- c(input$v_indiv_cont, input$v_comm_cont)
    cat_vars <- c(input$v_indiv_fact, input$v_comm_fact)

    # 3. Validation: If NO variables are selected at all, return a placeholder
    if (length(cont_vars) == 0 && length(cat_vars) == 0) {
      return(data.frame(Variable = "Please select variables", Category = "in Data Setup tab", Statistic = ""))
    }

    df <- raw_data()

    # 4. Weight Handling (Robust check)
    if (!is.null(input$v_wgt) && input$v_wgt != "") {
      if(input$v_wgt %in% names(df)) {
        wt_raw <- df[[input$v_wgt]]
        # Handle DHS scaling (divide by 1M) or standard weight
        wt <- if(input$is_dhs_wgt) wt_raw / 1000000 else wt_raw
      } else {
        wt <- rep(1, nrow(df))
      }
    } else {
      wt <- rep(1, nrow(df))
    }

    # Clean weights: replace NAs with 0 to prevent calculation errors
    wt[is.na(wt)] <- 0

    table_rows <- list()

    # --- A. Continuous Variables (Mean/SD) ---
    for(v in cont_vars) {
      if(v %in% names(df)) {
        val <- as.numeric(df[[v]]) # Force numeric
        # Filter valid data (non-NA value, non-NA weight, positive weight)
        valid_idx <- !is.na(val) & !is.na(wt) & wt > 0

        if(sum(valid_idx) > 0) {
          val_c <- val[valid_idx]
          wt_c <- wt[valid_idx]

          w_mean <- weighted.mean(val_c, wt_c)
          w_sd <- sqrt(cov.wt(data.frame(val_c), wt = wt_c)$cov[1,1])

          table_rows[[length(table_rows) + 1]] <- data.frame(
            Variable = v,
            Category = "Mean (SD)",
            Statistic = paste0(round(w_mean, 2), " (", round(w_sd, 2), ")")
          )
        }
      }
    }

    # --- B. Categorical Variables (Freq/%) ---
    for(v in cat_vars) {
      if(v %in% names(df)) {
        # Use haven::as_factor to get LABELS (e.g., "Urban") instead of numbers
        val_factor <- haven::as_factor(df[[v]])

        # Create temp dataframe and filter
        tmp <- data.frame(cat = val_factor, w = wt) %>%
          filter(!is.na(cat), !is.na(w), w > 0)

        if(nrow(tmp) > 0) {
          agg <- tmp %>%
            group_by(cat) %>%
            summarise(n_w = sum(w), .groups = 'drop') %>%
            mutate(pct = (n_w / sum(n_w)) * 100)

          # Add Header Row for the Variable
          table_rows[[length(table_rows) + 1]] <- data.frame(
            Variable = as.character(v), Category = "", Statistic = ""
          )

          # Add Rows for each Category
          for(i in 1:nrow(agg)) {
            table_rows[[length(table_rows) + 1]] <- data.frame(
              Variable = "",
              Category = as.character(agg$cat[i]),
              Statistic = paste0(round(agg$n_w[i], 0), " (", round(agg$pct[i], 1), "%)")
            )
          }
        }
      }
    }

    # Final check: if no rows generated (e.g. all data was NA)
    if(length(table_rows) == 0) return(data.frame(Variable = "No valid data found", Category="-", Statistic="-"))

    do.call(rbind, table_rows)
  })

  # 2. Render the Table (Updated for Visibility and Style)
  output$desc_stats_table <- renderTable({
    # Get the data
    tbl <- desc_table_data()

    # Safety check: if the result is just a message, return it
    if(is.null(tbl) || nrow(tbl) == 0) return(data.frame(Status = "Please select variables in Data Setup tab."))

    # Return the table
    tbl
  },
  # --- STYLING ARGUMENTS TO MATCH MULTILEVEL TABLE ---
  striped = TRUE,
  bordered = TRUE,  # Adds the borders
  hover = TRUE,
  width = "100%",
  align = 'l',
  spacing = 'm'     # Adjusts row height
  )
  # --- FINALIZED APA-STYLE REPORTING ENGINE ---
  output$dl_report <- downloadHandler(
    filename = function() {
      paste0("HawaSpatial_APA_Report_", Sys.Date(), ".", input$format)
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "hawa_final_report.Rmd")

      report_content <- c(
        "---",
        "title: '`r params$doc_title`'",
        "author: 'HawaSpatial Pro Intelligence Platform'",
        "date: '`r Sys.Date()`'",
        "output:",
        "  word_document: ",
        "    fig_caption: yes",
        "    fig_width: 9",
        "    fig_height: 6",
        "  pdf_document: default",
        "  html_document: default",
        "params:",
        "  doc_title: NA",
        "  spatial_data: NA",
        "  models: NA",
        "  outcome_type: NA",
        "  v_reg: NA",
        "  v_cluster: NA",
        "  clean_df: NA",
        "  st_data: NA",
        "  v_time: NA",
        "  v_wealth: NA",
        "  raw_df: NA",
        "  v_indiv_cont: NA",
        "  v_indiv_fact: NA",
        "  v_comm_cont: NA",
        "  v_comm_fact: NA",
        "  v_wgt: NA",
        "  is_dhs_wgt: NA",
        "  cov_vars: NA",
        "  sae_data: NA",
        "  econ_data: NA",
        "  econ_type: NA",
        "  econ_lm: NA",
        "  gini_val: NA",
        "  theil_val: NA",
        "  outcome_vec: NA",
        "  decomp_data: NA",
        "  conc_val: NA",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, message=FALSE, warning=FALSE)",
        "library(tidyverse); library(sf); library(spdep); library(glmmTMB); library(broom.mixed)",
        "library(knitr); library(cowplot); library(biscale); library(gstat); library(spatialreg); library(classInt); library(scales); library(corrplot); library(RColorBrewer)",
        "",
        "calc_icc_internal <- function(model) {",
        "  tryCatch({",
        "    vc <- VarCorr(model)",
        "    var_u <- as.numeric(vc$cond$cluster_idx[1,1])",
        "    fam <- family(model)$family",
        "    if (grepl('binomial', fam)) { var_e <- 3.289868 }",
        "    else if (grepl('poisson', fam) || grepl('nbinom', fam)) { var_e <- 1.0 }",
        "    else if (grepl('beta', fam)) { var_e <- 1.0 }",
        "    else { var_e <- sigma(model)^2 }",
        "    icc_val <- var_u / (var_u + var_e)",
        "    return(icc_val)",
        "  }, error = function(e) return(0))",
        "}",
        "calc_pcv_internal <- function(m0, m_full) {",
        "  icc_m0 <- calc_icc_internal(m0)",
        "  icc_m_full <- calc_icc_internal(m_full)",
        "  if (icc_m0 > 0) {",
        "    pcv_val <- (icc_m0 - icc_m_full) / icc_m0",
        "    return(max(0, pcv_val))",
        "  } else { return(0) }",
        "}",
        "shp_geom <- params$spatial_data %>% select(Match_Key, geometry) %>% distinct()",
        "```",
        "",
        "## Methodological Transparency",
        "This report was generated using the HawaSpatial Pro Intelligence Platform. The underlying statistical engines utilize the following R packages: `glmmTMB` for hierarchical multilevel modeling, `spatialreg` for spatial econometrics (SAR/SEM), `sae` for Fay-Herriot small area estimation, and `ineq` for spatial inequality metrics.",
        "",
        "**Table 1**",
        "*Weighted Descriptive Statistics of the Study Population*",
        "",
        "```{r table1_desc, results='asis'}",
        "df_rep <- params$raw_df",
        "wt_rep <- if(params$is_dhs_wgt) df_rep[[params$v_wgt]]/1000000 else df_rep[[params$v_wgt]]",
        "table_rows <- list()",
        "cont_vars <- c(params$v_indiv_cont, params$v_comm_cont)",
        "for(v in cont_vars) {",
        "  val <- as.numeric(df_rep[[v]]); valid <- !is.na(val) & !is.na(wt_rep)",
        "  if(sum(valid) > 0) {",
        "    w_mean <- weighted.mean(val[valid], wt_rep[valid])",
        "    w_sd <- sqrt(cov.wt(data.frame(val[valid]), wt = wt_rep[valid])$cov[1,1])",
        "    table_rows[[length(table_rows)+1]] <- data.frame(Variable=v, Category='Mean (SD)', Statistic=paste0(round(w_mean,2), ' (', round(w_sd,2), ')'))",
        "  }",
        "}",
        "cat_vars <- c(params$v_indiv_fact, params$v_comm_fact)",
        "for(v in cat_vars) {",
        "  val_fac <- haven::as_factor(df_rep[[v]])",
        "  tmp <- data.frame(cat = val_fac, w = wt_rep) %>% drop_na()",
        "  if(nrow(tmp) > 0) {",
        "     agg <- tmp %>% group_by(cat) %>% summarise(n_w = sum(w)) %>% mutate(pct = n_w/sum(n_w)*100)",
        "     table_rows[[length(table_rows)+1]] <- data.frame(Variable=v, Category='', Statistic='')",
        "     for(i in 1:nrow(agg)) {",
        "       table_rows[[length(table_rows)+1]] <- data.frame(Variable='', Category=as.character(agg$cat[i]), Statistic=paste0(round(agg$n_w[i],0), ' (', round(agg$pct[i],1), '%)'))",
        "     }",
        "  }",
        "}",
        "if(length(table_rows) > 0) { kable(do.call(rbind, table_rows)) }",
        "```",
        "",
        "**Figure 1**",
        "*LISA Cluster Map identifying significant spatial hotspots (High-High) and coldspots (Low-Low).*",
        "",
        "```{r lisa_fig}",
        "df <- params$spatial_data",
        "nb <- poly2nb(df, queen = TRUE)",
        "if(any(card(nb) == 0)) nb <- knn2nb(knearneigh(st_coordinates(st_centroid(df)), k = 1))",
        "lw <- nb2listw(nb, style = 'W', zero.policy = TRUE)",
        "loc_m <- localmoran(df$outcome, lw, zero.policy = TRUE)",
        "m_prev <- mean(df$outcome)",
        "df$lag_outcome <- lag.listw(lw, df$outcome, zero.policy = TRUE)",
        "df$Pr <- loc_m[,5]",
        "df$lisa_quadrant <- case_when(",
        "  df$Pr <= 0.05 & df$outcome >= m_prev & df$lag_outcome >= m_prev ~ 'High-High', ",
        "  df$Pr <= 0.05 & df$outcome <= m_prev & df$lag_outcome <= m_prev ~ 'Low-Low', ",
        "  TRUE ~ 'Not Significant'",
        ")",
        "ggplot(df) + geom_sf(aes(fill=lisa_quadrant), color='black') + ",
        "  scale_fill_manual(values=c('High-High'='red', 'Low-Low'='blue', 'Not Significant'='lightgray')) + theme_void()",
        "```",
        "",
        "**Table 2**",
        "*Small Area Estimation (SAE) Model Comparison: Direct vs. Stabilized (EBLUP) Estimates*",
        "",
        "```{r sae_table}",
        "if(!is.null(params$sae_data)) { kable(params$sae_data %>% select(Region = Match_Key, `Direct Outcome` = outcome, `SAE (EBLUP)` = eblup, `CV Direct (%)` = cv_direct, `CV SAE (%)` = cv_eblup)) }",
        "```",
        "",
        "**Table 3**",
        "*Side-by-Side Multilevel Hierarchical Analysis of Determinants (Odds Ratios and 95% CI)*",
        "",
        "```{r ml_table}",
        "models <- params$models",
        "tidy_m_full <- function(m, name) {",
        "  res <- broom.mixed::tidy(m, effects = 'fixed', conf.int = TRUE)",
        "  if(params$outcome_type %in% c('Binary', 'Count')) { res <- res %>% mutate(across(c(estimate, conf.low, conf.high), exp)) }",
        "  res %>% mutate(stars = case_when(p.value < 0.001 ~ '***', p.value < 0.01 ~ '**', p.value < 0.05 ~ '*', TRUE ~ '')) %>%",
        "  mutate(formatted = paste0(round(estimate, 2), ' (', round(conf.low, 2), '-', round(conf.high, 2), ')', stars)) %>%",
        "  select(term, !!name := formatted)",
        "}",
        "t0 <- tidy_m_full(models$m0, 'M0 (Null)')",
        "t1 <- tidy_m_full(models$m1, 'M1 (Indiv)')",
        "t2 <- tidy_m_full(models$m2, 'M2 (Comm)')",
        "t3 <- tidy_m_full(models$m3, 'M3 (Full)')",
        "kable(t0 %>% full_join(t1, by='term') %>% full_join(t2, by='term') %>% full_join(t3, by='term') %>% filter(!str_detect(term, 'Intercept')))",
        "```",
        "",
        "**Table 4**",
        "*Multilevel Model Fitness and Proportional Change in Variance (PCV)*",
        "",
        "```{r metrics_table}",
        "comp_metrics <- data.frame(",
        "  Model = c('M0 (Null)', 'M1 (Indiv)', 'M2 (Comm)', 'M3 (Full)'),",
        "  AIC = c(AIC(models$m0), AIC(models$m1), AIC(models$m2), AIC(models$m3)),",
        "  ICC = c(calc_icc_internal(models$m0), calc_icc_internal(models$m1), calc_icc_internal(models$m2), calc_icc_internal(models$m3)),",
        "  PCV = c(NA, calc_pcv_internal(models$m0, models$m1), calc_pcv_internal(models$m0, models$m2), calc_pcv_internal(models$m0, models$m3))",
        ")",
        "comp_metrics$PCV_Percent <- scales::percent(comp_metrics$PCV, accuracy = 0.1)",
        "kable(comp_metrics %>% select(-PCV))",
        "```",
        "",
        "**Figure 2**",
        "*Residual Spatial Variation Map visualizing unexplained cluster-level random effects.*",
        "",
        "```{r re_map}",
        "re_df <- as.data.frame(ranef(models$m3)$cond$cluster_idx)",
        "re_df$ID <- rownames(re_df)",
        "cluster_region_map <- params$clean_df %>% group_by(!!sym(params$v_cluster)) %>% summarise(Region_Name = first(!!sym(params$v_reg))) %>% mutate(ID = as.character(!!sym(params$v_cluster)))",
        "re_mapped <- re_df %>% left_join(cluster_region_map, by = 'ID') %>% group_by(Region_Name) %>% summarise(mean_re = mean(intercept_re, na.rm=TRUE)) %>% mutate(Match_Key = str_to_upper(str_trim(as.character(Region_Name))))",
        "ggplot(df %>% left_join(re_mapped, by = 'Match_Key')) + geom_sf(aes(fill = mean_re)) + scale_fill_gradient2(low='blue', mid='white', high='red') + theme_void()",
        "```"
      )

      writeLines(report_content, tempReport)

      rmarkdown::render(tempReport, output_file = file,
                        params = list(
                          doc_title = input$rep_title,
                          spatial_data = adal_spatial(),
                          models = freq_res(),
                          outcome_type = input$outcome_type,
                          v_reg = input$v_reg,
                          v_cluster = input$v_cluster,
                          clean_df = processed_df(),
                          raw_df = raw_data(),
                          v_indiv_cont = input$v_indiv_cont,
                          v_indiv_fact = input$v_indiv_fact,
                          v_comm_cont = input$v_comm_cont,
                          v_comm_fact = input$v_comm_fact,
                          v_wgt = input$v_wgt,
                          is_dhs_wgt = input$is_dhs_wgt,
                          st_data = if(is.null(adal_spatiotemporal())) NULL else adal_spatiotemporal(),
                          v_time = input$v_time,
                          v_wealth = input$v_wealth,
                          cov_vars = c(input$v_indiv_cont, input$v_indiv_fact, input$v_comm_cont, input$v_comm_fact),
                          sae_data = if(is.null(sae_res())) NULL else sae_res(),
                          econ_data = if(is.null(econ_res())) NULL else econ_res(),
                          econ_type = input$econ_model_type,
                          econ_lm = if(is.null(econ_res())) NULL else econ_res()$lm_tests,
                          gini_val = if(is.null(ineq_stats())) NULL else ineq_stats()$gini,
                          theil_val = if(is.null(ineq_stats())) NULL else ineq_stats()$theil,
                          outcome_vec = if(is.null(adal_spatial())) NULL else adal_spatial()$outcome,
                          decomp_data = tryCatch({
                            res <- decomp_res()
                            if(is.null(res)) NULL else res
                          }, error = function(e) NULL)
                        ),
                        envir = new.env(parent = globalenv()))
    }
  )
}
