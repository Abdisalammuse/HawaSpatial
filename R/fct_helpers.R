# --- Helper Function: Robust ICC for glmmTMB (Used by Shiny UI) ---
calc_freq_icc <- function(model) {
  tryCatch({
    # 1. Extract Cluster Variance (Tau^2)
    # glmmTMB stores VarCorr in a nested list structure $cond$cluster_idx
    vc <- VarCorr(model)
    var_u <- as.numeric(vc$cond$cluster_idx[1,1]) # Extract scalar variance

    # 2. Determine Residual Variance (Sigma^2) based on Family
    fam <- family(model)$family

    if (grepl("binomial", fam)) {
      var_e <- 3.289868
    } else if (grepl("poisson", fam) || grepl("nbinom", fam)) {
      var_e <- 1.0
    } else if (grepl("beta", fam)) {
      var_e <- 1.0
    } else {
      var_e <- sigma(model)^2
    }

    # 3. Calculate ICC
    icc_val <- var_u / (var_u + var_e)

    if (is.na(icc_val) || is.null(icc_val)) return(0)
    return(icc_val)

  }, error = function(e) {
    # If anything fails, return 0 (but print error to console for debugging)
    print(paste("ICC Error:", e$message))
    return(0)
  })
}

# --- Helper Function: Calculate PCV (Proportional Change in Variance) ---
calc_pcv <- function(m0, m_full) {
  icc_m0 <- calc_freq_icc(m0)
  icc_m_full <- calc_freq_icc(m_full)
  if (icc_m0 > 0) {
    pcv_val <- (icc_m0 - icc_m_full) / icc_m0
    return(max(0, pcv_val))
  } else { return(0) }
}
