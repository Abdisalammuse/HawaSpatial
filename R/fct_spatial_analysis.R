#' Generate ESDA Plots
#'
#' @description This function handles the 16 Exploratory Spatial Data Analysis methods.
#'
#' @param df A spatial dataframe (sf object) containing the outcome and geometry.
#' @param method A string indicating which of the 16 methods to execute.
#'
#' @import ggplot2
#' @import sf
#' @import spdep
#' @import dplyr
#' @importFrom scales percent
#' @importFrom cowplot plot_grid
#' @importFrom classInt classIntervals
#' @importFrom stats dnorm pnorm quantile weighted.mean
#' @importFrom magrittr %>%
#'
#' @export
generate_esda_plot <- function(df, method) {

  # --- INTERNAL SETUP ---
  # Ensure spatial weights are handled correctly within the package namespace
  nb <- spdep::poly2nb(df, queen = TRUE)
  if(any(spdep::card(nb) == 0)) {
    nb <- spdep::knn2nb(spdep::knearneigh(sf::st_coordinates(sf::st_centroid(df)), k = 1))
  }
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

  # --- METHOD LOGIC ---
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
    df$lisa <- case_when(df$Pr <= 0.05 & df$outcome >= m_prev & df$lag_outcome >= m_prev ~ "High-High", df$Pr <= 0.05 & df$outcome <= m_prev & df$lag_outcome <= m_prev ~ "Low-Low", TRUE ~ "Not Sig")
    ggplot(df) + geom_sf(aes(fill=lisa), color="black") + scale_fill_manual(values=c("High-High"="red", "Low-Low"="blue", "Not Sig"="white")) + theme_void()
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
    cor_res <- sp.correlogram(nb, df$outcome, order = 4, method = "I", style = "W", zero.policy = TRUE)
    cor_df <- data.frame(Lag = 1:4, Morans_I = cor_res$res[, 1], P_Value = 2 * (1 - pnorm(abs(cor_res$res[, 1] - cor_res$res[, 2]) / cor_res$res[, 3]))) %>% mutate(Significance = ifelse(P_Value < 0.05, "Significant", "Not Significant"))
    ggplot(cor_df, aes(x = Lag, y = Morans_I)) + geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + geom_line() + geom_point(aes(color = Significance), size = 4) + scale_color_manual(values = c("Significant" = "red", "Not Significant" = "blue")) + labs(title = "11. Spatial Correlogram", y = "Moran's I") + theme_minimal()
  } else if(method == "12. Regime Density") {
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
  } else {
    ggplot(df) + geom_sf(aes(fill=outcome)) + scale_fill_viridis_c() + theme_void()
  }
}

