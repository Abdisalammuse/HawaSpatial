#' @import shiny
#' @import sf
#' @import tidyverse
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardSidebar dashboardBody box jumbotron infoBox infoBoxOutput tabItems tabItem menuItem sidebarMenu callout renderInfoBox
#' @importFrom spatialreg lagsarlm errorsarlm impacts
#' @importFrom spdep poly2nb nb2listw localmoran localG localC geary.test moran.test moran.mc sp.correlogram knearneigh knn2nb card EBlocal lag.listw lm.RStests
#' @importFrom ggplot2 ggplot aes geom_sf geom_sf_text geom_line geom_area geom_vline geom_point geom_text geom_smooth geom_hline geom_density geom_boxplot geom_jitter facet_wrap theme_void theme_minimal theme element_text element_blank labs annotate scale_fill_distiller scale_fill_brewer scale_fill_gradient2 scale_fill_viridis_c scale_color_viridis_c scale_fill_manual scale_color_manual scale_fill_viridis_d
#' @importFrom dplyr mutate across everything filter left_join group_by summarise rename select distinct first group_modify ungroup case_when sym
#' @importFrom tidyr drop_na
#' @importFrom stringr str_to_upper str_trim str_detect
#' @importFrom magrittr %>%
#' @importFrom scales percent
#' @importFrom stats as.formula binomial coef cor cov.wt dnorm family gaussian glm lm model.matrix na.omit pnorm poisson predict quantile residuals sigma var weighted.mean AIC BIC
#' @importFrom tidyselect where all_of
#' @importFrom readr read_csv
#' @import glmmTMB
#' @import performance
#' @import broom.mixed
#' @import broom
#' @import sae
#' @import rmarkdown
#' @import ineq
#' @import classInt
#' @import corrplot
#' @import haven
#' @import lmtest
#' @import survey
#' @import ggrepel
#' @import RColorBrewer
#' @import viridis
#' @import gstat
#' @import biscale
#' @import cowplot
NULL
