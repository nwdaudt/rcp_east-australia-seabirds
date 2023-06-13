## Nicholas W Daudt - 2022

#### ----------------- Helper functions for plotting RCPs -------------------- #

# FUN: max value in row to 1 and others to 0
FUN_max1else0 <- function(x) {
  # All values other than the max to 0
  x <- replace(x, x < max(x), 0)
  # Then, 'max' values to 1
  x <- replace(x, x > 0, 1)
  return(x)
}

# FUN: any RCP assigned for less than "N" sites?
FUN_lessNsites <- function(x, n) {
  # Apply 'fun_max1_else0' by row, and transpose the matrix to correct format
  b <- t(apply(X = x, MARGIN = 1, FUN = FUN_max1else0))
  # colSum values of the matrix (cols are RCP groups), and if <=n replace with 1
  c <- replace((colSums(b) <= n), (colSums(b) <= n) == TRUE, 1)
  # sum to get the number of columns (RCP groups) with <=n sites allocated
  d <- sum(c)
  return(d)
}

## FUN: Probability maps for RCPs
FUN_prob_maps <- function(df = pred_data, 
                          basemap = aus_base_map, 
                          column_names = c("RCP_1", "RCP_2", "RCP_3")){
  
  # Required libraries
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(sf)
  library(classInt)
  library(colorspace)
  
  df_plots <- data.frame()
  
  for (column_name in column_names){
    
    # print(column_name)
    
    # Define additional column names based on column_name
    upper_col = paste0(column_name, ".upper")
    lower_col = paste0(column_name, ".lower")
    
    # Define fixed breaks
    brksRCP_m <- 
      classInt::classIntervals(df[[column_name]], 
                               n = 10, style = "fixed", 
                               fixedBreaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
    brksRCP_u <- 
      classInt::classIntervals(df[[upper_col]], 
                               n = 10, style = "fixed", 
                               fixedBreaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
    brksRCP_l <- 
      classInt::classIntervals(df[[lower_col]], 
                               n = 10, style = "fixed", 
                               fixedBreaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
    
    ## Generate data.frame to be used for creating final plots
    cols_to_select <- c(as.character(column_name), 
                        as.character(upper_col), 
                        as.character(lower_col), "geometry")
    
    plotRCP <- 
      df %>% 
      dplyr::select(all_of(cols_to_select)) %>% 
      dplyr::mutate(breaks_m = cut(.data[[column_name]], brksRCP_m$brks),
                    breaks_u = cut(.data[[upper_col]], brksRCP_u$brks),
                    breaks_l = cut(.data[[lower_col]], brksRCP_l$brks))
    
    plotRCP <- 
      plotRCP %>% 
      tidyr::pivot_longer(cols = all_of(c("breaks_m", "breaks_u", "breaks_l")),
                          names_to = "break_col",
                          values_to = "value") %>%
      dplyr::select(break_col, value, geometry) %>%
      dplyr::mutate(rcp_grp = as.character(column_name))
    
    plotRCP$rcp_grp <- gsub(pattern = "_", replacement = "", x = plotRCP$rcp_grp)
    plotRCP$rcp_grp <- factor(plotRCP$rcp_grp, levels = c("RCP1", "RCP2", "RCP3"))
    
    plotRCP$break_col <- factor(plotRCP$break_col, 
                                levels = c("breaks_l", "breaks_m", "breaks_u"),
                                labels = c("Lower CI", "Mean", "Upper CI"))
    
    # Store for plotting below
    df_plots <- rbind(df_plots, plotRCP)
    
    rm("brksRCP_l", "brksRCP_m", "brksRCP_u",
       "cols_to_select", "upper_col", "lower_col",
       "plotRCP")
    gc()
  }
  
  rcp_plot <- 
    basemap + 
    geom_sf(data = na.omit(df_plots), aes(fill = value), size = 0.15) + 
    coord_sf(xlim = c(140, 161), ylim = c(-9, -48)) + 
    scale_fill_discrete_sequential(palette = "Sunset", rev = FALSE) +
    scale_x_continuous(breaks = c(140, 150, 160)) +
    facet_grid(rows = vars(rcp_grp), cols = vars(break_col)) +
    guides(fill = guide_legend("Probability")) +
    theme(axis.text = element_text(size = 6),
          axis.text.x.bottom = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 8),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          legend.key.size = unit(0.5, "cm"))
  
  return(rcp_plot)
}