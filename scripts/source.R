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

## FUN: any RCP assigned for less than "N" sites?
FUN_lessNsites <- function(x, n) {
  # Apply 'FUN_max1else0' by row, and transpose the matrix to correct format
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

## FUN: Partial plots 
## ----- Function written by Matthew R. Schofield
## ----- Department of Mathematics and Statistics, University of Otago
FUN_partial_plot = function(beta.mat, 
                            predplot = c(NA,rep(0,ncol(beta.mat)-2)), 
                            xlm.real = c(-2,2), 
                            stand.vals = c(0,1), 
                            n = 1001, 
                            xlb = "", 
                            ylb = "Probability", 
                            location = "topright", 
                            coluse = palette()[1:(nrow(beta.mat)+1)],
                            model = c("pres_abs", "abund"), 
                            season = c("summer", "autumn", "winter", "spring")){
  ## beta.mat is a matrix of beta values.  This is obtained from coef(model)
  ## predplot is a vector (dimension = # of predictors).  The predictor with NA is that plotted.  The value for all other predictors gives the fixed value of that predictor.
  ## predplot = c(NA,0) plots the 1st predictor, setting the value of the 2nd predictor to 0.
  ## xlm.real is a vector of length 2.  It gives the range over which to vary the x value (on the original scale).
  ## stand.vals is a vector of length 2 that gives the values that define the standardization of the predictor values.  The first argument is the mean.  The second is the standard deviation.
  ## n is the number of points at which to plot the function (it should be large enough to ensure the lines are smooth)
  ## xlb is the x label
  ## ylb is the y label
  ## location gives the location of the legend (?legend for options)
  ## coluse is a vector giving the colours of the profiles (defaults to first 3 colours in the palette)
  ## model is the type of the specified model -- only needed because for a specific model, we need to swap colours/labels
  ## season is respective season -- only needed because for a specific model, we need to swap colours/labels
  
  npar = ncol(beta.mat)  # number of parameters (includes intercept)
  nprof = nrow(beta.mat)+1 # number of profiles to plot
  
  beta.mat = t(rbind(beta.mat,rep(0,npar)))  # this is a work around to simplify the calculation of the inverse logit function
  
  xlm.stand = (xlm.real - stand.vals[1])/stand.vals[2]
  xuse = seq(from = xlm.stand[1], to = xlm.stand[2], length.out = n) # specifying the range of the x values we are plotting over
  xuse.real = xuse*stand.vals[2] + stand.vals[1]
  
  ## the following code sets up the x matrix (at each point)
  xmat = matrix(NA,nrow = n, ncol = npar)
  xmat[,1] = 1 # intercept
  for(j in 1:(npar-1)){
    if(is.na(predplot[j])){
      xmat[,j+1] = xuse
    } else {
      xmat[,j+1] = predplot[j]
    }
  }
  
  ## use matrix algebra to "easily" calculate the inverse logit
  eta.mat = exp(xmat %*% beta.mat)
  sum.mat = matrix(rowSums(eta.mat),n,nprof)
  prob.mat = eta.mat/sum.mat
  
  if(model == "abund" & season == "spring"){
    prob.mat <- prob.mat[nrow(prob.mat):1, ]
  }
  
  ## plot the output
  plot(NA,NA,xlim = xlm.real, ylim = c(0,1), xlab = toupper(xlb), ylab = ylb,
       cex.lab = 1.5, cex.axis = 1)
  for(j in 1:nprof){
    lines(xuse.real, prob.mat[,j], col = coluse[j], lwd = 4)
  }
  legend(location, legend = paste("RCP", 1:nprof), col = coluse, lty = 1, bty = "n")
  
}


