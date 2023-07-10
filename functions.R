#-----------------------------------------------------------------------------
#
# Analysis of extended illness-death model
# 
# 0: admission
# 1: intermediate event 
# 2: discharge w/o intermediate event 
# 3: death w/o intermediate event 
# 4: discharge after intermediate event 
# 5: death after intermediate event 
#
# Function input:
#   aggregated transition hazards
# 
# Function output:
#   transition probabilities
#   AM, PAF
#   cLOS
#
# returned Plots:
# AM, PAF
# Stacked plots:
#   eidm
#
# structure of code:
# 
# define two functions:
#   1) transition_probs: calculation of Pij, PAF, AM, clos
#   2) stacked_plot_eidm: create stacked plots (use results of 1)
#  
# -> defining function combining previous functions
#
#-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------
# (1)
# Cube, M., Schumacher, M., Wolkewitz, M. (2017). Basic parametric analysis for a multi-state
# model in hospital epidemiology. Pages: 3-5. In: BMC Meidcal Research Methodology.

# (2)
# Cube, M., Schumacher, M., Wolkewitz, M. (2017). Multistate Modeling to Analyze Nosocomial
# Infection Data: An Introduction and Demonstration. Pages: 955-956.
# In: infection control & hospital epidemiology.
#
#-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------
# transition probability function for eidm
#-----------------------------------------------------------------------------

#---------------
#  Function 1.
#---------------

transition_prob <- function(t,
                            Lambda_01,
                            Lambda_02,
                            Lambda_03,
                            Lambda_14,
                            Lambda_15) {
  
  # Hazard Matrix
  Q <- matrix(0,
              nrow = 6,
              ncol = 6)
  
  Q[1, 2] <- Lambda_01
  Q[1, 3] <- Lambda_02
  Q[1, 4] <- Lambda_03
  Q[2, 5] <- Lambda_14
  Q[2, 6] <- Lambda_15
  
  Q[1, 1] <- -sum(Q[1, ])
  Q[2, 2] <- -sum(Q[2, ])
  
  # Probability Matrix
  P_0J <- expm(Q * t)[1, ]
  
  P00  <- P_0J[1]
  P01  <- P_0J[2]
  P02  <- P_0J[3]
  P03  <- P_0J[4]
  P04  <- P_0J[5]
  P05  <- P_0J[6]
  
  # Divers parameters
  comp_trans         <- P_0J[2] + P_0J[5] + P_0J[6]
  
  comp_dis_noTrans   <- P_0J[3]
  
  comp_death_noTrans <- P_0J[4]
  
  AM_t               <- (P05 / (P01 + P04 + P05)) - (P03 / (P00 + P02 + P03))
  
  PAF_t              <- (((P03 + P05) - (P03 / (P00 + P02 + P03))) / (P03 + P05)  )
  
  dat_c <-  c("time_t"             = t,
              "P00"                = P00,
              "P01"                = P01,
              "P02"                = P02,
              "P03"                = P03,
              "P04"                = P04,
              "P05"                = P05,
              "comp_trans"         = comp_trans,
              "comp_dis_noTrans"   = comp_dis_noTrans,
              "comp_death_noTrans" = comp_death_noTrans,
              "AM_t"               = AM_t,
              "PAF_t"              = PAF_t)
  
  return(dat_c)
  
}

#---------------
#  Function 2.
#---------------

transition_probs <-  function(s     = 0,
                              t_max = 100,
                              Lambda_01,
                              Lambda_02,
                              Lambda_03,
                              Lambda_14,
                              Lambda_15) {
  
  
  
  #create an artificial data.frame to save the computed rows into it:
  dat_df <-  data.frame("time_t"             = s:t_max,
                        "P00"                = s:t_max,
                        "P01"                = s:t_max,
                        "P02"                = s:t_max,
                        "P03"                = s:t_max,
                        "P04"                = s:t_max,
                        "P05"                = s:t_max,
                        "comp_trans"         = s:t_max,
                        "comp_dis_noTrans"   = s:t_max,
                        "comp_death_noTrans" = s:t_max,
                        "AM_t"               = s:t_max,
                        "PAF_t"              = s:t_max)
  
  
  
  # calculation of transition probabilities:
  for (i in seq_along(s:t_max)) {
    
    dat_df[i, ] <- transition_prob(t         = (s:t_max)[i],
                                   Lambda_01 = Lambda_01,
                                   Lambda_02 = Lambda_02,
                                   Lambda_03 = Lambda_03,
                                   Lambda_14 = Lambda_14,
                                   Lambda_15 = Lambda_15)
    
  }
  
  # the results:
  return(dat_df)
  
}




#-----------------------------------------------------------------------------
# stacked plot function for eidm
#-----------------------------------------------------------------------------


stacked_plot_eidm <-  function(s               = 0,
                               t_max           = 100,
                               x_lim           = 100,
                               plot_title      = "Stacked Probability Plot",
                               legend_position = c(0.8, 0.85),
                               order           = c(2, 3, 0, 5, 4, 1), # oder of stacked plot from top to bottom
                               Lambda_01 ,                            # initial - intermediate
                               Lambda_02 ,                            # initial - discharge
                               Lambda_03 ,                            # initial - death
                               Lambda_14 ,                            # intermediate - discharge
                               Lambda_15 ,                            # intermediate - death
                               area_col        = c("khaki1",          # inpatient w/o intermediate
                                                   "indianred1",      # intermediate inpatient
                                                   "cornflowerblue",  # discharge w/o intermediate
                                                   "darkblue",        # death w/o intermediate
                                                   "chocolate",       # discharge after intermediate
                                                   "chocolate4"),     # death after intermediate
                               legend_labels   = c("inpatient w/o intermediate",
                                                   "intermediate",
                                                   "discharge w/o intermediate",
                                                   "death w/o intermediate",
                                                   "discharge after intermediate",
                                                   "death after intermediate")) {  
  
  # transition probabilities
  trans_prob <- transition_probs(s         = 0,
                                 t_max     = t_max,
                                 Lambda_01 = Lambda_01,
                                 Lambda_02 = Lambda_02,
                                 Lambda_03 = Lambda_03,
                                 Lambda_14 = Lambda_14,
                                 Lambda_15 = Lambda_15)

  # stacket probabilities
  line1 <- trans_prob[, paste0("P0", order[6])]
  line2 <- line1 + trans_prob[, paste0("P0", order[5])]
  line3 <- line2 + trans_prob[, paste0("P0", order[4])]
  line4 <- line3 + trans_prob[, paste0("P0", order[3])]
  line5 <- line4 + trans_prob[, paste0("P0", order[2])]
  
  # plot those stacked probabilities
  hui <- trans_prob[trans_prob$time_t >= 0, ] %>%
    ggplot(aes()) +
    ggtitle(plot_title) +
    
    geom_ribbon(fill     = area_col[order[6] + 1],
                linetype = 0,
                mapping  = aes(x      = time_t, 
                               ymin   = 0,
                               ymax   = line1,
                               colour = "1")) +
    
    geom_ribbon(fill     = area_col[order[5] + 1],
                linetype = 0,
                mapping  = aes(x      = time_t,
                               ymin   = line1,
                               ymax   = line2,
                               colour = "2")) +
    
    geom_ribbon(fill     = area_col[order[4] + 1],
                linetype = 0,
                mapping  = aes(x      = time_t,
                               ymin   = line2,
                               ymax   = line3,
                               colour = "3")) +
    
    geom_ribbon(fill     = area_col[order[3] + 1],
                linetype = 0,
                mapping  = aes(x      = time_t,
                               ymin   = line3,
                               ymax   = line4,
                               colour = "4")) +
    
    geom_ribbon(fill     = area_col[order[2] + 1],
                linetype = 0,
                mapping  = aes(x      = time_t,
                               ymin   = line4,
                               ymax   = line5,
                               colour = "5")) +
    
    geom_ribbon(fill     = area_col[order[1] + 1],
                linetype = 0,
                mapping  = aes(x     = time_t,
                               ymin  = line5,
                               ymax  = 1,
                               colour= "6")) +
    
    xlim(0, x_lim) +
    
    theme(legend.position   = legend_position,
          legend.key.size   = unit(3.2, "mm"),
          legend.title      = element_text(size     = 16), #change legend title font size
          legend.text       = element_text(size     = 14),
          legend.background = element_rect(fill     = "grey85",
                                           size     = 1,
                                           linetype = "solid")) +
    
    scale_color_manual(labels = legend_labels[order + 1],
                       values = area_col[order + 1]) +
    
    guides(colour = guide_legend(override.aes = list(fill = area_col[order + 1]))) +
    labs(col="Events")
  
  return_list      <- list()
  return_list[[1]] <- trans_prob
  return_list[[2]] <- hui
  
  return(return_list)
  
}

#-----------------------------------------------------------------------------
# Combine stacked plots, PAF and AM
#-----------------------------------------------------------------------------

eidm_aggregated_hazards <- function(s                    = 0,
                                    t_max                = 100,
                                    x_lim                = 100,
                                    legend_position_eidm = c(0.8, 0.85),
                                    plot_title_eidm      = "Stacked Probability Plot",
                                    order_eidm           = c(2, 3, 0, 5, 4, 1),
                                    Lambda_01,
                                    Lambda_02,
                                    Lambda_03,
                                    Lambda_14,
                                    Lambda_15,
                                    area_col_eidm        = c("cornflowerblue", # discharge w/o intermediate
                                                             "darkblue",       # death w/o intermediate
                                                             "khaki1",         # inpatient w/o intermediate
                                                             "chocolate4",     # death after intermediate
                                                             "chocolate",      # discharge after intermediate
                                                             "indianred1"),    # intermediate inpatient
                                    legend_labels_eidm   = c("discharge w/o intermediate",
                                                             "death w/o intermediate",
                                                             "inpatient w/o intermediate",
                                                             "death after intermediate",
                                                             "discharge after intermediate",
                                                             "intermediate"),
                                    
                                    paf_title = "PAF(t)",
                                    am_title = "AM(t)") {
  
  hui_eidm <- stacked_plot_eidm(s               = 0,
                                t_max           = t_max,
                                x_lim           = x_lim,
                                Lambda_01 , # initial - intermediate
                                Lambda_02 , # initial - discharge
                                Lambda_03 , # initial - death
                                Lambda_14 , # intermediate - discharge
                                Lambda_15 , # intermediate - death
                                order           = order_eidm,
                                area_col        = area_col_eidm,
                                plot_title      = plot_title_eidm,
                                legend_position = legend_position_eidm,
                                legend_labels   = legend_labels_eidm) 
  
  
  #----------
  #  PAF_t
  #----------
  
  p1 <- ggplot(hui_eidm[[1]][hui_eidm[[1]]$time_t > 0,], # at t = 0 PAF is NA as each Pij is zero
               aes(time_t,
                   PAF_t)) +
    
    geom_area(aes(time_t,
                  PAF_t)) +
    
    # geom_hline(yintercept = 0) +
    ggtitle(paf_title) +
    xlim(ifelse(s == 0, 0, s), x_lim)
  
  
  #----------
  #  AM_t
  #----------
  
  
  p2 <- ggplot(hui_eidm[[1]][hui_eidm[[1]]$time_t>0,], # at t = 0 AM is NA as each Pij is zero
               aes(time_t,
                   AM_t)) +
    
    geom_area(aes(time_t,
                  AM_t)) +
    
    # geom_hline(yintercept = 0) +
    xlim(ifelse(s == 0, 0, s), x_lim) +
    ggtitle(am_title)
  
  
  
  results_list      <- list()
  results_list[[1]] <- hui_eidm[[1]]
  results_list[[2]] <- hui_eidm[[2]]
  results_list[[3]] <- p1
  results_list[[4]] <- p2
  
  names(results_list) <- c("transition Probabilities",
                           "stacked plot - eidm",
                           "PAF(t)",
                           "AM(t)")
  
  return(results_list)
  
}

# extract legend in order to obtain one legend.
g_legend <- function(a.gplot){
  
  tmp    <- ggplot_gtable(ggplot_build(a.gplot))
  leg    <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)

}

