
# ---- Function to plot the graphs ---- #

plot_power_analysis <- function(data) {
  
  ggplot(data, aes(x = sample_sizes)) +
    # plot power for each treatment effect
    geom_line(aes(y = power_effect1, color = paste0('Effect = ', taus[1])), size = 2) +
    geom_line(aes(y = power_effect2, color = paste0('Effect = ', taus[2])), size = 2) +
    geom_line(aes(y = power_effect3, color = paste0('Effect = ', taus[3])), size = 2) +
    # creates horizontal line at power = 0.8
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
    ylim(0, 1) +
    scale_x_continuous(breaks = seq(0, max(sample_sizes), by = 1000)) +
    labs(title = paste0("Power Analysis"),
         x     = "Sample sizes",
         y     = "Power") +
    theme_classic() + 
    scale_color_manual(name   = '',
                       values = colors,
                       labels = c(paste0('Effect = ', taus[1]),
                                  paste0('Effect = ', taus[2]),
                                  paste0('Effect = ', taus[3]))) +
    theme(legend.position = 'bottom',
          legend.title    = element_blank(),
          legend.text     = element_text(size = 16),
          plot.title      = element_text(size = 24, hjust = 0.5),
          axis.title      = element_text(size = 20),
          axis.text       = element_text(size = 16)) +
    guides(color = guide_legend(override.aes = list(size = 3)))
  
}