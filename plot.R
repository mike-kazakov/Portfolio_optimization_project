library(scales)
library(ggplot2)
library(tidyr)

### Descriptive Statistics ###

# Means
moments_annual <- data.frame(asset = moments$Asset,
                             mean = annualized_mean,
                             sd = annualized_sd)

ggplot(moments_annual, aes(x = asset, y = mean)) +
  geom_bar(stat = "identity", fill = "deepskyblue", color = "blue4") + 
  labs(title = "Annualized mean returns", 
       x = "Asset", 
       y = "Mean return") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),          # Remove grid lines
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set overall plot background to white
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray")
  )

# SD
ggplot(moments_annual, aes(x = asset, y = sd)) +
  geom_bar(stat = "identity", fill = "#FFA7A7", color = "red") + 
  labs(title = "Annualized standard deviations", 
       x = "Asset", 
       y = "Standard deviation") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),          # Remove grid lines
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set overall plot background to white
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray")
  )

# Kurtosis
ggplot(moments, aes(x = Asset, y = kurtosis)) +
  geom_bar(stat = "identity", fill = "orchid1", color = "orchid4") + 
  labs(title = "Kurtosis", 
       x = "Asset", 
       y = "Kurtosis") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),          # Remove grid lines
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set overall plot background to white
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray")
  )

# Skewness
ggplot(moments, aes(x = Asset, y = skewness)) +
  geom_bar(stat = "identity", fill = "orchid1", color = "orchid4") + 
  labs(title = "Skewness", 
       x = "Asset", 
       y = "Skewness") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),          # Remove grid lines
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set overall plot background to white
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray")
  )

# mu-sigma

plot(moments$sd, moments$mean, 
     pch = 19,
     col = "blue",
     xlab = "Standard Deviation", 
     ylab = "Mean Return", 
     xlim = c(0, 0.25),
     ylim = c(0, 0.025),
     main = "mu-sigma plot")


text(moments$sd, moments$mean, 
     labels = moments$Asset, 
     pos = 4,   # Position labels to the right of the points
     cex = 0.5, # Smaller font size for labels
     col = "black") # Label color

grid(col = "lightgrey")


# VaR and max drawdowns
moments_graph <- moments[,c("Asset","VaR","max_drawdown")]

# Create a new column 'drawdown_diff' that is the difference between max_drawdown and VaR
moments_graph$drawdown_diff <- moments_graph$max_drawdown - moments_graph$VaR

# Reshape the data into a long format for stacking
moments_long <- moments_graph %>%
  gather(key = "type", value = "value", c("VaR", "drawdown_diff"))

# Create the stacked bar chart
ggplot(moments_long, aes(x = Asset, y = value, fill = type)) +
  geom_bar(stat = "identity") + 
  labs(title = "Max Drawdowns and VaR", 
       x = "Asset", 
       y = "Drop") +
  scale_fill_manual(values = c("red", "#FFA7A7"),
                    labels = c("VaR" = "Value-at-Risk", "drawdown_diff" = "Max Drawdown")) +
  scale_y_continuous(breaks = seq(-1, 0, by = 0.1)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.title.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank(),          # Remove grid lines
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set overall plot background to white
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray"),
    legend.title = element_blank()
  )


### Efficient frontiers ###

# Unconstrained
plot(unconstrained_results$std_dev, unconstrained_results$mean_return, 
     type = 'l', 
     lwd = 2, # Increased linewidth
     xlab = "Standard Deviation", 
     ylab = "Mean Return", 
     xlim = c(0, 0.25), 
     main = "Unconstrained Efficient Set")

points(moments$sd, moments$mean, 
       col = "blue", 
       pch =  19) # Filled circles for scatterplot

text(moments$sd, moments$mean, 
     labels = moments$Asset, 
     pos = 4,   # Position labels to the right of the points
     cex = 0.5, # Smaller font size for labels
     col = "black") # Label color

grid(col = "lightgrey")

abline(a = rf, b = (tp_short$er - rf) / tp_short$sd, 
       col = "brown3", 
       lwd = 2)

points(tp_short$sd, tp_short$er, 
       bg = "yellow", 
       pch = 24,
       cex = 2) # Triangle symbol

# Constrained
plot(constrained_results$std_dev, constrained_results$mean_return, 
     type = 'l', 
     lwd = 2, # Increased linewidth
     xlab = "Standard Deviation", 
     ylab = "Mean Return", 
     xlim = c(0, 0.25),
     ylim = c(0, 0.025),
     main = "Constrained Efficient Set")

points(moments$sd, moments$mean, 
       col = "blue", 
       pch =  19) # Filled circles for scatterplot

text(moments$sd, moments$mean, 
     labels = moments$Asset, 
     pos = 4,   # Position labels to the right of the points
     cex = 0.4, # Smaller font size for labels
     col = "black") # Label color

grid(col = "lightgrey")

abline(a = rf, b = (tp_long$er - rf) / tp_long$sd, 
       col = "brown3", 
       lwd = 2)

points(tp_long$sd, tp_long$er, 
       bg = "yellow", 
       pch = 24,
       cex = 2) # Triangle symbol


### Performance of tangency portfolios ###
ggplot(data = tg_overall, aes(x = date)) +
  geom_line(aes(y = TG_long, color = "Tangency long"), size = 1) +
  geom_line(aes(y = EW, color = "Equally weighted"), size = 1) +
  geom_line(aes(y = TG_short, color = "Tangency short"), size = 1) +
  geom_line(aes(y = VW, color = "Value-weighted"), size = 1) +
  labs(title = "Tangency portfolios vs Benchmarks",
       x = "Date", y = "Cumulative return") +  # Removed 'color' argument here
  scale_color_manual(values = c("Tangency long" = "blue", 
                                "Equally weighted" = "orange", 
                                "Tangency short" = "blue4",
                                "Value-weighted" = "red")) +
  scale_x_date(
    breaks = seq(min(tg_overall$date), max(tg_overall$date), by = "4 years"),  # Tick every 2 years
    labels = date_format("%Y")  # Format as "Year"
  ) +
  theme(
    legend.justification = c(0, 1), 
    legend.position = c(0, 1),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


### Betas ###

plot(capm$beta, capm$t_return, 
     type = 'l', 
     lwd = 2,
     xlab = "Beta", 
     ylab = "Mean Return",
     ylim = c(0,0.03),
     main = "Theoretical vs Empirical SML")

points(capm$beta, capm$e_return, 
       col = "blue", 
       pch =  19)

text(capm$beta, capm$e_return, 
     labels = rownames(capm), 
     pos = 4,   # Position labels to the right of the points
     cex = 0.5, # Smaller font size for labels
     col = "black") # Label color

for (i in 1:length(capm$beta)) {
  segments(capm$beta[i], capm$t_return[i],  # Start at theoretical line
           capm$beta[i], capm$e_return[i],   # End at empirical point
           col = "gray",                    # Color of the line
           lty = 2)                          # Dashed line (lty = 2)
}

legend("topleft", 
       legend = c("Theoretical SML", "Empirical SML"), 
       col = c("black", "blue"), 
       lwd = c(2, NA),  # Line width for the theoretical line
       pch = c(NA, 19),
       cex = 0.8,
       bty = "n")  # No border around the legend


### Portfolio optimization ###

# Reshape the data to long format for ggplot
sims_long <- sims[sims$t %in% c(1, 2, 12, 60, 100), c("k", "mean_ER", "t")]
sims_long$t <- factor(sims_long$t)  # Convert t to a factor for color mapping

# Identify the maximum value on the first line
first_line_max <- sims_long[sims_long$t == levels(sims_long$t)[1], ]
max_point <- first_line_max[which.max(first_line_max$mean_ER), ]

# Create the plot
ggplot(sims_long, aes(x = k, y = mean_ER, color = t, group = t, linetype = t)) +
  geom_line(size = 1.1) +  # Solid and dashed lines are mapped by linetype
  geom_point(data = max_point, aes(x = k, y = mean_ER), 
             color = "red", size = 5) +  # Add a large red dot at max value
  scale_linetype_manual(values = c("solid", "longdash", "dashed", "dotted", "dotted")) +  # Map linetypes
  scale_color_manual(values = c("blue", "red3", "orange", "green4", "hotpink")) +  # Set custom colors for the groups
  labs(x = "Tangency portfolio window (months)", 
       y = "Excess returns", 
       title = "Simulations",
       color = "Frequencies (months)", 
       linetype = "Frequencies (months)") +  # Update legend title
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center-align the title
  scale_x_continuous(breaks = seq(min(sims_long$k), max(sims_long$k), by = 5)) +  # Increase tick interval on the x-axis
  scale_y_continuous(breaks = seq(-0.02, by = 0.002)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5) +  # Add a horizontal black line at y = 0
  theme(panel.grid.minor = element_blank())  # Optionally remove minor gridlines





# in-sample
ggplot(data = in_sample, aes(x = date)) +
  geom_line(aes(y = PF_index, color = "PF Index"), size = 1.2) +
  geom_line(aes(y = EW_index, color = "EW Index"), size = 1.2) +
  geom_line(aes(y = VW_index, color = "VW Index"), size = 1.2) +
  labs(title = "PF vs Benchmarks in-sample",
       x = "Date", y = "Cumulative return") +  # Removed 'color' argument here
  scale_color_manual(values = c("PF Index" = "blue", 
                                "EW Index" = "orange", 
                                "VW Index" = "red")) +
  scale_x_date(
    breaks = seq(min(in_sample$date), max(in_sample$date), by = "2 years"),  # Tick every 2 years
    labels = date_format("%Y")  # Format as "Year"
  ) +
  theme(
    legend.justification = c(0, 1), 
    legend.position = c(0, 1),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# out-of-sample
ggplot(data = out_of_sample, aes(x = date)) +
  geom_line(aes(y = PF_index, color = "PF Index"), size = 1.2) +
  geom_line(aes(y = EW_index, color = "EW Index"), size = 1.2) +
  geom_line(aes(y = VW_index, color = "VW Index"), size = 1.2) +
  labs(title = "PF vs Benchmarks out-of-sample",
       x = "Date", y = "Cumulative return") +  # Removed 'color' argument here
  scale_color_manual(values = c("PF Index" = "blue", 
                                "EW Index" = "orange", 
                                "VW Index" = "red")) +
  scale_x_date(
    breaks = seq(min(out_of_sample$date), max(out_of_sample$date), by = "2 years"),  # Tick every 2 years
    labels = date_format("%Y")  # Format as "Year"
  ) +
  theme(
    legend.justification = c(0, 1), 
    legend.position = c(0, 1),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )



# Additional plots
library(patchwork)

plot1 <- ggplot(data = returns, aes(x = date)) +
  geom_line(aes(y = cumprod(1+REPLIGEN), color = "REPLIGEN"), size = 1) +
  labs(title = "REPLIGEN", x = "Date", y = "Cumulative return") +
  scale_color_manual(values = c("REPLIGEN" = "blue")) +
  scale_x_date(
    breaks = seq(min(tg_overall$date), max(tg_overall$date), by = "4 years"),
    labels = date_format("%Y")
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

plot2 <- ggplot(data = returns, aes(x = date)) +
  geom_line(aes(y = cumprod(1+ENCOMPASS.HEALTH), color = "ENCOMPASS.HEALTH"), size = 1) +
  labs(title = "ENCOMPASS.HEALTH", x = "Date", y = "Cumulative return") +
  scale_color_manual(values = c("ENCOMPASS.HEALTH" = "blue4")) +
  scale_x_date(
    breaks = seq(min(tg_overall$date), max(tg_overall$date), by = "4 years"),
    labels = date_format("%Y")
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

plot3 <- ggplot(data = returns, aes(x = REPLIGEN)) +
  geom_histogram(bins = 100, color = 'black', fill = 'blue') +
  labs(title = "REPLIGEN") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

plot4 <- ggplot(data = returns, aes(x = ENCOMPASS.HEALTH)) +
  geom_histogram(bins = 100, color = 'black', fill = 'blue4') +
  labs(title = "ENCOMPASS.HEALTH") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

combined_plot <- (plot1 | plot2) / (plot3 | plot4)
combined_plot

