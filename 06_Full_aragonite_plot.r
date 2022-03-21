require(ggplot2) # for plotting
require(ggpubr) # for violin plots
require(RColorBrewer) # to define color scales
require(ggrepel) # for plot labeling

# Plot full aragonite dataset with regressions ------------------------------------------------------------
# Prepare secondary temperature axis
temp_labs <- c("0", "5", rep("", 3), "25", rep("", 4), "50", rep("", 14), "125", rep("", 24), "250", rep("", 149), "1000")
temp_labs_lowT <- c(as.character(seq(0, 30 ,5)), "", "40", "", "50", "", "60", "", "70", "", "80", "", "90", "", "100", rep("", 4), "125", rep("", 24), "250", rep("", 149), "1000")
temp_breaks <- seq(0, 1000, 5)

# Build custom color scales for full dataset plot
colorscale <- unlist(brewer.pal(length(unique(dat$Analysis)), "Set1"))
names(colorscale) <- unique(dat$Analysis[order(dat$Temp)])

uniquesamples <- data.frame(samplename = unique(violin_data$sample),
    Analysis = sapply(strsplit(unique(violin_data$sample), split = "_"), "[", 2)
) # Link sample to Analysis
fillscale <- colorscale[match(uniquesamples$Analysis, names(colorscale))]
names(fillscale) <- uniquesamples$samplename

Aragonite_D47_plot <- ggplot(data = dat, aes(10^6 / (Temp + 273.15) ^ 2 , D47)) +
# Regressions
    geom_ribbon(data = D47m_York_result,
        aes(x = x,
        y = fit,
        ymin = lwr,
        ymax = upr),
        fill = "orange",
        alpha = 0.3) +
    geom_line(data = D47m_York_result,
        aes(x = x, y = fit),
        color = "orange",
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_ribbon(data = D47m_lowT_York_result,
        aes(x = x,
        y = fit,
        ymin = lwr,
        ymax = upr),
        fill = "blue",
        alpha = 0.3) +
    geom_line(data = D47m_lowT_York_result,
        aes(x = x, y = fit),
        color = "blue",
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_line(data = Anderson21,
        aes(x = Temp, y = D47),
        color = "black",
        cex = 1,
        alpha = 1) +
    geom_line(data = MeinickeICDES[1:1001, ],
        aes(x = Temp, y = D47),
        color = "black",
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_line(data = Guo09,
        aes(x = Temp, y = D47_cc),
        color = "grey",
        cex = 1,
        alpha = 1) +
    geom_line(data = Guo09,
        aes(x = Temp, y = D47_ar),
        color = "grey",  
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
# Violin plots
    geom_violin(data = violin_data[which(!violin_data$outlier), ],
        aes(10^6 / (Temp + 273.15) ^ 2,
            y = D47,
            fill = sample),
            kernel = "rectangular",
            scale = "width",
            position = "identity",
            width = .1,
            cex = 0,
            alpha = 0.2,
            color = NA,
            trim = FALSE,
            show.legend = FALSE) +
# Aliquot datapoints
    geom_point(data = dat[which(!dat$D47_outlier), ],
        alpha = 0.2,
        cex = 1,
        aes(x = 10^6 / (Temp + 273.15) ^ 2,
            y = D47,
            color = Analysis,
            shape = type
        )) + 
#    geom_pointrange(data = dat[which(dat$D47_outlier), ],
#        alpha = 0.2,
#        cex = 0.5,
#        aes(x = 10^6 / (Temp + 273.15) ^ 2,
#            y = D47,
#            ymin = D47 - 2 * D47_SD,
#            ymax = D47 + 2 * D47_SD,
#            color = "outliers",
#            shape = type)) +
# Aggregated sample data
    geom_pointrange(data = D47stats,
        aes(x = 10^6 / (Temp + 273.15) ^ 2,
            y = D47_mean,
            ymin = D47_mean - CL95,
            ymax = D47_mean + CL95,
            color = Analysis,
            shape = type),
        cex = 1,
        alpha = 1) +
    geom_errorbarh(data = D47stats,
        aes(xmin = 10^6 / ((Temp + 2 * Temp_SD) + 273.15) ^ 2, 
        xmax = 10^6 / ((Temp - 2 * Temp_SD) + 273.15) ^ 2,
        y = D47_mean,
        color = Analysis),
        cex = 1,
        alpha = 1) +
# Plot layout
    ylim(0, max(D47stats$D47_mean + D47stats$CL95)) +
    scale_x_continuous(10 ^ 6 / T ^ 2 ~ "(K)",
        breaks = seq(0, 13.5, 1),
        minor_breaks = seq(0, 13.5, 0.25),
        sec.axis = sec_axis(~ sqrt(1e6 / .) - 273.15,
            "Temperature (°C)",
            breaks = temp_breaks,
            labels = temp_labs)) +
    scale_y_continuous(Delta[47] ~ "(\u2030"~"I-CDES)",
        breaks = seq(0, 0.8, 0.2)) +
    ggtitle("A") +
    labs(x = 10 ^ 6 / T ^ 2 ~ "(K)",
        y = Delta * Delta[47] ~ "(\u2030"~"I-CDES)",
        colour = "Legend") +
    scale_colour_manual(values = colorscale) +
    scale_fill_manual(values = fillscale) +
    scale_shape_manual(values = c(15:20)) +
    theme_bw()


# Add zoom in for low temperatures----------------------------------------------
Aragonite_D47_plot_lowT <- Aragonite_D47_plot +
    ylim(0.3, 0.8) +
    scale_x_continuous(10 ^ 6 / T ^ 2 ~ "(K)",
        breaks = seq(0, 13.5, 1),
        minor_breaks = seq(0, 13.5, 0.5),
        sec.axis = sec_axis(~ sqrt(1e6 / .) - 273.15,
            "Temperature (°C)",
            breaks = temp_breaks,
            labels = temp_labs_lowT),
        limits = c(7, 13.5)) +
    ggtitle("B")

combined_plots_Aragonite <- ggarrange(Aragonite_D47_plot,
    Aragonite_D47_plot_lowT,
    ncol = 1,
    common.legend = TRUE,
    legend="bottom"
)
