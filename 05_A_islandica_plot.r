require(ggplot2) # for plotting
require(ggpubr) # for violin plots
require(gridExtra) # for combining plots
require(RColorBrewer) # to define color scales
require(ggrepel) # for plot labeling

# Plot Arctica islandica data and calibration

# ----------------Preparation for plot asthetics--------------------------------
# Prepare secondary temperature axis
temp_labs <- c("0", "5", rep("", 3), "25", rep("", 4), "50", rep("", 14), "125", rep("", 24), "250", rep("", 149), "1000")
temp_labs_lowT <- c(as.character(seq(0, 30 ,5)), "", "40", "", "50", "", "60", "", "70", "", "80", "", "90", "", "100", rep("", 4), "125", rep("", 24), "250", rep("", 149), "1000")
temp_breaks <- seq(0, 1000, 5)

# Build custom color scales for A. islandica plot
Ais_colorscale <- unlist(brewer.pal(length(unique(Aisstats$sample)), "RdBu"))
names(Ais_colorscale) <- unique(Aisstats$sample[order(Aisstats$Temp, decreasing = TRUE)])

Ais_plot <- ggplot(data = dat, aes(10^6 / (Temp + 273.15) ^ 2 , D47)) +
# Anderson, Meinicke and Guo regressions
    geom_line(data = Anderson21,
        aes(x = Temp, y = D47),
        color = "black",
        cex = 1,
        alpha = 1) +
    geom_line(data = MeinickeICDES[1:301, ],
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
# Measurements
    geom_pointrange(data = Aisstats,
        aes(x = 10^6 / (Temp + 273.15) ^ 2,
            y = D47_mean,
            ymin = D47_mean - CL95,
            ymax = D47_mean + CL95,
            color = sample),
        position = position_dodge2(width = .05),
        cex = 1,
        alpha = 0.2) +
    geom_text_repel(data = Aisstats,
        aes(x = 10^6 / (Temp + 273.15) ^ 2,
            y = D47_mean,
            color = sample),
        label = Aisstats$N[order(Aisstats$Temp, decreasing = TRUE)],
        position = position_dodge2(width = .05),
        box.padding = 0.5) +
# Aggregated sample data
    geom_errorbar(data = D47stats[which(D47stats$Analysis == "this study"), ],
        aes(x = 10^6 / (Temp + 273.15) ^ 2,
            y = D47_mean,
            ymin = D47_mean - CL95,
            ymax = D47_mean + CL95,
            color = sample),
        width = 0.05,
        cex = 1,
        alpha = 1) +
    geom_errorbar(data = D47stats[which(D47stats$ID == "A.islandica"), ],
        aes(x = 10^6 / (Temp + 273.15) ^ 2,
            y = D47_mean,
            ymin = D47_mean - CL95,
            ymax = D47_mean + CL95,
            color = "Bernasconi18"),
        width = 0.05,
        cex = 1,
        alpha = 1) +
# Plot layout
    scale_x_continuous(10 ^ 6 / T ^ 2 ~ "(K)",
        breaks = seq(11, 14, 0.5),
        minor_breaks = seq(11, 14, 0.25),
        sec.axis = sec_axis(~ sqrt(1e6 / .) - 273.15,
            "Temperature (°C)",
            breaks = temp_breaks,
            labels = temp_labs_lowT)) +
    coord_cartesian(xlim = c(11.5, 13.5),
        ylim = c(0.6, 0.72)
    ) +
    ggtitle(expression("Arctica islandica "~Delta[47]~" vs Temperature")) +
    labs(x = 10 ^ 6 / T ^ 2 ~ "(K)",
        y = Delta[47] ~ "(\u2030"~"I-CDES)",
        colour = "Legend") +
    scale_colour_manual(values = Ais_colorscale) +
    scale_fill_manual(values = Ais_colorscale) +
    theme_bw() +
    theme(legend.position = "none")
