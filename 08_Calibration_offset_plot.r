require(ggplot2) # for plotting
require(ggpubr) # for violin plots
require(gridExtra) # for combining plots
require(RColorBrewer) # to define color scales
require(ggrepel) # for plot labeling

# Plot offset from Anderson and Meinicke
Calibration_offset_plot <- ggplot(data = calibration_offset) +
    # geom_violin(aes(x = Calibration,
    #         y = D47_offset),
    #     fill = "black",
    #     kernel = "rectangular",
    #     scale = "width",
    #     position = "identity",
    #     width = .5,
    #     cex = 0,
    #     alpha = 0.2,
    #     color = NA,
    #     trim = TRUE) +
    # geom_point(aes(x = Calibration,
    #         y = D47_offset),
    #     position = position_dodge2(width = .05),
    #     alpha = 0.1,
    #     cex = 1) +
    geom_errorbar(data = Calibration_offset_stats,
        aes(x = Calibration,
            ymin = D47_offset_Average - D47_offset_CL95,
            ymax = D47_offset_Average + D47_offset_CL95),
        cex = 1,
        width = 0.1) +
    geom_point(data = Calibration_offset_stats,
        aes(x = Calibration,
            y = D47_offset_Average),
        size = 3) +
    geom_hline(yintercept = 0,
        linetype = "dashed",
        size = 1,
        alpha = 0.5) +
    scale_y_continuous(expression(Delta[47]~"offset from calibration "~ "(\u2030"~"I-CDES)"),
        limits = c(-0.025, 0.1),
        breaks = seq(-0.025, 0.1, 0.01),
        sec.axis = sec_axis(~ sqrt(0.0391 * 10 ^ 6 / (mean_D47 + . - 0.154)) - 273.15 - mean_Temp,
            "Equivalent temperature offset (°C)",
            breaks = seq(-25, 25, 5),
            labels = seq(-25, 25, 5))) +
    theme_bw()

# Supplementary figure with expanded vertical axis
Calibration_offset_plot_zoomout <- ggplot(data = calibration_offset) +
    # geom_violin(aes(x = Calibration,
    #         y = D47_offset),
    #     fill = "black",
    #     kernel = "rectangular",
    #     scale = "width",
    #     position = "identity",
    #     width = .5,
    #     cex = 0,
    #     alpha = 0.2,
    #     color = NA,
    #     trim = TRUE) +
    geom_point(aes(x = Calibration,
            y = D47_offset),
        position = position_dodge2(width = .05),
        alpha = 0.1,
        cex = 1) +
    geom_errorbar(data = Calibration_offset_stats,
        aes(x = Calibration,
            ymin = D47_offset_Average - D47_offset_CL95,
            ymax = D47_offset_Average + D47_offset_CL95),
        cex = 1,
        width = 0.1) +
    geom_point(data = Calibration_offset_stats,
        aes(x = Calibration,
            y = D47_offset_Average),
        size = 3) +
    geom_hline(yintercept = 0,
        linetype = "dashed",
        size = 1,
        alpha = 0.5) +
    scale_y_continuous(expression(Delta[47]~"offset from calibration "~ "(\u2030"~"I-CDES)"),
        limits = c(-0.12, 0.12),
        breaks = seq(-0.12, 0.12, 0.03),
        sec.axis = sec_axis(~ sqrt(0.0391 * 10 ^ 6 / (mean_D47 + . - 0.154)) - 273.15 - mean_Temp,
            "Equivalent temperature offset (°C)",
            breaks = seq(-25, 25, 5),
            labels = seq(-25, 25, 5))) +
    theme_bw()

