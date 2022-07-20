require(ggplot2) # for plotting
require(ggpubr) # for violin plots
require(gridExtra) # for combining plots
require(RColorBrewer) # to define color scales
require(ggrepel) # for plot labeling

# Plot residuals of aragonite dataset with respect to regressions ------------------------------------------------------------

# Build custom color scales for full dataset plot
colorscale <- unlist(brewer.pal(length(unique(dat$Analysis)), "Set1"))
names(colorscale) <- unique(dat$Analysis[order(dat$Temp)])

uniquesamples <- data.frame(samplename = unique(violin_data$sample),
    Analysis = sapply(strsplit(unique(violin_data$sample), split = "_"), "[", 2)
) # Link sample to Analysis
fillscale <- colorscale[match(uniquesamples$Analysis, names(colorscale))]
names(fillscale) <- uniquesamples$samplename

# Plot residuals
York_residual_plot <- ggplot(data = dat, aes(10^6 / (Temp + 273.15) ^ 2 , D47res_York)) +
    geom_ribbon(data = D47m_York_result_res,
        aes(x = x,
        y = fit,
        ymin = lwr,
        ymax = upr),
        fill = "grey",
        alpha = 0.5) +
    geom_line(data = D47m_York_result_res,
        aes(x = x, y = fit),
        color = "grey",
        linetype = "dotted",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_York_result_res,
        aes(x = x, y = Anderson),
        color = "black",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_York_result_res[1:301, ],
        aes(x = x, y = Meinicke),
        color = "black",
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_York_result_res,
        aes(x = x, y = Guo_cc),
        color = "grey",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_York_result_res,
        aes(x = x, y = Guo_ar),
        color = "grey",
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_pointrange(data = D47stats,
        aes(x = 10^6 / (Temp + 273.15) ^ 2,
            y = D47res_York,
            ymin = D47res_York - CL95,
            ymax = D47res_York + CL95,
            color = Analysis,
            shape = type),
        cex = 1,
        alpha = 1) +
    geom_errorbarh(data = D47stats,
        aes(xmin = 10^6 / ((Temp + 2 * Temp_SD) + 273.15) ^ 2, 
        xmax = 10^6 / ((Temp - 2 * Temp_SD) + 273.15) ^ 2,
        y = D47res_York,
        color = Analysis),
        cex = 1,
        alpha = 1) +
# Plot layout
    scale_x_continuous(10 ^ 6 / T ^ 2 ~ "(K)",
        breaks = seq(0, 13.5, 1),
        minor_breaks = seq(0, 13.5, 0.5),
        sec.axis = sec_axis(~ sqrt(1e6 / .) - 273.15,
            "Temperature (°C)",
            breaks = temp_breaks,
            labels = temp_labs)) +
    coord_cartesian(xlim = c(0, 13.5),
        ylim = c(-0.05, 0.05)) +
    labs(x = 10 ^ 6 / T ^ 2 ~ "(K)",
        y = Delta * Delta[47] ~ "(\u2030"~"I-CDES)",
        colour = "Legend") +
    scale_colour_manual(values = colorscale) +
    scale_shape_manual(values = c(15:20)) +
    ggtitle("A") +
    theme_bw() +
    theme(legend.position = "none")
    
lowT_York_residual_plot <- ggplot(data = dat, aes(10^6 / (Temp + 273.15) ^ 2 , D47res_lowT_York)) +
    geom_ribbon(data = D47m_lowT_York_result_res,
        aes(x = x,
        y = fit,
        ymin = lwr,
        ymax = upr),
        fill = "grey",
        alpha = 0.5) +
    geom_line(data = D47m_lowT_York_result_res,
        aes(x = x, y = fit),
        color = "grey",
        linetype = "dotted",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_lowT_York_result_res,
        aes(x = x, y = Anderson),
        color = "black",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_lowT_York_result_res[1:301, ],
        aes(x = x, y = Meinicke),
        color = "black",
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_lowT_York_result_res,
        aes(x = x, y = Guo_cc),
        color = "grey",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_lowT_York_result_res,
        aes(x = x, y = Guo_ar),
        color = "grey",
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_pointrange(data = D47stats,
        aes(x = 10^6 / (Temp + 273.15) ^ 2,
            y = D47res_lowT_York,
            ymin = D47res_lowT_York - CL95,
            ymax = D47res_lowT_York + CL95,
            color = Analysis,
            shape = type),
        cex = 1,
        alpha = 1) +
    geom_errorbarh(data = D47stats,
        aes(xmin = 10^6 / ((Temp + 2 * Temp_SD) + 273.15) ^ 2, 
        xmax = 10^6 / ((Temp - 2 * Temp_SD) + 273.15) ^ 2,
        y = D47res_lowT_York,
        color = Analysis),
        cex = 1,
        alpha = 1) +
# Plot layout
    scale_x_continuous(10 ^ 6 / T ^ 2 ~ "(K)",
        breaks = seq(0, 13.5, 1),
        minor_breaks = seq(0, 13.5, 0.5),
        sec.axis = sec_axis(~ sqrt(1e6 / .) - 273.15,
            "Temperature (°C)",
            breaks = temp_breaks,
            labels = temp_labs_lowT)) +
    coord_cartesian(xlim = c(10 ^ 6 / (30 + 273.15) ^ 2, 13.5),
        ylim = c(-0.05, 0.05)) +
    labs(x = 10 ^ 6 / T ^ 2 ~ "(K)",
        y = Delta * Delta[47] ~ "(\u2030"~"I-CDES)",
        colour = "Legend") +
    scale_colour_manual(values = colorscale) +
    scale_shape_manual(values = c(15:20)) +
    ggtitle("B") +
    theme_bw()

Combined_residual_plot <- ggarrange(York_residual_plot,
    lowT_York_residual_plot,
    ncol = 1,
    common.legend = TRUE,
    legend="bottom"
    )
