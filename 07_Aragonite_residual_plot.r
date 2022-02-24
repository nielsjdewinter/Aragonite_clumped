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
    geom_line(data = D47m_York_result_res[1:1001, ],
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
#    geom_violin(data = dat[which(!dat$D47_outlier), ],
#        aes(10^6 / (270.15) ^ 2,
#            y = D47res_York),
#        fill = "black",
#        kernel = "rectangular",
#        scale = "width",
#        position = "identity",
#        width = .2,
#        cex = 0,
#        alpha = 0.2,
#        color = NA,
#        trim = TRUE) +
#    geom_pointrange(data = dat[which(!dat$D47_outlier), ],
#        aes(x = 10^6 / (270.15) ^ 2,
#            y = mean(D47res_York),
#            ymin = mean(D47res_York) - sd(D47res_York),
#            ymax = mean(D47res_York) + sd(D47res_York)),
#        color = "black",
#        cex = 1) +
# Plot layout
    scale_x_continuous(10 ^ 6 / T ^ 2 ~ "(K)",
        breaks = seq(0, 14, 1),
        minor_breaks = seq(0, 14, 0.25),
        sec.axis = sec_axis(~ sqrt(1e6 / .) - 273.15,
            "Temperature (°C)",
            breaks = temp_breaks,
            labels = temp_labs)) +
    coord_cartesian(xlim = c(0, 14),
        ylim = c(-0.1, 0.1)) +
    labs(x = 10 ^ 6 / T ^ 2 ~ "(K)",
        y = Delta * Delta[47] ~ "(\u2030"~"I-CDES)",
        colour = "Legend") +
    scale_colour_manual(values = colorscale) +
    scale_shape_manual(values = c(15:20)) +
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
    geom_line(data = D47m_lowT_York_result_res[1:1001, ],
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
# Add summary violin/pointrange
#    geom_violin(data = dat[which(!dat$D47_outlier & (dat$Analysis != "Muller17")), ],
#        aes(10^6 / (270.15) ^ 2,
#            y = D47res_lowT_York),
#        fill = "black",
#        kernel = "rectangular",
#        scale = "width",
#        position = "identity",
#        width = .2,
#        cex = 0,
#        alpha = 0.2,
#        color = NA,
#        trim = TRUE) +
#    geom_pointrange(data = dat[which(!dat$D47_outlier  & (dat$Analysis != "Muller17")), ],
#        aes(x = 10^6 / (270.15) ^ 2,
#            y = mean(D47res_lowT_York),
#            ymin = mean(D47res_lowT_York) - sd(D47res_lowT_York),
#            ymax = mean(D47res_lowT_York) + sd(D47res_lowT_York)),
#        color = "black",
#        cex = 1) +
    scale_x_continuous(10 ^ 6 / T ^ 2 ~ "(K)",
        breaks = seq(0, 14, 1),
        minor_breaks = seq(0, 14, 0.25),
        sec.axis = sec_axis(~ sqrt(1e6 / .) - 273.15,
            "Temperature (°C)",
            breaks = temp_breaks,
            labels = temp_labs_lowT)) +
    coord_cartesian(xlim = c(7, 14),
        ylim = c(-0.1, 0.1)) +
    labs(x = 10 ^ 6 / T ^ 2 ~ "(K)",
        y = Delta * Delta[47] ~ "(\u2030"~"I-CDES)",
        colour = "Legend") +
    scale_colour_manual(values = colorscale) +
    scale_shape_manual(values = c(15:20)) +
    theme_bw()
    
Combined_residual_plot <- grid.arrange(York_residual_plot + theme(legend.position = "none"),
#    York_residual_plot_margin,
#    poly_residual_plot + theme(legend.position = "none"),
#    poly_residual_plot_margin,
    lowT_York_residual_plot + theme(legend.position = "none"),
#    lowT_York_residual_plot_margin,
#    layout_matrix = rbind(
#        c(rep(1, 15), 2),
#        c(rep(3, 15), 4)
#        c(rep(5, 15), 6)
    ncol = 1
    )

pdf("E:/Dropbox//Research//postdoc//UNBIAS//Clumped Temperature Calibration/Aragonite D47 residual plots.pdf")
plot(Combined_residual_plot)
dev.off()
