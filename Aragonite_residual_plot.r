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
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_York_result_res,
        aes(x = x, y = Anderson),
        color = "black",
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_York_result_res[1:1001, ],
        aes(x = x, y = Meinicke),
        color = "black",
        linetype = "dotted",
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
    ylim(-0.1, 0.1) +
    scale_x_continuous(10 ^ 6 / T ^ 2 ~ "(K)",
        breaks = seq(0, 14, 1),
        minor_breaks = seq(0, 14, 0.25),
        sec.axis = sec_axis(~ sqrt(1e6 / .) - 273.15,
            "Temperature (°C)",
            breaks = temp_breaks,
            labels = temp_labs),
        limits = c(0, 14)) +
    labs(x = 10 ^ 6 / T ^ 2 ~ "(K)",
        y = Delta * Delta[47] ~ "(\u2030"~"I-CDES)",
        colour = "Legend") +
    scale_colour_manual(values = colorscale) +
    scale_shape_manual(values = c(15:20)) +
    theme_bw() +
    theme(legend.position = "none")
    
# Add separate summary violin/pointrange
York_residual_plot_margin <- ggplot(data = dat[which(!dat$D47_outlier), ]) +
    geom_violin(aes(1,
            y = D47res_York),
        fill = "black",
        kernel = "rectangular",
        scale = "width",
        position = "identity",
        cex = 0,
        alpha = 0.2,
        color = NA,
        trim = TRUE
    ) +
    geom_pointrange(data = dat[which(!dat$D47_outlier), ],
        aes(1,
            y = mean(D47res_York),
            ymin = mean(D47res_York) - sd(D47res_York),
            ymax = mean(D47res_York) + sd(D47res_York)),
        color = "black",
        cex = 1) +
    scale_y_continuous("",
        breaks = seq(-0.1, 0.1, 0.1),
        labels = rep("", 3),
        limits = c(-0.1, 0.1)) +
    scale_x_continuous("",
        breaks = seq(0.5, 1.5, 0.5),
        labels = rep("", 3),
        sec.axis = sec_axis(~ .,
            "",
            breaks = seq(0.5, 1.5, 0.5),
            labels = rep("", 3)),
        limits = c(0.5, 1.5)) +
    labs(x = "") +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(5.5, 0, 5.5, -20), "pt")
    )
    

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
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_lowT_York_result_res,
        aes(x = x, y = Anderson),
        color = "black",
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_lowT_York_result_res[1:1001, ],
        aes(x = x, y = Meinicke),
        color = "black",
        linetype = "dotted",
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
    ylim(-0.1, 0.1) +
    scale_x_continuous(10 ^ 6 / T ^ 2 ~ "(K)",
        breaks = seq(0, 14, 1),
        minor_breaks = seq(0, 14, 0.25),
        sec.axis = sec_axis(~ sqrt(1e6 / .) - 273.15,
            "Temperature (°C)",
            breaks = temp_breaks,
            labels = temp_labs_lowT),
        limits = c(7, 14)) +
    labs(x = 10 ^ 6 / T ^ 2 ~ "(K)",
        y = Delta * Delta[47] ~ "(\u2030"~"I-CDES)",
        colour = "Legend") +
    scale_colour_manual(values = colorscale) +
    scale_shape_manual(values = c(15:20)) +
    theme_bw()
    
# Add separate summary violin/pointrange
lowT_York_residual_plot_margin <- ggplot(data = dat[which(!dat$D47_outlier), ]) +
    geom_violin(aes(1,
            y = D47res_lowT_York),
        fill = "black",
        kernel = "rectangular",
        scale = "width",
        position = "identity",
        cex = 0,
        alpha = 0.2,
        color = NA,
        trim = TRUE
    ) +
    geom_pointrange(data = dat[which(!dat$D47_outlier), ],
        aes(1,
            y = mean(D47res_lowT_York),
            ymin = mean(D47res_lowT_York) - sd(D47res_lowT_York),
            ymax = mean(D47res_lowT_York) + sd(D47res_lowT_York)),
        color = "black",
        cex = 1) +
    scale_y_continuous("",
        breaks = seq(-0.1, 0.1, 0.1),
        labels = rep("", 3),
        limits = c(-0.1, 0.1)) +
    scale_x_continuous("",
        breaks = seq(0.5, 1.5, 0.5),
        labels = rep("", 3),
        sec.axis = sec_axis(~ .,
            "",
            breaks = seq(0.5, 1.5, 0.5),
            labels = rep("", 3)),
        limits = c(0.5, 1.5)) +
    labs(x = "") +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(5.5, 0, 5.5, -20), "pt")
    )

poly_residual_plot <- ggplot(data = dat, aes(10^6 / (Temp + 273.15) ^ 2 , D47res_poly)) +
    geom_ribbon(data = D47m_poly_MC_result_res,
        aes(x = x,
        y = fit,
        ymin = lwr,
        ymax = upr),
        fill = "grey",
        alpha = 0.5) +
    geom_line(data = D47m_poly_MC_result_res,
        aes(x = x, y = fit),
        color = "grey",
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_poly_MC_result_res,
        aes(x = x, y = Anderson),
        color = "black",
        linetype = "dashed",
        cex = 1,
        alpha = 1) +
    geom_line(data = D47m_poly_MC_result_res[1:1001, ],
        aes(x = x, y = Meinicke),
        color = "black",
        linetype = "dotted",
        cex = 1,
        alpha = 1) +
    geom_pointrange(data = D47stats,
        aes(x = 10^6 / (Temp + 273.15) ^ 2,
            y = D47res_poly,
            ymin = D47res_poly - CL95,
            ymax = D47res_poly + CL95,
            color = Analysis,
            shape = type),
        cex = 1,
        alpha = 1) +
    geom_errorbarh(data = D47stats,
        aes(xmin = 10^6 / ((Temp + 2 * Temp_SD) + 273.15) ^ 2, 
        xmax = 10^6 / ((Temp - 2 * Temp_SD) + 273.15) ^ 2,
        y = D47res_poly,
        color = Analysis),
        cex = 1,
        alpha = 1) +
# Add summary violin/pointrange
#    geom_violin(data = dat[which(!dat$D47_outlier), ],
#        aes(10^6 / (270.15) ^ 2,
#            y = D47res_poly),
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
#            y = mean(D47res_poly),
#            ymin = mean(D47res_poly) - sd(D47res_poly),
#            ymax = mean(D47res_poly) + sd(D47res_poly)),
#        color = "black",
#        cex = 1) +
    ylim(-0.1, 0.1) +
    scale_x_continuous(10 ^ 6 / T ^ 2 ~ "(K)",
        breaks = seq(0, 14, 1),
        minor_breaks = seq(0, 14, 0.25),
        sec.axis = sec_axis(~ sqrt(1e6 / .) - 273.15,
            "Temperature (°C)",
            breaks = temp_breaks,
            labels = temp_labs),
        limits = c(0, 14)) +
    labs(x = 10 ^ 6 / T ^ 2 ~ "(K)",
        y = Delta * Delta[47] ~ "(\u2030"~"I-CDES)",
        colour = "Legend") +
    scale_colour_manual(values = colorscale) +
    scale_shape_manual(values = c(15:20)) +
    theme_bw()
    
# Add separate summary violin/pointrange
poly_residual_plot_margin <- ggplot(data = dat[which(!dat$D47_outlier), ]) +
    geom_violin(aes(1,
            y = D47res_poly),
        fill = "black",
        kernel = "rectangular",
        scale = "width",
        position = "identity",
        cex = 0,
        alpha = 0.2,
        color = NA,
        trim = TRUE
    ) +
    geom_pointrange(data = dat[which(!dat$D47_outlier), ],
        aes(1,
            y = mean(D47res_poly),
            ymin = mean(D47res_poly) - sd(D47res_poly),
            ymax = mean(D47res_poly) + sd(D47res_poly)),
        color = "black",
        cex = 1) +
    scale_y_continuous("",
        breaks = seq(-0.1, 0.1, 0.1),
        labels = rep("", 3),
        limits = c(-0.1, 0.1)) +
    scale_x_continuous("",
        breaks = seq(0.5, 1.5, 0.5),
        labels = rep("", 3),
        sec.axis = sec_axis(~ .,
            "",
            breaks = seq(0.5, 1.5, 0.5),
            labels = rep("", 3)),
        limits = c(0.5, 1.5)) +
    labs(x = "") +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(5.5, 0, 5.5, -20), "pt")
    )

Combined_residual_plot <- grid.arrange(York_residual_plot + theme(legend.position = "none"),
    York_residual_plot_margin,
    poly_residual_plot + theme(legend.position = "none"),
    poly_residual_plot_margin,
    lowT_York_residual_plot + theme(legend.position = "none"),
    lowT_York_residual_plot_margin,
    layout_matrix = rbind(
        c(rep(1, 15), 2),
        c(rep(3, 15), 4),
        c(rep(5, 15), 6)
    ))