# Process D47 data for plotting in manuscript

require(tidyverse) # for data treatment
require(bfsl) # for York regressions

# Load combined aragonite dataset
dat <- as.data.frame(read.csv("<path>/01_Aragonite_compilation.csv", header = TRUE))
dat$sample <- paste(dat$Temp, dat$Analysis, sep = "_")

source("<path>/03_Average_error_propagation.r")

# ------------------------Summarize stats per data ID---------------------------

# First propagate uncertainties on bins per temperature, keeping data from different machines separate
D47stats_Machinebin <- dat[which(dat$D47_outlier != TRUE),] %>% # Summarize D47 statistics
    group_by(ID, Machine) %>%
    summarize(
        Temp = mean(Temp, na.rm = TRUE),
        Temp_SD = first(Temp_SD),
        Analysis = first(Analysis),
        type = first(type),
        sample = first(sample),
        D47_binmean = mean(D47, na.rm = TRUE),  # Calculate means per sample
        SD_bin = sd(D47, na.rm = TRUE), # Calculate stdevs per sample bin
        SD_ext = first(D47_SD), # External standard deviation (reported or based on check STD)
        binsd = if(is.na(SD_bin)){first(SD_ext)}else{max(SD_bin, SD_ext)}, # Find largest standard deviation
        Nbin = n(), # Calculate the number of modelled values, excluding NA's
        binse = binsd / sqrt(Nbin), # Calculate the standard error
        binCL95 = qt(0.95, Nbin) * binse, # Calculate the 95% confidence level
        param49_binmean = mean(param49, na.rm = TRUE), # Propagate param49 mean per sample
        param49_binsd = sd(param49, na.rm = TRUE), # Propagate param49 SD per sample
        Machine = first(Machine)
    ) %>%
    ungroup()

# write.csv(D47stats_Machinebin, "E:/Dropbox//Research//postdoc//UNBIAS//Clumped Temperature Calibration/Aragonite_dataset_stats_by_Machine.csv")

# Now combine sample groups from different machines into one bin
D47stats <- D47stats_Machinebin %>%
    group_by(ID) %>%
    summarize(
        Temp = mean(Temp, na.rm = TRUE),
        Temp_SD = first(Temp_SD),
        Analysis = first(Analysis),
        type = first(type),
        sample = first(sample),
        D47_mean = binmeans(x = D47_binmean, x_sd = binsd, n = Nbin, output = "mean"),  # Calculate means per sample
        sd = binmeans(x = D47_binmean, x_sd = binsd, n = Nbin, output = "SD"), # Calculate stdevs per sample bin
        N = sum(Nbin, na.rm = TRUE), # Calculate the number of modelled values, excluding NA's
        se = sd / sqrt(N), # Calculate the standard error
        CL95 = qt(0.95, N) * se, # Calculate the 95% confidence level
        param49_mean = binmeans(x = param49_binmean, x_sd = param49_binsd, n = Nbin, output = "mean"),  # Calculate means per sample
        param49_sd = binmeans(x = param49_binmean, x_sd = param49_binsd, n = Nbin, output = "SD"), # Calculate stdevs per sample bin
        param49_se = param49_sd / sqrt(N),
        param49_CL95 = qt(0.95, N) * param49_se
    )

# write.csv(D47stats, "E:/Dropbox//Research//postdoc//UNBIAS//Clumped Temperature Calibration/Aragonite_dataset_stats_grouped.csv")

# Summarize stats for Arctica islandica samples per specimen

# First propagate uncertainties on bins per specimen, keeping data from different machines separate
Aisstats_Machinebin <- dat[which(dat$D47_outlier != TRUE & dat$Analysis == "this study"), ] %>% # Summarize D47 statistics
    group_by(Specimen, Machine) %>%
    summarize(        
        sample = first(sample),
        Temp = mean(Temp, na.rm = TRUE),
        Temp_SD = first(Temp_SD),
        Analysis = first(Analysis),
        type = first(type),
        sample = first(sample),
        D47_binmean = mean(D47, na.rm = TRUE),  # Calculate means per sample
        SD_bin = sd(D47, na.rm = TRUE), # Calculate stdevs per sample bin
        SD_ext = first(D47_SD), # External standard deviation (reported or based on check STD)
        binsd = max(SD_bin, SD_ext), # Find largest standard
        Nbin = n(), # Calculate the number of modelled values, excluding NA's
        binse = binsd / sqrt(Nbin), # Calculate the standard error
        binCL95 = qt(0.95, Nbin) * binse, # Calculate the 95% confidence level
        param49_binmean = mean(param49, na.rm = TRUE), # Propagate param49 mean per sample
        param49_binsd = sd(param49, na.rm = TRUE), # Propagate param49 SD per sample
        Machine = first(Machine)
    ) %>%
    ungroup()

# write.csv(Aisstats_Machinebin, "E:/Dropbox//Research//postdoc//UNBIAS//Clumped Temperature Calibration/Ais_dataset_stats_by_Machine.csv")

# Now combine sample groups from different machines into one bin
Aisstats <- Aisstats_Machinebin %>%
    group_by(Specimen) %>%
    summarize(
        Temp = mean(Temp, na.rm = TRUE),
        Temp_SD = first(Temp_SD),
        Analysis = first(Analysis),
        type = first(type),
        sample = first(sample),
        D47_mean = binmeans(x = D47_binmean, x_sd = binsd, n = Nbin, output = "mean"),  # Calculate means per sample
        sd = binmeans(x = D47_binmean, x_sd = binsd, n = Nbin, output = "SD"), # Calculate stdevs per sample bin
        N = sum(Nbin, na.rm = TRUE), # Calculate the number of modelled values, excluding NA's
        se = sd / sqrt(N), # Calculate the standard error
        CL95 = qt(0.95, N) * se, # Calculate the 95% confidence level
        param49_mean = binmeans(x = param49_binmean, x_sd = param49_binsd, n = Nbin, output = "mean"),  # Calculate means per sample
        param49_sd = binmeans(x = param49_binmean, x_sd = param49_binsd, n = Nbin, output = "SD"), # Calculate stdevs per sample bin
        param49_se = param49_sd / sqrt(N),
        param49_CL95 = qt(0.95, N) * param49_se
    )

# write.csv(Aisstats, "E:/Dropbox//Research//postdoc//UNBIAS//Clumped Temperature Calibration/Ais_dataset_stats_grouped.csv")

# Update dat with new propagated SDs for regressions
newSDs <- D47stats[which(D47stats$Analysis == "this study"), which(colnames(D47stats) %in% c("ID", "sd"))]
dat$D47_SD[which(!is.na(match(dat$ID, newSDs$ID)))] <- newSDs$sd[match(dat$ID, newSDs$ID)[which(!is.na(match(dat$ID, newSDs$ID)))]]

# Monte Carlo sample from individual aliquot distributions for violin plots and polynomial regression including errors on D47
Nsim <- 10 ^ 4
violin_data <- data.frame(sample = rep(dat$sample, Nsim),
    type = rep(dat$type, Nsim),
    Analysis = rep(dat$Analysis, Nsim),
    ID = rep(dat$ID, Nsim),
    Specimen = rep(dat$Specimen, Nsim),
    Temp = rep(dat$Temp, Nsim),
    Temp_sampled = rnorm(Nsim * length(dat$Temp), dat$Temp, dat$Temp_SD),
    D47 = rnorm(Nsim * length(dat$D47), dat$D47, dat$D47_SD),
    param49 = rnorm(Nsim * length(dat$param49), dat$param49, dat$param49_sd),
    outlier = rep(dat$D47_outlier, Nsim)
)

violin_data$x <- 10 ^ 6 / (violin_data$Temp_sampled + 273.15) ^ 2 # Create 10^6/T^2 vector

# ------------------------Pairwise comparisons----------------------------------

Aisdata <- dat[which(dat$D47_outlier != TRUE & dat$Analysis == "this study"), ] # Isolate Arctica islandica data from this study
Ais_temp_aov <- aov(D47 ~ ID, data = Aisdata) # Conduct one-way ANOVA on temperature bins
capture.output(summary(Ais_temp_aov), file = "out/is_Pairwise_comp_temp_summary.txt") # Print summary of ANOVA
TukeyHSD(Ais_temp_aov) # Print results of Tukey multiple pairwise-comparisons (post-hoc Tukey's test) on temperature bins
write.csv(TukeyHSD(Ais_temp_aov)$ID, "out/is_Pairwise_comp_temp.csv") # Export summary of Tukey multiple pairwise-comparisons

Ais_spec1_aov <- aov(D47 ~ Specimen, data = Aisdata[which(Aisdata$Temp == 1.1), ]) # Conduct one-way ANOVA on Specimen bins within the 1 degree temperature treatment
capture.output(summary(Ais_spec1_aov), file = "out/is_Pairwise_comp_spec1_summary.txt") # Print summary of ANOVA
TukeyHSD(Ais_spec1_aov) # Print results of Tukey multiple pairwise-comparisons (post-hoc Tukey's test) on specimen bins
write.csv(TukeyHSD(Ais_spec1_aov)$Specimen, "out/is_Pairwise_comp_spec1.csv") # Export summary of Tukey multiple pairwise-comparisons

Ais_spec18_aov <- aov(D47 ~ Specimen, data = Aisdata[which(Aisdata$Temp == 18.0), ]) # Conduct one-way ANOVA on Specimen bins within the 18 degree temperature treatment
capture.output(summary(Ais_spec18_aov), file = "out/is_Pairwise_comp_spec18_summary.txt") # Print summary of ANOVA
TukeyHSD(Ais_spec18_aov) # Print results of Tukey multiple pairwise-comparisons (post-hoc Tukey's test) on specimen bins
write.csv(TukeyHSD(Ais_spec18_aov)$Specimen, "out/is_Pairwise_comp_spec18.csv") # Export summary of Tukey multiple pairwise-comparisons

# ----------------------------Regressions---------------------------------------

# TLinear York regression both with and without high temperature datapoints
D47m_York <- bfsl(x = 10^6 / (dat$Temp[dat$D47_outlier == FALSE] + 273.15) ^ 2,
    y = dat$D47[dat$D47_outlier == FALSE],
    sd_x = abs(10^6 / (dat$Temp[dat$D47_outlier == FALSE] + dat$Temp_SD[dat$D47_outlier == FALSE] + 273.15) ^ 2 - 10^6 / (dat$Temp[dat$D47_outlier == FALSE] - dat$Temp_SD[dat$D47_outlier == FALSE] + 273.15) ^ 2) / 2,
    sd_y = dat$D47_SD[dat$D47_outlier == FALSE],
    r = 0)
newdat_York <- data.frame(x = 10 ^6 / (seq(0, 1000, 0.1) + 273.15) ^ 2)
D47m_York_pred <- predict(D47m_York, newdata = newdat_York, se.fit = TRUE, interval = "confidence", level = 0.95)
D47m_York_result <- cbind(newdat_York, D47m_York_pred$fit)

D47m_lowT_York <- bfsl(x = 10^6 / (dat$Temp[dat$D47_outlier == FALSE & dat$Analysis != "Muller17"] + 273.15) ^ 2,
    y = dat$D47[dat$D47_outlier == FALSE & dat$Analysis != "Muller17"],
    sd_x = abs(10^6 / (dat$Temp[dat$D47_outlier == FALSE & dat$Analysis != "Muller17"] + dat$Temp_SD[dat$D47_outlier == FALSE & dat$Analysis != "Muller17"] + 273.15) ^ 2 - 10^6 / (dat$Temp[dat$D47_outlier == FALSE & dat$Analysis != "Muller17"] - dat$Temp_SD[dat$D47_outlier == FALSE & dat$Analysis != "Muller17"] + 273.15) ^ 2) / 2,
    sd_y = dat$D47_SD[dat$D47_outlier == FALSE & dat$Analysis != "Muller17"],
    r = 0)
newdat_lowT_York <- data.frame(x = 10 ^6 / (seq(0, 100, 0.1) + 273.15) ^ 2)
D47m_lowT_York_pred <- predict(D47m_lowT_York, newdata = newdat_lowT_York, se.fit = TRUE, interval = "confidence", level = 0.95)
D47m_lowT_York_result <- cbind(newdat_lowT_York, D47m_lowT_York_pred$fit)

# Calculate linear York regression on A. islandica and all bivalve data
Ais_York <- bfsl(x = 10^6 / (dat$Temp[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica")] + 273.15) ^ 2,
    y = dat$D47[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica")],
    sd_x = abs(10^6 / (dat$Temp[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica")] + dat$Temp_SD[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica")] + 273.15) ^ 2 - 10^6 / (dat$Temp[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica")] - dat$Temp_SD[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica")] + 273.15) ^ 2) / 2,
    sd_y = dat$D47_SD[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica")],
    r = 0)
Ais_York_pred <- predict(Ais_York, newdata = newdat_lowT_York, se.fit = TRUE, interval = "confidence", level = 0.95)
Ais_York_result <- cbind(newdat_lowT_York, Ais_York_pred$fit)

Mollusk_York <- bfsl(x = 10^6 / (dat$Temp[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica" | dat$Analysis == "Caldarescu21")] + 273.15) ^ 2,
    y = dat$D47[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica" | dat$Analysis == "Caldarescu21")],
    sd_x = abs(10^6 / (dat$Temp[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica" | dat$Analysis == "Caldarescu21")] + dat$Temp_SD[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica" | dat$Analysis == "Caldarescu21")] + 273.15) ^ 2 - 10^6 / (dat$Temp[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica" | dat$Analysis == "Caldarescu21")] - dat$Temp_SD[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica" | dat$Analysis == "Caldarescu21")] + 273.15) ^ 2) / 2,
    sd_y = dat$D47_SD[dat$D47_outlier == FALSE & (dat$Analysis == "this study" | dat$ID == "A.islandica" | dat$Analysis == "Caldarescu21")],
    r = 0)
Mollusk_York_pred <- predict(Mollusk_York, newdata = newdat_lowT_York, se.fit = TRUE, interval = "confidence", level = 0.95)
Mollusk_York_result <- cbind(newdat_lowT_York, Mollusk_York_pred$fit)

# Third order polynomial regression following Müller et al., 2019 and Jautzy et al., 2020
newdat <- data.frame(Temp = seq(0, 1000, 0.1))
D47m_poly <- lm(D47 ~ poly(I(10^6 / (Temp + 273.15) ^ 2), 3), data = dat, subset = which(dat$D47_outlier == FALSE))
D47m_poly_pred <- predict.lm(D47m_poly, newdata = newdat, se.fit = TRUE, interval = "confidence", level = 0.95)
D47m_poly_result <- cbind(10^6 / (newdat + 273.15) ^2, D47m_poly_pred$fit)

# Polynomial regression with errors on D47 and Temp using MC simulation
D47m_poly_MC <- lm(D47 ~ poly(x, 3), data = violin_data, subset = which(violin_data$outlier == FALSE))
D47m_poly_MC_pred <- predict.lm(D47m_poly_MC, newdata = newdat_York, se.fit = TRUE, interval = "confidence", level = 0.95)
D47m_poly_MC_result <- cbind(newdat_York, D47m_poly_MC_pred$fit)
# Readjust 95% CL calculations for actual degrees of freedom
D47m_poly_MC_result$lwr <- D47m_poly_MC_result$fit - (D47m_poly_MC_result$fit - D47m_poly_MC_result$lwr) * sqrt(D47m_poly_MC$df.residual) / sqrt(length(violin_data$D47) / Nsim - 4)
D47m_poly_MC_result$upr <- D47m_poly_MC_result$fit + (D47m_poly_MC_result$upr - D47m_poly_MC_result$fit) * sqrt(D47m_poly_MC$df.residual) / sqrt(length(violin_data$D47) / Nsim - 4) 


# -----------------------Add Preexisting calibrations---------------------------
# Add dummy data to plot Anderson calibration
Anderson21 <- data.frame(Temp = 10 ^ 6 / (seq(0, 1000, 0.1) + 273.15) ^ 2,
    D47 = 0.0391 * 10 ^ 6 / (seq(0, 1000, 0.1) + 273.15) ^ 2 + 0.154) # Latest Anderson calibration

# Add dummy data to plot Meinicke calibration
MeinickeICDES <- data.frame(Temp = 10 ^ 6 / (seq(0, 1000, 0.1) + 273.15) ^ 2,
    D47 = 0.0397 * 10 ^ 6 / (seq(0, 1000, 0.1) + 273.15) ^ 2 + 0.1518) # Recalculated Meinicke calibration

# Add theoretical calcite and aragonite calibration lines by Guo et al. 2009
Guo09 <- data.frame(Temp = 10 ^ 6 / (seq(0, 1000, 0.1) + 273.15) ^ 2,
    D47_cc = -3.33040 * 10 ^ 9 / (seq(0, 1000, 0.1) + 273.15) ^ 4 + 2.32415 * 10 ^ 7 / (seq(0, 1000, 0.1) + 273.15) ^ 3 - 2.91282 * 10 ^ 3 / (seq(0, 1000, 0.1) + 273.15) ^ 2 - 5.54042 / (seq(0, 1000, 0.1) + 273.15) + 0.23252,
    D47_ar = -3.43068 * 10 ^ 9 / (seq(0, 1000, 0.1) + 273.15) ^ 4 + 2.35766 * 10 ^ 7 / (seq(0, 1000, 0.1) + 273.15) ^ 3 - 8.06003 * 10 ^ 3 / (seq(0, 1000, 0.1) + 273.15) ^ 2 - 6.90300 / (seq(0, 1000, 0.1) + 273.15) + 0.22893)

# Arctica islandica plot (Figure 1)
source("05_A_islandica_plot.r")

# Full aragonite dataset plot (Figure 2)
source("06_Full_aragonite_plot.r")

# Summarize regression stats
regstats <- data.frame(regression = c("All data York", "low T York", "A. islandica York", "Mollusk York"),
    slope = c(D47m_York$coefficients[[2]], D47m_lowT_York$coefficients[[2]], Ais_York$coefficients[[2]], Mollusk_York$coefficients[[2]]),
    slope_SE = c(D47m_York$coefficients[[4]], D47m_lowT_York$coefficients[[4]], Ais_York$coefficients[[4]], Mollusk_York$coefficients[[4]]),
    intercept = c(D47m_York$coefficients[[1]], D47m_lowT_York$coefficients[[1]], Ais_York$coefficients[[1]], Mollusk_York$coefficients[[1]]),
    intercept_SE = c(D47m_York$coefficients[[3]], D47m_lowT_York$coefficients[[3]], Ais_York$coefficients[[3]], Mollusk_York$coefficients[[3]]),
    SER = c(sqrt(sum(D47m_York$residuals ^ 2) / D47m_York$df.residual), sqrt(sum(D47m_lowT_York$residuals ^ 2) / D47m_lowT_York$df.residual), sqrt(sum(Ais_York$residuals ^ 2) / Ais_York$df.residual), sqrt(sum(Mollusk_York$residuals ^ 2) / Mollusk_York$df.residual))
)

regstats_poly <- data.frame(regression = c("Polynomial fit_means", "Polynomial fit_MC"),
    Intercept = c(summary(D47m_poly)$coefficients[1, 1], summary(D47m_poly_MC)$coefficients[1, 1]),
    Intercept_SE = c(summary(D47m_poly)$coefficients[1, 2], summary(D47m_poly_MC)$coefficients[1, 2]),
    Term1 = c(summary(D47m_poly)$coefficients[2, 1], summary(D47m_poly_MC)$coefficients[2, 1]),
    Term1_SE = c(summary(D47m_poly)$coefficients[2, 2], summary(D47m_poly_MC)$coefficients[2, 2]),
    Temm2 = c(summary(D47m_poly)$coefficients[3, 1], summary(D47m_poly_MC)$coefficients[3, 1]),
    Term2_SE = c(summary(D47m_poly)$coefficients[3, 2], summary(D47m_poly_MC)$coefficients[3, 2]),
    Term3 = c(summary(D47m_poly)$coefficients[4, 1], summary(D47m_poly_MC)$coefficients[4, 1]),
    Term3_SE = c(summary(D47m_poly)$coefficients[4, 2], summary(D47m_poly_MC)$coefficients[4, 2]),
    SER = c(sigma(D47m_poly), sigma(D47m_poly_MC))
)

# Export summary of regression stats
# write.csv(regstats, "<path>/Aragonite_calibration_regression_statistics.csv")
# write.csv(regstats_poly, "<path>/Aragonite_calibration_polynomial_regression_statistics.csv")

# -------------------------Regression residuals---------------------------------
# Extract D47 values and their errors for temperatures of all samples
newdat_York <- data.frame(x = 10 ^6 / (dat$Temp + 273.15) ^ 2)
dat$D47res_York <- dat$D47 - predict(D47m_York, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
dat$D47res_lowT_York <- dat$D47 - predict(D47m_lowT_York, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
dat$D47res_poly <- dat$D47 - predict(D47m_poly_MC, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
dat$D47res_Anderson <- dat$D47 - (0.0391 * 10 ^ 6 / (dat$Temp + 273.15) ^ 2 + 0.154)
dat$D47res_Meinicke <- dat$D47 - (0.0397 * 10 ^ 6 / (dat$Temp + 273.15) ^ 2 + 0.1518)

newdat_York <- data.frame(x = 10 ^6 / (D47stats$Temp + 273.15) ^ 2)
D47stats$D47res_York <- D47stats$D47_mean - predict(D47m_York, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
D47stats$D47res_lowT_York <- D47stats$D47_mean - predict(D47m_lowT_York, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
D47stats$D47res_poly <- D47stats$D47_mean - predict(D47m_poly_MC, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
D47stats$D47res_Anderson <- D47stats$D47_mean - (0.0391 * 10 ^ 6 / (D47stats$Temp + 273.15) ^ 2 + 0.154)
D47stats$D47res_Meinicke <- D47stats$D47_mean - (0.0397 * 10 ^ 6 / (D47stats$Temp + 273.15) ^ 2 + 0.1518)

newdat_York <- data.frame(x = 10 ^6 / (Aisstats$Temp + 273.15) ^ 2)
Aisstats$D47res_York <- Aisstats$D47_mean - predict(D47m_York, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
Aisstats$D47res_lowT_York <- Aisstats$D47_mean - predict(D47m_lowT_York, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
Aisstats$D47res_poly <- Aisstats$D47_mean - predict(D47m_poly_MC, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
Aisstats$D47res_Anderson <- Aisstats$D47_mean - (0.0391 * 10 ^ 6 / (Aisstats$Temp + 273.15) ^ 2 + 0.154)
Aisstats$D47res_Meinicke <- Aisstats$D47_mean - (0.0397 * 10 ^ 6 / (Aisstats$Temp + 273.15) ^ 2 + 0.1518)

newdat_York <- data.frame(x = 10 ^6 / (violin_data$Temp + 273.15) ^ 2)
violin_data$D47res_York <- violin_data$D47 - predict(D47m_York, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
violin_data$D47res_lowT_York <- violin_data$D47 - predict(D47m_lowT_York, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
violin_data$D47res_poly <- violin_data$D47 - predict(D47m_poly_MC, newdata = newdat_York, se.fit = TRUE, interval = "none", level = 0.95)$fit
violin_data$D47res_Anderson <- violin_data$D47 - (0.0391 * 10 ^ 6 / (violin_data$Temp + 273.15) ^ 2 + 0.154)
violin_data$D47res_Meinicke <- violin_data$D47 - (0.0397 * 10 ^ 6 / (violin_data$Temp + 273.15) ^ 2 + 0.1518)

D47m_York_result_res <- D47m_York_result - D47m_York_result$fit
D47m_York_result_res$x <- D47m_York_result_res$x + D47m_York_result$fit
D47m_York_result_res$Anderson <- Anderson21$D47 - D47m_York_result$fit
D47m_York_result_res$Meinicke <- MeinickeICDES$D47 - D47m_York_result$fit
D47m_lowT_York_result_res <- D47m_lowT_York_result - D47m_lowT_York_result$fit
D47m_lowT_York_result_res$x <- D47m_lowT_York_result_res$x + D47m_lowT_York_result$fit
D47m_lowT_York_result_res$Anderson <- Anderson21$D47[1:1001] - D47m_lowT_York_result$fit
D47m_lowT_York_result_res$Meinicke <- MeinickeICDES$D47[1:1001] - D47m_lowT_York_result$fit
D47m_poly_MC_result_res <- D47m_poly_MC_result - D47m_poly_MC_result$fit
D47m_poly_MC_result_res$x <- D47m_poly_MC_result_res$x + D47m_poly_MC_result$fit
D47m_poly_MC_result_res$Anderson <- Anderson21$D47 - D47m_poly_MC_result$fit
D47m_poly_MC_result_res$Meinicke <- MeinickeICDES$D47 - D47m_poly_MC_result$fit

# Export summary of regression residuals
# write.csv(Aisstats, "<path>/Arctica_dataset_propagated_stats.csv")
# write.csv(D47stats, "<path>/Aragonite_dataset_propagated_stats.csv")

# Plot of residuals with respect to regressions (Figure 3)
source("07_Aragonite_residual_plot.r")

# Prepare summary of calibration offsets
calibration_offset <- data.frame(D47_offset = c(dat$D47res_Anderson[which(dat$type == "bivalve" & (dat$Analysis == "this study" | dat$Analysis == "Bernasconi18"))], dat$D47res_Meinicke[which(dat$type == "bivalve" & (dat$Analysis == "this study" | dat$Analysis == "Bernasconi18"))]),
    D47_SD = rep(dat$D47_SD[which(dat$type == "bivalve" & (dat$Analysis == "this study" | dat$Analysis == "Bernasconi18"))], 2),
    Calibration = c(rep("Anderson et al., 2021", length(dat$D47res_Meinicke[which(dat$type == "bivalve" & (dat$Analysis == "this study" | dat$Analysis == "Bernasconi18"))])), rep("Meinicke et al., 2020", length(dat$D47res_Meinicke[which(dat$type == "bivalve" & (dat$Analysis == "this study" | dat$Analysis == "Bernasconi18"))])))
)

# Calculate the temperature equivalent of the calibration offset
mean_Temp <- mean(dat$Temp[which(dat$type == "bivalve" & (dat$Analysis == "this study" | dat$Analysis == "Bernasconi18"))])
mean_D47 <- 0.0391 * 10 ^ 6 / (mean_Temp + 273.13) ^ 2 + 0.154

# Statistics of difference with calibration
Calibration_offset_stats <- calibration_offset %>% # Summarize D47 offset statistics
    group_by(Calibration) %>%
    summarize(
        N = n(), # Calculate the number of modelled values, excluding NA's
        D47_offset_Average = binmeans(x = D47_offset, x_sd = D47_SD, output = "mean"),
        D47_offset_SD = binmeans(x = D47_offset, x_sd = D47_SD, output = "SD"),
        D47_offset_SE = D47_offset_SD / sqrt(N - 1), # Calculate the standard error
        D47_offset_CL95 = qt(0.95, N - 1) * D47_offset_SE # Calculate the 95% confidence level
    ) %>%
    ungroup()

Calibration_offset_stats$Temp_offset_Average <- sqrt(0.0391 * 10 ^ 6 / (mean_D47 - Calibration_offset_stats$D47_offset_Average - 0.154)) - 273.15 - mean_Temp
Calibration_offset_stats$Temp_offset_SD <- sqrt(0.0391 * 10 ^ 6 / (mean_D47 - Calibration_offset_stats$D47_offset_Average - Calibration_offset_stats$D47_offset_SD - 0.154)) - 273.15 - mean_Temp - Calibration_offset_stats$Temp_offset_Average
Calibration_offset_stats$Temp_offset_SE <- sqrt(0.0391 * 10 ^ 6 / (mean_D47 - Calibration_offset_stats$D47_offset_Average - Calibration_offset_stats$D47_offset_SE - 0.154)) - 273.15 - mean_Temp - Calibration_offset_stats$Temp_offset_Average
Calibration_offset_stats$Temp_offset_CL95 <- sqrt(0.0391 * 10 ^ 6 / (mean_D47 - Calibration_offset_stats$D47_offset_Average - Calibration_offset_stats$D47_offset_CL95 - 0.154)) - 273.15 - mean_Temp - Calibration_offset_stats$Temp_offset_Average

# write.csv(Calibration_offset_stats, "<path>/Calibration_offset_stats.csv")

# Plot calibration offsets (Figure 4)
source("08_Calibration_offset_plot.r")
