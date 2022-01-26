# Calculate temperature uncertainty and offset relative to pre-existing calibrations

Temperature_offset <- function(D47_mean, D47_err, known_T, slope, int){
    mean_T <- sqrt(slope * 10 ^ 6 / (D47_mean - int)) - 273.15
    diff_T <- mean_T - known_T
    T_upr <- sqrt(slope * 10 ^ 6 / (D47_mean - D47_err - int)) - 273.15
    diff_T_upr <- T_upr - known_T
    T_lwr <- sqrt(slope * 10 ^ 6 / (D47_mean + D47_err - int)) - 273.15
    diff_T_lwr <- T_lwr - known_T
    T_err <- (T_upr - T_lwr) / 2
    result <- c(mean_T, T_upr, T_lwr, diff_T, diff_T_upr, diff_T_lwr, T_err)
    names(result) <- c("reconstructed Temperature", "max reconstructed Temperature", "min reconstructed Temperature", "Temperature offset", "max Temperature offset", "min Temperature offset", "Temperature uncertainty")
    return(result)
}