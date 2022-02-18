# Function to combine binned data based on the mean and standard deviation of the bins and their sample sizes.
# Based on: "Combining Multiple binmeand Data Points And Their Errors" by Ken Tatebe (http://docplayer.net/33088897-Combining-multiple-binmeand-data-points-and-their-errors.html)
# And: "Data Analyis Toolkit #12" by James Kirchner (http://seismo.berkeley.edu/~kirchner/Toolkits/Toolkit_12.pdf)
# Approach for combining more than two bins while weighting uncertainty and means by sample size and variance within the bin
# Worked out by Barbara Goudsmit (see SI document)

binmeans <- function(x, x_sd = NA, n = NA, verbose = FALSE, output = "All"){
    if(is.na(n)){
        n = rep(1, length(x)) # If no N are given, the bins are assumed to have equal sample sizes
    }
    if(is.na(x_sd)){
        x_sd = rep(1, length(x)) # If no SDs of the bins are given, the bins are assumed to have equal variance
    }
    if(length(n) == 1){
        N = n[1] # In case only one entry is provided, the mean and SD of the one entry are returned as the mean and SD of the result
        binmean = x[1]
        SD_bin = x_sd[1]
    }else{
        N <- sum(n) # Calculate total sample size
        binmean <- sum(x * n * x_sd ^ -2) / sum(n * x_sd ^ -2)  # Calculate weighted binmean (equation 16 in SI document)
        SD_bin <- sqrt(sum(n) / (sum(n) - 1) * sum((n - 1) + x_sd ^ -2 * n * (x - binmean) ^ 2) / sum(n * x_sd ^ -2)) # Calculate weighted standard deviation (equation 17 in SI document)
    }
    resultvec <- c(binmean, SD_bin, N)
    names(resultvec) <- c("binmean", "sd", "N")
    if(output == "SD"){
        return(resultvec[2])
    }else if(output == "mean"){
        return(resultvec[1])
    }else if(output == "All"){
        return(resultvec)
    }else{
        return("Error: output string not recognized")
    }
}
