# Function to propagate errors on individual aliquots through the D47 averages
# Based on: "Combining Multiple Averaged Data Points And Their Errors" by Ken Tatebe
# Iterative approach adding datapoints and propagating uncertainty

average_p <- function(x, x_sd, n = NA, verbose = FALSE, output = "SD"){
    if(is.na(n)){
        n = rep(1, length(x))
    }
    N <- n[1] + n[2] # Calculate sample size for first 2 samples
    average <- (x[1] * n[1] + x[2] * n[2]) / N # Calculate weighted average for first 2 samples
    sd_p <- sqrt(n[1]) # Calculate weighted standard deviation for first 2 samples
    for(i in 3:length(x)){ # Loop over x and their uncertainties x_sd
        sd_p <- sqrt(
            (N ^ 2 - N) / ((N + n[i]) ^ 2 - (N + n[i])) * sd_p ^ 2 + # Existing error term
            (n[i] ^ 2 - n[i]) / ((N + n[i]) ^ 2 - (N + n[i])) * x_sd[i] ^ 2 + # Error term of new datapoint
            N * n[i] * (average - x[i]) ^ 2 / ((N + n[i]) * ((N + n[i]) ^ 2 - (N + n[i]))) # Term resulting from distance between points
        )
        average <- (average * N + x[i] * n[i]) / (N + n[i]) # Increment total average
        N <- N + n[i] # Increment total sample size
    }
    resultvec <- c(average, sd_p, N)
    names(resultvec) <- c("average", "sd", "N")
    if(output == "SD"){
        return(resultvec[2])
    } else if(output == "All"){
        return(resultvec)
    } else{
        return("Error: output string not recognized")
    }
}

# Repeat calculations based on Monte Carlo simulations

propagate_MC <- function(x, x_sd, n = NA, Nsim = 10 ^ 4, verbose = FALSE, na.rm = TRUE, output = "SD"){ # Monte Carlo approach to estimate same errors
    if(is.na(n)){
        n = rep(1, length(x))
    }

    sims <- rep(NA, sum(Nsim * n)) # Vector to store simulated values
    for(i in 1:length(x)){
        sims[which(is.na(sims))[1]:(which(is.na(sims))[1] + Nsim * n[i] - 1)] <- rnorm(Nsim * n[i], x[i], x_sd[i]) # Add simulated values to vector
    }

    if(na.rm == TRUE){
        if(length(which(is.na(sims))) > 0){
            sims <- sims[-which(is.na(sims))] # Remove NA's from sims if called for
        }
    }

    average <- mean(sims) # Calculate average
    sd_p <- sd(sims) # Calculate standard deviation
    N <- length(sims) / Nsim # Calculate effective sample size

    resultvec <- c(average, sd_p, N)
    names(resultvec) <- c("average", "sd", "N")

    if(verbose == TRUE){
        print(resultvec)
    }
    if(output == "SD"){
        return(resultvec[2])
    } else if(output == "average"){
        return(resultvec[1])
    } else if(output == "All"){
        return(resultvec)
    } else if(output == "sims"){
        return(sims)
    } else{
        return("Error: output string not recognized")
    }
}
