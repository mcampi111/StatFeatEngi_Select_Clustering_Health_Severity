#############################
#Functions for bootstraping #
#############################

meanfun <- function(x, d) {
  return(mean(x[d], na.rm=TRUE))
}

varfun <- function(x, d) {
  return(var(x[d], na.rm=TRUE))
}

datafun <- function(data, indices) {
  resampled_data <- data[indices]
  return(resampled_data)
}

rank_transformfun <- function(data, indices) {
  bootstrap_data <- data[indices, ]
  rank_vector <- c(rank(bootstrap_data[, 1]), rank(bootstrap_data[, 2]))
  return(rank_vector)
}

rank_transformfun2 <- function(x, d) {
  ecdf_fun <- ecdf(x)
  return(ecdf_fun(x[d]))
}


###################
#CASE 1 --> NORMAL#
###################


my.mle_fun<- function(data){
  estimate <- mean(data, na.rm = TRUE)
  sd_estimate <- sd(data, na.rm = TRUE)
  return(list(estimate = estimate, sd = sd_estimate))
}


ran.gen_mle <- function(n, estimate, sd_estimate) {
  # Generate random samples using a normal distribution
  # based on the MLE estimate and standard deviation estimate
  if (is.na(sd_estimate) || sd_estimate <= 0) {
    return(rep(mean_estimate,n))
  } else {
    samples <- rnorm(n, mean = estimate, sd = sd_estimate)
    return(samples)
  }
  
}




custom_boot <- function(data, indices, mle_function, ran.gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))  
  
  mle_estimate <- mle_function(data[indices])
  
  if (is.numeric(mle_estimate)) {
    # Handle cases where mle_estimate is an atomic vector (numeric)
    estimate <- mle_estimate
    sd_estimate <- NA  # No standard deviation available
  } else {
    # Handle cases where mle_estimate is a list with "estimate" and "sd"
    estimate <- mle_estimate$estimate
    sd_estimate <- mle_estimate$sd
  }
  
  resampled_data <- ran.gen_function(length(indices), estimate, sd_estimate)
  
  return(get(stat)(resampled_data))
}



custom_boot2 <- function(data, indices, mle_function, ran.gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))  
  
  mle_data <- mle_function(data[indices, ])
  
  if (is.data.frame(mle_data)) {
    # Handle cases where mle_data is a data frame with ranked statistics
    resampled_data1 <- ran.gen_function(length(indices), mle_data$estimate[1], mle_data$sd[1])
    resampled_data2 <- ran.gen_function(length(indices), mle_data$estimate[2], mle_data$sd[2])
    resampled_data <- cbind(resampled_data1, resampled_data2)
  } else if (is.list(mle_data)) {
    # Handle cases where mle_data is a list with "estimate" and "sd"
    resampled_data1 <- ran.gen_function(length(indices), mle_data$estimate, mle_data$sd)
    resampled_data2 <- ran.gen_function(length(indices), mle_data$estimate, mle_data$sd)
    resampled_data <- cbind(resampled_data1, resampled_data2)
  } else if (is.numeric(mle_data)) {
    # Handle cases where mle_data is an atomic vector (numeric)
    resampled_data <- ran.gen_function(length(indices), mle_data, NA)
  } else {
    stop("Unsupported mle_data format")
  }
  
  return(get(stat)(resampled_data))
}

#################
#CASE 2 --> BETA#
#################

beta_mle_fun <- function(data) {
  # Calculate MLE parameters for a beta distribution
  shape1 <- sum(data/100) + 1
  shape2 <- length(data) - sum(data/100) + 1
  return(list(shape1 = shape1, shape2 = shape2))
}


ran.gen_mle_beta <- function(n, shape1, shape2) {
  if (is.na(shape2) || is.na(shape1) || shape2 <= 0) {
    return(rep(shape1, n))
  } else {
    samples <- rbeta(n, shape1 = shape1, shape2 = shape2)
    # Scale the resample to the 0-100% range if needed
    scaled_resample <- samples * 100
    #scaled_resample <- samples
    return(scaled_resample)
  }
}



custom_boot_beta <- function(data, indices, mle_function, ran.gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))  
  
  mle_estimate <- mle_function(data[indices])
  
  if (is.numeric(mle_estimate)) {
    # Handle cases where mle_estimate is an atomic vector (numeric)
    shape1 <- shape1
    shape2 <- NA  # No shape2 available
  } else {
    # Handle cases where mle_estimate is a list with "shape1" and "shape2"
    shape1 <- mle_estimate$shape1
    shape2 <- mle_estimate$shape2
  }
  
  resampled_data <- ran.gen_function(length(indices), shape1, shape2)
  
  return(get(stat)(resampled_data))
}



custom_boot2_beta <- function(data, indices, mle_function, ran.gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))  
  
  mle_data <- mle_function(data[indices, ])
  
  if (is.data.frame(mle_data)) {
    # Handle cases where mle_data is a data frame with ranked statistics
    resampled_data1 <- ran.gen_function(length(indices), mle_data$shape1[1], mle_data$shape2[1])
    resampled_data2 <- ran.gen_function(length(indices), mle_data$shape1[2], mle_data$shape2[2])
    resampled_data <- cbind(resampled_data1, resampled_data2)
  } else if (is.list(mle_data)) {
    # Handle cases where mle_data is a list with "shape1" and "shape2"
    resampled_data1 <- ran.gen_function(length(indices), mle_data$shape1, mle_data$shape2)
    resampled_data2 <- ran.gen_function(length(indices), mle_data$shape1, mle_data$shape2)
    resampled_data <- cbind(resampled_data1, resampled_data2)
  } else if (is.numeric(mle_data)) {
    # Handle cases where mle_data is an atomic vector (numeric)
    resampled_data <- ran.gen_function(length(indices), mle_data, NA)
  } else {
    stop("Unsupported mle_data format")
  }
  
  return(get(stat)(resampled_data))
}


custom_beta_unscreen <- function(data, indices, mle_function, ran.gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))
  
  num_indices <- length(indices)
  num_components <- ncol(data)
  resampled_data <- matrix(NA, nrow = num_indices, ncol = num_components)
  
  for (i in 1:num_indices) {
    # Extract the subset of data based on the indices
    resampled_subset <- data[indices[i], ]
    
    # Calculate MLE parameters for the resampled data
    mle_data <- mle_function(resampled_subset)
    
    if (is.numeric(mle_data)) {
      # Handle cases where mle_data is an atomic vector (numeric)
      resampled_data[i, ] <- ran.gen_function(num_components, mle_data, NA)
    } else {
      # Handle other cases (e.g., data frames or lists)
      shape1 <- mle_data$shape1
      shape2 <- mle_data$shape2
      resampled_data[i, ] <- ran.gen_function(num_components, shape1, shape2)
    }
  }
  
  return(get(stat)(resampled_data))
}



######################################
#CASE 3 --> Logit-Normal Distribution#
######################################

logit_normal_mle_fun <- function(data) {
  # Replace NAs with a placeholder value (e.g., 0)
  data <- ifelse(is.na(data), 0, data)
  
  # Calculate MLE parameters for a logit-normal distribution
  log_data <- log1p(data / 100 / (1 - data / 100))
  mean_logit <- mean(log_data, na.rm = TRUE)
  var_logit <- var(log_data, na.rm = TRUE)
  
  # Convert back to the original scale
  mean_original <- exp(mean_logit) / (1 + exp(mean_logit))
  sd_original <- sqrt((exp(var_logit) - 1) * exp(2 * mean_logit + var_logit))
  
  return(list(mean = mean_original, sd = sd_original))
}


ran.gen_mle_logit_normal <- function(n, mean, sd) {
  if (is.na(sd) || sd <= 0) {
    return(rep(NA, n))
  } else {
    logit_values <- rnorm(n, mean = log(mean / (1 - mean)), sd = sd)
    original_values <- exp(logit_values) / (1 + exp(logit_values))
    return(original_values)
  }
}


# Custom Boot Function for Logit-Normal Distribution
custom_boot_logit_normal <- function(data, indices, mle_function, ran.gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))  
  
  mle_parameters <- mle_function(data[indices])
  
  estimate <- mle_parameters$mean
  sd_estimate <- mle_parameters$sd
  
  resampled_data <- ran.gen_function(length(indices), estimate, sd_estimate)
  
  return(resampled_data)
}

# Custom Boot Function for Logit-Normal Distribution with Two Components
custom_boot2_logit_normal <- function(data, indices, mle_function, ran.gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))  
  
  mle_parameters <- mle_function(data[indices, ])
  
  estimate1 <- mle_parameters$mean[1]
  sd_estimate1 <- mle_parameters$sd[1]
  
  estimate2 <- mle_parameters$mean[2]
  sd_estimate2 <- mle_parameters$sd[2]
  
  resampled_data1 <- ran.gen_function(length(indices), estimate1, sd_estimate1)
  resampled_data2 <- ran.gen_function(length(indices), estimate2, sd_estimate2)
  
  resampled_data <- cbind(resampled_data1, resampled_data2)
  
  return(resampled_data)
}



################################
# 4 Beta-Binomial Distribution #
################################


beta_binomial_mle_fun <- function(data) {
  # Calculate MLE parameters for a Beta-Binomial distribution
  n <- length(data)
  sum_x <- sum(data/100)
  alpha_hat <- sum_x + 1
  beta_hat <- n - sum_x + 1
  
  return(list(alpha = alpha_hat, beta = beta_hat))
}




ran.gen_mle_beta_binomial <- function(n, alpha, beta) {
  # Generate random samples using a Beta-Binomial distribution
  samples <- rbeta(n, shape1 = alpha, shape2 = beta)
  # Scale the resample to the desired range if needed (e.g., 0-100%)
  scaled_resample <- samples * 100
  return(scaled_resample)
}



custom_boot_bin_beta <- function(data, indices, mle_function, ran.gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))  
  
  mle_estimate <- mle_function(data[indices])
  
  if (is.numeric(mle_estimate)) {
    # Handle cases where mle_estimate is an atomic vector (numeric)
    shape1 <- mle_estimate
    shape2 <- NA  # No shape2 available
  } else {
    # Handle cases where mle_estimate is a list with "alpha" and "beta"
    shape1 <- mle_estimate$alpha
    shape2 <- mle_estimate$beta
  }
  
  resampled_data <- ran.gen_function(length(indices), shape1, shape2)
  
  return(get(stat)(resampled_data))
}

custom_boot2_bin_beta <- function(data, indices, mle_function, ran.gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))  
  
  mle_data <- mle_function(data[indices, ])
  
  if (is.data.frame(mle_data)) {
    # Handle cases where mle_data is a data frame with ranked statistics
    resampled_data1 <- ran.gen_function(length(indices), mle_data$alpha[1], mle_data$beta[1])
    resampled_data2 <- ran.gen_function(length(indices), mle_data$alpha[2], mle_data$beta[2])
    resampled_data <- cbind(resampled_data1, resampled_data2)
  } else if (is.list(mle_data)) {
    # Handle cases where mle_data is a list with "alpha" and "beta"
    resampled_data1 <- ran.gen_function(length(indices), mle_data$alpha, mle_data$beta)
    resampled_data2 <- ran.gen_function(length(indices), mle_data$alpha, mle_data$beta)
    resampled_data <- cbind(resampled_data1, resampled_data2)
  } else if (is.numeric(mle_data)) {
    # Handle cases where mle_data is an atomic vector (numeric)
    resampled_data <- ran.gen_function(length(indices), mle_data, NA)
  } else {
    stop("Unsupported mle_data format")
  }
  
  return(get(stat)(resampled_data))
}

###################################
#CASE 5 --> Zero-Inflated Poisson #
###################################

# Custom MLE function for Zero-Inflated Poisson (ZIP) distribution
zip_mle_fun <- function(data) {
  # Fit a ZIP model to the data and extract parameters
  zip_model <- zeroinfl(data ~ 1 | 1, dist = "poisson")
  return(list(pi = coef(zip_model)["(Intercept)"], lambda = coef(zip_model)["count:(Intercept)"]))
}

# Custom random data generation function for ZIP distribution
zip_ran_gen_fun <- function(n, pi, lambda) {
  # Generate zeros and counts separately based on pi and lambda
  zeros <- rbinom(n, size = 1, prob = pi)
  counts <- ifelse(zeros == 0, rpois(n, lambda = lambda), 0)
  return(counts)
}


# Custom bootstrapping function for Zero-Inflated Poisson (ZIP) distribution
custom_boot_zip <- function(data, indices, mle_function, ran_gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))
  
  mle_estimate <- mle_function(data[indices])
  pi <- mle_estimate$pi
  lambda <- mle_estimate$lambda
  
  # Generate ZIP data based on estimated parameters
  resampled_data <- ran_gen_function(length(indices), pi, lambda)
  
  return(get(stat)(resampled_data))  # Modify this as needed for your specific statistic
}


# Custom bootstrapping function for Zero-Inflated Poisson distribution
custom_boot2_zip <- function(data, indices, mle_function, ran_gen_function, stat = "mean") {
  if (length(indices) == 0) return(rep(NA, length(data)))  
  
  mle_data <- mle_function(data[indices, ])
  
  if (is.data.frame(mle_data)) {
    # Handle cases where mle_data is a data frame with ranked statistics
    resampled_data1 <- ran_gen_function(length(indices), mle_data$p_zero[1], mle_data$lambda[1])
    resampled_data2 <- ran_gen_function(length(indices), mle_data$p_zero[2], mle_data$lambda[2])
    resampled_data <- cbind(resampled_data1, resampled_data2)
  } else if (is.list(mle_data)) {
    # Handle cases where mle_data is a list with "p_zero" and "lambda"
    resampled_data1 <- ran_gen_function(length(indices), mle_data$p_zero, mle_data$lambda)
    resampled_data2 <- ran_gen_function(length(indices), mle_data$p_zero, mle_data$lambda)
    resampled_data <- cbind(resampled_data1, resampled_data2)
  } else if (is.numeric(mle_data)) {
    # Handle cases where mle_data is an atomic vector (numeric)
    resampled_data <- ran_gen_function(length(indices), mle_data, NA)
  } else {
    stop("Unsupported mle_data format")
  }
  
  return(get(stat)(resampled_data))
}

