###################################################
# R script for the blog post "Demystifying Density Estimation for Designers"
# Author: Logan D. Clark
# Website: https://logandclark.com

# Created: May 2021
# Final Project Submission for SYS-6018 (Data Mining) at the University of Virginia
###################################################



#------------------------------------------------------------------------#
# Setup
#------------------------------------------------------------------------#


# Required Packages -------------------------------------------------------
# install.packages("tidyverse")
# install.oackages("skimr")
# install.packages("fitdistrplus")
# install.packages("ks")
# install.packages("mixtools")

library(tidyverse)      # Data handling
library(skimr)          # Variable summaries
library(fitdistrplus)   # Parametrid density estimation
library(ks)             # Kernel density estimation
library(mixtools)       # For mixture models




# Load Data ---------------------------------------------------------------

url <- "https://query.data.world/s/3cx3sheimvijxdcnfzba6pvouobnm4"
nba <- read_csv(url)



# Clean Data --------------------------------------------------------------

nba <- nba %>%
  # Make some of the variable names R-Friendly
  rename(
    "Index"               = "X1",
    "PickNumber"          = "Draft pick",
    "HeightNoShoes"       = "Height (No Shoes)",
    "HeightWithShoes"     = "Height (With Shoes)",
    "StandingReach"       = "Standing reach",
    "VerticalMax"         = "Vertical (Max)",
    "VerticalMaxReach"    = "Vertical (Max Reach)",
    "VerticalNoStep"      = "Vertical (No Step)",
    "VerticalNoStepReach" = "Vertical (No Step Reach)",
    "BodyFat"             = "Body Fat",
    "HandLength"          = "Hand (Length)",
    "HandWidth"           = "Hand (Width)"
  )

# Check for missing values
skimr::skim(nba)

# For this tutorial, we'll use the following variables
  # HeightNoShoes  : Player height without shoes
  # Wingspan       : Player wingspan (distance from fingertip of one hand to fingertip of other hand)
  # StandingReach  : Player standing reach (distance from ground to fingertip with arm extended upward)



# Generate Example Data: Bimodal Distribution -----------------------------

# Generate example data with a bimodal probability distribution
# Data sampled from a two component gaussian mixture model

# Set parameters
params_1 <- c(mean = 40, sd = 10) # Mean and SD of first component
params_2 <- c(mean = 10, sd = 8)  # Mean and SD of second component
weights <-  c(0.4, 0.6)           # Mixture weights
n <- 500                          # Number of observations to generate

# Generate group labels
set.seed(1899)
group_labels <- sample(x = c(1, 2), size = n, replace = TRUE, prob = weights)

# Sample from the density function associated with each group label
XVals <- if_else(group_labels == 1,
                 rnorm(n, mean = params_1[1], sd = params_1[2]),
                 rnorm(n, mean = params_2[1], sd = params_2[2])
)

bimodal_data <- tibble(XVals = XVals)

# Check the result
ggplot(data = bimodal_data) +
  geom_histogram(aes(x = XVals))




# Generate Example Data: Skewed Distribution ------------------------------

# Generate example data with a skewed right distribution
# Data sampled from an exponential distribution

set.seed(1890)
skewed_data <- tibble(
  XVals = rexp(500, rate  = 2)
)

# Check the result
ggplot(data = skewed_data) +
  geom_histogram(aes(x = XVals))




# Generate Example Data: Outliers -----------------------------------------

# Generate example data with 5 prominent outliers
# Data sampled from a normal distribution, with outlier values added in

# Generate data
set.seed(1887)
outlier_data <- tibble(
  XVals = rnorm(495, mean = 40, sd = 10)
)

# Generate three outlier observations
set.seed(1888)
outliers <- tibble(
  XVals = rnorm(5, mean = 100, sd = 5)
)

# Add outliers
outlier_data <- bind_rows(outlier_data, outliers)


# Check the result
ggplot(data = outlier_data) +
  geom_histogram(aes(x = XVals))



#------------------------------------------------------------------------#
# Introduction
#------------------------------------------------------------------------#

# Example density curve
ggplot(nba) +
  geom_density(aes(x = HeightNoShoes)) +
  xlab("Height")



# What is Density Estimation? ---------------------------------------------


# Figure: KDE for nba:HeightNoShoes
ggplot(nba) +
  geom_density(aes(x = HeightNoShoes, y = ..density..), color = "blue") +
  geom_point(aes(x = HeightNoShoes), y = 0, shape = 3) +
  geom_hline(yintercept = 0)



# Important Properties of Statistical Models ------------------------------

# Linear Relationship Example
plt_linear_relationship <- ggplot(nba, aes(x = HeightNoShoes, y = Wingspan)) +
  geom_point() +
  xlab("Player Height") +
  ylab("Wingspan")
plt_linear_relationship

ggsave("Linear Relationship.png")


# Linear Model Fit Examples

# Height by Wingspan
plt_linear_relationship +
  geom_smooth(formula = y ~ x, method = "lm")

ggsave("Linear Model Example 1.png")

#Height by Weight
ggplot(nba, aes(x = HeightNoShoes, y = Weight)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  xlab("Player Height") +
  ylab("Weight")

ggsave("Linear Model Example 2.png")

#Height by HandLength
ggplot(nba, aes(x = HeightNoShoes, y = VerticalMax)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  xlab("Player Height") +
  ylab("Max Vertical Leap")

ggsave("Linear Model Example 3.png")





#------------------------------------------------------------------------#
# Parametric Density Estimation
#------------------------------------------------------------------------#

# Gaussian Probability Density Plots --------------------------------------

# Generate data for plots
gaussian_curve_examples <- tibble(
  XVals = seq(0, 100, 1),                     # Grid of x values
  Mean_50_SD_20 = dnorm(XVals, mean = 50, sd = 20),  # Three different normal distributions
  Mean_40_SD_10 = dnorm(XVals, mean = 40, sd = 10),
  Mean_60_SD_5 = dnorm(XVals, mean = 60, sd = 5)
)

gaussian_curve_1_mean <- mean(gaussian_curve_examples$XVals)
gaussian_curve_1_sd <- sd(gaussian_curve_examples$XVals)


# Plot One Normal Curve
plt_gaussian_pdf <- ggplot(gaussian_curve_examples) +
  geom_line(aes(x = XVals, y = Mean_50_SD_20)) +
  geom_point(aes(x = XVals), y = 0, shape = 3) +
  geom_hline(yintercept = 0) +
  ylab("density") +
  xlab("Variable Values")
plt_gaussian_pdf

ggsave("Normal Curve.png")

# Add mean and SD lines to the one normal curve
plt_gaussian_pdf +
    geom_vline(xintercept = gaussian_curve_1_mean, color = "blue") +
    geom_vline(xintercept = gaussian_curve_1_mean + gaussian_curve_1_sd, color = "red") +
    geom_vline(xintercept = gaussian_curve_1_mean - gaussian_curve_1_sd, color = "red")

ggsave("Gaussian Curve Example.png")



# How values of mean and SD influence the shape of a gaussian dist --------

# Pivot data to enable grouping by distribution
gaussian_curve_examples_long <- gaussian_curve_examples %>%
  pivot_longer(
    cols = -XVals,
    names_to = "Distribution",
    values_to = "Density"
  )

# Plot all 3 normal curves on the same axis
ggplot(gaussian_curve_examples_long) +
  geom_line(aes(x = XVals, y = Density, color = Distribution)) +
  xlab("Variable Values")

ggsave("Gaussian Curves Example.png")




# Example: Implementing Gaussian DE on NBA Data ---------------------------

# Fit distribution using maximum likelihood estimation
gaussian_de_height <- fitdist(
  data = nba$HeightNoShoes,
  distr = "norm",
  method = "mle"
)

# Extract mean and SD of the fitted distribution
gaussian_de_height_mean <- gaussian_de_height$estimate[1]
gaussian_de_height_sd <- gaussian_de_height$estimate[2]


# Plot the estimated PDF curve over a histogram of the data, with multiple bin widths
gaussian_de_height_data <- tibble(
XVals = seq(min(nba$HeightNoShoes), max(nba$HeightNoShoes), 0.1),
YVals = dnorm(XVals, mean = gaussian_de_height_mean, sd = gaussian_de_height_sd) # Get probability estimates in the range of observed player heights
)

ggplot() +
  # Density histogram of player height
  geom_histogram(
    data = nba,
    aes(x = HeightNoShoes, y = after_stat(density)),
    binwidth = 0.25,
    alpha = 0.5
  ) +
  # Gaussian density estimate
  geom_line(
    data = gaussian_de_height_data,
    aes(x = XVals, y = YVals),
    size = 2,
    color = "blue"
  )

ggsave("NBA Gaussian Fit 1.png")


# Repeat this with two other binwidths to get a feel for the quality of the fit

# Binwidth = 0.50
ggplot() +
  # Density histogram of player height
  geom_histogram(
    data = nba,
    aes(x = HeightNoShoes, y = after_stat(density)),
    binwidth = 0.50,
    alpha = 0.5
  ) +
  # Gaussian density estimate
  geom_line(
    data = gaussian_de_height_data,
    aes(x = XVals, y = YVals),
    size = 2,
    color = "blue"
  )

ggsave("NBA Gaussian Fit 2.png")

# Binwidth = 0.75
ggplot() +
  # Density histogram of player height
  geom_histogram(
    data = nba,
    aes(x = HeightNoShoes, y = after_stat(density)),
    binwidth = 0.75,
    alpha = 0.5
  ) +
  # Gaussian density estimate
  geom_line(
    data = gaussian_de_height_data,
    aes(x = XVals, y = YVals),
    size = 2,
    color = "blue"
  )

ggsave("NBA Gaussian Fit 3.png")




# Helper function to fit gaussian DE and plot the result ------------------

# Arguments:
  # x: Tibble with a variable named XVals, containing a vector of datapoints to which to fit the density estimate

plot_gaussian_de <- function(x) {

  # Fit distribution using maximum likelihood estimation
  gaussian_de <- fitdist(
    data = x$XVals,
    distr = "norm",
    method = "mle"
  )

  # Extract mean and SD of the fitted distribution
  gaussian_de_mean <- gaussian_de$estimate[1]
  gaussian_de_sd <- gaussian_de$estimate[2]

  # Plot the estimated PDF curve over a histogram of the data
  gaussian_de_data <- tibble(
    XVals = seq(min(x$XVals), max(x$XVals), 0.1),
    YVals = dnorm(XVals, mean = gaussian_de_mean, sd = gaussian_de_sd) # Get probability estimates in the range of observed player heights
  )

  ggplot() +
    # Density histogram of bimodal data
    geom_histogram(
      data = x,
      aes(x = XVals, y = after_stat(density)),
      alpha = 0.5
    ) +
    # Gaussian density estimate
    geom_line(
      data = gaussian_de_data,
      aes(x = XVals, y = YVals),
      size = 2,
      color = "blue"
    ) +
    xlab("Variable Values")

}





# Example: Gaussian DE on Bimodal Data ------------------------------------

plot_gaussian_de(bimodal_data)

ggsave("Gaussian Bimodal Fit.png")



# Example: Gaussian DE on Outlier Data ------------------------------------

plot_gaussian_de(outlier_data)

ggsave("Gaussian Outlier Fit.png")


# Example: Gaussian DE on Skewed Data -------------------------------------

plot_gaussian_de(skewed_data)

ggsave("Gaussian Skewed Fit.png")




#------------------------------------------------------------------------#
# Kernel Density Estimation
#------------------------------------------------------------------------#



# Plot of a Kernel Density Estimate ---------------------------------------

ggplot(nba) +
  geom_density(aes(x = HeightNoShoes), kernel = "gaussian") +
  xlab("Player Height")



# Effects of Kernel Selection on KDE --------------------------------------

# Gaussian kernel
ggplot(nba) +
  geom_histogram(aes(x = HeightNoShoes, y = after_stat(density)), alpha = 0.5) +
  geom_density(aes(x = HeightNoShoes), kernel = "gaussian", color = "blue", size = 2) +
  xlab("Player Height")

ggsave("KDE Gaussian Kernel.png")

# Rectangular kernel
ggplot(nba) +
  geom_histogram(aes(x = HeightNoShoes, y = after_stat(density)), alpha = 0.5) +
  geom_density(aes(x = HeightNoShoes), kernel = "rectangular", color = "blue", size = 2) +
  xlab("Player Height")

ggsave("KDE Rectangular Kernel.png")

# Triangular kernel
ggplot(nba) +
  geom_histogram(aes(x = HeightNoShoes, y = after_stat(density)), alpha = 0.5) +
  geom_density(aes(x = HeightNoShoes), kernel = "triangular", color = "blue", size = 2) +
  xlab("Player Height")

ggsave("KDE Triangular Kernel.png")

# Epanechnikov kernel
ggplot(nba) +
  geom_histogram(aes(x = HeightNoShoes, y = after_stat(density)), alpha = 0.5) +
  geom_density(aes(x = HeightNoShoes), kernel = "epanechnikov", color = "blue", size = 2) +
  xlab("Player Height")

ggsave("KDE Epanechnikov Kernel.png")



# Effects of Bandwidth Selection on KDE -----------------------------------

# Bandwidths to try
bandwidths <- c(0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3)

# Initialize output
kde_by_bandwidth <- tibble(
  Bandwidth = numeric(),
  EvalPoint = numeric(),
  DensityEstimate = numeric()
)

# Calculate the KDE with each bandwidth value
for (i in bandwidths){

  # KDE using the bandwidth value for the current iteration
  kde_estimate <- kde(nba$HeightNoShoes, h = i)

  # For each iteration, extract bandwidth, evaluation points, and associated density estimates
  output <- tibble(
    Bandwidth = rep(i, length(kde_estimate$eval.points)),
    EvalPoint = kde_estimate$eval.points,
    DensityEstimate = kde_estimate$estimate
  )

  # Add the new rows for the current iteration to the the final output file
  kde_by_bandwidth <- bind_rows(kde_by_bandwidth, output)

}

# Plot the results
ggplot() +
  # Density histogram for comparison
  geom_histogram(
    data = nba,
    aes(x = HeightNoShoes, y = after_stat(density)),
    alpha = 0.5
    )+
  # KDE curve
  geom_line(data = kde_by_bandwidth,
            aes(x = EvalPoint, y = DensityEstimate)
  ) +
  # Separate plots for each bandwidth value
  facet_wrap(~Bandwidth) +
  xlab("Player Height")

ggsave("KDE Effects of Bandwidth.png")


# Example: Implementing KDE on NBA Data -----------------------------------

# Estimate the optimal kernel bandwidth using cross validation
nba_bw <- hscv(nba$HeightNoShoes)

# Alternatively, we could use a plug-in bandwidth selector.
hpi(nba$HeightNoShoes)
# This gives a slightly different result

# Use the CV bandwidth and a gaussian kernel to calculate the KDE
nba_kde <- kde(nba$HeightNoShoes, h = nba_bw)

# Extract the evaluation points and probability estimates
nba_kde_output <- tibble(
  EvalPoint = nba_kde$eval.points,
  DensityEstimate = nba_kde$estimate
)


# Plot the results with multiple bin widths to get a feel for the quality of the fit

# Bin width = 0.25
ggplot() +
  # Density histogram for comparison
  geom_histogram(
    data = nba,
    aes(x = HeightNoShoes, y = after_stat(density)),
    binwidth = 0.25,
    alpha = 0.5
  )+
  # KDE curve
  geom_line(data = nba_kde_output,
            aes(x = EvalPoint, y = DensityEstimate),
            color = "blue",
            size = 2
  ) +
  # Separate plots for each bandwidth value
  xlab("Player Height")

ggsave("NBA KDE Fit 1.png")

# Bin width = 0.5
ggplot() +
  # Density histogram for comparison
  geom_histogram(
    data = nba,
    aes(x = HeightNoShoes, y = after_stat(density)),
    binwidth = 0.5,
    alpha = 0.5
  )+
  # KDE curve
  geom_line(data = nba_kde_output,
            aes(x = EvalPoint, y = DensityEstimate),
            color = "blue",
            size = 2
  ) +
  # Separate plots for each bandwidth value
  xlab("Player Height")

ggsave("NBA KDE Fit 2.png")

# Bin width = 0.75
ggplot() +
  # Density histogram for comparison
  geom_histogram(
    data = nba,
    aes(x = HeightNoShoes, y = after_stat(density)),
    binwidth = 0.75,
    alpha = 0.5
  )+
  # KDE curve
  geom_line(data = nba_kde_output,
            aes(x = EvalPoint, y = DensityEstimate),
            color = "blue",
            size = 2
  ) +
  # Separate plots for each bandwidth value
  xlab("Player Height")

ggsave("NBA KDE Fit 3.png")




# Helper function to fit KDE and plot the result --------------------------

# Arguments:
# x: Tibble with a variable named XVals, containing a vector of datapoints to which to fit the density estimate
plot_kde <- function (x){

  # Estimate the optimal kernel bandwidth using cross validation
  bw <- hscv(x$XVals)

  # Use the CV bandwidth and a gaussian kernel to calculate the KDE
  kde_fit <- kde(x$XVals, h = bw)

  # Extract the evaluation points and probability estimates
  output <- tibble(
    EvalPoint = kde_fit$eval.points,
    DensityEstimate = kde_fit$estimate
  )

  # Plot the result
  ggplot(output) +
    # Density histogram for the variable of interest
    geom_histogram(
      data = x,
      aes(x = XVals, y = after_stat(density)),
      alpha = 0.5
    ) +
    # KDE curve
    geom_line(
      data = output,
      aes(x = EvalPoint, y = DensityEstimate),
      color = "blue",
      size = 2
    ) +
    xlab("Variable Values")

}





# Example: KDE on Bimodal Data --------------------------------------------

plot_kde(bimodal_data)

ggsave("KDE Bimodal Fit.png")



# Example: KDE with Outliers ----------------------------------------------

plot_kde(outlier_data)

ggsave("KDE Outlier Fit.png")



# Example: KDE with Skewed Data -------------------------------------------

plot_kde(skewed_data)

ggsave("KDE Skewed Fit.png")





#------------------------------------------------------------------------#
# Mixture Density Estimation
#------------------------------------------------------------------------#


# Helper function for generating gaussian mixture PDF ---------------------

# Arguments:
  # means: Vector containing the means for the first (index 1) and second (index 2) components
  # sds: Vector containing the standard deviations for the first (index 1) and second (index 2) components
  # weight:   Value to use for the relative weighting of the two components
  # x_values: A vector of x values for which to estimate the density

gaussian_mixture_pdf <- function(means, sds, weight, x_values){

  # Get probability densities from each distribution
  component_1 <- dnorm(x_values, mean = means[1], sd = sds[1])
  component_2 <- dnorm(x_values, mean = means[2], sd = sds[2])

  # Mix the two distributions
  component_mix <- component_1 * weight + component_2 * (1-weight)

  # Return the result
  return(component_mix)

}



# Effects of Changing Parameters on MDE -----------------------------------

# Helper function to plot a gaussian mixture PDF and its components

# Arguments
  # data:          Vector over which to estimate density
  # mean1 / mean2: Means of the component distributions
  # sd1 / sd2:     Standard deviations of the component distributions
  # Weight:        Weighting parameter value

plot_mixture_pdf <- function(data, mean1, sd1, mean2, sd2, weight){

  # Get probability densities from each distribution
  component_1 <- dnorm(data, mean = mean1, sd = sd1)
  component_2 <- dnorm(data, mean = mean2, sd = sd2)

  # Mix the two distributions
  component_mix <- component_1 * weight + component_2 * (1-weight)

  # Store the results in a tibble
  output <- tibble(
    XVals = data,
    Component1 = weight * component_1,
    Component2 = (1 - weight) * component_2,
    ComponentMix = component_mix
  )

  # Separate datasets for components and final curve
  output_final <- output %>%
    dplyr::select(XVals, ComponentMix)

  output_components <- output %>%
    dplyr::select(XVals, Component1, Component2)

  # Pivot output_components into a form conducive to plotting
  output_components_longer <- output_components %>%
    pivot_longer(
      cols = c(Component1, Component2),
      names_to = "Curve",
      values_to = "Density"
      )

  # Generate the plot
  ggplot() +
    # Plot a densityhistogram of the original data
    geom_histogram(
      data = output_final,
      aes(x = XVals, y = after_stat(density)),
      alpha = 0.5
    ) +
    # Plot the two component curves
    geom_line(
      data = output_components_longer,
      aes(x = XVals, y = Density, color = Curve),
      size = 2
    ) +
    # Plot the final MDE curve
    geom_line(
      data = output_final,
      aes(x = XVals, y = ComponentMix),
      color = "blue",
      size = 2
    ) +
    xlab("Variable Values")

}



# Example MDE's with different parameter values
plot_mixture_pdf(
  nba$HeightNoShoes,
  mean1 = 76,
  sd1 = 3,
  mean2 = 80,
  sd2 = 2,
  weight = 0.6
)
ggsave("Gaussian Mixture 1.png")

plot_mixture_pdf(
  nba$HeightNoShoes,
  mean1 = 80,
  sd1 = 2,
  mean2 = 75,
  sd2 = 2,
  weight = 0.6
)
ggsave("Gaussian Mixture 2.png")

plot_mixture_pdf(
  nba$HeightNoShoes,
  mean1 = 77,
  sd1 = 3,
  mean2 = 80,
  sd2 = 2,
  weight = 0.7
)
ggsave("Gaussian Mixture 3.png")



# Example: Implementing KDE on NBA Data -----------------------------------

# Estimate optimal parameter values using EM
set.seed(1990)
nba_mix <- mixtools::normalmixEM(x = nba$HeightNoShoes)


# Plot the result, using optimal values estimated above

# Combine data to plot
nba_mix_density <- tibble(
  XVal = seq(min(nba$HeightNoShoes), max(nba$HeightNoShoes), by = 0.1),
  Density = gaussian_mixture_pdf(
    means = nba_mix$mu,
    sds = nba_mix$sigma,
    weight = nba_mix$lambda[1],
    x_values = seq(min(nba$HeightNoShoes), max(nba$HeightNoShoes), by = 0.1)
  )
)


# Generate the plot with different histogram bin widths, to get a feel for the fit

# Binwidth = 0.25
ggplot() +
  # Density histogram of original data
  geom_histogram(
    data = nba,
    aes(x = HeightNoShoes, y = after_stat(density)),
    binwidth = 0.25,
    alpha = 0.5
  ) +
  # Mixture density estimate curve
  geom_line(
    data = nba_mix_density,
    aes(x = XVal, y = Density),
    color = "blue",
    size = 2
  )

ggsave("NBA MDE Fit 1.png")


# Binwidth = 0.50
ggplot() +
  # Density histogram of original data
  geom_histogram(
    data = nba,
    aes(x = HeightNoShoes, y = after_stat(density)),
    binwidth = 0.5,
    alpha = 0.5
  ) +
  # Mixture density estimate curve
  geom_line(
    data = nba_mix_density,
    aes(x = XVal, y = Density),
    color = "blue",
    size = 2
  )

ggsave("NBA MDE Fit 2.png")

# Binwidth = 0.75
ggplot() +
  # Density histogram of original data
  geom_histogram(
    data = nba,
    aes(x = HeightNoShoes, y = after_stat(density)),
    binwidth = 0.75,
    alpha = 0.5
  ) +
  # Mixture density estimate curve
  geom_line(
    data = nba_mix_density,
    aes(x = XVal, y = Density),
    color = "blue",
    size = 2
  )


ggsave("NBA MDE Fit 3.png")






# Helper function to fit MDE with 2 gaussians and plot result -------------

# Arguments:
# x: Tibble with a variable named XVals, containing a vector of datapoints to which to fit the density estimate

plot_mde <- function(x){

  # Estimate optimal parameter values using EM
  mix_fit <- mixtools::normalmixEM(x = x$XVals)


  # Plot the result, using optimal values estimated above

  # Combine data to plot
  mix_density <- tibble(
    XVal = seq(min(x$XVals), max(x$XVals), length.out = 200),
    Density = gaussian_mixture_pdf(
      means = mix_fit$mu,
      sds = mix_fit$sigma,
      weight = mix_fit$lambda[1],
      x_values = seq(min(x$XVals), max(x$XVals), length.out = 200)
    )
  )

  # Generate the plot
  ggplot() +
    # Density histogram of original data
    geom_histogram(
      data = x,
      aes(x = XVals, y = after_stat(density)),
      alpha = 0.5
    ) +
    # Mixture density estimate curve
    geom_line(
      data = mix_density,
      aes(x = XVal, y = Density),
      color = "blue",
      size = 2
    )

}





# Example: MDE on Bimodal Data --------------------------------------------

set.seed(1888)
plot_mde(bimodal_data)

ggsave("MDE Bimodal Fit.png")



# Example: MDE with Outliers ----------------------------------------------

set.seed(1889)
plot_mde(outlier_data)

ggsave("MDE Outlier Fit.png")

# Since data on the left came from a normal distribution, captured well with one of the normals
# This freed up the other normal curve to capture the outliers.

# If the structure on the left side of the curve were more complex, MDE could have missed the outliers
  # E.g. what happened below with the skewed data



# Example: MDE with Skewed Data -------------------------------------------

set.seed(1978)
plot_mde(skewed_data)

ggsave("MDE Skewed Fit.png")

# Captured the shape pretty well on the left side
# However, in this case, we spent both distributions on capturing everything on the left, so missed outliers on the right





