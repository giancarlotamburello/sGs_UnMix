library(mixtools)

# data: a vector containing the variable. For CO2 flux: data= log10(CO2_flux)

unmix <- function(data, n.pop, breaks="fd", adj.density = 1, fit=FALSE, theor.density=FALSE, is.log10=TRUE,lab=NULL, ...){
  
  data.mix <- normalmixEM(data, k = n.pop, ...)
  
  ## Histogram:
  
  if (is.null(lab)) {
    expression(paste("log ", "CO"[2], " flux"))
  }
  
  plot(data.mix, which=2, breaks=breaks, xlab2 = lab)
  lines(density(data, adjust = adj.density), lty=1, lwd=1.5)
  rug(data.mix$x)
  if (theor.density) {
    td <- list(0)
    for (ii in 1:n.pop) {
      td[[ii]] <- rnorm(data.mix$lambda[ii]*length(data), data.mix$mu[ii], data.mix$sigma[ii])
    }
    td <- unlist(td); attributes(td) <- NULL
    lines(density(td, adjust=adj.density), col="purple")
  }
  
  ## Fitting the data with one mixing distribution
  if (fit){
    qqnorm(data.mix$x)
    mix <- list(0)
    for (ii in 1:n.pop) {
      mix[[ii]] <- rnorm(data.mix$lambda[ii]*length(data), data.mix$mu[ii], data.mix$sigma[ii])
    }
    mix <- unlist(mix); attributes(mix) <- NULL
    points(qqnorm(mix, plot.it = FALSE), pch=16, cex=0.5, col="red")
  }
  
  ## Q-Q plot 
  # Sampling and plotting 100 distributions, consisting in the mixture of n.pop populations:
  qqnorm(data.mix$x, type="n")
  for (i in 1:100) {
    mix.pop <- list(0)
    for (ii in 1:n.pop) {
      mix.pop[[ii]] <- rnorm(data.mix$lambda[ii]*length(data), data.mix$mu[ii], data.mix$sigma[ii])
    }
    mix.pop <- unlist(mix.pop); attributes(mix.pop) <- NULL
    points(qqnorm(mix.pop, plot.it = FALSE), pch=16, cex=0.5, col="lightblue")
  }
  # Plotting the qqlines of the n.pop populations
  for (i in 1:n.pop) {
    qqline( rnorm(data.mix$lambda[i]*10000, data.mix$mu[i], data.mix$sigma[i]), col=i+1, lwd=2)
  }
  # Plotting the sample nscores
  points(qqnorm(data.mix$x, plot.it = FALSE), pch=16, cex=0.8)
  box(lwd=1.5)
  
  ## Output
  summary(data.mix)
  if (is.log10){
    # Calculating mean and sd of each lognormal population through Monte Carlo simulation
    mean.out <- 0; sd.out <- 0
    for (n in 1:n.pop) {
      pop <- 0
      for (nn in 1:5000) {
        pop[nn] <- mean(10^rnorm(data.mix$lambda[n]*length(data), data.mix$mu[n], data.mix$sigma[n]))
        # Draw length(data) data from the lognormal distribution of the n.pop population,
        # ^10, and calculate the mean. Repeat this operation 5000 times.
      }
      mean.out[n] <- mean(pop) # calculating the mean and sd of the 5000 mean values. 
      sd.out[n] <- sd(pop)
    }
    #out <- data.frame(mean=mean.out, sd=sd.out)
    return(list(data.mix=data.mix, mean=mean.out, sd=sd.out))
    #print(round(out, 2))
  }
  #invisible(data.mix)
}