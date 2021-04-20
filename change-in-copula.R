
install.packages("devtools")
library(devtools)

# for plotting contour curves
install.packages("plot3D")
library(plot3D)

# this is the newest version which is on GitHub 
install_github("spesenti/SWIM", ref = "develop")
library(SWIM)

require("copula")
library(copula)

source("plot_contour.R")

# generate bivariate copula

Nsim <- 10^4
##################### Comonotonic copula #####################

u_sim <- runif(Nsim, 0,1)
u <- cbind(u_sim, u_sim)  
#### only plot if number of data is small
 plot(u, main = "comonotonic coupla", 
     xlab = expression(U[1]), ylab = expression(U[2]))

##################### Countermonotonic copula #####################
# (U, 1 - U) has a comonotonic copula
u_sim <- runif(Nsim, 0,1)
u <- cbind(u_sim, 1 - u_sim)

plot(u, main = "comonotonic coupla", 
     xlab = expression(U[1]), ylab = expression(U[2]))

##################### Independent copula #####################

u <- cbind(runif(Nsim, 0,1), runif(Nsim, 0,1))

plot(u, main = "independent coupla", 
     xlab = expression(U[1]), ylab = expression(U[2]))


  
##################### Clayton copula #####################
clayton_copula <- claytonCopula(param = 5, dim = 2)
u <- rCopula(Nsim, clayton_copula)
plot(u, main = "Clayton coupla", 
     xlab = expression(U[1]), ylab = expression(U[2]))


##################### SWIM - stressed probability measure #####################
# stress VaR of the first component of u 
# alpha - level of VaR
# q_ratio - % increase in VaR
u_stressed <- stress_VaR(u, alpha = 0.8, q_ratio = 1.05)

# here are some visual comparison of both probability measures
plot_hist(u_stressed, xCol = 1, base = TRUE)
plot_quantile(u_stressed, xCol = 2, base = TRUE)
plot_weights(u_stressed, xCol = 1)
plot_weights(u_stressed, xCol = 2)



# copula contour plot under ORIGNIAL probability measure
# including scatter plot of 200 points
plot_copula_contour(u, rep(1, Nsim))


# copula contour plot under STRESSED probability measure
# including scatter plot of 200 points
plot_copula_contour(u, get_weights(u_stressed))
















