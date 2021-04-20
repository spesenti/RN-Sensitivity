# for realisations u, v, and weights w, plots the contour plot of the 
# copula under the scenario weights w. 

# maybe add the logistic transform Y = log(V / (1 - v))
# see notes SS_V2, Sim approach, Lambda

# u is a 2-dim dataframe
# w weights




plot_copula_contour <- function(u, w){

n <- dim(u)[1]
# bandwidth using Silverman's rule
h_u1 <- 1.06 * sd(u[, 1]) * n^(-1/5)
h_u2 <- 1.06 * sd(u[, 2]) * n^(-1/5)

u_matrix <- matrix(rep(seq(0, 1, length.out = 50), 50), nrow = 50, byrow = TRUE)
v_matrix <- t(u_matrix)


# w_x_dat contains in columns weights, u1, u2
KDE <- function(w_x_dat){
    .res <- w_x_dat[1] * dnorm((u_matrix - w_x_dat[2]) / h_u1) * 
     (dnorm((v_matrix- w_x_dat[3]) / h_u2) / n)
    return(.res)
    }

KDE_w <- rowSums(apply(cbind(w, u), FUN = KDE, MARGIN = 1))
# reshape vector to matrix
KDE_w <- matrix(KDE_w, nrow = 50)


contour2D(z = KDE_w, x = u_matrix[1, ], y = v_matrix[, 1], colkey = FALSE, 
          lwd = 2, xlab = expression(U[1]), ylab = expression(U[2]))
u_grid <- seq(1, Nsim, length.out = 200)
points(u[u_grid,1], u[u_grid,2])  
}
