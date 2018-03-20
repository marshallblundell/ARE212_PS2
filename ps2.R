#############################################################
#     File Name           :     ps2.R
#     Created By          :     MBlundell
#     Creation Date       :     [2018-03-11 19:25]
#     Last Modified       :     [2018-03-20 14:41]
#     Description         :      
#############################################################

# 
# Set up.
#
library(pacman)
p_load(readr, xlsx, ggplot2)

dir <- "C:/Users/mblundell/Documents/ARE/ARE212/PS2/"
originaldata <- paste0(dir, "OriginalData/")
output <- paste0(dir, "Output/")

# 
# Load functions
#

#
# demean() takes a vector and returns demeaned vector.
#
demean <- function(x) x - mean(x)

#
# reg() takes a vector y and a matrix X and regresses y on X
# Return value is matrix of coefficients.
# Does not include an intercept by default. User must append
# a vector of ones to X in that case.
#
reg <- function(y, X) {
    solve(t(X) %*% X) %*% t(X) %*% y
}

#
# pred() takes a matrix X and a matrix beta
# Return value is vector of predicted values.
#
pred <- function(X, beta) {
    as.vector(X %*% beta)
}

#
# R.squared() takes a vector e of residuals and a vector
# dependent variable y and returns centered R squared.
#
R.squared <- function(e, y) {
    y.demeaned <- demean(y)
    as.vector(1 - ((t(e) %*% e) / (t(y.demeaned) %*% y.demeaned)))
}

#
# adj.R.squared() takes a vector e of residuals and a vector
# dependent variable y, total degrees of freedom and residual degrees
# of freedom and calculates adjusted R-squared.
#
adj.R.squared <- function(e, y, df.t, df.e) {
    as.vector(1 - ((1 - R.squared(e, y)) * (df.t / df.e)))
}

# 
# report.stats() makes summary stats for a regression. 
# Takes a vector dependent variable y, a matrix of regressors X,
# and fitted values beta.
# Returns beta and R^2 in a list.
#
report.stats <- function(y, X, beta) {
    # Set up output
    out <- list()

    # Get some intermediate values
    df <- dim(X)[1] - dim(X)[2]
    y.hat  <- pred(X, beta)
    e <- y - y.hat
    n <- dim(X)[1]

    # Compile output
    rownames(beta)[1] <- "Intercept"
    out$beta <- beta
    # out$adj.R2 <- adj.R.squared(e, y, n - 1, df)
    out$R2 <- R.squared(e, y)
    out
}

# 
# t.test() peroforms two-sided t-test for a regression coefficient.
# Takes y, X, estimated beta, index of coefficient, and value
# of null hypothesis.
# Returns t-stat and p-value in a list.
#
t.test <- function(y, X, beta, i, null = 0) {
    y.hat  <- pred(X, beta)
    e <- y - y.hat
    df <- dim(X)[1] - dim(X)[2]
    s.sq <- (t(e) %*% e) / df
    t.stat <- (beta[i] - null) / (s.sq * solve(t(X) %*% X)[i, i])^0.5
    prob <- pt(t.stat, df)
    return(list(t.stat=t.stat, p.value=2 * min(prob, 1 - prob)))
}

# 
# pearson.r() calculates pearson's correlation coefficient for sample. 
# Takes vectors x and y as inputs.
# Returns scalar correlation coefficient.
#
pearson.r <- function(x, y) {
    x.dm <- demean(x)
    y.dm <- demean(y)
    r <- (t(y.dm) %*% x.dm) / ((t(y.dm) %*% y.dm)^0.5 * (t(x.dm) %*% x.dm)^0.5)
    return(r)
}

# 
# f.test() runs an f.test using nested models.
# takes y, X, and beta for two models. Model 1 must be nested
# or restricted version of model 2.
# Returns f-stat and p-value in a list.
#
f.test <- function(y.1, X.1, beta.1, y.2, X.2, beta.2) {
    e.1 <- y - pred(X.1, beta.1)
    e.2 <- y - pred(X.2, beta.2)
    df.1 <- dim(X.1)[1] - dim(X.1)[2]
    df.2 <- dim(X.2)[1] - dim(X.2)[2]
    ssr.1 <- t(e.1) %*% e.1
    ssr.2 <- t(e.2) %*% e.2
    f.stat <- ((ssr.1 - ssr.2) / (df.1 - df.2)) / (ssr.2 / df.2)
    p.value <- pf(f.stat, df.1 - df.2, df.2, lower.tail=F)
    return(list(f.stat=f.stat, p.value=p.value))
}

#
# Load data and check it out.
#
data <- read.xlsx(paste0(originaldata, "nerlove.xls"),
                   sheetIndex=1,
                   stringsAsFactors=F,
                   colClasses="numeric")

# Looks like a typo for labor wage
summary(data)
# Correct typo
data[which(data$PL > 180), c("PL")] <- data[which(data$PL > 180), c("PL")] / 100

# Total cost vs kWh
ggplot(data=data, aes(x=Q, y=TC)) +
geom_point() +
scale_y_continuous(breaks=seq(0, 140, 10)) +
scale_x_continuous(breaks=seq(0, 18000, 2000))

# Labor wage vs kWh
ggplot(data=data, aes(x=Q, y=PL)) +
geom_point() +
scale_y_continuous(breaks=seq(1.4, 2.4, .1)) +
scale_x_continuous(breaks=seq(0, 18000, 2000))

# Fuel price vs kWh
ggplot(data=data, aes(x=Q, y=PF)) +
geom_point() +
scale_y_continuous(breaks=seq(10, 50, 5)) +
scale_x_continuous(breaks=seq(0, 18000, 2000))

# Capital price vs kWh
ggplot(data=data, aes(x=Q, y=PK)) +
geom_point() +
scale_y_continuous(breaks=seq(130, 240, 10)) +
scale_x_continuous(breaks=seq(0, 18000, 2000))

# Sort by kWh
data <- data[order(data$Q),]
rownames(data) <- NULL # Fixes row names

# 
# Replicate regression I (page 176) in the paper.
#

# Make X
X <- cbind(1, log(data$Q), log(data$PL) - log(data$PF), log(data$PK) - log(data$PF))
colnames(X) <- c("K", "Y", "P1 - P3", "P2 - P3")

# Make Y
y <- log(data$TC) - log(data$PF)

# Run regression, get stats
# R2 matches
beta.1 <- reg(y, X)
report.stats(y, X, beta.1)

# 
# Conduct a hypothesis test for B_y = 1 as null.
#

# We get p-value of 7.5e-34. That's very small.
t.test(y, X, beta.1, i=2, null=1)

# Point estimate is .72. Since our coefficient on Y is 1 / r this
# means we have increasing returns to scale based on the point
# estimate.

# 
# Plot residuals against output.
# Looks like residuals have a quadratic relationship with output.
# Means firms of different size have different parameters for
# the cost function.
#
y.hat  <- pred(X, beta.1)
e <- y - y.hat
to.plot <- data.frame(output=X[, c("Y")], residuals=e)
ggplot(data=to.plot, aes(x=output, y=e)) +
geom_point() +
xlab("Output ln(kWh)") +
ylab("Residuals")

# Calculate correlation coefficient of residuals with
# output
# This is zero, which we would expect by construction.
pearson.r(X[, c("Y")], e)

# The above was log output so use regular output
pearson.r(data$Q, e)


# 
# Run separate regressions for industry quintiles
# Returns to scale is decreasing in firm size.
#
divided <- split(as.data.frame(cbind(y, X)), cut(1:dim(X)[1], 5))
lapply.reg.report <- function(x) {
    y <- x[, 1]
    X <- as.matrix(x[, -1])
    report.stats(y, X, reg(y, X))
}
size.regs <- lapply(divided, lapply.reg.report)

# Calculate returns to scale for each
lapply(size.regs, function(x) 1 / x$beta[2])

# 
# Create dummy variables for each industry. Interact with
# output to create five slope coefficients. Run model letting
# intercept and slope coefficients on outpus differ across
# plant size.
#

# Make matrix of dummy variables
all.bins <- cut(1:dim(X)[1], 5)
dummy <- function(bin){as.numeric(all.bins == bin)}
D <- sapply(unique(all.bins), dummy)

# Make X with fixed effects
X.fe <- cbind(D, D * X[, c("Y")], X[, 3:4])
colnames(X.fe) <- c(paste0("K.", 1:5),
                    paste0("Y.", 1:5),
                    colnames(X[, 3:4]))

# Make Y
y <- log(data$TC) - log(data$PF)

# Run regression
beta.fe <- reg(y, X.fe)
beta.fe

# 
# Construct a statistical test comparing the first model
# with the second.
#
f.test(y, X, beta.1, y, X.fe, beta.fe)

# 
# Test whether returns to scale decline with output
#

# Include quadratic term on output
X.rts <- cbind(X, log(data$Q)^2)
colnames(X.rts) <- c(colnames(X), "Y.sq")

# Run regression, get stats
# R2 matches
beta.rts <- reg(y, X.rts)

# Run test.
t.test(y, X.rts, beta.rts, i=5, null=0)
