library(readxl)
library(ggplot2)
library(e1071)
library(corrplot)
library(ggplot2)
library(quadprog)
library(timeSeries)
library(PerformanceAnalytics)
library(lubridate)
assets <- read_xlsx("healthcare.xlsx")
write.csv(assets,"healthcare.csv", row.names = F)
assets <- read.csv("healthcare.csv", na.strings = "NA")
assets$date <- as.Date(assets$date, format = "%Y-%m-%d")

# Drop columns with too many NAs
na_count <- sapply(assets, function(col) sum(is.na(col)))
assets <- assets[, na_count <= 206]
assets <- na.omit(assets)
rownames(assets) <- NULL

# Market returns
market_price <- read_xlsx("market.xlsx")
market_price <- subset(market_price, date >= as.POSIXct(assets[1, "date"]))

### Descriptive statistics ###

# Returns calculation
returns <- data.frame(date = assets$date[-1])
for (col in names(assets)[-1]) {
  returns[[col]] <- (assets[[col]][-1] / assets[[col]][-length(assets[[col]])]) - 1
}

# Moments
asset_returns <- returns[, -1]
mean_returns <- colMeans(asset_returns)
sd_returns <- apply(asset_returns, 2, sd)
skewness <- apply(asset_returns, 2, skewness)
kurtosis <- apply(asset_returns, 2, kurtosis)
max_drawdown <- sapply(asset_returns, min)
value_at_risk <- function(returns, p = 0.05) {
  return(quantile(returns, probs = p))
}
VaR <- apply(asset_returns, 2, value_at_risk)
annualized_mean <- mean_returns * 12
annualized_sd <- sd_returns * sqrt(12)
moments <- data.frame(
  Asset = colnames(asset_returns),
  mean = mean_returns,
  sd = sd_returns,
  kurtosis = kurtosis,
  skewness = skewness,
  max_drawdown = max_drawdown,
  VaR = VaR
)
rownames(moments) <- NULL

# Correlations
cor.data <- cor(asset_returns)
corrplot(cor.data,method='color', 
         tl.cex = 0.5,
         tl.col = "black")

### Constructing the efficient set ###
rf <- 1.0424^(1/12)-1
n_assets <- ncol(asset_returns)

# Define the range of target portfolio returns
num_points <- 1000
target_returns <- seq(-0.02, 0.05, length.out = num_points)

# Convert asset returns to a matrix for quadratic programming
returns_matrix <- as.matrix(asset_returns)
mean_returns <- colMeans(returns_matrix)

# Calculate the covariance matrix of the asset returns
cov_matrix <- cov(returns_matrix)

# Initialize results
unconstrained_results <- data.frame(mean_return = numeric(), std_dev = numeric())
constrained_results <- data.frame(mean_return = numeric(), std_dev = numeric())

# Number of assets
n_assets <- ncol(returns_matrix)

# Constraints for unconstrained optimization
A_eq <- rbind(rep(1, n_assets), mean_returns)  # Sum of weights = 1 and target return

for (target_return in target_returns) {
  # Equality constraint for the target return
  b_eq <- c(1, target_return)
  
  # Solve the quadratic programming problem (unconstrained)
  solution_unconstrained <- solve.QP(
    Dmat = cov_matrix, 
    dvec = rep(0, n_assets), 
    Amat = t(A_eq), 
    bvec = b_eq, 
    meq = 2
  )
  
  # Extract portfolio weights and calculate mean return and std dev (unconstrained)
  weights_unconstrained <- solution_unconstrained$solution
  portfolio_mean_unconstrained <- sum(weights_unconstrained * mean_returns)
  portfolio_std_unconstrained <- sqrt(t(weights_unconstrained) %*% cov_matrix %*% weights_unconstrained)
  
  # Store results (unconstrained)
  unconstrained_results <- rbind(unconstrained_results, data.frame(mean_return = portfolio_mean_unconstrained, std_dev = portfolio_std_unconstrained))
}

# Constraints for constrained optimization
constrained_target_returns <- seq(min(mean_returns), max(mean_returns), length.out = num_points)
A_eq_constrained <- rbind(rep(1, n_assets), mean_returns)

for (target_return in constrained_target_returns) {
  # Equality constraint for the target return
  b_eq_constrained <- c(1, target_return)
  
  # Constraints for constrained optimization (weights >= 0)
  A_constrained <- rbind(A_eq_constrained, diag(n_assets))
  b_constrained <- c(b_eq_constrained, rep(0, n_assets))
  
  # Solve the quadratic programming problem (constrained)
  solution_constrained <- solve.QP(
    Dmat = cov_matrix, 
    dvec = rep(0, n_assets), 
    Amat = t(A_constrained), 
    bvec = b_constrained, 
    meq = 2
  )
  
  # Extract portfolio weights and calculate mean return and std dev (constrained)
  weights_constrained <- solution_constrained$solution
  portfolio_mean_constrained <- sum(weights_constrained * mean_returns)
  portfolio_std_constrained <- sqrt(t(weights_constrained) %*% cov_matrix %*% weights_constrained)
  
  # Store results (constrained)
  constrained_results <- rbind(constrained_results, data.frame(mean_return = portfolio_mean_constrained, std_dev = portfolio_std_constrained))
}

### Tangency portfolios ###
source("tangency_pf.R")

# Unconstrained
tp_short <- tangency.portfolio(mean_returns,cov_matrix,rf,short = T)

# Constrained
tp_long <- tangency.portfolio(mean_returns,cov_matrix,rf,short = F)

### Performance ###
# Equal weights
equal_weights <- rep(1/n_assets, n_assets)
EW_return <- rowSums(asset_returns * equal_weights)
EW_index <- cumprod(1 + EW_return)

# Value-weighted
cumulative_values <- sapply(asset_returns,function(x) cumprod(1 + x))
VW_return <- rowSums(cumulative_values * asset_returns) / rowSums(cumulative_values)
VW_index <- cumprod(1 + VW_return)

# Tangency short-selling
tangency_short_return <- rowSums(tp_short$weights * asset_returns)
tangency_short_index <- cumprod(1 + tangency_short_return)

# Tangency long
tangency_long_return <- rowSums(tp_long$weights * asset_returns)
tangency_long_index <- cumprod(1 + tangency_long_return)

# Everything in one df
tg_overall <- data.frame(date = returns$date,
                         EW = EW_index,
                         VW = VW_index,
                         TG_short = tangency_short_index,
                         TG_long = tangency_long_index)


### Betas ###
market <- diff(market_price$`US-DS Market`, lag = 1) / head(market_price$`US-DS Market`, -1)

# Empirical betas
betas <- numeric(ncol(asset_returns))
names(betas) <- colnames(asset_returns)
for (asset in colnames(asset_returns)) {
  model <- lm(asset_returns[[asset]] ~ market)
  betas[asset] <- coef(model)["market"]
}

# Theoretical returns
t_returns <- rf + betas * (mean(market) - rf)

# Plotting
capm <- data.frame(beta = betas,
                   e_return = mean_returns,
                   t_return = t_returns)

### Portfolio optimization ###

# Dates in-sample
start_date <- as.Date("2000-01-02")
end_date <- as.Date("2013-01-02")
first_date <- as.Date(returns[1,"date"])

# Dates out-of-sample
start_date_2 <- as.Date("2013-02-02")
end_date_2 <- as.Date("2025-01-02")
first_date_2 <- start_date

# Other
months <- floor(as.numeric(start_date - first_date) / 30.44)
total_months <- floor(as.numeric(end_date - first_date) / 30.44)
sims <- numeric(0)
max_t <- months
min_t <- 1
max_k <- months
min_k <- 1
tp_weights <- numeric(0)
shorts <- F
returns_sliced <- returns[returns$date >= first_date & returns$date <= end_date,]

ER_sim <- function(t,k, shorts = F) {
  for (i in (1+months):nrow(returns_sliced)) {
    if ((i == (1+months)) | (!((i - (1+months)) %% t))) {
      mat <- returns_sliced[(i-k):i,-1]
      er <- colMeans(mat)
      covmat <- cov(mat)
      tp <- tangency.portfolio(er,covmat,rf,shorts = shorts)
      tp_weights <- rbind(tp_weights,tp$weights)
    } else {
      tp_weights <- rbind(tp_weights,tp$weights)
    }
  }
  ret_start_end <- as.matrix(returns[returns$date >= start_date & returns$date <= end_date,-1])
  pf_returns <- rowSums(tp_weights * ret_start_end)
  EW_sliced <- EW_return[as.numeric(rownames(ret_start_end))]
  ER <- pf_returns - EW_sliced
  mean_ER <- mean(ER)
  sim <- c(t = t, k = k, mean_ER = mean_ER)
  return(sim)
}

sims <- read.csv("sims.csv")
sims <- unique(sims[,-1])
optimum <- sims[which.max(sims$mean_ER),]
t <- optimum[,"t"]
k <- optimum[,"k"]

# Backtesting
backtest <- function(start_date,end_date,first_date,t,k, shorts = F, weights = F) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  first_date <- as.Date(first_date)
  months <- floor(as.numeric(start_date - first_date) / 30.44)
  total_months <- floor(as.numeric(end_date - first_date) / 30.44)
  tp_weights <- numeric(0)
  shorts <- shorts
  returns_sliced <- returns[returns$date >= first_date & returns$date <= end_date,]
  t <- t
  k <- k
  for (i in (1+months):nrow(returns_sliced)) {
    if ((i == (1+months)) | (!((i - (1+months)) %% t))) {
      mat <- returns_sliced[(i-k):i,-1]
      er <- colMeans(mat)
      covmat <- cov(mat)
      tp <- tangency.portfolio(er,covmat,rf,shorts = shorts)
      tp_weights <- rbind(tp_weights,tp$weights)
    } else {
      tp_weights <- rbind(tp_weights,tp$weights)
    }
  }
  ret_start_end <- as.matrix(returns[returns$date >= start_date & returns$date <= end_date,-1])
  pf_returns <- rowSums(tp_weights * ret_start_end)
  EW_sliced <- EW_return[as.numeric(rownames(ret_start_end))]
  date <- returns[returns$date >= start_date & returns$date <= end_date,"date"]
  test <- data.frame(date = date,EW = EW_sliced, PF = pf_returns)
  tp_weights <- as.data.frame(tp_weights)
  tp_weights <- cbind(date,tp_weights)
  if (weights == F) {
    return(test)
  } else {
    return(tp_weights)
  }
}


in_sample <- backtest(start_date = start_date,end_date = end_date, first_date = first_date, t = t, k = k)
weights_in_sample <- backtest(start_date = start_date,end_date = end_date, first_date = first_date, t = t, k = k, weights = T)
out_of_sample <- backtest(start_date = start_date_2,end_date = end_date_2, first_date = first_date_2,t = t, k = k)
weights_of_sample <- backtest(start_date = start_date_2,end_date = end_date_2, first_date = first_date_2,t = t, k = k, weights = T)

## VW benchmarks calculation

# In-sample
insample_ret <- returns[returns$date >= "2000-01-02" & returns$date <= "2013-01-02",-1]
cumulative_values_insamle <- sapply(insample_ret,function(x) cumprod(1 + x))
VW_return_insmaple <- rowSums(cumulative_values_insamle * insample_ret) / rowSums(cumulative_values_insamle)
in_sample$VW <- VW_return_insmaple
in_sample$EW_index <- cumprod(1 + in_sample$EW)
in_sample$PF_index <- cumprod(1 + in_sample$PF)
in_sample$VW_index <- cumprod(1 + in_sample$VW)

# Out-of-sample
outofsample_ret <- returns[returns$date >= "2013-02-02" & returns$date <= "2025-01-02",-1]
cumulative_values_outofsample <- sapply(outofsample_ret,function(x) cumprod(1 + x))
VW_return_outofsample <- rowSums(cumulative_values_outofsample * outofsample_ret) / rowSums(cumulative_values_outofsample)
out_of_sample$VW <- VW_return_outofsample
out_of_sample$EW_index <- cumprod(1 + out_of_sample$EW)
out_of_sample$PF_index <- cumprod(1 + out_of_sample$PF)
out_of_sample$VW_index <- cumprod(1 + out_of_sample$VW)




