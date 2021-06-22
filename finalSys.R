
#parameters
days_count <- 1000
traders_count <- 1000
a <- 0.2 * 10^(-4)
K <- 0.02
alpha_const <- 1.5
eta_sd <- 0.026
sigma_sd <- 0.025
tau_sd <- 0.01
beta_sd <- 0.05
mean_gaussian <- 0.0013
sd_gaussian <- 0.023
experiment <- c(1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0,1, 5, 4, 9, 0)


#inicialization
createTradersListMod <- function(nt){
  past_prices_vector <- head(experiment, 51)
  tech_traders_count <- round(traders_count * nt)
  fundamental_traders_count <- traders_count - round(traders_count * nt)
  strategy_type <- c(rep("technical", tech_traders_count), rep("fundamental", traders_count - tech_traders_count))
  cash <- rpareto(traders_count, shape = alpha_const, scale = 0.5) + 30
  while (max(cash) - min(cash) > 70 || max(cash) - min(cash) < 69){
    cash <- rpareto(traders_count, shape = alpha_const, scale = 0.5) + 30
  }
  f <- rnorm( fundamental_traders_count, 0.05, 0.04)
  c <- rnorm(tech_traders_count, 0.04, 0.04)
  type_coef <- c(c,f)
  technical_memory <- round(runif(traders_count, min = 1, max = 50))
  smoothing_coef <- -(technical_memory - 50) * 0.69 / 49 + 0.3
  technical_price <- vector(mode="numeric", length=0)
  for (i in 1:tech_traders_count){
    sum <- 0
    for (j in 1:technical_memory[i]){
      sum <- sum + ( smoothing_coef[i]^(j - 1)) * log(past_prices_vector[52 - j]/past_prices_vector[52 - j - 1])
    }
    technical_price <- c( technical_price, sum * smoothing_coef[i])
  }
  fundamental_price <- rep(experiment[50] + rnorm(1, 0, eta_sd), fundamental_traders_count)
  type_price <- c(technical_price, fundamental_price)
  type_expect <- rep(NaN, traders_count)
  asset_amount <- rep(0,traders_count)
  df_traders <- data.frame(strategy_type, cash, type_coef, technical_memory, smoothing_coef, type_price, type_expect, asset_amount)
  #View(df_traders)
  return(df_traders)
}

#modeling
new_model <- function(nt){
  past_prices_vector <- head(experiment, 51)
  traders <- createTradersListMod(nt)
  iid_eta <- rnorm(days_count - 50, mean = 0, sd = eta_sd)
  iid_beta <- rnorm(days_count - 50, mean = 0, sd = beta_sd)
  iid_sigma <- rnorm(days_count - 50, mean = 0, sd = sigma_sd)
  iid_tau <- rnorm(days_count - 50, mean = 0, sd = tau_sd)
  tech_traders <- head(traders, round(nt*traders_count))
  tech_traders_count <- nrow(tech_traders)
  fund_traders <- tail(traders, traders_count - round(nt*traders_count))
  fund_traders_count <- nrow(fund_traders)
  log_ratio_V <- rnorm(n = days_count - 50, mean = mean_gaussian, sd = sd_gaussian)
  log_ratio <- rnorm(n = 3, mean = mean_gaussian , sd = sd_gaussian)
  v_3 <- past_prices_vector[1]
  v_2 <- v_3 * exp(log_ratio[1])
  v_1 <- v_2 * exp(log_ratio[2])
  v_0 <- v_1 * exp(log_ratio[3])
  for (day in 51:days_count){
    sell_orders <- 0
    buy_orders <- 0
    for (i in 1:tech_traders_count){
      tech_traders$type_price[i] <- tech_traders$type_price[i] * tech_traders$smoothing_coef[i] - log(past_prices_vector[day - tech_traders$technical_memory[i]]/past_prices_vector[day - tech_traders$technical_memory[i] --1])*(tech_traders$smoothing_coef[i]^(tech_traders$technical_memory[i] + 1)) +tech_traders$smoothing_coef[i] *log(past_prices_vector[day - 1]/past_prices_vector[day - 2])
    }
    v_3 <- v_2
    v_2 <- v_1
    v_1 <- v_0
    v_0 <- v_1 * exp(log_ratio_V[day - 51])
    #if (TRUE && ((v_0 >= v_1) && (v_1 >= v_2) && (v_2 >= v_3)) || ((v_0 <= v_1) && (v_1 <= v_2) && (v_2 <= v_3))) {
     # r <- 1.1
    #} else if (TRUE && ((v_0 >= v_1) && (v_1 >= v_2)) || ((v_0 <= v_1) && (v_1 <= v_2))) {
     # r <- 1
    #} else
    r <- 0.9
    fund_traders_type_price <- r * log(v_0 / past_prices_vector[day-1]) + iid_eta[day-51]
    fundamental_new_comp <- fund_traders$type_coef * (fund_traders_type_price - past_prices_vector[day - 1]) + iid_tau[day - 51]
    fund_traders_type_expect <- past_prices_vector[day - 1] + fundamental_new_comp
    technical_new_comp <- tech_traders$type_coef *(past_prices_vector[day - 1] - tech_traders$type_price) + iid_beta[day - 51]
    tech_traders_type_expect <- past_prices_vector[day - 1] +  technical_new_comp
    
    
    #making of the decicion: "sell", "buy" or "no action"
    tech_decicions <- rep("no???action", tech_traders_count)
    fund_decicions <- rep("no???action", fund_traders_count)
    for (i in 1:tech_traders_count){
      if ((tech_traders$type_expect[i] < past_prices_vector[day-1]) && (tech_traders$asset_amount > 0)) {
        tech_decicions[i] <- "sell"
        sell_orders <- sell_orders + 1
      }
      if ((tech_traders$type_expect[i] > past_prices_vector[day-1]) && (past_prices_vector[day-1] < 0)){
        tech_decicions [i] <- "buy"
        buy_orders <- buy_orders + 1
      }
    }
    for (i in 1:fund_traders_count){
      if (TRUE){
        fund_decicions[i] <- "buy"
        buy_orders <- buy_orders + 1
      }
    }
    
    #succeeding of the orders
    for (i in 1:tech_traders_count){
      if (tech_decicions[i] == "buy"){
        tech_traders$asset_amount[i] <- tech_traders$asset_amount[i]+1
        tech_traders$cash[i] <- tech_traders$cash[i] - past_prices_vector[day1]
      }
      if (tech_decicions[i] == "sell"){
        tech_traders$asset_amount[i] <- tech_traders$asset_amount[i]-1
        tech_traders$cash[i] <- tech_traders$cash[i] +  past_prices_vector[day-1]
      }
    }
    for (i in 1:fund_traders_count){
      if (fund_decicions[i] == "buy"){
        fund_traders$asset_amount[i] <- fund_traders$asset_amount[i] + 1
        fund_traders$cash[i] <- fund_traders$cash[i] - past_prices_vector[day-1]
      }
      if (fund_decicions[i] == "sell"){
        fund_traders$asset_amount[i] <- fund_traders$asset_amount[i] - 1
        fund_traders$cash[i] <- fund_traders$cash[i] + past_prices_vector[day-1]
      }
    }
    
    #market???maker making new price
    new_price <- past_prices_vector[day-1] * ((1 + a * (buy_orders - sell_orders ))) + iid_sigma[day - 51]
    past_prices_vector <- c(past_prices_vector , new_price)
  }
  plot(past_prices_vector)
  return(past_prices_vector)
}

createTradersListMod(0.6)
new_model(0.6)

