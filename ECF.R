library(quantmod)
require(tidyverse)
require(dplyr)
#my portfo
myStocks <- c("QQQ", "VOO", "AAPL","HRB","AMZN","ETHE","GBTC","JWN","ZM","NVDA")
getSymbols(myStocks, src="yahoo", from="2020-01-01", to="2021-01-01")

#chekcing 
head(ETHE)
chartSeries(ETHE)

#get data into dataframe and trasnfer to timeseries
a = lapply(myStocks, function(sym) dailyReturn(na.omit(getSymbols(sym, from="2020-01-01", to="2021-01-01", auto.assign=FALSE))))
myRetData = do.call(merge.xts,a)
colnames(myRetData) = myStocks

#define variable
#daily return time series
myRetData
combinedreturn=apply(myRetData[,1:10],2,sum)
meanDailyReturns=mean()
#make it to a dataframe, it would be less problematic...
meanDailyReturns=data.frame(meanDailyReturns)
meanDailyReturns["Stocks"]=myStocks
meanDailyReturns=rename(meanDailyReturns,ABC = meanDailyReturns )
meanDailyReturns=pivot_wider(meanDailyReturns, names_from = Stocks, values_from = ABC)
#cool

#yearly return
Annual_return=(meanDailyReturns+1)^252-1
#cool

#function set up
port_returns = weight * myRetData
risk = cov(myRetData)
weight = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
port_risk <- sqrt(t(weight) %*% (risk %*% weight))
print(port_risk)

#setting a function that can take different weight and caculate stock return
returnofport = function(x) {
  returns = 0
  for (i in 1:length(x)) {
    returns = returns + myRetData[,i] * x[i] #weighted day return time series
  }
  return (returns)
}

#find a way that can take account into both risk and weight
#as GA is not accepting two objective, so I pick Sharpe Ratio as it count both object into one
#based on risk free rate = 0%
sharpe = function(x) {
  returns = returnofport(x)
  return (mean(returns)/sqrt(cov(returns))) #mean daily return/(risk)
}
#setting number limit for the function
#forming function, everything adding up should be equal to 1 isnt it?
all_weight = function(x) {
  weight_constraint = (sum(x)-1)**2   # "sum x = 1" constraint
  for (i in 1:length(x)) {
    weight_constraint = weight_constraint + 
      max(c(0,x[i]-1))**2 + 
      max(c(0,-x[i]))**2    
  }
  return (weight_constraint)
}

#then use the constraint to form the fitness
obj = function(x) {
  # We want the maximum Sharpe ratio, so we multiply it by -1 to fit an optimization problem
  return (-sharpe(x)+100*all_weight(x))
}

#GA
ga_res = ga(type="real-valued", 
  
  # "ga" function performs maximization, so we must
  # multiply the objective function by -1
  function(x){-obj(x)}, 
  
  # x_i >= 0
  lower = rep(0,ncol(myRetData)), 
  
  # x_i <= 1
  upper = rep(1,ncol(myRetData)), 
  
  # Maximum number of iterations 
  maxiter = 5000, 
  
  # If the maximum fitness remains the same for 50
  # consecutive transactions, stop the algorithm
  run=500, 
  
  # We want to see the partial results of the process
  # while it performs
  monitor=TRUE,
  
  # Seed useful for replicating the results
  seed=1
)

#Store the resulting weights in a vector
sol = as.vector(summary(ga_res)$solution)
opt_port=cbind(names(myRetData),sol)
ga_res@solution
plot(ga_res)
solution=summary(ga_res)
solution$fitness
sum(ga_res@solution)
#The portfolio return with 0.1133568

allratio = function(x)
{
  port_returns = mean(x * myRetData)
  port_risk <- sqrt(t(x) %*% (cov(myRetData)%*% x))
  sharpe_ratio = port_returns/port_risk
  result = list("sharpe" = sharpe_ratio)
  print(result)
}
best_weight_2021=data.frame(ga_res@solution)
allratio(as.numeric(best_weight_2021[1,]))

#compare it to a balanced portfolio
#make a balanced portfolio with all 0.1
#Define Variable
weight = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
#sum(daily return)/day
#dailyReturn in time series = myRetData
combinedreturn=apply(myRetData,2,sum)
meanDailyReturns=combinedreturn/252
port_returns = mean(weight * myRetData)
risk = cov(myRetData)
weight = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)
port_risk <- sqrt(t(weight) %*% (risk %*% weight))
print(port_risk)
# Since Risk free rate is 0%
sharpe_ratio <- port_returns/port_risk
print(sharpe_ratio)
#0.009555346

#then use the above portfolio to the future to see if it is worked
myStocks_future <- c("QQQ", "VOO", "AAPL","HRB","AMZN","ETHE","GBTC","JWN","ZM","NVDA")
getSymbols(myStocks_future, src="yahoo", from="2021-01-01", to="2022-01-01")

a = lapply(myStocks_future, function(sym) dailyReturn(na.omit(getSymbols(sym, from="2021-01-01", auto.assign=FALSE))))
myRetData_future = do.call(merge.xts,a)
colnames(myRetData_future) = myStocks

#portfolio weight
weight
best_weight_2020=ga_res@solution
#random weight
ran_weight1=c(0.4,0.0,0.0,0.0,0.0,0.1,0.1,0.1,0.1,0.2)
ran_weight2=c(0.8,0.05,0.05,0.025,0.025,0.025,0.025,0.0,0.0,0.0)
ran_weight3=c(0,0,1,0,0,0,0,0,0,0)
ran_weight4=c(1,0,0,0,0,0,0,0,0,0)

allratio_future = function(x)
{
  port_returns = mean(x * myRetData_future)
  risk = cov(myRetData_future)
  port_risk <- sqrt(t(x) %*% (risk %*% x))
  sharpe_ratio = port_returns/port_risk
  result = list("sharpe" = sharpe_ratio)
  print(result)
}

#Sharpe
allratio_future(weight)
allratio_future(best_weight_2020[1,])
allratio_future(ran_weight1)
allratio_future(ran_weight2)

#since some of the stocks from the portfolio is suffering lost, so preformance not the best. the trend of different stock would not
#be the same each year, but it were able to absorb some risk, as some of my stocks return is negative.

#modify my fitness function so can adjust my target
#modify the fitness function to evaluate more possiblilty

#max return
#as we only focusing on return, we might just remove the fraction given by the risk, so we can make decision without considering the risk
return_max = function(x) {
  returns = returnofport(x)
  
  return (mean(returns)/1) #meanDailyReturns
  
}

obj_return_max = function(x) {
  return (-return_max(x)+100*all_weight(x))
}

#GA
ga_res_return = ga(type="real-valued", function(x){-obj_return_max(x)}, lower = rep(0,ncol(myRetData)), upper = rep(1,ncol(myRetData)), 
  maxiter = 5000, run=500, monitor=TRUE,seed=1)

#Store the resulting weights in a vector
ga_res_return@solution
solution_return=summary(ga_res_return)
solution_return$fitness
sum(ga_res_return@solution)

#less risky
return_less_risk = function(x) {
  returns = returnofport(x)
  
  return (sqrt(cov(returns))) #min_risk
}

#remove the - here
obj_return_less_risk = function(x) {
  return (return_max(x)+100*all_weight(x))
}
#- the riskas we want min risk as GA would aim for the max
ga_res_less_risk = ga(type="real-valued", function(x){-obj_return_less_risk(x)}, lower = rep(0,ncol(myRetData)), upper = rep(1,ncol(myRetData)), 
                   maxiter = 5000, run=500, monitor=TRUE,seed=1)

ga_res_less_risk@solution
solution_less_risk=summary(ga_res_less_risk)
solution_less_risk$fitness
sum(ga_res_less_risk@solution)

#3 models
ga_res@solution
ga_res_less_risk@solution
ga_res_return@solution

sol1 = as.vector(summary(ga_res_less_risk)$solution)
sol2 = as.vector(summary(ga_res_return)$solution)
opt_port=data.frame(cbind(names(myRetData),sol))
return_port=data.frame(cbind(names(myRetData),sol1))
min_risk_port=data.frame(cbind(names(myRetData),sol2))

#bar chart 
library(ggplot2)
# Barplot

opt_bar=
ggplot(opt_port,aes(x = fct_reorder(V1,sol), y = as.numeric(sol), fill = V1)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'V1', y = 'sol', title = "Best Solution")+
  scale_y_continuous(labels = scales::percent) 

risk_bar=
  
  ggplot(min_risk_port,aes(x = fct_reorder(V1,sol2), y = as.numeric(sol2), fill = V1)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'V1', y = 'sol2', title = "Min Wisk Solution")+
  scale_y_continuous(labels = scales::percent) 

return_bar=
  ggplot(return_port,aes(x = fct_reorder(V1,sol1), y = as.numeric(sol1), fill = V1)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'V1', y = 'sol1', title = "Max Return Solution")+
  scale_y_continuous(labels = scales::percent) 

#see it together
return_bar
risk_bar
opt_bar