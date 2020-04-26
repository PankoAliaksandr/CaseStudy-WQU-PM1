# Step 1. Download data from Yahoo.
# Step 2. Calculate Monthly returns
# Step 3. Calculate the monthly returns of an equally weighted portfolio
# Step 4. Graphically represent the cumulative monthly returns
# Step 5. Calculate mean, median and standard deviation
# Step 6. Calculate the overall variance of all portfolio returns


# Step 1. Download data: MSFT, YHOO, ORCL, EBAY, CSCO

#--------------------------------------------------------------------------

library(quantmod)

#MSFT
getSymbols('MSFT', from = "2016-04-29", to = "2017-04-29", src='yahoo')

#YHOO
getSymbols('YHOO', from = "2016-04-29", to = "2017-04-29", src='yahoo')

#ORCL
getSymbols('ORCL', from = "2016-04-29", to = "2017-04-29", src='yahoo')

#EBAY
getSymbols('EBAY', from = "2016-04-29", to = "2017-04-29", src='yahoo')

#CSCO
getSymbols('CSCO', from = "2016-04-29", to = "2017-04-29", src='yahoo')

#---------------------------------------------------------------------------

# Step 2. Calculate monthly returns

#---------------------------------------------------------------------------
library(xts)

#MSFT
MSFT_Monthly_Return = monthlyReturn(MSFT$MSFT.Adjusted,subset = NULL, type='arithmetic', leading = TRUE) 
#YHOO
YHOO_Monthly_Return = monthlyReturn(YHOO$YHOO.Adjusted,subset = NULL, type='arithmetic', leading = TRUE)
#ORCL
ORCL_Monthly_Return = monthlyReturn(ORCL$ORCL.Adjusted,subset = NULL, type='arithmetic', leading = TRUE)
#EBAY
EBAY_Monthly_Return = monthlyReturn(EBAY$EBAY.Adjusted,subset = NULL, type='arithmetic', leading = TRUE)
#CSCO
CSCO_Monthly_Return = monthlyReturn(CSCO$CSCO.Adjusted,subset = NULL, type='arithmetic', leading = TRUE)

# Merge all stocks' returns in 1 time series object
All_stocks_monthly_returns = ts.union(list(MSFT_Monthly_Return, YHOO_Monthly_Return, ORCL_Monthly_Return,
                                           EBAY_Monthly_Return, CSCO_Monthly_Return), dframe = TRUE)
# Drop first zero line
All_stocks_monthly_returns = All_stocks_monthly_returns[-1,]

# Change colomns names
colnames(All_stocks_monthly_returns) <- c('MSFT', 'YHOO', 'ORCL','EBAY','CSCO')
#---------------------------------------------------------------------------

# Step 3. Combinations

#---------------------------------------------------------------------------

# All possible combinations

Stocks <-c('MSFT', 'YHOO', 'ORCL', 'EBAY','CSCO')
Number_of_stocs_in_portfolio = 3
Combinations = combn(Stocks, Number_of_stocs_in_portfolio)
colnames(Combinations) <- DefineNames()

# Create portfolios (choose 3 out of 5 => 10 variants)

# FUNCTION: Partfolio Return Function
#-----------------------------------------------------------
GetPortfolioReturn = function(Return_Matrix){
  matrix = as.matrix(portfolio_instance)
  v <- c()
  col_num = ncol(matrix)
  for(i in 1:nrow(matrix)){
    sum = 0
    for(j in 1:col_num){
      sum = sum + matrix[i,j]
    }
    portfolio_instance_return = sum/col_num
    v = append(v,portfolio_instance_return)
  }
  return(v)
}
#-----------------------------------------------------------

# Final Matrix with portfolio returns

matrix_final = matrix(nrow = nrow(All_stocks_monthly_returns), ncol = ncol(Combinations))
for(i in 1:ncol(Combinations)) {
  stocks_in_portfolio <- c(Combinations[1,i], Combinations[2,i], Combinations[3,i])
  portfolio_instance = All_stocks_monthly_returns[,stocks_in_portfolio]
  matrix_final[,i] = GetPortfolioReturn(portfolio_instance)
}
colnames(matrix_final) <- DefineNames()
print(matrix_final)

# Step 4. Cumulative monthly returns 

# FUNCTION: Get cumulative returns for one portfolio
#---------------------------------------------------------------
GetCumulativeReturn = function(portfolio){
  a = array(dim = 12)
  print(a)
  for(i in 1:12){
    if(i == 1){
      a[i] = portfolio[i]
    }
    else{
      a[i] = ( (a[i-1]+1) * (1 + portfolio[i] )) - 1  
    }
    
  }
  return(a) 
}
#---------------------------------------------------------------

#FUNCTION: Get cumulative returns matrix for all portfolios
#---------------------------------------------------------------
GetCumulativeReturnAll = function(all_portfolios){
  cumulative_return_all = matrix(nrow = nrow(all_portfolios), ncol = ncol(all_portfolios))
  for(i in 1:ncol(all_portfolios)){
    one_portfolio_cum_returns = GetCumulativeReturn(portfolio = all_portfolios[,i])
    cumulative_return_all[,i] = one_portfolio_cum_returns
  }
  return(cumulative_return_all)
}
#---------------------------------------------------------------

cumulative_all = GetCumulativeReturnAll(matrix_final)

#define portfolios names
DefineNames = function(){
names <- c()
for(i in 1: ncol(Combinations)){
  name = paste(Combinations[1,i], Combinations[2,i], Combinations[3,i])
  names = append(names,name)
}
return(names)
}
  names = DefineNames()
  colnames(cumulative_all) <- names


# Step 5.  Mean, Median, St. deviation
statistics_matrix = matrix( nrow = 3, ncol = 10)
for(i in 1:10 ){
  statistics_matrix[1,i] = mean(matrix_final[,i])
  statistics_matrix[2,i] = median(matrix_final[,i])
  statistics_matrix[3,i] = sd(matrix_final[,i])
}

colnames( statistics_matrix) <- DefineNames()
rownames(statistics_matrix) <- c('Mean', 'Median', 'St. Dev.')

# Step 6. Graphical representation

#Portfolio 1
plot(cumulative_all[,1], xlab = 'months', ylab = 'return',  main = names[1])
lines(cumulative_all[,1])
abline(h = statistics_matrix[1,1], col = 'red')
abline(h = statistics_matrix[2,1], col = 'yellow')
abline(h = statistics_matrix[3,1], col = 'blue')
legend(x = "topleft", c("Mean", "Median", "Standard Deviation"), 
       col = c('red','yellow','blue'),
       lwd = c(1, 1, 1))

#Portfolio 2
plot(cumulative_all[,2], xlab = 'months', ylab = 'return',  main = names[2])
lines(cumulative_all[,2])
abline(h = statistics_matrix[1,2], col = 'red')
abline(h = statistics_matrix[2,2], col = 'yellow')
abline(h = statistics_matrix[3,2], col = 'blue')
legend(x = "topleft", c("Mean", "Median", "Standard Deviation"), 
       col = c('red','yellow','blue'),
       lwd = c(1, 1, 1))

#Portfolio 3
plot(cumulative_all[,3], xlab = 'months', ylab = 'return',  main = names[3])
lines(cumulative_all[,3])
abline(h = statistics_matrix[1,3], col = 'red')
abline(h = statistics_matrix[2,3], col = 'yellow')
abline(h = statistics_matrix[3,3], col = 'blue')
legend(x = "topleft", c("Mean", "Median", "Standard Deviation"), 
       col = c('red','yellow','blue'),
       lwd = c(1, 1, 1))

#Portfolio 4
plot(cumulative_all[,4], xlab = 'months', ylab = 'return',  main = names[4])
lines(cumulative_all[,4])
abline(h = statistics_matrix[1,4], col = 'red')
abline(h = statistics_matrix[2,4], col = 'yellow')
abline(h = statistics_matrix[3,4], col = 'blue')
legend(x = "topleft", c("Mean", "Median", "Standard Deviation"), 
       col = c('red','yellow','blue'),
       lwd = c(1, 1, 1))

#Portfolio 5
plot(cumulative_all[,5], xlab = 'months', ylab = 'return',  main = names[5])
lines(cumulative_all[,5])
abline(h = statistics_matrix[1,5], col = 'red')
abline(h = statistics_matrix[2,5], col = 'yellow')
abline(h = statistics_matrix[3,5], col = 'blue')
legend(x = "topleft", c("Mean", "Median", "Standard Deviation"), 
       col = c('red','yellow','blue'),
       lwd = c(1, 1, 1))

#Portfolio 6
plot(cumulative_all[,6], xlab = 'months', ylab = 'return',  main = names[6])
lines(cumulative_all[,6])
abline(h = statistics_matrix[1,6], col = 'red')
abline(h = statistics_matrix[2,6], col = 'yellow')
abline(h = statistics_matrix[3,6], col = 'blue')
legend(x = "topleft", c("Mean", "Median", "Standard Deviation"), 
       col = c('red','yellow','blue'),
       lwd = c(1, 1, 1))

#Portfolio 7
plot(cumulative_all[,7], xlab = 'months', ylab = 'return',  main = names[7])
lines(cumulative_all[,7])
abline(h = statistics_matrix[1,7], col = 'red')
abline(h = statistics_matrix[2,7], col = 'yellow')
abline(h = statistics_matrix[3,7], col = 'blue')
legend(x = "topleft", c("Mean", "Median", "Standard Deviation"), 
       col = c('red','yellow','blue'),
       lwd = c(1, 1, 1))

#Portfolio 8
plot(cumulative_all[,8], xlab = 'months', ylab = 'return',  main = names[8])
lines(cumulative_all[,8])
abline(h = statistics_matrix[1,8], col = 'red')
abline(h = statistics_matrix[2,8], col = 'yellow')
abline(h = statistics_matrix[3,8], col = 'blue')
legend(x = "topleft", c("Mean", "Median", "Standard Deviation"), 
       col = c('red','yellow','blue'),
       lwd = c(1, 1, 1))

#Portfolio 9
plot(cumulative_all[,9], xlab = 'months', ylab = 'return',  main = names[9])
lines(cumulative_all[,9])
abline(h = statistics_matrix[1,9], col = 'red')
abline(h = statistics_matrix[2,9], col = 'yellow')
abline(h = statistics_matrix[3,9], col = 'blue')
legend(x = "topleft", c("Mean", "Median", "Standard Deviation"), 
       col = c('red','yellow','blue'),
       lwd = c(1, 1, 1))

#Portfolio 10
plot(cumulative_all[,10], xlab = 'months', ylab = 'return',  main = names[10])
lines(cumulative_all[,10])
abline(h = statistics_matrix[1,10], col = 'red')
abline(h = statistics_matrix[2,10], col = 'yellow')
abline(h = statistics_matrix[3,10], col = 'blue')
legend(x = "topleft", c("Mean", "Median", "Standard Deviation"), 
       col = c('red','yellow','blue'),
       lwd = c(1, 1, 1))

# Step 7. Calculate general variance

# FUNCTION: Calculate general variance for all portfolios
CalculateGeneralVariance = function(monthly_returns){
  general_return <- c()
  
  for(i in 1:ncol(monthly_returns)){
    general_return = append(general_return, monthly_returns[,i])
  }
  
  general_stdev = sd(general_return)
  general_variance = general_stdev ** 2 
  return(general_variance)
}

general_variace = CalculateGeneralVariance(matrix_final)
print(paste('general variance =',general_variace))


# Step 8. Distribution

# Shapiro Test for all portfolios returns
shapiro_test <- c()
for(i in 1:ncol(matrix_final)){
  shapiro_test =  append(shapiro_test, shapiro.test(matrix_final[,i])$p.value)
}

# Portfolio 1
hist(matrix_final[,1],xlab = 'Returns', ylab = 'Frequency',  main = names[1])
print(paste('shapiro test p.value =', shapiro_test[1]))
if(shapiro_test[1] >= 0.05){
  print('Distribution is Normal')
}
if(shapiro_test[1] < 0.05){
  print('Distribution is not Normal')
}

# Portfolio 2
hist(matrix_final[,2],xlab = 'Returns', ylab = 'Frequency',  main = names[2])
print(paste('shapiro test p.value =', shapiro_test[2]))
if(shapiro_test[2] >= 0.05){
  print('Distribution is Normal')
}
if(shapiro_test[2] < 0.05){
  print('Distribution is not Normal')
}
# Portfolio 3
hist(matrix_final[,3],xlab = 'Returns', ylab = 'Frequency',  main = names[3])
print(paste('shapiro test p.value =', shapiro_test[3]))
if(shapiro_test[3] >= 0.05){
  print('Distribution is Normal')
}
if(shapiro_test[3] < 0.05){
  print('Distribution is not Normal')
}
# Portfolio 4
hist(matrix_final[,4],xlab = 'Returns', ylab = 'Frequency',  main = names[4])
print(paste('shapiro test p.value =', shapiro_test[4]))
if(shapiro_test[4] >= 0.05){
  print('Distribution is Normal')
}
if(shapiro_test[4] < 0.05){
  print('Distribution is not Normal')
}
# Portfolio 5
hist(matrix_final[,5],xlab = 'Returns', ylab = 'Frequency',  main = names[5])
print(paste('shapiro test p.value =', shapiro_test[5]))
if(shapiro_test[5] >= 0.05){
  print('Distribution is Normal')
}
if(shapiro_test[5] < 0.05){
  print('Distribution is not Normal')
}
# Portfolio 6
hist(matrix_final[,6],xlab = 'Returns', ylab = 'Frequency',  main = names[6])
print(paste('shapiro test p.value =', shapiro_test[6]))
if(shapiro_test[6] >= 0.05){
  print('Distribution is Normal')
}
if(shapiro_test[6] < 0.05){
  print('Distribution is not Normal')
}
# Portfolio 7
hist(matrix_final[,7],xlab = 'Returns', ylab = 'Frequency',  main = names[7])
print(paste('shapiro test p.value =', shapiro_test[7]))
if(shapiro_test[7] >= 0.05){
  print('Distribution is Normal')
}
if(shapiro_test[7] < 0.05){
  print('Distribution is not Normal')
}
# Portfolio 8
hist(matrix_final[,8],xlab = 'Returns', ylab = 'Frequency',  main = names[8])
print(paste('shapiro test p.value =', shapiro_test[8]))
if(shapiro_test[8] >= 0.05){
  print('Distribution is Normal')
}
if(shapiro_test[8] < 0.05){
  print('Distribution is not Normal')
}
# Portfolio 9
hist(matrix_final[,9],xlab = 'months', ylab = 'return',  main = names[9])
print(paste('shapiro test p.value =', shapiro_test[9]))
if(shapiro_test[9] >= 0.05){
  print('Distribution is Normal')
}
if(shapiro_test[9] < 0.05){
  print('Distribution is not Normal')
}
# Portfolio 10
hist(matrix_final[,10],xlab = 'months', ylab = 'return',  main = names[10])
print(paste('shapiro test p.value =', shapiro_test[10]))
if(shapiro_test[10] >= 0.05){
  print('Distribution is Normal')
}
if(shapiro_test[10] < 0.05){
  print('Distribution is not Normal')
}

