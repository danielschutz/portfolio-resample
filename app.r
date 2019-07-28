library(fPortfolio)
library(quantmod)
library(tidyverse)
library(ggplot2)
library(xts)
library(PerformanceAnalytics)
library(MASS)
library(reshape2)
library(plotly)

# Choose ticker universe
tickers <- c('AAPL', 'FB', 'GOOGL', 'AMZN', 'NFLX')

# Create environment to store quantmod data and download data
tickerData <- new.env()
getSymbols(tickers, src="yahoo", env = tickerData)

# Figure out the lenght of the shortest history in the ticker universe
minHist <- min(as.data.frame(eapply(tickerData, function(x){nrow(x)})))

dfTickers <- data.frame('AAPL' = tail(tickerData$AAPL$AAPL.Adjusted, minHist),
                        'FB' = tail(tickerData$FB$FB.Adjusted, minHist),
                        'GOOGL' = tail(tickerData$GOOGL$GOOGL.Adjusted, minHist),
                        'AMZN' = tail(tickerData$AMZN$AMZN.Adjusted, minHist),
                        'NFLX' = tail(tickerData$NFLX$NFLX.Adjusted, minHist)
                        )

colnames(dfTickers) <- tickers
dfTickers <- as.xts(dfTickers)

# Convert daily prices to monthly returns
histMonthly <- as.ts(apply(dfTickers, 2, function(x){to.monthly(x)[,1]}))

monthlyRets <- na.omit(CalculateReturns(histMonthly))

# Find mean and covariance of historical returns
histMean <- colMeans(monthlyRets)
histCov <- cov(monthlyRets)

# Generate multivariate normal distribution
dist <- mvrnorm(10000, histMean, histCov)
rownames(dist) <- rownames(dfTickers$AAPL[-1,])
dist <- as.timeSeries(dist)


weightsList <- NULL
for(i in seq(1000)){
  

  distSample <- as.timeSeries(dist[sample(nrow(dist), 1000),])
  ports <- portfolioFrontier(data=distSample, spec = portfolioSpec(), constraints = "LongOnly")
  simWeights <- as.data.frame(getWeights(ports))
  simWeights <- simWeights %>% mutate('SIM' = i) %>% mutate('Frontier' = 1:nrow(simWeights))
  weightsList <- rbind(weightsList, simWeights)
  
}

weightedFront <- weightsList %>% group_by(Frontier) %>% summarise(mean(AAPL), mean(FB), mean(GOOGL), mean(AMZN), mean(NFLX))
colnames(weightedFront) <- c('Frontier', tickers)

vizWeights <- melt(weightedFront, id.vars = 'Frontier', value.name = 'Weight', variable.name = 'Asset')


viz <- ggplot(vizWeights) + geom_bar(aes(x=Frontier, y=Weight, colour=Asset), stat='identity', position = 'stack')
ggplotly(viz)

