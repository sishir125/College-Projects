#KL Divergence.

Pred <- read.csv('stock_prediction.csv', header = TRUE)
head(Pred)


Pred <- Pred$Predictions
Pred <- data.frame(Pred)
Pred_ret <- ROC(Pred)
Pred_ret <- na.omit(Pred_ret)
mean(Pred_ret)

AUG <- read.csv("Actual and Augmented Prices.csv", header = T)
class(AUG)
AUG <- as.data.frame(AUG)
AUG$Date <- as.Date(AUG$Date, "%d/%m/%Y")
rownames(AUG) <- AUG$Date
AUG <- AUG[-1]
head(AUG)
AUG1 <- as.xts(AUG)
class(AUG1)
portf1 <- portfolio.spec(colnames(AUG1))
portf1 <- add.constraint(portf1, type="weight_sum", min_sum=1, max_sum=1)
portf1 <- add.constraint(portf1, type="box", min=0.0125, max=.375)
portf1 <- add.objective(portf1, type="return", name="mean")
portf1 <- add.objective(portf1, type="risk", name="StdDev")

AUG_Ret <- na.omit(ROC(AUG))
length(portf1)
optPort1 <- optimize.portfolio(AUG_Ret, portf1, optimize_method = "ROI", trace=TRUE)
optPort1

weight1 <- c(0.1293, 0.0999, 0.2202, 0.0125, 0.0583, 0.1234, 0.0784, 0.0361, 0.0887, 0.0638 ,0.0894 ); length(weight1)

#return of portfolio after adjusting weights 
AUG_Ret <- Return.portfolio(AUG_Ret, weights = weight1, rebalance_on = "years")
head(AUG_Ret)
mean(AUG_Ret)


#Histogram & KULLBACK DIVERGENCE
h <- hist(port_returns, freq=FALSE, breaks=10, sub = "Port Returns"); data.frame(h$density) #portfolio return histogram
#Last returns caluculated for Weighter portfolio based on Mean-variance 
#ALL_Risk_Returns <- na.omit(ROC(ALL_Risk1)); head(ALL_Risk_Returns)
h1 <- hist(port_returns[1610:2514], freq=FALSE, breaks=seq(min(port_returns), max(port_returns), length.out = 100), main = "Portfolio Return Histogram", sub = "Port Returns"); data.frame(h1$density) #lines(xx, dnorm(xx, mean=0.000586209, sd=0.01155599)) 
h2 <- hist(Pred_ret, freq = FALSE, breaks = seq(min(Pred_ret), max(Pred_ret), length.out = 100), main = "Porfolio Prediction Return Histogram"); data.frame(h2$density)
length(Pred_ret)
h2$density[2:3] <- c(0.6745413,0.6745413)
h3 <- hist(AUG_Ret[1610:2514], freq=FALSE, breaks=seq(min(AUG_Ret), max(AUG_Ret), length.out = 100), main = "Augmented Portfolio Return Histogram"); data.frame(h3$density)
h1$density; h2$density; h3$density
#KullBack-lieber divergence
kullback <- ((h1$density))*log((h1$density)/(h2$density))
kullback <- sum((kullback))
plot(h1)
length(port_returns[1610:2514]);length(AUG_Ret[1610:2514]);length(Pred_ret)
KL1 <- KLD(h1$density, h2$density)
KL2 <- KLD(h1$density, h3$density)
plot(KL2$KLD.py.px, type="l")
plot(h1)
KL1;KL2


Kullback_Divergence = data.frame(KL1$sum.KLD.px.py, KL2$sum.KLD.px.py)
Kullback_Divergence <- t(Kullback_Divergence)
colnames(Kullback_Divergence) <- ("Kullback")
rownames(Kullback_Divergence) <- c("P(original)vsP(Predicted)", "P(original)vsP(Augmented)")
new_port_ret <- na.omit(diff(log(port), lag=1))
port_returns1 <- Return.portfolio(new_port_ret, weights = weight, rebalance_on = "years")
mean(AUG_Ret)

xx <- seq(min(port_returns[1610:2514]), max(port_returns[1610:2514]), length=200)
yy <- seq(min(Pred_ret), max(Pred_ret), length=200)
zz <- seq(min(AUG_Ret[1609:2514]), max(AUG_Ret[1609:2514]), length=200)
hist(port_returns[1610:2514], probability = TRUE)
plot(xx, dnorm(xx, mean=0.000586209, sd=0.01155599), type = "l", ylim = c(-1,40))
lines(yy, dnorm(yy, mean=0.000337663, sd=0.002989294), col = "red")
lines(zz, dnorm(zz, mean= -0.003262702, sd=0.03360387), col = "blue")


