nn <- 2400
runs <- 10000
intercept <- -2.1512
odds.ratio <- 1.5
odds.ratio2 <- 1
beta <- log(odds.ratio)
beta2<-log(odds.ratio2)
proportion  <-  replicate(
  n = runs,
  expr = {
    xtest <- rbinom(n=nn,size=1,prob=0.30)
    linpred <- intercept + (xtest * beta)
    prob <- exp(linpred)/(1 + exp(linpred))
    runis <- runif(length(xtest),0,1)
    ytest <- ifelse(runis < prob,1,0)
    prop <- length(which(ytest <= 0.5))/length(ytest)
  }
)
summary(proportion)

cas_temoinsexpoi$annee<-year(cas_temoinsexpoi$datenaissance)
cas_temoinsexpoi$annee<-as.factor(cas_temoinsexpoi$annee)

result <-  replicate(
  n = runs,
  expr = {
    xtest <- rbinom(n=nn,size=1,prob=0.30)
    xtest10 <- rbinom(n=nn,size=1,prob=0.25)
    xtest11<- rbinom(n=nn,size=1,prob=0.25)
    xtest12 <- rbinom(n=nn,size=1,prob=0.25)
    xtest13 <- rbinom(n=nn,size=1,prob=0.25)
    
    
    linpred <- intercept + (xtest * beta)+ (xtest11 * beta2)+ (xtest12 * beta2)+ (xtest13 * beta2)
    prob <- exp(linpred)/(1 + exp(linpred))
    runis <- runif(length(xtest),0,1)
    ytest <- ifelse(runis < prob,1,0)
    summary(model <- glm(ytest ~ xtest+xtest11+xtest12+xtest13,  family = "binomial"))$coefficients[2,4] < .05
  }
)
print(sum(result)/runs)
