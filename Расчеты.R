library(Ecdat)
library(tidyverse)
library(conquer)
Base1<-na.omit(Ecdat::Schooling)
glimpse(Base1)
Y <- Base1$wage76
X <- as.matrix(Base1[,c(7,9,25,28)])
mod_1<-conquer(X,Y,tau=0.9)
mod_1$coeff

mod_1_1<-conquer(X,Y,tau=0.9,kernel = c("triangular"), ci = TRUE)
mod_1_1$coeff
mod_1_1$normCI


mod_2 <- conquer.cv.reg(X, Y, tau = 0.9, kernel = "Gaussian", penalty = "lasso")
mod_2$coeff



tau_t <- seq(0.1, 0.9, by = 0.05)
mod_3 <-conquer.process(X,Y,tauSeq = tau_t)
colnames(mod_3$coeff) <- tau_t
t(mod_3$coeff)

lambda_v <- 10^(seq(-4, -1, by = 0.1))
Res <- matrix(0, nrow = 31, ncol = 5)
for(i in 1:31) { 
mod_4 <- conquer.reg(X,Y,lambda = lambda_v[i],tau = 0.9, penalty = c("lasso"))
Res[i,]<-mod_4$coeff}
rownames(Res)<-lambda_v
Res


