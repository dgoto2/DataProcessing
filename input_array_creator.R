## create an array for an index with year, season, and length class

library(tidyr)
library(gdata)
library(tidyverse)

## read input data
## capelin length-weight
refwt=read.table(file="/Users/a5614/Dropbox/stox_eca_BarentsSea/capelin/capelin_weight_length.csv", 
                   header = T, sep = ",")
## herring length-weight
refwt=read.table(file="/Users/a5614/Dropbox/stox_eca_BarentsSea/nss_herring_refwt.csv", 
                 header = T, sep = ",")

## linear regression
refwt_long = gather(refwt, year, weight, "X1972":"X2017", factor_key=T) %>% na.omit()
refwt_long = refwt_long %>% mutate(weight = weight/1000.0) %>% filter(weight > 0)
refwt_long$logL = log(refwt_long$meanL)
refwt_long$logW = log(refwt_long$weight)
summary(refwt_long)

#library(FSA)
cap_lw <- lm(logW ~ logL, data = refwt_long)
summary(cap_lw)
plot(cap_lw)
plot(logW ~ logL, data = refwt_long)
abline(cap_lw, col = 'red')

## Predictions
require(graphics)
predict(cap_lw)
newdata <- data.frame(meanL = seq(0, 25, 0.5))
predict(cap_lw, newdata, se.fit = TRUE)
pred.w.plim <- predict(cap_lw, newdata, interval = "prediction")
pred.w.clim <- predict(cap_lw, newdata, interval = "confidence")
matplot(newdata$meanL, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "weight y")


## create an array 
nyears <- 70
narea <- 1
nstep <- 4
nclass <- nrow(refwt_long) ## nlenghth, nfleet, etc.
frstyr <- 1972
ncol <- 4

data <- matrix(NA, nrow = narea*nstep*nyears*nclass, ncol = ncol)
data <- as.data.frame(data)
colnames(data) <- c("year", "step", "length", "weight") ## reference weight               
#colnames(data) <- c("year", "step", "area", "fleet", "biomass") ## reference weight 
#data$fleet <- "cod.russia.catch.future"
#data$biomass <- 0
  
for (i in 1:nyears){
  ii = (i-1)*nstep*narea*nclass + 1
  #print(ii)
  data$year[ii:(ii+nstep*narea*nclass-1)] = i+(frstyr-1)
  
  for (j in 1:nstep) {
    jj <- (i-1)*nstep*narea*nclass + (j-1)*narea*nclass + 1
    data$step[jj:(jj+nclass*narea-1)] = j
    #data$length[jj:(jj+nlength-1)] = refwt$len_class
    for (k in 1:narea) {
      kk <- (i-1)*nstep*narea*nclass + (j-1)*narea*nclass + (k-1)*nclass + 1
      data$area[kk:(kk+nclass-1)] = k
      
    }
  }
}

data$length <- refwt_long$len_class
data$weight <- refwt_long$weight

data <- data %>% select(year, step, length, weight)

## export the output
write.fwf(data, file = "BScapelin_weights.txt", sep = " ", quote = F,#width = 11,
          colnames=T, rowCol=NULL, justify="right")

