mass81=read.table(file="/Users/a5614/Dropbox/git/gadget-barents/cod_weights.csv", 
                   header = T, sep = ",")

mass <- NULL
mass_all <- NULL
for (i in 1981:2050){
  mass <- mass81
  mass$year <- i
  mass_all <- rbind(mass_all, mass)
  
  print(paste("The year is", unique(mass$year)))
}

mass_all2 <- mass_all
mass_all2$area <- 2
mass_all_all <- rbind(mass_all, mass_all2)

library(gdata)
## export output
write.fwf(mass_all_all, file = "cod_weight.txt", sep = " ", quote = F,#width = 11,
          colnames=T, rowCol=NULL, justify="right")