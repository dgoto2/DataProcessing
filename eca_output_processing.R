write.csv(pred$TotalCount, file="eca_cod2015_totalcount.csv")

write.csv(pred$MeanLength, file="eca_cod2015_meanlength.csv")

write.csv(pred$MeanWeight, file="eca_cod2015_meanweight.csv")

write.csv(pred$LengthIntervalsLog, file="eca_cod2015_lengthintervalslog.csv")


# compute mean values for numbers-age-length
N_age_length = matrix(nrow=146, ncol=18)
for (i in 1:18) {
  for (ii in 1:146){
  N_age_length[ii,i] = mean(pred$TotalCount[ii,i,])
  }
}
write.csv(N_age_length, file="eca_cod2015_meancount.csv")



#####
mymat = matrix(nrow=30, ncol=30) # create a 30 x 30 matrix (of 30 rows and 30 columns)

for(i in 1:dim(mymat)[1])  # for each row
{
  for(j in 1:dim(mymat)[2]) # for each column
  {
    mymat[i,j] = i*j     # assign values based on position: product of two indexes
  }
}


