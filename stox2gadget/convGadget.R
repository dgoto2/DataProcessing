# converting Stox output files to a Gadget format
# 28 June 2018

library(data.table)


processGadget <- function(data){

  # Remove NAs
  data <- na.omit(data, cols=c("age","LenGrp"))

  # Get MaxMin length
  maxMinLen <- sort(unique(data[["LenGrp"]]))
  maxMinLen <- c(head(maxMinLen,1),tail(maxMinLen,1))
 
  # Create mean values because we still have .ids
  stage1 <- data[, lapply(.SD, mean, na.rm=TRUE), by = c("year","LenGrp","age"), .SDcols =! ".id"]

  # Delete zero
  stage1 <- na.omit(stage1)

  # Summing
  stage2 <- stage1[, lapply(.SD, sum, na.rm=TRUE), by = c("year","LenGrp"), .SDcols =! c("age")]
  stage3 <- stage1[, lapply(.SD, sum, na.rm=TRUE), by = c("year"), .SDcols =! c("age", "LenGrp")]

  # Creating additional columns
  stage1 <- stage1[, c("step", "area"):= list(1,"allareas")]
  stage2 <- stage2[, c("step", "area", "age") := list(1,"allareas","allages")]
  stage3 <- stage3[, c("step", "area", "age", "LenGrp") := list(1,"allareas","allages", paste("len", maxMinLen[1], maxMinLen[2], sep="-"))]

  ret <- list(stage1,stage2,stage3)

  # Set column order
  ret <- lapply(ret, function(x){setcolorder(x, c("year", "step", "area","age", "LenGrp", "Ab.Sum", "Weight.Sum"))})

  return(ret)

}

processResults <- readRDS("results.rds")

gadgetFormats <- lapply(names(processResults), function(x) processGadget(processResults[[x]]$tempTBLLK))
names(gadgetFormats) <- names(processResults)

#write.csv(gadgetFormats, file="results_gadget.csv")

save(gadgetFormats, file="results_gadget.Rdata")

saveRDS(gadgetFormats, file=paste0(homeDir,"/results_gadget.rds"))


