## Install Rstox
devtools::install_github("SEA2DATA/Rstox", ref="develop")

## Install the next generation of Biotic, Echosounder and Landings XML data reader for R
devtools::install_github("REDUS-IMR/RNMDAPI")

### Extract the data

library(Rstox)
library(RNMDAPI)
library(data.table)

## identify the surveys to download 
survey1 <- "Barents Sea NOR-RUS 0-group cruise in autumn"
survey2 <- "Barents Sea NOR-RUS demersal fish cruise in winter"

cruise <- Rstox::getNMDinfo("cs")
names(cruise)

## Define cruise series to get
seriesToGet <- c(survey1) #c("Barents Sea NOR-RUS 0-group cruise in autumn", "Barents Sea NOR-RUS demersal fish cruise in winter")

## Download and extract XMLs for the series
extractAll <- function(series, cruise, target = c("biotic", "echosounder")) {
  
  xx <- cruise[[series]]
  
  getOne <- function(line, target) {
    ret <- lapply(target, function(x) {
      z <- Rstox::searchNMDCruise(cruisenr = line[["Cruise"]], shipname = line[["ShipName"]], datasource = x)
      if(!ncol(z) == 1) {
        ## Download file
        download.file(URLencode(z[1, "fileURL"]), "temp.xml")
        ## Convert to R data.table					
        out <-RNMDAPI::readNMDxmlFile("temp.xml", stream =T)
        unlink("temp.xml")
        return(out)
      }else {
        return(NA)
      }
    })
    
    names(ret) <- target
    
    return(ret)
  }
  
  y <- apply(xx, 1, getOne, target)
  names(y) <- apply(xx, 1, function(x) paste(x, collapse="-"))
  return(y)
}

currDir <- getwd()
setwd(tempdir())
out <- lapply(seriesToGet, extractAll, cruise)
names(out) <- seriesToGet
setwd(currDir)


### Explore the data
#saveRDS(out$`Barents Sea NOR-RUS 0-group cruise in autumn`, "barents_0group_alldata.rds")
#saveRDS(out$`Barents Sea NOR-RUS demersal fish cruise in winter`, "barents_demersal_alldata.rds")


# Some samples
names(out$"Barents Sea NOR-RUS 0-group cruise in autumn")
names(out$"Barents Sea NOR-RUS demersal fish cruise in winter")



# # Biotic data for a particular cruise
# names(out$"Barents Sea NOR-RUS demersal fish cruise in winter"$"1-2018209-Johan Hjort-2018"$biotic)
# 
# out$"Barents Sea NOR-RUS demersal fish cruise in winter"$"1-2018209-Johan Hjort-2018"$biotic$fishstation
# out$"Barents Sea NOR-RUS demersal fish cruise in winter"$"1-2018209-Johan Hjort-2018"$biotic$catchsample
# out$"Barents Sea NOR-RUS demersal fish cruise in winter"$"1-2018209-Johan Hjort-2018"$biotic$individual
# 
# # Echosounder data for a particular cruise
# names(out$"Barents Sea NOR-RUS ecosystem cruise in autumn"$"1-2018209-Johan Hjort-2018"$echosounder)
# out$"Barents Sea NOR-RUS ecosystem cruise in autumn"$"1-2018209-Johan Hjort-2018"$echosounder$sa_by_acocat

