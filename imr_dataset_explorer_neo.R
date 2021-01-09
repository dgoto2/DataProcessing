## Install Rstox
Sys.setenv('JAVA_HOME'="C:/Program Files/Java/jre1.8.0_251/") ## change the location of the java if updated
devtools::install_github("SEA2DATA/Rstox", ref="develop")

## Install the next generation of Biotic, Echosounder and Landings XML data reader for R
install.packages("data.table", repos="https://Rdatatable.gitlab.io/data.table")
# or, which install only if newer version (by git commit hash) is available
data.table::update.dev.pkg()
# install.packages("devtools")
devtools::install_github("r-lib/xml2")
devtools::install_github("StoXProject/RstoxData")

### Extract the data

library(Rstox)
library(RstoxData)
library(data.table)
library(tidyverse)

## identify the surveys to download 
setwd("/Users/daisu/Dropbox/stox.eca.BarentsSea/")

survey1 <- "Barents Sea NOR-RUS 0-group cruise in autumn"
survey2 <- "Barents Sea NOR-RUS demersal fish cruise in winter"

cruise <- Rstox::getNMDinfo("cs")
names(cruise)

## Define cruise series to get
seriesToGet <- c(survey2) #c("Barents Sea NOR-RUS 0-group cruise in autumn", "Barents Sea NOR-RUS demersal fish cruise in winter")

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
	useXsd <- ""
	if(x == "biotic") useXsd <- "nmdbioticv3"				
	else if(x == "echosounder") useXsd <- "nmdechosounderv1"				
        out <-RstoxData::readXmlFile("temp.xml", useXsd = useXsd)
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


# Merge biotic
bioticTables <- c("agedetermination", "tag","preylengthfrequencytable", "copepodedevstagefrequencytable", "prey", "individual", "catchsample", "fishstation", "mission", "missions")
mergeBiotic <- function(x) {
	out <- list()
	for (i in bioticTables) {
		tmp <- lapply(names(x), function(y) {z <- x[[y]][["biotic"]]; if(length(z) > 0 && !is.na(z)){ z <- z[[i]]; if(length(z) > 0) return(z)}})
		out[[i]] <- rbindlist(tmp)
	}
	return(out)
}
bioticAll <- lapply(out, mergeBiotic)
saveRDS(bioticAll, "barents_demersal_biotic.rds")

# Merge echosounder
echosounderTables <- c("sa", "sa_by_acocat", "ch_type", "frequency", "distance", "acocat", "echosounder_dataset", "distance_list", "acocat_list")
mergeEchosounder <- function(x) {
	out <- list()
	for (i in echosounderTables) {
		tmp <- lapply(names(x), function(y) {z <- x[[y]][["echosounder"]]; if(length(z) > 0 && !is.na(z)){ z <- z[[i]]; if(length(z) > 0) return(z)}})
		out[[i]] <- rbindlist(tmp)
	}
	return(out)
}
echosounderAll <- lapply(out, mergeEchosounder)
saveRDS(echosounderAll, "barents_demersal_ecospunder.rds")





##########################################################################################################################
## data exploration
## load downloaded data
## select biotic data
barents_demersal_biotic <- readRDS("barents_demersal_biotic.rds")
str(barents_demersal_biotic, max.level = 3)

## select ecosounder data
barents_demersal_ecospunder <- readRDS("barents_demersal_ecospunder.rds")
str(barents_demersal_ecospunder, max.level = 3)
    
## all demersal data
barents_demersal_alldata <- readRDS("barents_demersal_alldata.rds")
str(barents_demersal_alldata, max.level = 2)

## TO GET SPECIES NAME FOR INDIVIDUAL SAMPLES - USE serialnumber & catchsampleid
## ex. for 2015
catchsample <- (barents_demersal_alldata$`1-2015202-Johan Hjort-2015`$biotic$catchsample)
individual <- (barents_demersal_alldata$`1-2015202-Johan Hjort-2015`$biotic$individual)
survey <- individual %>% left_join(catchsample)
glimpse(survey)

## "TO GET (variables)" LIST





