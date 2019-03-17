library(data.table)
library(tidyverse)
library(ggplot2)
library(gdata)


# set wd with eca data
setwd("/Users/a5614/Dropbox/stox_eca_barentssea/ECA/cod catch_1989-2017_by_season/")

setwd("/Users/daisu/Dropbox/stox_eca_barentssea/eca/cod catch_1989-2017_by_season")


# Get All Files
files <- list.files(".", pattern="*.txt")


processGadget <- function(input) {
  # Assuming filename follows *report.txt and *.gadget
  gadgetFileName <- paste0(unlist(strsplit(input, "report"))[1], "gadget")
  
  if(!file.exists(gadgetFileName))
    return(NULL)
  
  lines <- readLines(gadgetFileName)
  
  # Grep using the first lines only
  lengths <- as.numeric(read.table(text=tail(unlist(strsplit(lines[grep("#lengthgroups", head(lines))],"[:]")), 1)))
  ages <- as.numeric(read.table(text=gsub("[+]", "", tail(unlist(strsplit(lines[grep("#ages", head(lines))],"[:]")), 1))))
  nLines <- length(lengths)
  
  # Get the number of iterations
  iters <- as.numeric(grep("^[0-9]+$", lines))
  
  # Prepare matrices
  temp <- array(NA, dim=list(length(lengths), length(ages), length(iters)), dimnames=list(lengths, ages, c(1:length(iters))))
  
  # Read all
  for(iter in 1:length(iters)) {
    start <- iters[iter]
    temp[, , iter] <- as.matrix(read.table(text=lines[(start + 1):(start + nLines)]))
  }
  
  # Calculate means
  out <- apply(temp, c(1, 2), mean)
  
  return(out)
}

parseOneFile <- function (input) {
  lines <- readLines(input)
  
  header <- trimws(tail(unlist(strsplit(lines[grep("extracted from", lines)],"[:]")), 1))
  
  # Some assumptions for below:
  # There is only one year and only one season in the text files. Otherwise, we will
  # only extract the last year/season from the sets
  
  year <- as.numeric(tail(unlist(strsplit(lines[grep("years: ", lines)],"[ ]")), 1))
  season <- as.numeric(gsub("season", "", tail(unlist(strsplit(lines[grep("seasons: ", lines)],"[ ]")), 1)))
  
  # Below are extracted from header
  species <-  sub("^(\\D+).*$", "\\1", header)
  
  tmpH <- unlist(strsplit(header, "[_]"))
  fleet <- tmpH[3]
  
  tmpH <- tmpH[-c(1:3)]
  area <- paste0(tmpH[-length(tmpH)], collapse = "_")
  
  # Get the table boundaries
  boundaries <- list("start" = c(grep("^age", lines), grep("^length", lines)),
                     "end" = grep("^Mean", lines))
  
  
  # Make output
  out <- lapply(seq_along(boundaries[["start"]]), 
                function (x) data.table(
                  data.table("species" = species, "fleet" = fleet, "area" = area, "year" = year, "season" = season), 
                  read.csv(text=lines[boundaries[["start"]][x]:boundaries[["end"]][x] - 1], sep = "\t", skip = 1, stringsAsFactors=FALSE)
                )
  )
  
  out[["gadget"]] <- processGadget(input)
  
  return(out)
}


# First pass
individualOut <- lapply(files, parseOneFile)

# Catch at age
CA <- rbindlist(lapply(individualOut, function(x) x[[1]]))

# Length at age
LA <- rbindlist(lapply(individualOut, function(x) x[[2]]))
LA2 = LA %>%
  filter(year==1989 & season ==2) %>%
  group_by(age) %>%
  summarise(mean = mean(mean),
            mean_sd = mean(sd))

# Weight at age
WA <- rbindlist(lapply(individualOut, function(x) x[[3]]))

# Catch at length
CL <- rbindlist(lapply(individualOut, function(x) x[[4]]))

# Age-length distr.
ALK <- lapply(individualOut, function(x) x[[5]])
names(ALK) <- files
#individualOut[[1]][[1]][,1:5]
#ALK2=(data.frame(matrix(unlist(ALK), nrow=180*861, byrow=T)))
ALK2=(individualOut[[5]][[5]])
ALK2=cbind(CL, ALK2)
ALK2=ALK2 %>% select(-mean,-sd,-X5.,-X95.)
str(ALK2)
ALK3=gather(ALK2, age, number, "1":"15", factor_key=TRUE)
ALK3$number=format(ALK3$number,  scientific = F)
levels(ALK3$age) <- list("age1"="1", "age2"="2", "age3"="3", "age4"="4", "age5"="5", "age6"="6", "age7"="7", "age8"="8", 
                                "age9"="9", "age10"="10", "age11"="11", "age12"="12", "age13"="13", "age14"="14", "age15"="15") 

LA$age = as.numeric(LA$age)
LAimm = LA %>%
  filter(age < 8) %>%
  group_by(age, season) %>%
  summarise(mean_allyears = mean(mean)) %>%
  mutate(age_season1=str_c(age, "_", season)) #%>%
LAimm$age_season2 = 1:nrow(LAimm) 
LAimm

# compute linear growth models
growImm <- lm(mean_allyears ~ age_season2, data=LAimm)  # build linear regression model on full data
print(growImm); summary(growImm)
plot(growImm)

# mature fish
LAmat = LA %>%
  filter(age > 7) %>%
  group_by(age, season) %>%
  summarise(mean_allyears = mean(mean)) %>%
  mutate(age_season1=str_c(age, "_", season)) #%>%
LAmat$age_season2 = 1:nrow(LAmat) 
LAmat

# compute linear growth models
growmat <- lm(mean_allyears ~ age_season2, data=LAmat)  # build linear regression model on full data
print(growmat); summary(growmat)
plot(growmat)


plot_LA = ggplot(LA, aes(age, mean, group = fleet)) +
  #scale_color_manual(values=wes_palette("Darjeeling1")) +
  scale_colour_manual(values = c("royalblue3",  "black", "red"))+# "darkorange1")) +
  geom_point(aes(col=fleet), size=1.6) +
  geom_smooth(aes(linetype=fleet, col=fleet), se = F, span = 0.05, size=1.2) +
  xlab("age (years)") + 
  ylab("length (cm)") +
  theme_bw() +
  theme(#panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),	
    axis.text.x = element_text(size=12), 
    axis.text.y =element_text(size=12)) +
  ## lengend
  theme(legend.background = element_blank()) +
  #theme(legend.position =  "none") +
  theme(legend.position="top") +
  ## Title appearance
  #theme(legend.title = element_text(colour="black", size=11, face="bold")) +
  theme(legend.title = element_blank()) +
  # Label appearance
  theme(legend.text = element_text(colour="black", size = 15)) +
  theme(plot.title = element_text(hjust = 0.5, size = 5)) +
  #stat_smooth(size = 1.5, color = "red", alpha = 0.5, fill = "gray")
  theme(legend.key = element_blank()) +
  facet_wrap(~ area, scales = "free_y", strip.position = "top", ncol = 3) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  theme(strip.text.x = element_text(size = 15, colour = "darkblue"))
#strip.text.x = element_blank())
plot_LA

# export the data sets
#CL$number = CL$number*1000 # convert to number in thousand
# length distribution
colnames(CL)[5] = "step"
colnames(CL)[7] = "number"
CL$age = "allages"
catchagelendst_allgears = CL %>%
  filter(fleet == "allgears" & area != "totalarea") %>%
  select(year, step,  area, age, length, number)
catchagelendst_gill = CL %>%
  filter(fleet == "gillnet" & area != "totalarea")%>%
  select(year, step,  area, age, length, number)
catchagelendst_trawlothers = CL %>%
  filter(fleet == "trawl others"& area != "totalarea" )%>%
  select(year, step,  area, age, length, number)

#; year	step	area	age	length	number
write.fwf(catchlendst_allgears, file = "BScod_catchlndst_allgears.txt", sep = " ", quote = F,
          colnames=T, rowCol=NULL, justify="right")
write.fwf(catchlendst_gill, file = "BScod_catchlndst_gill.txt", sep = " ", quote = F,
          colnames=T, rowCol=NULL, justify="right")  
write.fwf(catchlendst_trawlothers, file = "BScod_catchlndst_trawlothers.txt", sep = " ", quote = F,
          colnames=T, rowCol=NULL, justify="right")


# age-length distribution
colnames(ALK3)[5] = "step"
catchagelendst_allgears = ALK3 %>%
  filter(fleet == "allgears" & area != "totalarea") %>%
  select(year, step,  area, age, length, number)
catchagelendst_gill = ALK3 %>%
  filter(fleet == "gillnet" & area != "totalarea")%>%
  select(year, step,  area, age, length, number)
catchagelendst_trawlothers = ALK3 %>%
  filter(fleet == "trawl others"& area != "totalarea" )%>%
  select(year, step,  area, age, length, number)
#; year	step	area	age	length	number

write.fwf(catchagelendst_allgears, file = "BScod_catchagelndst_allgears.txt", sep = " ", quote = F,
          colnames=T, rowCol=NULL, justify="right")
write.fwf(catchagelendst_gill, file = "BScod_catchagelndst_gill.txt", sep = " ", quote = F,
          colnames=T, rowCol=NULL, justify="right")  
write.fwf(catchagelendst_trawlothers, file = "BScod_catchagelndst_trawlothers.txt", sep = " ", quote = F,
          colnames=T, rowCol=NULL, justify="right")

# total catch
colnames(CA)[5] = "step"
colnames(CA)[7] = "number"
CA$number = CA$number*1000 # convert to number in thousand
colnames(WA)[5] = "step"
colnames(WA)[7] = "mass"
CA = CA %>% select(fleet, year, step,  area, age, number)
WA = WA %>% select(fleet, year, step,  area, age, mass)
totalcatch_allgears = CA %>%
  left_join(WA) %>%
  filter(fleet == "allgears" & area != "totalarea") %>%
  group_by(year, step, area) %>%
  summarise(number = sum(number*mass))
totalcatch_allgears = as.matrix(totalcatch_allgears)
totalcatch_gill = CA %>%
  left_join(WA) %>%
  filter(fleet == "gillnet" & area != "totalarea") %>%
  group_by(year, step, area) %>%
  summarise(number = sum(number*mass))
totalcatch_gill = as.matrix(totalcatch_gill)
totalcatch_trawlothers = CA %>%
  left_join(WA) %>%
  filter(fleet == "trawl others" & area != "totalarea") %>%
  group_by(year, step, area) %>%
  summarise(number = sum(number*mass))
totalcatch_trawlothers = as.matrix(totalcatch_trawlothers)
write.fwf(totalcatch_allgears, file = "BScod_catch_allgears.txt", sep = " ", quote = F,
          colnames=T, rowCol=NULL, justify="right")
write.fwf(totalcatch_gill, file = "BScod_catch_gill.txt", sep = " ", quote = F,
          colnames=T, rowCol=NULL, justify="right")  
write.fwf(totalcatch_trawlothers, file = "BScod_catch_trawlothers.txt", sep = " ", quote = F,
          colnames=T, rowCol=NULL, justify="right")
