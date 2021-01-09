library(data.table)

# Get All Files
files <- list.files(".", pattern="*.txt")

# Remove plus age sign data.table
removePlusSign <- function(x) {
	x[grepl("[+]", age), age := substr(age, 1, length(age) - 1)]
	x[, age:=as.numeric(age)]
}

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
CA <- removePlusSign(rbindlist(lapply(individualOut, function(x) x[[1]])))

# Length at age
LA <- removePlusSign(rbindlist(lapply(individualOut, function(x) x[[2]])))

# Weight at age
WA <- removePlusSign(rbindlist(lapply(individualOut, function(x) x[[3]])))

# Catch at length
CL <- rbindlist(lapply(individualOut, function(x) x[[4]]))

# Age-length distr.
ALK <- lapply(individualOut, function(x) x[[5]])
names(ALK) <- files

