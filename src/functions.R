
##
## functions.R - functions used to support Euclidean distance calculation
##
## Include fixes to reflect further infomation on distance calculation and the variable "count"
##
## USAGE: 
##  source('./src/functions.R') # merely loads functions into R workspace
##



##
## function to replace NAs inline
##
fillnas <- function(x, replacement.value){
	x[is.na(x)] <- replacement.value
	return(x)
}

##
## function to fill NAs forward in a vector
## each NA takes the previous available value
##
forwardfill <- function(x){
	fillvalue <- x[1]
	for (i in 1:length(x))
		if (is.na(x[i])) x[i] <- fillvalue  else  fillvalue <- x[i] 
	return(x)
}

##
## function for writing messages with time prepended
##
catt <- function(..., file='', append = FALSE){
	cat('[',as.character(Sys.time()), '] ', sep='', file=file, append = append)
	cat(..., file=file, append = append)
	cat('\n', file=file, append = append)
}

##
## standard deviation, with special handling for single values
##
sd2 <- function(x, na.rm=F){
	##
	## standard deviation function with special handling for single values
	## 	

	# all data missing gives NA result
	if (all(is.na(x))) return(NA)

	# single unique value in x => return that value
	distinct.vals <- unique(x[!is.na(x)])
	if (length(distinct.vals)==1) return(distinct.vals)
	
	# else usual sd calculation
	return(sd(x,na.rm=na.rm))

}


##
## function to read config file
##
read.configfile <- function(filename){

	## function to read in config file
	config <- read.table(filename, 
				sep='|', 
				header=F, 
				na="Null",
				stringsAsFactors = F,
				fill=F,
				row.names=NULL,
				col.names=c("FeatureName","DataType","UseInDistCalc",
						"GroupByInNorm","GroupByInDist",
						"WeightInDist") 
					)
	config$WeightInDist <- as.numeric(config$WeightInDist)
	
	## die if any rows have duplicate names
	dup.idx <- duplicated(config$FeatureName)
	if (any(dup.idx))
		stop(paste('Config file contains more than one entry for some features: ',
				paste(unique(config$FeatureName[dup.idx]), collapse=',')
			))

	return(config)
}

##
## function to read feature file (i.e. the main data)
##
read.featurefile <- function(filename, configdata){

	# read in all data rows without parsing them
	alldatarows  <- readLines(filename, -1)
	alldatarows  <- alldatarows[grepl('[a-zA-Z0-9]', alldatarows)] # exclude whitespace rows without some character or numeric data

	# chop up the pipe delimited data
	pipe.idx         <- regexpr('|',alldatarows,fixed=TRUE) # find where the pipe occurs in each string
	featurename.vec  <- substring(alldatarows,1,pipe.idx-1) # extract the first ...
	featurevalue.vec <- substring(alldatarows,pipe.idx+1,nchar(alldatarows)) # ... and second part of string

	# find the item names 
	itemnames <- alldatarows[pipe.idx == -1] # rows without pipes are the item names
	nitems    <- length(itemnames)
	itemname.vec                 <- rep(NA_character_,length(alldatarows))
	itemname.vec[pipe.idx == -1] <- alldatarows[pipe.idx == -1] # item name rows are those not containing "|"
	itemname.vec                 <- forwardfill(itemname.vec)
	if (anyDuplicated(itemnames)) stop('Duplicate items found in data, not allowed')

	# after dropping the item name rows, data is simply triples of (itemname, featurename, value)
	itemname.vec     <- itemname.vec[pipe.idx != -1]
	featurename.vec  <- featurename.vec[pipe.idx != -1]
	featurevalue.vec <- featurevalue.vec[pipe.idx != -1]
	rm(alldatarows, pipe.idx) # tidy up unnecessary variables


	# warn about duplicated data
	dup.chk.data    <- paste(itemname.vec,featurename.vec,featurevalue.vec,sep='|')
	dup.idx.fwd     <- duplicated(dup.chk.data)
	if (any(dup.idx.fwd)){

		# count up duplicates, and produce warning msg
		dup.idx.back    <- duplicated(dup.chk.data,fromLast=T)
		dup.idx         <- dup.idx.fwd|dup.idx.back
		n.dups.excl     <- sum(dup.idx.fwd)
		n.dups.left     <- sum(dup.idx) - n.dups.excl
		example.idx     <- head(which(dup.idx),5)
		warning(paste(n.dups.left,' rows had duplicates; ',
				  n.dups.excl,' duplicate data rows were excluded. ',
				  'Example rows: ',paste(example.idx,collapse=', '),
				  sep=''))

		# omit duplicates, keeping the first occurence
		idx.keep         <- !dup.idx.fwd ## keep obsvns that aren't duplicates
		featurename.vec  <- featurename.vec[idx.keep]
		itemname.vec     <- itemname.vec[idx.keep]
		featurevalue.vec <- featurevalue.vec[idx.keep]
	}

	# warn about unmatched feature names and then drop them
	unknown.featurenames.flag <- !(featurename.vec %in% c(configdata$FeatureName))
	if (any(unknown.featurenames.flag)){
		unknown.featurenames <- unique(featurename.vec[unknown.featurenames.flag])
		warning(paste(length(unknown.featurenames),' feature names in input data weren\'t defined in config file',
				  ' (',paste(head(unknown.featurenames),collapse=", "),') ',
				  ' => excluded ',sum(unknown.featurenames.flag),' data rows',
				  sep=''))
		idx.keep         <- !unknown.featurenames.flag ## keep data rows that have known featurenames only
		featurename.vec  <- featurename.vec[idx.keep]
		itemname.vec     <- itemname.vec[idx.keep]
		featurevalue.vec <- featurevalue.vec[idx.keep]
	}


	# We now have (itemname, feature name, feature value) triples in the three ".vec" variables
	# To see this in debugging, try looking at:
	#     head(data.frame(itemname.vec, featurename.vec, featurevalue.vec),20)
	
	# So we can easily populate a rectangular data frame, feature-by-feature 
	#    - all non-specified entries will be NA, but of correct data type
 	#    - start off creating a list, then coerce this to a data.frame
	#    - all rows and columns of dataframe named correctly

 	nfeatures    <- nrow(configdata)
	maindata  <- list()
	for (ifeature in 1:nfeatures){  # create columns one by one, of correct data type

		# select the data for this particular feature
		idx.vec      <- featurename.vec==configdata$FeatureName[ifeature]
		idx.itemname <- match(itemname.vec[idx.vec], itemnames)
		if (sum(idx.vec)==0) 
			warning(paste('No items had a feature value for feature <',
						configdata$FeatureName[ifeature ],'> !',sep=''))
		maindata[[ifeature]] <- 
			switch(configdata$DataType[ifeature],
				Integer = {
						# have to identify whether binary or not
						data.values <- as.integer(featurevalue.vec[idx.vec])
						unique.vals <- unique(data.values)
						if (all(unique.vals %in% 0:1)){	
							x <- integer(nitems) ## binary values have zeros for missing values
						}else{
							x <- integer(nitems) ### use zeros for integers too 
									### or put in *NA ## other integers could have NAs for omitted values
						}
						x[idx.itemname] <- data.values
						x
						},
				Real    = {
						x <- double(nitems) * NA
						x[idx.itemname] <- as.double(featurevalue.vec[idx.vec])
						x

						},
				String  = {
						x <- rep(NA_character_, nitems)
						x[idx.itemname] <- as.character(featurevalue.vec[idx.vec])
						x
						},
				{stop(paste("Unknown data type in configuration file ",
						"for feature number ",ifeature))} # error if not one of the known datatypes
			)
	}
	names(maindata) <- configdata$FeatureName # name the list items
	maindata <- data.frame(maindata, stringsAsFactors=FALSE, row.names=itemnames)	

	# return the dataframe
	return(maindata)
}


##
## Function to normalise the feature data
##
normalise.features <- function(features, config){
	##
	##    normalise the values, using stats across the relevant groupings
	##


	## collapse the grouping into a single vector
	columns.to.use <- which(config$GroupByInNorm=="Yes") # implicitly excludes NAs
	grouping <- interaction(features[,columns.to.use,drop=F], # drop=F in [] ensures it behaves consistently if a single column is selected
					drop = TRUE, # drop combinations of factors which don't exist in the data
					sep  = '|')  # use "|" as seperator as it can't exist in the given data format
	grouping.frequencies <- table(grouping)
	if (any(singles <- (grouping.frequencies<2))){
			warning(paste(length(singles)," normalisation groups have a single value.\n",
					  	"E.g. ",
						paste(config$FeatureName[columns.to.use],collapse='|')," =\n",
						paste('\t',head(names(singles)),collapse='\n'),
						sep=''))
		}

	## normalise features one by one
	features.normed <- features
	for (ifeature in 1:nrow(config)){

		## only normalise the features which are used in distance calculation
		if (config[ifeature,"UseInDistCalc"]=="Yes"){

			## extract the relevant feature vector
			idatavec <- features[,ifeature]
	
			## compute statistics for each grouping level
			mean.by.group <- tapply(idatavec, grouping, mean, na.rm=T)
			### sd.by.group   <- tapply(idatavec, grouping, sd, na.rm=T) 		
			sd2.by.group   <- tapply(idatavec, grouping, sd2, na.rm=T) # returns value itself if only one values		

			## now normalise the values

			#features.normed[,ifeature] <- 
			#	ifelse( is.na(sd.by.group[grouping]) | sd.by.group[grouping]==0,
			#		  idatavec,	
			#		  (idatavec - mean.by.group[grouping]) / sd.by.group[grouping]
			#		)
			
			features.normed[,ifeature] <- round(((idatavec - mean.by.group[grouping]) / sd2.by.group[grouping]),2)

			
		}
	}

	return(features.normed)
}


##
## Function to normalise the feature data
##
normalise.features.2dp <- function(features, config){
	##
	##    normalise the values, using stats across the relevant groupings
	##
	##    ".dp2" - round mean and sd to 2 decimals first 
	##

	## collapse the grouping into a single vector
	columns.to.use <- which(config$GroupByInNorm=="Yes") # implicitly excludes NAs
	grouping <- interaction(features[,columns.to.use,drop=F], # drop=F in [] ensures it behaves consistently if a single column is selected
					drop = TRUE, # drop combinations of factors which don't exist in the data
					sep  = '|')  # use "|" as seperator as it can't exist in the given data format
	grouping.frequencies <- table(grouping)
	if (any(singles <- (grouping.frequencies<2))){
			warning(paste(length(singles)," normalisation groups have a single value.\n",
					  	"E.g. ",
						paste(config$FeatureName[columns.to.use],collapse='|')," =\n",
						paste('\t',head(names(singles)),collapse='\n'),
						sep=''))
		}

	## normalise features one by one
	features.normed <- features
	for (ifeature in 1:nrow(config)){

		## only normalise the features which are used in distance calculation
		if (config[ifeature,"UseInDistCalc"]=="Yes"){

			## extract the relevant feature vector
			idatavec <- features[,ifeature]
	
			## compute statistics for each grouping level
			mean.by.group <- round(tapply(idatavec, grouping, mean, na.rm=T),2)
			### sd.by.group   <- tapply(idatavec, grouping, sd, na.rm=T) 		
			sd2.by.group   <- round(tapply(idatavec, grouping, sd2, na.rm=T),2) # returns value itself if only one values		

			## now normalise the values

			#features.normed[,ifeature] <- 
			#	ifelse( is.na(sd.by.group[grouping]) | sd.by.group[grouping]==0,
			#		  idatavec,	
			#		  (idatavec - mean.by.group[grouping]) / sd.by.group[grouping]
			#		)
			
			features.normed[,ifeature] <- round(((idatavec - mean.by.group[grouping]) / sd2.by.group[grouping]),2)

			
		}
	}

	return(features.normed)
}




##
## function to compute Euclidean distance between two items i and j
##
euclidean <- function(i, j, features.normed, config){

	##
	## compute Euclidean distance between two items i and j
	##

	# pull out relevant vectors for computation
	x       <- c(as.matrix(features.normed[i,config$UseInDistCalc=="Yes"]))
	y       <- c(as.matrix(features.normed[j,config$UseInDistCalc=="Yes"]))
	wt      <- config[config$UseInDistCalc=="Yes",'WeightInDist']

	# find missing values
	x.avail <- !is.na(x)
	y.avail <- !is.na(y)

	# replace missing values with zeros where the other item has a value
	x[y.avail & !x.avail] <- 0         
	y[x.avail & !y.avail] <- 0         

	# do actual calculation
	count       <- sum(ifelse(x.avail|y.avail, 1, 0))
	distance    <- sqrt(sum(wt * (x - y)^2, na.rm=T))
	fit         <- 100 - 100*distance/count

	return(c(distance,count,fit))
}



distance.calc <- function(features.normed, config, outfile="", maxrows=Inf, msg.frequency=100){

	##
	## Algorithm Step 2 & 3:
	##    Compute Euclidean distances between items x and y, 
	##    within the specified groupings,
	##    and convert this to a fit measure.
	## 
	## maxrows - maximum number of data rows to write, for debugging (Inf to override)
	## msg.frequency - write message after every <msg.frequency> calculations (NA to suppress)
	##

	## initialise output file
	if (outfile != ""){
		cat("", file=outfile, append=F) # clear output file ready for appending
	}else{
		cat('\n\nExample output data:\n',file=outfile,append=F) 
	}

	## form grouping vector according to config file
	columns.to.use <- which(config$GroupByInDist=="Yes") # implicitly excludes NAs
	grouping       <- interaction(features.normed[,columns.to.use, drop=F], # drop=F in [] ensures it behaves consistently if a single column is selected
					drop = TRUE, # drop combinations of factors which don't exist in the data
					sep  = '|')  # use "|" as seperator as it can't exist in the given data format
	itemnames      <- row.names(features.normed)

	## loop over items and calculate distance between them
	rowcount <- 0
	if (!is.na(msg.frequency)) catt('Distance calculation ',rowcount)
	for (i in 1:length(itemnames)){       

		# find all items in same group
		current.group      <- grouping[i]
		items.in.samegroup <- which(grouping == current.group)

		# only compute distance to later items to halve number of computes
		items.in.samegroup <- items.in.samegroup[items.in.samegroup > i] 

		for (j in items.in.samegroup){   
			calc.i.j <- euclidean(i, j, features.normed, config)
			cat(itemnames[i], '|', itemnames[j], '|', format(round(calc.i.j[3],2)),'\n',
				file=outfile, sep='', append=TRUE)
			rowcount <- rowcount + 1
			if (!is.na(msg.frequency) && (rowcount %% msg.frequency)==0) catt('Distance calculation ',rowcount)
			if (rowcount == maxrows) return()
		}
	}
	return()
}



##
## main function, bringing together all the above
##
compute_comparables <- function(input_file, config_file, output_file){ 

	##
	## main function
	##

	## load data
	catt('Loading config data ...')
	config     <- read.configfile(config_file)
	catt('Config data loaded.')

	catt('Loading input data ...')
	features  <- read.featurefile(input_file, config)
	catt('Input data loaded into',nrow(features),'x',ncol(features),'dataframe.')

	## now normalise the data by mean and standard deviation within groups
	catt('Normalising feature data ...')
	features.normed <- normalise.features.2dp(features, config)
	catt('Feature data normalised.')

	## now do final distance calculation
	distance.calc(features.normed, config, outfile=output_file)
}




####----------------------------- END OF FUNCTIONS ---------------------------


