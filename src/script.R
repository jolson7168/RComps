
##
## script5.R - development towards the Euclidean distance function
##
## Include fixes to reflect further infomation on distance calculation and the variable "count".
## Still far from perfect match.
##
## Directory tree assumed to look like:
##     /src/Rcomps/             the R working directory
##     /src/Rcomps/src/           contains all R source code for now
##     /src/Rcomps/test/data/   contains input and output data 
##     /src/Rcomps/test/config/ contains configuration files 
##
## USAGE: 
##  setwd('/src/RComps/')      ## set working directory to RComps first [AMEND THIS FOR YOUR INSTALLATION]
##  source('/src/Rcomps/src/script.R') ## run this script
##


####### EXAMPLE OF MAIN FUNCTION CALL

setwd('/src/RComps') 
## load functions into R
source('./src/functions.R') # assumes function file is in subdir "/src/RComps/src" of workind directory

## call main routine
inputfile   <- "./test/data/sample_input.dat"    
configfile  <- "./test/config/test_config.dat"  # with add'l item  
outfile     <- "./R_test_output.dat"   # can use empty string here for output to screen (i.e. stdout)
compute_comparables(inputfile, configfile, outfile)

catt('Calculations complete, see file "',outfile,'" for output', sep='')






if (FALSE) {
######### more in-depth run, for debugging etc
catt("Starting a script for debugging purposes")
inputfile  <- "./test/data/sample_input.dat"      
configfile <- "./test/config/test_config.dat"  
plotfile   <- "./test/data/R_test_plot.ps"
sampleoutputfile <- './test/data/sample_output.dat'

## load data
catt('Loading config data ...')
config  <- read.configfile(configfile)
catt('Config data loaded.')

catt('Loading input data ...')
features  <- read.featurefile(inputfile, config)
catt('Input data loaded into',nrow(features),'x',ncol(features),'dataframe.')

## show the data which was read in
cat('\n\nR version of config data:\n')
print(head(config))
cat('\n\nR version of input data:\n')
print(head(features,3))
print(tail(features,3))

## quick summary of the feature data items
cat('\n\nFrequency table view of input data:\n')
print(sapply(features, 
      	function(x) c(head(-sort(-table(x))),NAs=sum(is.na(x))),
		simplify = F)
      )

## now normalise the data by mean and standard deviation
catt('Normalising feature data ...')
features.normed <- normalise.features.2dp(features, config)
catt('Feature data normalised.')


### try lining up my output data with joe's data to look at agreement

# load Joe's distances
maxrows <- 1000 # limit number of distances to speed it up (set to Inf to switch off limit)
example.output.raw <- read.table(sampleoutputfile,                 #### EDIT THIS
					sep='|',header=F,col.names=c('Item1','Item2','Fit'),
			           colClasses=c('character','character','numeric'),stringsAsFactors=F)
ntest  <- min(nrow(example.output.raw), maxrows)
example.output <- example.output.raw[1:ntest,]

## calc my distances for same pairs of items as in the file
myoutput <- matrix(NA,ntest,3)
dimnames(myoutput)[[2]] <- c('distance','count','fit')
for (i in 1:ntest){
	myoutput[i,] <- euclidean(example.output[i,1], example.output[i,2], features.normed, config)
}

## display some of the data in the R session
cat('\n\nComparison between Oracle (cols 1 - 3) and R (cols 4-6) output:\n')
print(head(cbind(example.output,myoutput),20))
cat('\n\n')

## create postscript file
postscript(file=plotfile)
plot(example.output[,3],myoutput[,3],
	pch='+',
	xlim=c(0,100),ylim=c(0,100),
	xlab='D(x,y) based on Oracle calcn', 
	ylab='D(x,y) based on R calcn')
abline(0,1)
grid()
dev.off()

## same plot on screen
plot(example.output[,3],myoutput[,3],
	pch='+',
	xlim=c(0,100),ylim=c(0,100),
	xlab='D(x,y) based on Oracle calcn', 
	ylab='D(x,y) based on R calcn')
abline(0,1)
grid()

## manually identify points on the plot
## run this section manually by pasting into R
if (interactive <- FALSE){ 
	cat('Identify points manually by clicking on points within the plot window ....\n')
	bad.fit <- identify(example.output[,3],myoutput[,3],
		paste('D(',example.output$Item1,',',example.output$Item2,')',sep=''))
	print(head(cbind(example.output,myoutput)[bad.fit,],50))
	bad.items <- unique(c(example.output[bad.fit,1],example.output[bad.fit,2]))
	print(features[bad.items,])
	print(features.normed[bad.items,])
}
}



