
transtree <- function(bbrefid){
  
# This script will create a transaction tree for a baseball player

# read in your retrosheet transaction csv (found here: http://www.retrosheet.org/transactions/)
dat <- read.csv("./tran.csv", stringsAsFactors = FALSE)

# alright, we want to get the player we're interested in first
## let's set it as David Aardsmaa to start, we'll make it interactive later
# bbrefid <- "aardd001"

#Can uncomment this section to do an interactive entry w/o rshiny
# n <- readline(prompt="Enter a bbrefid: ")
#test <- TRUE
# while(test){
#   n <- readline(prompt="Not a valid bbrefid, try again: ")
#   if(length(grep(paste("^",n,"$",sep=""), dat$BBREFID) > .5))test <- FALSE
# }
# bbrefid <- n

# let's only go one level deep for now
level <- 1

# # let's get a subset of the csv with just our players transactions
dat.sub <- subset(dat, BBREFID == bbrefid)
real.dat <- subset(dat, TRANS_ID %in% dat.sub$TRANS_ID)
# 
# # now let's sort by date of transaction
ordered.dat <- dat.sub[order(dat.sub$DATE),]
# ordered.dat <- real.dat[order(real.dat$DATE),]
node_shapes(ordered.dat,real.dat,bbrefid)
}
