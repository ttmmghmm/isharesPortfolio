source("data.R")
source("optimise.R")

getLabel <- function(a,b) paste(a,b,sep=".")

getLabels <- function(a,b) {
  r <- NULL
  for(x in a) for(y in b) r <- c(r, getLabel(x,y))
  r
}

getFreq <- function(n) {
  if (n=="Quarterly") return(4)
  if (n=="Semi-Annual") return(2)
  if (n=="Monthly") return(12)
  return(NA)
}

getNormalisedMonth <- function(d) {
  x <- merge(d$keyfacts[,c("id","freq"),],d$dist[,c("id","month")], by="id", all=TRUE)
  x <- aggregate(x$id, by=list(id=x$id,freq=x$freq,month=x$month),NROW)
  x <- x[order(x$id,x$x, decreasing=TRUE),]
  y <- unique(x[,c("id","freq")])
  r <- NULL
  for (i in 1:NROW(y)) r <- rbind(r, head(x[x$id==y[i,]$id,],y[i,]$freq))
  r[,c("id","month")]
}

getEnhanced <- function(d) {
  x <- aggregate(d$dist$dist, by=list(id=d$dist$id), mean)
  names(x) <- c("id","dist")
  d$keyfacts <- merge(d$keyfacts, x, by="id", all.x=TRUE)
  d$keyfacts$dist <- d$keyfacts$dist/d$keyfacts$nav
  d$keyfacts$freq <- unlist(lapply(d$keyfacts$freq, getFreq))
  d
}

getCashflowMatrix <- function(d) {
  d$holdings <- d$holdings[d$holdings$sector != "Cash and/or Derivatives",]
  m <- matrix(0,nrow=NROW(unique(d$holdings$zone))*12, ncol=NROW(d$keyfacts))
  colnames(m) <- d$keyfacts$id
  rownames(m) <- getLabels(unique(d$holdings$zone), 1:12)
  x <- aggregate(d$holdings$holding, by=list(id=d$holdings$id, zone=d$holdings$zone), sum)
  x <- merge(x,d$keyfacts[,c("id","dist")], by="id", all=TRUE)
  x$dist <- x$x * x$dist
  x <- merge(getNormalisedMonth(d), x, by="id", all=TRUE)
  m[cbind(getLabel(x$zone,x$month), x$id)] <- x$dist
  m
}

printResults <- function(d,m,o,investment=100000) {
  names(o) <- colnames(m)
  o <- o[o > 0]
  print("INVESTMENTS")
  print("------------------------------------------------")
  print(o)
  y <- rowSums(t(t(m) * o))
  y <- aggregate(as.vector(y), by=list(month=as.numeric(unlist(regmatches(names(y),regexpr("\\d+$",names(y)))))), sum)
  y$monthly <- investment * y$x
  y$cumulative <- cumsum(y$monthly)
  y$x <- NULL
  print("CASHFLOW")
  print("------------------------------------------------")
  print(y)
  print("ZONE EXPOSURE")
  print("------------------------------------------------")
  h <- d$holdings[d$holdings$id %in% names(o),]
  h <- aggregate(h$holding, by=list(id=h$id, zone=h$zone), sum)
  h <- merge(h, data.frame(id=names(o), alloc=as.vector(o)), by="id",all.x=TRUE)
  h$exposure <- h$x * h$alloc
  h <- aggregate(h$exposure, by=list(zone=h$zone), sum)
  h$exposure <- h$x
  h$x <- NULL
  print(h)
}

ids <-getProductURLS()
d <- getProducts(ids)
d <- getEnhanced(d)
#d <- getEnhanced(getSavedData())
m <- getCashflowMatrix(d)
o <- getOptimisedWeights(m, 0.045)
printResults(d,m,o,100000)