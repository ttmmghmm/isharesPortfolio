require(httr)
require(XML)
require(jsonlite)
require(tcltk)

getProductURLS <- function() {
  d <- GET("http://www.ishares.com/uk/individual/en/search/product-search-results?siteEntryPassthrough=true&searchText=IDVY+IUKD+IAPD+SEDY+SLXX+IEBC+IGLT+SEML+SEMB+IBTS+IUKP&searchType=productSearch&start=1&maxResults=50&pageSize=50", timeout(5))
  d <- content(d, as="text")
  unlist(regmatches(d, gregexpr("(?<=en/products/)\\d+/[A-Z]+", d, perl=TRUE)))
}

getProduct <- function(id) {
  prefix <- "http://www.ishares.com"
  url <- paste(prefix, "/uk/individual/en/products/",id,"?siteEntryPassthrough=true", sep="")
  d <- content(GET(url), as="text", timeout(5))
  holdingsUrl <- paste(prefix, regmatches(d,regexpr("/uk/individual/en/products/.+?(?=\\.ajax\\?tab=all)", d, perl=TRUE)), ".ajax?tab=all&fileType=json", sep="")
  distributionsUrl <- paste(prefix, regmatches(d,regexpr("/uk/individual/en/products/.+?(?=\\.ajax\\?tab=distributions)", d, perl=TRUE)), ".ajax?tab=distributions&fileType=json", sep="")
  d <- gsub("\n","",iconv(d, to="UTF-8"))
  nav <- gsub("[^0-9\\.]","",regmatches(d, regexpr("(?<=nav-value\">).+?(?=<)",d,perl=TRUE)))
  dist <- gsub("\n|\\\\t|\\\\n","",content(GET(distributionsUrl, timeout(5)), as="text"))
  dist <- fromJSON(regmatches(dist,regexpr("\\[.+\\]", dist)))
  hold <- gsub("\n|\\\\t|\\\\n","",content(GET(holdingsUrl, timeout(5)), as="text"))
  hold <- fromJSON(regmatches(hold,regexpr("\\[.+\\]", hold)))
  dist <- data.frame(id=id,date=as.Date(dist$colPayableDate$display, "%d/%b/%Y"), dist=as.numeric(dist$colTotalDistribution$raw), stringsAsFactors=FALSE)
  hold <- data.frame(id=id,isin=hold$colIsin,name=hold$colIssueName,holding=as.numeric(hold$colHoldingPercent$raw)/100, sector=hold$colSectorName, country=hold$colCountryOfRisk, stringsAsFactors=FALSE)
  d <- readHTMLTable(d, header=FALSE)
  d <- d[[NROW(d)-2]]
  val <- function(n) as.character(head(d$V2[grepl(n,d$V1)],1))
  d <- data.frame(id=id,class=val("Asset Class"), nav=as.numeric(nav), currency=val("Base Currency"), expense=val("Total Expense Ratio"), freq=val("Distribution Frequency"), method=val("Methodology"), structure=val("Product Structure"), stringsAsFactors=FALSE)
  d$expense <- as.numeric(gsub("%","",d$expense)) / 100
  list(keyfacts=d,holdings=hold,dist=dist)
}

getProducts <- function(ids) {
  keyfacts <- holdings <- dist <- NULL
  pb <- tkProgressBar(min = 0, max = NROW(ids), initial = 0, title="Data collection in progress")
  for (i in 1:NROW(ids)) {
    id <- ids[i]
    setTkProgressBar(pb, i, label = paste("Downloading", id, " (", i, "of",NROW(ids),")"))
    x <- getProduct(id)
    keyfacts <- rbind(keyfacts,x$keyfacts)
    holdings <- rbind(holdings,x$holdings)
    dist <- rbind(dist,x$dist)
  }
  holdings$zone <- "W3"
  holdings$zone[grepl("United States|Canada", holdings$country)] <- "W0"
  holdings$zone[grepl("United Kingdom|Guernsey|Australia|Ireland", holdings$country)] <- "W1"
  holdings$zone[grepl("Euroland|France|Germany|Belgium|Austria|Sweden|Denmark|Netherlands|Norway|Luxembourg", holdings$country)] <- "W2"
  dist$month <- as.numeric(strftime(dist$date,"%m"))
  close(pb)
  list(keyfacts=keyfacts,holdings=holdings,dist=dist)
}

getSavedData <- function(f="prod.rds") readRDS(f)