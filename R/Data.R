library(magrittr)
library(rvest)
library(xml2)
library(stringi)
library(haven)


getRoundData <- function(country,round) {
	allrounds <- getNumberofRounds(country)
	if(is.na(country) || is.na(round) || !is.numeric(round)) {
		stop("Country and/or round variable missing.")
	} else if(is.numeric(round) && round > allrounds){
		stop("The selected round is beyond the number of rounds available and/or is not numeric.")
	} else {

	}
}