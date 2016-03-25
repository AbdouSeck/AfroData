library(magrittr)
library(rvest)
library(xml2)
library(stringi)

startpage <- "http://www.afrobarometer.org"
mainpage <- read_html("http://www.afrobarometer.org/countries", encoding ="UTF-8")

countries <- mainpage %>% html_nodes('table') %>%
             extract(1:1) %>% html_nodes('td') %>%
             html_text() %>% trimws() %>%
             gsub('Ã´','o',.)

countries <- countries %>%
             lapply(.,function(x) nchar(x)>0) %>%
             unlist() %>% countries[.] %>% repair_encoding()

#Use eval(parse(filename,encoding="UTF-8")) to source the file insteadd of source()

links <- mainpage %>% html_nodes('table') %>%
         extract(1:1) %>% html_nodes('td a') %>%
         html_attr('href')

 combined <- data.frame(countries=countries, links=links, countrycodes=tolower(countries))

getCountries <- function() {
	startpage <- "http://www.afrobarometer.org"
	mainpage <- read_html("http://www.afrobarometer.org/countries", encoding ="UTF-8")
	countries <- mainpage %>% html_nodes('table') %>%
             extract(1:1) %>% html_nodes('td') %>%
             html_text() %>% trimws() %>%
             gsub('Ã´','o',.)
    countries <- countries %>%
             lapply(.,function(x) nchar(x)>0) %>%
             unlist() %>% countries[.] %>% stringi::stri_conv(.,from=guess_encoding(.)[1,,drop=FALSE]$encoding)
    return(countries)
}

getLinks <- function() {
	links <- mainpage %>% html_nodes('table') %>%
         extract(1:1) %>% html_nodes('td a') %>%
         html_attr('href')
    return(links)
}
getNumberofRounds <- function(country) {
	return(getRounds(country) %>% length())
}

getRounds <- function(country){
	regexp <- paste0(substr(country,nchar(country)-3,nchar(country)),"\\sRound\\s[0-9]\\sdata",sep="",collapse="")
	countrypage <- read_html(paste(startpage,combined[grepl(tolower(country),tolower(countries)),2],sep="",collapse=""),encoding='UTF-8') %>%
	               html_nodes('div a')
	countryrounds <- countrypage[unlist(lapply(countrypage, function(x) grepl("View more",html_text(x))))] %>% 
               extract(4:4) %>% html_attr('href')%>% 
               read_html(.,encoding='UTF-8') %>% html_nodes('div a') %>% 
               .[grepl(regexp,html_text(.),ignore.case=TRUE)] %>% html_text(.,trim=TRUE)
	return(countryrounds)
}

getData <- function(country,round,output="foreign") {
	allrounds <- getNumberofRounds(country)
	if(is.na(country) || is.na(round) || !is.numeric(round)) {
		stop("Country and/or round variable missing.")
	} else if(is.numeric(round) && round > allrounds){
		stop("The selected round is beyond the number of rounds available and/or is not numeric.")
	} else {

	}
}