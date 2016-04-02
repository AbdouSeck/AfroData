library(magrittr)
library(rvest)
library(xml2)
library(stringi)



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

getNumberofRounds <- function(country) {
	return(getRounds(country) %>% length())
}

getRounds <- function(country){
    startpage <- "http://www.afrobarometer.org"
    mainpage <- read_html("http://www.afrobarometer.org/countries", encoding ="UTF-8")

    countries <- mainpage %>% html_nodes('table') %>%
             extract(1:1) %>% html_nodes('td') %>%
             html_text() %>% trimws() %>%
             gsub('Ã´','o',.)

    countries <- countries %>%
             lapply(.,function(x) nchar(x)>0) %>%
             unlist() %>% countries[.] %>% stringi::stri_conv(.,from=guess_encoding(.)[1,,drop=FALSE]$encoding)
             stopifnot(country %in% countries)
    #Use eval(parse(filename,encoding="UTF-8")) to source the file insteadd of source()

    links <- mainpage %>% html_nodes('table') %>%
         extract(1:1) %>% html_nodes('td a') %>%
         html_attr('href')

    combined <- data.frame(countries=countries, links=links, countrycodes=tolower(countries))
	regexp <- paste0(substr(country,nchar(country)-3,nchar(country)),"\\sRound\\s\\d+\\sdata",sep="",collapse="")
	countrypage <- read_html(paste(startpage,combined[grepl(tolower(country),tolower(countries)),2],sep="",collapse=""),encoding='UTF-8') %>%
	               html_nodes('div a')
    if(length(countrypage[grepl('View more',html_text(countrypage))])<4) {
        stop(paste("There are not yet any data sets available for",country,sep=" ",collapse=""))
    }
	countryrounds <- countrypage[unlist(lapply(countrypage, function(x) grepl("View more",html_text(x))))] %>% 
               extract(4:4) %>% html_attr('href')%>% 
               read_html(.,encoding='UTF-8') %>% html_nodes('div a') %>% 
               .[grepl(regexp,gsub('\\.','',html_text(.)),ignore.case=TRUE)] %>% html_text(.,trim=TRUE) %>%
               stringi::stri_conv(.,from=guess_encoding(.)[1,,drop=FALSE]$encoding)
	return(countryrounds)
}