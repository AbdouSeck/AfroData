
#' getCountries
#'
#'
#' Useful helper getting all the available countries on Afrobarometer's site.
#' The listing of a country does not indicate that Afrobarometer has data on that country.
#'
#'
#' @return a list of all the available countries on Afrobarometer's site.
#' @export
#'
#' @examples
#' #List all the countries on which the organization conducts surveys
#' allcountries <- getCountries()
#' ifelse("Senegal" %in% allcountries, "Senegal is handled by Afrobarometer.", "Senegal is not handled.")
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

#' getRounds
#'
#'A helper for figuring out the list of rounds a given country has.
#'
#' @param country   A string character representation of the country whose round data you're looking to get.
#'
#' @return The list of all the available rounds (data) your country of selection has.
#' @export
#'
#' @examples
#' sen_rounds <- getRounds(country = 'Senegal')
#' paste("The latest round of Senegal is",gsub('Senegal', '',sen_rounds[1]), sep="",collapse="")
#'
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

#' getNumberofRounds
#'
#' Same thing as getRounds, but only returns the number of rounds.
#'
#' @param country A string character representation of the country whose round data you're looking to get.
#'
#' @return The number of rounds a given country has. Equivalent of \code{length(getRounds(country))}.
#' @export
#'
#' @examples
#' #getting the number of rounds for Senegal
#' sen_roundnum <- getNumberofRounds(country='Senegal')
#' paste('Senegal has ',sen_roundnum, ' rounds of data',sep="",collapse="")
#'
getNumberofRounds <- function(country) {
  return(getRounds(country) %>% length())
}

#' getMergedRounds
#'
#'Gets the all the rounds for which merged datasets are available
#'
#' @return A list of all the rounds for which there is a merged dataset available
#' @export
#'
#' @examples
#'
#' #Basic call
#' allrounds <- getMergedRounds()
#' allrounds
#'
getMergedRounds <- function() {
  mergedpage <- read_html("http://afrobarometer.org/data/merged-data", encoding="UTF-8")
  Rounds <- mergedpage %>% html_nodes('li a') %>% extract(39:48) %>% html_text() %>% extract(seq(1,10,2))
  return(Rounds)
}
