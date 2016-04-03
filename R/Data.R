
getRoundData <- function(country,round=1,..) {
	if(is.na(country) || is.na(round) || !is.numeric(round)) {
		stop("Country and/or round variable missing.")
	} else if(is.numeric(round) && round > 6){
		stop("The selected round is beyond the number of rounds available and/or is not numeric.")
	} else {
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
	    .[grepl(regexp,gsub('\\.','',html_text(.)),ignore.case=TRUE)]
	  rounds <- countryrounds %>% html_text(.,trim=T)
	  links <- countryrounds %>% html_attr('href') %>%
	    lapply(.,function(x) paste(startpage,x,sep="",collapse="")) %>%
	    unlist()
	  df1 <- data.frame(rounds,links)
	  data <- read_html(as.character(df1$links[round]),encoding='UTF-8') %>%
	    html_nodes('div span a') %>% extract(1:1) %>% html_attr('href') %>%
	    as.character() %>% rio::import(.,haven=FALSE, reencode='UTF-8')
	  return(data)
	}
}
