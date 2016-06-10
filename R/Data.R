
#' getRoundData
#'
#' Gets a dataset for a given country and round. The default value of round is 1
#'
#' @param country A string character representation of the country whose round data you're looking to get.
#' @param round The given round from which you are grabbing data. The default value is 1.
#'
#' @return A dataframe with rows converted into their numeric values from the survey codebook. Please work with the codebook
#' @export
#'
#' @examples
#'
#'#Extracting a column to check the level of internet usage
#' sen_r1 <- getRoundData(country='Senegal',round=1)
#' internet_usage <- sen_r1 %>% dplyr::select(Q92B) %>%
#'    dplyr::mutate(CellUsage = Q92B)) %>%
#'    dplyr::select(CellUsage)
#'str(internet_usage)
#'
#'
getRoundData <- function(country,round=1) {
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
	  df1 <- data.frame(rounds,links) %>% .[nrow(.):1,]
	  data <- read_html(as.character(df1$links[round]),encoding='UTF-8') %>%
	    html_nodes('div span a') %>% extract(1:1) %>% html_attr('href') %>%
	    as.character()
	  df3 <- getfiledata(country, round, data)
	  return(df3)
	}
}

#' getMergedData
#'
#' Gets a marged dataset for a given round. The default value is round 1.
#'
#' @param roundNumber An integer to indicate the round from which you are getting the data
#'
#' @return A dataframe with rows converted into their numeric values from the survey codebook. Please work with the codebook
#' @export
#'
#' @examples
#' #Extracting a column to check what citizens think about their freedom of expression
#' merged5 <- getMergedData(roundNumber = 5)
#' expression_freedom <- merged5 %>% dplyr::select(Q17A) %>%
#'  dplyr::mutate(express_freedom = Q17A) %>%
#'  dplyr::select(express_freedom)
#'str(expression_freedom)
#'
getMergedData <- function(roundNumber = 1) {
  startpage <- "http://www.afrobarometer.org"
	if(is.na(roundNumber) || !is.numeric(roundNumber)) stop("The round variable should be a number, not an NA or string")
  if(roundNumber > length(getMergedRounds())) stop("The round number must be less than or equal to the output of getMergedRounds()")
  mergedpage <- read_html("http://afrobarometer.org/data/merged-data", encoding="UTF-8")
  Links <- mergedpage %>% html_nodes('li a') %>% html_attr('href') %>% extract(39:48) %>%
    extract(seq(1,10,2))
  MergedLink <- paste(startpage,grep(paste("round",roundNumber,sep = "-",collapse=""),Links,value = T),sep="",collapse="")
  MergedLink <- read_html(MergedLink, encoding = "UTF-8") %>% html_nodes('div span a') %>% extract(2:2) %>% html_attr('href') %>%
    as.character()
  MergedData <- getfilemerged(roundNumber, MergedLink)
  return(MergedData)
}
