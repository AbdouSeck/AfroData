#Curl the file locally and name it conventionally

getfiledata <- function(country, round = 1, urlpath) {
  if (!dir.exists(paste(getwd(),"AfroData",sep="/",collapse=""))) {
    dir.create("AfroData")
  }

  if(nchar(urlpath) == 0) stop("The URL is missing.")

  filemeta <- curl::curl_fetch_disk(url=urlpath, path=paste("AfroData/",country,"Round",round,"data.sav", sep="", collapse=""))
  filename <- filemeta$content
  df2 <- haven::read_sav(filename) %>% as.data.frame()
  return(df2)
}

getfilemerged <- function(round = 1, urlpath) {
  if (!dir.exists(paste(getwd(),"AfroData",sep="/",collapse=""))) {
    dir.create("AfroData")
  }

  if(nchar(urlpath) == 0) stop("The URL is missing.")

  mergedmeta <- curl::curl_fetch_disk(url=urlpath, path=paste("AfroData/","Merged","Round",round,"data.sav", sep="", collapse=""))
  df2 <- haven::read_sav(mergedmeta$content) %>% as.data.frame()
  return(df2)
}
