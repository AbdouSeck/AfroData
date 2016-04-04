#Curl the file locally and name it conventionally

getfiledata <- function(country, round = 1, urlpath) {
  if (!dir.exists(paste(getwd(),"AfroData",sep="/",collapse=""))) {
    dir.create("AfroData")
  }
  filemeta <- curl::curl_fetch_disk(url=urlpath, path=paste("AfroData/",country,"Round",round,"data", sep="", collapse=""))
  filename <- filemeta$content
  df2 <- haven::read_sav(filename)
  return(df2)
}
