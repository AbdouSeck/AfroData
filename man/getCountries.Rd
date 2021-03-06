% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Countries.R
\name{getCountries}
\alias{getCountries}
\title{getCountries}
\usage{
getCountries()
}
\value{
a list of all the available countries on Afrobarometer's site.
}
\description{
Useful helper getting all the available countries on Afrobarometer's site.
The listing of a country does not indicate that Afrobarometer has data on that country.
}
\examples{
#List all the countries on which the organization conducts surveys
allcountries <- getCountries()
ifelse("Senegal" \%in\% allcountries, "Senegal is handled by Afrobarometer.", "Senegal is not handled.")
}

