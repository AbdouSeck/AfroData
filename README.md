# AfroData

A wrapper for Afrobarometer's datasets.

## Installation
```
install.packages('devtools')
library('devtools')
devtools::install_github('AbdouSeck/AfroData')

```

## Usage

```
getCountries() # to get the countries available
getRounds(countryname) # to get the rounds of a specific country.
getNumberofRounds(countryname) # get the number of rounds of a specific country
getRoundData(country, round) # fetch the data for a specific round

```
## To do:

1. Adding functions for fetching merged datasets

2. Adding more features (i.e. getting the codebook and specifying whether to read the data with haven's `read_sav` or foreign's `read.spss`).

3. Working on vectorizing the functions, so that it becomes possible to fetch more than one dataset at a time.

4. Using some form of memoization to cache the results (this is much needed since each function call is quite expensive).



## Contributing:

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request

## History

2016

## Credits
Afrobarometer Data, [All Countries, [All Rounds], [All Available Years]

## License
License: pending...
