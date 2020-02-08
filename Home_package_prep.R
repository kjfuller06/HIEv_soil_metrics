#load packages for HIEv data
.libPaths("C:/Users/kjf00/Documents/R/win-library/3.6")

#load packages for downloadTOA5 function
library(tidyr)
# library(gdata)
library(devtools)
library(data.table)
install_bitbucket("remkoduursma/HIEv")
library(HIEv)
setToken("vyyk6yyDYMPwaymASW7Q")
library(reshape2)
# library(doBy)
