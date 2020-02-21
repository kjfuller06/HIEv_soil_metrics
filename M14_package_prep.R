#load packages for HIEv data
.libPaths("C:/Users/90946112/R/win-library/3.6.2")

#load packages for downloadTOA5 function
# library(gdata)
library(devtools)
library(data.table)
install_bitbucket("remkoduursma/HIEv")
library(HIEv)
setToken("vyyk6yyDYMPwaymASW7Q")
library(reshape2)
# library(doBy)
# library(gridExtra)
library(tidyverse)
