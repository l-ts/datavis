library(dplyr)
library(ggplot2)
library(lubridate)
library(dygraphs)
library(reshape2)
library(zoo)
library(highcharter)
library(xts)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

suicide2 = read.csv('./data/suicide II OECD.csv', header = TRUE, sep = ",",fileEncoding="UTF-16LE")
suicide2[[1]] = as.character(suicide2[[1]])

dfColnames = unlist(lapply(strsplit(colnames(suicide2),'\\.\\.'),function (x) gsub("^\\.+|\\.+$", "", x)))
suicide2DF = data.frame()

suicide2 = lapply(suicide2[[1]],strsplit,',')

for (i in seq(length(suicide2))) {
    for(j in seq(length(dfColnames)-1)) {
        suicide2DF[i,j] = unlist(suicide2[i])[j]
    }
}
colnames(suicide2DF) = dfColnames[seq(length(dfColnames)-1)]

suicide2DF$TIME = as.integer(lapply(suicide2DF$TIME,gsub,pattern = '"',replacement = ''))
suicide2DF$Value = as.double(suicide2DF$Value)

suicide1DF = data.frame(read.csv('./data/suicide0.csv',sep=';'))
colnames(suicide1DF) = c("GEOSEX", "2011", "2012", "2013", "2014", "2015", "Pop2011", "Pop2012", "Pop2013", "Pop2014", "Pop2015")

suicide1DF$'2011Prc' = suicide1DF$'2011' / suicide1DF$'Pop2011'
suicide1DF$'2012Prc' = suicide1DF$'2012' / suicide1DF$'Pop2012'
suicide1DF$'2013Prc' = suicide1DF$'2013' / suicide1DF$'Pop2013'
suicide1DF$'2014Prc' = suicide1DF$'2014' / suicide1DF$'Pop2014'
suicide1DF$'2015Prc' = suicide1DF$'2015' / suicide1DF$'Pop2015'

