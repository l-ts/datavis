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


#dataGRC = suicide1DF[suicide1DF$'GEO/SEX' == 'Greece',]
#dataGRC = melt(dataGRC[,c('2011Prc','2012Prc','2013Prc','2014Prc','2015Prc')])
#dataGRC$variable = c(2011,2012,2013,2014,2015) 
#dataGRC$Country = 'Greece'
#
#population = read.csv('suicide0.csv')

#meltdf = melt(suicide2DF[,c("LOCATION","TIME","Value")],id=c("LOCATION","Value","TIME"))
#
#meltdfRestEurope = data.frame(
#    subset(meltdf, LOCATION != "GRC") %>% 
#    group_by(TIME) %>% 
#    summarise(Value = mean(Value)) %>% 
#    mutate(LOCATION = 'Rest Europe Average*')
#)
#
#restEuropeCountries = paste(unique(meltdf[!meltdf$LOCATION %in% c('GRC','Rest Europe Average*'),'LOCATION']),collapse = ',')
#meltdf = rbind(meltdf[,c("LOCATION","TIME","Value")],meltdfRestEurope[,c("LOCATION","TIME","Value")])
#
#colnames(meltdf) = c("Location", "Year", "Suicides")
#
#DS2P2 = ggplot(subset(meltdf, Location == "GRC"),aes(x=Year,y=Suicides, color = Location)) +
#    geom_line(size=1) + 
#    labs(y = 'Suicides')+ 
#    theme(axis.line = element_line(
#        colour = 'darkblue',
#        size = 0.75,
#        linetype = "solid",
#        color = '#fcfcfc'),
#        text=element_text(size=11, 
#                          #       family="Comic Sans MS"))
#                          #       family="CM Roman"))
#                          #       family="TT Times New Roman"))
#                          #       family="Sans"))
#                          family="Serif"))+
#    scale_y_continuous(breaks=seq(from = 1, to = 17, by = 2)) +
#    scale_x_continuous(breaks=seq(from = 2004, to = 2014, by = 1)) +
#    geom_ribbon(aes(ymin=0, ymax=40), alpha=.6,fill = '#ff8080')

### multiplot
#multiPlotCounter = 0
#for (Country in unique(meltdf$Location)) {
#    assign(
#        paste0('plot_',as.character(multiPlotCounter)),
#        ggplot(meltdf,aes(x=Year,y=Suicides)) +
#            geom_line(data=subset(meltdf, Location == Country), size=1) +
#            labs(y = '')+ #,caption = restEuropeCountries) +
#            theme(
#                text=element_text(
#                    size=11,
#                    family="Serif"
#                ),
#                axis.title.x=element_blank(),
#                axis.ticks.x=element_blank(),
#                axis.text.x=element_blank(),
#                axis.title.y=element_blank(),
#                axis.text.y=element_blank(),
#                axis.ticks.y=element_blank()
#            )+
#            ylim(1, 25) +
#            xlim(2004,2014) +
#            ggtitle(Country)
#    )
#    multiPlotCounter = multiPlotCounter + 1
#}

#multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#    library(grid)
#    
#    # Make a list from the ... arguments and plotlist
#    plots <- c(list(...), plotlist)
#    
#    numPlots = length(plots)
#    
#    # If layout is NULL, then use 'cols' to determine layout
#    if (is.null(layout)) {
#        # Make the panel
#        # ncol: Number of columns of plots
#        # nrow: Number of rows needed, calculated from # of cols
#        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                         ncol = cols, nrow = ceiling(numPlots/cols))
#    }
#    
#    if (numPlots==1) {
#        print(plots[[1]])
#        
#    } else {
#        # Set up the page
#        grid.newpage()
#        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#        
#        # Make each plot, in the correct location
#        for (i in 1:numPlots) {
#            # Get the i,j matrix positions of the regions that contain this subplot
#            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#            
#            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                            layout.pos.col = matchidx$col))
#        }
#    }
#}

#DS2P3 = multiplot(plot_0,plot_1, plot_2, plot_3, plot_4, plot_5,
#          plot_6, plot_7, plot_8, plot_9, plot_10,
#          plot_11, plot_12, plot_13, plot_14, plot_15,
#          plot_16, plot_17, plot_18, plot_19, plot_20,
#          plot_21, plot_22, plot_23, cols=6)

#meltdfDygraph = reshape(meltdf, direction = "wide", idvar = "Year", timevar = "Location")
#colnames(meltdfDygraph) = lapply(colnames(meltdfDygraph),gsub,pattern = 'Suicides.',replacement = '')
#
#meltdfDygraph$Date = as.Date(paste0(as.character(meltdfDygraph$Year),'-01-01'),"%Y-%m-%d")
#
#meltdfDygraph=xts(meltdfDygraph[,-1],meltdfDygraph$Date)

#DS2P1 = dygraph(meltdfDygraph, main = "") %>% 
#    dyAxis("x", drawGrid = FALSE) %>%
#    dyOptions(includeZero = TRUE,
#              colors = c('black','black','black','black','black',
#                         'black','black','blue','black','black',
#                         'black','black','black','black','black',
#                         'black','black','black','black','black',
#                         'black','black','black','red'),
#              axisLineColor = "navy", 
#              gridLineColor = "lightblue",
#              drawXAxis = F
#    ) %>%
#dyRangeSelector(dateWindow = c("2004-01-01", "2017-01-01"))

#library(tidyverse)
#library(readxl)
#
#
#
#
##multiPlotCounter = 0
##
##for (Country in suicide1DF$'GEO/SEX'){
##    data = suicide1DF[suicide1DF$'GEO/SEX' == Country,]
##    data = melt(data[,c('2011Prc','2012Prc','2013Prc','2014Prc','2015Prc')])
##    data$variable = c(2011,2012,2013,2014,2015)
##    data$Country = Country
##    data = rbind(data,dataGRC)
##    
##    assign(
##        paste0('plot1_',as.character(multiPlotCounter)),
##        ggplot(data,aes(x=variable,y=value,color = Country)) +
##            geom_line(data=subset(data, Country == "Greece"), size=0.5) + 
##            geom_line(data=subset(data, Country != "Greece"), size=0.5) + 
##            labs(y = '')+
##            theme(
##                text=element_text(
##                    size=11,
##                    family="Serif"
##                )
##            )+
##        ylim(-0.2,0.6) +
##        xlim(2011,2015) +
##        ggtitle(Country) +
##        scale_color_manual(values=c("#000000", "#0000ff"))
##    )
##    multiPlotCounter = multiPlotCounter + 1
##}
##DS1P1 = multiplot(plot1_0,plot1_1, plot1_2, plot1_3, plot1_4, plot1_5,
##          plot1_6, plot1_7, plot1_8, plot1_9, plot1_10, cols=7)
#          #plot1_11, plot1_12, plot1_13, plot1_14, plot1_15,
#          #plot1_16, plot1_17, plot1_18, plot1_19, plot1_20,
#          #plot1_21, plot1_22, plot1_23, plot1_24, plot1_25,
#          #plot1_26, plot1_27, plot1_28, plot1_29, plot1_30,
#          #plot1_31, plot1_32, plot1_33, cols=7)
#
##library(rworldmap)
##library(mapproj)
##
##worldMap = data.frame(getMap())
##worldMap$'GEO/SEX' = as.character( worldMap$NAME)
##suicide1DF = data.frame(inner_join(suicide1DF, worldMap, by = 'GEO/SEX'))
#suicide1DF$diff = (suicide1DF$'2015' - suicide1DF$'2011')/suicide1DF$'2011'
#
#
#suicide1DF = suicide1DF[order(suicide1DF$diff, decreasing = TRUE),]
#
#library(scales)
#suicide1DF$GEOSEX = suicide1DF$'GEO/SEX'
#
#suicide1DF$colours  = ifelse(suicide1DF$'GEOSEX' == 'Greece','Greece',ifelse(suicide1DF$'GEOSEX' == 'European Union (current composition)','Europe Average','Other'))
#
#DS1P2 = ggplot(suicide1DF, aes(x =  reorder(GEOSEX, -diff), y = diff, color = colours)) +
#    geom_bar(stat = "identity") + 
#    scale_y_continuous("difference between 2011-2015(%)", labels = percent_format()) +
#    theme(
#        text=element_text(
#            size=11
#        ),
#        axis.title.x = element_text(size = 11, colour = "black"),
#        axis.ticks.x=element_blank(),
#        axis.text.x=element_blank()
#    ) + 
#    labs(x = "Countries") + 
#    scale_fill_manual(values = colours)
#P <- ggplot() + geom_polygon(data = suicide1DF, aes(x = LON, y = LAT, group = suicide1DF$GEO.SEX, fill = suicide1DF$diff * 1000),
#                             colour = "black", size = 0.1) +
#    coord_map(xlim = c(-13, 35),  ylim = c(32, 71))
#
#P <- P + scale_fill_gradient(name = "Growth Rate", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50")
#
#P <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
#    #panel.background = element_rect(fill = NA, colour = NA),
#    axis.text.x = element_blank(),
#    axis.text.y = element_blank(), axis.ticks.x = element_blank(),
#    axis.ticks.y = element_blank(), axis.title = element_blank(),
#    #rect = element_blank(),
#    plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))