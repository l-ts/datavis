library(shiny)
library(shinyjs)
library(DT)
library(highcharter)

server = shinyServer(function(input, output, session) {
    
    output$HomePageMsc = renderText('MSc in Data Science')

    output$HomePageCourse = renderText('Data visualization and communication')
    
    output$HomePagePT = renderText('Part Time 2017-2019')
    
    output$HomePageAuthor = renderUI({
        HTML('Author <b>Tsolas Leonidas</b>')
    })
    
    output$HomePageDescTitle = renderText('Objective of the Study')
    output$HomePageDescription = renderText('The objective of this study is to compare the suicide rates in Greece with the rest Europian countries and determine if there is a higher incremental trend or a different behaviour')
    
    renderPlot1_1 = function() {

        meltdf = melt(suicide2DF[,c("LOCATION","TIME","Value")],id=c("LOCATION","Value","TIME"))
        
        meltdfRestEurope = data.frame(
            subset(meltdf, LOCATION != "GRC") %>% 
                group_by(TIME) %>% 
                summarise(Value = mean(Value)) %>% 
                mutate(LOCATION = 'Rest Europe Average*')
        )
        
        restEuropeCountries = paste(unique(meltdf[!meltdf$LOCATION %in% c('GRC','Rest Europe Average*'),'LOCATION']),collapse = ',')
        meltdf = rbind(meltdf[,c("LOCATION","TIME","Value")],meltdfRestEurope[,c("LOCATION","TIME","Value")])
        
        colnames(meltdf) = c("Location", "Year", "Suicides")
        
        meltdfDygraph = reshape(meltdf, direction = "wide", idvar = "Year", timevar = "Location")
        colnames(meltdfDygraph) = lapply(colnames(meltdfDygraph),gsub,pattern = 'Suicides.',replacement = '')
        
        meltdfDygraph$Date = as.Date(paste0(as.character(meltdfDygraph$Year),'-01-01'),"%Y-%m-%d")
        
        meltdfDygraph=xts(meltdfDygraph[,-1],meltdfDygraph$Date)
        
        output$FDDygraph1 = renderDygraph({
            dygraph(meltdfDygraph, main = "Annual number of suicides in Europe (per 100,000 people) ") %>% 
                dyAxis("x", drawGrid = FALSE) %>%
                dyOptions(includeZero = TRUE,
                          colors = c('black','black','black','black','black',
                                     'black','black','blue','black','black',
                                     'black','black','black','black','black',
                                     'black','black','black','black','black',
                                     'black','black','black','red'),
                          axisLineColor = "navy", 
                          gridLineColor = "lightblue",
                          drawXAxis = TRUE) %>%
                dyRangeSelector(dateWindow = c("2004-01-01", "2014-06-01")) %>%
                dyAxis("x", axisLabelFormatter="function(d) { return d.getFullYear() }") %>%
                dyAxis("x", ticker="function(a, b, pixels, opts, dygraph, vals) {
                       return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)
        }")
            
    }) 
    }
    
    renderPlot1_2 = function() {

        meltdf = melt(suicide2DF[,c("LOCATION","TIME","Value")],id=c("LOCATION","Value","TIME"))
        
        meltdfRestEurope = data.frame(
            subset(meltdf, LOCATION != "GRC") %>% 
                group_by(TIME) %>% 
                summarise(Value = mean(Value)) %>% 
                mutate(LOCATION = 'Rest Europe Average*')
        )
        
        restEuropeCountries = paste(unique(meltdf[!meltdf$LOCATION %in% c('GRC','Rest Europe Average*'),'LOCATION']),collapse = ',')
        meltdf = rbind(meltdf[,c("LOCATION","TIME","Value")],meltdfRestEurope[,c("LOCATION","TIME","Value")])
        
        colnames(meltdf) = c("Location", "Year", "Suicides")
        
        meltdfDygraph = reshape(meltdf, direction = "wide", idvar = "Year", timevar = "Location")
        colnames(meltdfDygraph) = lapply(colnames(meltdfDygraph),gsub,pattern = 'Suicides.',replacement = '')
        
        meltdfDygraph$Date = as.Date(paste0(as.character(meltdfDygraph$Year),'-01-01'),"%Y-%m-%d")
        
        meltdfDygraph=xts(meltdfDygraph[,-1],meltdfDygraph$Date)
        
        meltdfDygraph = meltdfDygraph[ , which(names(meltdfDygraph) %in% input$FDCountrySelector)]
        
        output$FDDygraph2 = renderDygraph({
            dygraph(meltdfDygraph, main = "Annual number of suicides in Europe (per 100,000 people) ") %>% 
                dyAxis("x", drawGrid = FALSE) %>%
                dyOptions(includeZero = TRUE,
                          axisLineColor = "navy", 
                          gridLineColor = "lightblue",
                          drawXAxis = TRUE) %>%
                dyRangeSelector(dateWindow = c("2004-01-01", "2014-06-01")) %>%
                dyAxis("x", axisLabelFormatter="function(d) { return d.getFullYear() }") %>%
                dyAxis("x", ticker="function(a, b, pixels, opts, dygraph, vals) {
                           return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)
                    }")
            
        }) 
    }
    
    renderPlot1_3 = function() {

        meltdf = melt(suicide2DF[,c("LOCATION","TIME","Value")],id=c("LOCATION","Value","TIME"))
        
        meltdfRestEurope = data.frame(
            subset(meltdf, LOCATION != "GRC") %>% 
                group_by(TIME) %>% 
                summarise(Value = mean(Value)) %>% 
                mutate(LOCATION = 'Rest Europe Average*')
        )
        
        restEuropeCountries = paste(unique(meltdf[!meltdf$LOCATION %in% c('GRC','Rest Europe Average*'),'LOCATION']),collapse = ',')
        meltdf = rbind(meltdf[,c("LOCATION","TIME","Value")],meltdfRestEurope[,c("LOCATION","TIME","Value")])
        
        colnames(meltdf) = c("Location", "Year", "Suicides")
        
        output$FDPlot3 = renderPlot({
            ggplot(meltdf, aes(Year, Suicides)) +
                geom_line() +
                facet_wrap(~Location, strip.position = "bottom") +
                theme(strip.background = element_blank(), strip.placement = "outside")
        }) 
        
    }
    
    renderPlot1_4 = function() {

        meltdf = melt(suicide2DF[,c("LOCATION","TIME","Value")],id=c("LOCATION","Value","TIME"))
        
        meltdfRestEurope = data.frame(
            subset(meltdf, LOCATION != "GRC") %>% 
                group_by(TIME) %>% 
                summarise(Value = mean(Value)) %>% 
                mutate(LOCATION = 'Rest Europe Average*')
        )
        
        restEuropeCountries = paste(unique(meltdf[!meltdf$LOCATION %in% c('GRC','Rest Europe Average*'),'LOCATION']),collapse = ',')
        meltdf = rbind(meltdf[,c("LOCATION","TIME","Value")],meltdfRestEurope[,c("LOCATION","TIME","Value")])
        
        colnames(meltdf) = c("Location", "Year", "Suicides")
        
        output$FDPlot4 = renderPlot({
            ggplot(subset(meltdf, Location == input$FDCountrySelector2),aes(x=Year,y=Suicides, color = Location,ymin=0, ymax=40)) +
                geom_line(size=1) + 
                labs(y = 'Suicides')+ 
                theme(axis.line = element_line(
                    colour = 'darkblue',
                    size = 0.75,
                    linetype = "solid",
                    color = '#fcfcfc'),
                    text=element_text(size=11, 
                                      #       family="Comic Sans MS"))
                                      #       family="CM Roman"))
                                      #       family="TT Times New Roman"))
                                      #       family="Sans"))
                                      family="Serif"))+
                scale_y_continuous(breaks=seq(from = 1, to = 40, by = 2)) +
                scale_x_continuous(breaks=seq(from = 2004, to = 2014, by = 1)) +
                geom_ribbon(aes(ymin=0, ymax=Suicides), alpha=.6,fill = '#ff8080')
        })
    }
    
    renderPlot2_1 = function() {
        data(wrld_simpl)
        wrld_simpl@data$id <- wrld_simpl@data$NAME
        wrld = fortify(wrld_simpl, region="id")
        wrld = subset(wrld, id %in% suicide1DF$GEOSEX)
        
        output$myImage <- renderImage({
            # Read myImage's width and height. These are reactive values, so this
            # expression will re-run whenever they change.
            width  <- session$clientData$output_myImage_width
            height <- session$clientData$output_myImage_height
            
            # For high-res displays, this will be greater than 1
            pixelratio <- session$clientData$pixelratio
            
            # A temp file to save the output.
            outfile <- tempfile(fileext='.png')
            
            # Generate the image file
            png(outfile, width = width*pixelratio, height = height*pixelratio,
                res = 72*pixelratio)
            ggplot() +
                geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.1) +
                geom_map(data=suicide1DF, map=wrld, aes(map_id=GEOSEX, fill=GEOSEX),  color="white", size=0.1) +
                scale_fill_manual(values=colorRampPalette(brewer.pal(9, 'Reds'))(length(suicide1DF$'2015Prc')),name="Country") +
                coord_map() + 
                labs(x="", y="") +
                theme(plot.background = element_rect(fill = "transparent", colour = NA),
                      panel.border = element_blank(),
                      panel.background = element_rect(fill = "transparent", colour = NA),
                      panel.grid = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      legend.position = "none"
                )
            dev.off()
            
            # Return a list containing the filename
            list(src = outfile,
                 width = width,
                 height = height,
                 alt = "This is alternate text")
        }, deleteFile = TRUE)
    }
    
    observeEvent(input$FD,{
        if(input$FD == 'Plot 1'){
            renderPlot1_1()
        }
               
        if(input$FD == 'Plot 2'){
            renderPlot1_2()
        }
        
        if(input$FD == 'Plot 3'){
            renderPlot1_3()
        }
        
        if(input$FD == 'Plot 4'){
            renderPlot1_4()
        }
    })
    
    observeEvent(input$FDCountrySelector,{
        if(input$FD == 'Plot 2') {
            renderPlot1_2()
        }
    })
    
    observeEvent(input$FDCountrySelector2,{
        renderPlot1_4()
    })
    
    observeEvent(input$SD,{
        if(input$SD == 'Plot 1'){
            renderPlot2_1()
        }
    })
    
})