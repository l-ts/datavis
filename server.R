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
            labs(y = 'Suicide Rates')+ 
            theme(axis.line = element_line(
                colour = 'darkblue',
                size = 0.75,
                linetype = "solid",
                color = '#fcfcfc'),
                text=element_text(size=11,family="Serif"))+
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
        suicide1DF$teste = suicide1DF$'2015Prc'*100000
        output$SDPlot1 = renderPlot({
            ggplot() +
            geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.1) +
            ggtitle('Suicides rates in Europe - 2015 (Scale 1-30 people per 100,000)') +
            geom_map(data=suicide1DF, map=wrld, aes(map_id=GEOSEX, fill=factor(teste)),  color="white", size=0.1) +
            scale_fill_manual(values=colorRampPalette(brewer.pal(9, 'Reds'))(length(suicide1DF$teste)),name="") +
            coord_map() + 
            labs(x="", y="") +
            theme(plot.background = element_rect(fill = "transparent", colour = NA),
                  panel.border = element_blank(),
                  panel.background = element_rect(fill = "transparent", colour = NA),
                  panel.grid = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  plot.title = element_text(hjust = 0.5),
                  aspect.ratio = 1/1.4
            ) +
            guides(fill = FALSE)
        })
    }
    
    
    renderPlot2_2 = function() {
        suicide1DF$'Prc2015_2011' = (suicide1DF$'2015'-suicide1DF$'2011') / suicide1DF$'2011'
        suicide1DF$'GreeceInd' = ifelse(suicide1DF$GEOSEX == 'Greece',1,0)
        suicide1DF = suicide1DF %>% arrange(desc(Prc2015_2011))
        
        output$SDPlot2 = renderPlot({
            ggplot(suicide1DF,title = '', aes(x=as.factor(reorder(GEOSEX,Prc2015_2011)), y=Prc2015_2011, fill = factor(GreeceInd))) + 
            geom_bar(stat= "identity", width = 0.8) +
            coord_flip()+ 
            xlab("") + 
            ylab('% variation') +
            ggtitle('% variation in suicides rates percentages from 2011 to 2015 per country') +
            theme_economist() +
            theme_economist_white(gray_bg = FALSE)+
            scale_fill_manual(values=c("#938b8b", "#7ebdfb"), labels=c("Other", "Greece"),name=" ",drop = FALSE) +
            theme(
                panel.grid.major = element_line(linetype = "blank"), 
                panel.grid.minor = element_line(linetype = "blank"), 
                panel.background = element_rect(fill = NA),
                plot.title = element_text(hjust = 0.5),
                axis.line = element_blank(),
                aspect.ratio = 1/3
            ) +
            scale_y_continuous(breaks=seq(from = -0.25, to = 0.75, by = 0.05)) + 
            guides(fill=FALSE)
        },height = 2000)
    }
    
    Countries = data.frame(cbind(num = seq(nrow(suicide1DF)),country = sort(as.character(unique(suicide1DF$GEOSEX)))))
    
    output$plot2_3dt = DT::renderDataTable({
        datatable(
            Countries,
            rownames=FALSE,
            colnames = FALSE,
            selection = list(mode = "single",selected = 1),
            extensions = c('Scroller'),
            callback = JS(" table.on('click', 'td', function() {
                    var td = $(this);
                    Shiny.onInputChange('plot23selectedcountry', table.row( this ).data()[1]);
                    Shiny.onInputChange('SelectedClickEvent', String(window.performance.now()));
                })"),
            options = list(
                pageLength = 200, 
                paging = TRUE, 
                autoWidth = TRUE,
                ordering=F,
                dom = 't',
                columnDefs =  list(list(visible=FALSE, targets=c(0))),
                scroller = TRUE,
                scrollY = 350
            )
        )
    })
    
    renderPlot2_3 = function() {
        
        if(!is.null(input$plot2_3dt_rows_selected)){
            suicide1DF = suicide1DF[suicide1DF$GEOSEX == input$plot23selectedcountry,]
        }
        else{
            suicide1DF = suicide1DF[suicide1DF$GEOSEX == 'Austria',]
        }
        suicide1DFDT = data.frame(
            cbind(
                rbind(2011,2012,2013,2014,2015),
                rbind(
                    suicide1DF$'2011Prc'*100000,
                    suicide1DF$'2012Prc'*100000,
                    suicide1DF$'2013Prc'*100000,
                    suicide1DF$'2014Prc'*100000,
                    suicide1DF$'2015Prc'*100000
                )
            )
        )
        colnames(suicide1DFDT) = c('years','suicides')
        
        suicide1DFDT$years = as.numeric(suicide1DFDT$years)
        
        
        output$SDPlot3 = renderPlot({
            ggplot(suicide1DFDT,aes(x=years,y=suicides,ymin=0, ymax=35)) +
                geom_line(size=0.8) + 
                labs(y = 'Number of suicides (per 100,000 people)')+ 
                theme(axis.line = element_line(
                    colour = '#000000',
                    size = 0.75,
                    linetype = "solid",
                    color = '#4DA3F9'),
                    text=element_text(size=11,family="Serif"))+
                scale_y_continuous(breaks=seq(from = 1, to = 35, by = 3)) +
                scale_x_continuous(breaks=seq(from = 2011, to = 2015, by = 1)) +
                geom_ribbon(aes(ymin=0, ymax=35), alpha=.6,fill = '#ffffff')
        })
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
        if(input$SD == 'Plot 2'){
            renderPlot2_2()
        }
        if(input$SD == 'Plot 3'){
            renderPlot2_3()
        }
    })
    
    observeEvent(input$SelectedClickEvent,{
        renderPlot2_3()
    })
    
})