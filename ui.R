library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(DT)
library(shinycssloaders)

source('scriptPlots.r')

ui = navbarPage("Suicides in EU", id = "VoltronTabs",
    useShinyjs(),
    tabPanel("Home",
        fluidRow(
            column(4,
                tags$div(img(src = "AUEB_Logo.jpg"),style='vertical-align:middle;')
            ),
            column(4,
                br(),
                div(
                    style = 'text-align:center;font-size:25px;',
                    textOutput('HomePageMsc')
                ),
                div(
                    style = 'text-align:center;font-size:19px;',
                    textOutput('HomePageCourse')
                ),
                div(
                    style = 'text-align:center;font-size:16px;',
                    textOutput('HomePagePT'),
                    br()
                )
            ),
            column(4,
                div(
                    br(),
                    br(),
                    style = 'text-align:center;font-size:14px;',
                    htmlOutput('HomePageAuthor'),
                    br()
                )
            )
        ),
        fluidRow(
            column(12,
                div(
                    style = 'border-top:1px solid #000000;'
                )
            )
        ),
        fluidRow(
            column(3),
            column(6,
                div(
                    style = 'text-align:center;font-size:18px;',
                    br(),
                    br(),
                    br(),
                    textOutput('HomePageDescTitle')
                ),
                div(
                    style = 'text-align:center;font-size:16px;',
                    br(),
                    textOutput('HomePageDescription'),
                    br(),
                    br()
                )
            )
        ),
        fluidRow(
            column(12,
                   div(
                       style = 'border-top:1px solid #000000;'
                   )
            )
        )
        
    ),
    tabPanel("First Dataset",
        fluidRow(
            column(3,
                selectInput("FD",'Choose Plot',choices = c('Plot 1', 'Plot 2', 'Plot 3', 'Plot 4'))
            ),
            column(9,
                conditionalPanel(
                    condition = "input.FD == 'Plot 2'",
                    prettyCheckboxGroup(
                        inputId = 'FDCountrySelector',
                        label = 'Select Countries',
                        choices = c("AUT","BEL","CZE","DNK","FIN","FRA","DEU","GRC","HUN","IRL","ITA","LUX","NLD","POL","PRT","SVK",'Rest Europe Average*',"ESP","SWE","GBR","EST","SVN","LVA","LTU"),
                        selected = c('GRC','Rest Europe Average*'),
                        inline = TRUE,
                        status = "info",
                        animation = 'tada'
                    )
                ),
                conditionalPanel(
                    condition = "input.FD == 'Plot 4'",
                    selectInput(
                        inputId = 'FDCountrySelector2',
                        label = 'Choose Coyntry',
                        choices = c("AUT","BEL","CZE","DNK","FIN","FRA","DEU","GRC","HUN","IRL","ITA","LUX","NLD","POL","PRT","SVK",'Rest Europe Average*',"ESP","SWE","GBR","EST","SVN","LVA","LTU"),
                        selected = 'GRC'
                    )
                )
            )
        ),
        fluidRow(
            column(12,
                br(),
                conditionalPanel(
                    condition = "input.FD == 'Plot 1'",
                    dygraphOutput('FDDygraph1')
                ),
                conditionalPanel(
                    condition = "input.FD == 'Plot 2'",
                    dygraphOutput('FDDygraph2')
                ),
                conditionalPanel(
                    condition = "input.FD == 'Plot 3'",
                    plotOutput('FDPlot3')
                ),
                conditionalPanel(
                    condition = "input.FD == 'Plot 4'",
                    plotOutput('FDPlot4')
                )
            )
        )
        
    ),
    tabPanel("Second Dataset",
        fluidRow(
            column(12,
                selectInput("SD",'Choose Plot',choices = c('Plot 1', 'Plot 2')),
                br(),
                plotOutput('SDPlot')
            )
        ),
        fluidRow(
            column(12,
                br(),
                conditionalPanel(
                    condition = "input.SD == 'Plot 1'",
                    imageOutput("myImage")
                )
            )
        )
                   
    )
)