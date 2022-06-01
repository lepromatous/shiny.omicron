
library(shiny)
library(plotly)
library(shinydashboard)
library(DT)
source("helpers.R")
source("helpers_heatmap.R")

# begin ui ----
ui<-dashboardPage( skin = "black",
                   
# header ----
dashboardHeader(title = "SARS-CoV-2 Variant and Lineage Investigator", titleWidth = '100%'),

# sidebar with selectors and data sources ----
dashboardSidebar(width = 375,
                sidebarMenu(
                  h4("Select a tab for analysis"),
                            br(),
                  
                  # MENU ITEMS ----
                      menuItem("Omicron Lineage Proportions", tabName = "omprop"),
                      menuItem("Heatmap by Country", tabName = "heat"),
                            br(),
                            hr(),
                                      
# data sources ----         
h4("Data Sources:"),
        tags$ol(
          tags$li(
            tags$a(href="https://www.gisaid.org",
                   "GISAID")),
          tags$li(
            tags$a(href="https://covariants.org",
                   "Covariants.org")),
          tags$li(
            tags$a(href="https://epi.quartzsoftware.com",
                   "Infectious Diseases Data Repository"))
        ), # close ol
        
        hr(),

# general notes ----        
h4("Notes:"),
        tags$ul(
          tags$li("Data updated each evening at 10:00pm CST"),
          tags$li("Computations may not be exact due to rounding"), 
          tags$li("App last updated 31 May 2022"), 
          
        ),

        hr(),

# creator ----        
h4("Created by:"), 
        tags$ul(
          tags$li("Timothy Wiemken, PhD"),
          tags$li("Christopher Prener, PhD"),
              )
        
)), # close sidebar menu and dash sidebar


# dash body with css ----
dashboardBody(
 tags$head(tags$style(HTML('
                      .main-header .logo {
                      font-family: "Georgia", Times, "Times New Roman", serif;
                      font-weight: bold;
                      font-size: 20px;
                      }'
 ))), ### close tags
 
 # tags for auto resize plotly object ----
 tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
                     
# TAB 1 sidebar selector for omicron proportion ----
tabItems(
 tabItem(tabName = "omprop",

            selectInput(inputId = "countriez",
                      "Select Country",
                      choices = sort(country_list),
                      selected= "USA"),
# omicron proportion plot ----
uiOutput("omicron_prop_output"),
 ), # close tab item 1, omicron bar chart

# TAB 2 sidebar selector for omicron proportion ----
  tabItem(tabName = "heat",
          
          selectInput(inputId = "lineage",
                      "Select Pango Lineage",
                      choices = sort(unique(lookup$pango)),
                      selected= "BA.1"),
  # omicron proportion plot ----
  uiOutput("omicron_heat_output")
  ) # close tab item2 heatmap

) # close tab items chunk


) # close dashboard body

) # close UI





# server ----
server <- function(input, output, session) {
  
# plot of omicron proportion by country ----
observeEvent(input$dimension,{
  
  output$omicron_prop_output <- renderUI({
    renderPlotly({
      plotly::ggplotly(variant.plot(input$countriez), width = (0.75*as.numeric(input$dimension[1])), height = (0.8*as.numeric(input$dimension[2])))
    })
  })
}) # close omicron prop output
  

  # plot of omicron heatmap by country ----
  observeEvent(input$dimension,{
    
  output$omicron_heat_output <- renderUI({
      renderPlotly({
        plotly::ggplotly(variant.heat(input$lineage), width = (0.75*as.numeric(input$dimension[1])), height = (0.8*as.numeric(input$dimension[2])))
      })
  })
  }) # close omicron heat output
  
  
  
} # close server

# Run the application 
shinyApp(ui = ui, server = server)
