# Libraries
library(readr)
library(readxl)
library(dplyr)
library(plotly)
library(forcats)
library(forecast)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(sf)
library(maps)
library(gifski)
library(leaflet)
library(ggmap)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)
library(purrr)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(rsconnect)
library(zoo)

# Data sets

# Load Covid data for Switzerland from GitHub repository
data_swiss <- read_csv("https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total_v2.csv")
# Load Canton population data from excell csv file made from Wikipedia data
canton_swiss <- read_xlsx("swiss_cantons.xlsx")
# Join data frames
data_swiss = left_join(data_swiss, canton_swiss, by = c(abbreviation_canton_and_fl = "Canton_abbr"))
# Modify dataframe by adding more variables
data_swiss <- data_swiss %>% group_by(abbreviation_canton_and_fl) %>%
    mutate(new_cases = ncumul_conf - lag(ncumul_conf, default = first(ncumul_conf), order_by = date))
data_swiss <- data_swiss %>% group_by(abbreviation_canton_and_fl) %>%
    mutate(new_deaths = ncumul_deceased - lag(ncumul_deceased, default = first(ncumul_deceased), order_by = date))
data_swiss <- data_swiss %>%
    mutate(pop_10thous = Pop/10000)
data_swiss <- data_swiss %>%
    mutate(new_cases_per_10thous = new_cases/pop_10thous)
data_swiss <- data_swiss %>%
    mutate(new_deaths_per_10thous = new_deaths/pop_10thous)
data_swiss <- data_swiss %>%
    mutate(new_cases_smoothed = zoo::rollmean(new_cases, k = 7, fill = NA))
data_swiss <- data_swiss %>%
    mutate(new_deaths_smoothed = zoo::rollmean(new_deaths, k = 7, fill = NA))
data_swiss <- data_swiss%>%
    mutate(ncumul_deceased_per_10thous = ncumul_deceased/pop_10thous)
data_swiss <- data_swiss%>%
    mutate(ncumul_conf_per_10thous = ncumul_conf/pop_10thous)

#Download spatial data frame
swiss_geo <- st_read("shp/g1k20.shp")
swiss_geo <- st_as_sf(swiss_geo)
swiss_geo <- st_transform(swiss_geo, 4326)
swiss_geo$KTNAME <- as.character(swiss_geo$KTNAME)
swiss_geo$KTNAME <- replace(swiss_geo$KTNAME, swiss_geo$KTNAME == "Zürich", "Zurich")
swiss_geo$KTNAME <- replace(swiss_geo$KTNAME, swiss_geo$KTNAME == "Bern / Berne", "Bern")
swiss_geo$KTNAME <- replace(swiss_geo$KTNAME, swiss_geo$KTNAME == "Luzern", "Lucerne")
swiss_geo$KTNAME <- replace(swiss_geo$KTNAME, swiss_geo$KTNAME == "Fribourg / Freiburg", "Fribourg")
swiss_geo$KTNAME <- replace(swiss_geo$KTNAME, swiss_geo$KTNAME == "Appenzell Ausserrhoden", "Appenzell A. Rh.")
swiss_geo$KTNAME <- replace(swiss_geo$KTNAME, swiss_geo$KTNAME == "Appenzell Innerrhoden", "Appenzell I. Rh.")
swiss_geo$KTNAME <- replace(swiss_geo$KTNAME, swiss_geo$KTNAME == "Graubünden / Grigioni / Grischun", "Graubünden")
swiss_geo$KTNAME <- replace(swiss_geo$KTNAME, swiss_geo$KTNAME == "Valais / Wallis", "Valais")
swiss_geo$KTNAME <- replace(swiss_geo$KTNAME, swiss_geo$KTNAME == "Genève", "Geneva")

# Merge with geo data
data_swiss_geo <- left_join(swiss_geo, data_swiss, by = c("KTNAME" = "Canton"))

# Create new data frame with Switzerland totals
data_swiss_noNA <- data_swiss %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
switzerland_new_cases <- data_swiss_noNA %>%
    group_by(date) %>%
    summarize(switzerland_new_cases = sum(new_cases, na.rm = TRUE))
switzerland_new_cases_smoothed <- data_swiss_noNA %>%
    group_by(date) %>%
    summarize(switzerland_new_cases_smoothed = sum(new_cases_smoothed, na.rm = TRUE)) %>%
    select(-date)
switzerland_new_deaths <- data_swiss_noNA %>%
    group_by(date) %>%
    summarize(switzerland_new_deaths = sum(new_deaths, na.rm = TRUE))%>%
    select(-date)
switzerland_new_deaths_smoothed <- data_swiss_noNA %>%
    group_by(date) %>%
    summarize(switzerland_new_deaths_smoothed = sum(new_deaths_smoothed, na.rm = TRUE)) %>%
    select(-date)
data_total_swiss <- cbind(switzerland_new_cases, switzerland_new_cases_smoothed, switzerland_new_deaths, switzerland_new_deaths_smoothed)

# Calculate trend
tot14days_last <- data_swiss %>%
    group_by(abbreviation_canton_and_fl) %>%
    filter(date <= max(date), date >= max(date)-14) %>%
    summarize(tot14days_last = sum(new_cases, na.rm = TRUE))
tot14days_previous <- data_swiss %>%
    group_by(abbreviation_canton_and_fl) %>%
    filter(date <= max(date)-15, date >= max(date)-29) %>%
    summarize(tot14days_previous = sum(new_cases, na.rm = TRUE)) %>%
    select(-abbreviation_canton_and_fl)
trend <- cbind(tot14days_last, tot14days_previous)
trend <- trend %>%
    mutate(change_percemt = round((tot14days_last-tot14days_previous)/tot14days_last*100, 0))
trend <- left_join(canton_swiss, trend, by = c(Canton_abbr = "abbreviation_canton_and_fl"))
trend_swiss_geo <- left_join(swiss_geo, trend, by = c("KTNAME" = "Canton"))

# App
header <- dashboardHeader(title = "Covid-19 Switzerland")

sidebar <- dashboardSidebar( 
    sidebarMenu (
        menuItem("Timeline", tabName = "Timeline", icon = icon("calendar-alt")), 
        menuItem("Maps and Stats", tabName = "Maps", icon = icon("chart-bar")),
        menuItem("14 days trend", tabName = "Trend", icon = icon("chart-line")),
        menuItem("About", tabName = "About", icon = icon("comment-alt")),
        menuItem("Source code", icon = icon("code"), 
                 href = "https://github.com/vivvi87/Swiss-Covid-Shiny-Dashboard"),
        menuItem("Source data", icon = icon("database"),
                 href = "https://github.com/openZH/covid_19")
    )
)

body <- dashboardBody( 
    tabItems(
        tabItem(tabName = "Timeline",
                fluidRow(
                    valueBoxOutput("box_cases"),
                    valueBoxOutput("box_deaths"),
                    valueBoxOutput("box_canton")
                ),
                fluidRow(
                    tabBox(width = 10, title ="Switzerland Covid-19 timeline",
                           tabPanel("Cases", plotlyOutput("swiss_timeline")),
                           tabPanel("Deaths", plotlyOutput("swiss_timeline_d"))
                    ),
                    box(width = 2,
                        sliderInput("dates", "Select dates:",
                                    min(data_total_swiss$date), max(data_total_swiss$date),
                                    value = c(as.Date("2020-09-20"), max(data_total_swiss$date))
                        )
                    )
                ),
                fluidRow(
                    tabBox(width = 10, title ="Swiss cantons Covid-19 timeline",
                           tabPanel("Cases", plotlyOutput("canton_timeline")),
                           tabPanel("Deaths", plotlyOutput("canton_timeline_d"))
                    ),
                    box(width = 2,
                        sliderInput("dates_canton", "Select dates:",
                                    min(data_swiss$date), max(data_swiss$date),
                                    value = c(as.Date("2020-09-20"), max(data_swiss$date))
                        ),
                        selectInput("canton", "Select canton:",
                                    selected = "Geneva",
                                    choices = c(levels(as.factor(data_swiss$Canton))),
                                    multiple = FALSE
                        )
                    )
                )
        ),
        tabItem(tabName = "Maps",
                fluidRow(
                    tabBox(title = "Total cases",
                           tabPanel("Absolute", leafletOutput("map_cases_abs")),
                           tabPanel("Every 10000 people", leafletOutput("map_cases"))
                    ),
                    tabBox(title = "Total deaths",
                           tabPanel("Absolute", leafletOutput("map_deaths_abs")),
                           tabPanel("Every 10000 people", leafletOutput("map_deaths"))
                    )
                ),
                fluidRow(
                    tabBox(title = "Total cases",
                           tabPanel("Absolute", plotlyOutput("cases_abs")),
                           tabPanel("Every 10000 people", plotlyOutput("cases"))
                    ),
                    tabBox(title = "Total deaths",
                           tabPanel("Absolute", plotlyOutput("deaths_abs")),
                           tabPanel("Every 10000 people", plotlyOutput("deaths"))
                    )
                )
        ),
        tabItem(tabName = "About",
                fluidRow(
                    box(width = 12,
                        h2("About"),
                        "This dashboard has been built using the data found in the GitHub repository ", em("https://github.com/openZH/covid_19"), ", which collect Covid-19 data for Switzerland and Lichtenstain.",
                        "The data is updated roughly once a week, and in order to minimize bugs and errors caused by missing values in the source data, the data in Maps and stats are displayed with a 3 days delay, as indicated when hovering on the graph.",
                        "The data analysis as well as the source code of the dashboard can be found at ", em("https://github.com/vivvi87/Swiss-Covid-Shiny-Dashboard"), ". Both source code and data can be directly accessed from the sidebar."
                    )
                )
        ),
        tabItem(tabName = "Trend",
                fluidRow(
                    valueBoxOutput("swiss_trend")
                ),
                fluidRow(
                    box(title = "Map - 14 days variation %", width = 6,
                        leafletOutput("variation_map")
                    ),
                    box(title = "Chart - 14 days variation %", width = 6,
                        plotlyOutput("variation_chart")
                    )
                ),
                fluidRow(
                    DT::dataTableOutput("trend_table")
                )
        )
    )
)

server <- function(input, output) {
    output$swiss_timeline <- renderPlotly({
        data_total_swiss %>%
            filter(date >= input$dates[1] & date <= input$dates[2]) %>%
            plot_ly() %>%
            add_bars(x = ~date, 
                     y = ~switzerland_new_cases,
                     color = I("black"),
                     opacity = 0.5,
                     text = ~paste(date, "<br>", "New cases: ", round(switzerland_new_cases, 1)),
                     hoverinfo = "text",
                     name = "New cases") %>%
            add_lines(x = ~date, 
                      y = ~switzerland_new_cases_smoothed,
                      color = I("orange"),
                      text = ~paste(date, "<br>", "New cases (7-days average): ", round(switzerland_new_cases_smoothed, 0)),
                      hoverinfo = "text",
                      name = "new cases (7-days average)") %>%
            layout(yaxis = list(title = "Number of Covid-19 cases",
                                showgrid = F,
                                range = c(0, 11500)),
                   xaxis = list(title = " "),
                   legend = list(x = 0, y = 1)) %>%
            config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    output$swiss_timeline_d <- renderPlotly({
        data_total_swiss %>%
            filter(date >= input$dates[1] & date <= input$dates[2]) %>%
            plot_ly() %>%
            add_bars(x = ~date, 
                     y = ~switzerland_new_deaths,
                     color = I("black"),
                     opacity = 0.5,
                     text = ~paste(date, "<br>", "New deaths: ", round(switzerland_new_deaths, 1)),
                     hoverinfo = "text",
                     name = "New deaths") %>%
            add_lines(x = ~date, 
                      y = ~switzerland_new_deaths_smoothed,
                      color = I("orange"),
                      text = ~paste(date, "<br>", "New deaths (7-days average): ", round(switzerland_new_deaths_smoothed, 0)),
                      hoverinfo = "text",
                      name = "new deaths (7-days average)") %>%
            layout(yaxis = list(title = "Number of deaths",
                                showgrid = F,
                                range = c(0, 125)),
                   xaxis = list(title = " "),
                   legend = list(x = 0, y = 1)) %>%
            config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    output$canton_timeline <- renderPlotly({
        data_swiss %>%
            filter(date >= input$dates_canton[1] & date <= input$dates_canton[2]) %>%
            filter(Canton == input$canton) %>%
            plot_ly() %>%
            mutate(Canton = as.character(Canton)) %>%
            add_lines(x = ~date, 
                      y = ~new_cases, 
                      fill = "tozeroy", 
                      fillcolor= 'rgba(153,102,204,0.5)',
                      line = list(color = 'rgba(153,102,204,0.6)'),
                      text = ~paste(Canton, "<br>", "Date: ", date, "<br>", "New Cases: ", new_cases),
                      hoverinfo = "text") %>%
            layout(yaxis = list(title = "Number of Covid-19 Cases",
                                showgrid = F),
                   xaxis = list(title = " ", showgrid = F)) %>%
            config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    output$canton_timeline_d <- renderPlotly({
        data_swiss %>%
            filter(date >= input$dates_canton[1] & date <= input$dates_canton[2]) %>%
            filter(Canton == input$canton) %>%
            plot_ly() %>%
            mutate(Canton = as.character(Canton)) %>%
            add_lines(x = ~date, 
                      y = ~new_deaths, 
                      fill = "tozeroy", 
                      fillcolor= 'rgba(153,102,204,0.5)',
                      line = list(color = 'rgba(153,102,204,0.6)'),
                      text = ~paste(Canton, "<br>", "Date: ", date, "<br>", "New deaths: ", new_deaths),
                      hoverinfo = "text") %>%
            layout(yaxis = list(title = "Number of deaths",
                                showgrid = F),
                   xaxis = list(title = " ", showgrid = F)) %>%
            config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    output$variation_map <- renderLeaflet({
        rc1 <- colorRampPalette(colors = c("purple", "white"), space = "Lab")(length(which(trend_swiss_geo$change_percemt < 0)))
        rc2 <- colorRampPalette(colors = c("moccasin", "orange"), space = "Lab")(length(which(trend_swiss_geo$change_percemt > 0)))
        rampcols <- c(rc1, rc2)
        pal <- colorNumeric(palette = rampcols, domain = trend_swiss_geo$change_percemt)
        trend_swiss_geo %>%
            leaflet(options = leafletOptions(minZoom = 7.2)) %>%
            setView(lat = 46.9, lng = 8.3, zoom = 7.2) %>%
            addProviderTiles("CartoDB") %>%
            addPolygons(fillColor = ~pal(change_percemt),
                        weight = 1,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(weight = 2,
                                                     color = "#666",
                                                     dashArray = "",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        label = ~paste0(KTNAME, ": ", 
                                        round(change_percemt, 0), " %")) %>%
            addLegend(pal = pal, 
                      values = ~change_percemt, 
                      opacity = 0.7, 
                      title = NULL, 
                      position = "bottomright")
    })
    output$variation_chart <- renderPlotly({
        trend %>%
            plot_ly() %>%
            add_bars(x = ~Canton,
                     y= ~change_percemt,
                     color = ~change_percemt < 0, colors = c("darkorange3", "mediumpurple3"),
                     opacity = 0.6,
                     text = ~paste(Canton, "<br>", round(change_percemt, 0), "% variation"),
                     hoverinfo = "text") %>%
            layout(yaxis = list(title = "14 days variation %",
                                showgrid = F),
                   xaxis = list(title = " ",
                                showgrid = F)) %>%
            hide_legend() %>%
            config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    output$map_cases_abs <- renderLeaflet({
        data_swiss_geo_last <- data_swiss_geo %>%
            filter(date == max(date)-3)
        pal2 <- colorBin("BuPu", domain = data_swiss_geo$ncumul_conf)
        labels2 <- sprintf(
            "%s<br/><strong>%s</strong><br/> %s total Covid-19 cases",
            data_swiss_geo_last$date, data_swiss_geo_last$KTNAME, data_swiss_geo_last$ncumul_conf
        ) %>% lapply(htmltools::HTML)
        data_swiss_geo_last %>%
            leaflet(options = leafletOptions(minZoom = 7.2)) %>%
            setView(lat = 46.7, lng = 8.4, zoom = 7.2) %>%
            addProviderTiles("CartoDB") %>%
            addPolygons(fillColor = ~pal2(ncumul_conf),
                        weight = 1,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(weight = 2,
                                                     color = "#666",
                                                     dashArray = "",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        label = labels2) %>%
            addLegend(pal = pal2, 
                      values = ~ncumul_conf, 
                      opacity = 0.7, 
                      title = NULL, 
                      position = "bottomright")
    })
    output$map_cases <- renderLeaflet({
        data_swiss_geo_last <- data_swiss_geo %>%
            filter(date == max(date)-3)
        pal3 <- colorBin("BuPu", domain = data_swiss_geo$ncumul_conf_per_10thous)
        labels3 <- sprintf(
            "%s<br/><strong>%s</strong><br/> %s total Covid-19 cases",
            data_swiss_geo_last$date, data_swiss_geo_last$KTNAME, data_swiss_geo_last$ncumul_conf_per_10thous
        ) %>% lapply(htmltools::HTML)
        data_swiss_geo_last %>%
            leaflet(options = leafletOptions(minZoom = 7.2)) %>%
            setView(lat = 46.7, lng = 8.4, zoom = 7.2) %>%
            addProviderTiles("CartoDB") %>%
            addPolygons(fillColor = ~pal3(ncumul_conf_per_10thous),
                        weight = 1,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(weight = 2,
                                                     color = "#666",
                                                     dashArray = "",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        label = labels3) %>%
            addLegend(pal = pal3, 
                      values = ~ncumul_conf_per_10thous, 
                      opacity = 0.7, 
                      title = NULL, 
                      position = "bottomright")
    })
    output$map_deaths_abs <- renderLeaflet({
        data_swiss_geo_last <- data_swiss_geo %>%
            filter(date == max(date)-3)
        pal4 <- colorBin("YlOrBr", domain = data_swiss_geo$ncumul_deceased)
        labels4 <- sprintf(
            "%s<br/><strong>%s</strong><br/> %s total Covid-19 cases",
            data_swiss_geo_last$date, data_swiss_geo_last$KTNAME, data_swiss_geo_last$ncumul_deceased
        ) %>% lapply(htmltools::HTML)
        data_swiss_geo_last %>%
            leaflet(options = leafletOptions(minZoom = 7.2)) %>%
            setView(lat = 46.7, lng = 8.4, zoom = 7.2) %>%
            addProviderTiles("CartoDB") %>%
            addPolygons(fillColor = ~pal4(ncumul_deceased),
                        weight = 1,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(weight = 2,
                                                     color = "#666",
                                                     dashArray = "",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        label = labels4) %>%
            addLegend(pal = pal4, 
                      values = ~ncumul_deceased, 
                      opacity = 0.7, 
                      title = NULL, 
                      position = "bottomright")
    })
    output$map_deaths <- renderLeaflet({
        data_swiss_geo_last <- data_swiss_geo %>%
            filter(date == max(date)-3)
        pal5 <- colorBin("YlOrBr", domain = data_swiss_geo$ncumul_deceased_per_10thous)
        labels5 <- sprintf(
            "%s<br/><strong>%s</strong><br/> %s total Covid-19 cases",
            data_swiss_geo_last$date, data_swiss_geo_last$KTNAME, data_swiss_geo_last$ncumul_deceased_per_10thous
        ) %>% lapply(htmltools::HTML)
        data_swiss_geo_last %>%
            leaflet(options = leafletOptions(minZoom = 7.2)) %>%
            setView(lat = 46.7, lng = 8.4, zoom = 7.2) %>%
            addProviderTiles("CartoDB") %>%
            addPolygons(fillColor = ~pal5(ncumul_deceased_per_10thous),
                        weight = 1,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(weight = 2,
                                                     color = "#666",
                                                     dashArray = "",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        label = labels5) %>%
            addLegend(pal = pal5, 
                      values = ~ncumul_deceased_per_10thous, 
                      opacity = 0.7, 
                      title = NULL, 
                      position = "bottomright")
    })
    output$cases_abs <- renderPlotly({
        data_swiss %>%
            plot_ly() %>%
            filter(date == max(date)-3) %>%
            add_bars(x = ~reorder(Canton, -ncumul_conf),
                     y = ~ncumul_conf,
                     color = I("black"),
                     opacity = 0.5,
                     hoverinfo = "text",
                     text = ~paste(date, "<br>", Canton, "<br>", ncumul_conf, "total cases")) %>%
            layout(yaxis = list(title = "Total Covid-19 cases", showgrid = F),
                   xaxis = list(title = " ")) %>%
            config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    output$cases <- renderPlotly({
        data_swiss %>%
            plot_ly() %>%
            filter(date == max(date)-3) %>%
            add_bars(x = ~reorder(Canton, -ncumul_conf_per_10thous),
                     y = ~ncumul_conf_per_10thous,
                     color = I("black"),
                     opacity = 0.5,
                     hoverinfo = "text",
                     text = ~paste(date, "<br>", Canton, "<br>", round(ncumul_conf_per_10thous, 0), "total cases every 10000 people")) %>%
            layout(yaxis = list(title = "Total Covid-19 cases", showgrid = F),
                   xaxis = list(title = " ")) %>%
            config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    output$deaths_abs <- renderPlotly({
        data_swiss %>%
            plot_ly() %>%
            filter(date == max(date)-3) %>%
            add_bars(x = ~reorder(Canton, -ncumul_deceased),
                     y = ~ncumul_deceased,
                     color = I("black"),
                     opacity = 0.5,
                     hoverinfo = "text",
                     text = ~paste(date, "<br>", Canton, "<br>", ncumul_deceased, "total deaths")) %>%
            layout(yaxis = list(title = "Total deaths", showgrid = F),
                   xaxis = list(title = " ")) %>%
            config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    output$deaths <- renderPlotly({
        data_swiss %>%
            plot_ly() %>%
            filter(date == max(date)-3) %>%
            add_bars(x = ~reorder(Canton, -ncumul_deceased_per_10thous),
                     y = ~ncumul_deceased_per_10thous,
                     hoverinfo = "text",
                     color = I("black"),
                     opacity = 0.5,
                     text = ~paste(date, "<br>", Canton, "<br>", round(ncumul_deceased_per_10thous, 0), "total deaths every 10000 people")) %>%
            layout(yaxis = list(title = "Total deaths", showgrid = F),
                   xaxis = list(title = " ")) %>%
            config(displayModeBar = FALSE, displaylogo = FALSE)
    })
    output$box_cases <- renderValueBox({
        box_cases_val <- summarize(data_total_swiss, sum(switzerland_new_cases))
        valueBox(box_cases_val, "Total cases in Switzerland", color = "yellow", icon = icon("virus"))
    })
    output$box_deaths <- renderValueBox({
        box_deaths_val <- summarize(data_total_swiss, sum(switzerland_new_deaths))
        valueBox(box_deaths_val, "Total deaths in Switzerland", color = "yellow", icon = icon("skull"))
    })
    output$box_canton <- renderValueBox({
        a <- data_swiss %>% filter(date == max(date))
        box_canton_val <- a$Canton[which.max(a$ncumul_conf)]
        valueBox(box_canton_val, "Canton with highest number of cases", color = "yellow", icon = icon("arrow-up"))
    })
    output$swiss_trend <- renderValueBox({
        tot14days_last_swiss <- summarize(trend, sum(tot14days_last))
        tot14days_previous_swiss <- summarize(trend, sum(tot14days_previous))
        swiss_trend_val <- (tot14days_last_swiss-tot14days_previous_swiss)/tot14days_last_swiss*100
        valueBox(paste0(round(swiss_trend_val, 0), "%"), "14 days variation of cases in Switzerland", color = "yellow", icon = if(swiss_trend_val >= 0){icon("arrow-alt-circle-up")} else {icon("arrow-alt-circle-down")})
    })
    output$trend_table <- DT::renderDataTable({
        trend_table <- trend %>%
            select(-Pop, -Canton_abbr)
        DT::datatable(trend_table, 
                      rownames = FALSE,
                      class = "hover",
                      colnames = c("Canton", "total cases last 14 days", "total cases previous 14 days", "variation %"))
    })
} 


ui <- dashboardPage(skin = "purple", header, sidebar, body)
shiny::shinyApp(ui, server)