header <- dashboardHeader(title = "Covid-19 Switzerland")

sidebar <- dashboardSidebar( 
  sidebarMenu (
    menuItem("Timeline", tabName = "Timeline", icon = icon("calendar-alt")), 
    menuItem("Maps and Stats", tabName = "Maps", icon = icon("chart-bar")),
    menuItem("14 days trend", tabName = "Trend", icon = icon("chart-line")),
    menuItem("About", tabName = "About", icon = icon("comment-alt")),
    menuItem("Source code", icon = icon("code"), 
             href = " "),
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
              box(title ="About", width = 12,
                  "This dashboard has been built using the data found in the GitHub repository https://github.com/openZH/covid_19, which collect Covid-19 data for Switzerland and Lichtenstain.",
                  "The data is updated at best once a day at varying times, but in order to avoid missing values and errors, the data in Maps and stats are displayed with a 2 days delay, as indicated when hovering on the data.",
                  "The data analysis as well as the source code of the app can be found at: .
                  Both source code and data can be directly accessed from the sidebar.")
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
    pal <- colorBin("PuOr", domain = trend_swiss_geo$change_percemt, reverse = TRUE)
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
                  label = ~paste0(name_en, ": ", 
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
      filter(date == max(date)-2)
    labels2 <- sprintf(
      "%s<br/><strong>%s</strong><br/> %s total Covid-19 cases",
      data_swiss_geo_last$date, data_swiss_geo_last$Canton, data_swiss_geo_last$ncumul_conf
    ) %>% lapply(htmltools::HTML)
    data_swiss_geo_last %>%
      leaflet(options = leafletOptions(minZoom = 7.2)) %>%
      setView(lat = 46.9, lng = 8.3, zoom = 7) %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(weight = 1, col = "grey") %>%
      addCircleMarkers(~longitude, ~latitude,
                       radius = ~data_swiss_geo_last$ncumul_conf/2200,
                       stroke = TRUE,
                       color = "orange",
                       weight = 2,
                       fillOpacity = 0.5,
                       label = labels2)
  })
  output$map_cases <- renderLeaflet({
    data_swiss_geo_last <- data_swiss_geo %>%
      filter(date == max(date)-2)
    labels1 <- sprintf(
      "%s<br/><strong>%s</strong><br/> %s Covid-19 cases every 10000 people",
      data_swiss_geo_last$date, data_swiss_geo_last$Canton, round(data_swiss_geo_last$ncumul_conf_per_10thous,0)
    ) %>% lapply(htmltools::HTML)
    data_swiss_geo_last %>%
      leaflet(options = leafletOptions(minZoom = 7.2)) %>%
      setView(lat = 46.9, lng = 8.3, zoom = 7) %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(weight = 1, col = "grey") %>%
      addCircleMarkers(~longitude, ~latitude,
                       radius = ~(data_swiss_geo_last$ncumul_conf_per_10thous/50),
                       stroke = TRUE,
                       color = "orange",
                       weight = 2,
                       fillOpacity = 0.5,
                       label = labels1)
  })
  output$map_deaths_abs <- renderLeaflet({
    data_swiss_geo_last <- data_swiss_geo %>%
      filter(date == max(date)-2)
    labels4 <- sprintf(
      "%s<br/><strong>%s</strong><br/> %s total deaths",
      data_swiss_geo_last$date, data_swiss_geo_last$Canton, data_swiss_geo_last$ncumul_deceased
    ) %>% lapply(htmltools::HTML)
    data_swiss_geo_last %>%
      leaflet(options = leafletOptions(minZoom = 7.2)) %>%
      setView(lat = 46.9, lng = 8.3, zoom = 7) %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(weight = 1, col = "grey") %>%
      addCircleMarkers(~longitude, ~latitude,
                       radius = ~data_swiss_geo_last$ncumul_deceased/40,
                       stroke = TRUE,
                       color = "mediumorchid",
                       weight = 2,
                       fillOpacity = 0.4,
                       label = labels4)
  })
  output$map_deaths <- renderLeaflet({
    data_swiss_geo_last <- data_swiss_geo %>%
      filter(date == max(date)-2)
    labels3 <- sprintf(
      "%s<br/><strong>%s</strong><br/> %s deaths every 10000 people",
      data_swiss_geo_last$date, data_swiss_geo_last$Canton, round(data_swiss_geo_last$ncumul_deceased_per_10thous,0)
    ) %>% lapply(htmltools::HTML)
    data_swiss_geo_last %>%
      leaflet(options = leafletOptions(minZoom = 7.2)) %>%
      setView(lat = 46.9, lng = 8.3, zoom = 7) %>%
      addProviderTiles("CartoDB") %>%
      addPolygons(weight = 1, col = "grey") %>%
      addCircleMarkers(~longitude, ~latitude,
                       radius = ~data_swiss_geo_last$ncumul_deceased_per_10thous,
                       stroke = TRUE,
                       color = "mediumorchid",
                       weight = 2,
                       fillOpacity = 0.4,
                       label = labels3)
  })
  output$cases_abs <- renderPlotly({
    data_swiss %>%
      plot_ly() %>%
      filter(date == max(date)-2) %>%
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
      filter(date == max(date)-2) %>%
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
      filter(date == max(date)-2) %>%
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
      filter(date == max(date)-2) %>%
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