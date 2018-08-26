library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(lubridate)
library(shinyalert)
library(shinycssloaders)


x = c("Hillary Clinton", "Donald Trump", "Others")
t = list(size = 8, color = 'blue')
colors <-
  c('rgb(211,94,96)', 'rgb(114,147,203)', 'rgb(144,103,167))')
color_map <- c(trump = "blue", clinton = "red")
options(spinner.color.background = "#F5F5F5")
states = c(
  "Alabama",
  "Alaska",
  "Arizona",
  "Arkansas",
  "California",
  "Colorado",
  "Connecticut",
  "Delaware",
  "Florida",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Illinois",
  "Indiana",
  "Iowa",
  "Kansas",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "Minnesota",
  "Mississippi",
  "Missouri",
  "Montana",
  "Nebraska",
  "Nevada",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "New York",
  "North Carolina",
  "North Dakota",
  "Ohio",
  "Oklahoma",
  "Oregon",
  "Pennsylvania",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Tennessee",
  "Texas",
  "Utah",
  "Vermont",
  "Virginia",
  "Washington",
  "West Virginia",
  "Wisconsin",
  "Wyoming"
)

ui = dashboardPage(
  skin = "red",
  
  dashboardHeader(title = "TRUMP vs. CLINTON"),
  
  dashboardSidebar(
    sidebarMenu(
      selectInput("pollgrade", "Select Poll Grade:", ""),
      
      selectInput("pollster", "Select PollSter:", ""),
      
      selectInput("date", "Select Start Date:", ""),
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
        
        
      ),
      
      
      
      useShinyalert(),
      
      
      menuItem(
        "Statistics",
        tabName = "stats",
        icon = icon("stats", lib = "glyphicon")
      ),
      
      br(),
      
      helpText(
        "Use Grade:B,",
        br(),
        " Pollster: Google Consumer Surveys",
        br(),
        " for data on USA maps"
      ),
      
      helpText(
        "   Missing data on USA map for",
        br(),
        " 9/27/2016, 10/13/2016,",
        br(),
        " 9/29/2016, 9/15/2016, 9/1/2016,",
        br(),
        " 8/18/2016, 8/3/2016"
      )
      
      
    )
    
  ),
  
  dashboardBody(uiOutput("boxContentUI2"),
                
                tabItems(
                  tabItem(
                    tabName = "dashboard",
                    fluidRow(
                      valueBoxOutput ('clintonvb'),
                      valueBoxOutput ('trumpvb'),
                      valueBoxOutput ('othersvb')
                    ),
                    
                    fluidRow(
                      box(
                        title = "Selected Data",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 500,
                        color = "red",
                        dataTableOutput("table1") %>% withSpinner()
                      )
                    )
                    
                    
                  ),
                  
                  tabItem(
                    tabName = "stats",
                    fluidRow(
                      box(
                        title = "Comparison by Bar Graph",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        
                        plotlyOutput("comparisonbg") %>% withSpinner()
                      ),
                      box(
                        title = "Comparison by Pie Chart",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        
                        plotlyOutput("comparisonpc") %>% withSpinner()
                      )
                      
                    ) ,
                    fluidRow(
                      box(
                        title = "Line Graph by Pollster Grade",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 100,
                        
                        plotlyOutput("comparisonpg") %>% withSpinner()
                      )
                      
                      
                    ),
                    
                    fluidRow(
                      box(
                        title = "Difference Over the period",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 100,
                        
                        plotlyOutput("diff") %>% withSpinner()
                      )
                      
                      
                    ),
                    
                    fluidRow(
                      box(
                        title = p(
                          "Map Representation of Pre Poll Election Data",
                          actionButton(
                            "titleBtId",
                            "",
                            icon = icon("warning-sign", lib = "glyphicon"),
                            class = "btn-xs",
                            title = "Update"
                          )
                        ),
                        
                        # title = "Map Representation of Pre Poll Election Data",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 500,
                        
                        plotlyOutput("usamap") %>% withSpinner()
                        
                      )
                    ),
                    
                    fluidRow(
                      box(
                        title = "Map Representation of Actual Election Results",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        width = 500,
                        
                        plotlyOutput("map2016") %>% withSpinner()
                      )
                    )
                    
                    
                  )
                  
                ))
)

server <- function(input, output, session) {
  datain <- reactive({
    df <-
      read.csv("www/presidential_polls.csv")
    df %>% distinct
    df <- df[(df$state == "U.S."),]
    return(df)
  })
  
  data_for_map = reactive({
    df <-
      read.csv("www/presidential_polls.csv")
    return(df)
  })
  
  data_for_map1 = reactive({
    df <-
      read.csv("www/result.csv", header = TRUE, sep = ",")
    return(df)
  })
  
  shinyalert(
    title = "Note",
    text = "Map with Election Data on Statistics page will only work with Google Consumer Surveys Pollster",
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#CD5C5C",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  observe({
    df = datain()
    updateSelectInput(session,
                      "pollgrade",
                      choices = unique(df$grade),
                      selected = 'B')
  })
  
  observe({
    df = datain()
    df = df[df$grade == input$pollgrade,]
    updateSelectInput(session, "pollster", choices = unique(df$pollster))
  })
  
  observe({
    df = datain()
    df = df[df$pollster == input$pollster,]
    updateSelectInput(session, "date", choices = unique(df$startdate))
  })
  
  countval = reactive({
    df = datain()
    df = df[df$pollster == input$pollster,]
    df = df[df$startdate == input$date,]
    
    return(df)
  })
  
  output$clintonvb = renderValueBox({
    df = datain()
    df = df[df$pollster == input$pollster,]
    df = df[df$startdate == input$date,]
    clintoncount = round(mean(df$adjpoll_clinton), 2)
    valueBox(paste(clintoncount, " %", sep = ""), "Hillary Clinton", color = 'red')
  })
  
  output$trumpvb = renderValueBox({
    df = datain()
    df = df[df$pollster == input$pollster,]
    df = df[df$startdate == input$date,]
    trumpcount = round(mean(df$adjpoll_trump), 2)
    valueBox(paste(trumpcount, " %", sep = ""), "Donald Trump", color = 'blue')
  })
  
  output$othersvb = renderValueBox({
    df = datain()
    df = df[df$pollster == input$pollster,]
    ans1 = countval()
    otherscount = 100 - mean(ans1$adjpoll_trump) - mean(ans1$adjpoll_clinton)
    ans = paste(round(otherscount[1], 2), "%", sep = "")
    valueBox(ans, "Others", color = 'black')
  })
  
  output$table1 = renderDataTable({
    countval()
  })
  
  output$comparisonbg = renderPlotly({
    ans1 = countval()
    y1 = round(mean(ans1$adjpoll_clinton), 2)
    y2 = round(mean(ans1$adjpoll_trump), 2)
    y3 = 100 - mean(ans1$adjpoll_trump) - mean(ans1$adjpoll_clinton)
    y3 = round(y3, 2)
    y = c(y1, y2, y3)
    
    data <- data.frame(x, y)
    
    plot_ly(data,
            x =  ~ x,
            insidetextfont = list(color = '#FFFFFF')) %>%
      add_trace(
        x = ~ x,
        y = ~ y,
        type = 'bar',
        hoverinfo = paste(x, " :", y, " %"),
        text = y,
        textposition = 'auto',
        marker = list(
          color = c('indianred', 'rgb(114,147,203)', 'rgb(144,103,167)'),
          line = list(
            color = c('indianred', 'rgb(114,147,203)', 'rgb(144,103,167)'),
            width = 1.5
          )
        )
      ) %>%
      layout(
        title = paste(
          "Trump vs. Clinton vs. Others by ",
          input$pollster,
          " on ",
          input$date,
          sep = ""
        ),
        font = t,
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })
  
  output$comparisonpc = renderPlotly({
    df = datain()
    df = df[df$pollster == input$pollster,]
    df = df[df$startdate == input$date,]
    ans1 = countval()
    y1 = round(mean(df$adjpoll_clinton), 2)
    y2 = round(mean(df$adjpoll_trump), 2)
    y3 = 100 - mean(df$adjpoll_trump) - mean(df$adjpoll_clinton)
    y3 = round(y3, 2)
    y = c(y1, y2, y3)
    t = list(size = 8, color = 'blue')
    data <- data.frame(x, y)
    
    plot_ly(
      labels =  ~ x,
      values = ~ y,
      type = 'pie',
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      text = ~ paste(x, y, '%'),
      marker = list(
        colors = colors,
        line = list(color = '#FFFFFF', width = 1)
      ),
      
      showlegend = FALSE
    ) %>%
      layout(
        title = paste(
          "Trump vs. Clinton vs. Others by ",
          input$pollster,
          " on ",
          input$date,
          sep = ""
        ),
        font = t,
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
  })
  
  difference1 = reactive({
    df = datain()
    
    df$difference = df$adjpoll_clinton - df$adjpoll_trump
    return(df)
  })
  
  highestvalue = reactive({
    df = datain()
    df %>%
      group_by(grade) %>%
      filter(row_number(desc(abs(poll_wt))) == 1)
  })
  
  agency_wise_highest_different_date = reactive({
    df = datain()
    df = df[df$pollster == input$pollster,]
    df %>%
      group_by(startdate) %>%
      filter(row_number(desc(abs(poll_wt))) == 1)
  })
  
  top_pollwt_grade_wise = reactive({
    df = datain()
    df = df[df$grade == input$pollgrade,]
    df1 =  df %>%
      group_by(startdate) %>%
      filter(row_number(desc(abs(poll_wt))) == 1)
    df1$enddate <- lubridate::mdy(df1$enddate)
    df1 = dplyr::arrange(df1, enddate)
    return (df1)
  })
  
  output$diff = renderPlotly({
    df = difference1()
    
    df1 = select(df,
                 startdate,
                 samplesize,
                 adjpoll_clinton,
                 adjpoll_trump,
                 difference)
    df1$startdate = lubridate::mdy(df1$startdate)
    var1 = c("samplesize", "difference")
    df1 =  aggregate(df1[var1], by = df1["startdate"], mean)
    
    plot_ly(
      df1,
      x =  ~ startdate,
      y =  ~ difference,
      type = 'scatter',
      mode = 'lines+markers'
    ) %>%
      layout(
        title = "Percentile Difference in Vote Lead (Clinton - Trump) (All Data)",
        xaxis = list(title = "Months"),
        yaxis = list (title = "Difference")
      )
  })
  
  output$comparisonpg = renderPlotly({
    df = top_pollwt_grade_wise()
    x1 = df[["enddate"]]
    y1 = df[["adjpoll_clinton"]]
    y2 = df[["adjpoll_trump"]]
    y3 = 100 - y1 - y2
    plot_ly(
      x = ~ x1,
      y = ~ y1,
      name = 'Trump',
      type = 'scatter',
      mode = 'lines+markers',
      group =  ~ x1,
      line = list(color = 'rgb(114,147,203)')
    ) %>%
      add_trace(
        y = ~ y2,
        name = 'Clinton',
        line = list(color = 'indianred')
      ) %>%
      add_trace(
        y = ~ y3,
        name = 'Others',
        line = list(color = 'rgb(144,103,167)')
      ) %>%
      
      layout(
        title = paste(
          "Polls run by ",
          input$pollgrade,
          " pollsters over a period of time",
          sep = ""
        ),
        xaxis = list(title = "Months"),
        yaxis = list (title = "Percentage")
      )
    
  })
  
  output$map2016 = renderPlotly({
    df <- data_for_map1()
    
    df$win = factor(df$win, levels = c("trump", "clinton"))
    cols = c("blue", "red")
    
    df$hover <-
      with(
        df,
        paste(
          df$state,
          '<br>',
          "Trump",
          trump_win,
          "<br>",
          "Clinton",
          clinton_win,
          "<br>",
          "Others",
          others
        )
      )
    
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      # showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    plot_geo(df, locationmode = 'USA-states') %>%
      add_trace(
        text = ~ hover,
        locations = ~ abbr,
        color = ~ win,
        colors = c("red", "blue")
      ) %>%
      
      colorbar(title = "Percentage") %>%
      layout(title = 'Actual Election Results',
             geo = g)
    
  })
  
  output$usamap = renderPlotly({
    df = data_for_map()
    df = df[df$pollster == 'Google Consumer Surveys',]
    df = df[df$startdate == input$date,]
    
    df = subset(df, state != "U.S.")
    df$abbr = state.abb[match(df$state, state.name)]
    df$abbr[is.na(df$abbr)] = "DC"
    df %>% group_by(state, startdate, abbr) %>% summarise(poll_wt = max(poll_wt))
    
    df$hover <-
      with(
        df,
        paste(
          state,
          '<br>',
          "Sample Size:",
          samplesize,
          "<br>",
          "Trump",
          adjpoll_trump,
          "<br>",
          "Clinton",
          adjpoll_clinton,
          "<br>",
          "Others",
          (100 - adjpoll_trump - adjpoll_clinton)
        )
      )
    
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      # showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    plot_geo(df, locationmode = 'USA-states') %>%
      add_trace(
        z = ~ samplesize,
        text = ~ hover,
        locations = ~ abbr,
        color = ~ samplesize,
        colors = 'Blues'
      ) %>%
      colorbar(title = "Sample Size") %>%
      layout(title = 'Google Survey State Wise Results',
             geo = g)
  })
  
  output$boxContentUI2 <- renderUI({
    input$titleBtId
    shinyalert(
      title = "Note",
      text = "Map with Election Data on Statistics page will only work with Google Consumer Surveys Pollster",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#CD5C5C",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
}

shinyApp(ui, server)