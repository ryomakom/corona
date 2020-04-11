#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(lubridate)
library(zoo)
library(shiny)
library(scales)

options(encoding = 'UTF-8')

japan <- read_csv("https://dl.dropboxusercontent.com/s/6mztoeb6xf78g5w/COVID-19.csv",
                  locale = locale(encoding = "utf8")) %>%
    mutate(date=mdy(確定日)) %>% rename(place=居住都道府県)

japan_daily <-japan %>% group_by(date, place) %>%
    summarize(n = n()) %>%
    group_by(place) %>%
    mutate(cum = cumsum(n),
           daily_increase_rate = n/cum,
           moving_average_increase_rate = rollmean(daily_increase_rate, 7, fill = NA)) %>%
    ungroup() %>%
    filter(date >= "2020-03-20")

pref <- japan %>% pull(place) %>%
    unique() %>% sort()

ui <- fluidPage(
    titlePanel("新型コロナ感染者の人数と増加率の推移"),
            selectInput(inputId = "pref",
                        label = "感染者の居住地（選択できます）", choices = pref,
                        multiple = TRUE,
                        selected = c("東京都","神奈川県","埼玉県","千葉県","大阪府","兵庫県","福岡県")
                        ),
        fluidRow(
            column(width = 6, plotOutput("plot1")),
            column(width = 6, plotOutput("plot2")),
            column(width = 6,
                   checkboxInput(inputId = "logscale",
                                 label = "対数表示", value = TRUE)

        )
    )
)

server <- function(input, output) {

    output$plot1 <- renderPlot({
        graph <- japan_daily %>% filter(place %in% input$pref) %>%
            ggplot(aes_string(x = "date", y = "cum",
                              color = "place")) +
            ggtitle("感染者数の推移") +
            geom_line() +
            labs(x = "日付", y = "感染者数", color = "居住地") +
            theme_bw(base_family = "HiraKakuProN-W3")
    if(input$logscale)
        graph <- graph + scale_y_log10()
    return(graph)
    })
    
    output$plot2 <- renderPlot({
        japan_daily %>% filter(place %in% input$pref) %>%
            ggplot(aes_string(x = "date", y = "moving_average_increase_rate",
                              color = "place")) +
            geom_line() + ggtitle("感染者数の増加率（7日間の移動平均）") +
            labs(x = "日付", y = "増加率", color = "居住地") +
            scale_y_continuous(labels = percent) +
            theme_bw(base_family = "HiraKakuProN-W3")
        })}
shinyApp(ui = ui, server = server)
