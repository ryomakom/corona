#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)

#fontの導入
library(showtext)
font_add_google("Noto Sans JP", "NSJP")
showtext_auto()

#東洋経済サイトからのデータ読み込み
data <- read_csv("https://raw.githubusercontent.com/kaz-ogiwara/covid19/master/data/prefectures.csv")

#実効再生産数の算出。定義は東洋経済サイトと同じ
rt_data <- data %>%
    mutate(day=as.Date(str_c(year,month,date,sep = "-"))) %>%
    select(-year,-month,-date) %>%
    group_by(prefectureNameJ) %>%
    mutate(rt=((testedPositive-lag(testedPositive,7))/(lag(testedPositive,7)-lag(testedPositive,14)))^(5/7)) %>%
    mutate(rt=ifelse(rt=="NaN",0,ifelse(rt=="Inf",0,rt))) %>%
    mutate(rt=round(rt,digits=1))

#地図データ作成（https://qiita.com/masaki_kubota/items/98ddec63492a8d71f9d1を参考）
JP_plot <- tribble(
    ~Prefecture, ~Code, ~x, ~y,
    "北海道", "HK", 16, 14,
    "青森県",　"AO", 15, 11,
    "岩手県",　"IT", 15, 10,
    "宮城県",　"MG", 15, 9,
    "秋田県",　"AK", 14, 10,
    "山形県",　"YG", 14, 9,
    "福島県",　"FS", 15, 8,
    "茨城県",　"IB", 15, 6,
    "栃木県",　"TC", 15, 7,
    "群馬県",　"GU", 14, 7,
    "埼玉県",　"ST", 14, 6,
    "千葉県",　"CB", 15, 5,
    "東京都",　"TY", 14, 5,
    "神奈川県",　"KN", 14, 4,
    "新潟県",　"NI", 14, 8,
    "富山県",　"TM", 13, 7,
    "石川県",　"IS", 12, 7,
    "福井県",　"FI", 12, 6,
    "山梨県",　"YN", 13, 5,
    "長野県",　"NA", 13, 6,
    "岐阜県",　"GI", 12, 5,
    "静岡県",　"SZ", 13, 4,
    "愛知県",　"AI", 12, 4,
    "三重県",　"ME", 11, 4,
    "滋賀県",　"SI", 11, 5,
    "京都府",　"KY", 10, 5,
    "大阪府",　"OS", 9, 5,
    "兵庫県",　"HG", 8, 5,
    "奈良県",　"NR", 10, 4,
    "和歌山県",　"WA", 9, 4,
    "鳥取県",　"TT", 7, 5,
    "島根県",　"SM", 6, 5,
    "岡山県",　"OY", 7, 4,
    "広島県",　"HS", 6, 4,
    "山口県",　"YA", 5, 5,
    "徳島県",　"TK", 7, 1,
    "香川県",　"KA", 7, 2,
    "愛媛県",　"EH", 6, 2,
    "高知県",　"KO", 6, 1,
    "福岡県",　"FO", 4, 3,
    "佐賀県",　"SG", 3, 3,
    "長崎県",　"NS", 2, 3,
    "熊本県",　"KU", 3, 2,
    "大分県",　"OI", 4, 2,
    "宮崎県",　"MZ", 4, 1,
    "鹿児島県", "KG", 3, 1,
    "沖縄県",　"OK", 1, 1)

# Define Map sizes
JP_plot$Scale <- c(3,rep(1,46)) #北海道だけ少し大きく

# Abbreviated Prefectues
JP_plot$Prefec <- JP_plot$Prefecture
JP_plot$Prefec <- gsub("県", "", JP_plot$Prefec)  
JP_plot$Prefec <- gsub("府", "", JP_plot$Prefec) 
JP_plot$Prefec <- gsub("東京都", "東京", JP_plot$Prefec)

#地図にデータを合体
JP_plot <- JP_plot %>% left_join(rt_data, by=c("Prefecture"="prefectureNameJ"))

#最新の日付を抽出
latest <- JP_plot %>% select(day) %>% summarize(max(day)) %>% pull()

#描画

ui <- fluidPage(
    sidebarPanel(
        `sliderInput`(inputId = "day",
        label = "日付",
        min = as.Date("2020-04-01","%Y-%m-%d"),
        max = as.Date(latest,"%Y-%m-%d"),
        value=as.Date("2020-05-01"),
        timeFormat="%Y-%m-%d",
        animate = TRUE),
                 ),
    mainPanel(
        `plotOutput`("map")
        )
    )
server <- function(input, output) {
    output$map <- `renderPlot`({
        theday <- input$day
        JP_plot %>%
            filter(day==theday) %>%
            mutate(rt_bin = cut(rt,
                                #breaks = c(-1, 0.4, 0.8, 1.2, 1.6, 2.0, 2.4, 2.8, 3.2, 3.4)
                                breaks = c(-100, 0.4, 2.8, 3.2, 100)
            )
            ) %>%
            ggplot(aes(x = x,
                       y = y,
                       width = Scale,
                       height = Scale)
                   ) + 
            geom_tile(aes(fill = rt), color = "white") + 
            geom_text(aes(label = Prefec), size = 2.7, color = "black") + 
            #theme_bw(base_family="HiraKakuProN-W3") +
            theme_bw(base_family="NSJP") +
            coord_fixed(ratio = 1) + 
            theme(
                #panel.background = element_blank(),
                panel.grid = element_blank(), 
                axis.title = element_blank(), 
                axis.text = element_blank(), 
                axis.ticks = element_blank()) +
            #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 1) +
            #scale_fill_brewer(palette="RdBu") +
            #scale_fill_gradientn(colours = c("darkred", "orange", "yellow", "white"))+
            scale_fill_binned(low="white", high="red",
                              breaks = c(0.3, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 2.4),
                              guide = guide_coloursteps(even.steps = TRUE,
                                                        barheight = unit(4.5, "in"),
                                                        barwidth = unit(0.1, "in"),
                                                        show.limits = TRUE)
                              ) +
            labs(title="各都道府県の実効再生産数", fill = "実効再生産数")
        })
}

shinyApp(ui, server)
