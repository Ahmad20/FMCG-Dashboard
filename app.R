library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(plotly)
library(europepmc)
source("function.R")
### Default shop list
shop_list <- c("nestle.official.store", "indofooddapurcitarasa", 
               "unileverindonesia", "wingsofficialshop", "pgofficialstore", 
               "mayoraofficialstore", "johnsonnjohnson.id", "kapalapistore",
               "frisianflagofficial")
### Create Empty dataframe
shop_df <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(shop_df) <- c("Shop.id", "Shopname", "Shop.location", "Shop.rating", "Shop.follower")
### Append shop details to dataframe
for (shop in shop_list){
  shop_df[nrow(shop_df)+1,] <- set_shop(shop)
}
#shop_df

ui <- shinyUI(
  dashboardPage(
    
    skin = "midnight",
    #title = "FMCG Indonesia | Tetris 2022",
    title = "FMCG Indonesia | Tetris 2022",
    dashboardHeader(
      title = tagList(
        tags$span(
          class = "logo-mini", "AJT",
        ),
        tags$span(
          class = "logo-lg", "Tetris 2022 | DQLab",
        )
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          text = "Trend 2022", 
          tabName = "page1",
          badgeLabel = "New!", 
          badgeColor = "green",
          icon = icon("chart-line")
        ),
        menuItem(
          text = "Shop Details", 
          tabName = "page2",
          badgeLabel = "Updated!", 
          badgeColor = "green",
          icon = icon("info")
        ),
        menuItem(
          text = "Solution", 
          tabName = "page3",
          badgeLabel = "!", 
          badgeColor = "yellow",
          icon = icon("gear")
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "page1",
          
          fluidRow(
            box(
              title = "FMCG Trend in 2022", 
              closable = FALSE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 12,
              
              infoBoxOutput(
                outputId = "digital", 
                width = 4
              ),
              
              infoBoxOutput(
                outputId = "mental",
                width = 4
              ),
              
              infoBoxOutput(
                outputId = "nature",
                width = 4
              ),
              htmlOutput("ref1")
            )
          ),
          
          fluidRow(
            box(
              title = "Customer in 2022", 
              closable = FALSE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 12,
              
              infoBoxOutput(
                outputId = "settler", 
                width = 4
              ),
              
              infoBoxOutput(
                outputId = "stabiliser",
                width = 4
              ),
              
              infoBoxOutput(
                outputId = "optimist",
                width = 4
              ),
              htmlOutput("ref2")
            )
          )
        ),
        tabItem(
          tabName = "page2",
          
          fluidRow(
            box(
              title = "Overview Dashboard", 
              closable = FALSE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 12,
              
              infoBoxOutput(
                outputId = "nshop", 
                width = 4
              ),
              
              infoBoxOutput(
                outputId = "popularshop",
                width = 4
              ),
              
              infoBoxOutput(
                outputId = "ratingshop",
                width = 4
              )
            )
          ),
          
          fluidRow(
            
            box(
              title = "Select Shop Name", 
              closable = FALSE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 3,
              #height = "600px",
              
              selectInput(
                inputId = "shopname", 
                label = "Select Shop Name", 
                choices = c(shop_df$Shopname),
                selected = "Wings Official Shop"
              ),
              plotlyOutput(
                outputId = "pie"
              )
            ),
            
            box(
              title = "Graph", 
              closable = FALSE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 6,
              height = "800px",
              
              plotlyOutput(
                outputId = "graph"
              )
            ),
            
            box(
              title = "Shop Details", 
              closable = FALSE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 3,
              height = "600px",

              htmlOutput("text1")
            )
          )
        ),
        
        tabItem(
          tabName = "page3",
          
          fluidRow(
            box(
              title = "Product Solution", 
              closable = FALSE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 12,
              
              infoBoxOutput(
                outputId = "priceless", 
                width = 3
              ),
              
              infoBoxOutput(
                outputId = "satisfy",
                width = 3
              ),
              
              infoBoxOutput(
                outputId = "purpose",
                width = 3
              ),
              infoBoxOutput(
                outputId = "available",
                width = 3
              )
            )
          ),
            
          fluidRow(
            box(
              title = "Technical Solution", 
              closable = FALSE, 
              enable_label = TRUE,
              label_status = "danger",
              status = "primary", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              width = 12,
              
              infoBoxOutput(
                outputId = "connection", 
                width = 6
              ),
              
              infoBoxOutput(
                outputId = "recommendation",
                width = 6
              )
            )
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  ## Page 1
  ### Box 1
  output$digital <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 35px;", "80%"), 
            subtitle = "Pengalaman Selalu Terhubung",
            color = "blue", 
            icon = icon("wifi"),
            fill = F)
  })
  
  output$mental <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 35px;", "61%"), 
            subtitle = "Kesadaran Pada Kesehatan Mental",
            color = "maroon", 
            icon = icon("brain"),
            fill = F)
  })
  
  output$nature <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 35px;", "55%"), 
            subtitle = "Peduli Pada Lingkungan",
            color = "green", 
            icon = icon("leaf"),
            fill = F)
  })
  
  output$ref1 <- renderUI({
    HTML("<a href='https://blog.usetada.com/id/seperti-apa-tren-industri-fmcg-di-2022'
    target='_blank'>
         Sumber</a>")
  })
  output$settler <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 30px;", "The Settlers"), 
            subtitle = "Fokus pada pekerjaan",
            color = "olive", 
            icon = icon("briefcase"),
            fill = F)
  })
  
  output$stabiliser <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 30px;", "The Stabilisers"), 
            subtitle = "Fokus pada keseimbangan hidup",
            color = "aqua", 
            icon = icon("balance-scale"),
            fill = F)
  })
  
  output$optimist <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 30px;", "The Optimist"), 
            subtitle = "Fokus pada kesenangan hidup",
            color = "yellow", 
            icon = icon("play"),
            fill = F)
  })
  
  output$ref2 <- renderUI({
    HTML("<a href='https://www.wgsn.com/assets/marketing/WGSN_Future_Consumer_2022_Executive_Summary.pdf'
    target='_blank'>
         Sumber</a>")
  })
  
  ## Page2
  output$nshop <- renderInfoBox({
    
    infoBox(title = "Jumlah Toko",
            value = tags$p(style = "font-size: 35px;", nrow(shop_df)), 
            #subtitle = FALSE,
            color = "red", 
            icon = icon("store"),
            fill = F)
    
  })
  
  output$popularshop <- renderInfoBox({
    
    infoBox(title = "Popular Shop",
            value = tags$p(style = "font-size: 20px;", 
                           shop_df[shop_df$Shop.follower == max(shop_df$Shop.follower), ]$Shopname), 
            subtitle = shop_df[shop_df$Shop.follower == max(shop_df$Shop.follower), ]$Shop.location,
            color = "green",
            icon = icon("user"),
            fill = F)
    
  })
  
  output$ratingshop <- renderInfoBox({
    
    infoBox(title = "Highest Rating", 
            value = tags$p(style = "font-size: 20px;",
                           shop_df[shop_df$Shop.rating == max(shop_df$Shop.rating), ]$Shopname),
            subtitle = shop_df[shop_df$Shop.rating == max(shop_df$Shop.rating), ]$Shop.location,
            color = "blue",
            icon = icon("star"),
            fill = F)
    
  })
  
  output$graph <- renderPlotly({
    plot_ly(
      data =get_product(get_shopid(shop_df, input$shopname)) %>%
        select(name, sold) %>% arrange(-sold) %>%head(7),
      x= ~name,
      y= ~sold, 
      type = "bar", color = ~name
    ) %>% 
      layout(title = list(text = input$shopname, xanchor = "center", font = list(color ="white")), 
            yaxis = list(title = "Sold", gridcolor="white"), 
             xaxis = list(showticklabels = FALSE, title=""), 
             showlegend = F) %>%
      layout(plot_bgcolor = 'rgb(39,44,48)', paper_bgcolor = 'rgb(39,44,48)',
             font = list(color = '#FFF'))
  })
  
  data_for_plot <-
    shop_df %>%
    count(Shop.location)
  output$pie <- renderPlotly({
    plot_ly(data_for_plot, labels = ~Shop.location, values = ~n, type = 'pie', hole = 0.6,
            textposition = 'inside',textinfo = 'percent') %>%
      layout(title = list(text = "Sebaran Lokasi Toko", xanchor = "center", font = list(color ="white", family="Arial")),
             showlegend = F,
             margin = list(l = 20, r = 20, t = 30)) %>%
      layout(paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$text1 <- renderUI({
    mainText <- shop_df[shop_df$Shop.id == get_shopid(shop_df, input$shopname), ]
    str1 <- paste("Name :", mainText$Shopname)
    str2 <- paste("Follower :", mainText$Shop.follower)
    str3 <- paste("Rating :", round(as.numeric(mainText$Shop.rating),2))
    str4 <- paste("Location :", mainText$Shop.location)
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
  })
  
  ## Page 3
  output$priceless <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 35px;", "Price"), 
            subtitle = "Menyesuaikan harga",
            color = "lime", 
            icon = icon("dollar"),
            fill = F)
  })
  
  output$satisfy <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 35px;", "Satifying"), 
            subtitle = "Memberikan kepuasan kepada pelanggan",
            color = "fuchsia", 
            icon = icon("thumbs-up"),
            fill = F)
  })
  
  output$purpose <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 35px;", "Purpose"), 
            subtitle = "Produk yang berfungsi untuk konsumen",
            color = "blue", 
            icon = icon("user"),
            fill = F)
  })
  
  output$available <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 35px;", "Available"), 
            subtitle = "Ketersediaan produk selalu",
            color = "teal", 
            icon = icon("cubes"),
            fill = F)
  })
  
  output$connection <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 35px;", "Connection"), 
            subtitle = "Kesiapan dengan perkembangan jaringan",
            color = "red", 
            icon = icon("globe"),
            fill = F)
  })
  
  output$recommendation <- renderInfoBox({
    infoBox(title = "",
            value = tags$p(style = "font-size: 35px;", "Recommendation System"), 
            subtitle = "Kesiapan dengan perkembangan jaringan",
            color = "aqua", 
            icon = icon("shapes"),
            fill = F)
  })
}
shinyApp(ui, server)