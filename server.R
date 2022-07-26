server <- function(input, output) {
  
  # Remove scientific notation
  options(scipen=999)
  
  # Read data
  df <- reactive({
    df <- read_csv("bid-tabulations-1.csv")
    
    # Replace NA values with 0s 
    df <- df %>% mutate_at(vars("Bid Price"), ~replace_na(.,0))
    
    # Replace NA values with 0s 
    df <- df %>% mutate_at(vars("Class Number"), ~replace_na(.,0))
    
    # Remove Bids without Bid Item values 
    df <- df %>% drop_na("Bid Item")
    
    # Change date format from m/d/y to Y-m-d
    df$`Bid Opening Date` <- format(as.Date(df$`Bid Opening Date`, "%m/%d/%Y"), format = "%Y-%m-%d")
    df
  })
  
  # Number formatting
  f1 <- function(num) {
    format(num, big.mark = ' ')
  }
  
  # Minimum bid price value over the months
  min_bid_price <- reactive({
    min_df <- df() %>%
      summarise(min = f1(min(df()$`Bid Price`)))
    min_df$min
  })
  
  # Maximum bid price value over the months
  max_bid_price <- reactive({
    max_df <- df() %>%
      summarise(max = f1(max(df()$`Bid Price`)))
    max_df$max
  })
  
  # Mean bid price value over the months
  mean_bid_price <- reactive({
    mean_df <- df() %>%
      summarise(mean = f1(mean(df()$`Bid Price`)))
    mean_df$mean
  })
  
  # Value boxes
  output$min_bp <- renderValueBox({
    valueBox(
      value = min_bid_price(),
      subtitle = "Minimum Bid Price",
      color = "danger",
      icon = icon("coins"),
      gradient = TRUE
      
    )
  })
  
  output$max_bp <- renderValueBox({
    valueBox(
      value = max_bid_price(),
      subtitle = "Maximum Bid Price",
      color = "danger",
      icon = icon("coins"),
      gradient = TRUE
      
    )
  })
  
  output$mean_bp <- renderValueBox({
    valueBox(
      value = mean_bid_price(),
      subtitle = "Average Bid Price",
      color = "danger",
      icon = icon("coins"),
      gradient = TRUE
      
    )
  })
  
  # Distribution of Bid Prices 
  output$fig1 <- renderPlotly({
    df() %>%
      plot_ly(
        y = ~`Bid Price`,
        type = 'violin',
        box = list(visible = T),meanline = list(visible = T), x0 = 'Bid Price') %>% 
      layout(
        title = "Distribution of Bid Price",
        yaxis = list(title = "%", zeroline = F))
  })
  
  # Most Frequent Bids 
  
  most_popular_df <- reactive({
    # Contact Name
    most_popular_bid_df <- df() %>% group_by(`Contact  Name`) %>% 
      summarise(requests = n()) %>% 
      arrange(desc(requests))

    cn <- most_popular_bid_df$`Contact  Name`[1]
    cnb <- most_popular_bid_df$requests[1]
    
    # Bid Title
    most_popular_bid_df1 <- df() %>% group_by(`Bid Title`) %>% 
      summarise(requests = n()) %>% 
      arrange(desc(requests))
    bt <- most_popular_bid_df1$`Bid Title`[1]
    btb <- most_popular_bid_df1$requests[1]
    
    # Bid Item
    most_popular_bid_df2 <- df() %>% group_by(`Bid Item`) %>% 
      summarise(requests = n()) %>% 
      arrange(desc(requests))

    bi <- most_popular_bid_df2$`Bid Item`[1]
    bib <- most_popular_bid_df2$requests[1]
    
    # Bidder Name
    most_popular_bid_df3 <- df() %>% group_by(`Bidder Name`) %>% 
      summarise(requests = n()) %>% 
      arrange(desc(requests))
    
    bn <- most_popular_bid_df3$`Bidder Name`[1]
    bnb <- most_popular_bid_df3$requests[1]
    
    most_popular_df <- data.frame(
      Indicator = c("Contact Name", "Bid Title", "Bid Item", "Bidder Name"),
      Most_Frequent = c(cn, bt, bi, bn),
      Bids = c(cnb, btb, bib, bnb)
    )
    most_popular_df
  })
  
  
  
  
  # Data Table
  output$table1 <- renderDT({
    DT::datatable(most_popular_df(),
                  rownames = F,
                  options = list(pageLength = 5, scrollX = TRUE, info = FALSE,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#1f77b4', 'color': '#fff'});",
                                   "}")))
  })
  
  # Average Bid Price Overtime
  output$fig2 <- renderHighchart({
    
    # Average Bid Price
    average_bid_price_df <- df() %>% group_by(`Bid Opening Date`) %>% 
      summarise(Average_bid_price = round(mean(`Bid Price`),2))
    average_bid_price_df$`Bid Opening Date` <- as.Date(average_bid_price_df$`Bid Opening Date`)
    
    # Average Bid Price Chart
    bid_vp <- xts(x = average_bid_price_df$Average_bid_price, 
                  order.by = as.POSIXct(average_bid_price_df$`Bid Opening Date`))
    highchart(type = "stock") %>%
      hc_add_series(bid_vp,
                    type = "line",
                    color = "#1f77b4") %>%
      hc_title(text = "Average Bid Price Overtime") %>%
      hc_yAxis(title = list(text = "Average Bid Price"),
               opposite = FALSE)
  })
  
  # Bid Indicator
  output$bid_indicator_select <- renderUI({
    varSelectInput("bid_indicator", label = "Bid Indicator",
                   df()[c(9,2,5,6)],
                   selected = df()[2],
                   width = 250)
  })
  
  # Bid Indicator According to Average Bid Price
  bid_category_df <- reactive({
    bid_category_df <- df() %>% group_by(!!input$bid_indicator) %>% 
      summarise(average_bid_price = round(mean(`Bid Price`),2)) %>% 
      arrange(desc(average_bid_price))
    bid_category_df
  })
  
  # Top 5 Highest Average Bid Contact Name
  top5_bids <- reactive({
    top5_bids <- bid_category_df() %>% 
      slice_head(n=5)
    top5_bids
  })
  
  # Top 5 Lowest Average Bid Contact Name
  bottom5_bids <- reactive({
    bottom5_bids <- bid_category_df() %>% 
      slice_tail(n=5)
    bottom5_bids
  })
  
  
  output$fig_top5_bp <- renderHighchart({
    top5_bids() %>% 
      hchart("column", hcaes(x = !!input$bid_indicator, y = average_bid_price)) %>% 
      hc_title(
        text = "Average Bid Price") %>% 
      hc_colors("#1f77b4") %>% 
      hc_xAxis(title = list(text = paste0(input$bid_indicator))) %>% 
      hc_yAxis(title = list(text = "Average Bid Price"))
  })
  
  output$fig_bottom5_bp <- renderHighchart({
    bottom5_bids() %>% 
      hchart("column", hcaes(x = !!input$bid_indicator, y = average_bid_price)) %>% 
      hc_title(
        text = "Average Bid Price") %>% 
      hc_colors("#1f77b4") %>% 
      hc_xAxis(title = list(text = paste0(input$bid_indicator))) %>% 
      hc_yAxis(title = list(text = "Average Bid Price"))
  })
    
  
  # Full Data Table
  output$table2 <- renderDT({
    DT::datatable(df(),
                  rownames = F,
                  options = list(pageLength = 2, scrollX = TRUE, info = FALSE,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#1f77b4', 'color': '#fff'});",
                                   "}")))
  })
  
  # Contact Name Average Bid Price overtime 
  cn_avg_bid_price_df <- reactive({
    cn_avg_bid_price_df <- df() %>% 
      group_by(`Contact  Name`, `Bid Opening Date`) %>% 
      summarise(Average_bid_price = round(mean(`Bid Price`),2))
    cn_avg_bid_price_df
  })
 
  
  # Contact Name Select
  output$cn_select <- renderUI({
    selectizeInput('cn_sel',
                label = "Select Contact Name",
                choices = unique(cn_avg_bid_price_df()$`Contact  Name`),
                selected = cn_avg_bid_price_df()$`Contact  Name`[1],
                multiple = TRUE,
                options = list(maxItems = 3))
  })
  
  output$fig3 <- renderHighchart({
    cn_avg_bid_price_df() %>% 
      filter(`Contact  Name` %in% !!input$cn_sel) %>% 
      hchart("line", hcaes(x = `Bid Opening Date`, y = Average_bid_price, group = `Contact  Name`)) %>% 
      hc_title(
        text = paste0("Average Bid Price Overtime")
      ) %>% 
      hc_xAxis(title = list(text = "Date")) %>% 
      hc_yAxis(title = list(text = "Average Bid Price"))
  })
  
}