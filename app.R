library(shiny)
library(plotly)
library(leaflet)

# =====================================================
# BASE CASE FROM DECK (Appendix-6)
# =====================================================

BASE_PRICE        <- 599
BASE_USERS        <- 200000
BASE_SUB_RATE     <- 40
MONTHLY_SUB       <- 15.59
COGS              <- 350

# =====================================================
# GLOBAL EXPANSION DATA (Appendix-7)
# =====================================================

country_data <- data.frame(
  country = c(
    "United States", "United Kingdom", "Germany", "France",
    "Japan", "South Korea", "Singapore", "UAE",
    "China", "India", "Australia"
  ),
  device_price = c(
    "$599", "£599", "€649", "€649",
    "¥89,800", "₩890,000", "SGD 799", "AED 2,499",
    "¥4,999 RMB", "₹39,999", "AUD 899"
  ),
  subscription = c(
    "$15/month", "£15/month", "€15/month", "€15/month",
    "¥1,800/month", "₩19,000/month", "SGD 20/month", "AED 59/month",
    "¥79 RMB/month", "₹699/month", "AUD 22/month"
  ),
  phase = c(
    "Phase 1 (2026–27)", "Phase 2 (2028–30)", "Phase 2 (2028–30)", "Phase 2 (2028–30)",
    "Phase 2 (2028–30)", "Phase 2 (2028–30)", "Phase 2 (2028–30)", "Phase 2 (2028–30)",
    "Phase 3 (2030+)", "Phase 3 (2030+)", "Phase 2 (2028–30)"
  ),
  lat = c(38, 55, 51, 46, 36, 36.5, 1.35, 24, 35, 21, -25),
  lon = c(-97, -3, 10, 2, 138, 128, 103.8, 54, 104, 78, 133),
  stringsAsFactors = FALSE
)

# =====================================================
# UI
# =====================================================

ui <- navbarPage(
  
  title = "Google Glass Relaunch — Team Autonomy, IIM Sirmaur",
  
  # --------------------------------------------------
  # TAB 1: REVENUE MODEL
  # --------------------------------------------------
  
  tabPanel(
    
    "Revenue Model",
    
    sidebarLayout(
      
      sidebarPanel(
        
        h4("Pricing Sensitivity"),
        
        sliderInput(
          "price",
          "Device Price ($)",
          min   = 399,
          max   = 999,
          value = 599,
          step  = 25
        ),
        
        tags$hr(),
        
        h4("Subscription Mix"),
        p("Weighted avg: $15.59/month"),
        p("Gemini Plus (70%) — $7.99"),
        p("Gemini Pro (25%) — $19.99"),
        p("Gemini Ultra (5%) — $99.99"),
        
        tags$hr(),
        
        p(strong("Base case from deck:")),
        p("$599 price · 200K users · 40% sub adoption"),
        p(em("Source: Appendix-6"))
        
      ),
      
      mainPanel(
        
        # KPI Row 1
        fluidRow(
          column(3, wellPanel(h5("Expected Users"),       h3(textOutput("users")))),
          column(3, wellPanel(h5("Sub Adoption Rate"),    h3(textOutput("sub_rate")))),
          column(3, wellPanel(h5("Device Revenue"),       h3(textOutput("device_rev")))),
          column(3, wellPanel(h5("Total Revenue"),        h3(textOutput("total_rev"))))
        ),
        
        # KPI Row 2
        fluidRow(
          column(3, wellPanel(h5("COGS (fixed)"),         h3("$350"))),
          column(3, wellPanel(h5("Gross Margin / Unit"),  h3(textOutput("gross_margin")))),
          column(3, wellPanel(h5("Gross Margin %"),       h3(textOutput("margin_pct")))),
          column(3, wellPanel(h5("Subscription Revenue"), h3(textOutput("sub_rev"))))
        ),
        
        br(),
        
        plotlyOutput("pricing_curve", height = "420px"),
        
        br(),
        
        plotlyOutput("subscription_mix", height = "360px")
        
      )
    )
  ),
  
  # --------------------------------------------------
  # TAB 2: UNIT ECONOMICS
  # --------------------------------------------------
  
  tabPanel(
    
    "Unit Economics",
    
    fluidRow(
      
      column(
        6,
        h4("Revenue Waterfall — Selected Price vs Base Case"),
        plotlyOutput("waterfall", height = "420px")
      ),
      
      column(
        6,
        h4("Break-even Analysis"),
        plotlyOutput("breakeven", height = "420px")
      )
      
    ),
    
    br(),
    
    fluidRow(
      
      column(
        12,
        h4("Comparable Unit Economics (from Appendix-3)"),
        tableOutput("unit_econ_table")
      )
      
    )
    
  ),
  
  # --------------------------------------------------
  # TAB 3: GLOBAL EXPANSION
  # --------------------------------------------------
  
  tabPanel(
    
    "Global Expansion",
    
    fluidRow(
      column(12,
             p(strong("Click any marker to see local pricing and subscription details.")),
             leafletOutput("world_map", height = 580)
      )
    ),
    
    br(),
    
    fluidRow(
      column(12,
             h4("Country Pricing Table (Appendix-7)"),
             tableOutput("country_table")
      )
    )
    
  )
  
)

# =====================================================
# SERVER
# =====================================================

server <- function(input, output, session) {
  
  # --------------------------------------------------
  # REACTIVES
  # --------------------------------------------------
  
  expected_users <- reactive({
    round(BASE_USERS * (BASE_PRICE / input$price)^1.5)
  })
  
  sub_rate <- reactive({
    pmin(70, pmax(20, BASE_SUB_RATE + ((BASE_PRICE - input$price) / 10)))
  })
  
  device_revenue <- reactive({
    expected_users() * input$price
  })
  
  subscription_revenue <- reactive({
    expected_users() * (sub_rate() / 100) * MONTHLY_SUB * 12
  })
  
  total_revenue <- reactive({
    device_revenue() + subscription_revenue()
  })
  
  gross_margin_unit <- reactive({
    input$price - COGS
  })
  
  # --------------------------------------------------
  # KPI OUTPUTS
  # --------------------------------------------------
  
  output$users <- renderText({
    format(expected_users(), big.mark = ",", scientific = FALSE)
  })
  
  output$sub_rate <- renderText({
    paste0(round(sub_rate(), 1), "%")
  })
  
  output$device_rev <- renderText({
    paste0("$", format(round(device_revenue()), big.mark = ","))
  })
  
  output$total_rev <- renderText({
    paste0("$", format(round(total_revenue()), big.mark = ","))
  })
  
  output$gross_margin <- renderText({
    val <- gross_margin_unit()
    if (val < 0) paste0("-$", abs(round(val))) else paste0("$", round(val))
  })
  
  output$margin_pct <- renderText({
    paste0(round((gross_margin_unit() / input$price) * 100, 1), "%")
  })
  
  output$sub_rev <- renderText({
    paste0("$", format(round(subscription_revenue()), big.mark = ","))
  })
  
  # --------------------------------------------------
  # REVENUE SENSITIVITY CURVE
  # --------------------------------------------------
  
  output$pricing_curve <- renderPlotly({
    
    prices         <- seq(399, 999, by = 25)
    users_vec      <- round(BASE_USERS * (BASE_PRICE / prices)^1.5)
    adoption_vec   <- pmin(70, pmax(20, BASE_SUB_RATE + ((BASE_PRICE - prices) / 10)))
    device_rev_vec <- users_vec * prices
    sub_rev_vec    <- users_vec * (adoption_vec / 100) * MONTHLY_SUB * 12
    total_rev_vec  <- device_rev_vec + sub_rev_vec
    
    plot_ly() %>%
      add_lines(
        x = prices, y = device_rev_vec,
        name = "Device Revenue",
        line = list(color = "#E8A200", dash = "dot")
      ) %>%
      add_lines(
        x = prices, y = sub_rev_vec,
        name = "Subscription Revenue",
        line = list(color = "#8B6914", dash = "dot")
      ) %>%
      add_lines(
        x = prices, y = total_rev_vec,
        name = "Total Revenue",
        line = list(color = "#FFFFFF", width = 3)
      ) %>%
      add_markers(
        x = input$price, y = total_revenue(),
        name = "Selected Scenario",
        marker = list(color = "#FF4444", size = 12)
      ) %>%
      add_markers(
        x = 599,
        y = BASE_USERS * 599 + BASE_USERS * 0.40 * MONTHLY_SUB * 12,
        name = "Deck Base Case",
        marker = list(color = "#00CC44", size = 12, symbol = "star")
      ) %>%
      layout(
        title = "Revenue Sensitivity to Device Pricing",
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor  = "#1a1a2e",
        font   = list(color = "#FFFFFF"),
        xaxis  = list(title = "Device Price ($)", color = "#FFFFFF"),
        yaxis  = list(title = "Revenue (USD)", color = "#FFFFFF"),
        legend = list(font = list(color = "#FFFFFF"))
      )
    
  })
  
  # --------------------------------------------------
  # SUBSCRIPTION MIX DONUT (Appendix-6)
  # --------------------------------------------------
  
  output$subscription_mix <- renderPlotly({
    
    mix <- data.frame(
      Tier  = c("Gemini AI Plus ($7.99)", "Gemini AI Pro ($19.99)", "Gemini AI Ultra ($99.99)"),
      Share = c(70, 25, 5)
    )
    
    plot_ly(
      mix,
      labels = ~Tier,
      values = ~Share,
      type   = "pie",
      hole   = 0.6,
      marker = list(colors = c("#E8A200", "#8B6914", "#4A3700"))
    ) %>%
      layout(
        title = "Subscription Tier Mix — Weighted Avg: $15.59/month",
        paper_bgcolor = "#1a1a2e",
        font   = list(color = "#FFFFFF"),
        legend = list(font = list(color = "#FFFFFF"))
      )
    
  })
  
  # --------------------------------------------------
  # WATERFALL
  # --------------------------------------------------
  
  output$waterfall <- renderPlotly({
    
    plot_ly(
      x      = c("Device Revenue", "Subscription Revenue", "Total Revenue"),
      y      = c(round(device_revenue()), round(subscription_revenue()), round(total_revenue())),
      type   = "bar",
      marker = list(color = c("#E8A200", "#8B6914", "#FFFFFF"))
    ) %>%
      layout(
        title = paste0("Revenue Breakdown at $", input$price),
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor  = "#1a1a2e",
        font  = list(color = "#FFFFFF"),
        xaxis = list(color = "#FFFFFF"),
        yaxis = list(title = "USD", color = "#FFFFFF")
      )
    
  })
  
  # --------------------------------------------------
  # BREAK-EVEN
  # --------------------------------------------------
  
  output$breakeven <- renderPlotly({
    
    units_vec      <- seq(10000, 500000, by = 10000)
    fixed_costs    <- 50000000
    total_cost_vec <- fixed_costs + units_vec * COGS
    total_rev_vec  <- units_vec * input$price +
      units_vec * (sub_rate() / 100) * MONTHLY_SUB * 12
    
    plot_ly() %>%
      add_lines(
        x = units_vec, y = total_rev_vec,
        name = "Total Revenue",
        line = list(color = "#E8A200")
      ) %>%
      add_lines(
        x = units_vec, y = total_cost_vec,
        name = "Total Cost",
        line = list(color = "#FF4444")
      ) %>%
      layout(
        title = "Break-even: Revenue vs Cost",
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor  = "#1a1a2e",
        font  = list(color = "#FFFFFF"),
        xaxis = list(title = "Units Sold", color = "#FFFFFF"),
        yaxis = list(title = "USD", color = "#FFFFFF"),
        legend = list(font = list(color = "#FFFFFF"))
      )
    
  })
  
  # --------------------------------------------------
  # UNIT ECONOMICS TABLE (Appendix-3)
  # --------------------------------------------------
  
  output$unit_econ_table <- renderTable({
    
    data.frame(
      Metric = c(
        "Selling Price (ASP)", "COGS / BOM", "Gross Profit", "Gross Margin %",
        "Marketing & CAC", "CM1", "R&D + Support + Operations", "CM2",
        "Corporate Overheads", "EBITDA", "Taxes & Misc", "PAT"
      ),
      `Original $1500 Model` = c(
        "$1,500", "$210", "$1,290", "86%",
        "$300", "$990", "$500", "$490",
        "$250", "$240", "$60", "$180"
      ),
      `Enterprise $499 Model` = c(
        "$499", "$210", "$289", "58%",
        "$80", "$209", "$120", "$89",
        "$40", "$49", "$12", "$37"
      ),
      `Relaunch $599 Model` = c(
        paste0("$", input$price),
        "$350",
        paste0("$", input$price - 350),
        paste0(round(((input$price - 350) / input$price) * 100, 1), "%"),
        "~$100", "—", "—", "—", "—", "—", "—", "—"
      ),
      check.names = FALSE
    )
    
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # --------------------------------------------------
  # GLOBAL MAP (Appendix-7)
  # --------------------------------------------------
  
  phase_colors <- c(
    "Phase 1 (2026–27)" = "#E8A200",
    "Phase 2 (2028–30)" = "#8B6914",
    "Phase 3 (2030+)"   = "#4A3700"
  )
  
  output$world_map <- renderLeaflet({
    
    leaflet(country_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng         = ~lon,
        lat         = ~lat,
        radius      = 10,
        color       = ~phase_colors[phase],
        fillColor   = ~phase_colors[phase],
        fillOpacity = 0.9,
        popup = ~paste0(
          "<b>", country, "</b><br>",
          "<b>Phase:</b> ", phase, "<br>",
          "<b>Device Price:</b> ", device_price, "<br>",
          "<b>Subscription:</b> ", subscription
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors   = c("#E8A200", "#8B6914", "#4A3700"),
        labels   = c("Phase 1 (2026–27)", "Phase 2 (2028–30)", "Phase 3 (2030+)"),
        title    = "Expansion Phase"
      )
    
  })
  
  # --------------------------------------------------
  # COUNTRY PRICING TABLE
  # --------------------------------------------------
  
  output$country_table <- renderTable({
    country_data[, c("country", "phase", "device_price", "subscription")]
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
}

# =====================================================
shinyApp(ui, server)
