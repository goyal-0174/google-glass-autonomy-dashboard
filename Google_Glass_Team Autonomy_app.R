# ============================================================
# Google Glass Relaunch — Team Autonomy Dashboard
# IIM Sirmaur | Beautiful Redesign with bslib + plotly
# ============================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(leaflet)
library(dplyr)

# ── COLOUR PALETTE ──────────────────────────────────────────
COL_BG      <- "#0D0F1A"   # deep navy-black
COL_CARD    <- "#141728"   # card surface
COL_BORDER  <- "#1E2240"   # subtle borders
COL_ACCENT  <- "#6C63FF"   # electric violet  ← signature colour
COL_GOLD    <- "#F5A623"   # warm amber/gold
COL_TEAL    <- "#00D4AA"   # fresh teal
COL_RED     <- "#FF4C6A"   # coral red
COL_TEXT    <- "#E8EAFF"   # near-white text
COL_MUTED   <- "#8B92B8"   # muted text

# ── DATA ────────────────────────────────────────────────────

# Unit economics table
unit_econ <- data.frame(
  Metric             = c("Selling Price (ASP)", "COGS / BOM", "Gross Profit",
                         "Gross Margin %", "Marketing & CAC", "CM1",
                         "R&D + Support + Ops", "CM2", "Corporate Overheads",
                         "EBITDA", "Taxes & Misc", "PAT"),
  Original_1500      = c("$1,500","$210","$1,290","86%","$300","$990",
                         "$500","$490","$250","$240","$60","$180"),
  Enterprise_499     = c("$499","$210","$289","58%","$80","$209",
                         "$120","$89","$40","$49","$12","$37"),
  Relaunch_599       = c("$599","$350","$249","41.6%","~$100","—",
                         "—","—","—","—","—","—"),
  stringsAsFactors   = FALSE
)

# Global expansion data
countries <- data.frame(
  country      = c("United States","United Kingdom","Germany","France",
                   "Japan","South Korea","Singapore","UAE","China","India","Australia"),
  phase        = c("Phase 1 (2026–27)","Phase 2 (2028–30)","Phase 2 (2028–30)",
                   "Phase 2 (2028–30)","Phase 2 (2028–30)","Phase 2 (2028–30)",
                   "Phase 2 (2028–30)","Phase 2 (2028–30)","Phase 3 (2030+)",
                   "Phase 3 (2030+)","Phase 2 (2028–30)"),
  device_price = c("$599","£599","€649","€649","¥89,800","₩890,000",
                   "SGD 799","AED 2,499","¥4,999 RMB","₹39,999","AUD 899"),
  subscription = c("$15/month","£15/month","€15/month","€15/month",
                   "¥1,800/month","₩19,000/month","SGD 20/month",
                   "AED 59/month","¥79 RMB/month","₹699/month","AUD 22/month"),
  lat = c(37.09, 55.37, 51.16, 46.22, 36.20, 35.90, 1.35, 23.42, 35.86, 20.59, -25.27),
  lng = c(-95.71, -3.43, 10.45, 2.21, 138.25, 127.76, 103.81, 53.84, 104.19, 78.96, 133.77),
  phase_num = c(1,2,2,2,2,2,2,2,3,3,2),
  stringsAsFactors = FALSE
)

phase_colors <- c("1" = COL_TEAL, "2" = COL_GOLD, "3" = COL_RED)

# ── CUSTOM CSS ──────────────────────────────────────────────
custom_css <- tags$style(HTML(paste0("
/* ── Base ── */
body, .bslib-page-navbar {
  background: ", COL_BG, " !important;
  color: ", COL_TEXT, ";
  font-family: 'Inter', 'Segoe UI', sans-serif;
}

/* ── Navbar ── */
.navbar {
  background: linear-gradient(135deg, #0D0F1A 0%, #141728 100%) !important;
  border-bottom: 2px solid ", COL_ACCENT, " !important;
  box-shadow: 0 4px 30px rgba(108,99,255,0.25);
  padding: 10px 20px;
}
.navbar-brand {
  font-size: 1.1rem !important;
  font-weight: 700 !important;
  color: ", COL_TEXT, " !important;
  letter-spacing: 0.5px;
}
.nav-link {
  color: ", COL_MUTED, " !important;
  font-weight: 500;
  padding: 8px 18px !important;
  border-radius: 8px !important;
  margin: 0 3px;
  transition: all 0.3s ease;
}
.nav-link:hover, .nav-link.active {
  color: ", COL_TEXT, " !important;
  background: rgba(108,99,255,0.2) !important;
}
.nav-link.active {
  border-bottom: 2px solid ", COL_ACCENT, " !important;
}

/* ── Cards ── */
.glass-card {
  background: ", COL_CARD, ";
  border: 1px solid ", COL_BORDER, ";
  border-radius: 16px;
  padding: 24px;
  margin-bottom: 20px;
  box-shadow: 0 8px 32px rgba(0,0,0,0.4);
  transition: transform 0.2s ease, box-shadow 0.2s ease;
}
.glass-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 12px 40px rgba(108,99,255,0.15);
}

/* ── Value Boxes ── */
.val-box {
  background: ", COL_CARD, ";
  border: 1px solid ", COL_BORDER, ";
  border-radius: 14px;
  padding: 20px 22px;
  text-align: left;
  margin-bottom: 16px;
  position: relative;
  overflow: hidden;
  transition: all 0.3s ease;
}
.val-box::before {
  content: '';
  position: absolute;
  top: 0; left: 0;
  width: 4px; height: 100%;
  border-radius: 14px 0 0 14px;
}
.val-box.violet::before { background: ", COL_ACCENT, "; }
.val-box.gold::before   { background: ", COL_GOLD, ";   }
.val-box.teal::before   { background: ", COL_TEAL, ";   }
.val-box.red::before    { background: ", COL_RED, ";    }
.val-box:hover { transform: translateY(-3px); box-shadow: 0 10px 30px rgba(0,0,0,0.5); }
.val-label { font-size: 0.78rem; color: ", COL_MUTED, "; text-transform: uppercase; letter-spacing: 1.2px; margin-bottom: 6px; }
.val-number { font-size: 1.7rem; font-weight: 800; color: ", COL_TEXT, "; line-height: 1; }
.val-sub { font-size: 0.8rem; color: ", COL_MUTED, "; margin-top: 5px; }

/* ── Section Headings ── */
.sec-title {
  font-size: 1.15rem;
  font-weight: 700;
  color: ", COL_TEXT, ";
  margin-bottom: 4px;
  display: flex;
  align-items: center;
  gap: 10px;
}
.sec-title::after {
  content: '';
  flex: 1;
  height: 1px;
  background: linear-gradient(90deg, ", COL_ACCENT, "55, transparent);
  margin-left: 8px;
}
.sec-sub {
  font-size: 0.82rem;
  color: ", COL_MUTED, ";
  margin-bottom: 18px;
}

/* ── Slider ── */
.irs--shiny .irs-bar { background: ", COL_ACCENT, "; }
.irs--shiny .irs-handle { background: ", COL_ACCENT, "; border-color: ", COL_ACCENT, "; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background: ", COL_ACCENT, ";
}
.irs--shiny .irs-line { background: ", COL_BORDER, "; }

/* ── DT Table ── */
.dataTables_wrapper { color: ", COL_TEXT, "; }
table.dataTable { background: ", COL_CARD, " !important; color: ", COL_TEXT, " !important; border: none !important; }
table.dataTable thead th {
  background: #1A1E35 !important;
  color: ", COL_ACCENT, " !important;
  border-bottom: 2px solid ", COL_ACCENT, " !important;
  font-weight: 700;
  text-transform: uppercase;
  font-size: 0.75rem;
  letter-spacing: 1px;
}
table.dataTable tbody tr { border-bottom: 1px solid ", COL_BORDER, " !important; }
table.dataTable tbody tr:hover { background: rgba(108,99,255,0.08) !important; }
.dataTables_filter input, .dataTables_length select {
  background: ", COL_CARD, " !important;
  color: ", COL_TEXT, " !important;
  border: 1px solid ", COL_BORDER, " !important;
  border-radius: 8px;
  padding: 4px 10px;
}
.dataTables_info, .dataTables_filter label, .dataTables_length label { color: ", COL_MUTED, "; }
.paginate_button { color: ", COL_MUTED, " !important; border-radius: 6px !important; }
.paginate_button.current { background: ", COL_ACCENT, " !important; color: white !important; border: none !important; }

/* ── Leaflet ── */
.leaflet-container { border-radius: 14px; }

/* ── Scrollbar ── */
::-webkit-scrollbar { width: 6px; }
::-webkit-scrollbar-track { background: ", COL_BG, "; }
::-webkit-scrollbar-thumb { background: ", COL_BORDER, "; border-radius: 3px; }

/* ── Page padding ── */
.tab-content { padding: 24px 20px; }

/* ── Badge ── */
.phase-badge {
  display: inline-block;
  padding: 3px 10px;
  border-radius: 20px;
  font-size: 0.72rem;
  font-weight: 600;
  letter-spacing: 0.5px;
}
.phase-1 { background: rgba(0,212,170,0.15); color: ", COL_TEAL, "; }
.phase-2 { background: rgba(245,166,35,0.15); color: ", COL_GOLD, "; }
.phase-3 { background: rgba(255,76,106,0.15); color: ", COL_RED, "; }
")))

# ── PLOTLY THEME ────────────────────────────────────────────
plotly_theme <- function(p) {
  p %>% layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font          = list(family = "Inter, sans-serif", color = COL_TEXT),
    xaxis = list(
      gridcolor   = COL_BORDER,
      linecolor   = COL_BORDER,
      tickfont    = list(color = COL_MUTED),
      title       = list(font = list(color = COL_MUTED))
    ),
    yaxis = list(
      gridcolor   = COL_BORDER,
      linecolor   = COL_BORDER,
      tickfont    = list(color = COL_MUTED),
      title       = list(font = list(color = COL_MUTED))
    ),
    legend = list(
      font        = list(color = COL_TEXT),
      bgcolor     = "rgba(20,23,40,0.8)",
      bordercolor = COL_BORDER,
      borderwidth = 1
    ),
    margin = list(l = 50, r = 20, t = 40, b = 50),
    hoverlabel = list(
      bgcolor   = "#1A1E35",
      font      = list(color = COL_TEXT),
      bordercolor = COL_ACCENT
    )
  )
}

# ── HELPER: value box HTML ───────────────────────────────────
vbox <- function(label, value, sub = NULL, color = "violet") {
  div(class = paste("val-box", color),
      div(class = "val-label", label),
      div(class = "val-number", value),
      if (!is.null(sub)) div(class = "val-sub", sub)
  )
}

# ── UI ──────────────────────────────────────────────────────
ui <- page_navbar(
  title = tags$span(
    tags$span("🥽", style = "margin-right:8px;"),
    "Google Glass Relaunch",
    tags$span(" — Team Autonomy · IIM Sirmaur",
              style = paste0("font-size:0.78rem; color:", COL_MUTED, "; font-weight:400; margin-left:6px;"))
  ),
  
  theme = bs_theme(
    bg         = COL_BG,
    fg         = COL_TEXT,
    primary    = COL_ACCENT,
    secondary  = COL_GOLD,
    base_font  = font_google("Inter"),
    bootswatch = NULL
  ),
  
  custom_css,
  
  # ── TAB 1 : REVENUE MODEL ──────────────────────────────────
  nav_panel("💰 Revenue Model",
            fluidRow(
              # ── Sidebar controls ──
              column(3,
                     div(class = "glass-card",
                         div(class = "sec-title", "🎛️ Pricing Controls"),
                         br(),
                         tags$label("Device Price ($)", style = paste0("color:", COL_MUTED, "; font-size:0.8rem; text-transform:uppercase; letter-spacing:1px;")),
                         sliderInput("device_price", NULL,
                                     min = 399, max = 999, value = 599, step = 25,
                                     pre = "$", ticks = TRUE),
                         br(),
                         tags$label("Expected Users", style = paste0("color:", COL_MUTED, "; font-size:0.8rem; text-transform:uppercase; letter-spacing:1px;")),
                         sliderInput("users", NULL,
                                     min = 50000, max = 500000, value = 200000, step = 10000,
                                     pre = "", ticks = FALSE),
                         br(),
                         tags$label("Subscription Adoption %", style = paste0("color:", COL_MUTED, "; font-size:0.8rem; text-transform:uppercase; letter-spacing:1px;")),
                         sliderInput("sub_rate", NULL,
                                     min = 10, max = 80, value = 40, step = 5,
                                     post = "%", ticks = FALSE),
                         br(),
                         div(style = paste0("background:rgba(108,99,255,0.1); border:1px solid rgba(108,99,255,0.3); border-radius:10px; padding:14px; font-size:0.8rem; color:", COL_MUTED, ";"),
                             "📌 Base case from deck:",
                             br(), br(),
                             tags$b(style = paste0("color:", COL_TEXT), "$599 price · 200K users · 40% sub adoption"),
                             br(), br(),
                             tags$em("Source: Appendix-6")
                         )
                     )
              ),
              
              # ── KPI boxes ──
              column(9,
                     fluidRow(
                       column(3, uiOutput("kpi_users")),
                       column(3, uiOutput("kpi_sub")),
                       column(3, uiOutput("kpi_device_rev")),
                       column(3, uiOutput("kpi_total_rev"))
                     ),
                     fluidRow(
                       column(3, uiOutput("kpi_cogs")),
                       column(3, uiOutput("kpi_gross")),
                       column(3, uiOutput("kpi_gm_pct")),
                       column(3, uiOutput("kpi_sub_rev"))
                     ),
                     
                     fluidRow(
                       column(6,
                              div(class = "glass-card",
                                  div(class = "sec-title", "📊 Revenue Breakdown"),
                                  div(class = "sec-sub", "Device vs Subscription revenue at selected price"),
                                  plotlyOutput("plot_revenue_bar", height = "300px")
                              )
                       ),
                       column(6,
                              div(class = "glass-card",
                                  div(class = "sec-title", "📈 Sensitivity to Device Pricing"),
                                  div(class = "sec-sub", "How revenue changes across price points"),
                                  plotlyOutput("plot_sensitivity", height = "300px")
                              )
                       )
                     ),
                     
                     div(class = "glass-card",
                         div(class = "sec-title", "🍩 Subscription Tier Mix"),
                         div(class = "sec-sub", "Weighted average: $15.59/month  ·  Gemini AI tiers"),
                         plotlyOutput("plot_donut", height = "260px")
                     )
              )
            )
  ),
  
  # ── TAB 2 : UNIT ECONOMICS ─────────────────────────────────
  nav_panel("📐 Unit Economics",
            fluidRow(
              column(8,
                     div(class = "glass-card",
                         div(class = "sec-title", "💧 Revenue Waterfall"),
                         div(class = "sec-sub", "From Gross Profit to PAT — Original $1,500 model"),
                         plotlyOutput("plot_waterfall", height = "360px")
                     ),
                     div(class = "glass-card",
                         div(class = "sec-title", "⚖️ Break-even Analysis"),
                         div(class = "sec-sub", "Revenue vs Cost curves — find the break-even point"),
                         plotlyOutput("plot_breakeven", height = "320px")
                     )
              ),
              column(4,
                     div(class = "glass-card",
                         div(class = "sec-title", "📋 Comparable Unit Economics"),
                         div(class = "sec-sub", "Appendix-3 data across all three models"),
                         DTOutput("tbl_unit_econ")
                     )
              )
            )
  ),
  
  # ── TAB 3 : GLOBAL EXPANSION ───────────────────────────────
  nav_panel("🌍 Global Expansion",
            fluidRow(
              column(3,
                     div(class = "glass-card",
                         div(class = "sec-title", "🗺️ Expansion Phases"),
                         br(),
                         div(class = "val-box teal",
                             div(class = "val-label", "Phase 1"),
                             div(class = "val-number", "2026–27"),
                             div(class = "val-sub", "🇺🇸 United States only")
                         ),
                         div(class = "val-box gold",
                             div(class = "val-label", "Phase 2"),
                             div(class = "val-number", "2028–30"),
                             div(class = "val-sub", "🇬🇧 🇩🇪 🇫🇷 🇯🇵 🇰🇷 🇸🇬 🇦🇪 🇦🇺")
                         ),
                         div(class = "val-box red",
                             div(class = "val-label", "Phase 3"),
                             div(class = "val-number", "2030+"),
                             div(class = "val-sub", "🇨🇳 🇮🇳  Emerging markets")
                         ),
                         br(),
                         div(style = paste0("font-size:0.78rem; color:", COL_MUTED, "; background:rgba(0,0,0,0.2); border-radius:10px; padding:12px;"),
                             "💡 Click any map marker to see local pricing and subscription details."
                         )
                     )
              ),
              column(9,
                     div(class = "glass-card",
                         div(class = "sec-title", "🌐 Interactive World Map"),
                         div(class = "sec-sub", "Click markers for country-level pricing details"),
                         leafletOutput("map_global", height = "400px")
                     ),
                     div(class = "glass-card",
                         div(class = "sec-title", "📋 Country Pricing Table"),
                         div(class = "sec-sub", "Appendix-7 · Local pricing by market"),
                         DTOutput("tbl_countries")
                     )
              )
            )
  )
)

# ── SERVER ──────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Reactive calculations ──────────────────────────────────
  device_rev  <- reactive(input$device_price * input$users)
  sub_users   <- reactive(round(input$users * input$sub_rate / 100))
  sub_rev_mo  <- reactive(sub_users() * 15.59)
  sub_rev_yr  <- reactive(sub_rev_mo() * 12)
  total_rev   <- reactive(device_rev() + sub_rev_yr())
  cogs        <- reactive(350 * input$users)
  gross       <- reactive(device_rev() - cogs())
  gm_pct      <- reactive(round(gross() / device_rev() * 100, 1))
  
  fmt <- function(x) {
    if (abs(x) >= 1e9) paste0("$", round(x/1e9,2), "B")
    else if (abs(x) >= 1e6) paste0("$", round(x/1e6,1), "M")
    else if (abs(x) >= 1e3) paste0("$", round(x/1e3,0), "K")
    else paste0("$", round(x,0))
  }
  
  # ── KPI boxes ──────────────────────────────────────────────
  output$kpi_users      <- renderUI(vbox("Expected Users",   format(input$users, big.mark=","), "target customers", "violet"))
  output$kpi_sub        <- renderUI(vbox("Sub Adoption",     paste0(input$sub_rate, "%"),  paste0(format(sub_users(), big.mark=","), " subscribers"), "teal"))
  output$kpi_device_rev <- renderUI(vbox("Device Revenue",   fmt(device_rev()),  paste0("@ $", input$device_price, " ASP"), "gold"))
  output$kpi_total_rev  <- renderUI(vbox("Total Revenue",    fmt(total_rev()),   "device + subscription yr1", "red"))
  output$kpi_cogs       <- renderUI(vbox("COGS (fixed)",     "$350",  "per unit BOM cost", "violet"))
  output$kpi_gross      <- renderUI(vbox("Gross Margin/Unit", paste0("$", input$device_price - 350), "ASP minus COGS", "teal"))
  output$kpi_gm_pct     <- renderUI(vbox("Gross Margin %",   paste0(gm_pct(), "%"), "at selected ASP", "gold"))
  output$kpi_sub_rev    <- renderUI(vbox("Subscription Rev", fmt(sub_rev_yr()), "annual recurring", "red"))
  
  # ── Revenue Bar ────────────────────────────────────────────
  output$plot_revenue_bar <- renderPlotly({
    df <- data.frame(
      Category = c("Device Revenue", "Subscription Revenue", "Total Revenue"),
      Value    = c(device_rev(), sub_rev_yr(), total_rev()) / 1e6
    )
    plot_ly(df, x = ~Category, y = ~Value, type = "bar",
            marker = list(color = c(COL_ACCENT, COL_TEAL, COL_GOLD),
                          line  = list(color = "rgba(0,0,0,0)", width = 0)),
            hovertemplate = "<b>%{x}</b><br>$%{y:.1f}M<extra></extra>") %>%
      layout(yaxis = list(title = "USD (Millions)"), xaxis = list(title = "")) %>%
      plotly_theme()
  })
  
  # ── Sensitivity ────────────────────────────────────────────
  output$plot_sensitivity <- renderPlotly({
    prices <- seq(399, 999, by = 25)
    d_rev  <- prices * input$users / 1e6
    s_rev  <- rep(sub_rev_yr() / 1e6, length(prices))
    t_rev  <- d_rev + s_rev
    
    plot_ly() %>%
      add_lines(x = prices, y = d_rev, name = "Device Revenue",
                line = list(color = COL_ACCENT, width = 2, dash = "dot")) %>%
      add_lines(x = prices, y = s_rev, name = "Subscription Revenue",
                line = list(color = COL_TEAL,   width = 2, dash = "dot")) %>%
      add_lines(x = prices, y = t_rev, name = "Total Revenue",
                line = list(color = COL_GOLD,   width = 3)) %>%
      add_markers(x = input$device_price,
                  y = (input$device_price * input$users + sub_rev_yr()) / 1e6,
                  name = "Selected", marker = list(color = COL_GOLD, size = 12,
                                                   symbol = "star", line = list(color = "white", width = 1.5))) %>%
      layout(xaxis = list(title = "Device Price ($)"),
             yaxis = list(title = "Revenue (USD Millions)")) %>%
      plotly_theme()
  })
  
  # ── Donut ──────────────────────────────────────────────────
  output$plot_donut <- renderPlotly({
    plot_ly(
      labels = c("Gemini AI Plus ($7.99)", "Gemini AI Pro ($19.99)", "Gemini AI Ultra ($99.99)"),
      values = c(70, 25, 5),
      type   = "pie", hole = 0.6,
      marker = list(colors = c(COL_GOLD, "#C8860A", "#8B5E07"),
                    line   = list(color = COL_BG, width = 3)),
      textinfo   = "label+percent",
      hovertemplate = "<b>%{label}</b><br>Share: %{percent}<extra></extra>"
    ) %>%
      layout(showlegend = FALSE,
             annotations = list(list(text = "<b>Wtd Avg<br>$15.59/mo</b>",
                                     x = 0.5, y = 0.5, showarrow = FALSE,
                                     font = list(color = COL_TEXT, size = 13)))) %>%
      plotly_theme()
  })
  
  # ── Waterfall ──────────────────────────────────────────────
  output$plot_waterfall <- renderPlotly({
    labels <- c("ASP","COGS","Gross Profit","Mktg & CAC","CM1",
                "R&D+Ops","CM2","Corp OH","EBITDA")
    values <- c(1500,-210,NA,-300,NA,-500,NA,-250,NA)
    base   <- c(0,1290,0,1290,990,990,490,490,240)
    colors_bar <- c(COL_TEAL, COL_RED, COL_ACCENT,
                    COL_RED, COL_ACCENT, COL_RED, COL_ACCENT, COL_RED, COL_GOLD)
    
    plot_ly(
      x = labels,
      y = c(1500, 210, 1290, 300, 990, 500, 490, 250, 240),
      base  = c(0, 1290, 0, 990, 0, 490, 0, 240, 0),
      type  = "bar",
      marker= list(color = colors_bar,
                   line  = list(color = "rgba(0,0,0,0)", width = 0)),
      hovertemplate = "<b>%{x}</b><br>$%{y}<extra></extra>"
    ) %>%
      layout(yaxis = list(title = "USD per Unit"),
             xaxis = list(title = ""),
             bargap = 0.3) %>%
      plotly_theme()
  })
  
  # ── Break-even ─────────────────────────────────────────────
  output$plot_breakeven <- renderPlotly({
    units      <- seq(0, 500000, by = 10000)
    fixed_cost <- 120e6 + 250e6   # R&D + Corp OH (illustrative)
    var_cost   <- 350
    total_cost <- (fixed_cost + var_cost * units) / 1e6
    total_rev2 <- (599 * units) / 1e6
    
    plot_ly() %>%
      add_lines(x = units/1e3, y = total_rev2, name = "Total Revenue",
                line = list(color = COL_GOLD, width = 3)) %>%
      add_lines(x = units/1e3, y = total_cost, name = "Total Cost",
                line = list(color = COL_RED,  width = 3)) %>%
      layout(xaxis = list(title = "Units Sold (thousands)"),
             yaxis = list(title = "USD (Millions)")) %>%
      plotly_theme()
  })
  
  # ── Unit Economics Table ────────────────────────────────────
  output$tbl_unit_econ <- renderDT({
    datatable(unit_econ,
              colnames = c("Metric","Original $1,500","Enterprise $499","Relaunch $599"),
              rownames = FALSE,
              options  = list(
                dom         = "t",
                pageLength  = 15,
                ordering    = FALSE,
                scrollX     = TRUE
              )
    ) %>%
      formatStyle("Metric",
                  fontWeight = "bold",
                  color      = COL_MUTED) %>%
      formatStyle("Relaunch_599",
                  color      = COL_GOLD,
                  fontWeight = "bold")
  })
  
  # ── Global Map ─────────────────────────────────────────────
  output$map_global <- renderLeaflet({
    pal <- colorFactor(
      palette = c(COL_TEAL, COL_GOLD, COL_RED),
      domain  = c(1, 2, 3)
    )
    
    leaflet(countries) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addCircleMarkers(
        lng    = ~lng, lat = ~lat,
        color  = ~pal(phase_num),
        fillColor   = ~pal(phase_num),
        fillOpacity = 0.9,
        radius = 10,
        stroke = TRUE,
        weight = 2,
        popup  = ~paste0(
          "<div style='background:#141728; color:#E8EAFF; padding:14px; border-radius:10px; border:1px solid #1E2240; font-family:Inter,sans-serif; min-width:180px;'>",
          "<b style='font-size:1rem;'>", country, "</b><br>",
          "<span style='color:#8B92B8; font-size:0.78rem;'>", phase, "</span><hr style='border-color:#1E2240; margin:8px 0;'>",
          "📱 Device: <b>", device_price, "</b><br>",
          "📡 Sub: <b>", subscription, "</b>",
          "</div>"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors   = c(COL_TEAL, COL_GOLD, COL_RED),
        labels   = c("Phase 1 (2026–27)", "Phase 2 (2028–30)", "Phase 3 (2030+)"),
        title    = "Expansion Phase",
        opacity  = 0.9
      )
  })
  
  # ── Countries Table ────────────────────────────────────────
  output$tbl_countries <- renderDT({
    df <- countries[, c("country","phase","device_price","subscription")]
    datatable(df,
              colnames  = c("Country", "Phase", "Device Price", "Subscription"),
              rownames  = FALSE,
              options   = list(
                dom        = "ftp",
                pageLength = 6,
                scrollX    = TRUE
              )
    ) %>%
      formatStyle("phase",
                  color = styleEqual(
                    c("Phase 1 (2026–27)", "Phase 2 (2028–30)", "Phase 3 (2030+)"),
                    c(COL_TEAL, COL_GOLD, COL_RED)
                  ),
                  fontWeight = "bold"
      )
  })
}

# ── RUN ─────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
