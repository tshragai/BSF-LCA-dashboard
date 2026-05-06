# ui.R

stat_card <- function(label_id, value_id, value_color) {
  div(class = "stat-card",
    div(class = "stat-label", textOutput(label_id, inline = TRUE)),
    div(class = "stat-value", style = paste0("color: ", value_color, ";"),
      textOutput(value_id, inline = TRUE))
  )
}

chart_card <- function(...) {
  div(class = "chart-card", ...)
}

date_selector <- function(start_id, end_id) {
  fluidRow(
    column(6, selectInput(start_id, "From:", choices = month_choices,
                          selected = month_choices[1])),
    column(6, selectInput(end_id,   "To:",   choices = month_choices,
                          selected = month_choices[length(month_choices)]))
  )
}

ui <- navbarPage(
  id    = "active_tab",
  title = tags$span(
    tags$img(src = "HERI-logo-FINAL-RGB.webp", height = "120px",
             style = "margin-right: 16px; vertical-align: middle;"),
    "Black Soldier Fly (BSF) Farm Life Cycle Analysis (LCA) Dashboard"
  ),

  tags$head(
    tags$style(HTML("

      /* ── Base ── */
      body, .navbar, h1, h2, h3, h4, label, .selectize-input {
        font-family: 'Avenir Next', 'Avenir', 'Nunito Sans', sans-serif !important;
      }
      body { background-color: #f8f7f5; }

      /* ── Navbar ── */
      .navbar {
        background-color: #ffffff !important;
        border-bottom: 2px solid #F6AE2D;
        box-shadow: 0 2px 10px rgba(0,0,0,0.06);
        min-height: 150px;
      }
      .navbar-brand {
        font-size: 28px; font-weight: 700; color: #111 !important;
        letter-spacing: -0.3px; padding-top: 20px; padding-bottom: 20px;
      }
      .navbar-nav > li > a {
        padding-top: 55px !important; padding-bottom: 20px !important;
        font-size: 16px !important;
      }
      .nav > li > a {
        font-size: 14px; font-weight: 600; color: #444 !important;
        letter-spacing: 0.1px;
      }
      .nav > li.active > a, .nav > li.active > a:focus {
        color: #111 !important; background: transparent !important;
        border-bottom: 2px solid #111;
      }

      /* ── Section titles ── */
      .section-title {
        font-size: 26px; font-weight: 700; color: #111;
        text-align: center; padding: 24px 0 20px;
        letter-spacing: -0.4px;
      }
      /* ── Hero card (CO₂ grand total) ── */
      .hero-card {
        background: #ffffff;
        border-radius: 14px;
        box-shadow: 0 2px 14px rgba(0,0,0,0.07);
        text-align: center;
        padding: 40px 24px 44px;
        margin: 0 6px 24px;
        border-top: 4px solid #0081A7;
      }
      .hero-label {
        font-size: 18px; font-weight: 600; color: #666;
        margin-bottom: 16px; letter-spacing: 0.1px;
      }
      .hero-value {
        font-size: 64px; font-weight: 700; color: #0081A7;
        line-height: 1; margin: 0;
      }

      /* ── Breakdown section divider ── */
      .breakdown-title {
        font-size: 20px; font-weight: 700; color: #444;
        text-align: center; padding: 16px 0 20px;
        letter-spacing: -0.3px;
        border-top: 1px solid #e8e4df;
        margin: 8px 6px 0;
      }

      /* ── Stat cards ── */
      .stat-row.row { display: flex; flex-wrap: wrap; }
      .stat-row.row > [class*='col-'] { display: flex; flex-direction: column; }
      .stat-card {
        flex: 1;
        background: #ffffff;
        border-radius: 14px;
        box-shadow: 0 2px 14px rgba(0,0,0,0.07);
        text-align: center;
        padding: 28px 20px 32px;
        margin: 0 6px 20px;
      }
      .stat-label {
        font-size: 16px; font-weight: 600; color: #666;
        margin-bottom: 14px; line-height: 1.5;
      }
      .stat-value { font-size: 32px; font-weight: 700; margin: 0; line-height: 1; }

      /* ── Chart cards ── */
      .chart-card {
        background: #ffffff;
        border-radius: 14px;
        box-shadow: 0 2px 14px rgba(0,0,0,0.07);
        padding: 8px 4px 4px;
        margin: 0 4px 16px;
      }

      /* ── Toggles ── */
      .toggle-wrap { text-align: center; padding: 8px 0 28px; }
      .toggle-wrap .btn-group {
        border-radius: 50px;
        overflow: hidden;
        box-shadow: 0 1px 6px rgba(0,0,0,0.12);
      }
      .toggle-wrap .btn-group > .btn {
        border-radius: 0 !important;
        font-family: 'Avenir Next', 'Avenir', 'Nunito Sans', sans-serif !important;
        font-weight: 600; font-size: 13px;
        padding: 9px 24px; border: none;
        transition: background 0.15s, color 0.15s;
      }
      .toggle-wrap .btn-group > .btn:first-child {
        border-radius: 50px 0 0 50px !important;
      }
      .toggle-wrap .btn-group > .btn:last-child  {
        border-radius: 0 50px 50px 0 !important;
      }
      .toggle-wrap .btn-default           { background: #ffffff; color: #555; }
      .toggle-wrap .btn-default.active,
      .toggle-wrap .btn-default:active    { background: #222; color: #ffffff; box-shadow: none; }

      /* ── Date selector ── */
      .date-select { padding: 20px 12px 4px; }

      /* ── Download button ── */
      #dl_csv {
        position: fixed; top: 11px; right: 20px; z-index: 9999;
        background: #222; color: white; border: none;
        font-family: 'Avenir Next', 'Avenir', 'Nunito Sans', sans-serif !important;
        font-weight: 600; font-size: 13px;
        padding: 7px 18px; border-radius: 50px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.18);
        transition: background 0.15s;
      }
      #dl_csv:hover { background: #444; }

      /* ── Info box ── */
      .info-box {
        margin: 8px 8px 24px;
        padding: 20px 28px;
        border-left: 4px solid #F6AE2D;
        color: #777; font-size: 14px; font-style: italic;
        background: #ffffff;
        border-radius: 0 14px 14px 0;
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
      }

    "))
  ),

  # ── Tab 1: Farm Production ──────────────────────────────────────────────────
  tabPanel("Farm Production",
    div(class = "date-select", date_selector("start1", "end1")),
    div(class = "section-title", "Farm Production"),
    fluidRow(class = "stat-row",
      column(3, stat_card("lbl_waste1",      "val_waste1",      col_waste)),
      column(3, stat_card("lbl_larvae1",     "val_larvae1",     col_larvae)),
      column(3, stat_card("lbl_fertilizer1", "val_fertilizer1", col_fertilizer)),
      column(3, stat_card("lbl_eggs1",       "val_eggs1",       col_eggs))
    ),
    div(class = "toggle-wrap",
      radioGroupButtons(
        inputId  = "view_mode1",
        label    = NULL,
        choices  = c("Monthly" = "monthly", "Cumulative" = "cumulative"),
        selected = "monthly",
        status   = "default"
      )
    ),
    fluidRow(
      column(3, chart_card(plotOutput("plot_waste",      height = "400px"))),
      column(3, chart_card(plotOutput("plot_larvae",     height = "400px"))),
      column(3, chart_card(plotOutput("plot_fertilizer", height = "400px"))),
      column(3, chart_card(plotOutput("plot_eggs",       height = "400px")))
    )
  ),

  # ── Download button (fixed) ─────────────────────────────────────────────────
  tags$div(downloadButton("dl_csv", "Download CSV")),

  # ── Tab 2: CO₂ Emissions Averted ───────────────────────────────────────────
  tabPanel("CO\u2082 Emissions Averted",
    div(class = "date-select", date_selector("start2", "end2")),
    div(class = "section-title", "CO\u2082 Emissions Averted"),
    div(class = "toggle-wrap",
      radioGroupButtons(
        inputId  = "co2_threshold",
        label    = NULL,
        choices  = c("Lower Threshold" = "lower", "Mean (Default)" = "mean", "Upper Threshold" = "upper"),
        selected = "mean",
        status   = "default"
      )
    ),

    # \u2500\u2500 Block 1: Hero total + cumulative chart \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    fluidRow(
      column(12,
        div(class = "hero-card",
          div(class = "hero-label", textOutput("lbl_co2_hero")),
          div(class = "hero-value", textOutput("val_co2_grand"))
        )
      )
    ),
    div(class = "toggle-wrap",
      radioGroupButtons(
        inputId  = "chart_mode_total",
        label    = NULL,
        choices  = c("Cumulative CO₂ Averted" = "cumulative",
                     "Monthly CO₂ Averted"    = "averted",
                     "Conventional vs BSF"          = "comparison"),
        selected = "cumulative",
        status   = "default"
      )
    ),
    fluidRow(
      column(12, chart_card(plotOutput("plot_co2_total", height = "320px")))
    ),

    # \u2500\u2500 Block 2: Breakdown by category \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    div(class = "breakdown-title", "Breakdown by Category"),
    fluidRow(class = "stat-row",
      column(4, stat_card("lbl_co2_waste2",     "val_co2_waste2",      col_co2_waste)),
      column(4, stat_card("lbl_co2_feed2",       "val_co2_feed2",       col_co2_feed)),
      column(4, stat_card("lbl_co2_fertilizer2", "val_co2_fertilizer2", col_co2_fert))
    ),
    div(class = "toggle-wrap",
      radioGroupButtons(
        inputId  = "chart_mode",
        label    = NULL,
        choices  = c("Monthly CO\u2082 Averted" = "averted",
                     "Conventional vs BSF" = "comparison",
                     "Cumulative CO\u2082 Averted" = "cumulative"),
        selected = "averted",
        status   = "default"
      )
    ),
    fluidRow(
      column(4, chart_card(plotOutput("plot_co2_waste",     height = "400px"))),
      column(4, chart_card(plotOutput("plot_co2_feed",       height = "400px"))),
      column(4, chart_card(plotOutput("plot_co2_fertilizer", height = "400px")))
    ),
    div(class = "info-box",
      "CO\u2082-equivalent values were calculated using a gate-to-gate Life Cycle Assessment (LCA) framework covering organic waste collection, black soldier fly (BSF) bioconversion, and processing into larvae and frass. Emission factors were sourced from established databases and models (e.g., EPA WARM, GREET, and Ecoinvent) and applied to defined functional units based on observed production data. Averted emissions were estimated through system expansion by comparing BSF outputs to conventional landfill disposal, imported animal feed, and synthetic fertilizer production. Conventional CO\u2082 emission factors can be toggled between lower threshold (0.6, 1.1, 1.2 kg CO\u2082/kg for waste, feed, and fertilizer respectively), mean (0.9, 1.4, 2.15), and upper threshold (1.2, 1.7, 3.1) estimates from the source literature. Mean values are shown by default."
    )
  )
)
