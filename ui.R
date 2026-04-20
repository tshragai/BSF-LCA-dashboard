# ui.R

stat_card <- function(label_id, value_id, value_color) {
  div(class = "stat-card",
    p(class = "stat-label", textOutput(label_id)),
    p(class = "stat-value", style = paste0("color: ", value_color, ";"),
      textOutput(value_id, inline = TRUE))
  )
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
  title = "BSF Farm LCA Dashboard",

  tags$head(tags$style(HTML("
    body           { background-color: #ffffff; font-family: sans-serif; }
    .navbar        { background-color: #ffffff !important; border-bottom: 2px solid #222; }
    .navbar-brand  { font-size: 24px; font-weight: 700; color: #111 !important; }
    .nav > li > a  { font-size: 15px; font-weight: 600; color: #333 !important; }

    .section-title { font-size: 26px; font-weight: 700; color: #111;
                     text-align: center; padding: 16px 0 12px; }

    .grand-total   { font-size: 22px; font-weight: 700; color: #111;
                     text-align: center; padding: 0 0 20px; }

    .stat-card     { text-align: center; padding: 10px 10px 24px; }
    .stat-label    { font-size: 15px; color: #333; margin-bottom: 4px; }
    .stat-value    { font-size: 30px; font-weight: 700; margin: 0; }

    .toggle-wrap   { text-align: center; padding-bottom: 16px; }

    .date-select   { padding: 0 20px 10px; }

    .info-box      { margin: 24px 16px 8px; padding: 16px 24px;
                     border-left: 4px solid #aaa; color: #555; font-size: 14px;
                     font-style: italic; background: #fafafa; }
  "))),

  # ── Tab 1: Farm Production ──────────────────────────────────────────────────
  tabPanel("Farm Production",
    div(class = "section-title", "Farm Production"),
    div(class = "date-select", date_selector("start1", "end1")),
    fluidRow(
      column(4, stat_card("lbl_waste1",      "val_waste1",      "#4a7ca3")),
      column(4, stat_card("lbl_larvae1",     "val_larvae1",     "#2d5a27")),
      column(4, stat_card("lbl_fertilizer1", "val_fertilizer1", "#8b4513"))
    ),
    fluidRow(
      column(4, plotOutput("plot_waste",      height = "420px")),
      column(4, plotOutput("plot_larvae",     height = "420px")),
      column(4, plotOutput("plot_fertilizer", height = "420px"))
    )
  ),

  # ── Tab 2: CO₂ Emissions Avoided ───────────────────────────────────────────
  tabPanel("CO\u2082 Emissions Avoided",
    div(class = "section-title", "CO\u2082 Emissions Avoided"),
    div(class = "date-select", date_selector("start2", "end2")),
    div(class = "grand-total", textOutput("val_co2_grand")),
    fluidRow(
      column(4, stat_card("lbl_co2_waste2",      "val_co2_waste2",      "#b07ec8")),
      column(4, stat_card("lbl_co2_feed2",        "val_co2_feed2",       "#2d4a7a")),
      column(4, stat_card("lbl_co2_fertilizer2",  "val_co2_fertilizer2", "#e09b00"))
    ),
    div(class = "toggle-wrap",
      radioGroupButtons(
        inputId  = "chart_mode",
        label    = NULL,
        choices  = c("KG CO\u2082 Avoided" = "averted",
                     "Conventional vs BSF" = "comparison"),
        selected = "averted",
        status   = "default"
      )
    ),
    fluidRow(
      column(4, plotOutput("plot_co2_waste",     height = "420px")),
      column(4, plotOutput("plot_co2_feed",       height = "420px")),
      column(4, plotOutput("plot_co2_fertilizer", height = "420px"))
    ),
    div(class = "info-box",
      "How CO\u2082 values are calculated: [Placeholder \u2014 add a description of your LCA
      methodology here, including system boundaries, emission factors, and data sources.]"
    )
  )
)
