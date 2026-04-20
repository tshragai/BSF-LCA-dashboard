# server.R

server <- function(input, output, session) {

  # ── Filtered data ───────────────────────────────────────────────────────────
  dat1 <- reactive({
    lca_data |> filter(date >= as.Date(input$start1), date <= as.Date(input$end1))
  })

  dat2 <- reactive({
    lca_data |> filter(date >= as.Date(input$start2), date <= as.Date(input$end2))
  })

  # ── Date range labels ───────────────────────────────────────────────────────
  range_label <- function(start_input, end_input) {
    is_all <- start_input == month_choices[1] &&
              end_input   == month_choices[length(month_choices)]
    if (is_all) return("farm lifetime")
    s <- format(as.Date(start_input), "%B %Y")
    e <- format(as.Date(end_input),   "%B %Y")
    if (s == e) s else paste0(s, " \u2013 ", e)
  }

  lbl1 <- reactive({ range_label(input$start1, input$end1) })
  lbl2 <- reactive({ range_label(input$start2, input$end2) })

  # ── Tab 1: stat card labels ─────────────────────────────────────────────────
  output$lbl_waste1      <- renderText(paste0("Total farm waste processed, ",      lbl1()))
  output$lbl_larvae1     <- renderText(paste0("Total farm larvae produced, ",       lbl1()))
  output$lbl_fertilizer1 <- renderText(paste0("Total farm fertilizer produced, ",   lbl1()))

  # ── Tab 1: stat card values ─────────────────────────────────────────────────
  output$val_waste1      <- renderText(paste0(comma(sum(dat1()$kg_waste_processed,    na.rm=TRUE), accuracy=1), " kg"))
  output$val_larvae1     <- renderText(paste0(comma(sum(dat1()$kg_larvae_produced,     na.rm=TRUE), accuracy=1), " kg"))
  output$val_fertilizer1 <- renderText(paste0(comma(sum(dat1()$kg_fertilizer_produced, na.rm=TRUE), accuracy=1), " kg"))

  # ── Tab 1: plots ────────────────────────────────────────────────────────────
  output$plot_waste <- renderPlot({
    make_bar_chart(dat1(), "kg_waste_processed", "Monthly waste processed", "#4a7ca3")
  }, res = 96)
  output$plot_larvae <- renderPlot({
    make_bar_chart(dat1(), "kg_larvae_produced", "Monthly larvae produced", "#2d5a27")
  }, res = 96)
  output$plot_fertilizer <- renderPlot({
    make_bar_chart(dat1(), "kg_fertilizer_produced", "Monthly fertilizer produced", "#8b4513")
  }, res = 96)

  # ── Tab 2: stat card labels ─────────────────────────────────────────────────
  output$lbl_co2_waste2      <- renderText(paste0("Total CO\u2082 avoided via waste averted from landfill, ",   lbl2()))
  output$lbl_co2_feed2       <- renderText(paste0("Total CO\u2082 avoided via animal feed not imported, ",       lbl2()))
  output$lbl_co2_fertilizer2 <- renderText(paste0("Total CO\u2082 avoided via chemical fertilizer not used, ",  lbl2()))

  # ── Tab 2: grand total ──────────────────────────────────────────────────────
  output$val_co2_grand <- renderText({
    d     <- dat2()
    total <- sum(d$averted_co2_waste, na.rm=TRUE) +
             sum(d$averted_co2_feed,  na.rm=TRUE) +
             sum(d$averted_co2_fertilizer, na.rm=TRUE)
    paste0("Total CO\u2082 Emissions Avoided, ", lbl2(), ": ", comma(total, accuracy=1), " kg")
  })

  # ── Tab 2: stat card values ─────────────────────────────────────────────────
  output$val_co2_waste2      <- renderText(paste0(comma(sum(dat2()$averted_co2_waste,       na.rm=TRUE), accuracy=1), " kg"))
  output$val_co2_feed2       <- renderText(paste0(comma(sum(dat2()$averted_co2_feed,        na.rm=TRUE), accuracy=1), " kg"))
  output$val_co2_fertilizer2 <- renderText(paste0(comma(sum(dat2()$averted_co2_fertilizer,  na.rm=TRUE), accuracy=1), " kg"))

  # ── Tab 2: plots ────────────────────────────────────────────────────────────
  output$plot_co2_waste <- renderPlot({
    if (input$chart_mode == "averted") {
      make_bar_chart(dat2(), "averted_co2_waste",
                     "Monthly CO\u2082 avoided via\nwaste averted from landfill",
                     "#b07ec8", y_label = "KG CO\u2082")
    } else {
      make_comparison_chart(dat2(), "conventional_co2_waste", "bsf_co2_waste",
                            "Monthly CO\u2082: waste\n(conventional vs BSF)", "#b07ec8")
    }
  }, res = 96)

  output$plot_co2_feed <- renderPlot({
    if (input$chart_mode == "averted") {
      make_bar_chart(dat2(), "averted_co2_feed",
                     "Monthly CO\u2082 avoided via\nanimal feed not imported",
                     "#2d4a7a", y_label = "KG CO\u2082")
    } else {
      make_comparison_chart(dat2(), "conventional_co2_feed", "bsf_co2_feed",
                            "Monthly CO\u2082: feed\n(conventional vs BSF)", "#2d4a7a")
    }
  }, res = 96)

  output$plot_co2_fertilizer <- renderPlot({
    if (input$chart_mode == "averted") {
      make_bar_chart(dat2(), "averted_co2_fertilizer",
                     "Monthly CO\u2082 avoided via\nchemical fertilizer not used",
                     "#e09b00", y_label = "KG CO\u2082")
    } else {
      make_comparison_chart(dat2(), "conventional_co2_fertilizer", "bsf_co2_fertilizer",
                            "Monthly CO\u2082: fertilizer\n(conventional vs BSF)", "#e09b00")
    }
  }, res = 96)

}
