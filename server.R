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

  # ── CSV download ────────────────────────────────────────────────────────────
  output$dl_csv <- downloadHandler(
    filename = function() {
      if (input$active_tab == "Farm Production") {
        s <- format(as.Date(input$start1), "%b%Y")
        e <- format(as.Date(input$end1),   "%b%Y")
      } else {
        s <- format(as.Date(input$start2), "%b%Y")
        e <- format(as.Date(input$end2),   "%b%Y")
      }
      paste0("lca_data_", s, "_to_", e, ".csv")
    },
    content = function(file) {
      data <- if (input$active_tab == "Farm Production") dat1() else dat2()
      write.csv(data, file, row.names = FALSE)
    }
  )

  # ── Tab 1: stat card labels ─────────────────────────────────────────────────
  output$lbl_waste1      <- renderText(paste0("Total waste processed, ",      lbl1()))
  output$lbl_larvae1     <- renderText(paste0("Total larvae produced, ",       lbl1()))
  output$lbl_fertilizer1 <- renderText(paste0("Total fertilizer produced, ",   lbl1()))

  # ── Tab 1: stat card values ─────────────────────────────────────────────────
  output$val_waste1      <- renderText(paste0(comma(sum(dat1()$kg_waste_processed,    na.rm=TRUE), accuracy=1), " kg"))
  output$val_larvae1     <- renderText(paste0(comma(sum(dat1()$kg_larvae_produced,     na.rm=TRUE), accuracy=1), " kg"))
  output$val_fertilizer1 <- renderText(paste0(comma(sum(dat1()$kg_fertilizer_produced, na.rm=TRUE), accuracy=1), " kg"))

  # ── Tab 1: plots ────────────────────────────────────────────────────────────
  output$plot_waste <- renderPlot({
    if (input$view_mode1 == "monthly") {
      make_bar_chart(dat1(), "kg_waste_processed", "Monthly waste processed", col_waste)
    } else {
      make_cumulative_chart(dat1(), "kg_waste_processed", "Cumulative waste processed", col_waste)
    }
  }, res = 96)
  output$plot_larvae <- renderPlot({
    if (input$view_mode1 == "monthly") {
      make_bar_chart(dat1(), "kg_larvae_produced", "Monthly larvae produced", col_larvae)
    } else {
      make_cumulative_chart(dat1(), "kg_larvae_produced", "Cumulative larvae produced", col_larvae)
    }
  }, res = 96)
  output$plot_fertilizer <- renderPlot({
    if (input$view_mode1 == "monthly") {
      make_bar_chart(dat1(), "kg_fertilizer_produced", "Monthly fertilizer produced", col_fertilizer)
    } else {
      make_cumulative_chart(dat1(), "kg_fertilizer_produced", "Cumulative fertilizer produced", col_fertilizer)
    }
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
                     col_co2_waste, y_label = "KG CO\u2082")
    } else if (input$chart_mode == "comparison") {
      make_comparison_chart(dat2(), "conventional_co2_waste", "bsf_co2_waste",
                            "Monthly CO\u2082: waste\n(conventional vs BSF)", col_co2_waste)
    } else {
      make_cumulative_chart(dat2(), "averted_co2_waste",
                            "Cumulative CO\u2082 avoided via\nwaste averted from landfill",
                            col_co2_waste, y_label = "KG CO\u2082")
    }
  }, res = 96)

  output$plot_co2_feed <- renderPlot({
    if (input$chart_mode == "averted") {
      make_bar_chart(dat2(), "averted_co2_feed",
                     "Monthly CO\u2082 avoided via\nanimal feed not imported",
                     col_co2_feed, y_label = "KG CO\u2082")
    } else if (input$chart_mode == "comparison") {
      make_comparison_chart(dat2(), "conventional_co2_feed", "bsf_co2_feed",
                            "Monthly CO\u2082: feed\n(conventional vs BSF)", col_co2_feed)
    } else {
      make_cumulative_chart(dat2(), "averted_co2_feed",
                            "Cumulative CO\u2082 avoided via\nanimal feed not imported",
                            col_co2_feed, y_label = "KG CO\u2082")
    }
  }, res = 96)

  output$plot_co2_fertilizer <- renderPlot({
    if (input$chart_mode == "averted") {
      make_bar_chart(dat2(), "averted_co2_fertilizer",
                     "Monthly CO\u2082 avoided via\nchemical fertilizer not used",
                     col_co2_fert, y_label = "KG CO\u2082")
    } else if (input$chart_mode == "comparison") {
      make_comparison_chart(dat2(), "conventional_co2_fertilizer", "bsf_co2_fertilizer",
                            "Monthly CO\u2082: fertilizer\n(conventional vs BSF)", col_co2_fert)
    } else {
      make_cumulative_chart(dat2(), "averted_co2_fertilizer",
                            "Cumulative CO\u2082 avoided via\nchemical fertilizer not used",
                            col_co2_fert, y_label = "KG CO\u2082")
    }
  }, res = 96)

}
