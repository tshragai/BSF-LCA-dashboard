# global.R

library(googledrive)
library(googlesheets4)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(shinyWidgets)
library(ggplot2)
library(scales)

# ── Helpers ────────────────────────────────────────────────────────────────────

parse_lca_file <- function(drive_file) {
  tmp <- tempfile(fileext = ".xlsx")
  drive_download(drive_file, path = tmp, overwrite = TRUE, verbose = FALSE)

  parts      <- str_match(drive_file$name, "BSFfarmLCA_(\\d{2})_(\\d{4})")
  month_year <- paste0(parts[2], "-", parts[3])

  df     <- read_excel(tmp, sheet = "Daily Log (HERI log)", col_names = FALSE)
  ms_row <- which(sapply(df[[1]], function(x) !is.na(x) && trimws(as.character(x)) == "Monthly Sum"))

  tibble(
    month_year             = month_year,
    kg_waste_processed     = as.numeric(df[[3]][ms_row]),
    kg_larvae_produced     = as.numeric(df[[13]][ms_row]),
    kg_fertilizer_produced = as.numeric(df[[16]][ms_row])
  )
}

add_calculated_cols <- function(df) {
  df |> mutate(
    date                        = as.Date(paste0("01-", month_year), format = "%d-%m-%Y"),
    conventional_co2_waste      = kg_waste_processed     * 0.9,
    conventional_co2_feed       = kg_larvae_produced     * 1.4,
    conventional_co2_fertilizer = kg_fertilizer_produced * 2.15,
    bsf_co2_waste               = kg_waste_processed     * 0.025,
    bsf_co2_feed                = kg_larvae_produced     * 0.52,
    bsf_co2_fertilizer          = 0,
    averted_co2_waste           = conventional_co2_waste      - bsf_co2_waste,
    averted_co2_feed            = conventional_co2_feed       - bsf_co2_feed,
    averted_co2_fertilizer      = conventional_co2_fertilizer - bsf_co2_fertilizer
  )
}

# ── Eggs helper ────────────────────────────────────────────────────────────────

eggs_sheet_id <- "1m98JnZ0MElWLkJ0104lg_bsAxJbkrei4MpGIS-rCwqQ"

read_eggs_monthly <- function(sheet_id) {
  process_tab <- function(tab_name, egg_col_idx) {
    tab_year <- as.integer(regmatches(tab_name, regexpr("\\d{4}", tab_name)))
    df   <- read_sheet(sheet_id, sheet = tab_name)
    tibble(
      date = as.Date(df[[1]]),
      eggs = suppressWarnings(as.numeric(df[[egg_col_idx]]))
    ) |>
      filter(!is.na(date), !is.na(eggs)) |>
      mutate(
        date       = as.Date(format(date, paste0(tab_year, "-%m-%d"))),
        month_year = format(date, "%m-%Y")
      ) |>
      group_by(month_year) |>
      summarise(monthly_eggs = sum(eggs, na.rm = TRUE), .groups = "drop")
  }
  bind_rows(
    process_tab("Daily Log (HERI log)(2026)", 21),  # col U
    process_tab("Daily Log (HERI log)(2025)", 20)   # col T
  )
}

# ── Data loading ───────────────────────────────────────────────────────────────
# Tries to sync from Google Drive. If auth fails (e.g. on shinyapps.io),
# falls back silently to the bundled outputs/lca_data.csv.

tryCatch({
  drive_auth(cache = ".secrets", email = "tshragai@stanford.edu",
             scopes = c("https://www.googleapis.com/auth/drive",
                        "https://www.googleapis.com/auth/spreadsheets"))
  gs4_auth(token = drive_token())

  drive_files <- drive_find(
    q = c("'1DOguXcDo9dGf37XjlciUMkENQV00XmoU' in parents", "trashed = false"),
    pattern                   = "BSFfarmLCA_",
    includeItemsFromAllDrives = TRUE,
    supportsAllDrives         = TRUE
  )

  # ── Load cached metadata and raw data ────────────────────────────────────
  meta_path  <- "outputs/file_metadata.rds"
  cached_meta <- if (file.exists(meta_path)) readRDS(meta_path) else setNames(list(), character(0))

  cached_raw <- if (file.exists("outputs/lca_data.csv")) {
    read.csv("outputs/lca_data.csv") |>
      select(month_year, kg_waste_processed, kg_larvae_produced, kg_fertilizer_produced)
  } else {
    tibble(month_year = character(), kg_waste_processed = numeric(),
           kg_larvae_produced = numeric(), kg_fertilizer_produced = numeric())
  }

  # ── Compare Drive modifiedTime against cache ──────────────────────────────
  current_meta <- setNames(
    vapply(seq_len(nrow(drive_files)),
           \(i) as.character(drive_files$drive_resource[[i]]$modifiedTime),
           character(1)),
    drive_files$name
  )

  needs_download <- names(current_meta)[vapply(names(current_meta), \(n) {
    is.null(cached_meta[[n]]) || cached_meta[[n]] != current_meta[[n]]
  }, logical(1))]

  # ── Download only new or modified files ───────────────────────────────────
  if (length(needs_download) > 0) {
    message("Downloading ", length(needs_download), " new/modified file(s) from Drive...")
    dl_files   <- drive_files[drive_files$name %in% needs_download, ]
    fresh_data <- map_dfr(seq_len(nrow(dl_files)), \(i) parse_lca_file(dl_files[i, ]))
  } else {
    message("All Drive files up to date — using cached data.")
    fresh_data <- tibble(month_year = character(), kg_waste_processed = numeric(),
                         kg_larvae_produced = numeric(), kg_fertilizer_produced = numeric())
  }

  # ── Pull unchanged rows from the cached CSV ───────────────────────────────
  unchanged_my <- map_chr(
    setdiff(drive_files$name, needs_download),
    \(n) { m <- str_match(n, "BSFfarmLCA_(\\d{2})_(\\d{4})"); paste0(m[2], "-", m[3]) }
  )
  cached_subset <- cached_raw |> filter(month_year %in% unchanged_my)

  # ── Combine, compute, filter ──────────────────────────────────────────────
  lca_data <- bind_rows(fresh_data, cached_subset) |>
    add_calculated_cols() |>
    filter(date >= as.Date("2025-07-01")) |>
    arrange(date)

  message("Reading egg counts from daily log...")
  eggs_data <- tryCatch(
    read_eggs_monthly(eggs_sheet_id),
    error = function(e) {
      message("Could not read egg data: ", conditionMessage(e))
      tibble(month_year = character(), monthly_eggs = NA_real_)
    }
  )
  lca_data <- lca_data |> left_join(eggs_data, by = "month_year")

  write.csv(lca_data, "outputs/lca_data.csv", row.names = FALSE)
  saveRDS(current_meta, meta_path)
  message("outputs/lca_data.csv and file_metadata.rds updated.")

}, error = function(e) {
  message("Drive sync unavailable (", conditionMessage(e), ") — loading from outputs/lca_data.csv")
  df <- read.csv("outputs/lca_data.csv")
  lca_data <<- df |>
    select(month_year, kg_waste_processed, kg_larvae_produced, kg_fertilizer_produced) |>
    add_calculated_cols() |>
    filter(date >= as.Date("2025-07-01")) |>
    arrange(date) |>
    left_join(
      if ("monthly_eggs" %in% names(df)) select(df, month_year, monthly_eggs) else tibble(month_year = character(), monthly_eggs = NA_real_),
      by = "month_year"
    )
})

# ── Lifetime totals ────────────────────────────────────────────────────────────

total_waste_processed         <- sum(lca_data$kg_waste_processed,     na.rm = TRUE)
total_larvae_produced         <- sum(lca_data$kg_larvae_produced,      na.rm = TRUE)
total_fertilizer_produced     <- sum(lca_data$kg_fertilizer_produced,  na.rm = TRUE)

total_co2_averted_waste       <- sum(lca_data$averted_co2_waste,       na.rm = TRUE)
total_co2_averted_feed        <- sum(lca_data$averted_co2_feed,        na.rm = TRUE)
total_co2_averted_fertilizer  <- sum(lca_data$averted_co2_fertilizer,  na.rm = TRUE)

total_co2_averted             <- total_co2_averted_waste + total_co2_averted_feed + total_co2_averted_fertilizer

saveRDS(
  list(
    total_waste_processed        = total_waste_processed,
    total_larvae_produced        = total_larvae_produced,
    total_fertilizer_produced    = total_fertilizer_produced,
    total_co2_averted_waste      = total_co2_averted_waste,
    total_co2_averted_feed       = total_co2_averted_feed,
    total_co2_averted_fertilizer = total_co2_averted_fertilizer,
    total_co2_averted            = total_co2_averted
  ),
  "outputs/totals.rds"
)

# Month choices for dropdowns (chronological, used in ui.R)
month_choices <- setNames(
  format(lca_data$date, "%Y-%m-%d"),
  format(lca_data$date, "%B %Y")
)

# ── Color palette ──────────────────────────────────────────────────────────────
col_waste      <- col_co2_waste  <- "#B6843D"  # Warm brown/gold
col_larvae     <- col_co2_feed   <- "#6B7A60"  # Muted sage green (darkened)
col_fertilizer <- col_co2_fert   <- "#EA5137"  # Coral red
col_eggs                         <- "#D4A843"  # Warm golden yellow
col_co2_total                    <- "#3B7A57"  # Forest green (total CO₂ averted)

# ── Charts ─────────────────────────────────────────────────────────────────────

theme_dash <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      panel.border       = element_rect(color = "#dddddd", fill = NA, linewidth = 0.8),
      panel.grid.major.y = element_line(color = "#f0f0f0", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(color = "#555555", size = 12),
      axis.title         = element_text(color = "#333333", size = 13, face = "bold"),
      plot.title         = element_text(color = "#111111", size = 14, face = "bold",
                                        hjust = 0, lineheight = 1.3, margin = margin(b = 12)),
      plot.margin        = margin(16, 20, 12, 16)
    )
}

make_bar_chart <- function(data, y_col, title, fill_color, y_label = "KG") {
  plot_data <- data |> filter(!is.na(.data[[y_col]]))

  ggplot(plot_data, aes(x = reorder(format(date, "%b '%y"), date), y = .data[[y_col]])) +
    geom_col(fill = fill_color, color = "#555555", alpha = 0.8, width = 0.7) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, x = "Month", y = y_label) +
    theme_dash() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

make_cumulative_chart <- function(data, y_col, title, line_color, y_label = "KG") {
  plot_data <- data |>
    filter(!is.na(.data[[y_col]])) |>
    arrange(date) |>
    mutate(
      cumval      = cumsum(.data[[y_col]]),
      month_label = reorder(format(date, "%b '%y"), date)
    )

  ggplot(plot_data, aes(x = month_label, y = cumval, group = 1)) +
    geom_area(fill = line_color, alpha = 0.15) +
    geom_line(color = line_color, linewidth = 1.4) +
    geom_point(color = line_color, fill = "white", shape = 21, size = 3.5, stroke = 2) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
    labs(title = title, x = "Month", y = y_label) +
    theme_dash() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

make_comparison_chart <- function(data, conv_col, bsf_col, title, bsf_color) {
  plot_data <- data |>
    select(date, Conventional = all_of(conv_col), BSF = all_of(bsf_col)) |>
    pivot_longer(-date, names_to = "type", values_to = "value") |>
    filter(!is.na(value)) |>
    mutate(
      type        = factor(type, levels = c("Conventional", "BSF")),
      month_label = reorder(format(date, "%b '%y"), date)
    )

  ggplot(plot_data, aes(x = month_label, y = value, fill = type)) +
    geom_col(position = position_dodge(width = 0.8), color = "#555555", alpha = 0.8, width = 0.38) +
    scale_fill_manual(values = c("Conventional" = "#7A9BB5", "BSF" = bsf_color)) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, x = "Month", y = "KG CO\u2082", fill = NULL) +
    theme_dash() +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      legend.text     = element_text(size = 12)
    )
}
