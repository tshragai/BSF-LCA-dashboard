# global.R

library(googledrive)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(shinyWidgets)
library(ggplot2)
library(scales)

# Authenticate with Google Drive (opens browser on first run)
drive_auth()

# ── Helpers ────────────────────────────────────────────────────────────────────

read_cell <- function(path, cell) {
  read_excel(path, sheet = "Overview", range = cell, col_names = FALSE)[[1]][[1]]
}

parse_lca_file <- function(drive_file) {
  tmp <- tempfile(fileext = ".xlsx")
  drive_download(drive_file, path = tmp, overwrite = TRUE, verbose = FALSE)

  parts      <- str_match(drive_file$name, "BSFfarmLCA_(\\d{2})_(\\d{4})")
  month_year <- paste0(parts[2], "-", parts[3])

  tibble(
    month_year                  = month_year,
    kg_waste_processed          = as.numeric(read_cell(tmp, "B9")),
    kg_larvae_produced          = as.numeric(read_cell(tmp, "B10")),
    kg_fertilizer_produced      = as.numeric(read_cell(tmp, "B11")),
    conventional_co2_waste      = as.numeric(read_cell(tmp, "C34")),
    bsf_co2_waste               = as.numeric(read_cell(tmp, "C22")),
    conventional_co2_feed       = as.numeric(read_cell(tmp, "E34")),
    bsf_co2_feed                = as.numeric(read_cell(tmp, "D22")),
    conventional_co2_fertilizer = as.numeric(read_cell(tmp, "G34")),
    bsf_co2_fertilizer          = as.numeric(read_cell(tmp, "E22"))
  )
}

add_calculated_cols <- function(df) {
  df |> mutate(
    date                   = as.Date(paste0("01-", month_year), format = "%d-%m-%Y"),
    averted_co2_waste      = conventional_co2_waste      - bsf_co2_waste,
    averted_co2_feed       = conventional_co2_feed       - bsf_co2_feed,
    averted_co2_fertilizer = conventional_co2_fertilizer - bsf_co2_fertilizer
  )
}

# ── Smart data loading ─────────────────────────────────────────────────────────

# Months already saved locally
existing_months <- if (file.exists("outputs/lca_data.csv")) {
  read.csv("outputs/lca_data.csv")$month_year
} else {
  character(0)
}

# All files in Drive
drive_files <- drive_find(
  q = c("'1DOguXcDo9dGf37XjlciUMkENQV00XmoU' in parents", "trashed = false"),
  pattern                   = "BSFfarmLCA_",
  includeItemsFromAllDrives = TRUE,
  supportsAllDrives         = TRUE
)

# Which Drive files represent months not yet in the local CSV
drive_months <- str_match(drive_files$name, "BSFfarmLCA_(\\d{2})_(\\d{4})")
drive_months  <- paste0(drive_months[, 2], "-", drive_months[, 3])
new_files     <- drive_files[!drive_months %in% existing_months, ]

if (nrow(new_files) > 0) {
  message("Downloading ", nrow(new_files), " new file(s) from Drive...")

  new_rows <- map_dfr(seq_len(nrow(new_files)), \(i) parse_lca_file(new_files[i, ])) |>
    add_calculated_cols()

  lca_data <- if (length(existing_months) > 0) {
    read.csv("outputs/lca_data.csv") |>
      mutate(date = as.Date(date)) |>
      bind_rows(new_rows) |>
      arrange(date)
  } else {
    new_rows |> arrange(date)
  }

  write.csv(lca_data, "outputs/lca_data.csv", row.names = FALSE)
  message("outputs/lca_data.csv updated.")

} else {
  message("No new Drive files — loading from outputs/lca_data.csv")
  lca_data <- read.csv("outputs/lca_data.csv") |> mutate(date = as.Date(date))
}

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

# ── Charts ─────────────────────────────────────────────────────────────────────

theme_dash <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      panel.border       = element_rect(color = "#333333", fill = NA, linewidth = 0.8),
      panel.grid.major.y = element_line(color = "#eeeeee", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(color = "#333333", size = 12),
      axis.title         = element_text(color = "#111111", size = 14, face = "bold"),
      plot.title         = element_text(color = "#111111", size = 15, hjust = 0.5,
                                        lineheight = 1.2, margin = margin(b = 10)),
      plot.margin        = margin(12, 16, 12, 16)
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
    scale_fill_manual(values = c("Conventional" = "#aaaaaa", "BSF" = bsf_color)) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, x = "Month", y = "KG CO\u2082", fill = NULL) +
    theme_dash() +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      legend.text     = element_text(size = 12)
    )
}
