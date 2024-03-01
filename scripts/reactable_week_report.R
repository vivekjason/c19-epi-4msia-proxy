library(reactable)
library(htmltools)
library(tidyverse)
library(shiny)
library(viridis)

#pull data
epid_report <- read.csv("data/epid_report.csv")

#prepare the columns for the epid report
trans_cols <- c("mean_ir", "perc_change_ir", "mean_mr", "perc_change_mr", "mean_rt_full", "perc_change_rt")
capacity_cols <- c("mean_bor", "trend_bor", "mean_ior", "trend_ior", "mean_vor", "trend_vor")

#all colims
epid_report <- epid_report[, c("state", trans_cols, capacity_cols)]

change_column <- function(maxWidth = 40, ...) {
  colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
}

number_column <- function(class = NULL, ...) {
  colDef(align = "center", class = paste("cell number", class), ...)
}

number_col_column <- function(class = NULL, ...) {
  colDef(align = "center", class = paste("cell number", class), 
         ...)
}

perc_column <- function(maxWidth = 50, class = NULL, ...) {
  colDef(
    align = "center",
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa")
      } else {
        list(color = "#111", background = knockout_pct_color(value))
      }
    },
    ...
  )
}

perc_column2 <- function(maxWidth = 50, class = NULL, ...) {
  colDef(
    align = "center",
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa")
      } else {
        list(color = "#111", background = knockout_pct_color2(value))
      }
    },
    ...
  )
}

format_pct <- function(value) {
  if (value == 0) "  \u2013 "    # en dash for 0%
  else if (value == 1) ">99%"  # checkmark for 100%
  else if (value < 0.01) " <1%"
  else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100), "%"), width = 4)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

off_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 1.3)
off_rating_color2 <- make_color_pal(c("#44ab43", "#f8fcf8",  "#ff2700"), bias = 1.3)
knockout_pct_color <- make_color_pal(c("#009c1a", "#22b600",  "#26cc00", "#7be382", "#d2f2d4", "#ffdc73", "#ffcf40", "#ffbf00", "#bf9b30", "#a67c00"), bias = 2)
knockout_pct_color2 <- make_color_pal(rev(c("#009c1a", "#22b600",  "#26cc00", "#7be382", "#d2f2d4", "#ffdc73", "#ffcf40", "#ffbf00", "#bf9b30", "#a67c00")), bias = 2)

# Icon to indicate trend: unchanged, up, down, or new
trend_indicator <- function(value = c("unchanged", "up", "down", "new")) {
  value <- match.arg(value)
  label <- switch(value,
                  unchanged = "Unchanged", up = "Trending up",
                  down = "Trending down", new = "New")
  
  # Add img role and tooltip/label for accessibility
  args <- list(role = "img", title = label)
  
  if (value == "unchanged") {
    args <- c(args, list("–", style = "color: #666; font-weight: 700"))
  } else if (value == "up") {
    args <- c(args, list("^", style = "color: #cd1a2b; font-size: 20px; font-weight: 700"))
  } else if (value == "down") {
    args <- c(args, list("v", style = "color: #1ed760; font-size: 16px; font-weight: 700"))
  } else {
    args <- c(args, list("-", style = "color: #2e77d0; font-size: 10px"))
  }
  do.call(span, args)
}

tbl <- reactable(
  epid_report,
  pagination = FALSE,
  defaultColGroup = colGroup(headerClass = "group-header"),
  columnGroups = list(
    colGroup(name = "Transmission", columns = trans_cols),
    colGroup(name = "Healthcare capacity", columns = capacity_cols)
  ),
  defaultColDef = colDef(class = "cell", headerClass = "header"),
  columns = list(
    state = colDef(
      maxWidth = 120,
      headerStyle = list(fontWeight = 400), 
      name="State",
      cell = function(value) {
        img_src <- knitr::image_uri(sprintf("images/%s.png", value))
        image <- img(src = img_src, height = "32px", alt = "")
        tagList(
          div(style = list(display = "inline-block", width = "70px"), image),
          value
        )
      }
    ),
    # date = colDef(defaultSortOrder = "asc", align = "center", maxWidth = 75, name="Date",
    #                class = "cell group", headerStyle = list(fontWeight = 700)),
    mean_ir = number_col_column(name = "7-day IR", maxWidth = 40),
    perc_change_ir = change_column(
      name = "% change*",
      cell = function(value) {
        scaled <- 1-(value - min(epid_report$perc_change_ir)) / (max(epid_report$perc_change_ir) - min(epid_report$perc_change_ir))
        color <- off_rating_color(scaled)
        value <- format(round(value, 1), nsmall = 1)
        div(class = "date-rating", style = list(background = color), value)
      }
    ),
    mean_mr = number_col_column(name = "7-day MR", maxWidth = 40),
    perc_change_mr = change_column(
      name = "% change*",
      cell = function(value) {
        min_val <- min(epid_report$perc_change_mr, na.rm = TRUE)
        max_val <- max(epid_report$perc_change_mr, na.rm = TRUE)
        # Check if all values are the same or if there's no variation
        if (min_val == max_val) {
          # Assign a default scaled value or handle differently if all values are the same
          scaled <- 0.5 # Midpoint, or any other logic you prefer
        } else {
          # Proceed with the original scaling calculation
          scaled <- 1 - (value - min_val) / (max_val - min_val)
        }
        color <- off_rating_color(scaled)
        value <- format(round(value, 1), nsmall = 1)
        div(class = "date-rating", style = list(background = color), value)
      }
    ),
    mean_rt_full = number_column(name = "Rt", maxWidth =100),
    perc_change_rt = change_column(
      name = "% change*",
      cell = function(value) {
        scaled <- 1-(value - min(epid_report$perc_change_rt)) / (max(epid_report$perc_change_rt) - min(epid_report$perc_change_rt))
        color <- off_rating_color(scaled)
        value <- format(round(value, 1), nsmall = 1)
        div(class = "date-rating", style = list(background = color), value)
      }
    ),
    
    mean_bor = perc_column(name = "BOR (%)"),
    trend_bor = colDef(
      header = span("BOR trend", class = "sr-only"),
      sortable = FALSE,
      align = "center",
      width = 25,
      cell = function(value) trend_indicator(value)
    ),
    mean_ior = perc_column(name = "IOR (%)"),
    trend_ior = colDef(
      header = span("Trend", class = "sr-only"),
      sortable = FALSE,
      align = "center",
      width = 25,
      cell = function(value) trend_indicator(value)
    ),
    mean_vor = perc_column(name = "VOR (%)"),
    trend_vor = colDef(
      header = span("Trend", class = "sr-only"),
      sortable = FALSE,
      align = "center",
      width = 25,
      cell = function(value) trend_indicator(value)
    )
  ),
  # Emphasize borders between groups when sorting by group
  rowClass = JS("
    function(rowInfo, state) {
      const firstSorted = state.sorted[0]
      if (firstSorted && firstSorted.id === 'group') {
        const nextRow = state.pageRows[rowInfo.viewIndex + 1]
        if (nextRow && rowInfo.row.group !== nextRow.group) {
          return 'group-last'
        }
      }
    }"
  ),
  showSortIcon = FALSE,
  borderless = TRUE,
  class = "standings-table")



div(class = "standings",
    div(class = "title",
        h2(" "),
        ""
    ),
    tbl,
    "*IR=incidence rate, MR=mortality rate, Rt=time-varying reproductive number, BOR=Bed-occupancy rate, IOR= ICU-occupancy rate, VOR=ventilator-utilisation rate, % change calculated as the percentage change of indicator compared to the last week"
)

saveRDS(tbl, 'plots/epid_report_tbl.rds')
