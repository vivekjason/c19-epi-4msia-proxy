library(reactable)
library(htmltools)
library(tidyverse)
library(viridis)

#pull data
capacity <- read.csv('data/capacity.csv')

#strucutre data for reactable
bed_cols <- c("hosp_beds", "bor", "change_bor")
icu_cols <- c("icu_beds", "ior", "change_ior")
vent_cols <- c("vent_beds",  "vor", "change_vor")
capacity <- capacity %>% group_by(state) %>%
  mutate(date=as.Date(date),
         hosp_beds=as.numeric(hosp_beds),
         icu_beds=as.numeric(icu_beds),
         vent_beds=as.numeric(vent_beds),
         bor=bor/100,
         ior=ior/100,
         vor=vor/100) %>%
  filter(date==max(date)) %>%
  ungroup() %>% as.data.frame()
capacity <- capacity[, c("state", bed_cols, icu_cols, vent_cols)]

change_column <- function(maxWidth = 55, ...) {
  colDef(maxWidth = maxWidth, align = "center", class = "cell number", ...)
}

bed_column <- function(class = NULL, ...) {
  colDef(maxWidth = 70, align = "center", class = paste("cell number", class), ...)
}

occupancy_column <- function(maxWidth = 70, class = NULL, ...) {
  colDef(
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

format_pct <- function(value) {
  if (value == 0) "  \u2013 "    # en dash for 0%
  else if (value == 1) "\u2713"  # checkmark for 100%
  else if (value < 0.01) " <1%"
  else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100), "%"), width = 4)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

off_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 1.3)
col <- rev(as.vector(viridis::viridis(100)))
knockout_pct_color <- make_color_pal(col, bias = 2)

tbl <- reactable(
  capacity,
  pagination = FALSE,
  defaultColGroup = colGroup(headerClass = "group-header"),
  columnGroups = list(
    colGroup(name = "Hospital", columns = bed_cols),
    colGroup(name = "Intensive care unit", columns = icu_cols),
    colGroup(name = "Ventilators", columns = vent_cols)
  ),
  defaultColDef = colDef(class = "cell", headerClass = "header"),
  columns = list(
    state = colDef(
      minWidth = 50,
      headerStyle = list(fontWeight = 700), 
      name="State",
      cell = function(value) {
        img_src <- knitr::image_uri(sprintf("images/%s.png", value))
        image <- img(src = img_src, height = "36px", alt = "")
        tagList(
          div(style = list(display = "inline-block", width = "70px"), image),
          value
        )
      }
    ),
    # date = colDef(defaultSortOrder = "asc", align = "center", maxWidth = 75, name="Date",
    #                class = "cell group", headerStyle = list(fontWeight = 700)),
    hosp_beds = bed_column(name = "Beds*", class = "border-left"),
    bor = occupancy_column(name = "Occupancy (%)", maxWidth = 90 ),
    change_bor = change_column(
      name = "% change**",
      maxWidth = 90,
      cell = function(value) {
        scaled <- 1-(value - min(capacity$change_bor)) / (max(capacity$change_bor) - min(capacity$change_bor))
        color <- off_rating_color(scaled)
        value <- format(round(value, 1), nsmall = 1)
        div(class = "date-rating", style = list(background = color), value)
      }
    ),
    
    icu_beds = bed_column(name = "Beds*", class = "border-left"),
    ior = occupancy_column(name = "Occupancy (%)", maxWidth = 90),
    change_ior = change_column(
      name = "% change**", 
      maxWidth = 90,
      defaultSortOrder = "asc",
      cell = function(value) {
        scaled <- 1 - (value - min(capacity$change_ior)) / (max(capacity$change_ior) - min(capacity$change_ior))
        color <- off_rating_color(scaled)
        value <- format(round(value, 1), nsmall = 1)
        div(class = "date-rating", style = list(background = color), value)
      }
    ),
    
    vent_beds = bed_column(name = "Ventilators*", class = "border-left"),
    vor = occupancy_column(name = "Occupancy (%)", maxWidth = 90),
    change_vor = change_column(
      name = "% change**", 
      maxWidth = 90,
      defaultSortOrder = "asc",
      cell = function(value) {
        scaled <- 1 - (value - min(capacity$change_vor)) / (max(capacity$change_vor) - min(capacity$change_vor))
        color <- off_rating_color(scaled)
        value <- format(round(value, 1), nsmall = 1)
        div(class = "date-rating", style = list(background = color), value)
      }
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
    "*total number of beds/ ventilators available
    **absolute change from 7-days prior, Negative number = decrease occupancy, Positive number = increase occupancy"
)

saveRDS(tbl, 'plots/healthcare_tbl.rds')