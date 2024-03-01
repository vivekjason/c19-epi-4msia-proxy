library(htmltools)
library(tidyverse)
library(plotly)

#load data
transmission <- read.csv('data/transmission.csv')

#change transmission date format
transmission <- transmission %>% mutate(date=as.Date(date))

#develop the data 
trans_region <- transmission %>% select(state, date, rolldeaths_7d, population_new) %>%
  mutate(region = ifelse(state == 'Malaysia', "Malaysia",
                         ifelse(state == 'Perlis', "Northern region",
                                ifelse(state == 'Kedah', "Northern region",
                                       ifelse(state == 'Penang', "Northern region",
                                              ifelse(state == 'Perak', "Northern region",
                                                     ifelse(state == 'Selangor', "Central region",
                                                            ifelse(state == 'KL', "Central region",
                                                                   ifelse(state == 'W.P. Putrajaya', "Central region",
                                                                          ifelse(state == 'Negeri Sembilan', "Southern region",
                                                                                 ifelse(state == 'Melaka', "Southern region",
                                                                                        ifelse(state == 'Johor', "Southern region",
                                                                                               ifelse(state == 'Kelantan', "Eastern region",
                                                                                                      ifelse(state == 'Terengganu', "Eastern region",
                                                                                                             ifelse(state == 'Pahang', "Eastern region",
                                                                                                                    ifelse(state == 'Sabah', "Sabah",
                                                                                                                           ifelse(state == 'Sarawak', "Sarawak", "Sabah"))))))))))))))))) %>%
  group_by(region, date) %>%
  mutate(rolldeaths_7d=sum(rolldeaths_7d),
         population_new=sum(population_new)) %>%
  ungroup() %>% unique() %>%
  mutate(date=as.Date(date),
         mr_7day=(rolldeaths_7d/population_new)*100000) %>%
  select(date, region, mr_7day)

#legends & margisn,
m <- list(
  l = 20,
  r = 40,
  b = 20,
  t = 20,
  pad = 5
)

l <- list(
  bgcolor = "transparent",
  bordercolor = "transparent")

# Plot
mr_curve_state <- plot_ly() %>%
  add_lines(data=transmission, x=~date, y = ~mr_7day, name=~state, color=~state,
            colors=c("#0000FF", "#63B8FF", "#006400", "#008B45", "#54FF9F", "#C71585", "#EEC900", "#FF8C00", "#68228B", "#FFAEB9", "#CD2626", "#008B8B", "#ADD8E6",
                     "#F08080", "#696969", "#2B2B2B", "#FF1493"))%>%
  layout(titlefont=list(size=12),
         xaxis = list(title="Date",
                      rangeselector = list(
                        buttons = list(
                          list(
                            count = 3,
                            label = "3 mo",
                            step = "month",
                            stepmode = "backward"),
                          list(
                            count = 6,
                            label = "6 mo",
                            step = "month",
                            stepmode = "backward"),
                          list(
                            count = 1,
                            label = "1 yr",
                            step = "year",
                            stepmode = "backward"),
                          list(
                            count = 1,
                            label = "YTD",
                            step = "year",
                            stepmode = "todate"),
                          list(step = "all"))),
                      rangeslider = list(type = "date")),
         yaxis = list(side="left", tickfont= list(color = 'grey9', size=8), color='grey9', 
                      range=c(0,max(transmission$mr_7day, na.rm=T)*1.1),
                      title="7-day mortality rate"),
         legend = list(x = 1.05, y = 0.9)) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
  layout(legend = l,
         margin=m)

#save the plot
saveRDS(mr_curve_state, "plots/mr_curve_state.rds")

#######################################################################################################
mr_curve_state_log10 <- plot_ly() %>%
  add_lines(data=transmission, x=~date, y = ~mr_7day, name=~state, color=~state,
            colors=c("#0000FF", "#63B8FF", "#006400", "#008B45", "#54FF9F", "#C71585", "#EEC900", "#FF8C00", "#68228B", "#FFAEB9", "#CD2626", "#008B8B", "#ADD8E6",
                     "#F08080", "#696969", "#2B2B2B", "#FF1493"))%>%
  layout(titlefont=list(size=12),
         xaxis = list(title="Date",
                      rangeselector = list(
                        buttons = list(
                          list(
                            count = 3,
                            label = "3 mo",
                            step = "month",
                            stepmode = "backward"),
                          list(
                            count = 6,
                            label = "6 mo",
                            step = "month",
                            stepmode = "backward"),
                          list(
                            count = 1,
                            label = "1 yr",
                            step = "year",
                            stepmode = "backward"),
                          list(
                            count = 1,
                            label = "YTD",
                            step = "year",
                            stepmode = "todate"),
                          list(step = "all"))),
                      rangeslider = list(type = "date")),
         yaxis = list(side="left", tickfont= list(color = 'grey9', size=8), color='grey9', 
                      title="7-day mortality rate",
                      type = "log",  
                      tickvals = as.list(c(0.1,1,10,100,1000))),
         legend = list(x = 1.05, y = 0.9)) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
  layout(legend = l,
         margin=m)

#save the plot
saveRDS(mr_curve_state_log10, "plots/mr_curve_statelog10.rds")

#######################################################################################################
mr_curve_region <- plot_ly() %>%
  add_lines(data=trans_region, x=~date, y = ~mr_7day, name=~region, color=~region,
            colors=c("#0000FF", "#CD2626", "#008B45", "#63B8FF", "#2B2B2B", "#FFAEB9", "#EEC900"))%>%
  layout(titlefont=list(size=12),
         xaxis = list(title="Date",
                      rangeselector = list(
                        buttons = list(
                          list(
                            count = 3,
                            label = "3 mo",
                            step = "month",
                            stepmode = "backward"),
                          list(
                            count = 6,
                            label = "6 mo",
                            step = "month",
                            stepmode = "backward"),
                          list(
                            count = 1,
                            label = "1 yr",
                            step = "year",
                            stepmode = "backward"),
                          list(
                            count = 1,
                            label = "YTD",
                            step = "year",
                            stepmode = "todate"),
                          list(step = "all"))),
                      rangeslider = list(type = "date")),
         yaxis = list(side="left", tickfont= list(color = 'grey9', size=8), color='grey9', 
                      title="7-day mortality rate",
                      range=c(0,max(trans_region$mr_7day, na.rm=T)*1.1)),
         legend = list(x = 1.05, y = 0.9)) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
  layout(legend = l,
         margin=m)

#save the plot
saveRDS(mr_curve_region, "plots/mr_curve_region.rds")

#######################################################################################################
mr_curve_region_log10 <- plot_ly() %>%
  add_lines(data=trans_region, x=~date, y = ~mr_7day, name=~region, color=~region,
            colors=c("#0000FF", "#CD2626", "#008B45", "#63B8FF", "#2B2B2B", "#FFAEB9", "#EEC900"))%>%
  layout(titlefont=list(size=12),
         xaxis = list(title="Date",
                      rangeselector = list(
                        buttons = list(
                          list(
                            count = 3,
                            label = "3 mo",
                            step = "month",
                            stepmode = "backward"),
                          list(
                            count = 6,
                            label = "6 mo",
                            step = "month",
                            stepmode = "backward"),
                          list(
                            count = 1,
                            label = "1 yr",
                            step = "year",
                            stepmode = "backward"),
                          list(
                            count = 1,
                            label = "YTD",
                            step = "year",
                            stepmode = "todate"),
                          list(step = "all"))),
                      rangeslider = list(type = "date")),
         yaxis = list(side="left", tickfont= list(color = 'grey9', size=8), color='grey9', 
                      title="7-day mortality rate",
                      type = "log",  
                      tickvals = as.list(c(0.1,1,10,100,1000))),
         legend = list(x = 1.05, y = 0.9)) %>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
  layout(legend = l,
         margin=m)

#save the plot
saveRDS(mr_curve_region_log10, "plots/mr_curve_regionlog10.rds")
