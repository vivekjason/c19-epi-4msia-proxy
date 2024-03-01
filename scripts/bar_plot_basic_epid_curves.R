library(htmltools)
library(tidyverse)
library(plotly)

#load data
transmission <- read.csv('data/transmission.csv')
capacity <- read.csv('data/capacity.csv')

#develop the data 
trans <- capacity %>% select(state, date, hosp_covid, icu_covid, vent_covid) %>%
  left_join(transmission, by=c("state", "date")) %>%
  mutate(date=as.Date(date),
         cases_ximport.cluster=cases_new-cases_import-cases_cluster,
         cases_xvax=cases_new-cases_pvax-cases_fvax,
         cases_xhospital=cases_new-hosp_covid-icu_covid-vent_covid,
         cases_xdead=cases_new-deaths_new,
         
         deaths_xbid=deaths_new-deaths_bid,
         deaths_xvax=deaths_new-deaths_pvax-deaths_fvax-deaths_boost,
         
         perc_cases_ximport=(cases_ximport.cluster/(cases_ximport.cluster+cases_import+cases_cluster))*100,
         perc_cases_import=(cases_import/(cases_ximport.cluster+cases_import+cases_cluster))*100,
         perc_cases_clustert=(cases_fvax/(cases_ximport.cluster+cases_import+cases_cluster))*100,
         
         perc_cases_xvax=(cases_xvax/(cases_pvax+cases_fvax+cases_xvax))*100,
         perc_cases_pvax=(cases_pvax/(cases_pvax+cases_fvax+cases_xvax))*100,
         perc_cases_fvax=(cases_fvax/(cases_pvax+cases_fvax+cases_xvax))*100,
         
         perc_cases_xhospital=(cases_xhospital/(cases_xhospital-hosp_covid-icu_covid-vent_covid))*100,
         perc_cases_hosp_covid=(hosp_covid/(cases_xhospital-hosp_covid-icu_covid-vent_covid))*100,
         perc_cases_icu_covid=(icu_covid/(cases_xhospital-hosp_covid-icu_covid-vent_covid))*100,
         perc_cases_vent_covid=(vent_covid/(cases_xhospital-hosp_covid-icu_covid-vent_covid))*100,
         
         perc_cases_xdead=(cases_xdead/(cases_xdead+deaths_new))*100,
         perc_cases_dead=(deaths_new/(cases_xdead+deaths_new))*100,
         
         perc_deaths_xbid=(deaths_xbid/(deaths_bid+deaths_xbid))*100,
         perc_deaths_bid=(deaths_bid/(deaths_bid+deaths_xbid))*100,
         
         perc_deaths_xvax=(deaths_xvax/(deaths_xvax-deaths_pvax-deaths_fvax-deaths_boost))*100,
         perc_deaths_pvax=(deaths_pvax/(deaths_xvax-deaths_pvax-deaths_fvax-deaths_boost))*100,
         perc_deaths_fvax=(deaths_fvax/(deaths_xvax-deaths_pvax-deaths_fvax-deaths_boost))*100,
         perc_deaths_boost=(deaths_boost/(deaths_xvax-deaths_pvax-deaths_fvax-deaths_boost))*100,
  )

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

#plot epidemic curves for all cases
# engineer the data to wide form
mas_trans_df <- trans %>% select(date, state, cases_new) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_new") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#write a loop to plot and save all the plots
loop.vector <- 2:ncol(mas_trans_df)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- mas_trans_df[,c(1,i)]
  colnames(x) <- c("date", "cases")
  
  # Plot
  epid_curve <- plot_ly(data = x, x = ~date) %>% 
    add_bars(y = ~cases, 
             name = "Daily cases", marker=list(color="grey")) %>%
    layout(titlefont=list(size=12),
           xaxis = list( title="Date",
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
                        range=c(0,max(x$cases, na.rm=T)*1.1),
                        title="Daily cases"),
           #yaxis2 = list(overlaying = "y", side = "right",
           #tickfont = list(color = 'grey9', size=10), color='grey9', 
           #range=c(0,max(mas_healthcare_test_df$ma_7day_tr)*1.1),
           #title = "% change from baseline"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m)
  
  #save the plot
  saveRDS(epid_curve, paste0("plots/epid_curve_daily_",names(mas_trans_df[i]),".rds"))
}

#######################################################################################################################################
#######################################################################################################################################
#plot epidemic curves for all cases
# engineer the data to wide form
mas_trans_df <- trans %>% select(date, state, cases_active) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_active") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#write a loop to plot and save all the plots
loop.vector <- 2:ncol(mas_trans_df)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- mas_trans_df[,c(1,i)]
  colnames(x) <- c("date", "cases")
  
  # Plot
  epid_curve <- plot_ly(data = x, x = ~date) %>% 
    add_bars(y = ~cases, 
             name = "Active cases", marker=list(color="grey")) %>%
    layout(titlefont=list(size=12),
           xaxis = list( title="Date",
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
                        range=c(0,max(x$cases, na.rm=T)*1.1),
                        title="Active cases"),
           #yaxis2 = list(overlaying = "y", side = "right",
           #tickfont = list(color = 'grey9', size=10), color='grey9', 
           #range=c(0,max(mas_healthcare_test_df$ma_7day_tr)*1.1),
           #title = "% change from baseline"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m)
  
  #save the plot
  saveRDS(epid_curve, paste0("plots/epid_curve_active_",names(mas_trans_df[i]),".rds"))
}

#######################################################################################################################################
#######################################################################################################################################
#plot epidemic curves for all cases
# engineer the data to wide form
mas_trans_df <- trans %>% select(date, state, hosp_covid) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "hosp_covid") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#write a loop to plot and save all the plots
loop.vector <- 2:ncol(mas_trans_df)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- mas_trans_df[,c(1,i)]
  colnames(x) <- c("date", "cases")
  
  # Plot
  epid_curve <- plot_ly(data = x, x = ~date) %>% 
    add_bars(y = ~cases, 
             name = "Hospitalised cases", marker=list(color="grey")) %>%
    layout(titlefont=list(size=12),
           xaxis = list( title="Date",
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
                        range=c(0,max(x$cases, na.rm=T)*1.1),
                        title="Hospitalised cases"),
           #yaxis2 = list(overlaying = "y", side = "right",
           #tickfont = list(color = 'grey9', size=10), color='grey9', 
           #range=c(0,max(mas_healthcare_test_df$ma_7day_tr)*1.1),
           #title = "% change from baseline"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m)
  
  #save the plot
  saveRDS(epid_curve, paste0("plots/epid_curve_hospital_",names(mas_trans_df[i]),".rds"))
}

#######################################################################################################################################
#######################################################################################################################################
#plot epidemic curves for all cases
# engineer the data to wide form
mas_trans_df <- trans %>% select(date, state, icu_covid) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "icu_covid") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#write a loop to plot and save all the plots
loop.vector <- 2:ncol(mas_trans_df)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- mas_trans_df[,c(1,i)]
  colnames(x) <- c("date", "cases")
  
  # Plot
  epid_curve <- plot_ly(data = x, x = ~date) %>% 
    add_bars(y = ~cases, 
             name = "ICU cases", marker=list(color="grey")) %>%
    layout(titlefont=list(size=12),
           xaxis = list( title="Date",
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
                        range=c(0,max(x$cases, na.rm=T)*1.1),
                        title="ICU cases"),
           #yaxis2 = list(overlaying = "y", side = "right",
           #tickfont = list(color = 'grey9', size=10), color='grey9', 
           #range=c(0,max(mas_healthcare_test_df$ma_7day_tr)*1.1),
           #title = "% change from baseline"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m)
  
  #save the plot
  saveRDS(epid_curve, paste0("plots/epid_curve_icu_",names(mas_trans_df[i]),".rds"))
}