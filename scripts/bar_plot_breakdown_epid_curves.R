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
         cases_xhospital=cases_new-hosp_covid-icu_covid,
         cases_xdead=cases_new-deaths_new,
         
         deaths_xbid=deaths_new-deaths_bid,
         deaths_xvax=deaths_new-deaths_pvax-deaths_fvax-deaths_boost,
         
         perc_cases_ximport=round((cases_ximport.cluster/(cases_ximport.cluster+cases_import+cases_cluster))*100,1),
         perc_cases_import=round((cases_import/(cases_ximport.cluster+cases_import+cases_cluster))*100,1),
         perc_cases_cluster=round((cases_cluster/(cases_ximport.cluster+cases_import+cases_cluster))*100,1),
         
         perc_cases_xvax=round((cases_xvax/(cases_pvax+cases_fvax+cases_xvax))*100,1),
         perc_cases_pvax=round((cases_pvax/(cases_pvax+cases_fvax+cases_xvax))*100,1),
         perc_cases_fvax=round((cases_fvax/(cases_pvax+cases_fvax+cases_xvax))*100,1),
         
         perc_cases_xhospital=round((cases_xhospital/(cases_xhospital+hosp_covid+icu_covid))*100,1),
         perc_cases_hosp_covid=round((hosp_covid/(cases_xhospital+hosp_covid+icu_covid))*100,1),
         perc_cases_icu_covid=round((icu_covid/(cases_xhospital+hosp_covid+icu_covid))*100,1),
         
         perc_cases_child=round((cases_child/(cases_child+cases_adolescent+cases_adult+cases_elderly))*100,1),
         perc_cases_adol=round((cases_adolescent/(cases_child+cases_adolescent+cases_adult+cases_elderly))*100,1),
         perc_cases_adult=round((cases_adult/(cases_child+cases_adolescent+cases_adult+cases_elderly))*100,1),
         perc_cases_elder=round((cases_elderly/(cases_child+cases_adolescent+cases_adult+cases_elderly))*100,1),
         
         perc_cases_xdead=round((cases_xdead/(cases_xdead+deaths_new))*100,1),
         perc_cases_dead=round((deaths_new/(cases_xdead+deaths_new))*100,1),
         
         perc_deaths_xbid=round((deaths_xbid/(deaths_bid+deaths_xbid))*100,1),
         perc_deaths_bid=round((deaths_bid/(deaths_bid+deaths_xbid))*100,1),
         
         perc_deaths_xvax=round((deaths_xvax/(deaths_xvax-deaths_pvax-deaths_fvax-deaths_boost))*100,1),
         perc_deaths_pvax=round((deaths_pvax/(deaths_xvax-deaths_pvax-deaths_fvax-deaths_boost))*100,1),
         perc_deaths_fvax=round((deaths_fvax/(deaths_xvax-deaths_pvax-deaths_fvax-deaths_boost))*100,1),
         perc_deaths_boost=round((deaths_boost/(deaths_xvax-deaths_pvax-deaths_fvax-deaths_boost))*100,1),
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

#create the dataset for deaths
cases <- trans %>% select(state, date, cases_xdead) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_xdead") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

deaths <- trans %>% select(state, date, deaths_new) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "deaths_new") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(cases)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- cases[,c(1,i)]
  colnames(x) <- c("date", "cases")
  y <- deaths[,c(1,i)]
  colnames(y) <- c("date", "deaths")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~cases, 
             name = "Daily cases", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~deaths,
             name = "New deaths", marker = list(color = 'rgb(255,240,0)'),
             yaxis = 'y2') %>%
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
                        range=c(0,(max(x$cases, na.rm=T)+max(y$deaths, na.rm=T))*1.1),
                        title="Cases"),
           yaxis2 = list(overlaying = "y", side = "right", 
                         tickfont = list(color = 'grey9', size=10), color='grey9', 
                         range=c(0,max(y$deaths)*2.1), title = "Deaths"), 
           legend = list(x = 1.05, y = 0.9),
           barmode="stack",
           hovermode="x unified") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    
    layout(legend = l,
           margin=m)
  
  #save the plot
  saveRDS(epid_curve_breakdown, paste0("plots/epid_curve_breakdown_deaths_",names(cases[i]),"_daily",".rds"))
}

#create the dataset for deaths
cases <- trans %>% select(state, date, perc_cases_xdead) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_xdead") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

deaths <- trans %>% select(state, date, perc_cases_dead) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_dead") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(cases)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- cases[,c(1,i)]
  colnames(x) <- c("date", "cases")
  y <- deaths[,c(1,i)]
  colnames(y) <- c("date", "deaths")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~cases, 
             name = "Daily cases", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~deaths,
             name = "Daily deaths", marker = list(color = 'rgb(255,240,0)'),
             yaxis = 'y2') %>%
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
                        range=c(0, 100),
                        title="Cases"),
           yaxis2 = list(overlaying = "y", side = "right", 
                         tickfont = list(color = 'grey9', size=10), color='grey9', 
                         range=c(0, 100), title = "Deaths"), 
           legend = list(x = 1.05, y = 0.9),
           barmode="stack",
           hovermode="x unified") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    
    layout(legend = l,
           margin=m)
  
  #save the plot
  saveRDS(epid_curve_breakdown, paste0("plots/epid_curve_breakdown_deaths_",names(cases[i]),"_perc",".rds"))
}

#######################################################################################################
#######################################################################################################
#######################################################################################################
#create the dataset for deaths
cases <- trans %>% select(state, date, cases_ximport.cluster) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_ximport.cluster") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

import <- trans %>% select(state, date, cases_import) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_import") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

cluster <- trans %>% select(state, date, cases_cluster) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_cluster") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(cases)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- cases[,c(1,i)]
  colnames(x) <- c("date", "cases")
  y <- import[,c(1,i)]
  colnames(y) <- c("date", "import")
  z <- cluster[,c(1,i)]
  colnames(z) <- c("date", "cluster")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~cases, 
             name = "Unlinked cases", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~import, marker = list(color = 'rgb(255,240,0)'),
             name = "Imported cases") %>%
    add_bars(data = z, x = ~date, y = ~cluster, marker = list(color = 'rgb(242,49,242)'),
             name = "Cluster-linked cases") %>%
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
                        range=c(0,(max(x$cases, na.rm=T)+max(y$import, na.rm=T)+max(z$cluster, na.rm=T))*1.1),
                        title="Cases"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack",
           hovermode="x unified") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    
    layout(legend = l,
           margin=m)
  
  #save the plot
  saveRDS(epid_curve_breakdown, paste0("plots/epid_curve_breakdown_import_",names(cases[i]),"_daily.rds"))
}

#create the dataset for deaths
#create the dataset for deaths
cases <- trans %>% select(state, date, perc_cases_ximport) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_ximport") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

import <- trans %>% select(state, date, perc_cases_import) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_import") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

cluster <- trans %>% select(state, date, perc_cases_cluster) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_cluster") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(cases)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- cases[,c(1,i)]
  colnames(x) <- c("date", "cases")
  y <- import[,c(1,i)]
  colnames(y) <- c("date", "import")
  z <- cluster[,c(1,i)]
  colnames(z) <- c("date", "cluster")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~cases, 
             name = "Unlinked cases", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~import,
             name = "Import cases", marker = list(color = 'rgb(255,240,0)')) %>%
    add_bars(data = z, x = ~date, y = ~cluster,
             name = "Cluster cases", marker = list(color = 'rgb(242,49,242)')) %>%
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
                        range=c(0, 100),
                        title="Cases"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack",
           hovermode="x unified") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    
    layout(legend = l,
           margin=m)
  
  #save the plot
  saveRDS(epid_curve_breakdown, paste0("plots/epid_curve_breakdown_import_",names(cases[i]),"_perc",".rds"))
}

#######################################################################################################
#######################################################################################################
#######################################################################################################
#create the dataset for deaths
cases <- trans %>% select(state, date, cases_xvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_xvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

pvax <- trans %>% select(state, date, cases_pvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_pvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

fvax <- trans %>% select(state, date, cases_fvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_fvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(cases)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- cases[,c(1,i)]
  colnames(x) <- c("date", "cases")
  y <- pvax[,c(1,i)]
  colnames(y) <- c("date", "pvax")
  z <- fvax[,c(1,i)]
  colnames(z) <- c("date", "fvax")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~cases, 
             name = "Unvaccinated cases", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~pvax,
             name = "Partially vaccinated cases", marker = list(color = 'rgb(255,240,0)')) %>%
    add_bars(data = z, x = ~date, y = ~fvax,
             name = "Fully vaccinated cases", marker = list(color = 'rgb(242,49,242)')) %>%
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
                        range=c(0,(max(x$cases, na.rm=T)+max(y$pvax, na.rm=T)+max(z$fvax, na.rm=T))*1.1),
                        legend = list(x = 1.05, y = 0.9)),
                        barmode="stack",
                        hovermode="x unified") %>%
             config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
             
             layout(legend = l,
                    margin=m)
           
           #save the plot
           saveRDS(epid_curve_breakdown, paste0("plots/epid_curve_breakdown_vax_",names(cases[i]),"_daily",".rds"))
}

#create the dataset for deaths
#create the dataset for deaths
cases <- trans %>% select(state, date, perc_cases_xvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_xvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

pvax <- trans %>% select(state, date, perc_cases_pvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_pvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

fvax <- trans %>% select(state, date, perc_cases_fvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_fvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(cases)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- cases[,c(1,i)]
  colnames(x) <- c("date", "cases")
  y <- pvax[,c(1,i)]
  colnames(y) <- c("date", "pvax")
  z <- fvax[,c(1,i)]
  colnames(z) <- c("date", "fvax")
 
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~cases, 
             name = "Unvaccinated cases", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~pvax,
             name = "Partially vaccinated cases", marker = list(color = 'rgb(255,240,0)')) %>%
    add_bars(data = z, x = ~date, y = ~fvax,
             name = "Fully vaccinated cases", marker = list(color = 'rgb(242,49,242)')) %>%
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
                        range=c(0, 100),
                        title="Cases"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack",
           hovermode="x unified") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    
    layout(legend = l,
           margin=m)
  
  #save the plot
  saveRDS(epid_curve_breakdown, paste0("plots/epid_curve_breakdown_vax_",names(cases[i]),"_perc",".rds"))
}

#######################################################################################################
#######################################################################################################
#######################################################################################################
#create the dataset for hospitalisation
cases <- trans %>% select(state, date, cases_xhospital) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_xhospital") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

hospital <- trans %>% select(state, date, hosp_covid) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "hosp_covid") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

icu <- trans %>% select(state, date, icu_covid) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "icu_covid") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")


#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(cases)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- cases[,c(1,i)]
  colnames(x) <- c("date", "cases")
  y <- hospital[,c(1,i)]
  colnames(y) <- c("date", "hospital")
  z <- icu[,c(1,i)]
  colnames(z) <- c("date", "icu")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~cases, 
             name = "Non-hospitalised cases", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~hospital,
             name = "Hospitalised cases", marker = list(color = 'rgb(255,240,0)')) %>%
    add_bars(data = z, x = ~date, y = ~icu,
             name = "ICU cases", marker = list(color = 'rgb(242,49,242)')) %>%
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
                        range=c(0,(max(x$cases, na.rm=T)+max(y$hospital, na.rm=T)+max(z$icu, na.rm=T))*1.1),
                        legend = list(x = 1.05, y = 0.9)),
                        barmode="stack",
                        hovermode="x unified") %>%
             config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
             
             layout(legend = l,
                    margin=m)
           
           #save the plot
           saveRDS(epid_curve_breakdown, paste0("plots/epid_curve_breakdown_hosp_",names(cases[i]),"_daily",".rds"))
}

#create the dataset for deaths
#create the dataset for deaths
cases <- trans %>% select(state, date, perc_cases_xhospital) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_xhospital") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

hospital <- trans %>% select(state, date, perc_cases_hosp_covid) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_hosp_covid") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

icu <- trans %>% select(state, date, perc_cases_icu_covid) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_icu_covid") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")


#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(cases)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- cases[,c(1,i)]
  colnames(x) <- c("date", "cases")
  y <- hospital[,c(1,i)]
  colnames(y) <- c("date", "hospital")
  z <- icu[,c(1,i)]
  colnames(z) <- c("date", "icu")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~cases, 
             name = "Non-hospitalised cases", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~hospital,
             name = "Hospitalised cases", marker = list(color = 'rgb(255,240,0)')) %>%
    add_bars(data = z, x = ~date, y = ~icu,
             name = "ICU cases", marker = list(color = 'rgb(242,49,242)')) %>%
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
                        range=c(0, 100),
                        title="Cases"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack",
           hovermode="x unified") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    
    layout(legend = l,
           margin=m)
  
  #save the plot
  saveRDS(epid_curve_breakdown, paste0("plots/epid_curve_breakdown_hosp_",names(cases[i]),"_perc",".rds"))
}

#######################################################################################################
#######################################################################################################
#######################################################################################################
#create the dataset for hospitalisation
cases <- trans %>% select(state, date, cases_adult) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_adult") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

cases_child <- trans %>% select(state, date, cases_child) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_child") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

cases_adolescent <- trans %>% select(state, date, cases_adolescent) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_adolescent") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

cases_elderly <- trans %>% select(state, date, cases_elderly) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_elderly") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")


#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(cases)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- cases[,c(1,i)]
  colnames(x) <- c("date", "cases")
  y <- cases_child[,c(1,i)]
  colnames(y) <- c("date", "cases_child")
  z <- cases_adolescent[,c(1,i)]
  colnames(z) <- c("date", "cases_adolescent")
  z2 <- cases_elderly [,c(1,i)]
  colnames(z2) <- c("date", "cases_elderly")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~cases, 
             name = "Adults", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~cases_child,
             name = "Children", marker = list(color = 'rgb(255,240,0)')) %>%
    add_bars(data = z, x = ~date, y = ~cases_adolescent,
             name = "Adolescents", marker = list(color = 'rgb(242,49,242)')) %>%
    add_bars(data = z2, x = ~date, y = ~cases_elderly,
             name = "Elderly", marker = list(color = 'rgb(101,101,191)')) %>%
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
                        range=c(0,(max(x$cases, na.rm=T) + max(y$cases_child, na.rm=T) + 
                                     max(z$cases_adolescent, na.rm=T) + max(z2$cases_elderly, na.rm=T))*1.1),
                        legend = list(x = 1.05, y = 0.9)),
                        barmode="stack",
                        hovermode="x unified") %>%
             config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
             
             layout(legend = l,
                    margin=m)
           
           #save the plot
           saveRDS(epid_curve_breakdown, paste0("plots/epid_curve_breakdown_age_",names(cases[i]),"_daily",".rds"))
}

#create the dataset for deaths
#create the dataset for hospitalisation
cases <- trans %>% select(state, date, perc_cases_adult) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_adult") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

cases_child <- trans %>% select(state, date, perc_cases_child) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_child") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

cases_adolescent <- trans %>% select(state, date, perc_cases_adol) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_adol") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

cases_elderly <- trans %>% select(state, date, perc_cases_elder) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_cases_elder") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")


#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(cases)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- cases[,c(1,i)]
  colnames(x) <- c("date", "cases")
  y <- cases_child[,c(1,i)]
  colnames(y) <- c("date", "cases_child")
  z <- cases_adolescent[,c(1,i)]
  colnames(z) <- c("date", "cases_adolescent")
  z2 <- cases_elderly [,c(1,i)]
  colnames(z2) <- c("date", "cases_elderly")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~cases, 
             name = "Adults", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~cases_child,
             name = "Children", marker = list(color = 'rgb(255,240,0)')) %>%
    add_bars(data = z, x = ~date, y = ~cases_adolescent,
             name = "Adolescents", marker = list(color = 'rgb(242,49,242)')) %>%
    add_bars(data = z2, x = ~date, y = ~cases_elderly,
             name = "Elderly", marker = list(color = 'rgb(101,101,191)')) %>%
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
                        range=c(0, 100),
                        title="Cases"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack",
           hovermode="x unified") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    
    layout(legend = l,
           margin=m)
  
  #save the plot
  saveRDS(epid_curve_breakdown, paste0("plots/epid_curve_breakdown_age_",names(cases[i]),"_perc",".rds"))
}
