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
deaths <- trans %>% select(state, date, deaths_xbid) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "deaths_xbid") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

bid <- trans %>% select(state, date, deaths_bid) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "deaths_bid") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(deaths)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- deaths[,c(1,i)]
  colnames(x) <- c("date", "deaths")
  y <- bid[,c(1,i)]
  colnames(y) <- c("date", "bid")
  
  # Plot
  death_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~deaths, 
             name = "Daily deaths", marker = list(color = 'rgb(255,240,0)')) %>%
    add_bars(data = y, x = ~date, y = ~bid,
             name = "Brought-in-dead deaths", marker = list(color = 'rgb(242,49,242)')) %>%
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
                        range=c(0,(max(x$deaths, na.rm=T)+max(y$bid, na.rm=T))*1.1),
                        title="Deaths"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack",
           hovermode="x unified") 
  
  #save the plot
  saveRDS(death_curve_breakdown, paste0("plots/death_curve_breakdown_bid_",names(deaths[i]),"_daily",".rds"))
}

#create the dataset for deaths
deaths <- trans %>% select(state, date, perc_deaths_xbid) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_deaths_xbid") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

bid <- trans %>% select(state, date, perc_deaths_bid) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_deaths_bid") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(deaths)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- deaths[,c(1,i)]
  colnames(x) <- c("date", "deaths")
  y <- bid[,c(1,i)]
  colnames(y) <- c("date", "bid")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~deaths, 
             name = "Daily deaths", marker = list(color = 'rgb(255,240,0)')) %>%
    add_bars(data = y, x = ~date, y = ~bid,
             name = "Brought-in-dead deaths", marker = list(color = 'rgb(242,49,242)')) %>%
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
                        title="Deaths"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack",
           hovermode="x unified")
  
  #save the plot
  saveRDS(epid_curve_breakdown, paste0("plots/death_curve_breakdown_bid_",names(deaths[i]),"_perc",".rds"))
}

#######################################################################################################
#######################################################################################################
#######################################################################################################
#create the dataset for deaths
deaths <- trans %>% select(state, date, deaths_xvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "deaths_xvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

pvax <- trans %>% select(state, date, deaths_pvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "deaths_pvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

fvax <- trans %>% select(state, date, deaths_fvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "deaths_fvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

boost <- trans %>% select(state, date, deaths_boost) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "deaths_boost") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(deaths)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- deaths[,c(1,i)]
  colnames(x) <- c("date", "deaths")
  y <- pvax[,c(1,i)]
  colnames(y) <- c("date", "pvax")
  z <- fvax[,c(1,i)]
  colnames(z) <- c("date", "fvax")
  z2 <- boost [,c(1,i)]
  colnames(z2) <- c("date", "boost")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~deaths, 
             name = "Unvaccinated", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~pvax,
             name = "Partial vaccination", marker = list(color = 'rgb(255,240,0)')) %>%
    add_bars(data = z, x = ~date, y = ~fvax,
             name = "Full vaccination", marker = list(color = 'rgb(242,49,242)')) %>%
    add_bars(data = z2, x = ~date, y = ~boost,
             name = "Booster vaccination", marker = list(color = 'rgb(101,101,191)')) %>%
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
                        range=c(0,(max(x$deaths, na.rm=T) + max(y$pvax, na.rm=T) + 
                                     max(z$fvax, na.rm=T) + max(z2$boost, na.rm=T))*1.1),
                        legend = list(x = 1.05, y = 0.9),
                        title="Deaths"),
           barmode="stack",
           hovermode="x unified") 
  
  #save the plot
  saveRDS(epid_curve_breakdown, paste0("plots/death_curve_breakdown_vax_",names(deaths[i]),"_daily",".rds"))
}

#create the dataset for deaths
#create the dataset for deaths
deaths <- trans %>% select(state, date, perc_deaths_xvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_deaths_xvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

pvax <- trans %>% select(state, date, perc_deaths_pvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_deaths_pvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

fvax <- trans %>% select(state, date, perc_deaths_fvax) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_deaths_fvax") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

boost <- trans %>% select(state, date, perc_deaths_boost) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "perc_deaths_boost") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(deaths)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- deaths[,c(1,i)]
  colnames(x) <- c("date", "deaths")
  y <- pvax[,c(1,i)]
  colnames(y) <- c("date", "pvax")
  z <- fvax[,c(1,i)]
  colnames(z) <- c("date", "fvax")
  z2 <- boost [,c(1,i)]
  colnames(z2) <- c("date", "boost")
  
  # Plot
  epid_curve_breakdown <- plot_ly() %>% 
    add_bars(data = x, x = ~date, y = ~deaths, 
             name = "Unvaccinated", marker=list(color="grey")) %>%
    add_bars(data = y, x = ~date, y = ~pvax,
             name = "Partial vaccination", marker = list(color = 'rgb(255,240,0)')) %>%
    add_bars(data = z, x = ~date, y = ~fvax,
             name = "Full vaccination", marker = list(color = 'rgb(242,49,242)')) %>%
    add_bars(data = z2, x = ~date, y = ~boost,
             name = "Booster vaccination", marker = list(color = 'rgb(101,101,191)')) %>%
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
                        title="Deaths"),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack",
           hovermode="x unified") 
  
  #save the plot
  saveRDS(epid_curve_breakdown, paste0("plots/death_curve_breakdown_vax_",names(deaths[i]),"_perc",".rds"))
}
