library(htmltools)
library(tidyverse)
library(plotly)

#load data
capacity <- read.csv('data/capacity.csv')

#legends & margisn
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

#all ages
or <- capacity %>% select(state, date, bor) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "bor") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

beds <- capacity %>% select(state, date, hosp_beds) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "hosp_beds") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(or)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- or[,c(1,i)]
  colnames(x) <- c("date", "bor")
  y <- beds[,c(1,i)]
  colnames(y) <- c("date", "hosp_beds")

  
  # Plot
  or_plot <- plot_ly() %>% 
  add_lines(data = y , x = ~date, y = ~round(hosp_beds,0), name = "Number of beds", 
            line=list(width = 4, color = 'rgb(255,111,105)')) %>%
    add_lines(data = x , x = ~date, y = ~round(bor,1), name ="Bed occupancy rate (%)" , 
              opacity=0.4, line=list(width = 4, dash = 'dot', color = 'rgb(255,111,105)'), yaxis = 'y2') %>%
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
                        range=c(0,max(y$hosp_beds)*1.1),
                        title="Number of beds"),
           yaxis2 = list(overlaying = "y", side = "right",
                         tickfont = list(color = 'grey9', size=10), color='grey9', 
                         range=c(0,max(x$bor)*1.1),
                         title = "Bed occupancy rate (%)")) %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m,
           hovermode = "x unified")
  
  #save the plot
  saveRDS(or_plot, paste0("plots/healthcare_curve_",names(or[i]), "_bor",".rds"))
}

#all ages
or <- capacity %>% select(state, date, ior) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "ior") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

beds <- capacity %>% select(state, date, icu_beds) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "icu_beds") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(or)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- or[,c(1,i)]
  colnames(x) <- c("date", "ior")
  y <- beds[,c(1,i)]
  colnames(y) <- c("date", "icu_beds")
  
  
  # Plot
  or_plot <- plot_ly() %>% 
    add_lines(data = y , x = ~date, y = ~round(icu_beds,0), name = "Number of ICU beds", 
              line=list(width = 4, color = 'rgb(255,204,92)')) %>%
    add_lines(data = x , x = ~date, y = ~round(ior,1), name ="ICU occupancy rate (%)" , 
              opacity=0.4, line=list(width = 4, dash = 'dot', color = 'rgb(255,204,92)'), yaxis = 'y2') %>%
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
                        range=c(0,max(y$icu_beds)*1.1),
                        title="Number of ICU beds"),
           yaxis2 = list(overlaying = "y", side = "right",
                         tickfont = list(color = 'grey9', size=10), color='grey9', 
                         range=c(0,max(x$ior)*1.1),
                         title = "ICU ccupancy rate (%)")) %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m,
           hovermode = "x unified")
  
  #save the plot
  saveRDS(or_plot, paste0("plots/healthcare_curve_",names(or[i]), "_ior",".rds"))
}

#all ages
or <- capacity %>% select(state, date, vor) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "vor") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

beds <- capacity %>% select(state, date, vent_beds) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "vent_beds") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

#plot epidemic curves for all cases
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(or)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- or[,c(1,i)]
  colnames(x) <- c("date", "vor")
  y <- beds[,c(1,i)]
  colnames(y) <- c("date", "vent_beds")
  
  
  # Plot
  or_plot <- plot_ly() %>% 
    add_lines(data = y , x = ~date, y = ~round(vent_beds,0), name = "Number of beds with ventilator", 
              line=list(width = 4, color = 'rgb(136,216,176)')) %>%
    add_lines(data = x , x = ~date, y = ~round(vor,1), name ="Ventilator utilisation rate (%)" , 
              opacity=0.4, line=list(width = 4, dash = 'dot', color = 'rgb(136,216,176)'), yaxis = 'y2') %>%
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
                        range=c(0,max(y$vent_beds)*1.1),
                        title="Number of ventilatied beds"),
           yaxis2 = list(overlaying = "y", side = "right",
                         tickfont = list(color = 'grey9', size=10), color='grey9', 
                         range=c(0,max(x$vor)*1.1),
                         title = "Ventilator utilisation rate (%)")) %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = list(x = 0.01, y = 0.99)) %>% 
    layout(legend = l,
           margin=m,
           hovermode = "x unified")
  
  #save the plot
  saveRDS(or_plot, paste0("plots/healthcare_curve_",names(or[i]), "_vor",".rds"))
}
