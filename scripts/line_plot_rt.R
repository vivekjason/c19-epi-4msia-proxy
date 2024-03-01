library(htmltools)
library(tidyverse)
library(plotly)

#load data
transmission <- read.csv('data/transmission.csv')

#create 3 datasets for each rt level
rt <- transmission %>% select(state, date, rt) %>%
  mutate(date=as.Date(date),
         rt=round(rt, 3)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "rt") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

lower <- transmission %>% select(state, date, lower) %>%
  mutate(date=as.Date(date),
         lower=round(lower, 3)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "lower") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

upper <- transmission %>% select(state, date, upper) %>%
  mutate(date=as.Date(date),
         upper=round(upper, 3)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "upper") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

mas_trans_df <- transmission %>% select(date, state, cases_new) %>%
  mutate(date=as.Date(date)) %>%
  pivot_wider(id_cols = "date",
              names_from = "state",
              values_from = "cases_new") %>%
  rename("KL"="W.P. Kuala Lumpur", 
         "Putrajaya"="W.P. Putrajaya",
         "Labuan"="W.P. Labuan",
         "NS"="Negeri Sembilan",
         "Penang"="Pulau Pinang")

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
#write a loop to plot and save all the plots
loop.vector <- 2:ncol(rt)

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x, y, z and a
  x <- rt[,c(1,i)]
  colnames(x) <- c("date", "rt")
  y <- lower[,c(1,i)]
  colnames(y) <- c("date", "lower")
  z <- upper[,c(1,i)]
  colnames(z) <- c("date", "upper")
  a <- mas_trans_df[,c(1,i)]
  colnames(a) <- c("date", "cases")
  
  # Plot
  rt_curve <- plot_ly() %>% 
    add_bars(data = a, x = ~date, y = ~cases, name = "Daily cases", 
             marker=list(color="grey")) %>%
    add_lines(data = x, x = ~date, y = ~rt, name = paste("R",'<sup>',"t",'</sup>'), 
              line=list(width = 4, color = 'rgb(205, 12, 24)'),
              yaxis = 'y2') %>%
    add_lines(data = y, x = ~date, y = ~lower, name = paste("95% CI upper lower interval","R",'<sup>',"t",'</sup>'), 
              opacity=0.4, line=list(width = 4, dash = 'dot', color = 'rgb(205, 12, 24)'), 
              yaxis = 'y2') %>%
    add_lines(data = z, x = ~date, y = ~upper, name = paste("95% CI upper upper interval","R",'<sup>',"t",'</sup>'),
              opacity=0.4, line=list(width = 4, dash = 'dot', color = 'rgb(205, 12, 24)'), 
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
           yaxis = list(tickfont= list(color = 'grey9', size=8), color='grey9', 
                        range=c(0,max(a$cases)*1.1),
                        title="Daily cases"),
           yaxis2 = list(overlaying = "y", side = "right",
                         tickfont = list(color = 'grey9', size=10), color='grey9', 
                         range=c(-1, 2.079442/2),
                         type = "log",
                         tickvals = as.list(c(seq(0.2,1,0.2), seq(2,10,2))),
                         title = paste("R",'<sup>',"t",'</sup>')),
           legend = list(x = 1.05, y = 0.9),
           barmode="stack") %>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian","lasso2d", "select2d", "pan2d","zoom2d")) %>%
    layout(legend = l,
           margin=m,
           hovermode="x unified")
  
  #save the plot
  saveRDS(rt_curve, paste0("plots/rt_curve_",names(rt[i]),".rds"))
}

