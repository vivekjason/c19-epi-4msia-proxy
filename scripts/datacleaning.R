#load packages
rm(list=ls())
required_packages <- c("tidyverse", "EpiEstim", "readxl", "readr", "zoo", "lubridate", "arrow")
not_installed <- required_packages[!(required_packages %in% installed.packages()[ , "Package"])]    
if(length(not_installed)) install.packages(not_installed)                                           
suppressWarnings(lapply(required_packages, require, character.only = TRUE))

# National cluster data
#call the data
national <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/cases_malaysia.csv")

#extract cluster data
cluster <- national %>% select("date", "cases_new", "cases_import", 
                               "cluster_import", "cluster_religious", "cluster_community", "cluster_highRisk", 
                               "cluster_education", "cluster_detentionCentre", "cluster_workplace")

#write to the data
write.csv(cluster, "data/cluster.csv")


# National case and death data
#call the data
cases_mas <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/cases_malaysia.csv")
deaths_mas <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/deaths_malaysia.csv")
cases_state <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/cases_state.csv")
deaths_state <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/deaths_state.csv")

#extract natioal data
cases_mas <- cases_mas %>% mutate(cases_truncate=cases_new-cases_import-cluster_detentionCentre,
                                  state="Malaysia",
                                  ma_7day_cases=rollmean(cases_new, k = 7, fill = "extend"),
                                  ma_7day_cases_truncate=rollmean(cases_truncate, k = 7, fill = "extend"),
                                  ma_7day_cases_truncate=ifelse(is.na(ma_7day_cases_truncate),ma_7day_cases, ma_7day_cases_truncate),
                                  rollcases_7d=rollsum(cases_new, k=7,fill=NA, align="right"),
                                  time=seq(1, nrow(cases_mas),1)) %>%
  select("date","time", "state", "cases_new", "cases_import", "cases_recovered", "cases_active", "cases_cluster", "cases_pvax", "cases_fvax", "cases_child", 
         "cases_adolescent", "cases_adult", "cases_elderly", "cases_truncate", "ma_7day_cases", "ma_7day_cases_truncate", "rollcases_7d")

#extract state case data
cases_state <- cases_state %>% mutate(cases_truncate=cases_new-cases_import) %>%
  group_by(state) %>%mutate(ma_7day_cases=rollmean(cases_new, k = 7, fill = "extend"),
                            ma_7day_cases_truncate=rollmean(cases_truncate, k = 7, fill = "extend"),
                            ma_7day_cases_truncate=ifelse(is.na(ma_7day_cases_truncate),ma_7day_cases, ma_7day_cases_truncate),
                            rollcases_7d=rollsum(cases_new, k=7,fill=NA, align="right"),
                            time=seq(1, nrow(cases_mas),1)) %>%
  select("date", "time", "state", "cases_new", "cases_import", "cases_recovered", "cases_active", "cases_cluster", "cases_pvax", "cases_fvax", "cases_child", 
         "cases_adolescent", "cases_adult", "cases_elderly", "cases_truncate", "ma_7day_cases", "ma_7day_cases_truncate", "rollcases_7d")

#extract national deaths
deaths_mas <-  deaths_mas %>% mutate(state="Malaysia",
                                     ma_7day_deaths=rollmean(deaths_new, k = 7, fill = "extend"),
                                     rolldeaths_7d=rollsum(deaths_new, k=7,fill=NA, align="right"))

#extract state deaths
deaths_state <-  deaths_state %>% group_by(state) %>%
  mutate(ma_7day_deaths=rollmean(deaths_new, k = 7, fill = "extend"),
         rolldeaths_7d=rollsum(deaths_new, k=7,fill=NA, align="right"))

#merge sets
cases <- bind_rows (cases_mas, cases_state)
deaths <- bind_rows(deaths_mas, deaths_state)
transmission <- left_join(cases, deaths, by=c("date", "state")) 

#divide into rt set and estimate
rt_input <- transmission %>% select (date, state, ma_7day_cases_truncate) %>%
  pivot_wider(id_cols=date,
              names_from=state, 
              values_from=ma_7day_cases_truncate) %>%
  replace(is.na(.), 0)

#create a dummy table
rt_output <- data.frame(time=seq(8,nrow(rt_input),1),
                        "Malaysia" = rep(NA, nrow(rt_input)-7),    # Create example data
                        "Johor" = rep(NA, nrow(rt_input)-7),
                        "Kedah" = rep(NA, nrow(rt_input)-7),
                        "Kelantan" = rep(NA, nrow(rt_input)-7),    # Create example data
                        "Melaka" = rep(NA, nrow(rt_input)-7),
                        "Negeri Sembilan" = rep(NA, nrow(rt_input)-7),
                        "Pahang" = rep(NA, nrow(rt_input)-7),
                        "Perak" = rep(NA, nrow(rt_input)-7),
                        "Perlis" = rep(NA, nrow(rt_input)-7),    # Create example data
                        "Pulau Pinang" = rep(NA, nrow(rt_input)-7),
                        "Sabah" = rep(NA, nrow(rt_input)-7),
                        "Sarawak" = rep(NA, nrow(rt_input)-7),
                        "Selangor" = rep(NA, nrow(rt_input)-7),
                        "Terengganu" = rep(NA, nrow(rt_input)-7),
                        "W.P. Kuala Lumpur" = rep(NA, nrow(rt_input)-7),
                        "W.P. Labuan" = rep(NA, nrow(rt_input)-7),
                        "W.P. Putrajaya" = rep(NA, nrow(rt_input)-7))

lower_output <- data.frame(time=seq(8,nrow(rt_input),1),
                           "Malaysia" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Johor" = rep(NA, nrow(rt_input)-7),
                           "Kedah" = rep(NA, nrow(rt_input)-7),
                           "Kelantan" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Melaka" = rep(NA, nrow(rt_input)-7),
                           "Negeri Sembilan" = rep(NA, nrow(rt_input)-7),
                           "Pahang" = rep(NA, nrow(rt_input)-7),
                           "Perak" = rep(NA, nrow(rt_input)-7),
                           "Perlis" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Pulau Pinang" = rep(NA, nrow(rt_input)-7),
                           "Sabah" = rep(NA, nrow(rt_input)-7),
                           "Sarawak" = rep(NA, nrow(rt_input)-7),
                           "Selangor" = rep(NA, nrow(rt_input)-7),
                           "Terengganu" = rep(NA, nrow(rt_input)-7),
                           "W.P. Kuala Lumpur" = rep(NA, nrow(rt_input)-7),
                           "W.P. Labuan" = rep(NA, nrow(rt_input)-7),
                           "W.P. Putrajaya" = rep(NA, nrow(rt_input)-7))

upper_output <- data.frame(time=seq(8,nrow(rt_input),1),
                           "Malaysia" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Johor" = rep(NA, nrow(rt_input)-7),
                           "Kedah" = rep(NA, nrow(rt_input)-7),
                           "Kelantan" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Melaka" = rep(NA, nrow(rt_input)-7),
                           "Negeri Sembilan" = rep(NA, nrow(rt_input)-7),
                           "Pahang" = rep(NA, nrow(rt_input)-7),
                           "Perak" = rep(NA, nrow(rt_input)-7),
                           "Perlis" = rep(NA, nrow(rt_input)-7),    # Create example data
                           "Pulau Pinang" = rep(NA, nrow(rt_input)-7),
                           "Sabah" = rep(NA, nrow(rt_input)-7),
                           "Sarawak" = rep(NA, nrow(rt_input)-7),
                           "Selangor" = rep(NA, nrow(rt_input)-7),
                           "Terengganu" = rep(NA, nrow(rt_input)-7),
                           "W.P. Kuala Lumpur" = rep(NA, nrow(rt_input)-7),
                           "W.P. Labuan" = rep(NA, nrow(rt_input)-7),
                           "W.P. Putrajaya" = rep(NA, nrow(rt_input)-7))

#loop the case counts for each state
count <- 2
for(i in 2:ncol(rt_input)) {# for-loop over columns
  rt <- estimate_R(rt_input[,i],
                   method="parametric_si",
                   config = make_config(list(
                     mean_si = 5.12, 
                     std_si = 1.86)))
  rt <- as.data.frame(rt$R[,c(3,5,11)])
  rt_output[,count] <- rt[,1]
  lower_output[,count] <- rt[,2]
  upper_output[,count] <- rt[,3]
  count <- count+1
}

#pivot rt
rt_output <- rt_output %>% pivot_longer(-time,
                                        names_to = "state",
                                        values_to = "rt") %>%
  mutate(state = replace(state, state=="Negeri.Sembilan", "Negeri Sembilan"),
         state = replace(state, state=="Pulau.Pinang", "Pulau Pinang"),
         state = replace(state, state=="W.P..Kuala.Lumpur", "W.P. Kuala Lumpur"),
         state = replace(state, state=="W.P..Labuan", "W.P. Labuan"),
         state = replace(state, state=="W.P..Putrajaya", "W.P. Putrajaya"))
  
lower_output <- lower_output %>% pivot_longer(-time,
                                              names_to = "state",
                                              values_to = "lower") %>%
  mutate(state = replace(state, state=="Negeri.Sembilan", "Negeri Sembilan"),
         state = replace(state, state=="Pulau.Pinang", "Pulau Pinang"),
         state = replace(state, state=="W.P..Kuala.Lumpur", "W.P. Kuala Lumpur"),
         state = replace(state, state=="W.P..Labuan", "W.P. Labuan"),
         state = replace(state, state=="W.P..Putrajaya", "W.P. Putrajaya"))
upper_output <- upper_output %>% pivot_longer(-time,
                                              names_to = "state",
                                              values_to = "upper") %>%
  mutate(state = replace(state, state=="Negeri.Sembilan", "Negeri Sembilan"),
         state = replace(state, state=="Pulau.Pinang", "Pulau Pinang"),
         state = replace(state, state=="W.P..Kuala.Lumpur", "W.P. Kuala Lumpur"),
         state = replace(state, state=="W.P..Labuan", "W.P. Labuan"),
         state = replace(state, state=="W.P..Putrajaya", "W.P. Putrajaya"))

#join up the rt to the transmission set
transmission <- transmission %>% left_join(rt_output, by=c("time", "state")) %>%
  left_join(lower_output, by=c("time", "state")) %>%
  left_join(upper_output, by=c("time", "state"))

#input population data
pop <- read_csv("data/demographics.csv", 
                         col_types = cols(population_new = col_number()))
pop <- pop %>% select (state, population_new)

#calaculate moving ir and mr
transmission <-  transmission %>% left_join(pop, by="state") %>%
  group_by(state) %>%
  mutate(ir_7day=(rollcases_7d/population_new)*100000,
         mr_7day=(rolldeaths_7d/population_new)*100000) 

#write to the data
write.csv(transmission, "data/transmission.csv")

#hospital capacity
#load data
hospital <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/hospital.csv")
icu <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/icu.csv")
pkrc <- read.csv("https://raw.githubusercontent.com/MoH-Malaysia/covid19-public/main/epidemic/pkrc.csv")

#extract an occupancy rate for each level - state and national
bor_national <- hospital %>% dplyr::select(date, beds, hosp_covid, hosp_pui, hosp_noncovid) %>%
  group_by(date) %>%
  summarise(hosp_beds=sum(beds),
            hosp_covid=sum(hosp_covid),
            hosp_pui=sum(hosp_pui),
            hosp_noncovid=sum(hosp_noncovid),
            hosp=sum(hosp_covid+hosp_pui+hosp_noncovid),
  ) %>%
  mutate(bor=(hosp/hosp_beds)*100,
         state="Malaysia") %>%
  dplyr::select(date, state, bor, hosp_covid, hosp_pui, hosp_noncovid, hosp_beds) 

bor_state <- hospital %>% dplyr::select(date, state, beds, hosp_covid, hosp_pui, hosp_noncovid) %>%
  mutate(bor=((hosp_covid+hosp_pui+hosp_noncovid)/beds)*100) %>%
  dplyr::select(date, state, bor, hosp_covid, hosp_pui, hosp_noncovid, beds) %>%
  rename(date=date, state=state, bor=bor, hosp_covid=hosp_covid, 
         hosp_pui=hosp_pui, hosp_noncovid=hosp_noncovid, hosp_beds=beds)

icu_national <- icu %>% dplyr::select(date, beds_icu_total, icu_covid, icu_noncovid, icu_pui) %>%
  group_by(date) %>%
  summarise(icu_beds=sum(beds_icu_total),
            icu_covid=sum(icu_covid),
            icu_pui=sum(icu_pui),
            icu_noncovid=sum(icu_noncovid),
            icu=sum(icu_covid+icu_pui+icu_noncovid))%>%
  mutate(ior=(icu/icu_beds)*100,
         state="Malaysia") %>%
  dplyr::select(date, state, ior, icu_pui, icu_covid, icu_noncovid, icu_beds)

icu_state <- icu %>% dplyr::select(date, state, beds_icu_total, icu_covid, icu_noncovid, icu_pui) %>%
  mutate(ior=((icu_pui+icu_covid+icu_noncovid)/beds_icu_total)*100) %>%
  dplyr::select(date, state, ior, icu_pui, icu_covid, icu_noncovid, beds_icu_total) %>%
  rename(icu_beds=beds_icu_total)

vent_national <- icu %>% dplyr::select(date, vent, vent_port, vent_pui, vent_covid, vent_noncovid) %>%
  group_by(date) %>%
  summarise(vent_beds=sum(vent+vent_port),
            vent_covid=sum(vent_covid),
            vent_pui=sum(vent_pui),
            vent_noncovid=sum(vent_noncovid),
            ven=sum(vent_covid+vent_pui+vent_noncovid))%>%
  mutate(vor=(ven/vent_beds)*100,
         state="Malaysia") %>%
  dplyr::select(date, state, vor, vent_covid, vent_pui, vent_noncovid, vent_beds)

vent_state <- icu %>% dplyr::select(date, state, vent, vent_port, vent_pui, vent_covid,vent_noncovid) %>%
  mutate(vor=((vent_covid+vent_pui+vent_noncovid)/(vent+vent_port))*100) %>%
  dplyr::select(date, state, vor, vent_covid, vent_pui, vent_noncovid, vent, vent_port)%>%
  mutate(vent_beds=vent+vent_port)

pkrc_national <- pkrc %>% dplyr::select(date, pkrc_covid, pkrc_pui, pkrc_noncovid, beds) %>%
  group_by(date) %>%
  summarise(pkrc=sum(pkrc_covid+pkrc_pui+pkrc_noncovid),
            pkrc_covid=sum(pkrc_covid),
            pkrc_pui=sum(pkrc_pui),
            pkrc_noncovid=sum(pkrc_noncovid),
            pkrc_beds=sum(beds))%>%
  mutate(por=(pkrc/pkrc_beds)*100, 
         state="Malaysia") %>%
  dplyr::select(date, state, por, pkrc_covid, pkrc_pui, pkrc_noncovid, pkrc_beds) 

pkrc_state <- pkrc %>% dplyr::select(date, state, pkrc_covid, pkrc_pui, pkrc_noncovid, beds) %>%
  mutate(por=((pkrc_covid+pkrc_pui+pkrc_noncovid)/beds)*100) %>%
  dplyr::select(date, state, por, pkrc_covid, pkrc_pui, pkrc_noncovid, beds) %>%
  rename(date=date, state=state, por=por, pkrc_covid=pkrc_covid, pkrc_pui=pkrc_pui, 
         pkrc_noncovid=pkrc_noncovid, pkrc_beds=beds)

#merge the state and national rates
hosp_national <- bor_national %>% 
  left_join(icu_national, by=c("date", "state")) %>%
  left_join(vent_national, by=c("date", "state")) %>%
  left_join(pkrc_national, by=c("date", "state")) %>%
  mutate(date=as.Date(date))

hosp_state <- bor_state %>% 
  left_join(icu_state, by=c("date", "state")) %>%
  left_join(vent_state, by=c("date", "state")) %>%
  left_join(pkrc_state, by=c("date", "state"))  %>%
  mutate(date=as.Date(date)) 

#merge together
capacity <- bind_rows(hosp_national, hosp_state)

#get the absolute change from each day compared to 7-days prior
capacity <- capacity %>% group_by(state) %>%
  mutate(ma_7day_bor=rollmean(bor, k = 7, align = "right", fill = NA),
         ma_7day_ior=rollmean(ior, k = 7, align = "right", fill = NA),
         ma_7day_vor=rollmean(vor, k = 7, align = "right", fill = NA),
         change_bor=bor-lag(ma_7day_bor,1),
         change_ior=ior-lag(ma_7day_ior,1),
         change_vor=vor-lag(ma_7day_vor,1))

#write to the data
write.csv(capacity, "data/capacity.csv")

#situational report
#get a weekly summary of key indicators
transmission <- read.csv('data/transmission.csv')
capacity <- read.csv('data/capacity.csv')

transmission_7day <- transmission %>% group_by(state) %>%
  mutate(date=as.Date(date),
         ir_7day=round(as.numeric(ir_7day),1),
         mr_7day=round(as.numeric(mr_7day),1),
         rt=round(as.numeric(rt),1),
         lower=round(as.numeric(lower),1),
         upper=round(as.numeric(upper),1)) %>%
  filter(date>max(date)-14) %>%
  mutate(week=ifelse(date>max(date)-7,1,2)) %>%
  ungroup() %>% as.data.frame() %>%
  group_by(state, week) %>%
  summarise(mean_ir=mean(ir_7day),
            mean_mr=mean(mr_7day),
            mean_rt=mean(rt),
            mean_lower=mean(lower),
            mean_upper=mean(upper)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(perc_change_ir=round(((mean_ir-lag(mean_ir))/lag(mean_ir))*100,1),
         perc_change_mr=round(((mean_mr-lag(mean_mr))/lag(mean_mr))*100,1),
         perc_change_rt=round(((mean_rt-lag(mean_rt))/lag(mean_rt))*100,1),
         mean_rt_full=paste0(round(mean_rt,2), " (", round(mean_lower,1), ", ", round(mean_upper,1), ")")) %>%
  ungroup() %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  replace(is.na(.), 0) %>%
  filter(week==2) %>%
  select(-week) 


capacity_7day <- capacity %>% group_by(state) %>%
  mutate(date=as.Date(date),
         bor=bor/100,
         ior=ior/100,
         vor=vor/100) %>%
  filter(date>max(date)-14) %>%
  mutate(week=ifelse(date>max(date)-7,1,2)) %>%
  ungroup() %>% as.data.frame() %>%
  group_by(state, week) %>%
  summarise(mean_bor=mean(bor),
            mean_ior=mean(ior),
            mean_vor=mean(vor)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(perc_change_bor=round(((mean_bor-lag(mean_bor))/lag(mean_bor))*100,1),
         perc_change_ior=round(((mean_ior-lag(mean_ior))/lag(mean_ior))*100,1),
         perc_change_vor=round(((mean_vor-lag(mean_vor))/lag(mean_vor))*100,1)) %>%
  ungroup() %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  replace(is.na(.), 0) %>%
  mutate(trend_bor=ifelse(perc_change_bor<0, "down", 
                          ifelse(perc_change_bor>0, "up", "unchanged")),
         trend_ior=ifelse(perc_change_ior<0, "down", 
                          ifelse(perc_change_ior>0, "up", "unchanged")),
         trend_vor=ifelse(perc_change_vor<0, "down", 
                          ifelse(perc_change_vor>0, "up", "unchanged"))) %>%
  filter(week==2) %>%
  select(-week)

#join all the sets
epid_report <- left_join(transmission_7day, capacity_7day, by="state") %>%
  replace(is.na(.), 0) %>%
  as.data.frame() %>% mutate_if(is.numeric, round, digits=2)

epid_report_msia <- epid_report %>% filter(state=="Malaysia") %>% mutate(state=as.factor(state))
epid_report_state <- epid_report %>% filter(state!="Malaysia") %>%
  mutate(state=as.factor(state)) %>%
  arrange(desc(mean_ir))
epid_report <- bind_rows(epid_report_msia, epid_report_state)

#write epid report to csv
write.csv(epid_report, "data/epid_report.csv")

