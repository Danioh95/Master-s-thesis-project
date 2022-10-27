library(R.devices)
library(ggforce)
library(rgdal)
library(raster)
library(rgeos)
library(geodist)
library(ggmap)
library(patchwork)
library(lubridate)
library(vroom)
library(jsonlite)
library(readr)
library(ggplot2)
library(treemapify)
library(SAVE)
library(xts)
library(viridis)
library(dplyr)
library(imputeTS)
library(ggridges)
library(multcomp)
library(tidyverse)
library(gridExtra)
library(forecast)
library(vars)
library(snpar)
library(tsfgrnn)
library(nnfor)
library(plotrix)
library(tseries)
library(nnfor)
options(lubridate.week.start = 1)

Sys.setlocale("LC_ALL","Italian")



source("init.R")

# we set the `theme_bw()` as the default theme
# theme_update(                   # we update some elements of the theme:
#  axis.ticks = element_blank(), # axis ticks are not drawn
#  axis.text  = element_blank(), # ticks are not labelled
#  axis.title = element_blank()  # axis will not be labelled
#  )


# Loading of *measurement campaigns*

#We first load the time intervals where measurements data have been validated and are void of interference due to
#the presence of the operator.


orari_monitoraggi <- read_csv2("orari_monitoraggi.csv",
                               col_types = "ccc"
) %>%
  mutate(
    dt_install = ymd_hms(dt_install),
    dt_preleva = ymd_hms(dt_preleva)
  ) %>%
  arrange(id_interve, dt_install)


#We now load the measurement campaigns, selecting only the *validated* measurement intervals indicated in the
#`orari_monitoraggi` tibble.


campagne_monitoraggi <- read_sf(
  dsn = "./shapefiles",
  layer = "centraline",
  quiet = TRUE
) %>%
  st_transform(crs = 4326) %>%
  dplyr::select(-c(gid,minimo:medio, periodo, field_15, starts_with("dt_"))) %>% # cancella campi inutili
  mutate(
    seriale = str_extract(seriale, "\\d+$"),
    id_interve = as.character(id_interve)
    #across(x:y, as.numeric)
  ) %>%
  left_join(orari_monitoraggi, by = "id_interve") %>%
  relocate(starts_with("dt_"), .after = id_interve) %>%
  arrange(id_interve)



#Since we want to run analysis that take into account the type of day (which day of the week it is, whether it is a *weekday* or *weekend*, a *working* or *non-working*, or if there is a national *holiday* that turns a weekday into a non working one) we first gather the monitored years in the `monitored_years` tibble and then we create the `holiday_df` that contains the holidays for each day of the monitored years.


monitored_years <- campagne_monitoraggi %>%
  as_tibble() %>%
  summarise(
    from = min(dt_install),
    to = max(dt_preleva),
  ) %>%
  mutate(
    across(everything(), year),
    years = list(seq.int(from, to, 1))
  ) %>%
  dplyr::select(years) %>%
  unnest(years) %>%
  pull()

URLs_json <- paste0(
  "https://date.nager.at/api/v2/publicholidays/",
  monitored_years, "/IT"
)

holidays_df <- map_dfr(URLs_json, ~ fromJSON(.x)) %>%
  as_tibble() %>%
  filter(type == "Public") %>%
  dplyr::select(date, localName, name) %>%
  mutate(
    date = as.Date(date),
    across(.cols = -date, factor)
  )


# Measurement campaigns data

#We can now load the measurement data for each monitoring campaign, recalling that each is fragmented into multiple files typically spanning one month. A file can contain only a portion of a month if the monitoring has started (ended) after (before) the beginning (end) of a month.


monitoraggi <- list.files(
  path = "./dati",
  pattern = "^POSTAZIONE.+\\.TXT",
  full.names = TRUE
) %>%
  tibble(filenames = .) %>%
  rowwise() %>%
  mutate(
    sommario = list(estrai_sommario(filenames))
  ) %>%
  unnest(sommario) %>%
  left_join(campagne_monitoraggi, ., by = "seriale") %>%
  arrange(id_interve, inizio, fine) %>%
  filter(inizio >= as.Date(dt_install), fine <= as.Date(dt_preleva)) %>%
  group_by(id_interve) %>%
  mutate(
    manca_monitoraggio = if_else(min(inizio) > as.Date(dt_install) | max(fine) < as.Date(dt_preleva), TRUE, FALSE)
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    misure = list(importa_monitoraggio(filenames, codice, seriale))
  ) %>%
  unnest(misure) %>%
  group_by(id_interve) %>%
  filter(between(DateTime, min(dt_install), max(dt_preleva))) %>%
  mutate(across(starts_with(c("RSM_W", "Peak")), ~ .x / max(.x), .names = "{.col}_norm")) %>%
  ungroup() %>%
  relocate(DateTime, .after = Time) %>%
  dplyr::select(-c(Date, Time, inizio, fine)) %>%
  mutate(
    Year = year(DateTime),
    Month = month(DateTime, label = TRUE, abbr = TRUE),
    Day = day(DateTime),
    Hour = hour(DateTime),
    Weekday = wday(DateTime, label = TRUE, abbr = TRUE),
    Weekend = case_when(
      Weekday %in% c("sab", "dom") ~ "Yes",
      TRUE ~ "No"
    ),
    date = as.Date(DateTime)
  ) %>%
  left_join(holidays_df, by = "date") %>%
  dplyr::select(-date) %>%
  mutate(
    Holiday = case_when(
      !is.na(localName) | Weekend == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    across(c(Year, Hour, Day, Weekend, Holiday), factor)
  )

to_be_excluded <- monitoraggi %>% 
  filter(manca_monitoraggio == TRUE) %>% 
  group_by(id_interve) %>% 
  summarize(n = n()/240) %>% pull(id_interve)

monitoraggi <- monitoraggi %>% 
  filter(!id_interve %in% to_be_excluded)



#Definiamo il nostro monitoraggi

monitoraggi <- monitoraggi %>%
  dplyr::select(id_interve,DateTime,RMS_W,Peak_W,Year,Month,Day,Hour,Weekday,Weekend,Holiday)

st_geometry(monitoraggi)<-NULL

moni_na <- na_ma(x=monitoraggi$RMS_W, k = 3, weighting = "exponential",maxgap = Inf)
monitoraggi$RMS_W <- as.numeric(moni_na)

moni_na <- na_ma(x=monitoraggi$Peak_W, k = 3, weighting = "exponential",maxgap = Inf)
monitoraggi$Peak_W <- as.numeric(moni_na)

#andamento RMS per id_interve dopo la sostituzione dei valori mancanti
monitoraggi %>%
  ggplot( aes(x=DateTime, RMS_W))+
  geom_line(aes(color=Holiday,group=1))+
  labs(x=NULL)+
  facet_wrap(~id_interve,scales = "free")

#aggiunta valori normalizzati
monitoraggi<- monitoraggi%>%
  group_by(id_interve)%>%
  mutate(RMS_W_norm = (RMS_W-min(RMS_W))/(max(RMS_W)-min(RMS_W)),Peak_W_norm = (Peak_W-min(Peak_W))/(max(Peak_W)-min(Peak_W)))

#In quali stazioni i valori di RMS intesi come media sulle 24 ore risultano oltre il limite di legge?
oltre_limite <- monitoraggi %>%
  group_by(id_interve,Day) %>%
  summarise(mean=sqrt(mean(RMS_W^2))) %>%
  filter(mean > 6)

#monitoraggi puntuali per id_interve
monitoraggi_puntuali_per_id_interve <- monitoraggi%>%
  group_by(id_interve)%>%
  summarize(mean_RMS_W = round(sqrt(mean(RMS_W^2)),2),median_RMS_W = median(RMS_W),sd_RMS_W = sd(RMS_W),var_RMS_W = var(RMS_W),min_RMS_W = min(RMS_W),max_RMS_W=max(RMS_W),range_RMS_W=(max_RMS_W -min_RMS_W),osservazioni=n(), giorni_di_osservazione= round(osservazioni/240,2)) %>%
  arrange(desc(mean_RMS_W)) 

#In quali stazioni i valori di RMS_W inteso come media ogni 6 minuti eccedono 6 V/m ed in che percentuale?
oltre_limite <-monitoraggi %>%
  group_by(id_interve) %>%
  filter(RMS_W > 6) %>%
  summarise(over_limit =n()) %>%
  left_join(monitoraggi_puntuali_per_id_interve,"id_interve")%>%
  mutate(over_limit_percentuale = round(over_limit/osservazioni*100,1)) %>%
  ggplot(aes(over_limit_percentuale, reorder(id_interve,over_limit_percentuale)))+
  geom_bar(stat = 'identity')+
  labs(x=NULL,y=NULL)+
  geom_text(aes(label=paste0(over_limit_percentuale,"%")), position=position_stack(vjust=0.5)) +
  theme_bw() 

ggsave("Oltre_limite.png",
       plot = oltre_limite, width = 15, height= 9.27)


#Quante osservazioni abbiamo per ogni id_interve?
monitoraggi_puntuali_per_id_interve %>%
  ggplot(aes(giorni_di_osservazione,reorder(id_interve,osservazioni)))+
  geom_bar(stat = 'identity')+
  labs(y=NULL)+
  geom_text(aes(label=giorni_di_osservazione), position=position_stack(vjust=0.5)) +
  theme_bw() 



#Bar chart medie per id_interve
monitoraggi_puntuali_per_id_interve%>%
  ggplot(aes(mean_RMS_W,reorder(id_interve,mean_RMS_W),fill = sd_RMS_W))+
  geom_bar(stat = "identity")+
  labs(y=NULL)+
  stat_summary(fun=mean, geom="point", shape=20, size=4)+
  geom_errorbar(aes(xmin = min_RMS_W, 
                    xmax = max_RMS_W, group=1),
                width = .1)+
  theme_bw() 





#Bar chart medie per Holiday e per tutti i id_interve
monitoraggi %>%
  group_by(id_interve,Holiday) %>%
  summarize(mean_RMS_W= sqrt(mean(RMS_W^2)),sd_RMS_W = sd(RMS_W),min_RMS_W=min(RMS_W),max_RMS_W=max(RMS_W)) %>%
  ggplot(aes(Holiday,mean_RMS_W,fill = sd_RMS_W))+
  geom_bar(stat="identity")+
  stat_summary(fun=mean, geom="point", shape=20, size=4)+
  geom_errorbar(aes(ymin = min_RMS_W, 
                    ymax = max_RMS_W, group=1),
                width = .1)+
  facet_wrap(~id_interve)



#Bar chart medie per weekend e non per tutti i id_interve
monitoraggi %>%
  group_by(id_interve,Weekend) %>%
  summarize(mean_RMS_W= sqrt(mean(RMS_W^2)),sd_RMS_W = sd(RMS_W),min_RMS_W=min(RMS_W),max_RMS_W=max(RMS_W)) %>%
  ggplot(aes(Weekend,mean_RMS_W,fill = sd_RMS_W))+
  geom_bar(stat="identity")+
  stat_summary(fun=mean, geom="point", shape=20, size=4)+
  geom_errorbar(aes(ymin = min_RMS_W, 
                    ymax = max_RMS_W, group=1),
                width = .1)+
  facet_wrap(~id_interve)

#bar chart del coefficiente di variazione
cvmoni<-monitoraggi %>%
  group_by(id_interve) %>%
  summarize(mean_RMS= sqrt(mean(RMS_W^2)), sd_RMS= sd(RMS_W), cv= sd_RMS/mean_RMS)

cvmoni%>% 
  ggplot(aes(y= reorder(id_interve,cv),x=cv))+
  geom_bar(stat="identity")


#bar chart coefficiente di variazione e medie per id_interve
cvmoni %>%
  ggplot(aes(y= reorder(id_interve,cv),x=cv))+
  geom_bar(stat="identity")+
  geom_point(aes(y=id_interve,x=sd_RMS), color="red") +
  geom_point(aes(y=id_interve,x=mean_RMS), color="blue")  



#Bar chart medie per Weekday per tutti i id_interve
monitoraggi %>%
  group_by(id_interve,Weekday) %>%
  summarize(mean_RMS_W = sqrt(mean(RMS_W^2)),sd_RMS_W= sd(RMS_W),min_RMS_W=min(RMS_W),max_RMS_W=max(RMS_W)) %>%
  ggplot(aes(Weekday,mean_RMS_W,fill = sd_RMS_W))+
  geom_bar(stat="identity")+
  stat_summary(fun=mean, geom="point", shape=20, size=4)+
  geom_errorbar(aes(ymin = min_RMS_W, 
                    ymax = max_RMS_W, group=1),
                width = .2)+
  labs(X=NULL)+
  facet_wrap(~id_interve)



#Bar chart medie orarie per tutti i id_interve
monitoraggi %>%
  group_by(id_interve,Hour) %>%
  summarize(mean_RMS_W = sqrt(mean(RMS_W^2)),sd_RMS_W = sd(RMS_W),min_RMS_W=min(RMS_W),max_RMS_W=max(RMS_W)) %>%
  ggplot(aes(Hour,mean_RMS_W,fill = sd_RMS_W))+
  geom_bar(stat="identity")+
  stat_summary(fun=mean, geom="point", shape=20, size=4)+
  geom_errorbar(aes(ymin = min_RMS_W, 
                    ymax = max_RMS_W, group=1),
                width = .4)+
  facet_wrap(~id_interve)



#andamento RMS_W per tutti i id_interve
monitoraggi %>%
  ggplot( aes(x=DateTime, RMS_W))+
  geom_line(aes(color=Holiday,group=1))+
  labs(x=NULL)+
  facet_wrap(~id_interve,scales = "free")



#grafico per evidenziare la media oraria e giornaliera per tutti i id_interve
monitoraggi %>%
  group_by(id_interve,Weekday, Hour) %>%
  summarize(media_RMS_W_normalized = sqrt(mean(RMS_W_norm^2))) %>%
  ggplot(aes(x = Hour, y = Weekday)) +
  geom_raster(aes(fill = media_RMS_W_normalized)) +
  labs(x = NULL,y = NULL)+
  facet_wrap(vars(id_interve))+
  scale_fill_viridis()



#density plot
#density plot per ogni id_interve
density_per_id_interve <-ggplot(monitoraggi,aes(x = RMS_W)) +
  geom_density(fill="Blue2", alpha=.5)+ 
  ggtitle(label="Density plot of RMS_W")+
  facet_wrap(~id_interve, scales= "free")

ggsave("density_per_id_interve.png",
       plot =density_per_id_interve, width = 15, height= 9.27)



#density plot per holiday per ogni id_interve
ggplot(monitoraggi,aes(x = RMS_W_norm, y = Holiday,fill= Holiday,),alpha = 0.1) +
  geom_density_ridges()+
  ggtitle(label="Density plot by weekend of RMS_W normalized")+
  facet_wrap(~id_interve)



#ANOVA_by_Holiday
anova_Holiday <- monitoraggi %>%
  nest_by(.key = "df") %>%
  mutate(
    tabella = list(
      lm(RMS_W~Holiday,data= df)%>%
        anova()
    ))
anova_Holiday$tabella


#density plot per weekday per ogni id_interve
ggplot(monitoraggi,aes(x = RMS_W_norm, y = Weekday)) +
  geom_density_ridges()+
  ggtitle(label="Density plot by weekdays of RMS_W normalized")+
  labs(y = NULL)+
  facet_wrap(~id_interve)



# anova_by_weekdays
anova_Weekday <- monitoraggi %>%
  nest_by(.key = "df") %>%
  mutate(
    tabella = list(
      lm(RMS_W~Weekday,data= df)%>%
        anova()
    ))

anova_Weekday$tabella


#density plot per ore e per ogni id_interve
density_plot_by_hour <- ggplot(monitoraggi,aes(x = RMS_W, y = Hour),alpha = 0.1) +
  geom_density_ridges(fill="Blue2", alpha=.5)+ 
  ggtitle(label="Density plot by hour of RMS_W")+
  facet_wrap(~id_interve, scales="free")


ggsave("density_by_hour.png",
       plot =density_plot_by_hour, width = 15, height= 9.27)

anova_Hour <- monitoraggi %>%
  nest_by(.key = "df") %>%
  mutate(
    tabella = list(
      lm(RMS_W~Hour,data= df)%>%
        anova()
    ))

anova_Hour$tabella

#Box-plot
#Box-plot per ogni id_interve
ggplot(monitoraggi,aes(x = 1,y = RMS_W_norm))+
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2) + 
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  facet_wrap(~id_interve)


ggplot(monitoraggi,aes(y = reorder(id_interve, RMS_W, fun=mean),x = RMS_W))+
  geom_boxplot(fill= "Blue", alpha=0.3,width = .3) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="Blue2", fill="Blue2")+
  labs(y=NULL)+
  coord_flip()

ggsave("Boxplot_per_id_interve.png",
       plot =box_plot_per_id_interve, width = 15, height= 9.27)



#Box-plot per holiday per ogni id_interve 
monitoraggi %>% filter(id_interve %in% c(12111,12145,12168,12308)) %>%
ggplot(aes(x = Holiday,y = RMS_W,fill=Holiday))+
  geom_boxplot(alpha=0.6) +
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="Black", fill="Black")+
  facet_wrap(~id_interve, scales= "free")

ggsave("Box_plot_per_holiday.png",
       plot =Box_plot_per_holiday, width = 15, height= 9.27)

#Box-plot per weekday per ogni id_interve 
Box_plot_per_weekday <- ggplot(monitoraggi,aes(x = Weekday,y = RMS_W))+
  geom_boxplot(fill= "Blue", alpha=0.3,width = .3) +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="Blue2", fill="Blue2")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  facet_wrap(~id_interve,scales="free")


ggsave("Box_plot_per_weekday.png",
       plot =Box_plot_per_weekday, width = 15, height= 9.27)


#Box-plot per ore e per ogni id_interve   
Box_plot_by_hour <- ggplot(monitoraggi,aes(x = Hour,y= RMS_W))+
  geom_boxplot(fill= "Blue", alpha=0.3,width = .3) +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="Blue2", fill="Blue2")+
  facet_wrap(~id_interve, scales= "free")


ggsave("Box_plot_by_hour.png",
       plot =Box_plot_by_hour, width = 15, height= 9.27)


#analisi dei picchi
#andamento valori di picco confrontato con andamento RMS_W per tutti i id_interve 
monitoraggi %>%
  ggplot( aes(x=DateTime))+
  geom_line(aes(y=Peak_W),color="Red3")+
  geom_line(aes(y=RMS_W), color= "Blue4")+
  facet_wrap(~id_interve,scales = "free")

#monitoraggi puntuali picchi tabella
monitoraggi_puntuali_picchi <- monitoraggi%>%
  group_by(id_interve)%>%
  summarize(mean_Peak_W = sqrt(mean(Peak_W^2)),median_Peak_W = median(Peak_W),sd_Peak_W = sd(Peak_W),var_Peak_W = var(Peak_W),min_Peak_W = min(Peak_W),max_Peak_W=max(Peak_W),range_Peak_W= (max_Peak_W - min_Peak_W),osservazioni=n()) %>%
  arrange(desc(mean_Peak_W))

#density plot per ogni id_interve
ggplot(monitoraggi)+
  geom_density(aes(RMS_W),fill="Blue4",alpha=0.5)+
  geom_density(aes(Peak_W),fill= "Red3",alpha=0.5)+
  facet_wrap(~id_interve,scales= "free")


#density plot per ogni id_interve e holiday
ggplot(monitoraggi)+
  geom_density(aes(RMS_W),fill="Blue4",alpha=0.5)+
  geom_density(aes(Peak_W),fill= "Red3",alpha=0.5)+
  facet_grid(id_interve~Holiday,scales = "free")


#Box-plot per weekday per ogni id_interve 
ggplot(monitoraggi,aes(x = Weekday))+
  geom_boxplot(aes(y=Peak_W), fill="Red", alpha=0.3,width = .3)+
  stat_summary(aes(y=Peak_W,group=1),fun=mean, geom="line",size=1, color="red",alpha=0.3)+
  geom_boxplot(aes(y=RMS_W), fill="Blue", alpha=0.3,width = .3)+
  stat_summary(aes(y=RMS_W,group=1),fun=mean, geom="line",size=1, color="Blue",alpha=0.3)+
  facet_wrap(~id_interve,scales = "free")


#Box-plot per ore e per ogni id_interve
ggplot(monitoraggi,aes(x = Hour))+
  geom_boxplot(aes(y=Peak_W), fill="Red", alpha=0.3,width = .3)+
  stat_summary(aes(y=Peak_W,group=1),fun=mean, geom="line",size=1, color="red",alpha=0.3)+
  geom_boxplot(aes(y=RMS_W), fill="Blue", alpha=0.3,width = .3)+
  stat_summary(aes(y=RMS_W,group=1),fun=mean, geom="line",size=1, color="Blue",alpha=0.3)+
  facet_wrap(~id_interve,scales = "free")

#QQplot
ggplot(monitoraggi, aes(sample=RMS_W))+
  stat_qq()+
  stat_qq_line()+
  facet_wrap(~id_interve,scales="free")

#Raggruppiamo i dati con frequenze diverse
monitoraggi <- monitoraggi %>%
  mutate(freq=as.factor("6 min"))

#12 min 
monitoraggi12 <- monitoraggi %>% 
  group_by(id_interve,Time_step = cut(DateTime, breaks = "12 min"))%>%
  summarize(mean = sqrt(mean(RMS_W^2)),sd = sd(RMS_W)) %>% 
  mutate(RMS_W_norm = round((mean-min(mean))/(max(mean)-min(mean)),2),
         freq=as.factor("12 min"),RMS_W = mean, DateTime = as.POSIXct(Time_step, tz = "CET"))


#30 min
monitoraggi30 <- monitoraggi %>% 
  group_by(id_interve,Time_step = cut(DateTime, breaks = "30 min"))%>%
  summarize(mean = sqrt(mean(RMS_W^2)),sd = sd(RMS_W)) %>% 
  mutate(RMS_W_norm = round((mean-min(mean))/(max(mean)-min(mean)),2),
         freq=as.factor("30 min"),RMS_W = mean, DateTime = as.POSIXct(Time_step, tz = "CET"))


#1 hour
monitoraggi1h <- monitoraggi %>% 
  group_by(id_interve,Time_step = cut(DateTime, breaks = "1 hour"))%>%
  summarize(mean = sqrt(mean(RMS_W^2)),sd = sd(RMS_W)) %>% 
  mutate(RMS_W_norm = round((mean-min(mean))/(max(mean)-min(mean)),2),
         freq=as.factor("1 hour"),RMS_W = mean, DateTime = as.POSIXct(Time_step, tz = "CET"))

#2 hour
monitoraggi2h <- monitoraggi %>% 
  group_by(id_interve,Time_step = cut(DateTime, breaks = "2 hour"))%>%
  summarize(mean = sqrt(mean(RMS_W^2)),sd = sd(RMS_W)) %>% 
  mutate(RMS_W_norm = round((mean-min(mean))/(max(mean)-min(mean)),2),
         freq=as.factor("2 hour"),RMS_W = mean, DateTime = as.POSIXct(Time_step, tz = "CET"))
#4 hour
monitoraggi4h <- monitoraggi %>% 
  group_by(id_interve,Time_step = cut(DateTime, breaks = "4 hour"))%>%
  summarize(mean = sqrt(mean(RMS_W^2)),sd = sd(RMS_W)) %>% 
  mutate(RMS_W_norm = round((mean-min(mean))/(max(mean)-min(mean)),2),
         freq=as.factor("4 hour"),RMS_W = mean, DateTime = as.POSIXct(Time_step, tz = "CET"))

#6 hour
monitoraggi6h <- monitoraggi %>% 
  group_by(id_interve,Time_step = cut(DateTime, breaks = "6 hour"))%>%
  summarize(mean = sqrt(mean(RMS_W^2)),sd = sd(RMS_W)) %>% 
  mutate(RMS_W_norm = round((mean-min(mean))/(max(mean)-min(mean)),2),
         freq=as.factor("6 hour"),RMS_W = mean, DateTime = as.POSIXct(Time_step, tz = "CET"))
#8 hour
monitoraggi8h <- monitoraggi %>% 
  group_by(id_interve,Time_step = cut(DateTime, breaks = "8 hour"))%>%
  summarize(mean = sqrt(mean(RMS_W^2)),sd = sd(RMS_W)) %>% 
  mutate(RMS_W_norm = round((mean-min(mean))/(max(mean)-min(mean)),2),
         freq=as.factor("8 hour"),RMS_W = mean, DateTime = as.POSIXct(Time_step, tz = "CET"))

#12 hour
monitoraggi12h <- monitoraggi %>% 
  group_by(id_interve,Time_step = cut(DateTime, breaks = "12 hour"))%>%
  summarize(mean = sqrt(mean(RMS_W^2)),sd = sd(RMS_W)) %>% 
  mutate(RMS_W_norm = round((mean-min(mean))/(max(mean)-min(mean)),2),
         freq=as.factor("12 hour"),RMS_W = mean, DateTime = as.POSIXct(Time_step, tz = "CET"))

#24 hour
monitoraggi24h <- monitoraggi %>% 
  group_by(id_interve,Time_step = cut(DateTime, breaks = "24 hour"))%>%
  summarize(mean = sqrt(mean(RMS_W^2)),sd = sd(RMS_W)) %>% 
  mutate(RMS_W_norm = round((mean-min(mean))/(max(mean)-min(mean)),2),
         freq=as.factor("24 hour"),RMS_W = mean, DateTime = as.POSIXct(Time_step, tz = "CET"))


monitoraggi_tot <- rbind(monitoraggi24h, monitoraggi12h,monitoraggi8h,monitoraggi6h,monitoraggi4h,monitoraggi2h,monitoraggi1h,monitoraggi30,monitoraggi12,monitoraggi) %>%
  dplyr::select(-c(mean,Time_step))

#calcolo misure per ogni intervallo di media
monitoraggivalue <- monitoraggi_tot%>%
  group_by(freq,id_interve)%>%
  summarise(meanfr = sqrt(mean(RMS_W^2)),sdf = sd(RMS_W), stderr= std.error(RMS_W))


#mean
mean <-monitoraggivalue %>%
  ggplot(aes(freq,meanfr,group=1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  facet_wrap(~id_interve)


ggsave("mean.png",
       plot = mean, width = 15, height= 9.27)
#sd
sd <-monitoraggivalue %>%
  ggplot(aes(freq,sdf,group=1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  facet_wrap(~id_interve)

ggsave("sd.png",
       plot = sd, width = 15, height= 9.27)
#std.error  
std_error <-monitoraggivalue %>%
  ggplot(aes(freq,stderr,group=1))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  facet_wrap(~id_interve)

ggsave("std_error.png",
       plot = std_error, width = 15, height= 9.27)

#error bar
ggplot(monitoraggivalue, 
       aes(x = freq, 
           y = meanfr, 
           group = 1)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = meanfr - sdf, 
                    ymax = meanfr + sdf), 
                width = .1) +
  facet_wrap(~id_interve)


#indice freq diverse
m<- rbind(monitoraggi12,monitoraggi30,monitoraggi1h,monitoraggi2h,monitoraggi4h,monitoraggi6h,monitoraggi8h,monitoraggi12h)

ggplot(subset(m,id_interve == 11907),aes(DateTime,y=sd))+
  geom_line()+
  facet_wrap(~freq)

moni_na <- na_ma(x=monitoraggi$RMS_W, k = 3, weighting = "exponential",maxgap = Inf)
monitoraggi$RMS_W <- as.numeric(moni_na)
ma<- na_ma(m$sd, k = 3, weighting = "exponential",maxgap = Inf)
m$sd <-   as.numeric(ma)

sdmedia<- m%>%
  group_by(freq)%>%
  summarise(mediasd = mean(sd))

ggplot(sdmedia,aes(freq,mediasd))+
  geom_point()+
  geom_line(aes(freq,mediasd,group=1))+
  geom_label(aes(label = round(mediasd, 3))
             ,
             nudge_y=0.02)



#confronto distribuzioni tra valori normalizzati e non normalizzati a parità di frequenza: come cambiano le distribuzioni?
#density tra norm e non norm con frequenza di 6 min
ggplot(subset(monitoraggi_tot,freq == "6 min"))+
  geom_density(aes(x=RMS_W,fill="blue2",alpha=.4))+
  geom_density(aes(x=RMS_W_norm,fill="red2",alpha=.4))+
  facet_wrap(vars(id_interve),scales="free")

#ecdf tra norm e non norm con frequenza di 6 min
ggplot(subset(monitoraggi_tot,freq == "6 min"))+
  stat_ecdf(aes(x=RMS_W),color="red") +
  stat_ecdf(aes(x=RMS_W_norm),color="blue") +
  geom_hline(yintercept = 0.9) +
  facet_wrap(vars(id_interve),scales="free")+
  theme_bw()

#come cambiano le misure puntuali prima e dopo la normalizzazione?
#misure puntuali valori non normalizzati
monitoraggi_puntuali_norm <- monitoraggi%>%
  group_by(id_interve)%>%
  summarize(mean_RMS_W = round(sqrt(mean(RMS_W^2)),2),median_RMS_W = median(RMS_W),sd_RMS_W = sd(RMS_W),var_RMS_W = var(RMS_W),cv=sd_RMS_W/mean_RMS_W, min_RMS_W = min(RMS_W),max_RMS_W=max(RMS_W),range_RMS_W=(max_RMS_W -min_RMS_W)) %>%
  arrange(desc(mean_RMS_W))

#misure puntuali valori normalizzati
monitoraggi_puntuali_non_norm <- monitoraggi%>%
  group_by(id_interve)%>%
  summarize(mean_RMS_W = round(sqrt(mean(RMS_W_norm^2)),2),median_RMS_W = median(RMS_W_norm),sd_RMS_W = sd(RMS_W_norm),var_RMS_W = var(RMS_W_norm),cv=sd_RMS_W/mean_RMS_W, min_RMS_W = min(RMS_W_norm),max_RMS_W=max(RMS_W_norm),range_RMS_W=(max_RMS_W -min_RMS_W)) %>%
  arrange(desc(mean_RMS_W))

monitoraggi_puntuali_norm_e_non <- rbind(monitoraggi_puntuali_norm,monitoraggi_puntuali_non_norm ) %>% arrange(id_interve)

#BOX-PLOT tra norm e non norm con frequenza di 6 min
ggplot(subset(monitoraggi_tot,freq = "6 min"))+
  geom_boxplot(aes(y=RMS_W,x=0,fill="red"))+
  stat_summary(aes(y=RMS_W,x=0),fun=mean, geom="point", shape=20, size=4, color="blue", fill="blue")+
  geom_boxplot(aes(y=RMS_W_norm,x=1,fill="lue"))+
  stat_summary(aes(y=RMS_W_norm,x=1),fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  facet_wrap(vars(id_interve),scales="free")


#ragioniamo ora tra le diverse frequenze
#ecdf valori assoluti per tutte le frequenze di musira
ecdf_fino_a_2ore <- monitoraggi_tot%>%
  filter(freq %in% c("6 min","12 min","30 min","1 hour","2 hour"))%>%
  ggplot(aes(x =RMS_W,group=freq,geom="line",color=freq)) +
  stat_ecdf() +
  geom_hline(yintercept = 0.9) +
  labs(y=NULL)+
  facet_wrap(vars(id_interve),scales="free")+
  theme_bw()

ggsave("ecdf_fino_a_2ore.png",
       plot = ecdf_fino_a_2ore, width = 15, height= 9.27)

#density valori assoluti per tutte le frequenze di misura
density_fino_a_2ore <- monitoraggi_tot %>%
  filter(freq %in% c("6 min","12 min","30 min","1 hour","2 hour"))%>%
  ggplot(aes(x =RMS_W,group=freq,fill=freq)) +
  geom_density(alpha=0.2) +
  facet_wrap(vars(id_interve),scales="free")+
  theme_bw()

ggsave("density_fino_a_2ore.png",
       plot =density_fino_a_2ore, width = 15, height= 9.27)

#ecdf valori normalizzati per tutte le frequenze di misura
ggplot(monitoraggi_tot,aes(x =RMS_W_norm,group=freq,geom="line",color=freq)) +
  stat_ecdf() +
  geom_hline(yintercept = 0.9) +
  facet_wrap(vars(id_interve),scales="free")+
  theme_bw()

#ecdf valori normalizzati per diverse frequenze
ggplot(monitoraggi_tot,aes(x =RMS_W_norm,group=id_interve,geom="line",color=id_interve)) +
  stat_ecdf() +
  geom_hline(yintercept = 0.9) +
  facet_wrap(vars(freq),scales="free")+
  theme_bw()

#density valori normalizzati
ggplot(monitoraggi_tot,aes(x =RMS_W_norm,group=freq,fill=freq,alpha=0.3)) +
  geom_density() +
  facet_wrap(vars(id_interve),scales="free")+
  theme_bw()


#Kolmogorov-Smirnov test
#i dati s e w provengono dalla stessa distribuzione?

s <- monitoraggi_tot%>%
  filter(freq=="6 min",id_interve=="12349")
w <- monitoraggi_tot%>%
  filter(freq=="2 hour",id_interve=="12349")  

ks.test(s$RMS_W,w$RMS_W)

##ANALISI DELLE SERIE STORICHE


#time plot
#andamento RMS_W per tutti i id_interve
monitoraggi %>% filter( id_interve== c(12111,12145,12168,12308)) %>%
  ggplot( aes(x=DateTime, RMS_W))+
  geom_line(color="Blue4")+
  labs()+
  facet_wrap(~id_interve,scales = "free")

ggsave("time_plot_mini.png",
       plot = last_plot(), width = 15, height= 9.27)

#divisione in train e test per 27 stazioni
dataframe <- monitoraggi %>% 
  nest(.key = "misure") %>%
  mutate (
    ##serie storiche e suddivisione in training e testing
    MISURE_24 = map(misure,~filter(.x,row_number() <= 10*24*24)),
    RMS_NA = map(MISURE_24,~ifelse(.x$RMS_W %in% boxplot.stats(.x$RMS_W)$out,NA,.x$RMS_W)),
    RMS_WO = map(RMS_NA,~ na_ma(.x, k = 3, weighting = "exponential",maxgap = Inf)),
    RMS_WO_norm = map(RMS_WO,~scale(.x,min(.x),max(.x)-min(.x))),
    RMS_W_norm = map(RMS_WO_norm,~as.numeric(.x)),
    SERIE = map(RMS_W_norm,~msts(.x,seasonal.periods = c(60,80,120,240))),
    TRAIN = map(SERIE,~window(.x,start = c(19,1),end = c(21,240))),
    TEST = map(SERIE,~window(.x,start = c(22,1),end = c(24,240))))
    # MSTS_DETREND = map(SERIE,~detrend.series(.x, method = "Spline")),#Spline è un metodo utilizzato per detrendizzare la serie storica)
    # REDF.DAT = map(MSTS_DETREND,~redfit(.x , nsim=100)))


#grafico train e test
dataframe_test<- dataframe%>% 
  unnest(c(TEST)) %>% 
  mutate(time= c(5041:5760))


dataframe_train <- dataframe %>%
  unnest(TRAIN) %>%  
  dplyr::select(TRAIN) %>%
  mutate(time= c(1:5040))

colors <- c("TRAIN" = "gray45", "TEST" = "cadetblue") 
train_test <-ggplot()+
  geom_line(data = dataframe_train,aes(x=time,y=TRAIN,color="TRAIN"))+
  geom_line(data = dataframe_test,aes(x=time,y=TEST,color="TEST"))+
  labs(color = "Legend")+
  scale_color_manual(values = colors)+
  facet_wrap(~id_interve)


ggsave("train_test.png",
       plot = train_test, width = 15, height= 9.27)

#seasonal plot
dataframe <- dataframe %>%
  mutate(SEASONPLOT= map(TRAIN,~ggseasonplot(.x,year.labels=T)+ ggtitle((order_by=id_interve))))

seasonplot <-grid.arrange(dataframe$SEASONPLOT[[1]],dataframe$SEASONPLOT[[2]],dataframe$SEASONPLOT[[3]],
             dataframe$SEASONPLOT[[4]], dataframe$SEASONPLOT[[5]],dataframe$SEASONPLOT[[6]],
             dataframe$SEASONPLOT[[7]],dataframe$SEASONPLOT[[8]],dataframe$SEASONPLOT[[9]],
             dataframe$SEASONPLOT[[10]],dataframe$SEASONPLOT[[11]],dataframe$SEASONPLOT[[12]],dataframe$SEASONPLOT[[13]],
             dataframe$SEASONPLOT[[14]],dataframe$SEASONPLOT[[15]],dataframe$SEASONPLOT[[16]],dataframe$SEASONPLOT[[17]],
             dataframe$SEASONPLOT[[18]],dataframe$SEASONPLOT[[19]],dataframe$SEASONPLOT[[20]],dataframe$SEASONPLOT[[21]],
             dataframe$SEASONPLOT[[22]],dataframe$SEASONPLOT[[23]],dataframe$SEASONPLOT[[24]],dataframe$SEASONPLOT[[25]],
             dataframe$SEASONPLOT[[26]],dataframe$SEASONPLOT[[27]])


ggsave("seasonplot.png",
       plot = seasonplot, width = 15, height= 9.27)

#analisi trend per tutti id_interve
trend <- monitoraggi %>%
  nest_by(.key = "df") %>%
  mutate(
    tabella = list(
      lm(RMS_W~DateTime,data= df)
    ))

coef(trend$tabella[[26]])

#test Cox-Stewart per significatività del trend
test_trend <- monitoraggi %>%
  nest_by(.key = "df") %>%
  mutate(
    test= list(
      cs.test(df$RMS_W)
    ))
test_trend$test

#grafico trend per tutti gli id_interve
ggplot(monitoraggi,aes(DateTime,RMS_W))+
  geom_line()+
  stat_smooth(method = "lm",col= "red")+
  facet_wrap(~id_interve, scales="free")

#periodogramma per tutti id_interve
yrs.period <- rev(c((86400)))
yrs.labels <- rev(c("1Day"))
spettrogramma <- monitoraggi %>%
  ungroup()%>%
  relocate(RMS_W, .after = id_interve) %>%
  arrange(DateTime)%>%
  nest_by(id_interve, .key = "misure") %>%
  mutate(
    misure_new = list(
      misure %>%
        dplyr::select(DateTime, RMS_W) %>%
        xts(x = .$RMS_W, order.by = .$DateTime) %>%
        spec.pgram())
  )%>%
  mutate(
    grafico = list(
      data.frame(freq = misure_new$freq, spec = misure_new$spec)%>%
        ggplot() + geom_line(aes(x =  1/freq, y = spec))+
        scale_x_log10( "Period",breaks = yrs.period, labels = yrs.labels) + scale_y_log10()
      + ggtitle((order_by=id_interve)) 
    )
  )

#grafico spettrogramma per ogni id_interve
spettrogramma <-grid.arrange(spettrogramma$grafico[[1]],spettrogramma$grafico[[2]],spettrogramma$grafico[[3]],
             spettrogramma$grafico[[4]], spettrogramma$grafico[[5]],spettrogramma$grafico[[6]],
             spettrogramma$grafico[[7]],spettrogramma$grafico[[8]],spettrogramma$grafico[[9]],
             spettrogramma$grafico[[10]],spettrogramma$grafico[[11]],spettrogramma$grafico[[12]],spettrogramma$grafico[[13]],
             spettrogramma$grafico[[14]],spettrogramma$grafico[[15]],spettrogramma$grafico[[16]],spettrogramma$grafico[[17]],
             spettrogramma$grafico[[18]],spettrogramma$grafico[[19]],spettrogramma$grafico[[20]],spettrogramma$grafico[[21]],
             spettrogramma$grafico[[22]],spettrogramma$grafico[[23]],spettrogramma$grafico[[24]],spettrogramma$grafico[[25]],
             spettrogramma$grafico[[26]],spettrogramma$grafico[[27]])

ggsave("spettrogramma.png",
       plot = spettrogramma, width = 15, height= 9.27)


#analisi ACF e PACF per tutti i id_interve
df_acf <- monitoraggi %>% 
  group_by(id_interve) %>% 
  summarise(list_acf=list(acf(RMS_W, plot=FALSE,lag.max = Inf))) %>%
  mutate(acf_RMS_Ws=purrr::map(list_acf, ~as.numeric(.x$acf))) %>% 
  dplyr::select(-list_acf) %>% 
  unnest(cols = c(acf_RMS_Ws)) %>% 
  group_by(id_interve) %>% 
  mutate(lag=row_number() - 1)

df_pacf <- monitoraggi %>% 
  group_by(id_interve) %>% 
  summarise(list_pacf=list(pacf(RMS_W, plot=FALSE,lag.max=30))) %>%
  mutate(pacf_RMS_Ws=purrr::map(list_pacf, ~as.numeric(.x$acf))) %>% 
  dplyr::select(-list_pacf) %>% 
  unnest(cols = c(pacf_RMS_Ws)) %>% 
  group_by(id_interve) %>% 
  mutate(lag=row_number() - 1)

df_ci <- monitoraggi %>% 
  group_by(id_interve) %>% 
  summarise(ci = qnorm((1 + 0.95)/2)/sqrt(n()))


#Grafico AutoCorrelazione totale
ACF <-ggplot(df_acf, aes(x=lag, y=acf_RMS_Ws)) +
  geom_bar(stat="identity", width=.4) +
  geom_hline(yintercept = 0) +
  geom_hline(data = df_ci, aes(yintercept = -ci), color="blue", linetype="dotted") +
  geom_hline(data = df_ci, aes(yintercept = ci), color="blue", linetype="dotted") +
  labs(x="Lag", y=NULL) +
  facet_wrap(~id_interve,scales='free')

ggsave("ACF.png",
       plot = ACF, width = 15, height= 9.27)

#Grafico Autocorrelazione parziale
PACF <-ggplot(df_pacf, aes(x=lag, y=pacf_RMS_Ws)) +
  geom_bar(stat="identity", width=.4) +
  geom_hline(yintercept = 0) +
  geom_hline(data = df_ci, aes(yintercept = -ci), color="blue", linetype="dotted") +
  geom_hline(data = df_ci, aes(yintercept = ci), color="blue", linetype="dotted") +
  labs(x="Lag", y=NULL) +
  facet_wrap(~id_interve,scales='free')

ggsave("PACF.png",
       plot = PACF, width = 15, height= 9.27)

##predizione con neural networks
##reti neurali autoregressive##
dataframe<- dataframe%>%
  cbind(lags = c(6,5,6,2,6,1,8,6,11,4,7,9,5,4,3,4,2,5,2,7,6,13,4,20,4,13,6))

dataframe_nnar<-dataframe %>%
  mutate(
    NETmodel = map2(TRAIN,lags,~nnetar(.x,size = 3,p=.y,P=20,repeats=20,lambda= NULL)),
    INFONET = map(NETmodel,~.$model),
    #previsione reti e residui del modello
    FCAST_NET = map(NETmodel,~forecast(.x,h=720)),
    prevnet = map(FCAST_NET,~.$mean),
    RES_PREV_NET = map2(FCAST_NET,TEST,~.x$mean-.y),
    ACC = map2(FCAST_NET,TEST,~accuracy(.x,.y)),
    MAE_TRAIN_nnar = map(ACC,~round(.x[5],3)),
    MAE_TEST_nnar = map(ACC,~round(.x[6],3)),
  )


#grafico previsione nnar
dataframe_test_nnar <- dataframe_nnar %>% 
  unnest(c(TEST,prevnet)) %>% 
  mutate(time= c(721:1440))

dataframe_train <- dataframe %>%
  mutate(TRAIN_W = map(TRAIN,~window(.x,start = c(19,1),end = c(21,240)))) %>%
  unnest(TRAIN_W) %>%  
  dplyr::select(TRAIN_W) %>%
  mutate(time= c(1:720))

colors <- c("TRAIN" = "gray45", "TEST" = "cadetblue", "NNAR" = "red2") 
ggplot()+
  geom_line(data = dataframe_train,aes(x=time,y=TRAIN_W,color="TRAIN"))+
  geom_line(data = dataframe_test_nnar,aes(x=time,y=TEST,color="TEST"))+
  geom_line(data = dataframe_test_nnar,aes(x=time,y=prevnet,color="NNAR"))+
  labs(color = "Legend",y= "RMS normalized",x="lags") +
  scale_color_manual(values = colors)+
  facet_wrap(~id_interve)

ggsave("NNAR.png",
       plot = last_plot(), width = 15, height= 9.27)

#Grafico MAE nnar
fills  <- c("TRAIN" = "gray45", "TEST" = "red3")
ggplot(dataframe_nnar)+
  #geom_bar(aes(y=MAE_TRAIN_nnar,x=1,fill= "TRAIN"),stat="identity")+
  geom_bar(aes(y=MAE_TEST_nnar,x=2,fill="TEST"),stat="identity")+
 # geom_text(aes(y=MAE_TRAIN_nnar,x=1,label=MAE_TRAIN_nnar), position=position_stack(vjust=0.85)) +
  geom_text(aes(y=MAE_TEST_nnar,x=2,label=MAE_TEST_nnar), position=position_stack(vjust=0.85)) +
  theme_bw()+
  labs(fill = "Legend",x=NULL,y= "MAE") +
  scale_fill_manual(values = fills)+
  facet_wrap(~id_interve)

#Box-plot MAE NNAR
box_plot_MAE_NNAR <-dataframe_nnar %>%
  dplyr::select(MAE_TRAIN_nnar,MAE_TEST_nnar) %>%
  unnest(c(MAE_TRAIN_nnar,MAE_TEST_nnar))

ggplot(box_plot_MAE_NNAR,aes(x = 1,y = MAE_TEST_nnar))+
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")

#MAE medio NNAR
MAE_MEDIO_NNAR <- dataframe_nnar %>%
  dplyr::select(MAE_TRAIN_nnar,MAE_TEST_nnar) %>%
  unnest(c(MAE_TRAIN_nnar,MAE_TEST_nnar)) %>%
  ungroup() %>%
  summarize(MAE_MEDIO_TRAIN_NNAR=round(mean(MAE_TRAIN_nnar),3),MAE_MEDIO_TEST_NNAR= round(mean(MAE_TEST_nnar),3), VAR_MAE_TEST_NNAR= round(mean(MAE_TEST_nnar),3))

ggplot(MAE_MEDIO_NNAR)+
  geom_bar(aes(y=MAE_MEDIO_TRAIN_NNAR,x=1,fill= "TRAIN"),stat="identity")+
  geom_bar(aes(y=MAE_MEDIO_TEST_NNAR,x=2,fill="TEST"),stat="identity")+
  geom_text(aes(y=MAE_MEDIO_TRAIN_NNAR,x=1,label=MAE_MEDIO_TRAIN_NNAR), position=position_stack(vjust=0.85)) +
  geom_text(aes(y=MAE_MEDIO_TEST_NNAR,x=2,label=MAE_MEDIO_TEST_NNAR), position=position_stack(vjust=0.85)) +
  theme_bw()+
  labs(fill = "Legend",y="MAE MEDIO",x= NULL) +
  scale_fill_manual(values = fills)



##rete neurale: Generalized regression neural network##
dataframe_grnn <- dataframe%>%
  mutate(
    GRNNmodel= map2(TRAIN,lags,~grnn_forecasting(.x,h=720,sigma=0.10,lags= 1:240, msas="recursive",transform ="none")),
    prevgrnn = map(GRNNmodel,~.$prediction),
    INFOGRNN = map(GRNNmodel,~.$model),
    ACC_grnn = map2(prevgrnn,TEST,~accuracy(.x,.y)),
    MAE_TEST_grnn = map(ACC_grnn,~round(.x[3],3))
  )


#grafico grnn
dataframe_test_grnn <- dataframe_grnn %>% unnest(c(TEST,prevgrnn)) %>% 
  mutate(time= c(721:1440))


colors <- c("TRAIN" = "gray45", "TEST" = "cadetblue", "GRNN" = "red2") 
ggplot()+
  geom_line(data = dataframe_train,aes(x=time,y=TRAIN_W,color="TRAIN"))+
  geom_line(data = dataframe_test_grnn,aes(x=time,y=TEST,color="TEST"))+
  geom_line(data = dataframe_test_grnn,aes(x=time,y=prevgrnn,color="GRNN"))+
  labs(color = "Legend",y= "RMS normalized",x="lags") +
  scale_color_manual(values = colors)+
  facet_wrap(~id_interve)

ggsave("GRNN.png",
       plot = last_plot(), width = 15, height= 9.27)

#Grafico MAE grnn
fills  <- c("TEST" = "red3")
ggplot(dataframe_grnn)+
  geom_bar(aes(y=MAE_TEST_grnn,x=1,fill="TEST"),stat="identity")+
  geom_text(aes(y=MAE_TEST_grnn,x=1,label=MAE_TEST_grnn), position=position_stack(vjust=0.85)) +
  theme_bw()+
  labs(fill = "Legend") +
  scale_fill_manual(values = fills)+
  facet_wrap(~id_interve)

#Box-plot MAE GRNN
box_plot_MAE_GRNN <-dataframe_grnn %>%
  dplyr::select(MAE_TEST_grnn) %>%
  unnest(MAE_TEST_grnn)

ggplot(box_plot_MAE_GRNN,aes(x = 1,y = MAE_TEST_grnn))+
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")


#MAE medio grnn
MAE_MEDIO_GRNN <- dataframe_grnn %>%
  dplyr::select(MAE_TEST_grnn) %>%
  unnest(c(MAE_TEST_grnn)) %>%
  ungroup() %>%
  summarize(MAE_MEDIO_TEST_GRNN= round(mean(MAE_TEST_grnn),3))

ggplot(MAE_MEDIO_GRNN)+
  geom_bar(aes(y=MAE_MEDIO_TEST_GRNN,x=2,fill="TEST"),stat="identity")+
  geom_text(aes(y=MAE_MEDIO_TEST_GRNN,x=2,label=MAE_MEDIO_TEST_GRNN), position=position_stack(vjust=0.85)) +
  theme_bw()+
  labs(fill = "Legend",y="MAE MEDIO",x= NULL) +
  scale_fill_manual(values = fills)


##rete neurale: Extreme learning machine##
dataframe_elm <- dataframe%>%
  mutate(
    ELMmodel = map2(TRAIN,lags,~elm(.x,m = 240, hd = 100, type = "lm", reps = 20, comb = "median", difforder = 240,
                                    sel.lag =  FALSE, allow.det.season = TRUE, det.type = "trg",lags = 1:.y)),
    INFOELM = map(ELMmodel,~.$net),
    #previsione reti e residui del modello
    FCAST_elm = map(ELMmodel,~forecast(.x,h=720)),
    prevelm = map(FCAST_elm,~.$mean),
    RES_PREV_elm = map2(FCAST_elm,TEST,~.x$mean-.y),
    ACC_elm = map2(FCAST_elm,TEST,~accuracy(.x,.y)),
    MAE_TRAIN_elm = map(ACC_elm,~round(.x[5],3)),
    MAE_TEST_elm = map(ACC_elm,~round(.x[6],3))
  )

dataframe_elm$ELMmodel[1]
plot(dataframe_elm$ELMmodel[[1]])


#grafico elm
dataframe_test_elm <- dataframe_elm %>% unnest(c(TEST,prevelm)) %>% 
  mutate(time= c(721:1440))

colors <- c("TRAIN" = "gray45", "TEST" = "cadetblue", "ELM" = "red2") 
ggplot()+
  geom_line(data = dataframe_train,aes(x=time,y=TRAIN_W,color="TRAIN"))+
  geom_line(data = dataframe_test_elm,aes(x=time,y=TEST,color="TEST"))+
  geom_line(data = dataframe_test_elm,aes(x=time,y=prevelm,color="ELM"))+
  labs(color = "Legend",y= "RMS normalized",x="lags") +
  scale_color_manual(values = colors)+
  facet_wrap(~id_interve)

ggsave("ELM.png",
       plot = last_plot(), width = 15, height= 9.27)

#Grafico MAE elm
fills  <- c("TRAIN" = "gray45", "TEST" = "red3")
ggplot(dataframe_elm)+
  geom_bar(aes(y=MAE_TRAIN_elm,x=1,fill= "TRAIN"),stat="identity")+
  geom_bar(aes(y=MAE_TEST_elm,x=2,fill="TEST"),stat="identity")+
  geom_text(aes(y=MAE_TRAIN_elm,x=1,label=MAE_TRAIN_elm), position=position_stack(vjust=0.85)) +
  geom_text(aes(y=MAE_TEST_elm,x=2,label=MAE_TEST_elm), position=position_stack(vjust=0.85)) +
  theme_bw()+
  labs(fill = "Legend") +
  scale_fill_manual(values = fills)+
  facet_wrap(~id_interve)


#Box-plot MAE ELM
box_plot_MAE_ELM <-dataframe_elm %>%
  dplyr::select(MAE_TRAIN_elm,MAE_TEST_elm) %>%
  unnest(c(MAE_TRAIN_elm,MAE_TEST_elm))

ggplot(box_plot_MAE_ELM,aes(x = 1,y = MAE_TEST_elm))+
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")


#MAE medio ELM
MAE_MEDIO_ELM <- dataframe_elm %>%
  dplyr::select(MAE_TRAIN_elm,MAE_TEST_elm) %>%
  unnest(c(MAE_TRAIN_elm,MAE_TEST_elm)) %>%
  ungroup() %>%
  summarize(MAE_MEDIO_TRAIN_ELM=round(mean(MAE_TRAIN_elm),3),MAE_MEDIO_TEST_ELM= round(mean(MAE_TEST_elm),3))


ggplot(MAE_MEDIO_ELM)+
  geom_bar(aes(y=MAE_MEDIO_TRAIN_ELM,x=1,fill= "TRAIN"),stat="identity")+
  geom_bar(aes(y=MAE_MEDIO_TEST_ELM,x=2,fill="TEST"),stat="identity")+
  geom_text(aes(y=MAE_MEDIO_TRAIN_ELM,x=1,label=MAE_MEDIO_TRAIN_ELM), position=position_stack(vjust=0.85)) +
  geom_text(aes(y=MAE_MEDIO_TEST_ELM,x=2,label=MAE_MEDIO_TEST_ELM), position=position_stack(vjust=0.85)) +
  theme_bw()+
  labs(fill = "Legend",y="MAE MEDIO",x= NULL) +
  scale_fill_manual(values = fills)


##Rete neurale: Multi Layer Perceptron-- ricorda di mettere 3 giorni nel train per limiti dell'algoritmo (TRAIN dal 19,1 al 21,240)## 
dataframe_mlp <- dataframe %>%
  mutate(
    MLPmodel = map2(TRAIN,lags,~mlp(y=.x,m=240,hd=1,reps = 20, comb = "median", lags = .y, difforder = 240,
                                    sel.lag =  FALSE, allow.det.season = TRUE, det.type = "trg",
                                    hd.max = 1000)),
    INFOMLP = map(MLPmodel,~.$net),
    #previsione reti e residui del modello
    FCAST_mlp = map(MLPmodel,~forecast(.x,h=720)),
    prevmlp = map(FCAST_mlp,~.$mean),
    RES_PREV_mlp = map2(FCAST_mlp,TEST,~.x$mean-.y),
    ACC_mlp = map2(FCAST_mlp,TEST,~accuracy(.x,.y)),
    MAE_TRAIN_mlp = map(ACC_mlp,~round(.x[5],3)),
    MAE_TEST_mlp = map(ACC_mlp,~round(.x[6],3))
  )

dataframe_mlp$MLPmodel
plot(dataframe_mlp$MLPmodel[[27]])

#grafico mlp
dataframe_test_mlp <- dataframe_mlp %>% unnest(c(TEST,prevmlp)) %>% 
  mutate(time= c(721:1440))

colors <- c("TRAIN" = "gray45", "TEST" = "cadetblue", "MLP" = "red2") 
ggplot()+
  geom_line(data = dataframe_train,aes(x=time,y=TRAIN_W,color="TRAIN"))+
  geom_line(data = dataframe_test_mlp,aes(x=time,y=TEST,color="TEST"))+
  geom_line(data = dataframe_test_mlp,aes(x=time,y=prevmlp,color="MLP"))+
  labs(color = "Legend",y= "RMS normalized",x="lags") +
  scale_color_manual(values = colors)+
  facet_wrap(~id_interve, scales= "free")

ggsave("MLP.png",
       plot = last_plot(), width = 15, height= 9.27)


#Grafico MAE mlp
fills  <- c("TRAIN" = "gray45", "TEST" = "red3")
ggplot(dataframe_mlp)+
  geom_bar(aes(y=MAE_TRAIN_mlp,x=1,fill= "TRAIN"),stat="identity")+
  geom_bar(aes(y=MAE_TEST_mlp,x=2,fill="TEST"),stat="identity")+
  geom_text(aes(y=MAE_TRAIN_mlp,x=1,label=MAE_TRAIN_mlp), position=position_stack(vjust=0.85)) +
  geom_text(aes(y=MAE_TEST_mlp,x=2,label=MAE_TEST_mlp), position=position_stack(vjust=0.85)) +
  theme_bw()+
  labs(color = "Legend") +
  scale_fill_manual(values = fills)+
  facet_wrap(~id_interve)

#Box-plot MAE MLP
box_plot_MAE_MLP <-dataframe_mlp %>%
  dplyr::select(MAE_TRAIN_mlp,MAE_TEST_mlp) %>%
  unnest(c(MAE_TRAIN_mlp,MAE_TEST_mlp))

ggplot(box_plot_MAE_MLP,aes(x = 1,y = MAE_TEST_mlp))+
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")

#MAE medio MLP
MAE_MEDIO_MLP <- dataframe_mlp %>%
  dplyr::select(MAE_TRAIN_mlp,MAE_TEST_mlp) %>%
  unnest(c(MAE_TRAIN_mlp,MAE_TEST_mlp)) %>%
  ungroup() %>%
  summarize(MAE_MEDIO_TRAIN_MLP=round(mean(MAE_TRAIN_mlp),3),MAE_MEDIO_TEST_MLP= round(mean(MAE_TEST_mlp),3))

ggplot(MAE_MEDIO_MLP)+
  geom_bar(aes(y=MAE_MEDIO_TRAIN_MLP,x=1,fill= "TRAIN"),stat="identity")+
  geom_bar(aes(y=MAE_MEDIO_TEST_MLP,x=2,fill="TEST"),stat="identity")+
  geom_text(aes(y=MAE_MEDIO_TRAIN_MLP,x=1,label=MAE_MEDIO_TRAIN_MLP), position=position_stack(vjust=0.85)) +
  geom_text(aes(y=MAE_MEDIO_TEST_MLP,x=2,label=MAE_MEDIO_TEST_MLP), position=position_stack(vjust=0.85)) +
  theme_bw()+
  labs(fill = "Legend",y="MAE MEDIO",x= NULL) +
  scale_fill_manual(values = fills)

#CONFRONTO SPERIMENTALE
#confronto mae con linee e punti per ogni id_interve
dataframe_MAE_nnar <- dataframe_nnar %>% 
  dplyr::select(MAE_TEST_nnar) %>%
  unnest(c(MAE_TEST_nnar)) 

dataframe_MAE_grnn <- dataframe_grnn %>% 
  dplyr::select(MAE_TEST_grnn) %>%
  unnest(c(MAE_TEST_grnn)) 

dataframe_MAE_elm <- dataframe_elm %>% 
  dplyr::select(MAE_TEST_elm) %>%
  unnest(c(MAE_TEST_elm)) 

dataframe_MAE_mlp <- dataframe_mlp %>% 
  dplyr::select(MAE_TEST_mlp) %>%
  unnest(c(MAE_TEST_mlp)) 


colors  <- c("NNAR" = "Blue", "MLP" = "orange2", "GRNN" = "Green3", "ELM" = "Purple")
ggplot()+
  geom_point(data=dataframe_MAE_nnar, aes(x=id_interve, y=MAE_TEST_nnar,color="NNAR"))+
  geom_line(data=dataframe_MAE_nnar, aes(x=id_interve, y= MAE_TEST_nnar, group=1,color="NNAR"))+
  stat_summary(data=dataframe_MAE_nnar,aes(y=mean(MAE_TEST_nnar),x=id_interve,group=1,color="NNAR"),fun=mean,geom="line",linetype="longdash")+
  # geom_label(data=dataframe_MAE_nnar,aes(y=mean(MAE_TEST_nnar),x=1,label = round(mean(MAE_TEST_nnar), 3)),nudge_y = -0.030,,nudge_x = +0.50)+

 # geom_point(data=dataframe_MAE_grnn, aes(x=id_interve, y=MAE_TEST_grnn,color="GRNN"),shape=4)+
 # geom_line(data=dataframe_MAE_grnn, aes(x=id_interve, y= MAE_TEST_grnn, group=1,color="GRNN"))+
 # stat_summary(data=dataframe_MAE_grnn,aes(y=mean(MAE_TEST_grnn),x=id_interve,group=1,color="GRNN"),fun=mean,geom="line",linetype="longdash")+
 # #geom_label(data=dataframe_MAE_grnn,aes(y=mean(MAE_TEST_grnn),x=1,label = round(mean(MAE_TEST_grnn), 3)),nudge_y = +0.010,nudge_x = +0.50)+
 # 
 # geom_point(data=dataframe_MAE_elm, aes(x=id_interve, y=MAE_TEST_elm,color="ELM"))+
 # geom_line(data=dataframe_MAE_elm, aes(x=id_interve, y= MAE_TEST_elm, group=1,color="ELM"))+
 # stat_summary(data=dataframe_MAE_elm,aes(y=mean(MAE_TEST_elm),x=id_interve,group=1,color="ELM"),fun=mean,geom="line",linetype="longdash")+
 # #geom_label(data=dataframe_MAE_elm,aes(y=mean(MAE_TEST_elm),x=1,label = round(mean(MAE_TEST_elm), 3)),nudge_y = -0.010,nudge_x = +0.50)+

  geom_point(data=dataframe_MAE_mlp, aes(x=id_interve, y=MAE_TEST_mlp,color="MLP"),shape=4)+
  geom_line(data=dataframe_MAE_mlp, aes(x=id_interve, y= MAE_TEST_mlp, group=1,color="MLP"))+
  stat_summary(data=dataframe_MAE_mlp,aes(y=mean(MAE_TEST_mlp),x=id_interve,group=1,color="MLP"),fun=mean,geom="line",linetype="longdash")+
  # geom_label(data=dataframe_MAE_mlp,aes(y=mean(MAE_TEST_mlp),x=1,label = round(mean(MAE_TEST_mlp), 3)),nudge_y = -0.030,nudge_x = +0.50)+

  labs(color = "Legend") +
  scale_color_manual(values = colors)+
  labs(y="MAE")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))


ggsave("Mae_normalizzato_NNAR_MLP_testo.png",
       plot = last_plot(), width = 15, height= 9.27)


##confronto MAE MEDIO reti neurali
ggplot()+
  geom_bar(data=MAE_MEDIO_NNAR,aes(y=MAE_MEDIO_TEST_NNAR,x=1,fill= "NNAR"),stat="identity")+
  geom_text(data=MAE_MEDIO_NNAR,aes(y=MAE_MEDIO_TEST_NNAR,x=1,label=MAE_MEDIO_TEST_NNAR), position=position_stack(vjust=0.85)) +
  geom_bar(data=MAE_MEDIO_ELM,aes(y=MAE_MEDIO_TEST_ELM,x=2,fill= "ELM"),stat="identity")+
  geom_text(data=MAE_MEDIO_ELM,aes(y=MAE_MEDIO_TEST_ELM,x=2,label=MAE_MEDIO_TEST_ELM), position=position_stack(vjust=0.85)) +
  geom_bar(data=MAE_MEDIO_GRNN,aes(y=MAE_MEDIO_TEST_GRNN,x=3,fill= "GRNN"),stat="identity")+
  geom_text(data=MAE_MEDIO_GRNN,aes(y=MAE_MEDIO_TEST_GRNN,x=3,label=MAE_MEDIO_TEST_GRNN), position=position_stack(vjust=0.85)) +
  geom_bar(data=MAE_MEDIO_MLP,aes(y=MAE_MEDIO_TEST_MLP,x=4,fill= "MLP"),stat="identity")+
  geom_text(data=MAE_MEDIO_MLP,aes(y=MAE_MEDIO_TEST_MLP,x=4,label=MAE_MEDIO_TEST_MLP), position=position_stack(vjust=0.85)) +
  labs(x=NULL,y="MAE MEDIO TEST")


#Box plot confronto MAE reti neurali
ggplot()+
  geom_boxplot(data=box_plot_MAE_NNAR,aes(x=0,y=MAE_TEST_nnar,fill= "NNAR"))+
  stat_summary(data=box_plot_MAE_NNAR,aes(x=0,y=mean(MAE_TEST_nnar)),fun=mean, geom="point", shape=20, size=4, color="Black", fill="red")+
  geom_boxplot(data=box_plot_MAE_GRNN,aes(x=1,y=MAE_TEST_grnn,fill= "GRNN"))+
  stat_summary(data=box_plot_MAE_GRNN,aes(x=1,y=mean(MAE_TEST_grnn)),fun=mean, geom="point", shape=20, size=4, color="Black", fill="red")+
  geom_boxplot(data=box_plot_MAE_ELM,aes(x=2,y=MAE_TEST_elm,fill= "ELM"))+
  stat_summary(data=box_plot_MAE_ELM,aes(x=2,y=mean(MAE_TEST_elm)),fun=mean, geom="point", shape=20, size=4, color="Black", fill="red")+
  geom_boxplot(data=box_plot_MAE_MLP,aes(x=3,y=MAE_TEST_mlp,fill= "MLP"))+
  stat_summary(data=box_plot_MAE_MLP,aes(x=3,y=mean(MAE_TEST_mlp)),fun=mean, geom="point", shape=20, size=4, color="Black", fill="red")+
  labs(fill = "Legend",y="MAE",x="Neural networks") +
  scale_fill_manual(values = colors) + theme(axis.text.x = element_text(size = 0))
  
ggsave("Box plot confronto.png",
       plot = last_plot(), width = 15, height= 9.27)
