library(tidyverse)
library(fpp3)
library(cowplot)

# asyl applications -------------------------------------------------------
df = read.csv("Data/migr_asyappctzm_1_Data.csv")
View(df)
unique(df$GEO)

df_ts = df %>% 
  filter(GEO == "European Union - 27 countries (from 2020)",
         ASYL_APP == 'Asylum applicant',
         CITIZEN == "Extra-EU27 (from 2020)",
         Value != ":") %>% 
  mutate(GEO = "EU") %>% 
  select(TIME,Value) %>% 
  mutate(Value = str_replace(Value,",","")) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  mutate( Date = str_replace(TIME,"M","-")) %>% 
  mutate(Date = yearmonth(Date)) %>% 
  tsibble(index=Date)  %>% 
  select(Value,Date)
  
autoplot(df_ts)+
  scale_x_yearmonth(date_breaks = "3 month", date_labels = "%Y/%m")

gg_tsdisplay(df_ts)
  

# nights spend in hotels --------------------------------------------------
# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=tour_occ_nim&lang=en
df2 = read.csv("Data/tour_occ_nim_1_Data.csv")
View(df2)

unique(df2$NACE_R2)

df2_ts = df2 %>% 
  filter(GEO == "European Union - 27 countries (from 2020)",
         UNIT == "Number",
         C_RESID == 'Foreign country',
         UNIT == "Number",
         NACE_R2 == "Hotels and similar accommodation",
         Value != ":") %>% 
  # mutate(GEO = "EU") %>% 
  select(TIME,Value) %>%
  mutate(Value = gsub(" ","",Value)) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  mutate( Date = str_replace(TIME,"M","-")) %>% 
  mutate(Date = yearmonth(Date)) %>% 
  tsibble(index=Date)  %>% 
  select(Value,Date)

gg_tsdisplay(df2_ts)

autoplot(df2_ts)+
  scale_x_yearmonth(date_breaks = "4 month", date_labels = "%y/%m")
  

# unemployment ratio ------------------------------------------------------
# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=une_rt_m&lang=en

df3 = read.csv("Data/ei_lmhr_m_1_Data.csv")


df3_ts = df3 %>% 
  filter(INDIC == "Unemployment according to ILO definition - Total",
         S_ADJ == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",
         Value != ":") %>% 
  # View()
  select(TIME,Value) %>%
  # mutate(Value = gsub(" ","",Value)) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  mutate( Date = str_replace(TIME,"M","-")) %>% 
  mutate(Date = yearmonth(Date)) %>% 
# View()
  tsibble(index=Date)  %>% 
  select(Value,Date)

# graph EU-------------------------------------------------------------------


ggplot(df2_ts,aes(x=Date,y=Value))+
  geom_line(color='grey30',lty=1,size=0.4)+
  # geom_line(data=df2_ts,aes(x=Date,y=Value/1e+7),color='tomato3',size=0.4)+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "6 month", date_labels = "%y'%m")+
  labs(title = "Кол-во ночей в гостинице", subtitle = "Безработица",
       x="",y="Безработица * 10
ночи в гостинице / 1е+07")+
  theme(plot.title = element_text(color='tomato3'),
    plot.subtitle = element_text(color='grey30'),
    axis.text.x = element_text(color='grey30',size=10),
    axis.text.y = element_text(color='grey30',size=10),
    axis.title.y = element_text(size=13))



# UK data long ------------------------------------------------------------

dfUK = read.csv("Data/tour_occ_nim_1_Data_UK.csv")
View(dfUK)

unique(df2$NACE_R2)

dfUK_ts = dfUK %>% 
  filter(UNIT == "Number",
         C_RESID == 'Foreign country',
         UNIT == "Number",
         NACE_R2 == "Hotels and similar accommodation",
         Value != ":") %>% 
  mutate(GEO = "UK") %>%
  select(TIME,Value) %>%
  mutate(Value = gsub(" ","",Value)) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  mutate( Date = str_replace(TIME,"M","-")) %>% 
  mutate(Date = yearmonth(Date)) %>% 
  tsibble(index=Date)  %>% 
  select(Value,Date)

gg_tsdisplay(dfUK_ts)

autoplot(dfUK_ts)+
  scale_x_yearmonth(date_breaks = "6 month", date_labels = "%y/%m")


# graph UK ----------------------------------------------------------------
ggplot(dfUK_ts,aes(x=Date,y=Value*10))+
  geom_line(color='tomato3',lty=1,size=0.4)+
  geom_vline(xintercept = ymd('2012-07-01'),lty=2,color='grey70')+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "6 month", date_labels = "%y'%m")+
  labs(title = "Кол-во ночей в гостинице", subtitle = "UK",
       x="",y="")+
  theme(axis.text.x = element_text(color='grey30',size=10,angle=90),
        axis.text.y = element_text(color='grey30',size=10),
        axis.title.y = element_text(size=13))


# Greece data long ------------------------------------------------------------

dfGR = read.csv("Data/tour_occ_nim_1_Data_GR.csv")
View(dfGR)



dfGR_ts = dfGR %>% 
  filter(UNIT == "Number",
         C_RESID == 'Foreign country',
         UNIT == "Number",
         NACE_R2 == "Hotels and similar accommodation",
         Value != ":") %>% 
  mutate(GEO = "Greece") %>%
  select(TIME,Value) %>%
  mutate(Value = gsub(" ","",Value)) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  mutate( Date = str_replace(TIME,"M","-")) %>% 
  mutate(Date = yearmonth(Date)) %>% 
  tsibble(index=Date)  %>% 
  select(Value,Date)

gg_tsdisplay(dfGR_ts)

autoplot(dfUK_ts)+
  scale_x_yearmonth(date_breaks = "6 month", date_labels = "%y/%m")


# graph Greece ----------------------------------------------------------------
ggplot(dfGR_ts,aes(x=Date,y=Value*10))+
  geom_line(color='tomato3',lty=1,size=0.4)+
  geom_vline(xintercept = ymd('2004-08-13'),color='darkgreen',lty=2)+
  # geom_rect(xmin=ymd('2004-08-13'),
  #           xmax=ymd('2004-08-29'),
  #           ymax=1e+08,ymin=0,
  #           fill='tomato3',
  #           alpha=0.5)+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "6 month", date_labels = "%y'%m")+
  labs(title = "Кол-во ночей в гостинице", subtitle = "Greece",
       x="",y="")+
  theme(axis.text.x = element_text(color='grey30',size=10,angle=90),
        axis.text.y = element_text(color='grey30',size=10),
        axis.title.y = element_text(size=13))

