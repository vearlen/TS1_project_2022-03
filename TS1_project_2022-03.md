---
title: "Анализ Временных Рядов, проект"
author: "Светлана Андреева, Юлия Егорова, Илья Тищенко"
date: "2022-03-24"
output: 
  html_document:
    keep_md: true
    theme: flatly
    code_folding: hide
    toc: true
    toc_float: true
    font_size: 14
---



## 1 Импорт  
*Возьмите любой ряд с ежемесячными наблюдениями.*  

Ряд, количество ночей проведенных туристами, взяли с сайта [eurostat](https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=tour_occ_nim&lang=en), сам ряд можно скачать с [github](https://github.com/vearlen/TS1_project_2022-03/blob/main/Data/tour_occ_nim_1_Data.csv). Второй ряд, процент безработицы, тоже с сайта [eurostat](https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=une_rt_m&lang=en) и тоже можно взять копию [csv](https://github.com/vearlen/TS1_project_2022-03/blob/main/Data/ei_lmhr_m_1_Data.csv).



```r
# nights spend in hotels --------------------------------------------------
# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=tour_occ_nim&lang=en
df2 = read.csv("Data/tour_occ_nim_1_Data.csv")

# unique(df2$NACE_R2)

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


# unemployment ratio ------------------------------------------------------
# https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=une_rt_m&lang=en

df3 = read.csv("Data/ei_lmhr_m_1_Data.csv")

df3_ts = df3 %>% 
  filter(INDIC == "Unemployment according to ILO definition - Total",
         S_ADJ == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",
         Value != ":") %>% 
  select(TIME,Value) %>%
  # mutate(Value = gsub(" ","",Value)) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  mutate( Date = str_replace(TIME,"M","-")) %>% 
  mutate(Date = yearmonth(Date)) %>% 
  tsibble(index=Date)  %>% 
  select(Value,Date)
```

## 2 Визуализация    
*Визуализируйте сам ряд, ряд обычных и сезонных разностей, компоненты ряда, обычные и частные автокорреляционные функции.*  
Вот сам ряд.  


```r
ggplot(df2_ts,aes(x=Date,y=Value))+
  geom_line(color='grey30',lty=1,size=0.4)+
  # geom_line(data=df2_ts,aes(x=Date,y=Value/1e+7),color='#0066cc',size=0.4)+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "6 month", date_labels = "%y'%m")+
  labs(title = "Кол-во ночей в гостинице", 
       x="",y="")+
  theme(plot.title = element_text(color='black'),
    plot.subtitle = element_text(color='grey30'),
    axis.text.x = element_text(color='grey30',size=10,angle=90),
    axis.text.y = element_text(color='grey30',size=10),
    axis.title.y = element_text(size=13))
```

![](TS1_project_2022-03_files/figure-html/nights plot-1.png)<!-- -->

Можно посмотреть на два ряда вместе, потом нам это пригодится.  


```r
ggplot(df3_ts,aes(x=Date,y=Value*10))+
  geom_line(color='grey30',lty=5,size=0.4)+
  geom_line(data=df2_ts,aes(x=Date,y=Value/1e+6),color='#0066cc',size=0.4)+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "6 month", date_labels = "%y'%m")+
  labs(title = "Кол-во ночей в гостинице", subtitle = "Безработица",
       x="",y="Безработица * 10
ночи в гостинице / 1е+06")+
  theme(plot.title = element_text(color='#0066cc'),
    plot.subtitle = element_text(color='grey30'),
    axis.text.x = element_text(color='grey30',size=10,angle=90),
    axis.text.y = element_text(color='grey30',size=10),
    axis.title.y = element_text(size=13))
```

![](TS1_project_2022-03_files/figure-html/autoplot unempl-1.png)<!-- -->

**Тут нужен комментарий.**    
Кажется у нас нет особой сезонности на pacf графике, ну по крайней мере не больше второго лага точно.  


```r
gg_tsdisplay(df2_ts,plot_type = 'partial')+
  labs(title = "Ночи проведенные в гостиницах")
```

![](TS1_project_2022-03_files/figure-html/acf pacf nights-1.png)<!-- -->
  
STL разложение.  
  

```r
stl_model = model(df2_ts,
                  decomp = STL(Value ~ trend(window = 30) +
                                 season(window = 50)))
components(stl_model) %>% autoplot()
```

![](TS1_project_2022-03_files/figure-html/STL-1.png)<!-- -->
  
Посмотрим на разности.  


```r
nights = df2_ts %>% 
  rename('N_nights'='Value')
nights = mutate(nights, diff = difference(N_nights, order_by = Date))
gg_tsdisplay(nights, y = diff, plot_type = 'partial')+
  labs(title="Разность ряда")
```

![](TS1_project_2022-03_files/figure-html/diff-1.png)<!-- -->
  
В том числе сезонные. Очевидно что ковидный период подпортил статистику. Разница не выглядит стационарной.


```r
nights = mutate(nights, diff_seas = difference(N_nights, lag = 12, order_by = Date))
gg_tsdisplay(nights, y = diff_seas, plot_type = 'partial')+
  labs(title="Сезонная разность (12 месяцев)")
```

![](TS1_project_2022-03_files/figure-html/season diff-1.png)<!-- -->
  
Если отрезать ковидный период, то разница уже больше похожа на стационарный ряд. ARIMA (p,1,q)?  


```r
nights = mutate(nights, diff_seas = difference(N_nights, lag = 12, order_by = Date))
nights_cut = filter(nights, Date < ymd("2020-03-01"))
gg_tsdisplay(nights_cut, y = diff_seas, plot_type = 'partial')+
  labs(title="Сезонная разность (12 мес) без последних двух лет")
```

![](TS1_project_2022-03_files/figure-html/season diff w_out covid-1.png)<!-- -->
  
## 3 Стационарность  
  
*Является ли ряд стационарным?*  
  
### ADF с константой    


```r
#3. ADF с константой
# H0: ts = ARIMA(p, 1, q) + trend (нестационарный ряд)
# Ha: ts = ARIMA(p, 0, q) + const (стационарный ряд)
summary(ur.df(nights$N_nights, type = 'drift',
                selectlags = 'AIC'))# H0 отвергается на 5% уровне значимости
```

```
## 
## ############################################### 
## # Augmented Dickey-Fuller Test Unit Root Test # 
## ############################################### 
## 
## Test regression drift 
## 
## 
## Call:
## lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -35494483  -6649223  -1564895   6600159  24814553 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.257e+07  2.472e+06   5.085 1.65e-06 ***
## z.lag.1     -2.045e-01  3.529e-02  -5.795 7.52e-08 ***
## z.diff.lag   6.922e-01  7.146e-02   9.687 3.67e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11940000 on 103 degrees of freedom
## Multiple R-squared:  0.5071,	Adjusted R-squared:  0.4975 
## F-statistic: 52.97 on 2 and 103 DF,  p-value: < 2.2e-16
## 
## 
## Value of test-statistic is: -5.7947 16.7916 
## 
## Critical values for test statistics: 
##       1pct  5pct 10pct
## tau2 -3.46 -2.88 -2.57
## phi1  6.52  4.63  3.81
```

```r
                                    # H0 отвергается на 1% уровне значимости
                                    # первое наблюдаемое значение -3.0 как в лекциях
                                    # сравниваем с первой строкой tau2
```
  
Наблюдаемое значение -5.79, критическое -2.88 (на 5% уровне значимости).  Наблюдаемое меньше, т.е. Н0 отвергается - ряд стационарный.  


 ### KPSS c константой  


```r
# KPSS с константой
# H0: ts = mu + stat (стационарный ряд)
# Ha: ts = mu + stat + rw (нестационарный ряд)
summary(ur.kpss(nights$N_nights, type = 'mu'))  # H0 не отвергается на 5% уровне значимости
```

```
## 
## ####################### 
## # KPSS Unit Root Test # 
## ####################### 
## 
## Test is of type: mu with 4 lags. 
## 
## Value of test-statistic is: 0.4021 
## 
## Critical value for a significance level of: 
##                 10pct  5pct 2.5pct  1pct
## critical values 0.347 0.463  0.574 0.739
```

```r
                                                # наблюдаемое меньше чем критическое
```
  
Получается что наблюдаемое значение статистики меньше чем критическое (на 5% уровне), т.е. Н0 не отвергается, значит ряд стационарный.  
    
Подсчитаем также KPSS тест с трендом. **не уверен что это нужно** 
  

```r
# 2. KPSS с трендом
# H0: ts = trend + stat (стационарный ряд)
# Ha: ts = trend + stat + rw (нестационарный ряд)
summary( ur.kpss(nights$N_nights, type = 'tau')) # H0 отвергается на 5% уровне значимости
```

```
## 
## ####################### 
## # KPSS Unit Root Test # 
## ####################### 
## 
## Test is of type: tau with 4 lags. 
## 
## Value of test-statistic is: 0.1712 
## 
## Critical value for a significance level of: 
##                 10pct  5pct 2.5pct  1pct
## critical values 0.119 0.146  0.176 0.216
```

```r
                                                 # наблюдаемое больше чем критическое
```
  
Выходит что наблюдаемое значение больше чем критическое (на 5% уровне значимости), значит H0 отвергается. Вероятно это можно объяснить тем, что у нас есть тренд.  
  
  
## 4 Преобразование  
*Если разумно применить к исходному ряду какое-либо преобразование, то примените его, мотивировав свой выбор.*  

## 5 Деление  
*Поделите ряд на тестовую и обучающую выборку.*  


```r
df_all = left_join(nights,df3_ts)
df_all = rename(df_all,'unempl'='Value')
df_all = mutate(df_all, diff_unempl = difference(unempl,1),
                sdiff_unempl = difference(unempl,12))
df_train <- filter(df_all, Date < ymd ('2021-01-01'))
df_test <- filter(df_all, Date >= ymd ('2021-01-01'))
```
  
## 6 Модели  
*Оцените ряд моделей/алгоритмов на тестовой выборке.*  


```r
fit_models <- df_train %>% 
  model(
    snaive = SNAIVE(N_nights),
    arima = ARIMA(N_nights),
    arima_r = ARIMA(N_nights ~ unempl),
    arima110_r = ARIMA(N_nights ~ unempl+ pdq(1,1,0)),
    ets = ETS(N_nights),
    prophet = prophet(N_nights ~ season ('year', 10, type='additive')),
    sarima111_1xx_r = ARIMA(N_nights ~ unempl + pdq(1, 1, 1) +PDQ(1, 0:1, 0:2)),
    theta_decomp = decomposition_model(
                STL(N_nights ~ season(window=Inf)),
                THETA(season_adjust),
                SNAIVE(season_year))
  )
```
  
Можно посмотреть отчет.   

```r
report(fit_models$arima[[1]])
```

```
## Series: N_nights 
## Model: ARIMA(2,1,2)(1,1,1)[12] 
## 
## Coefficients:
##          ar1      ar2      ma1     ma2     sar1     sma1
##       1.6142  -0.9114  -1.2573  0.6472  -0.2896  -0.5098
## s.e.  0.1110   0.0861   0.1353  0.1362   0.2270   0.2459
## 
## sigma^2 estimated as 3.826e+13:  log likelihood=-1416.39
## AIC=2846.79   AICc=2848.28   BIC=2863.72
```


```r
fct = forecast(fit_models,new_data=df_test)
```

```r
fct %>% 
  accuracy(df_all) %>% 
  arrange(MAPE) %>% 
  # select(-MASE,-RMSSE) %>% 
  select(-.type) %>% 
  mutate_if(is.numeric,round,2) %>% 
  datatable(rownames = FALSE,options=list(pageLength=9), autoHideNavigation = TRUE)
```

```{=html}
<div id="htmlwidget-28d3b7219a57a2238666" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-28d3b7219a57a2238666">{"x":{"filter":"none","autoHideNavigation":true,"data":[["arima110_r","sarima111_1xx_r","prophet","theta_decomp","arima_r","arima","ets","snaive"],[8816939.29,2099916.03,9451131.89,744794.49,17681963.07,27017217.24,-15793338.43,8331459.67],[16366203,14644554.62,22589471.14,16013294.9,23822474.31,29832721.8,22567128.93,28309733.58],[11872146.16,12259733.8,17112606.78,13165861.86,18930739.18,27017217.24,18731603.58,25260921.33],[21.62,-33.81,6.17,-37.67,65.59,171.13,-170.02,-157.86],[49.05,74.48,84.42,85.08,97.15,171.13,180.11,259.19],[1.11,1.14,1.6,1.23,1.76,2.52,1.75,2.35],[0.64,0.58,0.89,0.63,0.94,1.17,0.89,1.11],[0.79,0.79,0.8,0.8,0.77,0.69,0.77,0.74]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>.model<\/th>\n      <th>ME<\/th>\n      <th>RMSE<\/th>\n      <th>MAE<\/th>\n      <th>MPE<\/th>\n      <th>MAPE<\/th>\n      <th>MASE<\/th>\n      <th>RMSSE<\/th>\n      <th>ACF1<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":9,"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8]}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[9,10,25,50,100]}},"evals":[],"jsHooks":[]}</script>
```


```r
accuracy(fct,df_all) %>% 
  select(MASE,.model) %>% 
  ggplot(aes(y=reorder(.model,-MASE),x=MASE,label=round(MASE,2)))+
  geom_col(width = 0.6,alpha=0.9,color='#0057b7',fill='#ffd700')+
  geom_text(size=5,hjust = -0.5,color='#0057b7')+
  theme_cowplot()+
  scale_x_continuous(expand = expansion(mult = c(0,0.2)))+
  labs(y="Модели")
```

![](TS1_project_2022-03_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

**можно запустить кросс валидацию**

```r
df_slide = slide_tsibble(df_all,
                         .size=24,.step=3)
models_slide = model(df_slide,
    snaive = SNAIVE(N_nights),
    arima = ARIMA(N_nights),
    arima110_r = ARIMA(N_nights ~ unempl+ pdq(1,1,0)),
    sarima111_1xx_r = ARIMA(N_nights ~ unempl + pdq(1, 1, 1) +PDQ(1, 0:1, 0:2)),
    theta_decomp = decomposition_model(
                STL(N_nights ~ season(window=Inf)),
                THETA(season_adjust),
                SNAIVE(season_year))
  )
```


```r
# нужно спрогнозировать безработицу чтобы сделать прогноз
# fct_slide = forecast(models_slide,h=12)
```

  
## 7 Победитель  
*Выберите наилучшую модель*  

Ради лучшей визаулизации я обрезал начало ряда, но только для графика!  


```r
df_all_begin_cut = filter(df_all, Date > ymd("2018-01-01"))
# arima_r
plt_arima_r = fct %>% 
  filter(.model == "arima110_r") %>% 
  autoplot(lty=2,fill='#0057b7')+
  autolayer(df_all_begin_cut)+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "4 month", date_labels = "%y'%m")+
  theme(axis.text.x = element_text(color='grey',size=10,angle=90))+
  labs(title = "ARIMA with regression")

# SARIMA
plt_sarima = fct %>% 
  filter(.model == "sarima111_1xx_r") %>% 
  autoplot(lty=2,fill='#dbaf00')+
  autolayer(df_all_begin_cut)+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "4 month", date_labels = "%y'%m")+
  theme(axis.text.x = element_text(color='grey',size=10,angle=90))+
  labs(title = "SARIMA with regression")

# ets
plt_ets = fct %>% 
  filter(.model == "ets") %>% 
  autoplot(lty=2,fill='grey20')+
  autolayer(df_all_begin_cut)+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "4 month", date_labels = "%y'%m")+
  theme(axis.text.x = element_text(color='grey',size=10,angle=90))+
  labs(title = "ETS")

plot_grid(plt_arima_r,plt_sarima,plt_ets,nrow=3)
```

![](TS1_project_2022-03_files/figure-html/display best models-1.png)<!-- -->
  
### Средняя модель  
  

```r
av_model <- fit_models %>% 
  mutate(mean = (arima110_r + sarima111_1xx_r)/2,
         three = (arima110_r + sarima111_1xx_r+prophet)/3)
```


```r
fct_w_av = forecast(av_model,new_data = df_test)
```


```r
fct_w_av %>% 
  accuracy(df_all) %>% 
  arrange(MAPE) %>% 
  select(-.type) %>% 
  mutate_if(is.numeric,round,2) %>% 
  datatable(rownames = FALSE,options=list(pageLength=9), autoHideNavigation = TRUE)
```

```{=html}
<div id="htmlwidget-74810f493c8b6eea80cc" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-74810f493c8b6eea80cc">{"x":{"filter":"none","autoHideNavigation":true,"data":[["arima110_r","mean","three","sarima111_1xx_r","prophet","theta_decomp","arima_r","arima","ets","snaive"],[8816939.29,5458427.66,6787620.63,2099916.03,9565318.74,744794.49,17681963.07,27017217.24,-15793338.43,8331459.67],[16366203,15111740.81,17506722.79,14644554.62,22483661.59,16013294.9,23822474.31,29832721.8,22567128.93,28309733.58],[11872146.16,11675417.5,13338483.74,12259733.8,16968554.88,13165861.86,18930739.18,27017217.24,18731603.58,25260921.33],[21.62,-6.1,-1.78,-33.81,7.69,-37.67,65.59,171.13,-170.02,-157.86],[49.05,53,60.57,74.48,83.81,85.08,97.15,171.13,180.11,259.19],[1.11,1.09,1.24,1.14,1.58,1.23,1.76,2.52,1.75,2.35],[0.64,0.59,0.69,0.58,0.88,0.63,0.94,1.17,0.89,1.11],[0.79,0.79,0.8,0.79,0.8,0.8,0.77,0.69,0.77,0.74]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>.model<\/th>\n      <th>ME<\/th>\n      <th>RMSE<\/th>\n      <th>MAE<\/th>\n      <th>MPE<\/th>\n      <th>MAPE<\/th>\n      <th>MASE<\/th>\n      <th>RMSSE<\/th>\n      <th>ACF1<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":9,"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8]}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[9,10,25,50,100]}},"evals":[],"jsHooks":[]}</script>
```
Все равно ARIMA c регрессией осталась лучшей. 

## 8 Удивить!  

Можно взять как раз разницу безработицы и впихнуть в лучшую модель, но этим никого не удивить.    


```r
ggplot(df_all,aes(x=Date,y=sdiff_unempl*100))+
  geom_line(color='grey30',lty=5,size=0.4)+
  geom_line(data=df_all,aes(x=Date,y=N_nights/1e+6),color='#0066cc',size=0.4)+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "6 month", date_labels = "%y'%m")+
  labs(title = "Кол-во ночей в гостинице", subtitle = "Безработица сезонный прирост (х100)",
       x="",y="Безработица прирост * 100
ночи в гостинице / 1е+06")+
  theme(plot.title = element_text(color='#0066cc'),
    plot.subtitle = element_text(color='grey30'),
    axis.text.x = element_text(color='grey30',size=10,angle=90),
    axis.text.y = element_text(color='grey30',size=10),
    axis.title.y = element_text(size=13))
```

![](TS1_project_2022-03_files/figure-html/autoplot unempl diff-1.png)<!-- -->

```r
fit_models_seasons <- df_train %>% 
  model(
    arima_r = ARIMA(N_nights ~ diff_unempl),
    arima110_r = ARIMA(N_nights ~ diff_unempl+ pdq(1,1,0)),
    arima110_r_s = ARIMA(N_nights ~ sdiff_unempl+ pdq(1,1,0)),
    sarima111_1xx_r = ARIMA(N_nights ~ diff_unempl + pdq(1, 1, 1) +PDQ(1, 0:1, 0:2)),
    sarima111_1xx_r_s = ARIMA(N_nights ~ sdiff_unempl + pdq(1, 1, 1) +PDQ(1, 0:1, 0:2)),
      )
```

```r
fct_season = forecast(fit_models_seasons,new_data=df_test)
```


```r
fct_season %>% 
  accuracy(df_all) %>% 
  arrange(MAPE) %>% 
  select(-.type) %>% 
  mutate_if(is.numeric,round,2) %>% 
  datatable(rownames = FALSE,options=list(pageLength=9), autoHideNavigation = TRUE)
```

```{=html}
<div id="htmlwidget-6194c7a1d66819ebf60a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-6194c7a1d66819ebf60a">{"x":{"filter":"none","autoHideNavigation":true,"data":[["arima110_r","arima110_r_s","sarima111_1xx_r_s","arima_r","sarima111_1xx_r"],[9517219.33,8615610.87,14477246.75,20259246.55,-240356.39],[18994028.25,15853896.07,19257062.14,29538376.76,16745488.75],[13661581.54,11790690.16,15041963.8,22492380.12,14385875.71],[8.66,32.14,67.95,40.62,-64.38],[54.41,55.43,82.13,97.02,106.04],[1.27,1.1,1.4,2.1,1.34],[0.75,0.62,0.76,1.16,0.66],[0.8,0.77,0.75,0.81,0.8]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>.model<\/th>\n      <th>ME<\/th>\n      <th>RMSE<\/th>\n      <th>MAE<\/th>\n      <th>MPE<\/th>\n      <th>MAPE<\/th>\n      <th>MASE<\/th>\n      <th>RMSSE<\/th>\n      <th>ACF1<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":9,"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8]}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[9,10,25,50,100]}},"evals":[],"jsHooks":[]}</script>
```
  
Но c cезонной безработицей модель не получилась лучше.  


```r
# ets
plt_arima_s = fct_season %>% 
  filter(.model == "arima110_r_s") %>% 
  autoplot(lty=2,fill='#dbaf00')+
  autolayer(df_all_begin_cut)+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "4 month", date_labels = "%y'%m")+
  theme(axis.text.x = element_text(color='grey',size=10,angle=90))+
  labs(title = "ARIMA with regression on seasonal difference")

plot_grid(plt_arima_r,plt_arima_s,nrow = 2)
```

![](TS1_project_2022-03_files/figure-html/compare best-1.png)<!-- -->
  
Еще можно предсказать в будущее.  


```r
fit_full <- model(df_all,
  ARIMA(N_nights ~ 0 + pdq(1,1,0))
)

fct_future = forecast(fit_full,h=24)
```

```r
fct_future %>% 
  autoplot(color="#0066cc",alpha=0.7)+
  autolayer(df_all_begin_cut)+
  scale_x_yearmonth(date_breaks = "4 month", date_labels = "%y'%m")+
  theme_cowplot()+
  background_grid()+
   theme(axis.text.x = element_text(color='grey',size=10,angle=90))+
  labs(title="Предсказание в будущее",
       subtitle = "ARIMA")
```

![](TS1_project_2022-03_files/figure-html/plot future-1.png)<!-- -->
  
## 9 Случай из жизни  

