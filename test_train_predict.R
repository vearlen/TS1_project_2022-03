
# predict models ----------------------------------------------------------

#filter dates before COVID
nights = df2_ts %>% 
  filter(Date < ymd('2020-02-01'))

dim(nights)#number of observations

#STL decomp
stl_model = model(nights,
                  decomp = STL(Value~ trend(window = 20) + 
                                 season(window=12)))

components(stl_model) %>% autoplot()

# split train&test --------------------------------------------------------

nights_train = filter(nights, Date < ymd('2018-02-24'))
nights_test = filter(nights, Date >= ymd('2018-02-24'))
nights_test
nights_train

model = model(nights_train,
              ets = ETS(Value),
              snaive = SNAIVE(Value),
              theta_decomp = decomposition_model(
                STL(Value ~ season(window=Inf)),
                THETA(season_adjust),
                SNAIVE(season_year)))

fct = forecast(model, h=24)
accuracy(fct,nights) %>% 
  arrange(MAE)

theta_1 = fct %>% 
  filter(.model %in% c("theta_decomp")) %>% 
  autoplot(lty=2,fill='tomato3')+
  autolayer(nights)+
  theme_cowplot()+
  background_grid()+
  labs(title = "THETA")


snaive_2 = fct %>% 
  filter(.model %in% c("snaive")) %>% 
  autoplot(lty=2,fill='tomato3')+
  autolayer(nights)+
  theme_cowplot()+
  background_grid()+
  labs(title = "SNAIVE")

ets_3 = fct %>% 
  filter(.model %in% c("ets")) %>% 
  autoplot(lty=2,fill='tomato3')+
  autolayer(nights)+
  theme_cowplot()+
  background_grid()+
  labs(title = "ETS")

plot_grid(theta_1,snaive_2,ets_3,nrow = 3)


# regression with ARIMA ---------------------------------------------------
# https://otexts.com/fpp3/regarima.html

df2_ts
df3_ts
df_merge = df2_ts %>% 
  rename('nights'='Value')

df_merge = left_join(df_merge,df3_ts)
df_merge = rename(df_merge,'unempl' = 'Value')
df_merge

cor(df_merge$nights,df_merge$unempl)
# split train&test with covid
df_merge_train <- filter(df_merge, Date < ymd ('2021-01-01'))
df_merge_test <- filter(df_merge, Date >= ymd ('2021-01-01'))
autoplot(df_merge_train,.vars=nights)

# fit model with regression
fit <- df_merge_train %>% 
model(ARIMA(nights ~ unempl+ pdq(1,1,0)))

report(fit)

#forecast arima with regression
fct_arima = forecast(fit,new_data = df_merge_test)

# save plot with regression 
plt_arima = fct_arima %>% 
autoplot(lty=2,fill='tomato3')+
  autolayer(df_merge)+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "4 month", date_labels = "%y'%m")+
  theme(axis.text.x = element_text(color='grey',size=10,angle=90))+
  labs(title = "ARIMA with regression")



# fit theta
theta_fit = model(df_merge_train,
                  theta_decomp = decomposition_model(
                    STL(nights ~ season(window=Inf)),
                    THETA(season_adjust),
                    SNAIVE(season_year)))
fct_theta = forecast(theta_fit,new_data=df_merge_test)

# compare accuracy for both 
accuracy(fct_arima,df_merge)
accuracy(fct_theta,df_merge)

# plot for theta
plt_theta = fct_theta %>% 
  autoplot(lty=2,fill='tomato3')+
  autolayer(df_merge)+
  theme_cowplot()+
  background_grid(size.major = 0.2)+
  scale_x_yearmonth(date_breaks = "4 month", date_labels = "%y'%m")+
  theme(axis.text.x = element_text(color='grey',size=10,angle=90))+
  labs(title = "THETA")

# plot two plots together
plot_grid(plt_arima,plt_theta,nrow = 2)


# with prophet model ------------------------------------------------------
# https://otexts.com/fpp3/prophet.html
library(fable.prophet)

fit_w_prophet <- df_merge_train %>% 
  model(
    arima = ARIMA(nights ~ unempl+ pdq(1,1,0)),
    ets = ETS(nights),
    prophet = prophet(nights ~ season ('year', 10, type='additive'))
  )

fc_proh = forecast(fit_w_prophet,new_data = df_merge_test)

fc_proh %>% autoplot()
fc_proh %>% accuracy(df_merge) %>% arrange(MAPE)

df_merge_cut = filter(df_merge,Date > ymd("2016-01-01"))
fc_proh %>% 
  autoplot(alpha=0.5)+
  autolayer(df_merge_cut)
