if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, xgboost, caret, corrplot, zoo, chron, tis, suncalc, hydroTSM, forecast)

file <- file.choose()
dir <- dirname(file)
setwd(dir)

wx_f <- list.files(pattern = glob2rx("results*.csv"))

df_wx <- (lapply(wx_f, function(x) read.csv(x, stringsAsFactors = FALSE)))
df_wx <- do.call("rbind", df_wx) %>%
  rename(month=mo,
         dayofmonth=da,
         precipitation=prcp) %>%
  mutate(temp = (temp - 32)*(5/9))

df <- read.csv(file, sep=",", stringsAsFactors = FALSE) %>%
  rename(date=Datetime,
         load=PJME_MW) %>%
  arrange(date) %>%
  mutate(date=ymd_hms(date),
         split=ifelse(date<="2015-01-01", "train", "test")) %>%
  group_by(split) %>%
  mutate(load_peak = rnorm(n() , mean=30000, sd=1000)) %>%
  ungroup() %>%
  mutate(load = ifelse(load<=18750, 18503, load),
         year=year(date),
         quarter=quarter(date),
         month=month(date),
         weekofyear=week(date),
         dayofyear=dayOfYear(date),
         dayofmonth=dayOfMonth(date),
         dayofweek=dayOfWeek(date),
         hour=hour(date),
         sunrise=getSunlightTimes(date = as.Date(date), lat = 40, lon = -75, tz = "EST", keep = c("sunrise", "sunset"))$sunrise,
         sunset=getSunlightTimes(date = as.Date(date), lat = 40, lon = -75, tz = "EST", keep = c("sunrise", "sunset"))$sunset,
         daytime=ifelse(date>=sunrise & date<=sunset, 1, 0),
         weekend=as.numeric(is.weekend(date)),
         holiday=as.numeric(isHoliday(date)),
         businessday = ifelse(weekend==0 & holiday==0, 1, 0),
         season=time2season(date, out.fmt="seasons"),
         season=case_when(season=="winter"~1,
                          season=="spring"~2,
                          season=="summer"~3,
                          season=="autumm"~4,
                          TRUE~5)) %>%
  select(-sunrise, -sunset) %>%
  left_join(., df_wx, by = c("year", "month", "dayofmonth")) %>%
  mutate(temp=as.numeric(na.interp(temp)),
         precipitation=as.numeric(na.interp(precipitation))) %>%
  arrange(date)

df_loadmean_prevweek <- df %>%
  arrange(year, weekofyear) %>%
  group_by(year, weekofyear, split) %>%
  summarise(loadmean_prevweek = mean(load, na.rm = TRUE)) %>%
  group_by(split) %>%
  mutate(loadmean_prevweek = lag(loadmean_prevweek, 1)) %>%
  ungroup()

df <- left_join(df, df_loadmean_prevweek, by = c("year", "weekofyear", "split")) %>%
  select(date, load, loadmean_prevweek, everything()) %>%
  select(-split)

df$precipitation[c(23049, 8678, 23456, 5345, 145234, 123905)] <- NA
df$temp[c(1123, 9048, 10485)] <- df$temp[c(1123, 9048, 129485)]*17

write.table(df, file = "../PJM_Load.csv", sep = ",", row.names = FALSE, na = "")

ggplot(df, aes(x=date, y=loadmean_prevweek)) +
  geom_point()

training <- df %>% filter(date <= "2015-01-01")
testing <- df %>% filter(date > "2015-01-01")

X_train = xgb.DMatrix(as.matrix(training %>% select(-load, -date, -season)))
y_train = training$load
X_test = xgb.DMatrix(as.matrix(testing %>% select(-load, -date, -season)))
y_test = testing$load

xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100,200),
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)

set.seed(0) 
xgb_model = train(
  X_train,
  y_train,  
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)

lm_model <- lm(load ~ lasthour, training)

predicted = predict(xgb_model, X_test)
residuals = y_test - predicted
MSE = mean(residuals^2)
RMSE = sqrt(MSE)
MSE
RMSE
MAE(predicted, y_test)

predicted_lm = predict(lm_model, testing)
residuals_lm = testing$load - predicted_lm
MSE_lm = mean(residuals_lm^2)
RMSE_lm = sqrt(MSE_lm)
MSE_lm
RMSE_lm
MAE(predicted_lm, testing$load)

xgb.importance(colnames(X_train), model = xgb_model$finalModel)

plot_df = as.data.frame(cbind(predicted = predicted,
                              actual = y_test))

ggplot(plot_df, aes(predicted, actual)) +
  geom_point(color = "darkred", alpha = 0.5) + 
  geom_abline()

test_date <- cbind(testing, actual=y_test, xgb=predicted, lm=predict(lm_model, testing)) %>%
  select(date, actual, xgb, lm) %>%
  gather(metric, value, -date, -actual) %>%
  mutate(residuals=actual-value)

ggplot(test_date, aes(x=date, y=residuals, color=metric)) +
  geom_line()


#Class

df_model <- df %>%
  select(year, month, dayofmonth, load, season) %>%
  group_by(year, month, dayofmonth) %>%
  summarise(load=sum(load), season=as.factor(mean(season))) %>%
  ungroup() 

ggplot(df_model, aes(x=season, y=load)) +
  geom_boxplot()


training <- df_model %>% filter(year <= 2015)
testing <- df_model %>% filter(year > 2015)

X_train = xgb.DMatrix(as.matrix(training %>% select(load)))
y_train = training$season
X_test = xgb.DMatrix(as.matrix(testing %>% select(load)))
y_test = testing$season

xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100,200),
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)

set.seed(0) 
xgb_model = train(
  X_train,
  y_train,  
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)

confusionMatrix(predict(xgb_model, X_test), y_test)
