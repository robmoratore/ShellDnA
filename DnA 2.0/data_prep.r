library(pacman)
p_load(tidyverse, stringr, scales, sn)

gen_corr_vec <- function(in_vec, corr = 0.4, skewness = 2, range = c(0, 1)) {
  m_in <- cbind(scale(in_vec), rsn(n=NROW(in_vec), alpha=skewness))
  out_vec <- m_in %*% solve(chol(var(m_in))) %*% chol(matrix(c(1, corr, corr, 1), ncol=2)) * sd(in_vec) + mean(in_vec)
  out_vec <- rescale(out_vec[,2], to = range)
  return(out_vec)
}


data_path <- "/Users/robmortore/Documents/GitHub/ShellDnA/DnA\ 2.0/dunnhumby-the-complete-journey"

df_transactions <- read.csv(file.path(data_path, "transaction_data.csv"), stringsAsFactors = FALSE)
colnames(df_transactions) <- toupper(colnames(df_transactions))

df_prod <- read.csv(file.path(data_path, "product.csv"), stringsAsFactors = FALSE)
colnames(df_prod) <- toupper(colnames(df_prod))

df_demo <- read.csv(file.path(data_path, "hh_demographic.csv"), stringsAsFactors = FALSE)
colnames(df_demo) <- toupper(colnames(df_demo))

df_coupon <- read.csv(file.path(data_path, "coupon_redempt.csv"), stringsAsFactors = FALSE)
colnames(df_coupon) <- toupper(colnames(df_coupon))

df_campaign <- read.csv(file.path(data_path, "campaign_desc.csv"), stringsAsFactors = FALSE)
colnames(df_campaign) <- toupper(colnames(df_campaign))

df_time_in_app <- read.csv(file.path(data_path, "time_on_app.csv"), stringsAsFactors = FALSE)
colnames(df_time_in_app) <- toupper(colnames(df_time_in_app))

df_transactions <- df_transactions %>%
  filter(HOUSEHOLD_KEY %in% df_demo$HOUSEHOLD_KEY)

all_dates_hh <- expand.grid(seq(min(df_transactions$DAY), max(df_transactions$DAY)),
                         unique(df_transactions$HOUSEHOLD_KEY)) %>%
  ungroup() %>%
  as.matrix() %>%
  as.data.frame() %>%
  rename(DAY = Var1,
         HOUSEHOLD_KEY = Var2) %>%
  left_join(., df_transactions %>% select(DAY, WEEK_NO) %>% unique(), by = "DAY")
  
df_prod_final <- df_prod %>%
  mutate(DEPARTMENT = case_when(grepl("GASOL", SUB_COMMODITY_DESC) ~ "KIOSK-GAS",
                                DEPARTMENT == " " ~ "OTHER",
                                TRUE ~ DEPARTMENT),
         COMMODITY_DESC = case_when(grepl("GASOL", SUB_COMMODITY_DESC) ~ "FUEL",
                                    COMMODITY_DESC == " " ~ "OTHER",
                                TRUE ~ COMMODITY_DESC)) %>%
  select(PRODUCT_ID, DEPARTMENT, COMMODITY_DESC) %>%
  group_by(DEPARTMENT, COMMODITY_DESC) %>%
  mutate(id = group_indices()) %>%
  mutate(id = round(112304982315*id*runif(1, 0, 100)^runif(1, 1, 3), 0),
         id = str_sub(as.character(id), 3, 12)) %>%
  ungroup()

df_prod_ids <- df_prod_final %>%
  select(PRODUCT_ID, id)

df_prod_final <- df_prod_final %>%
  select(-PRODUCT_ID) %>%
  rename(PRODUCT_ID = id) %>%
  select(PRODUCT_ID, DEPARTMENT, COMMODITY_DESC) %>%
  unique() %>%
  arrange(DEPARTMENT, COMMODITY_DESC, PRODUCT_ID)

write.csv(df_prod_final, file.path(data_path, "processed", "clean", "products.csv"), row.names = FALSE)

rm(df_prod)
  
df_transactions_final <- df_transactions %>%
  select(DAY, WEEK_NO, HOUSEHOLD_KEY, PRODUCT_ID, SALES_VALUE, QUANTITY, RETAIL_DISC, COUPON_MATCH_DISC) %>%
  mutate(DISCOUNT = RETAIL_DISC + COUPON_MATCH_DISC) %>%
  select(-RETAIL_DISC, -COUPON_MATCH_DISC) %>%
  left_join(., df_prod_ids, by="PRODUCT_ID") %>%
  select(-PRODUCT_ID) %>%
  rename(PRODUCT_ID = id) %>%
  group_by(DAY, WEEK_NO, HOUSEHOLD_KEY, PRODUCT_ID) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  left_join(df_prod_final, by = "PRODUCT_ID") %>%
  mutate(QUANTITY = case_when(COMMODITY_DESC == "FUEL" ~ round(QUANTITY/1000, 2),
                              TRUE ~ as.double(QUANTITY))) %>%
  select(-DEPARTMENT, -COMMODITY_DESC)  %>%
  arrange(WEEK_NO, DAY, HOUSEHOLD_KEY)
  
write.csv(df_transactions_final, file.path(data_path, "processed", "clean", "transactions.csv"), row.names = FALSE)
  
rm(df_prod_ids, df_transactions)

df_demo_final <- df_demo %>%
  rowwise() %>%
  mutate(AGE_DESC = gsub("\\+", "", AGE_DESC),
         AGE_DESC = gsub("-", " ", AGE_DESC),
         AGE_DESC = mean(lapply(str_split(AGE_DESC, " "), as.integer)[[1]], na.rm = TRUE),
         AGE_DESC = round(AGE_DESC, 0),
         
         INCOME_DESC = gsub("k|under|\\+", "", tolower(INCOME_DESC)),
         INCOME_DESC = gsub("-", " ", INCOME_DESC),
         INCOME_DESC = mean(lapply(str_split(INCOME_DESC, " "), as.integer)[[1]], na.rm = TRUE),
         INCOME_DESC = round(INCOME_DESC*1000, 0),
         
         HOMEOWNER_DESC = case_when(HOMEOWNER_DESC == "Probable Owner" ~ "Homeowner",
                                    HOMEOWNER_DESC == "Probable Renter" ~ "Renter",
                                    TRUE ~ HOMEOWNER_DESC),
         
         KID_CATEGORY_DESC = gsub("\\+", "", KID_CATEGORY_DESC),
         KID_CATEGORY_DESC = gsub("None/Unknown", "0", KID_CATEGORY_DESC),
         KID_CATEGORY_DESC = as.integer(KID_CATEGORY_DESC),
         
         HOUSEHOLD_SIZE_DESC = as.integer(gsub("\\+", "", HOUSEHOLD_SIZE_DESC)),
         
         NUM_ADULTS = case_when(str_detect(HH_COMP_DESC, "2") ~ 2,
                                str_detect(HH_COMP_DESC, "1|Single") ~ 1,
                                TRUE ~ as.double(HOUSEHOLD_SIZE_DESC - KID_CATEGORY_DESC)),
         
         NUM_CHILDREN = HOUSEHOLD_SIZE_DESC - NUM_ADULTS,
         
         MARITAL_STATUS = case_when(MARITAL_STATUS_CODE == "A" ~ 1,
                                    MARITAL_STATUS_CODE == "B" ~ 0,
                                    HOUSEHOLD_SIZE_DESC > 1 ~ 1,
                                    TRUE ~ 0)
         ) %>%
  ungroup() %>%
  select(HOUSEHOLD_KEY, AGE_DESC, HOUSEHOLD_SIZE_DESC, NUM_ADULTS, NUM_CHILDREN, MARITAL_STATUS, INCOME_DESC) %>%
  rename(AGE_MEAN = AGE_DESC,
         HOUSEHOLD_SIZE = HOUSEHOLD_SIZE_DESC,
         INCOME_MEAN = INCOME_DESC)  %>%
  arrange(HOUSEHOLD_KEY)

write.csv(df_demo_final, file.path(data_path, "processed", "clean", "demographics.csv"), row.names = FALSE)

rm(df_demo)  

df_week_trans <- df_transactions_final %>%
  left_join(., all_dates_hh, by = c("DAY", "WEEK_NO", "HOUSEHOLD_KEY")) %>%
  select(WEEK_NO, HOUSEHOLD_KEY, SALES_VALUE, DISCOUNT) %>%
  mutate_at(vars(SALES_VALUE, DISCOUNT), ~ifelse(is.na(.), 0, .)) %>%
  group_by(WEEK_NO, HOUSEHOLD_KEY) %>%
  summarise_all(sum) %>%
  group_by(HOUSEHOLD_KEY) %>%
  arrange(HOUSEHOLD_KEY, WEEK_NO) %>%
  mutate(prev_ammount = lag(SALES_VALUE),
         prev_ammount = ifelse(is.na(prev_ammount), 0, prev_ammount)) %>%
  ungroup() %>%
  mutate(DISCOUNT_2 = abs(DISCOUNT),
         DISCOUNT_2 = (DISCOUNT_2 - min(DISCOUNT_2))/(max(DISCOUNT_2) - min(DISCOUNT_2)) + 1,
         FUEL_PRICE_DIFF = runif(1, 0, 1) + SALES_VALUE/DISCOUNT_2) %>%
  group_by(WEEK_NO) %>%
  mutate(FUEL_PRICE_DIFF = mean(FUEL_PRICE_DIFF),
         FUEL_PRICE_DIFF = (FUEL_PRICE_DIFF/3) - 24) %>%
  ungroup() %>%
  left_join(., df_time_in_app, by = c("WEEK_NO", "HOUSEHOLD_KEY")) %>%
  rename(TIME_IN_APP = TIME_SPENT_ON_APP) %>%
  mutate(FUEL_PRICE_DIFF = round(rescale(FUEL_PRICE_DIFF, to = c(-9, 13)), 0)) %>%
  select(WEEK_NO, HOUSEHOLD_KEY, TIME_IN_APP, FUEL_PRICE_DIFF, SALES_VALUE, DISCOUNT)

# 
# 
# ggplot(df_week_trans, aes(x=TIME_IN_APP*abs(DISCOUNT), y=SALES_VALUE)) +
#   geom_point()
# 
# cor(df_week_trans$TIME_IN_APP*abs(df_week_trans$DISCOUNT), df_week_trans$SALES_VALUE)
# 
# cor(df_week_trans$TIME_IN_APP, df_week_trans$SALES_VALUE)



df_coupon_summary <- df_coupon %>%
  left_join(., all_dates_hh, by = c("HOUSEHOLD_KEY", "DAY")) %>%
  group_by(HOUSEHOLD_KEY, WEEK_NO) %>%
  summarise(NUM_COUPONS = NROW(unique(COUPON_UPC)),
            NUM_CAMPAIGNS = NROW(unique(CAMPAIGN))) %>%
  ungroup()

df_promo_final <- df_coupon_summary %>%
  right_join(., all_dates_hh %>% select(HOUSEHOLD_KEY, WEEK_NO) %>% unique(), by = c("HOUSEHOLD_KEY", "WEEK_NO")) %>%
  mutate_at(vars(NUM_COUPONS, NUM_CAMPAIGNS), ~ifelse(is.na(.), 0, .)) %>%
  left_join(., df_week_trans, by = c("HOUSEHOLD_KEY", "WEEK_NO")) %>%
  select(HOUSEHOLD_KEY, WEEK_NO, TIME_IN_APP, NUM_COUPONS) %>%
  mutate(TIME_IN_APP = ifelse(is.na(TIME_IN_APP), runif(1, 5, 13), TIME_IN_APP),
         TIME_IN_APP = round(TIME_IN_APP, 0)) %>%
  select(WEEK_NO, HOUSEHOLD_KEY, TIME_IN_APP, NUM_COUPONS) %>%
  arrange(WEEK_NO, HOUSEHOLD_KEY)

write.csv(df_promo_final, file.path(data_path, "processed", "clean", "promotions.csv"), row.names = FALSE)

df_campaign_expanded <- lapply(1:NROW(df_campaign), function(x){
  data.frame(CAMPAIGN = df_campaign[x,]$CAMPAIGN, DAY = seq(df_campaign[x,]$START_DAY, df_campaign[x,]$END_DAY))
}) %>%
  do.call("rbind", .) %>%
  arrange(CAMPAIGN, DAY)

df_campaign_final <- df_week_trans %>%
  left_join(all_dates_hh, by = c("WEEK_NO", "HOUSEHOLD_KEY")) %>%
  group_by(DAY, WEEK_NO) %>%
  summarise(FUEL_PRICE_DIFF = mean(FUEL_PRICE_DIFF, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(., df_campaign_expanded, by = "DAY")  %>%
  group_by(WEEK_NO) %>%
  summarise(NUM_CAMPAIGNS = NROW(unique(CAMPAIGN)) - max(is.na(unique(CAMPAIGN))),
            FUEL_PRICE_DIFF = mean(FUEL_PRICE_DIFF, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(WEEK_NO)

write.csv(df_campaign_final, file.path(data_path, "processed", "clean", "campaigns.csv"), row.names = FALSE)

rm(all_dates_hh, df_campaign, df_campaign_expanded, df_coupon, df_coupon_summary, df_week_trans, df_time_in_app)


#### Ruin the data :p

df_transactions_bad <- df_transactions_final %>%
  rbind(., sample_n(., 9536)) %>%
  rbind(., sample_n(., 4787) %>%
          mutate_at(vars(DAY, HOUSEHOLD_KEY), ~.==NA) %>%
          mutate(PRODUCT_ID = ifelse(row_number() %% 7 <= 4, NA, PRODUCT_ID),
                 WEEK_NO = ifelse(row_number() %% 7 <= 4, NA, WEEK_NO),
                 SALES_VALUE = ifelse(row_number() %% 50 <= 14, NA, SALES_VALUE),
                 QUANTITY = ifelse(row_number() %% 35 <= 10, NA, QUANTITY),
                 DISCOUNT = ifelse(row_number() %% 123 >= 41, NA, DISCOUNT))) %>%
  mutate(SALES_VALUE = ifelse(!is.na(SALES_VALUE), paste0("$", SALES_VALUE), NA),
         DISCOUNT = ifelse(!is.na(DISCOUNT) & DISCOUNT!=0,
                           paste0("-$", abs(DISCOUNT)),
                           ifelse(DISCOUNT==0, paste0("$", abs(DISCOUNT)), NA))) %>%
  arrange(WEEK_NO, DAY, HOUSEHOLD_KEY, PRODUCT_ID) %>%
  mutate_all(~ifelse(is.na(.), "9999999", .))

sapply(df_transactions_bad, function(x) sum(x=="9999999"))


df_demo_bad <- df_demo_final %>%
  mutate(NUM_CHILDREN = ifelse(row_number() %% 7 <= 5, NA, NUM_CHILDREN)) %>%
  mutate_all(~ifelse(is.na(.), " ", .))

sapply(df_demo_bad, function(x) sum(x==" "))



df_prod_bad <- df_prod_final %>%
  rbind(., sample_n(., 12) %>%
          mutate(PRODUCT_ID = NA,
                 DEPARTMENT = ifelse(row_number() %% 7 <= 4, NA, DEPARTMENT),
                 COMMODITY_DESC = ifelse(row_number() %% 6 <= 4, NA, COMMODITY_DESC))) %>%
  mutate(PROD_SIZE = ifelse(runif(NROW(.), 0, 1)>0.5, "YES", "NO"),
         PROD_SIZE = ifelse(row_number() %% 12 <= 7, NA, PROD_SIZE))

sapply(df_prod_bad, function(x) sum(is.na(x)))


df_promo_bad <- df_promo_final %>%
  mutate(NUM_EMAILS = 0)
  
sapply(df_promo_bad, function(x) sd(x))


write.csv(df_demo_bad, file.path(data_path, "processed", "unholy", "demographics.csv"), row.names = FALSE)
write.csv(df_prod_bad, file.path(data_path, "processed", "unholy", "products.csv"), row.names = FALSE)
write.csv(df_promo_bad, file.path(data_path, "processed", "unholy", "promotions.csv"), row.names = FALSE)
write.csv(df_transactions_bad, file.path(data_path, "processed", "unholy", "transactions.csv"), row.names = FALSE)
write.csv(df_campaign_final, file.path(data_path, "processed", "unholy", "campaigns.csv"), row.names = FALSE)




