library(pacman)
p_load(tidyverse, stringr, scales)

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

write.csv(df_prod_final, file.path(data_path, "processed", "products.csv"), row.names = FALSE)

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
  
write.csv(df_transactions_final, file.path(data_path, "processed", "transactions.csv"), row.names = FALSE)
  
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
         
         MARITAL_STATUS = case_when(MARITAL_STATUS_CODE == "A" ~ 1,
                                    MARITAL_STATUS_CODE == "B" ~ 0,
                                    TRUE ~ as.double(NA))
         ) %>%
  ungroup() %>%
  select(HOUSEHOLD_KEY, AGE_DESC, HOUSEHOLD_SIZE_DESC, NUM_ADULTS, MARITAL_STATUS, INCOME_DESC) %>%
  rename(AGE_MEAN = AGE_DESC,
         HOUSEHOLD_SIZE = HOUSEHOLD_SIZE_DESC,
         INCOME_MEAN = INCOME_DESC)  %>%
  arrange(HOUSEHOLD_KEY)

write.csv(df_demo_final, file.path(data_path, "processed", "demographics.csv"), row.names = FALSE)

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
  mutate(DISCOUNT = abs(DISCOUNT),
         DISCOUNT = (DISCOUNT - min(DISCOUNT))/(max(DISCOUNT) - min(DISCOUNT)) + 1,
         FUEL_PRICE_DIFF = runif(1, 0, 1) + SALES_VALUE/DISCOUNT,
         TIME_IN_APP = 0.2*(3*runif(NROW(.), 0, 1) + 400 - prev_ammount*0.7 - SALES_VALUE/10),
         TIME_IN_APP = ifelse(TIME_IN_APP<=0, TIME_IN_APP + 15, TIME_IN_APP),
         TIME_IN_APP = abs(TIME_IN_APP - max(TIME_IN_APP)),
         TIME_IN_APP = round(TIME_IN_APP*runif(NROW(.), 0, 10)/5 + runif(NROW(.), 0, 10), 0),
         TIME_IN_APP = ifelse(TIME_IN_APP > runif(NROW(.), 98, 129), TIME_IN_APP * runif(1, 0.3, 0.5), TIME_IN_APP),
         TIME_IN_APP = ifelse(TIME_IN_APP < runif(NROW(.), 10, 30), TIME_IN_APP * runif(1, 1.3, 1.5), TIME_IN_APP),
         TIME_IN_APP = rescale(TIME_IN_APP, to = c(0, 127)),
         TIME_IN_APP = round(TIME_IN_APP, 0)) %>%
  group_by(WEEK_NO) %>%
  mutate(FUEL_PRICE_DIFF = mean(FUEL_PRICE_DIFF),
         FUEL_PRICE_DIFF = (FUEL_PRICE_DIFF/3) - 24) %>%
  ungroup() %>%
  mutate(FUEL_PRICE_DIFF = round(rescale(FUEL_PRICE_DIFF, to = c(-9, 13)), 0)) %>%
  select(WEEK_NO, HOUSEHOLD_KEY, TIME_IN_APP, FUEL_PRICE_DIFF)

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
  mutate(TIME_IN_APP = ifelse(is.na(TIME_IN_APP), runif(1, 5, 13), TIME_IN_APP)) %>%
  select(WEEK_NO, HOUSEHOLD_KEY, TIME_IN_APP, NUM_COUPONS) %>%
  arrange(WEEK_NO, HOUSEHOLD_KEY)

write.csv(df_promo_final, file.path(data_path, "processed", "promotions.csv"), row.names = FALSE)

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

write.csv(df_campaign_final, file.path(data_path, "processed", "campaigns.csv"), row.names = FALSE)

rm(all_dates_hh, df_campaign, df_campaign_expanded, df_coupon, df_coupon_summary, df_week_trans)




