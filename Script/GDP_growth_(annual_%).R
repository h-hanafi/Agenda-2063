library(tidyverse)
library(RColorBrewer)
library(caret)
library(randomForest)
library(xgboost)


#Inputing AU Member Countries
AU <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon	Central African Republic", 
        "Chad", "Comoros",	"Congo, Dem. Rep.",	"Congo, Rep.",	"Cote d'Ivoire",	"Djibouti",	"Egypt Arab Rep.",	"Equatorial Guinea", "Eritrea",	
        "Eswatini",	"Ethiopia",	"Gabon", "Gambia, The",	"Ghana", "Guinea", "Guinea-Bissau",	"Kenya", "Lesotho", "Liberia",	"Libya",	"Madagascar", 
        "Malawi", "Mali",	"Mauritania",	"Mauritius",	"Morocco",	"Mozambique",	"Namibia",	"Niger", "Nigeria",	"Rwanda",	"Sao Tome and Principe", 
        "Senegal",	"Seychelles", "Sierra Leone",	"Somalia", "South Africa", "South Sudan",	"Sudan", "Tanzania", "Togo",	"Tunisia", "Uganda", 
        "Zambia", "Zimbabwe")


#Loading Adujusted net national income per Capita
Raw_Data_Folder <- file.path(getwd(),"Raw_Data")
dir.create(Raw_Data_Folder)
download.file("http://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.KD.ZG?downloadformat=csv", file.path(Raw_Data_Folder,"API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_422196.zip"), mode = "wb")
GDP_Growth_zip <- "API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_422196.zip"
GDP_Growth_csv <- "API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_422196.csv"
unzip(file.path(Raw_Data_Folder,GDP_Growth_zip), exdir = file.path(Raw_Data_Folder,"GDP_Growth"))

#Reading the File into R
GDP_Growth <- read_csv(file.path(Raw_Data_Folder,"GDP_Growth",GDP_Growth_csv), skip = 3)

####################################################
#Wrangling Adjusted net national incompe per capita
####################################################

#Tidying the Data
colnames(GDP_Growth) <- str_replace(colnames(GDP_Growth), " ", "_") 
GDP_Growth <- GDP_Growth %>%filter(Country_Name %in% AU) %>% gather(key = "Year", value = "GDP_Growth", `1960`:`2019`, convert = TRUE) %>% 
  select(-c(X65,Indicator_Code)) %>% filter(Country_Name %in% AU) %>% select(-Country_Code, -Indicator_Name)

#Viewing The Data
GDP_Growth %>% 
  ggplot(aes(Year,Country_Name, fill = GDP_Growth)) + geom_tile(color = "black") + 
  scale_fill_distiller(palette = "Blues", na.value = "grey50", direction = 1) + 
  scale_x_continuous(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) + 
  theme(axis.text.y = element_text(hjust = 1))

#Removing years and countries with no data
No_Data <- GDP_Growth %>%
  group_by(Year) %>% summarize(No_Data = sum(!is.na(GDP_Growth))) 
GDP_Growth <- GDP_Growth %>% left_join(No_Data, by = "Year") %>% filter(No_Data != 0) %>% select(-No_Data) 

No_Data <- GDP_Growth %>% group_by(Country_Name) %>% summarize(No_Data = sum(!is.na(GDP_Growth))) 
GDP_Growth <- GDP_Growth %>% left_join(No_Data, by = "Country_Name") %>% filter(No_Data != 0) %>% select(-No_Data) 

#Tests Set
GDP_Growth %>% group_by(Country_Name) %>% filter(Year == 2018) %>% 
  summarize(Baseline = GDP_Growth) %>% filter(is.na(Baseline))

GDP_Growth %>% filter(Country_Name == "Eritrea") %>% filter(!is.na(GDP_Growth)) %>% tail()
Eritrea_2018 <- GDP_Growth %>% filter(Country_Name == "Eritrea", Year == 2011) %>% mutate(Year = 2018)

GDP_Growth %>% filter(Country_Name == "South Sudan") %>% filter(!is.na(GDP_Growth)) %>% tail()
South_Sudan_2018 <- GDP_Growth %>% filter(Country_Name == "South Sudan", Year == 2016) %>% mutate(Year = 2018)

GDP_Growth %>% filter(Country_Name == "Somalia") %>% filter(!is.na(GDP_Growth)) %>% tail()

GDP_Growth_test <- GDP_Growth %>% filter(Year == 2018, !Country_Name %in% c("Somalia","Eritrea","South Sudan")) %>% 
  rbind(Eritrea_2018) %>% rbind(South_Sudan_2018)

#Removing Test Data
GDP_Growth <- GDP_Growth %>% filter(Year != 2018) 

#Removing NA's
GDP_Growth <- GDP_Growth %>% filter(!is.na(GDP_Growth))

#Removing Somalia
GDP_Growth <- GDP_Growth %>% filter(Country_Name != "Somalia")

#Data Analysis
#Year Range
GDP_Growth %>% pull(Year) %>% max()
GDP_Growth %>% pull(Year) %>% min()

#Viewing the Data
GDP_Growth %>% 
  ggplot(aes(Year,Country_Name, fill = GDP_Growth)) + geom_tile(color = "black") + 
  scale_fill_distiller(palette = "Blues", na.value = "black", direction = 1) + scale_x_continuous(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) +
  theme(panel.background = element_rect(fill = "grey50", color = "black"), panel.grid = element_line(color = "grey50"))


#Continent Wide Trend
GDP_Growth %>% group_by(Year) %>% summarize(Africa_GDP_Growth = mean(GDP_Growth)) %>% 
  ggplot(aes(Year,Africa_GDP_Growth)) + geom_point() + geom_hline(yintercept = 7, color = "red", linetype = 2) + 
  geom_text(aes(x = 1995, y = 7, label = "7%", vjust = 1))

#Country
Country_Sample <- GDP_Growth %>% select(Country_Name) %>% unique() %>% sample_n(4) %>% pull()
GDP_Growth %>% filter(Country_Name %in% Country_Sample) %>%
  ggplot(aes(Year,GDP_Growth,color = Country_Name)) + geom_point() + facet_wrap(~Country_Name) + geom_hline(yintercept = 7, color = "red", linetype = 2) + 
  ylab("Annual GDP Growth")

GDP_Growth %>% group_by(Country_Name) %>% summarise(Growth_Target = mean(GDP_Growth > 7)) %>% 
  ggplot(aes(Country_Name,sort(Growth_Target))) + geom_bar(stat = "identity") + coord_flip() + ylab("Country") + xlab("Proportion of Years Target is reached")

GDP_Growth %>% group_by(Country_Name) %>% filter(Year >= 2013) %>% summarise(Growth_Target = mean(GDP_Growth > 7)) %>% 
  mutate(Country_Name = reorder(Country_Name,Growth_Target)) %>%
  ggplot(aes(Country_Name,Growth_Target)) + geom_bar(stat = "identity") + 
  coord_flip() + ylab("Country") + xlab("Proportion of Years Target is reached") + 
  ggtitle("Proportion of Years Target is Reached \n since 2013") + theme(plot.title = element_text(hjust = 0.5))

# model
Control <- trainControl(method = "cv", number = 15, p = 0.9)

fit_lm_GDP_Growth <- train(GDP_Growth ~ ., data = GDP_Growth, method = "lm", trControl = Control)
pred_lm_GDP_Growth <- predict(fit_lm_GDP_Growth, newdata = GDP_Growth_test)
RMSE_lm <- sqrt(mean((pred_lm_GDP_Growth - GDP_Growth_test$GDP_Growth)^2))

tune = data.frame(k = 1:10)
fit_knn_GDP_Growth <- train(GDP_Growth ~ ., data = GDP_Growth, method = "knn", tuneGrid = tune, trControl = Control)
plot(fit_knn_GDP_Growth)
pred_knn_GDP_Growth <- predict(fit_knn_GDP_Growth, newdata = GDP_Growth_test)
RMSE_knn <- sqrt(mean((pred_knn_GDP_Growth - GDP_Growth_test$GDP_Growth)^2))

tune <- expand.grid(nrounds = seq(50,150,50), lambda = seq(0.6,1,0.1), alpha = c(0,1e-04,1), eta = 0.3)
fit_xgb_GDP_Growth <- train(GDP_Growth ~ ., data = GDP_Growth, method = "xgbLinear", tuneGrid = tune, trControl = Control )
plot(fit_xgb_GDP_Growth)
pred_xgb_GDP_Growth <- predict(fit_xgb_GDP_Growth, newdata = GDP_Growth_test)
RMSE_xgb <- sqrt(mean((pred_xgb_GDP_Growth - GDP_Growth_test$GDP_Growth)^2))

tune = data.frame(mtry = seq(5,15,1))
fit_rf_GDP_Growth <- train(GDP_Growth ~ ., data = GDP_Growth, method = "rf", tuneGrid = tune, trControl = Control)
plot(fit_rf_GDP_Growth)
pred_rf_GDP_Growth <- predict(fit_rf_GDP_Growth, newdata = GDP_Growth_test)
RMSE_rf <- sqrt(mean((pred_rf_GDP_Growth - GDP_Growth_test$GDP_Growth)^2))

Results_GDP_Growth <- data_frame(method = c("lm","knn","xgb","rf"), RMSE = c(RMSE_lm,RMSE_knn,RMSE_xgb,RMSE_rf))

fit_GDP_Growth <- list(lm = fit_lm_GDP_Growth, knn = fit_knn_GDP_Growth, xgb = fit_xgb_GDP_Growth, rf = fit_rf_GDP_Growth)

#2023 goal
Best_Model <- pred_GDP_Growth[[Results_GDP_Growth$method[which.min(Results_GDP_Growth$RMSE)]]]

GDP_Growth_2023 <- GDP_Growth_test %>% select(Country_Name) %>% mutate(Year = 2023)

GDP_Growth_2023 <- GDP_Growth_2023 %>% mutate(GDP_Growth = predict(Best_Model, newdata = GDP_Growth_2023))

GDP_Growth_2023 %>% group_by(Year) %>% summarise(AU_GDP_Growth = mean(GDP_Growth)) 
  
GDP_Growth_2023 %>%
  ggplot(aes(Year,GDP_Growth)) + geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.3) + geom_hline(yintercept = 7, color = "red", linetype = 2) + 
  ylab("Annual GDP Growth")

GDP_Growth_2023 %>% filter(GDP_Growth >= 7) %>% select(Country_Name) 

GDP_Growth_2023 %>% filter(GDP_Growth >= 7) %>% select(Country_Name) %>% inner_join(GDP_Growth) %>%
  ggplot(aes(Year, GDP_Growth,color = Country_Name)) + geom_point(show.legend = FALSE) + geom_hline(yintercept = 7, color = "red", linetype = 2) +
  scale_color_brewer("Country",palette = "Dark2") + facet_wrap(~Country_Name)

