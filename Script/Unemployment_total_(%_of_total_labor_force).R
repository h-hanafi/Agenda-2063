#### Unemployment Rate Model

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("caret", repos = "http://cran.us.r-project.org")

### Inputing AU Member Countries
AU <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon	Central African Republic", 
        "Chad", "Comoros",	"Congo, Dem. Rep.",	"Congo, Rep.",	"Cote d'Ivoire",	"Djibouti",	"Egypt Arab Rep.",	"Equatorial Guinea", "Eritrea",	
        "Eswatini",	"Ethiopia",	"Gabon", "Gambia, The",	"Ghana", "Guinea", "Guinea-Bissau",	"Kenya", "Lesotho", "Liberia",	"Libya",	"Madagascar", 
        "Malawi", "Mali",	"Mauritania",	"Mauritius",	"Morocco",	"Mozambique",	"Namibia",	"Niger", "Nigeria",	"Rwanda",	"Sao Tome and Principe", 
        "Senegal",	"Seychelles", "Sierra Leone",	"Somalia", "South Africa", "South Sudan",	"Sudan", "Tanzania", "Togo",	"Tunisia", "Uganda", 
        "Zambia", "Zimbabwe")


### Loading Unemployment Rate

## Creating Raw Data Folder
Raw_Data_Folder <- file.path(getwd(),"Raw_Data")
dir.create(Raw_Data_Folder)

##Downloading and Unpacking the File
download.file("http://api.worldbank.org/v2/en/indicator/SL.UEM.TOTL.ZS?downloadformat=csv", file.path(Raw_Data_Folder,"API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_422140.zip"), mode = "wb")
Unemployment_zip <- "API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_422140.zip"
Unemployment_csv <- "API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_422140.csv"
unzip(file.path(Raw_Data_Folder,Unemployment_zip), exdir = file.path(Raw_Data_Folder,"Unemployment"))

##Reading the File into R
Unemployment <- read_csv(file.path(Raw_Data_Folder,"Unemployment",Unemployment_csv), skip = 3)


### Wrangling Adjusted net national incompe per capita

## Tidying the Data
colnames(Unemployment) <- str_replace(colnames(Unemployment), " ", "_") 
Unemployment <- Unemployment %>%
  filter(Country_Name %in% AU) %>% 
  gather(key = "Year", value = "Unemployment", `1960`:`2019`, convert = TRUE) %>% 
  select(-c(X65,Indicator_Code)) %>% 
  filter(Country_Name %in% AU) %>% 
  select(-Country_Code, -Indicator_Name)

## Viewing The Data
Unemployment %>% 
  ggplot(aes(Year,Country_Name, fill = Unemployment)) + 
  geom_tile(color = "black") + 
  scale_fill_distiller(palette = "Blues", na.value = "grey50", direction = 1, name = "Unemployment rate") + 
  scale_x_continuous(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) + 
  theme(axis.text.y = element_text(hjust = 1)) +
  ylab("Country")

## Removing years and countries with no data
No_Data <- Unemployment %>%
  group_by(Year) %>% summarize(No_Data = sum(!is.na(Unemployment))) 
Unemployment <- Unemployment %>% 
  left_join(No_Data, by = "Year") %>% filter(No_Data != 0) %>% select(-No_Data) 

No_Data <- Unemployment %>% group_by(Country_Name) %>% summarize(No_Data = sum(!is.na(Unemployment))) 
Unemployment <- Unemployment %>% 
  left_join(No_Data, by = "Country_Name") %>% filter(No_Data != 0) %>% select(-No_Data) 

## Setting Baseline and Test Sets
# Baseline
Unemployment %>% group_by(Country_Name) %>% filter(Year == c(2013)) %>% 
  summarize(Baseline = Unemployment) %>% filter(is.na(Baseline))

Baseline_Unemployment <- Unemployment %>% 
  group_by(Country_Name) %>% filter(Year == (2013)) %>% 
  summarize(Baseline = Unemployment) %>% filter(!is.na(Baseline))

# Tests Set
Unemployment %>% group_by(Country_Name) %>% filter(Year == c(2019)) %>% 
  summarize(Baseline = Unemployment) %>% filter(is.na(Baseline))

Unemployment_test <- Unemployment %>% filter(Year == 2019)

# Removing Test Data
Unemployment <- Unemployment %>% filter(Year != 2019) 

## Data Analysis
# Year Range
Unemployment %>% pull(Year) %>% max()
Unemployment %>% pull(Year) %>% min()

# Viewing the Data
Unemployment %>% 
  ggplot(aes(Year,Country_Name, fill = Unemployment)) + 
  geom_tile(color = "black") + 
  scale_fill_distiller(palette = "Blues", na.value = "black", direction = 1, name = "Unemployment rate") + 
  scale_x_continuous(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) +
  ylab("Country") +
  theme(panel.background = element_rect(fill = "grey50", color = "black"), panel.grid = element_line(color = "grey50"))

# Continent Wide Trend
Unemployment %>% group_by(Year) %>% summarize(Africa_Unemployment = mean(Unemployment)) %>% 
  ggplot(aes(Year,Africa_Unemployment)) + 
  geom_point() + geom_smooth(span = 0.75) +
  ylab("AU Average Unempolyment rate")

#Continent Wide Boxplot
Unemployment %>% group_by(Year) %>% mutate(Africa_Unemployment = mean(Unemployment)) %>%
  ggplot(aes(as.character(Year),Unemployment)) + 
  geom_boxplot() + 
  geom_point(aes(as.character(Year),Africa_Unemployment), color = "blue") +
  scale_color_distiller(palette = "Blues", na.value = "black", direction = 1, name = "Unemployment rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) + 
  xlab("Year") +
  ylab("Unemployment rate")

# Percentage difference between AU Average and the 2013 Baseline
Unemployment %>% 
  left_join(Baseline_Unemployment) %>% 
  group_by(Year) %>% 
  summarize(Africa_Unemployment = mean(Unemployment), Baseline = mean(Baseline)) %>% 
  mutate(Difference_to_Baseline = (Africa_Unemployment - Baseline)/Baseline) %>% 
  ggplot(aes(Year,Difference_to_Baseline)) + 
  geom_point() + 
  geom_smooth() +
  geom_vline(xintercept = 2013, linetype = 2, color = "blue") + 
  geom_hline(data = data_frame(yintercept = c(-0.25,0), color = c("black","red")),
                        aes(yintercept = yintercept, color = color),linetype = c(2,1),show.legend = FALSE) + 
  geom_text(data = data_frame(x = c(2013,2000), y = c(0.15,-0.25), label = c("2013", "25% below Baseline"), angle = c(90,0)), 
            aes(x = x, y = y, label = label, angle = angle, vjust = 1), size = 12 / .pt) + 
  scale_color_manual(values = c("red","black")) +
  ylab("Percentage difference between AU Average \n and the 2013 Baseline")

#Country sample
Country_Sample <- Unemployment %>% 
  filter(Year == max(Year)) %>% 
  left_join(Baseline_Unemployment) %>%  
  mutate(Difference_to_Baseline = (Unemployment - Baseline)/Baseline) %>% 
  mutate(Country_Name = reorder(Country_Name,Difference_to_Baseline)) %>% 
  top_n(-2) %>%
  pull(Country_Name) %>% unique() %>% as.character()

Country_Sample <- Unemployment %>% 
  filter(Year == max(Year)) %>% 
  left_join(Baseline_Unemployment) %>%  
  mutate(Difference_to_Baseline = (Unemployment - Baseline)/Baseline) %>% 
  mutate(Country_Name = reorder(Country_Name,Difference_to_Baseline)) %>% 
  top_n(2) %>% 
  pull(Country_Name) %>% unique() %>% as.character() %>% 
  c(.,Country_Sample)

Unemployment %>% 
  filter(Country_Name %in% Country_Sample) %>%
  left_join(Baseline_Unemployment) %>%  
  mutate(Difference_to_Baseline = (Unemployment - Baseline)/Baseline) %>% 
  mutate(Country_Name = reorder(Country_Name,Difference_to_Baseline)) %>%
  ggplot(aes(Year,Difference_to_Baseline)) + 
  geom_point() + 
  geom_smooth(span = 1) + 
  facet_wrap(~Country_Name, scales = "free") + 
  geom_hline(yintercept = -0.25, color = "red", linetype = 2) + 
  ylab("Percentage difference between Unemployment rate \n and the 2013 Baseline")

### model
## 10 Fold CV
Control <- trainControl(method = "cv", number = 15, p = 0.9)

##Linear Regression
fit_lm_Unemployment <- train(Unemployment ~ ., data = Unemployment, method = "lm", trControl = Control)
pred_lm_Unemployment <- predict(fit_lm_Unemployment, newdata = Unemployment_test)
RMSE_lm <- sqrt(mean((pred_lm_Unemployment - Unemployment_test$Unemployment)^2))

## K-Nearest Neighbors
tune = data.frame(k = 1:5)
fit_knn_Unemployment <- train(Unemployment ~ ., data = Unemployment, method = "knn", tuneGrid = tune, trControl = Control)
plot(fit_knn_Unemployment)
pred_knn_Unemployment <- predict(fit_knn_Unemployment, newdata = Unemployment_test)
RMSE_knn <- sqrt(mean((pred_knn_Unemployment - Unemployment_test$Unemployment)^2))

## Extreme Gradient Boosting
tune <- expand.grid(nrounds = seq(200,300,50), lambda = seq(1,3,0.5), alpha = c(0,1e-04), eta = 0.3)
fit_xgb_Unemployment <- train(Unemployment ~ ., data = Unemployment, method = "xgbLinear", tuneGrid = tune, trControl = Control )
plot(fit_xgb_Unemployment)
pred_xgb_Unemployment <- predict(fit_xgb_Unemployment, newdata = Unemployment_test)
RMSE_xgb <- sqrt(mean((pred_xgb_Unemployment - Unemployment_test$Unemployment)^2))

## Random Forest
tune = data.frame(mtry = seq(40,50,5))
fit_rf_Unemployment<- train(Unemployment ~ ., data = Unemployment, method = "rf", tuneGrid = tune, trControl = Control)
plot(fit_rf_Unemployment)
pred_rf_Unemployment <- predict(fit_rf_Unemployment, newdata = Unemployment_test)
RMSE_rf <- sqrt(mean((pred_rf_Unemployment - Unemployment_test$Unemployment)^2))

## Storing the Results
Results_Unemployment <- data_frame(method = c("lm","knn","xgb","rf"), RMSE = c(RMSE_lm,RMSE_knn,RMSE_xgb,RMSE_rf))

fit_Unemployment <- list(lm = fit_lm_Unemployment, knn = fit_knn_Unemployment, xgb = fit_xgb_Unemployment, rf = fit_rf_Unemployment)
Best_fit_Unemployment <- fit_Unemployment[[Results_Unemployment$method[which.min(Results_Unemployment$RMSE)]]]
Best_fit_Unemployment$method

### 2023 prediction
## generating the prediction
Unemployment_2023 <- Unemployment_test %>% mutate(Year = 2023) %>% select(-Unemployment)

Unemployment_2023 <- Unemployment_2023 %>% 
  mutate(Unemployment = predict(Best_fit_Unemployment, newdata = Unemployment_2023)) %>% 
  left_join(Baseline_Unemployment, by = "Country_Name") %>% 
  mutate(Difference_to_Baseline = (Unemployment - Baseline)/Baseline)

mu_Unemployment_2023 <- Unemployment_2023 %>% summarize(mean(Difference_to_Baseline)) %>% pull()

## Boxplot
Unemployment_2023 %>% 
  ggplot(aes(as.character(Year),Difference_to_Baseline)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.3) + 
  geom_hline(yintercept = -0.25, color = "red", linetype = 2) +
  geom_text(x = 1, y = -0.25, label = "25% below Baseline", vjust = 1, size = 12 / .pt) + 
  xlab("Year") + 
  ylab("Percentage Difference between Baseline Unemployment rate and \n Predicted Unemployment rate")

## Histogram

Unemployment_2023 %>%
  ggplot(aes(Difference_to_Baseline)) +
  geom_histogram(bins = 40, color = "black") + 
  geom_vline(xintercept = mu_Unemployment_2023, color = "red", linetype = 2) + 
  scale_y_continuous(expand = c(0,0))

## Countries achieveing goal
Unemployment_2023 %>% filter(Difference_to_Baseline <= -0.25) %>% 
  select(Country_Name, Unemployment, Difference_to_Baseline)

## Trend
Unemployment_2023 %>% 
  filter(Difference_to_Baseline <= -0.25) %>% select(Country_Name) %>% 
  inner_join(Unemployment, by = c("Country_Name")) %>% 
  left_join(Baseline_Unemployment) %>% 
  mutate(Difference_to_Baseline = (Unemployment - Baseline)/Baseline) %>%
  ggplot(aes(Year, Difference_to_Baseline)) + 
  geom_point(show.legend = FALSE)  + 
  geom_hline(yintercept = -0.25, color = "red", linetype =2) + 
  geom_smooth(span = 0.75) +
  facet_grid(Country_Name~.) + 
  ylab("Percentage Difference between Baseline Unemployment rate and \n Predicted Unemployment rate")

Unemployment_2023 %>% summarise(AU_Unemployment = mean(Unemployment), AU_Difference_to_Baseline = mean(Difference_to_Baseline))
