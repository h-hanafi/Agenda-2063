#### Annual GDP Growth

### Inputing AU Member Countries
AU <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon	Central African Republic", 
        "Chad", "Comoros",	"Congo, Dem. Rep.",	"Congo, Rep.",	"Cote d'Ivoire",	"Djibouti",	"Egypt Arab Rep.",	"Equatorial Guinea", "Eritrea",	
        "Eswatini",	"Ethiopia",	"Gabon", "Gambia, The",	"Ghana", "Guinea", "Guinea-Bissau",	"Kenya", "Lesotho", "Liberia",	"Libya",	"Madagascar", 
        "Malawi", "Mali",	"Mauritania",	"Mauritius",	"Morocco",	"Mozambique",	"Namibia",	"Niger", "Nigeria",	"Rwanda",	"Sao Tome and Principe", 
        "Senegal",	"Seychelles", "Sierra Leone",	"Somalia", "South Africa", "South Sudan",	"Sudan", "Tanzania", "Togo",	"Tunisia", "Uganda", 
        "Zambia", "Zimbabwe")


### Loading GDP_Growth
## Creating Raw Data Folder
Raw_Data_Folder <- file.path(getwd(),"Raw_Data")
dir.create(Raw_Data_Folder)
## Downloading and unpacking file
download.file("http://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.KD.ZG?downloadformat=csv", 
              file.path(Raw_Data_Folder,"API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_422196.zip"), mode = "wb")
GDP_Growth_zip <- "API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_422196.zip"
GDP_Growth_csv <- "API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_422196.csv"
unzip(file.path(Raw_Data_Folder,GDP_Growth_zip), exdir = file.path(Raw_Data_Folder,"GDP_Growth"))

## Reading the File into R
GDP_Growth <- read_csv(file.path(Raw_Data_Folder,"GDP_Growth",GDP_Growth_csv), skip = 3)


###Wrangling GDP Growth Data

## Tidying the Data
colnames(GDP_Growth) <- str_replace(colnames(GDP_Growth), " ", "_") 
GDP_Growth <- GDP_Growth %>% 
  filter(Country_Name %in% AU) %>% 
  gather(key = "Year", value = "GDP_Growth", `1960`:`2019`, convert = TRUE) %>% 
  select(-c(X65,Indicator_Code)) %>% 
  filter(Country_Name %in% AU) %>% 
  select(-Country_Code, -Indicator_Name)

## Viewing The Data
GDP_Growth %>%   
  mutate(GDP_Growth = ifelse(GDP_Growth >= 25,25,GDP_Growth), 
         GDP_Growth = ifelse(GDP_Growth <= -25,-25,GDP_Growth)) %>%
  ggplot(aes(Year,Country_Name, fill = GDP_Growth)) + 
  geom_tile(color = "black") + 
  scale_fill_distiller(palette = "RdBu", na.value = "grey50", direction = 1, name = "Annual GDP Growth") + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) + 
  theme(panel.background = element_rect(fill = "grey50", color = "black"), 
        panel.grid = element_line(color = "grey50")) +
  ylab("Country")


## Removing years and countries with no data
No_Data <- GDP_Growth %>%
  group_by(Year) %>% summarize(No_Data = sum(!is.na(GDP_Growth))) 
GDP_Growth <- GDP_Growth %>% 
  left_join(No_Data, by = "Year") %>% filter(No_Data != 0) %>% select(-No_Data) 

No_Data <- GDP_Growth %>% 
  group_by(Country_Name) %>% summarize(No_Data = sum(!is.na(GDP_Growth))) 
GDP_Growth <- GDP_Growth %>% 
  left_join(No_Data, by = "Country_Name") %>% filter(No_Data != 0) %>% select(-No_Data) 

## Tests Set
GDP_Growth %>% group_by(Country_Name) %>% filter(Year == 2018) %>% 
  summarize(Baseline = GDP_Growth) %>% filter(is.na(Baseline))

GDP_Growth %>% filter(Country_Name == "Eritrea") %>% filter(!is.na(GDP_Growth)) %>% tail()
Eritrea_2018 <- GDP_Growth %>% filter(Country_Name == "Eritrea", Year == 2011) %>% mutate(Year = 2018)

GDP_Growth %>% filter(Country_Name == "South Sudan") %>% filter(!is.na(GDP_Growth)) %>% tail()
South_Sudan_2018 <- GDP_Growth %>% filter(Country_Name == "South Sudan", Year == 2016) %>% mutate(Year = 2018)

GDP_Growth %>% filter(Country_Name == "Somalia") %>% filter(!is.na(GDP_Growth)) %>% tail()

GDP_Growth_test <- GDP_Growth %>% 
  filter(Year == 2018, !Country_Name %in% c("Somalia","Eritrea","South Sudan")) %>% 
  rbind(Eritrea_2018) %>% rbind(South_Sudan_2018)

#Removing Test Data
GDP_Growth <- GDP_Growth %>% filter(Year != 2018) 

#Removing NA's
GDP_Growth <- GDP_Growth %>% filter(!is.na(GDP_Growth))

#Removing Somalia
GDP_Growth <- GDP_Growth %>% filter(Country_Name != "Somalia")

### Data Analysis
## Year Range
GDP_Growth %>% pull(Year) %>% max()
GDP_Growth %>% pull(Year) %>% min()

## Viewing the Data
GDP_Growth %>%   
  mutate(GDP_Growth = ifelse(GDP_Growth >= 25,25,GDP_Growth), 
         GDP_Growth = ifelse(GDP_Growth <= -25,-25,GDP_Growth)) %>%
  ggplot(aes(Year,Country_Name, fill = GDP_Growth)) + 
  geom_tile(color = "black") + 
  scale_fill_distiller(palette = "RdBu", na.value = "grey50", direction = 1, name = "Annual GDP Growth") + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) + 
  theme(panel.background = element_rect(fill = "grey50", color = "black"), 
        panel.grid = element_line(color = "grey50")) +
  ylab("Country")


## Continent Wide Trend
GDP_Growth %>% group_by(Year) %>% summarize(Africa_GDP_Growth = mean(GDP_Growth)) %>% 
  ggplot(aes(Year,Africa_GDP_Growth)) + 
  geom_point() + 
  geom_hline(yintercept = 7, color = "red", linetype = 2) + 
  geom_vline(xintercept = 2013, color = "blue", linetype = 2) +
  geom_smooth(span = 0.75) +
  geom_text(data = data.frame(x = c(1995, 2013), 
                              y = c(7,2), 
                              label = c("Target Growth = 7%","2013"), 
                              angle = c(0,90), vjust = 1, 
                              fontface = "italic", size = 12 / .pt), 
            aes(label = label, 
                x = x, y = y, 
                angle = angle, 
                vjust= vjust, 
                fontface = fontface, 
                size = size), 
            show.legend = FALSE) + 
  ylab("Annual GPD Growth: \n Continent Wide")

## Continent Wide Boxplot
GDP_Growth %>% filter(Year >= 2005) %>%
  group_by(Year) %>% mutate(Africa_GDP_Growth = mean(GDP_Growth)) %>%
  ggplot(aes(as.character(Year),GDP_Growth)) + 
  geom_boxplot() + 
  geom_point(aes(as.character(Year),Africa_GDP_Growth), color = "blue") +
  geom_hline(yintercept = 7, color = "red", linetype = 2) +
  scale_color_distiller(palette = "Blues", na.value = "black", direction = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) + 
  xlab("Year") +
  ylab("Annual GDP Growth Rate") +
  ylim(-25,25)

## Country Sample analysis
Country_Sample <- GDP_Growth %>% 
  filter(Year == max(Year)) %>% 
  mutate(Country_Name = reorder(Country_Name,GDP_Growth)) %>% 
  top_n(2) %>%
  pull(Country_Name) %>% unique() %>% as.character()

Country_Sample <- GDP_Growth %>% 
  filter(Year == max(Year)) %>% 
  mutate(Country_Name = reorder(Country_Name,GDP_Growth)) %>% 
  top_n(-2) %>% 
  pull(Country_Name) %>% unique() %>% as.character() %>% 
  c(.,Country_Sample)

GDP_Growth %>% 
  filter(Country_Name %in% Country_Sample) %>%
  ggplot(aes(Year,GDP_Growth)) + 
  geom_point() + 
  geom_smooth(span = 1) + 
  facet_wrap(~Country_Name, scale = "free") + 
  geom_hline(yintercept = 7, color = "red", linetype = 2) + 
  ylab("Annual GDP Growth")

## Proportion of years were target was met
#Complete Data Set
GDP_Growth %>% 
  group_by(Country_Name) %>% 
  summarise(Growth_Target = mean(GDP_Growth > 7)) %>%
  mutate(Country_Name = reorder(Country_Name,Growth_Target)) %>%
  ggplot(aes(Country_Name,Growth_Target)) +
  geom_bar(stat = "identity") + 
  coord_flip() + 
  xlab("Country") + ylab("Proportion of Years Target is reached") +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_discrete(expand = c(0,0)) 

#since 2013
GDP_Growth %>% 
  group_by(Country_Name) %>% 
  filter(Year >= 2013) %>% 
  summarise(Growth_Target = mean(GDP_Growth > 7)) %>% 
  mutate(Country_Name = reorder(Country_Name,Growth_Target)) %>%
  ggplot(aes(Country_Name,Growth_Target)) + geom_bar(stat = "identity") + 
  coord_flip() + 
  xlab("Country") + ylab("Proportion of Years Target is reached  \n since 2013") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_discrete(expand = c(0,0)) 

### Building the model
## 10 Fold Cross Validation
Control <- trainControl(method = "cv", number = 15, p = 0.9)

## Linear Regression
fit_lm_GDP_Growth <- train(GDP_Growth ~ ., data = GDP_Growth, method = "lm", trControl = Control)
pred_lm_GDP_Growth <- predict(fit_lm_GDP_Growth, newdata = GDP_Growth_test)
RMSE_lm <- sqrt(mean((pred_lm_GDP_Growth - GDP_Growth_test$GDP_Growth)^2))

## K-Nearest Neighbours
tune = data.frame(k = 1:10)
fit_knn_GDP_Growth <- train(GDP_Growth ~ ., data = GDP_Growth, method = "knn", tuneGrid = tune, trControl = Control)
plot(fit_knn_GDP_Growth)
pred_knn_GDP_Growth <- predict(fit_knn_GDP_Growth, newdata = GDP_Growth_test)
RMSE_knn <- sqrt(mean((pred_knn_GDP_Growth - GDP_Growth_test$GDP_Growth)^2))

## Gradient Boosting
tune <- expand.grid(nrounds = seq(50,150,50), lambda = seq(0.6,1,0.1), alpha = c(0,1e-04,1), eta = 0.3)
fit_xgb_GDP_Growth <- train(GDP_Growth ~ ., data = GDP_Growth, method = "xgbLinear", tuneGrid = tune, trControl = Control )
plot(fit_xgb_GDP_Growth)
pred_xgb_GDP_Growth <- predict(fit_xgb_GDP_Growth, newdata = GDP_Growth_test)
RMSE_xgb <- sqrt(mean((pred_xgb_GDP_Growth - GDP_Growth_test$GDP_Growth)^2))

## Random Forest
tune = data.frame(mtry = seq(5,15,1))
fit_rf_GDP_Growth <- train(GDP_Growth ~ ., data = GDP_Growth, method = "rf", tuneGrid = tune, trControl = Control)
plot(fit_rf_GDP_Growth)
pred_rf_GDP_Growth <- predict(fit_rf_GDP_Growth, newdata = GDP_Growth_test)
RMSE_rf <- sqrt(mean((pred_rf_GDP_Growth - GDP_Growth_test$GDP_Growth)^2))

## Inputing the Results
Results_GDP_Growth <- data_frame(method = c("lm","knn","xgb","rf"), RMSE = c(RMSE_lm,RMSE_knn,RMSE_xgb,RMSE_rf))

fit_GDP_Growth <- list(lm = fit_lm_GDP_Growth, knn = fit_knn_GDP_Growth, xgb = fit_xgb_GDP_Growth, rf = fit_rf_GDP_Growth)

Best_fit_GDP_Growth <- fit_GDP_Growth[[Results_GDP_Growth$method[which.min(Results_GDP_Growth$RMSE)]]]
Best_fit_GDP_Growth$method

### 2023 goal

## Making our Predictions
GDP_Growth_2023 <- GDP_Growth_test %>% select(Country_Name) %>% mutate(Year = 2023)

GDP_Growth_2023 <- GDP_Growth_2023 %>% mutate(GDP_Growth = predict(Best_fit_GDP_Growth, newdata = GDP_Growth_2023))

mu_GDP_Growth_2023 <- GDP_Growth_2023 %>% summarise(AU_GDP_Growth = mean(GDP_Growth)) %>% pull()

## Histogram
GDP_Growth_2023 %>%
  ggplot(aes(GDP_Growth)) +
  geom_histogram(binwidth = 0.5, color = "black") + 
  scale_x_continuous(breaks = seq(-10,20,5))

##boxplot
GDP_Growth_2023 %>%
  ggplot(aes(as.character(Year),GDP_Growth)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(alpha = 0.3) + 
  geom_hline(yintercept = c(7,mu_GDP_Growth_2023), color = c("red","blue"), linetype = 2) +
  geom_text(data = data.frame(label = c("7%",signif(mu_GDP_Growth_2023,digits =3)), x = c(1.5,1.5), y= c(7,mu_GDP_Growth_2023)),
            aes(label = label, x = x, y = y,vjust = 1)) +
  ylab("Annual GDP Growth") +
  xlab("Year")  

## countries ahcieving the target

GDP_Growth_2023 %>% filter(GDP_Growth >= 7) %>% select(Country_Name) 

GDP_Growth_2023 %>% filter(GDP_Growth >= 7) %>% select(Country_Name) %>% 
  inner_join(GDP_Growth) %>%
  ggplot(aes(Year, GDP_Growth)) + 
  geom_point() + 
  geom_smooth(span = 1) +
  geom_hline(yintercept = 7, color = "red", linetype = 2) +
  scale_color_brewer("Country",palette = "Dark2") + 
  facet_wrap(Country_Name~., ncol = 2, scales = "free") + 
  ylab("Annual GDP Growth")

#### Unemployment Rate Model
### Loading Unemployment Rate

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

### Compounded Model

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("caret", repos = "http://cran.us.r-project.org")

##Preparing the Data
Model <- GDP_Growth %>% full_join(Unemployment)

#Removing years and countries with no data
No_Data <- Model %>%
  group_by(Year) %>% summarize(No_Data = sum(!is.na(Unemployment))) 
Model <- Model %>% left_join(No_Data, by = "Year") %>% filter(No_Data != 0) %>% select(-No_Data) 

No_Data <- Model %>% group_by(Country_Name) %>% summarize(No_Data = sum(!is.na(Unemployment))) 
Model <- Model %>% left_join(No_Data, by = "Country_Name") %>% filter(No_Data != 0) %>% select(-No_Data) 

No_Data <- Model %>%
  group_by(Year) %>% summarize(No_Data = sum(!is.na(GDP_Growth))) 
Model <- Model %>% left_join(No_Data, by = "Year") %>% filter(No_Data != 0) %>% select(-No_Data) 

No_Data <- Model %>% group_by(Country_Name) %>% summarize(No_Data = sum(!is.na(GDP_Growth))) 
Model <- Model %>% left_join(No_Data, by = "Country_Name") %>% filter(No_Data != 0) %>% select(-No_Data) 

#Removing NA's
Model <- Model %>% filter(!is.na(GDP_Growth))

#Setting Test Data
Unemployment_2018 <- Unemployment %>% filter(Year == 2018)
Model_test <- GDP_Growth_test %>% left_join(Unemployment_2018)

Model_test <- Model_test %>% filter(!is.na(GDP_Growth),!is.na(Unemployment))

##Viewing The Data
#GDP
Model %>% 
  mutate(GDP_Growth = ifelse(GDP_Growth >= 25,25,GDP_Growth), 
         GDP_Growth = ifelse(GDP_Growth <= -25,-25,GDP_Growth)) %>%
  ggplot(aes(Year,Country_Name, fill = GDP_Growth)) + 
  geom_tile(color = "black") + 
  scale_fill_distiller(palette = "RdBu", na.value = "grey50", direction = 1, name = "Annual GDP Growth") + 
  scale_x_continuous(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) + 
  ylab("Country") +
  theme(axis.text.y = element_text(hjust = 1), 
        panel.background = element_rect(fill = "grey50", color = "black"), 
        panel.grid = element_line(color = "grey50"))

# unemployment
Model %>% 
  ggplot(aes(Year,Country_Name, fill = Unemployment)) + 
  geom_tile(color = "black") + 
  scale_fill_distiller(palette = "Blues", na.value = "grey50", direction = 1, name = "Unemployment Rate") + 
  scale_x_continuous(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) + 
  ylab("Country") +
  theme(axis.text.y = element_text(hjust = 1), 
        panel.background = element_rect(fill = "grey50", color = "black"), 
        panel.grid = element_line(color = "grey50"))

# correlation vs. years
Model %>% group_by(Year) %>% summarize(cor = cor(GDP_Growth,Unemployment)) %>% 
  ggplot(aes(Year,cor)) + 
  geom_point(aes(color = ifelse(cor <= 0, "red","blue")), show.legend = FALSE) + geom_smooth(span = 0.8, alpha = 0.2,color = "black") + 
  ylab("Correlation between Annual GDP Growth and \n Unemployment Rate")

# correlation by country
Model %>% group_by(Country_Name) %>% summarize(cor = cor(GDP_Growth,Unemployment)) %>% 
  ggplot(aes(Country_Name,cor)) + 
  geom_point(aes(color = ifelse(cor <= 0, "red","blue")), show.legend = FALSE) + 
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.25)) + 
  ylab("Correlation between Annual GDP Growth and \n Unemployment Rate")

## Predicting Unemployment
# Setting the 10 fold cross validation
Control <- trainControl(method = "cv", number = 15, p = 0.9)

# Linear Regression Model
fit_lm_Model_Unemployment <- train(Unemployment ~ ., data = Model, method = "lm", trControl = Control)
pred_lm_Model_Unemployment <- predict(fit_lm_Model_Unemployment, newdata = Model_test)
RMSE_lm <- sqrt(mean((pred_lm_Model_Unemployment - Model_test$Unemployment)^2))

# k-Nearest Neighbours
tune = data.frame(k = 1:15)
fit_knn_Model_Unemployment <- train(Unemployment ~ ., data = Model, method = "knn", 
                                    tuneGrid = tune, trControl = Control)
plot(fit_knn_Model_Unemployment)
pred_knn_Model_Unemployment <- predict(fit_knn_Model_Unemployment, newdata = Model_test)
RMSE_knn <- sqrt(mean((pred_knn_Model_Unemployment - Model_test$Unemployment)^2))

# Gradient Boosting
tune <- expand.grid(nrounds = seq(300,400,50), lambda = seq(1,3,0.5), alpha = c(0,1e-04), eta = 0.3)
fit_xgb_Model_Unemployment <- train(Unemployment ~ ., data = Model, 
                                    method = "xgbLinear", trControl = Control )
plot(fit_xgb_Model_Unemployment)
pred_xgb_Model_Unemployment <- predict(fit_xgb_Model_Unemployment, newdata = Model_test)
RMSE_xgb <- sqrt(mean((pred_xgb_Model_Unemployment - Model_test$Unemployment)^2))

# Random Forest
tune = data.frame(mtry = seq(0,20,10))
fit_rf_Model_Unemployment <- train(Unemployment ~ ., data = Model, 
                                   method = "rf", tuneGrid = tune)
plot(fit_rf_Model_Unemployment)
pred_rf_Model_Unemployment <- predict(fit_rf_Model_Unemployment, newdata = Model_test)
RMSE_rf <- sqrt(mean((pred_rf_Model_Unemployment - Model_test$Unemployment)^2))

# Inputting Results
Results_Model_Unemployment <- data_frame(method = c("lm","knn","xgb","rf"), RMSE = c(RMSE_lm,RMSE_knn,RMSE_xgb,RMSE_rf))

Results_Unemployment %>% full_join(Results_Model_Unemployment, by = "method",suffix = c("_Original","_Compounded"))

fit_Model_Unemployment <- list(lm = fit_lm_Model_Unemployment, knn = fit_knn_Model_Unemployment, 
                               xgb = fit_xgb_Model_Unemployment, rf = fit_rf_Model_Unemployment)

#2023 Unemployment Prediction
Best_fit_Model_Unemployment <- fit_Model_Unemployment[[Results_Model_Unemployment$method[which.min(Results_Model_Unemployment$RMSE)]]]
Best_fit_Model_Unemployment$method

Model_Unemployment_2023 <- Model_test %>% mutate(Year = 2023) %>% select(Country_Name,Year)

Model_Unemployment_2023 <- Model_Unemployment_2023 %>% 
  mutate(GDP_Growth = predict(Best_fit_GDP_Growth, newdata = .)) %>%
  mutate(Unemployment = predict(Best_fit_Model_Unemployment, newdata = .)) %>% 
  left_join(Baseline_Unemployment, by = "Country_Name") %>% 
  mutate(Difference_to_Baseline = (Unemployment - Baseline)/Baseline)

Model_Unemployment_2023 %>% 
  ggplot(aes(as.character(Year),Difference_to_Baseline)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.3) + 
  geom_hline(yintercept = -0.25, color = "red", linetype = 2) +
  geom_text(x = 1, y = -0.25, label = "25% below Baseline", vjust = 1, size = 12 / .pt) + 
  xlab("Year") + 
  ylab("Percentage Difference between Baseline Unemployment rate and \n Predicted Unemployment rate")

Model_Unemployment_2023 %>% filter(Difference_to_Baseline <= -0.25) %>% select(Country_Name, Unemployment, Difference_to_Baseline)

Model_Unemployment_2023 %>% 
  filter(Difference_to_Baseline <= -0.25) %>% select(Country_Name) %>% 
  inner_join(Unemployment, by = c("Country_Name")) %>% 
  left_join(Baseline_Unemployment) %>% 
  mutate(Difference_to_Baseline = (Unemployment - Baseline)/Baseline) %>%
  ggplot(aes(Year, Difference_to_Baseline)) + 
  geom_point(show.legend = FALSE) + scale_color_brewer("Country",palette = "Dark2") + 
  geom_hline(yintercept = -0.25, color = "red", linetype =2) + 
  geom_smooth(span = 0.75) +
  facet_grid(Country_Name~.) + 
  ylab("Percentage Difference between Baseline Unemployment rate and \n Predicted Unemployment rate")

Model_Unemployment_2023 %>% summarise(AU_Unemployment = mean(Unemployment), AU_Difference_to_Baseline = mean(Difference_to_Baseline))

## Predicting GDP_Growth
# Linear Regression
fit_lm_Model_GDP_Growth <- train(GDP_Growth ~ ., data = Model, method = "lm", trControl = Control)
pred_lm_Model_GDP_Growth <- predict(fit_lm_Model_GDP_Growth, newdata = Model_test)
RMSE_lm <- sqrt(mean((pred_lm_Model_GDP_Growth - Model_test$GDP_Growth)^2))

#k-Nearest Neighbours
tune = data.frame(k = 90:100)
fit_knn_Model_GDP_Growth <- train(GDP_Growth ~ ., data = Model, method = "knn", tuneGrid = tune, trControl = Control)
plot(fit_knn_Model_GDP_Growth)
pred_knn_Model_GDP_Growth <- predict(fit_knn_Model_GDP_Growth, newdata = Model_test)
RMSE_knn <- sqrt(mean((pred_knn_Model_GDP_Growth - Model_test$GDP_Growth)^2))

# Gradient Boosting
tune <- expand.grid(nrounds = 50, lambda = c(0,1e-04,0.1,1), alpha = c(0,1e-04,0.1,1), eta = 0.3)
fit_xgb_Model_GDP_Growth <- train(GDP_Growth ~ ., data = Model, method = "xgbLinear", trControl = Control )
plot(fit_xgb_Model_GDP_Growth)
pred_xgb_Model_GDP_Growth <- predict(fit_xgb_Model_GDP_Growth, newdata = Model_test)
RMSE_xgb <- sqrt(mean((pred_xgb_Model_GDP_Growth - Model_test$GDP_Growth)^2))

# Random Forest
tune = data.frame(mtry = seq(2,10,2))
fit_rf_Model_GDP_Growth <- train(GDP_Growth ~ ., data = Model, method = "rf", tuneGrid = tune, trControl = Control)
plot(fit_rf_Model_GDP_Growth)
pred_rf_Model_GDP_Growth <- predict(fit_rf_Model_GDP_Growth, newdata = Model_test)
RMSE_rf <- sqrt(mean((pred_rf_Model_GDP_Growth - Model_test$GDP_Growth)^2))

# Inputting results
Results_Model_GDP_Growth <- data_frame(method = c("lm","knn","xgb","rf"), RMSE = c(RMSE_lm,RMSE_knn,RMSE_xgb,RMSE_rf))

Results_GDP_Growth %>% full_join(Results_Model_GDP_Growth, by = "method",suffix = c("_Original","_Compounded"))

fit_Model_GDP_Growth <- list(lm = fit_lm_Model_GDP_Growth, knn = fit_knn_Model_GDP_Growth, xgb = fit_xgb_Model_GDP_Growth, rf = fit_rf_Model_GDP_Growth)
Best_fit_Model_GDP_Growth <- fit_Model_GDP_Growth[[Results_Model_GDP_Growth$method[which.min(Results_Model_GDP_Growth$RMSE)]]]
Best_fit_Model_GDP_Growth$method

## 2023 GDP Growth prediction
# Generating the Prediction
Model_GDP_Growth_2023 <- Model_test %>% mutate(Year = 2023) %>% select(Country_Name,Year)

Model_GDP_Growth_2023 <- Model_GDP_Growth_2023 %>% 
  mutate(Unemployment = predict(Best_fit_Unemployment, newdata = .)) %>%
  mutate(GDP_Growth = predict(Best_fit_Model_GDP_Growth, newdata = .))

mu_Model_GDP_Growth_2023 <- Model_GDP_Growth_2023 %>% summarise(AU_GDP_Growth = mean(GDP_Growth)) %>% pull()

# Histogram
Model_GDP_Growth_2023 %>%
  ggplot(aes(GDP_Growth)) +
  geom_histogram(bins = 25, color = "black") + 
  geom_vline(xintercept = mu_GDP_Growth_2023, color = "blue", linetype = 2) +
  scale_x_continuous(breaks = c(mu_GDP_Growth_2023,7,seq(-10,20,5))) +
  scale_y_continuous(expand = c(0,0))

# Boxplot
Model_GDP_Growth_2023 %>%
  ggplot(aes(as.character(Year),GDP_Growth)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(alpha = 0.3) + 
  geom_hline(yintercept = c(7,mu_Model_GDP_Growth_2023), color = c("red","blue"), linetype = 2) +
  geom_text(data = data.frame(label = c("7%",signif(mu_Model_GDP_Growth_2023,digits =3)), x = c(1.5,1.5), y= c(7,mu_Model_GDP_Growth_2023)),
            aes(label = label, x = x, y = y,vjust = 1)) +
  ylab("Annual GDP Growth") +
  xlab("Year")  

# Succesful countries
Model_GDP_Growth_2023 %>% filter(GDP_Growth >= 7) %>% select(Country_Name) 

Model_GDP_Growth_2023 %>% filter(GDP_Growth >= 7) %>% select(Country_Name) %>% 
  inner_join(GDP_Growth) %>%
  ggplot(aes(Year, GDP_Growth)) + 
  geom_point() + 
  geom_hline(yintercept = 7, color = "red", linetype = 2) +
  geom_smooth(span = 1) + 
  scale_color_brewer("Country",palette = "Dark2") + 
  facet_wrap(.~Country_Name, scales = "free",ncol = 2) + 
  ylab("Annual GDP Growth")




