library(tidyverse)
library(RColorBrewer)
library(caret)
library(rpart)
library(randomForest)

#Inputing AU Member Countries
AU <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon	Central African Republic", 
        "Chad", "Comoros",	"Congo, Dem. Rep.",	"Congo, Rep.",	"C?te d'Ivoire",	"Djibouti",	"Egypt Arab Rep.",	"Equatorial Guinea", "Eritrea",	
        "Eswatini",	"Ethiopia",	"Gabon", "Gambia, The",	"Ghana", "Guinea", "Guinea-Bissau",	"Kenya", "Lesotho", "Liberia",	"Libya",	"Madagascar", 
        "Malawi", "Mali",	"Mauritania",	"Mauritius",	"Morocco",	"Mozambique",	"Namibia",	"Niger", "Nigeria",	"Rwanda",	"S?o Tom? and Principe", 
        "Senegal",	"Seychelles", "Sierra Leone",	"Somalia", "South Africa", "South Sudan",	"Sudan", "Tanzania", "Togo",	"Tunisia", "Uganda", 
        "Zambia", "Zimbabwe")


#Loading Adujusted net national income per Capita
Raw_Data_Folder <- file.path(getwd(),"Raw_Data")
dir.create(Raw_Data_Folder)
download.file("http://api.worldbank.org/v2/en/indicator/SL.UEM.TOTL.ZS?downloadformat=csv", file.path(Raw_Data_Folder,"API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_422140.zip"), mode = "wb")
Unemployment_zip <- "API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_422140.zip"
Unemployment_csv <- "API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_422140.csv"
unzip(file.path(Raw_Data_Folder,Unemployment_zip), exdir = file.path(Raw_Data_Folder,"Unemployment"))

#Reading the File into R
Unemployment <- read_csv(file.path(Raw_Data_Folder,"Unemployment",Unemployment_csv), skip = 3)

####################################################
#Wrangling Adjusted net national incompe per capita
####################################################

#Tidying the Data
colnames(Unemployment) <- str_replace(colnames(Unemployment), " ", "_") 
Unemployment <- Unemployment %>%filter(Country_Name %in% AU) %>% gather(key = "Year", value = "Unemployment", `1960`:`2019`, convert = TRUE) %>% 
  select(-c(X65,Indicator_Code)) %>% filter(Country_Name %in% AU) %>% select(-Country_Code, -Indicator_Name)

#Viewing The Data
Unemployment %>% 
  ggplot(aes(Year,Country_Name, fill = Unemployment)) + geom_tile(color = "black") + 
  scale_fill_distiller(palette = "Blues", na.value = "grey50", direction = 1) + 
  scale_x_continuous(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) + 
  theme(axis.text.y = element_text(hjust = 1))

#Removing years and countries with no data
No_Data <- Unemployment %>%
  group_by(Year) %>% summarize(No_Data = sum(!is.na(Unemployment))) 
Unemployment <- Unemployment %>% left_join(No_Data, by = "Year") %>% filter(No_Data != 0) %>% select(-No_Data) 

No_Data <- Unemployment %>% group_by(Country_Name) %>% summarize(No_Data = sum(!is.na(Unemployment))) 
Unemployment <- Unemployment %>% left_join(No_Data, by = "Country_Name") %>% filter(No_Data != 0) %>% select(-No_Data) 

#Setting Baseline and Test Sets
#Baseline
Unemployment %>% group_by(Country_Name) %>% filter(Year == c(2013)) %>% 
  summarize(Baseline = Unemployment) %>% filter(is.na(Baseline))

Baseline_Unemployment <- Unemployment %>% group_by(Country_Name) %>% filter(Year == (2013)) %>% 
  summarize(Baseline = Unemployment) %>% filter(!is.na(Baseline))

#Tests Set
Unemployment %>% group_by(Country_Name) %>% filter(Year == c(2019)) %>% 
  summarize(Baseline = Unemployment) %>% filter(is.na(Baseline))

Unemployment_test <- Unemployment %>% filter(Year == 2019)

#Removing Test Data
Unemployment <- Unemployment %>% filter(Year != 2019) 

#Data Analysis
#Year Range
Unemployment %>% pull(Year) %>% max()
Unemployment %>% pull(Year) %>% min()

#Viewing the Data
Unemployment %>% 
  ggplot(aes(Year,Country_Name, fill = Unemployment)) + geom_tile(color = "black") + 
  scale_fill_distiller(palette = "Blues", na.value = "black", direction = 1) + scale_x_continuous(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) +
  theme(panel.background = element_rect(fill = "grey50", color = "black"), panel.grid = element_line(color = "grey50"))

#Continent Wide Trend
Unemployment %>% group_by(Year) %>% summarize(Africa_Unemployment = mean(Unemployment)) %>% ggplot(aes(Year,Africa_Unemployment)) + geom_point() + geom_smooth(span = 0.75)

#Continent Wide Boxplot
Unemployment %>% ggplot(aes(as.character(Year),Unemployment)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Year")

#Regularized Boxpolt
Unemployment %>% group_by(Year) %>% mutate(Unemployment_R = Unemployment - mean(Unemployment)) %>% group_by(Country_Name) %>% mutate(Unemployment_mean = Unemployment_R - mean(Unemployment)) %>% 
  ggplot(aes(as.character(Year),Unemployment_R)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Year") + ylab("Unemployment")

#Percentage difference to the 2013 Baseline
Unemployment %>% left_join(Baseline_Unemployment) %>% group_by(Year) %>% summarize(Africa_Unemployment = mean(Unemployment), Baseline = mean(Baseline)) %>% 
  mutate(Difference_to_Baseline = (Africa_Unemployment - Baseline)/Baseline) %>% 
  ggplot(aes(Year,Difference_to_Baseline)) + geom_point() + geom_vline(xintercept = 2013, linetype = 2, color = "orange3") + 
  geom_hline(yintercept = -0.25, linetype = 2, color = "red3") + 
  geom_text(data = data_frame(x = c(2013,2000), y = c(0.15,-0.25), label = c("2013", "-25%"), angle = c(90,0)), aes(x = x, y = y, label = label, angle = angle, vjust = 1))

# model
fit_lm <- train(Unemployment ~ Country_Name + Year, data = Unemployment, method = "lm")
pred_lm <- predict(fit_lm, newdata = Unemployment_test)
RMSE_lm <- sqrt(mean((pred_lm - Unemployment_test$Unemployment)^2))

tune = data.frame(k = 1:5)
fit_knn <- train(Unemployment ~ Country_Name + Year, data = Unemployment, method = "knn", tuneGrid = tune)
plot(fit_knn)
pred_knn <- predict(fit_knn, newdata = Unemployment_test)
RMSE_knn <- sqrt(mean((pred_knn - Unemployment_test$Unemployment)^2))

tune = data.frame(cp = seq(0,0.5,0.05))
fit_rpart <- train(Unemployment ~ Country_Name + Year, data = Unemployment, method = "rpart", tuneGrid = tune)
plot(fit_rpart)
pred_rpart <- predict(fit_rpart, newdata = Unemployment_test)
RMSE_rpart <- sqrt(mean((pred_rpart - Unemployment_test$Unemployment)^2))

tune = data.frame(mtry = seq(40,60,10))
fit_rf <- train(Unemployment ~ Country_Name + Year, data = Unemployment, method = "rf", tuneGrid = tune)
plot(fit_rf)
pred_rf <- predict(fit_rf, newdata = Unemployment_test)
RMSE_rf <- sqrt(mean((pred_rf - Unemployment_test$Unemployment)^2))

Model_results_Unemployment <- data_frame(method = c("lm","knn","rpart","rf"), RMSE = c(RMSE_lm,RMSE_knn,RMSE_rpart,RMSE_rf))

#2023 goal
Unemployment_2023 <- Unemployment_test %>% mutate(Year = 2023) %>% select(-Unemployment)

Unemployment_2023 <- Unemployment_2023 %>% mutate(Unemployment = pred_rf)

Unemployment_2023 <- Unemployment_2023 %>% left_join(Baseline_Unemployment, by = "Country_Name") %>% mutate(Difference_to_Baseline = (Unemployment - Baseline)/Baseline)

Unemployment_2023 %>% ggplot(aes(Country_Name,Difference_to_Baseline)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0)) + 
  geom_hline(yintercept = -0.25, color = "red", linetype = 2) + geom_text(x = 40, y = -0.25, label = "-25%", vjust = 1)

Unemployment_2023 %>% ggplot(aes(as.character(Year),Difference_to_Baseline)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter(alpha = 0.3) + geom_hline(yintercept = -0.25, color = "red", linetype = 2) +
  geom_text(x = 1.5, y = -0.25, label = "-25%", vjust = 1, size = 5) + xlab("Year")

Unemployment_2023 %>% filter(Difference_to_Baseline <= -0.25) %>% select(Country_Name, Unemployment, Difference_to_Baseline)

Unemployment_2023 %>% filter(Difference_to_Baseline <= -0.25) %>% select(Country_Name) %>% inner_join(Unemployment, by = c("Country_Name")) %>% 
  ggplot(aes(Year,Unemployment,color = Country_Name)) + geom_point() + scale_color_brewer(palette = "Dark2") + facet_wrap(~Country_Name)

Unemployment_2023 %>% summarise(AU_Unemployment = mean(Unemployment), AU_Difference_to_Baseline = mean(Difference_to_Baseline))
