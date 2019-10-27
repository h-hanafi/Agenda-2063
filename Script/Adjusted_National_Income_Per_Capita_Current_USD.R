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
?download.file
#Loading Adujusted net national income per Capita
Raw_Data_Folder <- file.path(getwd(),"Raw_Data")
dir.create(Raw_Data_Folder)
download.file("http://api.worldbank.org/v2/en/indicator/NY.ADJ.NNTY.PC.CD?downloadformat=csv", file.path(Raw_Data_Folder,"API_NY.ADJ.NNTY.PC.CD_DS2_en_csv_v2_386426.zip"), mode = "wb")
IPC_zip <- "API_NY.ADJ.NNTY.PC.CD_DS2_en_csv_v2_386426.zip"
IPC_csv <- "API_NY.ADJ.NNTY.PC.CD_DS2_en_csv_v2_386426.csv"
unzip(file.path(Raw_Data_Folder,IPC_zip), exdir = file.path(Raw_Data_Folder,"IPC"))

#Reading the File into R
IPC <- read_csv(file.path(Raw_Data_Folder,"IPC",IPC_csv), skip = 3)

####################################################
#Wrangling Adjusted net national incompe per capita
####################################################

#Tidying the Data
colnames(IPC) <- str_replace(colnames(IPC), " ", "_") 
IPC <- IPC %>%filter(Country_Name %in% AU) %>% gather(key = "Year", value = "IPC", `1960`:`2018`, convert = TRUE) %>% 
  select(-c(X64,Indicator_Code)) %>% filter(Country_Name %in% AU) %>% select(-Country_Code)

#Viewing The Datas
IPC %>% 
  ggplot(aes(Year,Country_Name, fill = IPC)) + geom_tile(color = "black") + 
  scale_fill_distiller(palette = "Blues", na.value = "grey50", direction = 1) + 
  scale_x_continuous(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) + 
  theme(axis.text.y = element_text(hjust = 1))

#Removing years and countries with no data
No_Data <- IPC %>%
  group_by(Year) %>% summarize(No_Data = sum(!is.na(IPC))) 
IPC <- IPC %>% left_join(No_Data, by = "Year") %>% filter(No_Data != 0) %>% select(-No_Data) 

No_Data <- IPC %>% group_by(Country_Name) %>% summarize(No_Data = sum(!is.na(IPC))) 
IPC <- IPC %>% left_join(No_Data, by = "Country_Name") %>% filter(No_Data != 0) %>% select(-No_Data) 

#Setting Baseline and Test Sets
#Baseline
IPC %>% group_by(Country_Name) %>% filter(Year == c(2013)) %>% 
  summarize(Baseline = IPC) %>% filter(is.na(Baseline))

IPC %>% filter(Country_Name == "Eritrea" & !is.na(IPC))

Eritrea_2013 <- IPC %>% filter(Country_Name == "Eritrea", Year == 2011) %>% mutate(Baseline = IPC) %>% select(Country_Name, Baseline)

Baseline_IPC <- IPC %>% group_by(Country_Name) %>% filter(Year == (2013)) %>% 
  summarize(Baseline = IPC) %>% filter(!is.na(Baseline)) %>% rbind(Eritrea_2013)

#Tests Set
IPC %>% group_by(Country_Name) %>% filter(Year == c(2017)) %>% 
  summarize(Baseline = IPC) %>% filter(is.na(Baseline))

Eritrea_2017 <- IPC %>% filter(Country_Name == "Eritrea", Year == 2011) %>% mutate(Year = 2017) 

IPC_test <- IPC %>% filter(Year == 2017, Country_Name != "Eritrea") %>% rbind(Eritrea_2017)

#Removing Test Data
IPC <- IPC %>% filter(Year != 2017) 

#Removing NA's
IPC <- IPC %>% filter(!is.na(IPC))

#Data Analysis
#Year Range
IPC %>% pull(Year) %>% max()
IPC %>% pull(Year) %>% min()

#Continent Wide Boxplot
IPC %>% ggplot(aes(as.character(Year),IPC)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Regularized Boxpolt
IPC %>% group_by(Year) %>% mutate(IPC_R = IPC - mean(IPC)) %>% group_by(Country_Name) %>% mutate(IPC_mean = IPC_R - mean(IPC)) %>% 
  ggplot(aes(as.character(Year),IPC_R)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

IPC %>% 
  ggplot(aes(Year,Country_Name, fill = IPC)) + geom_tile(color = "black") + 
  scale_fill_distiller(palette = "Blues", na.value = "black", direction = 1) + scale_x_continuous(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) +
  theme(panel.background = element_rect(fill = "grey50", color = "black"), panel.grid = element_line(color = "grey50"))

IPC %>% group_by(Year) %>% summarize(Africa_IPC = mean(IPC)) %>% ggplot(aes(Year,Africa_IPC)) + geom_point() + geom_smooth(span = 0.5)

IPC %>% left_join(Baseline_IPC) %>% group_by(Year) %>% summarize(Africa_IPC = mean(IPC), Baseline = mean(Baseline)) %>% mutate(Change = (Africa_IPC - Baseline)/Baseline) %>%
  ggplot(aes(Year,Change)) + geom_point() + geom_vline(xintercept = 2013) + geom_hline(yintercept = 0.3) + 
  geom_text(data = data_frame(x = c(2013,2000), y = c(-0.5,0.35), label = c("2013", "30%"), angle = c(90,0)), aes(x = x, y = y, label = label, angle = angle, vjust = 1))






# model
fit_lm <- train(IPC ~ Country_Name + Year, data = IPC, method = "lm")
pred_lm <- predict(fit_lm, newdata = IPC_test)
RMSE_lm <- sqrt(mean((pred_lm - IPC_test$IPC)^2))

tune = data.frame(k = 1:5)
fit_knn <- train(IPC ~ Country_Name + Year, data = IPC, method = "knn", tuneGrid = tune)
plot(fit_knn)
pred_knn <- predict(fit_knn, newdata = IPC_test)
RMSE_knn <- sqrt(mean((pred_knn - IPC_test$IPC)^2))

tune = data.frame(cp = seq(0,0.5,0.05))
fit_rpart <- train(IPC ~ Country_Name + Year, data = IPC, method = "rpart", tuneGrid = tune)
plot(fit_rpart)
pred_rpart <- predict(fit_rpart, newdata = IPC_test)
RMSE_rpart <- sqrt(mean((pred_rpart - IPC_test$IPC)^2))

tune = data.frame(mtry = seq(30,60,10))
fit_rf <- train(IPC ~ Country_Name + Year, data = IPC, method = "rf", tuneGrid = tune)
plot(fit_rf)
pred_rf <- predict(fit_rf, newdata = IPC_test)
RMSE_rf <- sqrt(mean((pred_rf - IPC_test$IPC)^2))

Model_results <- data_frame(method = c("lm","knn","rpart","rf"), RMSE = c(RMSE_lm,RMSE_knn,RMSE_rpart,RMSE_rf))


?geom_boxplot


#2023 goal
IPC_2023 <- IPC_test %>% mutate(Year = 2023) %>% select(-IPC)

IPC_2023 <- IPC_2023 %>% mutate(IPC = pred_rf)

IPC_2023 <- IPC_2023 %>% left_join(Baseline_IPC, by = "Country_Name") %>% mutate(Change = (IPC - Baseline)/Baseline)
IPC_2023 %>% ggplot(aes(Country_Name,Change)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0)) + geom_hline(yintercept = 0.3, color = "red")
IPC_2023 %>% ggplot(aes(as.character(Year),Change)) + geom_boxplot(outlier.shape = NA) + geom_jitter(aes(color = Country_Name)) + geom_hline(yintercept = 0.3, color = "red")
IPC_2023 %>% filter(Change >= 0.3)

IPC %>% filter(Country_Name %in% c("Burundi","Congo, Dem. Rep.", "Congo, Rep.", "Ethiopia", "Sudan", "Eritrea")) %>% 
  ggplot(aes(Year, IPC, color = Country_Name)) + geom_point()

IPC_2023 %>% summarise(mean(IPC), mean(Change))


