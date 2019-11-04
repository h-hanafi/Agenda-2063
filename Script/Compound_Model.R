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

Results_Model_GDP_Growth <- data_frame(method = c("lm","knn","xgb","rf"), RMSE = c(RMSE_lm,RMSE_knn,RMSE_xgb,RMSE_rf))

Results_GDP_Growth %>% full_join(Results_Model_GDP_Growth, by = "method",suffix = c("_Original","_Compounded"))

fit_Model_GDP_Growth <- list(lm = fit_lm_Model_GDP_Growth, knn = fit_knn_Model_GDP_Growth, xgb = fit_xgb_Model_GDP_Growth, rf = fit_rf_Model_GDP_Growth)

#2023 GDP Growth prediction
Best_fit_Model_GDP_Growth <- fit_Model_GDP_Growth[[Results_Model_GDP_Growth$method[which.min(Results_Model_GDP_Growth$RMSE)]]]
Best_fit_Model_GDP_Growth$method

Model_GDP_Growth_2023 <- Model_test %>% mutate(Year = 2023) %>% select(Country_Name,Year)

Model_GDP_Growth_2023 <- Model_GDP_Growth_2023 %>% 
  mutate(Unemployment = predict(Best_fit_Unemployment, newdata = .)) %>%
  mutate(GDP_Growth = predict(Best_fit_Model_GDP_Growth, newdata = .))

mu_Model_GDP_Growth_2023 <- Model_GDP_Growth_2023 %>% summarise(AU_GDP_Growth = mean(GDP_Growth)) %>% pull()

Model_GDP_Growth_2023 %>%
  ggplot(aes(GDP_Growth)) +
  geom_histogram(bins = 25, color = "black") + 
  geom_vline(xintercept = mu_GDP_Growth_2023, color = "blue", linetype = 2) +
  scale_x_continuous(breaks = c(mu_GDP_Growth_2023,7,seq(-10,20,5))) +
  scale_y_continuous(expand = c(0,0))

Model_GDP_Growth_2023 %>%
  ggplot(aes(as.character(Year),GDP_Growth)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(alpha = 0.3) + 
  geom_hline(yintercept = c(7,mu_Model_GDP_Growth_2023), color = c("red","blue"), linetype = 2) +
  geom_text(data = data.frame(label = c("7%",signif(mu_Model_GDP_Growth_2023,digits =3)), x = c(1.5,1.5), y= c(7,mu_Model_GDP_Growth_2023)),
            aes(label = label, x = x, y = y,vjust = 1)) +
  ylab("Annual GDP Growth") +
  xlab("Year")  

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

