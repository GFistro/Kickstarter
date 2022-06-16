############### Kickstarter projects

############## Packages
library(tidyverse)
library(lubridate)
library(tidytext)
library(tm)
library(wordcloud2)
library(ROSE)
library(randomForest)
library(caret)
library(pROC)
library(ROCR)
library(gbm)
library(xgboost)
library(ggthemes)
library(mlr)
library(paletteer)



############# Data and cleaning
filepath <- "D:/Users/Gregor/Desktop/Kaggle/Kickstarter projects/ks-projects-201801.csv" 

kickstarter_data <- read_csv(filepath, col_names = T, cols(state = col_factor(), currency = col_factor(),
                                                           category = col_factor(), main_category = col_factor(),
                                                           country = col_factor())) #Reclassify suitable data as factors


glimpse(kickstarter_data) #Taking a look at our data
summary(kickstarter_data) # Distributions seem fine, some date irregularities
head(kickstarter_data)

table(year(kickstarter_data$launched), kickstarter_data$state) #We can see 7 examples of entries with date 1970, 6 of them cancelled, 1 suspended. Likely errors. Some of them are from 2018, most were still live at the time. We will omit those ##

kickstarter_clean <- kickstarter_data %>%
  filter(state %in% c("failed", "canceled", "successful", "suspended")) %>% #Omitting live and undefined projects
  mutate(state_bi = factor(ifelse(state == "successful", 1, 0))) %>% #Creating a factor -> 1 for a successful project, 0 otherwise
  mutate(launch_year = year(launched)) #We aren't interested in datetime in our case, just the year of the project

count(kickstarter_clean, state)
count(kickstarter_clean, state_bi) # We can see that around 36% registered projects were successful

glimpse(kickstarter_clean)

kickstarter_clean <- kickstarter_clean %>%
  select(- c(pledged, state, `usd pledged`, goal)) %>% #We remove redundant information -> we use usd_pledged_real (instead of pledged, usd pledged) and usd_goal_real instead of goal. State is also encoded in state_bi
  filter(!(launch_year %in% c(1970, 2018))) %>% #We remove the projects from 1970 and 2018
  group_by(main_category) %>%
  mutate(bi_cat = mean(as.numeric(state_bi) - 1)) %>%
  ungroup()

glimpse(kickstarter_clean)


nrow(kickstarter_data[kickstarter_data$usd_goal_real > 1000000, ]) #We see that we have 1085 projects with a funding goal of over a million dollars, which represents 0.3% of all projects.
kickstarter_data[kickstarter_data$usd_goal_real > 1000000 & kickstarter_data$state == "successful", ]$name #Out of the 1085 projects, only 11 were successfuly funded which represents 0.9% of all projects with funding goals over a million $


#------------------------------------------------------------------------------------------------

###################################### EXPLORATORY DATA ANALYSIS
  

#Which categories have the highest rate of successful projects

sapply(kickstarter_clean, function(x) sum(is.na(x))) #We take a look if there are any NAs in our data, find 4
which(is.na(kickstarter_clean$name)) 
kickstarter_clean[which(is.na(kickstarter_clean$name)), ] #We take a look at the 4 NAs, find nothing interesting so we remove them
kickstarter_clean <- kickstarter_clean[- which(is.na(kickstarter_clean$name)), ] #


#Project success rate by categories
kickstarter_clean %>%
  group_by(main_category) %>%
  summarise(success_per = mean(as.numeric(state_bi) - 1)) %>% #We summarise mean success rate by category to be visualised
  arrange(desc(success_per)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(main_category, - success_per), y = success_per)) +
  geom_col(fill = "#14505C") +
  ggtitle("Project Success Rate by Main Categories") +
  xlab(element_blank()) +
  ylab("Project Success Rate") +
  scale_y_continuous(labels = scales::percent, breaks = c(0.2, 0.4, 0.6)) +
  theme_fistro() +
  theme(axis.text.x = element_text(angle = 45, size = 10, vjust = 0.8),
        axis.title.y = element_text(vjust = 3),
        panel.grid.major.y = element_line())
  


#Which Categories have the Highest number of Backers

kickstarter_clean %>%
  group_by(main_category) %>%
  summarise(n_backers = sum(backers)/1000000) %>% #We divide the number of Backers by a million for clarity
  ungroup() %>%
  ggplot(aes(x = reorder(main_category, - n_backers), y = n_backers)) + 
  geom_col(fill = "#14505C") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), breaks = c(0.2, 1, 3, 5, 7, 11)) +
  ggtitle("Number of Backers per category") +
  theme_fistro()+
  ylab("Total number of Backers in Millions") +
  xlab(element_blank()) +
    theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 1, hjust = 1),
          axis.title.y = element_text(vjust = 3),
          panel.grid.major.y = element_line(),
          plot.title = element_text(hjust = 1, vjust = 1.5))



#Which categories attracted the most funding in total

kickstarter_clean %>%
  group_by(main_category) %>%
  summarise(total_pledged = sum(usd_pledged_real)/1000000) %>% #We divide by a million to get better readability
  ungroup() %>%
  ggplot(aes(x = reorder(main_category, - total_pledged), y = total_pledged)) +
  geom_bar(stat = "identity", fill = "#14505C") +
  scale_y_continuous(breaks = c(0, 50, 100, 200, 400, 600)) +
  xlab(element_blank()) +
  ylab("Ammount pledged in Million $") +
  ggtitle("Total Amount of $ pledged by Category") +
  theme_fistro() +
  theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 1, hjust = 0.8),
        axis.title.y = element_text(vjust = 3),
        panel.grid.major.y = element_line())


#Which Categories had the most of projects

kickstarter_clean %>%
  group_by(main_category) %>%
  summarise(n_proj = n()/1000) %>% #We transform to number of projects in thousands
  ggplot(aes(x = reorder(main_category, -n_proj), y = n_proj)) +
  geom_bar(stat = "identity", fill = "#14505C") +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  xlab(element_blank()) +
  ylab("Number of Projects (in thousands)") +
  ggtitle("Total Number of Projects Launched by Category") +
  theme_fistro() +
  theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 1, hjust = 0.8),
        axis.title.y = element_text(vjust = 3),
        panel.grid.major.y = element_line())



#The Distribution of funding goals between the categories and their average success rate

kickstarter_clean %>%
  ggplot(aes(x = main_category, y = usd_goal_real, fill = bi_cat * 100)) + #We transform the fill to %
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(0, 120000)) +
    scale_fill_paletteer_c("grDevices::BluGrn", direction = -1) +
    xlab(element_blank()) +
    ylab("Project funding goal ($)") +
    ggtitle("Funding Goal Distribution by Categories") +
    labs(fill = "Project Success \n      Rate in %") +
    theme_fistro() +
    theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 1, hjust = 0.8),
          axis.title.y = element_text(vjust = 3),
          panel.grid.major.y = element_line())
#Interestingly Food has high goals


#Comparison of the ammount of $ invested per project between categories

kickstarter_clean %>%
  ggplot(aes(x = main_category, y = usd_pledged_real, fill = bi_cat * 100)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 35000)) +
  scale_fill_paletteer_c("grDevices::BluGrn", direction = -1) +
  xlab(element_blank()) +
  ylab("Money Invested in $") +
  ggtitle("Average Amount of $ Invested in a Project by Categories") +
  labs(fill = "Project Success \n      Rate in %") +
  theme_fistro() +
  theme(axis.text.x = element_text(angle = 60, size = 10, vjust = 1, hjust = 0.8),
        axis.title.y = element_text(vjust = 3),
        panel.grid.major.y = element_line())



#Total Number of projects launched per year

kickstarter_clean %>%
  group_by(launch_year) %>%
  summarise(st_proj = n()) %>%
  ggplot(aes(x = launch_year, y = st_proj)) +
    geom_bar(stat = "identity", fill = "#14505C") +
    scale_x_continuous(breaks = seq(2009, 2017, 1)) +
    coord_cartesian(ylim = c(0, 90000)) +
    ylab("Number of Projects") +
    xlab("Launch Year") +
    ggtitle("Number of Projects Launched Between 2009 and 2017") +
    theme_fistro() +
    theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 0.8),
        axis.title.y = element_text(vjust = 3),
        panel.grid.major.y = element_line())
    


#Proportion of successful projects by year

kickstarter_clean %>%
  group_by(launch_year) %>%
  summarise(succ_rate = (mean(as.numeric(state_bi) - 1)) * 100)  %>%
  ungroup() %>%
  ggplot(aes(x = launch_year, y = succ_rate)) +
  geom_bar(stat = "identity", fill = "#14505C") +
  scale_x_continuous(breaks = seq(2009, 2017, 1)) +
  scale_y_continuous(breaks = c(0, seq(10, 50, 10))) +
  coord_cartesian(ylim = c(0, 55)) +
  ylab("Project Success rate in %") +
  xlab("Launch Year") +
  ggtitle("Overall Project Success Rate between 2009 and 2017") +
  theme_fistro() +
  theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 0.8),
        axis.title.y = element_text(vjust = 3),
        panel.grid.major.y = element_line())

#Total ammount of $ invested in all projects on kickstarter per year

kickstarter_clean %>%
  group_by(launch_year) %>%
  summarise(inv = sum(usd_pledged_real) / 1000000) %>% #Transform to millions of $ scale
  ungroup() %>%
  ggplot(aes(x = launch_year, y = inv)) +
  geom_bar(stat = "identity", fill = "#14505C") +
  scale_x_continuous(breaks = seq(2009, 2017, 1)) +
  coord_cartesian(ylim = c(0, 750)) +
  xlab("Lauch year") +
  ylab("Money invested (in million $)") +
  ggtitle("Total Amount of Money Invested in Projects \n on Kickstarter between 2009 and 2017") +
  theme_fistro() +
  theme(axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 0.8),
        axis.title.y = element_text(vjust = 3),
        panel.grid.major.y = element_line())


#################################### SENTIMENT ANALYSIS - AFINN in BING


#Sentiment analysis - does the sentiment value of the name of the project affect it's success - BINARY BING LEXICON

kickstarter_bing <- kickstarter_clean %>%
  unnest_tokens(word, name) %>%
  inner_join(get_sentiments("bing"), by = "word") #Preparing data for analysis

kickstarter_bing <- kickstarter_bing %>% 
  count(ID, sentiment) %>% #We split by ID and sentiment (on longer titles that have more relevant words)
  spread(sentiment, n) %>% #Spread the sentiment values to two columns (neg, pos)
  replace_na(list(negative = 0, positive = 0)) %>% #Replacing the resulting NAs
  mutate(ovrl_sen = positive - negative) %>% #Calculating the overall sentiment for every ID
  select(ID, ovrl_sen)


clean_bing <- inner_join(kickstarter_clean, kickstarter_bing, by = "ID") #Adding the sentiment values to the rest of the data

clean_bing %>%
  filter(ovrl_sen > -5 & ovrl_sen < 5) %>% #We filter for projects with sentiment values between -5 and 5, because there is only 1 project with a value of -6 and 8 projects with a value of 6
  group_by(ovrl_sen) %>%
  summarise(per = mean(as.numeric(state_bi) - 1), avg_fund = sum(usd_pledged_real / n()), nproj = n()) %>% #we calculate the average success rate of projects and the average ammount raised per project for every category
  ungroup() %>%
  ggplot(aes(x = ovrl_sen, y = per, fill = avg_fund)) + #We plot the proportion of successful projects and average ammount of $ funded per project by categories
    geom_col(position = "dodge2", alpha = 0.8) +
    geom_text(aes(label = paste0("* ", nproj), vjust = - 0.8)) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    scale_x_continuous(breaks = seq(-5, 5, by = 1)) +
    labs(title = "Proportion of Successful projects \n and Average Ammount of Money spent \n by BING Sentiment \n",
         x = "Overall BING Sentiment",
         y = "Proportion of Successful Projects in %",
         fill = "Average $ \n per project") +
    scale_fill_paletteer_c("grDevices::BluGrn", direction = -1) +
    geom_text(x = 2, y = 0.41, label = "* Number of projects", color = "#14505C") +
    theme_fistro() +
    theme(plot.title = element_text(vjust = 1),
          panel.grid.major.y = element_line(),
          axis.text.x = element_text(vjust = 5)) 

clean_bing %>%
  count(ovrl_sen) #We take a look at the distribution of different sentiment values in our database

summary(glm(state_bi ~ ovrl_sen, data = clean_bing, family = "binomial")) #We try a very basic logistic model to see if sentiment could potentially affect the probabilities of project success. The simple model is significant.
exp(confint(glm(state_bi ~ ovrl_sen, data = clean_bing, family = "binomial"))) #The simple model predicts an 8-10% reduction in project success with rising values of overall sentiment


#Sentiment analysis - does the sentiment value of the name of the project affect it's success - AFINN LEXICON

kickstarter_afinn <- kickstarter_clean %>%
  unnest_tokens(word, name) %>%
  inner_join(get_sentiments("afinn"), by = "word") #We prepare the data for analysis with an AFINN lexicon


#We calculate the sentiment value for every project
kickstarter_afinn <- kickstarter_afinn %>% 
  group_by(ID) %>%
  mutate(totsen = sum(value)) %>% #We calulate the overall sentiment value of a projecs with longer names by adding the sentiment scores of the included words together
  select(ID, totsen)

count(kickstarter_afinn, ID) %>% arrange(desc(n)) #We take a quick look to see how many sentiment sensitive words do the titles with the most of such words have
filter(kickstarter_afinn, ID == 1219933968) #We take a look at an example, seenig an overall sentiment value of -2 in this example
filter(kickstarter_clean, ID == 1219933968)$name #We take a look at the project's name

clean_afinn <- inner_join(kickstarter_clean, unique(kickstarter_afinn), by = "ID") #We join the AFINN sentiment values with the original dataset, using only unique values to avoid duplication
print(count(clean_afinn, totsen), n=30) #A brief overview of the range of our calculated sentiment values

clean_afinn %>%
  group_by(totsen) %>%
  summarise(per = mean(as.numeric(state_bi) - 1), cash = sum(usd_pledged_real / n()), nproj = n()) %>%
  filter(between(totsen, -8, 10)) %>% #We filter out instances of sentiment values that have fewer than 20 samples
  ungroup() %>%
  ggplot(aes(x = totsen, y = per, fill = cash)) +
  geom_col(position = "dodge2", width = 1, alpha = 0.8) +
  geom_text(aes(label = paste0("*", nproj), vjust = - 1), size = 2.2) +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.1, 0.2, 0.3, 0.35)) +
  scale_x_continuous(breaks = seq(-8, 10, by = 1)) +
  scale_fill_paletteer_c("grDevices::BluGrn", direction = -1) +
  geom_text(x = 8, y = 0.37, label = "* Number of Projects", size = 3, color = "#14505C") +
  theme_fistro() +
  labs(title = "Rate of Funding by AFINN Sentiment Value",
       x = "AFINN Sentiment Value",
       y = "Proportion of Successfuly Funded Projects (%)",
       fill = "Average Ammount \n of $ per Project",) +
  theme(plot.title = element_text(vjust = 1.4),
        panel.grid.major.y = element_line(),
        axis.text.x = element_text(vjust = 8))
#Any effects are much less pronounced

summary(glm(state_bi ~ totsen, data = clean_afinn, family = "binomial")) #We again try a very simple model, and discover that the sentiment values calculated with AFINN also appear to signifcantly affect project success rate
exp(confint(glm(state_bi ~ totsen, data = clean_afinn, family = "binomial"))) #This time the effect seems to be significantly lower (perhaps because of more bins in case of AFINN) and represents an decrease of about ~ 1 do 2%


#Extreme sentiment value examples
clean_bing %>%
  filter(ovrl_sen <= -4) %>% #We filter for the entries with the lowest sentiment scores from BING Lexicon analysis
  select(name, ovrl_sen) %>% 
  arrange(ovrl_sen) %>%
  print(n = 20)

clean_bing %>%
  filter(ovrl_sen >= 4) %>% #We filter for the entries with the highest sentiment scores from BING Lexicon analysis
  select(name, ovrl_sen) %>%
  arrange(desc(ovrl_sen)) %>%
  print(n = 20)

clean_afinn %>%
  filter(totsen < -8) %>% #We filter for the entries with the lowest sentiment scores from AFINN Lexicon analysis
  select(name, totsen) %>%
  arrange(totsen) %>%
  print(n = 20)

clean_afinn %>%
  filter(totsen > 11) %>% #We filter for the entries with the highest sentiment scores from AFINN Lexicon analysis
  select(name, totsen) %>%
  arrange(desc(totsen)) %>%
  print(n = 20)


#Wordclouds
set.seed(2000)

tekst <- kickstarter_data$name
kickstarter_corpus <- Corpus(VectorSource(tekst[sample(length(tekst), 1000)])) #We downsample the Corpus so that we get a reasonable output

clean_corpus <- kickstarter_corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>% #We do the standard cleaning procedure, removing numbers, punctuation and whitespace
  tm_map(stripWhitespace)

clean_corpus <- tm_map(clean_corpus, content_transformer(tolower))
clean_corpus <- tm_map(clean_corpus, removeWords, c(stopwords("english"), "canceled", "film", "book", "project", "game", "new", "album", "music", "art")) #We remove the most common generic words

dtm <- TermDocumentMatrix(clean_corpus) #Transforming the data to a document matrix form
dtm_matrix <- as.matrix(dtm)
words <- sort(rowSums(dtm_matrix), decreasing = T)
wordcloud_data <- data.frame(word = names(words), freq = words)

gbl_palette <- paletteer_c("grDevices::BluGrn", n = 50) #We use the custom palette we used before
gbl_palette2 <- c(gbl_palette, rep(gbl_palette[50], 350)) #We edit the palette so that we still get the color differentiation but recycle the lightest colour for the words with the lowest occurences
wordcloud2(data = wordcloud_data, color = gbl_palette2)
#We see mostly words such as first, world, life, help, mostly positive words


# We do a bit of cleanup
rm(tekst, words)
rm(dtm, dtm_matrix)
rm(wordcloud_data, clean_corpus, kickstarter_corpus)
rm(kickstarter_afinn, kickstarter_bing)


#############################################################################
#Given the less pronounced effect on AFINN, we decide to use the simpler, BING sentimental analysis data

nrow(clean_bing)#There is 109378 observations in the dataset that has sentiment values added to it
sum(clean_bing["state_bi"] == 0) #Of these 69312 were unsuccessful in funding
sum(clean_bing["state_bi"] == 1) #40066 projects were successfuly funded
40066/109378 #We see that ~ 36,6% of all projects were successful, suggesting that we need to balance our classess before analysis

nrow(clean_afinn)
sum(clean_afinn["state_bi"] == 0) 
sum(clean_afinn["state_bi"] == 1)
36005/102466 #Doing the same analysis on the AFINN dataset it returns ~ 35% project success rate

nrow(kickstarter_clean)
sum(kickstarter_clean["state_bi"] == 0) 
sum(kickstarter_clean["state_bi"] == 1)
133956/372287 #Doing the same analysis on the original dataset it returns ~ 36% project success rate, suggesting that our BING and AFINN samples are representative


clean_bing <- clean_bing %>%
  mutate(launched = as_date(launched)) %>% #We simplify the POSIXct data to date format, because the accuracy of date is enough
  mutate(duration = as.numeric(deadline - launched)) #We calculate the duration of the project

set.seed(2002)

split_rows <- sample(nrow(clean_bing), nrow(clean_bing) / 2) #We make and index for a 50/50 data split
train_data <- clean_bing[split_rows, ] #We create a training dataset
test_data <- clean_bing[-split_rows, ] #We create a testing dataset

table(train_data$state_bi) #Once again, we see that the data on the training set is quite unbalanced
over_rf <- ovun.sample(state_bi ~ ., data = train_data, method = "over") #We use oversampling to balance the dataset
table(over_rf$data$state_bi) #We see that the classes are now balanced 
over_data <- over_rf$data #We assign our data to a new dataset to use later


clean_over <- over_data %>%
  select(-c("ID", "name", "deadline", "backers", "usd_pledged_real", "bi_cat", "launched")) #We remove data we can't model with (ID, name), redundant data (deadline, launched, bi_cat) and data that can directly describe the outcome of the project (backers, usd_pledged_real)

clean_over <- clean_over[sample(nrow(clean_over)), ] #We shuffle the data, because they were split by state_bi variable from ovun.sample

rm(clean_afinn) #We do some more cleaning



################################### MODELLING ######################

kick_rf_model <- randomForest(state_bi ~ ., data = clean_over[, -1]) #We remove our category variable, as it has more than 53 categories and random forest isn't capable of handling it
print(kick_rf_model)
plot(kick_rf_model) #We see that the model stabilizes after about 100 trees
importance(kick_rf_model) #Measuring importance, the most important variables are usd_goal_real, duration and main_category. 


kick_rf_probs <- predict(kick_rf_model, newdata = test_data, type = "response") #We predict on test data using our model
kick_rf_probs_prob <- predict(kick_rf_model, newdata = test_data, type = "prob")
mean(kick_rf_probs == test_data$state_bi) #We get a prediction accuracy od 0.647
mean((kick_rf_probs_prob[, 2] - (as.numeric(test_data$state_bi) - 1))^2) #It gives a Brier score of 0.234, which is pretty bad


################################################################################## USING BRIER INSTEAD OF ACCURACY

clean_training_original <- train_data %>%
  select(-c("ID", "name", "deadline", "backers", "usd_pledged_real", "bi_cat", "launched")) #This time using the unbalanced dataset for our training subset

clean_test_original <- test_data %>%
  select(-c("ID", "name", "deadline", "backers", "usd_pledged_real", "bi_cat", "launched"))

kick_rf_model_bri <- randomForest(state_bi ~ ., data = clean_training_original[, -1])
print(kick_rf_model_bri) #Error rate is bigger in unbalanced dataset, as expected
plot(kick_rf_model_bri)
importance(kick_rf_model_bri) #We see a very similar situation with variable importance

kick_rf_probs_bri <- predict(kick_rf_model_bri, newdata = test_data, type = "response")
kick_rf_probs_bri_prob <- predict(kick_rf_model_bri, newdata = test_data, type = "prob")

mean(kick_rf_probs_bri == test_data$state_bi) #Gives a higher accuracy than the balanced dataset (due to imbalances and accuracy not being suitable)
mean((kick_rf_probs_bri_prob[, 2] - (as.numeric(test_data$state_bi) - 1))^2) #It gives a slightly lower Brier score of 0.223 suggesting the "unbalanced" dataset is better, but still a bad Brier score overall




###############################################################################################################


cmat <- confusionMatrix(data = kick_rf_probs, reference = test_data$state_bi, positive = "1")
print(cmat)

cmato <- confusionMatrix(data = kick_rf_probs_bri, reference = test_data$state_bi, positive = "1")
print(cmato) #Compared to the balanced class we see an expeceted increased discrepancy between senistivity and specificity

auc(test_data$state_bi, as.numeric(kick_rf_probs) - 1) #The RandomForest model on a balanced dataset gives us an AUC value of 0.636
auc(test_data$state_bi, as.numeric(kick_rf_probs_bri) - 1) #The AUC of the model without balanced classes is lower at 0.611


#Using ROCR to tune the model
rocr_prediction <- ROCR::prediction(kick_rf_probs_prob[, 2], test_data$state_bi) #Probamo prav naredit
rocr_prediction_auc_optimized <- ROCR::performance(rocr_prediction, "auc") #We adjust the model parameters to optimize for AUC
(rocr_auc <- as.numeric(rocr_prediction_auc_optimized@y.values)) #We see it is possible to reach an AUC value of 0.686 with optimization

rocr_cutoff <- ROCR::performance(rocr_prediction, "sens", "spec") #We use ROCR again to try to find an optimal cutoff value, maximizing sensitivity and specificity
plot(rocr_cutoff) #Ideally we would like to see a much squarer line, suggesting a stronger model, but the data obviously has limitations. The object also allows us to find a good cutoff value
opt_cutoff <- rocr_cutoff@alpha.values[[1]][which.max(rocr_cutoff@x.values[[1]] + rocr_cutoff@y.values[[1]])] #The optimal cutoff using our metrics is at 0.424
opt_cutoff

rocr_preds <- ifelse(kick_rf_probs_prob[, 2] > opt_cutoff, 1, 0) #We classify predictions according to our optimal cutoff
confusionMatrix(data = factor(rocr_preds), reference = test_data$state_bi, positive = "1") #We see a higher balanced accuracy compared to our two previous predicitons
auc(test_data$state_bi, rocr_preds) #We achieve the highest AUC so far with 0.6405





##### GBM model
over_data_gbm <- clean_over
over_data_gbm$state_bi <- as.numeric(over_data_gbm$state_bi) - 1

kick_gbm <- gbm(state_bi ~ ., data = over_data_gbm, distribution = "bernoulli", n.trees = 10000, cv.folds = 5, shrinkage = 0.001) #Naredimo random forrest z boostingom
summary(kick_gbm)
gbm_predict <- predict(kick_gbm, newdata = test_data, type = "response", n.trees = 10000)
gbm_preds <- ifelse(gbm_predict > 0.5, 1, 0)
table(gbm_preds)
gbm_preds_co <- ifelse(gbm_predict > 0.45, 1, 0)

auc(test_data$state_bi, gbm_preds) #GBM da AUC 0.651
auc(test_data$state_bi, gbm_preds_co) #Na custom cutoffu iz random forrest je AUC nekoliko slabsi, 0.646


#xgboost glede na blog na kagglu https://www.kaggle.com/rtatman/machine-learning-with-xgboost-in-r

xgtrain_comp <- clean_over %>%
  select(-c(state_bi, main_category, category, currency, country)) #Odstranimo vse non-numeric variable

head(clean_over$main_category) #Vidimo, da ni numeric valda

xgtrain_comp_lab <- clean_over$state_bi #shranimo labele loceno, da ne vplivajo na trening modela

categories <- model.matrix(~main_category + category + currency + country -1, clean_over) #pretvorimo kategoricne informacije v numericni format (one-hot encoding)

xgtrain_comp_num <- cbind(xgtrain_comp, categories) #Zdruzimo oba dataseta v dataframe
xgtrain_comp_mat <- data.matrix(xgtrain_comp_num) #Pretvorimo ta dataframe v matrix

xgsplit <- round(length(xgtrain_comp_lab) * 0.7) #Pripravis za 70/30 split
xg_train_dat <- xgtrain_comp_mat[1:xgsplit, ]
xg_train_lab <- xgtrain_comp_lab[1:xgsplit]

xg_test_dat <- xgtrain_comp_mat[-(1:xgsplit), ]
xg_test_lab <- xgtrain_comp_lab[-(1:xgsplit)]

xgtrain <- xgb.DMatrix(data = xg_train_dat, label = as.numeric(as.character(xg_train_lab)))
xgtest <- xgb.DMatrix(data = xg_test_dat, label = as.numeric(as.character(xg_test_lab))) #Naredis xgb.DMatrix file formata, ki sta bolj optimizirana za training

negative_cases <- sum(xg_train_lab == 0)
positive_cases <- sum(xg_train_lab == 1)


xg_bal_model_gam <- xgboost(xgtrain,
                        max.depth = 4,
                        nround = 5000,
                        early_stopping_rounds = 5,
                        objective = "binary:logistic",
                        gamma = 0.8)

print(xg_bal_model_gam)
xg_bal_gam_preds <- predict(xg_bal_model_gam, xgtest)
xg_bal_gam_err <- mean(as.numeric(xg_bal_gam_preds > 0.5) != xg_test_lab)

print(xg_bal_gam_err) #Dobro, da je manjsi error na testing data, kot na training - ni overfittanja
table(predictions = xg_bal_gam_preds>0.5, reality = xg_test_lab)

xgb.plot.multi.trees(feature_names = names(xgtrain_comp_mat), model = xg_bal_model_gam)

xg_bal_gam_preds_res <- ifelse(xg_bal_gam_preds > 0.5, 1, 0)
auc(xg_test_lab, xg_bal_gam_preds_res) #XGBoost da AUC 0.683




##### TUNING

xg_bal_model_gam_tun <- xgboost(xgtrain,
                            max.depth = 4,
                            nround = 5000,
                            early_stopping_rounds = 5,
                            objective = "binary:logistic",
                            gamma = 1.2,
                            min_child_weight = 4,
                            subsample = 0.8,
                            colsample_bytree = 0.74,
                            scale_pos_weight = negative_cases/positive_cases)
print(xg_bal_model_gam_tun)

xg_bal_gam_preds_tun <- predict(xg_bal_model_gam_tun, xgtest)
xg_bal_gam_err_tun <- mean(as.numeric(xg_bal_gam_preds_tun > 0.5) != xg_test_lab)
print(xg_bal_gam_err_tun) #Dobro, da je manjsi error na testing data, kot na training - ni overfittanja
table(predictions = xg_bal_gam_preds_tun>0.5, reality = xg_test_lab)

xg_bal_gam_preds_res_tun <- ifelse(xg_bal_gam_preds_tun > 0.5, 1, 0)

auc(xg_test_lab, xg_bal_gam_preds_res_tun) #S tunanim modelom dobimo AUC 0.71

xg_bal_model_gam_tun$params






##### TUNING Z MLR
#over_da je train data, test_data test data

# <- colnames(over_data)
#for(i in fact_col) set(over_data,j=i,value = factor(over_data[[i]]))
#for (i in fact_col) set(test_data,j=i,value = factor(test_data[[i]])) Mogoce ni treba, ker nimamo stringov
library(mlr3)
mlrover <- clean_over[sample(nrow(clean_over), (nrow(clean_over)/4)), ]
mlrtest <- clean_over[sample(nrow(clean_over), (nrow(clean_over)/4)), ]


traintask <- tsk(data = mlrover, target = "state_bi")
testtask <- tsk(data = mlrtest, target = "state_bi")

traintask <- createDummyFeatures(obj = traintask)
testtask <- createDummyFeatures(obj = testtask)


lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list(objective="binary:logistic", eval_metric="error", nrounds=100L, eta=0.1)
params <- makeParamSet(makeDiscreteParam("booster", values = "gbtree"), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
                        makeNumericParam("gamma", lower = 0, upper = 2))
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
ctrl <- makeTuneControlRandom(maxit = 30L)

mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)
mytune$y
#Result: booster=gbtree; max_depth=10; min_child_weight=2.53; subsample=0.617; colsample_bytree=0.78; gamma=1.62 : acc.test.mean=0.6700636
lrn_tune <- setHyperPars(lrn, par.vals = mytune$x)
lrnxgmodel <- train(learner = lrn_tune, task = traintask)




lrnxgpred <- predictLearner(lrnxgmodel, testtask)
confusionMatrix(lrnxgpred$data$response, lrnxgpred$data$truth)


#Model uspešnosti fundinga - logistic model


logi_mod <- glm(state_bi ~ ., data = clean_over, family = binomial)
summary(logi_mod)
logi_summary <- summary(logi_mod)


logipreds <- predict(logi_mod, newdata = test_data, type = "response")
logipreds2 <- ifelse(logipreds > 0.5, 1, 0)


table(predictions = logipreds2, truth = test_data$state_bi)
mean(logipreds2 == test_data$state_bi)

auc(test_data$state_bi, logipreds2) #Da AUC 0.64




#Primerjava auc vseh modelov
koncni_table <- data.frame(auc(test_data$state_bi, as.numeric(logipreds2)), #Navadni glm da AUC 0.6426
                  auc(test_data$state_bi, as.numeric(kick_rf_probs) - 1), #Navadni RF, AUC 0.6387
                  auc(test_data$state_bi, gbm_preds), #GBM da AUC 0.651
                  auc(xg_test_lab, xg_bal_gam_preds_res), #XGBoost da AUC 0.683
                  auc(xg_test_lab, xg_bal_gam_preds_res_tun)) #Iz random learnerja vstavljeni parametri v xgboost, da AUC 0.708
colnames(koncni_table) <- c("Logistic Regression", "Random Forest", "GBM", "XGBoost", "XGBoost Tuned")
rownames(koncni_table) <- "AUC Values"
koncni_table
