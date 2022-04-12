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
library(RColorBrewer)



############# Data and cleaning
filepath <- "D:/Users/Gregor/Desktop/Kaggle/Kickstarter projects/ks-projects-201801.csv"

kickstarter_data <- read_csv(filepath, col_names = T, cols(state = col_factor(), currency = col_factor(),
                                                           category = col_factor(), main_category = col_factor(),
                                                           country = col_factor()))


glimpse(kickstarter_data)
summary(kickstarter_data)
head(kickstarter_data)

table(year(kickstarter_data$launched), kickstarter_data$state) #Vidimo, da je par iz 1970, napake, iz 2018 pa so vsi se live

kickstarter_clean <- kickstarter_data %>%
  filter(state %in% c("failed", "canceled", "successful", "suspended")) %>%
  mutate(state_bi = factor(ifelse(state == "successful", 1, 0))) %>%
  mutate(launch_year = year(launched))

count(kickstarter_clean, state)
count(kickstarter_clean, state_bi)

glimpse(kickstarter_clean)
table(year(kickstarter_data$launched))

kickstarter_clean <- kickstarter_clean %>%
  select(- c(pledged, state, `usd pledged`, goal)) %>%
  filter(!(launch_year %in% c(1970, 2018))) %>%
  group_by(main_category) %>%
  mutate(bi_cat = mean(as.numeric(state_bi) - 1)) %>%
  ungroup()

glimpse(kickstarter_clean)


kickstarter_data[kickstarter_data$usd_goal_real > 1000000 & kickstarter_data$state == "successful", ]$name #vidimo, da je samo 11 projektov uspesno izvedlo pridobivanje vec kot 1.000.000 USD sredstev, kar predstavlja 0.9% vseh nad 1000000


#------------------------------------------------------------------------------------------------

###################################### EXPLORATORY DATA ANALYSIS
  

#Katere kategorije imajo veèji delež uspešnih projektov

sapply(kickstarter_clean, function(x) sum(is.na(x))) #Pogledamo, ce kje in koliko je NA-jev
which(is.na(kickstarter_clean$name)) #Tiste 4, ki so NA poiscemo
kickstarter_clean[which(is.na(kickstarter_clean$name)), ] #jih pregledamo
kickstarter_clean <- kickstarter_clean[- which(is.na(kickstarter_clean$name)), ] #Odstranimo NA-je


kickstarter_clean %>%
  group_by(main_category) %>%
  summarise(success_per = mean(as.numeric(state_bi) - 1)) %>%
  arrange(desc(success_per)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(main_category, - success_per), y = success_per)) +
  geom_col(fill = "darkslategrey") +
  ggtitle("% Of Successfully funded projects") +
  xlab(element_blank()) +
  ylab("Share of successful projects") +
  theme_fistro() +
  theme(axis.text.x = element_text(angle = 45, size = 10, vjust = 0.8),
        axis.title.y = element_text(vjust = 3))
  


#Katere kategorije imajo najveèji delež uspešnih projektov pobarvano s številom backerjev

kickstarter_clean %>%
  group_by(main_category) %>%
  summarise(success_per = mean(as.numeric(state_bi) - 1), n_backers = sum(backers)) %>%
  arrange(desc(success_per)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(main_category, - success_per), y = success_per, fill = n_backers)) +
  scale_fill_gradient(
    name = "Number of Backers",
    low = "#2F4F4F",
    high = "#ADADAD") +
  geom_col() +
  ggtitle("Number of Backers per category") +
  xlab(element_blank()) +
  ylab("Share of successful projects") +
  theme_fistro() +
  theme(axis.text.x = element_text(angle = 45, size = 10, vjust = 0.8),
        axis.title.y = element_text(vjust = 3),
        plot.title = element_text(hjust = 1))


#V katere kategorije se je steklo najveè denarja?

kickstarter_clean %>%
  group_by(main_category) %>%
  summarise(total_pledged = sum(usd_pledged_real)) %>%
  arrange(desc(total_pledged)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(main_category, - total_pledged), y = total_pledged)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))


#Katere kategorije so bolj pogoste? (Število projektov)

kickstarter_clean %>%
  group_by(main_category) %>%
  summarise(n_proj = n()) %>%
  ggplot(aes(x = reorder(main_category, -n_proj), y = n_proj)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))



#Kako so kategorije homogene? Glede na zahtevan znesek

kickstarter_clean %>%
  ggplot(aes(x = main_category, y = usd_goal_real, fill = bi_cat)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0, 100000)) +
    theme(axis.text.x = element_text(angle = 45))


#Kako so kategorije homogene glede na vložen denar

kickstarter_clean %>%
  ggplot(aes(x = main_category, y = usd_pledged_real, fill = bi_cat)) +
  geom_violin() +
  coord_cartesian(ylim = c(0, 75000)) +
  theme(axis.text.x = element_text(angle = 45))


#Število projektov po letih

kickstarter_clean %>%
  group_by(launch_year) %>%
  summarise(st_proj = n()) %>%
  ggplot(aes(x = launch_year, y = st_proj)) +
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = seq(2009, 2017, 1))


#Uspešen % fundinga po letih

kickstarter_clean %>%
  group_by(launch_year) %>%
  summarise(succ_rate = mean(as.numeric(state_bi) - 1)) %>%
  ungroup() %>%
  ggplot(aes(x = launch_year, y = succ_rate)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(2009, 2017, 1))

#Kolièina investiranega $ po letih

kickstarter_clean %>%
  group_by(launch_year) %>%
  summarise(inv = sum(usd_pledged_real)) %>%
  ungroup() %>%
  ggplot(aes(x = launch_year, y = inv)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(2009, 2017, 1))


#Vpliva dolžina projekta na njegovo uspešnost?



kickstarter_clean_sample <- sample_n(kickstarter_clean, 1000)

kickstarter_clean_sample <- kickstarter_clean_sample %>%
  mutate(duration = deadline - as.Date(launched))


ggplot(kickstarter_clean_sample, aes(x = state_bi, y = duration, group = state_bi)) +
  geom_boxplot(alpha = 0.3) +
  geom_jitter(alpha = 0.8, color = "peachpuff4") #Tam duration okoli 60 izgleda, da niso prevec uspesni

duration_model <- glm(state_bi ~ duration, data = kickstarter_clean_sample, family = "binomial")
summary(duration_model) ############# Tu je potrebno preveriti, ce je treba/lahko pretvoriti nazaj iz log oddsov v verjetnost zmanjsanja


#################################### SENTIMENT ANALYSIS - AFINN in BING


#Analiza sentimenta - ali so projekti s pozitivnim sentimentom bolj uspešni % - BINARY BING LEXICON

kickstarter_bing <- kickstarter_clean %>%
  unnest_tokens(word, name) %>%
  inner_join(get_sentiments("bing"), by = "word")

kickstarter_bing <- kickstarter_bing %>% 
  count(ID, sentiment) %>%
  spread(sentiment, n) %>%
  replace_na(list(negative = 0, positive = 0)) %>%
  mutate(ovrl_sen = positive - negative) %>%
  select(ID, ovrl_sen)


clean_bing <- inner_join(kickstarter_clean, kickstarter_bing, by = "ID")

clean_bing %>%
  filter(ovrl_sen > -5 & ovrl_sen < 5) %>% #Tu filteramo, ker je samo 1 projekt z -6 in 8 projektov z 6
  group_by(ovrl_sen) %>%
  summarise(per = mean(as.numeric(state_bi) - 1), cash = sum(usd_pledged_real / n()), nproj = n()) %>%
  ungroup() %>%
  ggplot(aes(x = ovrl_sen, y = per, fill = cash)) +
    geom_col(position = "dodge2") +
    geom_text(aes(label = nproj, vjust = - 1))

clean_bing %>%
  count(ovrl_sen)

summary(glm(state_bi ~ ovrl_sen, data = clean_bing, family = "binomial")) #Vidimo, da v zelo enostavnem modelu sentiment naslova igra signifikantno vlogo
exp(confint(glm(state_bi ~ ovrl_sen, data = clean_bing, family = "binomial"))) #Vidimo, da je vecanje sentimenta povezano z ~ 8 do 10% zmanjsanjem uspesnosti projekta, v zelo enostavnem modelu


#Analiza sentimenta - ali so projekti s pozitivnim sentimentom bolj uspešni % - AFINN LEXICON

kickstarter_afinn <- kickstarter_clean %>%
  unnest_tokens(word, name) %>%
  inner_join(get_sentiments("afinn"), by = "word")


#Izracun koncne sentiment vrednosti za vsak naslov
kickstarter_afinn <- kickstarter_afinn %>% 
  group_by(ID) %>%
  mutate(totsen = sum(value)) %>%
  select(ID, totsen)

count(kickstarter_afinn, ID) %>% arrange(desc(n)) #Da vidimo, kateri ID ima najvec primerov
filter(kickstarter_afinn, ID == 552538914)

clean_afinn <- inner_join(kickstarter_clean, unique(kickstarter_afinn), by = "ID") #Zdruziti sentiment vrednosti z originalnim df z ostalimi podatki, zdruzimo z unikati, da pocistimo podvajanje

count(clean_afinn, totsen)

summary(glm(state_bi ~ totsen, data = clean_afinn, family = "binomial")) #Vidimo, da je tudi z AFINN signifikantna razlika v zelo enostavnem modelu
exp(confint(glm(state_bi ~ totsen, data = clean_afinn, family = "binomial"))) #Tokrat je efekt precej manjsi (verjetno, ker je vec levelov) in sicer je vecanje totsena povezano z zmanjsanjem od ~ 1 do 2%


#Tabela z ekstremnimi sentiment value-i
clean_bing %>%
  filter(ovrl_sen <= -4) %>%
  select(name, ovrl_sen) %>%
  print(n = 20)

clean_bing %>%
  filter(ovrl_sen >= 4) %>%
  select(name, ovrl_sen) %>%
  print(n = 20)

clean_afinn %>%
  filter(totsen < -8) %>%
  select(name, totsen) %>%
  print(n = 20)

clean_afinn %>%
  filter(totsen > 11) %>%
  select(name, totsen) %>%
  print(n = 20)

##################----- Kratke predstavitve rezultatov dobljenih s sentiment analysis
#Wordcloudi


tekst <- kickstarter_data$name
kickstarter_corpus <- Corpus(VectorSource(tekst[sample(length(tekst), 1000)])) #Naredimo corpus sample velikosti 1000

clean_corpus <- kickstarter_corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

clean_corpus <- tm_map(clean_corpus, content_transformer(tolower))
clean_corpus <- tm_map(clean_corpus, removeWords, c(stopwords("english"), "canceled", "film", "book", "project", "game", "new", "album", "music")) #scistimo tekst

dtm <- TermDocumentMatrix(clean_corpus)
dtm_matrix <- as.matrix(dtm)
words <- sort(rowSums(dtm_matrix), decreasing = T)
wordcloud_data <- data.frame(word = names(words), freq = words)

wordcloud2(data = wordcloud_data, color = "random-dark")
#Vidimo, da je vecina stvari okoli love, smart, good, free, modern, fun, help, good, creative,...




#######################-------- Analiza podatkov pridobljenih s sentimentalno obdelavo


# Malo pobrisemo nepotrebne zadeve
rm(tekst, words)
rm(dtm, dtm_matrix)
rm(unikati, wordcloud_data, clean_corpus, kickstarter_corpus)
rm(kickstarter_afinn, kickstarter_bing)


#Prikaz distribucije, kaksna je uspesnost fundinga glede na overal sentiment projekta
clean_afinn %>%
  group_by(totsen) %>%
  summarise(per = mean(as.numeric(state_bi) - 1), cash = sum(usd_pledged_real / n()), nproj = n()) %>%
  ungroup() %>%
  ggplot(aes(x = totsen, y = per, fill = cash)) +
  geom_col(position = "dodge2", width = 1) +
  geom_text(aes(label = nproj, vjust = - 1)) #Ni vidnih nekih efektov



clean_bing %>%
  group_by(ovrl_sen) %>%
  summarise(per = mean(as.numeric(state_bi) - 1), cash = sum(usd_pledged_real / n()), nproj = n()) %>%
  ungroup() %>%
  ggplot(aes(x = ovrl_sen, y = per, fill = cash)) +
  geom_col(position = "dodge2", width = 1) +
  geom_text(aes(label = nproj, vjust = - 1)) #Pri bingu je efekt nekoliko boljse viden



#############################################################################
nrow(clean_bing)#vseh observationov je 109378
sum(clean_bing["state_bi"] == 0) #neuspesnih je 69312
sum(clean_bing["state_bi"] == 1) #uspesnih je 40066
40066/109378 #baseline je, da uspe 36,6% vseh projektov, klasa sta nekoliko unbalanced

clean_bing <- clean_bing %>%
  mutate(launched = as_date(launched)) %>%
  mutate(duration = as.numeric(deadline - launched))

set.seed(1412)

split_rows <- sample(nrow(clean_bing), nrow(clean_bing) / 2) #Splitaš na pol na training in test split
train_data <- clean_bing[split_rows, ]
test_data <- clean_bing[-split_rows, ]

table(test_data$state_bi) #Vidimo, da je case precej unbalanced
over_rf <- ovun.sample(state_bi ~ ., data = train_data, method = "over") #Z oversamplingom  balansiramo dataset
table(over_rf$data$state_bi) #Dobimo balansirana class-a
over_data <- over_rf$data






#Model uspešnosti fundinga - random forrest

rm(clean_bing, duration_model, kickstarter_clean_sample, kickstarter_bing, sentiment_model) #Malo je pocistimo pred modeliranjem

############!!!! POTREBNO JE BALANSIRATI SET? Lahko z classwt parametrom, tudi stratified sampling

clean_over <- over_data %>%
  select(-c("ID", "name", "deadline", "backers", "usd_pledged_real", "bi_cat", "launched"))

clean_over <- clean_over[sample(nrow(clean_over)), ] #Premesamo, ker prej so bili vsi case-i 0 in vsi case-i 1 skupaj


#Malo se pocistimo sproti
#rm(clean_afinn, modeldat_afinn)
################################### MODELLING ######################

kick_rf_model <- randomForest(state_bi ~ ., data = clean_over[, -1]) #Odstranimo category, ker ima prevec faktorjev in random forrest ni sposoben
print(kick_rf_model)
plot(kick_rf_model)
importance(kick_rf_model)


kick_rf_probs <- predict(kick_rf_model, newdata = test_data, type = "response")
cmat <- confusionMatrix(data = kick_rf_probs, reference = test_data$state_bi)
print(cmat)

auc(test_data$state_bi, as.numeric(kick_rf_probs) - 1) #RF da AUC 0.638


#Probamo ROCR za predictione
klasicni_prediction <- ROCR::prediction(kick_rf_model$votes[, 2], clean_over$state_bi) #Na training data
auc_temp_klasik <- ROCR::performance(klasicni_prediction, "auc")
klasik_auc <- as.numeric(auc_temp_klasik@y.values)
klasik_auc #Dobimo 0.67 AUC, precej bogo (glede na default)
klasik_plot_perf <- ROCR::performance(klasicni_prediction, "tpr", "fpr")
plot(klasik_plot_perf, colorize = T)

###Probamo dolociti cutoff
senspec_rf <- ROCR::performance(klasicni_prediction, "sens", "spec")
plot(senspec_rf)
senspec_rf@alpha.values[[1]][which.max(senspec_rf@x.values[[1]]+senspec_rf@y.values[[1]])] #Optimalni cutoff je tukaj 0.55
#Na ta nacin smo iskali cutoff


kick_rf_model_co <- randomForest(state_bi ~ ., data = clean_over[, -1], cutoff = c(0.55, 0.45)) #Odstranimo category, ker ima prevec faktorjev in random forrest ni sposoben
print(kick_rf_model_co)
plot(kick_rf_model_co)
importance(kick_rf_model_co)


kick_rf_probs_co <- predict(kick_rf_model_co, newdata = test_data, type = "response")
cmat_o <- confusionMatrix(data = kick_rf_probs_co, reference = test_data$state_bi)
print(cmat_o)

auc(test_data$state_bi, as.numeric(kick_rf_probs_co) - 1) #RF da AUC 0.641




##### Model uspešnosti fundinga - GBM model
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
