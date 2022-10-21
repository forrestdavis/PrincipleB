library(tidyverse)
library(lme4)
library(lmerTest)

path = '/Users/forrestdavis/Projects/PrincipleB/'

exp1 <- read.csv(paste(path, "results/PrincipleB_Exp1_combined.csv", sep=''))
exp2 <- read.csv(paste(path, "results/PrincipleB_Exp2_combined.csv", sep=''))
exp3 <- read.csv(paste(path, "results/PrincipleB_Exp3_combined.csv", sep=''))
exp4 <- read.csv(paste(path, "results/PrincipleB_Exp4_combined.csv", sep=''))
exp5 <- read.csv(paste(path, "results/PrincipleB_Exp5_combined.csv", sep=''))
exp6 <- read.csv(paste(path, "results/PrincipleB_Exp6_combined.csv", sep=''))


########################
#######  Exp 1/4   #####
########################

### Sum Coding
expname = "Exp1"

if(expname == "Exp1"){
  exp <- exp1
  exp$frameNum <- as.factor(exp$frameNum)
  exp$cond <- ifelse(exp$cond=="Match", 1, -1)
}

if(expname == "Exp4"){
  exp <- exp1
  exp$frameNum <- as.factor(exp$frameNum)
  exp$cond <- ifelse(exp$cond=="Match", 1, -1)
}

gptneo_model <- exp %>%
  filter(model == 'gpt-neo-2.7B' & pronoun=='obj') %>%
  lmer(surp ~ cond + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptneo_model)
anova(gptneo_model)

gptneo_model <- exp %>%
  filter(model == 'gpt-neo-2.7B' & pronoun=='poss') %>%
  lmer(surp ~ cond + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptneo_model)
anova(gptneo_model)

gptj_model <- exp %>%
  filter(model == 'gpt-j-6B' & pronoun=='obj') %>%
  lmer(surp ~ cond + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptj_model)
anova(gptj_model)

gptj_model <- exp %>%
  filter(model == 'gpt-j-6B' & pronoun=='poss') %>%
  lmer(surp ~ cond + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptj_model)
anova(gptj_model)

gpt2_model <- exp %>%
  filter(model == 'gpt2-xl' & pronoun=='obj') %>%
  lmer(surp ~ cond + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

gpt2_model <- exp %>%
  filter(model == 'gpt2-xl' & pronoun=='poss') %>%
  lmer(surp ~ cond + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

gpt3_model <- exp %>%
  filter(model == 'gpt3' & pronoun=='obj') %>%
  lmer(surp ~ cond + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt3_model)
anova(gpt3_model)

gpt3_model <- exp %>%
  filter(model == 'gpt3' & pronoun=='poss') %>%
  lmer(surp ~ cond + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt3_model)
anova(gpt3_model)

########################
#######    Exp 2   #####
########################

### Sum Coding
exp2$Matrix <- ifelse(exp2$cond %in% c("MatchMatch", "MatchMismatch") , 1, -1)
exp2$Embed <- ifelse(exp2$cond %in% c("MatchMatch", "MismatchMatch") , 1, -1)
exp2$frameNum <- as.factor(exp2$frameNum)

gptneo_model <- exp2 %>%
  filter(model == 'gpt-neo-2.7B' & pronoun=='obj') %>%
  lmer(surp ~ Matrix*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptneo_model)
anova(gptneo_model)

gptneo_model <- exp2 %>%
  filter(model == 'gpt-neo-2.7B' & pronoun=='poss') %>%
  lmer(surp ~ Matrix*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptneo_model)
anova(gptneo_model)

gptj_model <- exp2 %>%
  filter(model == 'gpt-j-6B' & pronoun=='obj') %>%
  lmer(surp ~ Matrix*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptj_model)
anova(gptj_model)

gptj_model <- exp2 %>%
  filter(model == 'gpt-j-6B' & pronoun=='poss') %>%
  lmer(surp ~ Matrix*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptj_model)
anova(gptj_model)

gpt2_model <- exp2 %>%
  filter(model == 'gpt2-xl' & pronoun=='obj') %>%
  lmer(surp ~ Matrix*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

gpt2_model <- exp2 %>%
  filter(model == 'gpt2-xl' & pronoun=='poss') %>%
  lmer(surp ~ Matrix*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

gpt3_model <- exp2 %>%
  filter(model == 'gpt3' & pronoun=='obj') %>%
  lmer(surp ~ Matrix*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt3_model)
anova(gpt3_model)

gpt3_model <- exp2 %>%
  filter(model == 'gpt3' & pronoun=='poss') %>%
  lmer(surp ~ Matrix*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt3_model)
anova(gpt3_model)

########################
#######    Exp 3   #####
########################

### Sum Coding
exp3$Matrix <- ifelse(exp3$cond %in% c("MatchMatchMatch", "MatchMatchMismatch", "MatchMismatchMatch", "MatchMismatchMismatch") , 1, -1)
exp3$Middle <- ifelse(exp3$cond %in% c("MatchMatchMatch", "MatchMatchMismatch", "MismatchMatchMatch", "MismatchMatchMismatch") , 1, -1)
exp3$Embed <- ifelse(exp3$cond %in% c("MatchMatchMatch", "MatchMismatchMatch", "MismatchMatchMatch", "MismatchMismatchMatch") , 1, -1)
exp3$frameNum <- as.factor(exp3$frameNum)

gptneo_model <- exp3 %>%
  filter(model == 'gpt-neo-2.7B' & pronoun=='obj') %>%
  lmer(surp ~ Matrix*Middle*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptneo_model)
anova(gptneo_model)

gptneo_model <- exp3 %>%
  filter(model == 'gpt-neo-2.7B' & pronoun=='poss') %>%
  lmer(surp ~ Matrix*Middle*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptneo_model)
anova(gptneo_model)

gptj_model <- exp3 %>%
  filter(model == 'gpt-j-6B' & pronoun=='obj') %>%
  lmer(surp ~ Matrix*Middle*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptj_model)
anova(gptj_model)

gptj_model <- exp3 %>%
  filter(model == 'gpt-j-6B' & pronoun=='poss') %>%
  lmer(surp ~ Matrix*Middle*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptj_model)
anova(gptj_model)

gpt2_model <- exp3 %>%
  filter(model == 'gpt2-xl' & pronoun=='obj') %>%
  lmer(surp ~ Matrix*Middle*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

gpt2_model <- exp3 %>%
  filter(model == 'gpt2-xl' & pronoun=='poss') %>%
  lmer(surp ~ Matrix*Middle*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

gpt3_model <- exp3 %>%
  filter(model == 'gpt3' & pronoun=='obj') %>%
  lmer(surp ~ Matrix*Middle*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt3_model)
anova(gpt3_model)

gpt3_model <- exp3 %>%
  filter(model == 'gpt3' & pronoun=='poss') %>%
  lmer(surp ~ Matrix*Middle*Embed + (1 | frameNum), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt3_model)
anova(gpt3_model)

########################
#######  Exp 5     #####
########################

### Sum Coding
exp5$item <-as.factor(exp5$item)
exp5$pronoun <- as.factor(exp5$pronoun)
exp5$contrast <- ifelse(exp$contrast == "Match", 1, -1)

gptneo_model <- exp5 %>%
  filter(model == 'gpt-neo-2.7B') %>%
  lmer(surp ~ cond + (1 | item) + (1|pronoun), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptneo_model)
anova(gptneo_model)

gptj_model <- exp5 %>%
  filter(model == 'gpt-j-6B') %>%
  lmer(surp ~ cond + (1 | item) + (1|pronoun), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptj_model)
anova(gptj_model)

gpt2_model <- exp5 %>%
  filter(model == 'gpt2-xl') %>%
  lmer(surp ~ cond + (1 | item) + (1|pronoun), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

gpt3_model <- exp5 %>%
  filter(model == 'gpt3') %>%
  lmer(surp ~ cond + (1 | item) + (1|pronoun),  
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt3_model)
anova(gpt3_model)

########################
#######  Exp 6     #####
########################

#### Sum Coding
exp6$adjunct <- ifelse(exp6$adjunct== "Constraint", 1, -1)
exp6$contrast <- ifelse(exp6$contrast=="Match", 1, -1)
exp6$item <- as.factor(exp6$item)
exp6$pronoun <- as.factor(exp6$pronoun)

gptneo_model <- exp6 %>%
  filter(model == 'gpt-neo-2.7B') %>%
  lmer(surp ~ adjunct*contrast + (1|item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptneo_model)
anova(gptneo_model)

gptj_model <- exp6 %>%
  filter(model == 'gpt-j-6B') %>%
  lmer(surp ~ adjunct*contrast + (1|item),  
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gptj_model)
anova(gptj_model)

gpt2_model <- exp6 %>%
  filter(model == 'gpt2-xl') %>%
  lmer(surp ~ adjunct*contrast + (1|item),  
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt2_model)
anova(gpt2_model)

gpt3_model <- exp6 %>%
  filter(model == 'gpt3') %>%
  lmer(surp ~ adjunct*contrast + (1|item), 
       data = ., REML = FALSE,
       control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

summary(gpt3_model)
anova(gpt3_model)

########################
#######  ms/bit    #####
########################

#The following preprocess is copied for Kush and Dillon (2021)
### Load data
data <- read.csv(paste(path,"KushDillon_Experiment2_All_Stimuli_J_Surps.csv", sep=''))

### Boolean flags to set for exclusion criteria
### To replicate results in paper, both are set to TRUE

remove.subjects <- TRUE                ### Do you wish to exclude subjects?
remove.outlier.RTs <- TRUE             ### Do you wish to trim outlier RTs?

### List of subjects to exclude, with reasons

excluded.subjects <-  c("0c92688650fc28778e38f29d3b02b3a3"       ### 55% accuracy
                        ,"18bae00ff0c2e917d8d1c722c02ae09d"      ### Debrief indicates non-native speaker (Korean)
                        ,"450b87c4effb3f110640a6ee59a53777"      ### Unacceptable debrief "no comment"
                        ,"dea99dd964ebf454177dbc9fb94626c4"      ### Unacceptable debrief "i dont know"; 53% accuracy
                        ,"122c00f000019a9cef2e5f75d0b0e723"      ### 74% accuracy
                        ,"18bae00ff0c2e917d8d1c722c02ae09d"      ### 72% accuracy
                        ,"6c853a467a774e7e727ec0293f28d21f"      ### 66% accuracy
                        ,"7c5a188432960341849c7edf558b0672"      ### 74% accuracy
                        ,"ad8eef6643a3a485e6dd78873e51117b"      ### 74% accuracy
                        ,"ce5d16036fee6ffa91ee8b128cb06b5e"      ### 64% accuracy
                        ,"b39cdafe3585f6a756b9fa31f602775b"      ### Debrief indicates non-native speaker (Hmong)
                        ,"f2a18eb33a42d57c1fb4da9a73c96cc3"      ### 61% accuracy
                        ,"6a0448de36c84426302630b4d52915fa"      ### 74% accuracy
                        ,"a3e48e531a62b3d421106b0c1519d27e"      ### 74% accuracy
                        ,"e92d42985b435c785e0286c040396230"      ### 74% accuracy
                        ,"0c7e2af7028242073c4ad56a12070836")    ### Debrief indicates non-native speaker (Spanish)   

### Preprocessing steps: Do you wish to exclude subjects, and/or trim RTs?

if (remove.subjects) {
  data <- data %>%
    filter(!(subj %in% excluded.subjects))
}

if (remove.outlier.RTs) {
  data <- data %>%
    filter(RT > 100 & RT < 3000) 
}

### Because the 'constraint' conditions in this experiment have one additional region, the region labels need to be modified to align them

data$region <- ifelse(data$region >= 2 & data$cond %in% c("CATAMARAN2-OBJ-Match",
                                                          "CATAMARAN2-OBJ-Misma"),data$region+1,data$region)


conditions <- c("CATAMARAN2-FIN-Match",
                "CATAMARAN2-FIN-Misma",
                "CATAMARAN2-OBJ-Match",
                "CATAMARAN2-OBJ-Misma")

#############
## GPT-NEO ##
#############
#Get spillover info following van Schijndel and Linzen (2021)
neo_spr <- data %>% mutate(logfreq = log(freq+1)) 
neo_spr$surp <- neo_spr$gptneo_surp

neo_spr <- neo_spr %>% filter(!(cond %in% conditions))

#neo_spr <- neo_spr %>% group_by_at(vars(UID, subj)) %>% 
neo_spr <- neo_spr %>% group_by_at(vars(UID)) %>% 
  mutate(previous_surp = dplyr::lag(surp, n = 1, order_by=region, default = NA),
         previous2_surp = dplyr::lag(surp, n = 2, order_by=region, default = NA),
         previous3_surp = dplyr::lag(surp, n = 3, order_by=region, default = NA),
         #previous_length = dplyr::lag(wordlen, n = 1, order_by=region, default = NA),
         #previous2_length = dplyr::lag(wordlen, n = 2, order_by=region, default = NA),
         #previous3_length = dplyr::lag(wordlen, n = 3, order_by=region, default = NA),
         previous_logfreq = dplyr::lag(logfreq, n = 1, order_by=region, default = NA),
         previous2_logfreq = dplyr::lag(logfreq, n = 2, order_by=region, default = NA),
         previous3_logfreq = dplyr::lag(logfreq, n = 3, order_by=region, default = NA)) %>% as.data.frame()

neo_spr <- neo_spr %>% filter(surp > 0 & previous_surp > 0  & previous2_surp > 0 & previous3_surp > 0) %>%
        filter(!is.na(previous_surp)) %>%
        filter(!is.na(previous2_surp)) %>%
        filter(!is.na(previous3_surp)) %>%
  filter(!is.na(previous_length)) %>%
  filter(!is.na(previous2_length)) %>%
  filter(!is.na(previous3_length)) %>%
  filter(!is.na(previous_logfreq)) %>%
  filter(!is.na(previous2_logfreq)) %>%
  filter(!is.na(previous3_logfreq))

#############
## GPT-J ##
#############
#Get spillover info following van Schijndel and Linzen (2021)
j_spr <- data %>% mutate(logfreq = log(freq+1)) 
j_spr$surp <- j_spr$gptj_surp

j_spr <- j_spr %>% filter(!(cond %in% conditions))

#j_spr <- j_spr %>% group_by_at(vars(UID, subj)) %>% 
j_spr <- j_spr %>% group_by_at(vars(UID)) %>% 
  mutate(previous_surp = dplyr::lag(surp, n = 1, order_by=region, default = NA),
         previous2_surp = dplyr::lag(surp, n = 2, order_by=region, default = NA),
         previous3_surp = dplyr::lag(surp, n = 3, order_by=region, default = NA),
         #previous_length = dplyr::lag(wordlen, n = 1, order_by=region, default = NA),
         #previous2_length = dplyr::lag(wordlen, n = 2, order_by=region, default = NA),
         #previous3_length = dplyr::lag(wordlen, n = 3, order_by=region, default = NA),
         previous_logfreq = dplyr::lag(logfreq, n = 1, order_by=region, default = NA),
         previous2_logfreq = dplyr::lag(logfreq, n = 2, order_by=region, default = NA),
         previous3_logfreq = dplyr::lag(logfreq, n = 3, order_by=region, default = NA)) %>% as.data.frame()

j_spr <- j_spr %>% filter(surp > 0 & previous_surp > 0  & previous2_surp > 0 & previous3_surp > 0) %>%
  filter(!is.na(previous_surp)) %>%
  filter(!is.na(previous2_surp)) %>%
  filter(!is.na(previous3_surp)) %>%
  filter(!is.na(previous_length)) %>%
  filter(!is.na(previous2_length)) %>%
  filter(!is.na(previous3_length)) %>%
  filter(!is.na(previous_logfreq)) %>%
  filter(!is.na(previous2_logfreq)) %>%
  filter(!is.na(previous3_logfreq))

#get ms/bit

formula.lmer <- RT ~ surp + previous_surp + previous2_surp + previous3_surp + 
  region + 
  logfreq*wordlen + previous_logfreq*previous_length + previous2_logfreq*previous2_length + previous3_logfreq*previous3_length + (1|subj)

neo_model.lmer <- lmer(formula.lmer,data=neo_spr)
j_model.lmer <- lmer(formula.lmer,data=j_spr)

summary(neo_model.lmer)
summary(j_model.lmer)

save(neo_model.lmer, file=paste(path, "results/gpt-neo_RT_fit.rda", sep=''))
save(j_model.lmer, file=paste(path, "results/gpt-j_RT_fit.rda", sep=''))
