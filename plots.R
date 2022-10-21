library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)

path = '/User/forrestdavis/Projects/PrincipleB/'

####################
# Exp 1 and Exp 4  #
####################
exp1 <- read.csv(paste(path, "results/PrincipleB_Exp1_combined.csv", sep=''))
exp4 <- read.csv(paste(path, "results/PrincipleB_Exp4_combined.csv", sep=''))


#Get GMME for Exp1
exp1_him <- exp1 %>% filter(pronoun == "obj")
exp1_his <- exp1 %>% filter(pronoun != "obj")

exp1_him_Match <- exp1_him %>% filter(cond=="Match")
exp1_him_Mismatch <- exp1_him %>% filter(cond!="Match")
exp1_his_Match <- exp1_his %>% filter(cond=="Match")
exp1_his_Mismatch <- exp1_his %>% filter(cond!="Match")

exp1_him_GMME <- exp1_him_Mismatch$surp - exp1_him_Match$surp
exp1_his_GMME <- exp1_his_Mismatch$surp - exp1_his_Match$surp

exp1_him <- exp1_him_Match %>% select(exp, verb, frameNum, uid, gender, cond, pronoun, sent, target, model)
exp1_his <- exp1_his_Match %>% select(exp, verb, frameNum, uid, gender, cond, pronoun, sent, target, model)

exp1_GMME <- rbind(exp1_him, exp1_his)  

exp1_GMME$GMME = c(exp1_him_GMME, exp1_his_GMME)

#Get GMME for Exp4
exp4_him <- exp4 %>% filter(pronoun == "obj")
exp4_his <- exp4 %>% filter(pronoun != "obj")

exp4_him_Match <- exp4_him %>% filter(cond=="Match")
exp4_him_Mismatch <- exp4_him %>% filter(cond!="Match")
exp4_his_Match <- exp4_his %>% filter(cond=="Match")
exp4_his_Mismatch <- exp4_his %>% filter(cond!="Match")

exp4_him_GMME <- exp4_him_Mismatch$surp - exp4_him_Match$surp
exp4_his_GMME <- exp4_his_Mismatch$surp - exp4_his_Match$surp

exp4_him <- exp4_him_Match %>% select(exp, verb, frameNum, uid, gender, cond, pronoun, sent, target, model)
exp4_his <- exp4_his_Match %>% select(exp, verb, frameNum, uid, gender, cond, pronoun, sent, target, model)

exp4_GMME <- rbind(exp4_him, exp4_his)  

exp4_GMME$GMME = c(exp4_him_GMME, exp4_his_GMME)

expGMME <- rbind(exp1_GMME, exp4_GMME)

expGMME_sum <- expGMME %>%
  group_by(exp, pronoun, model) %>%
  summarise( 
    n=n(),
    mean=mean(GMME),
    sd=sd(GMME)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1))

expGMME_sum %>% filter(!model %in% c('gpt-neo-1.3B', 'gpt-neo-125M', 'gpt2', 'gpt2-medium', 'gpt2-large')) %>% 
  mutate(across(model, factor, levels=c("gpt2-xl","gpt-neo-2.7B", "gpt-j-6B", "gpt3"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "gpt-neo-2.7B"="GPT-Neo", 
                        "gpt-j-6B"="GPT-J", "gpt3"="GPT-3")) %>%
  mutate(pronoun = recode(pronoun, "obj"="Object",
                          "poss"="Poss")) %>%
  ggplot(aes(x=exp, y=mean, fill=pronoun)) +
  geom_bar(position=position_dodge(), stat="identity", alpha=0.7) +
  ylim(-1.5, 5) + theme(text = element_text(size=20)) +  
  scale_x_discrete(breaks=c("simple", "About"),
                   labels=c("Simple", "Complex")) + labs(y="Gender Mismatch Effect (bits)", x='Experiment',title="Gender Mismatch Effect by Model and Experiment", fill="Pronoun") +
  scale_fill_manual(values=c('mediumpurple3', 
                             "#E69F00"))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9) +
  facet_wrap(~model)

ggsave(paste(path,"figures/Exp1-4.png", sep=""))

####################
#####  Exp 2  ######
####################
exp2 <- read.csv(paste(path, "results/PrincipleB_Exp2_combined.csv", sep=''))

exp2_him <- exp2 %>% filter(pronoun == "obj")
exp2_his <- exp2 %>% filter(pronoun != "obj")

exp2_him_MM <- exp2_him %>% filter(cond=="MatchMatch")
exp2_him_MF <- exp2_him %>% filter(cond=="MatchMismatch")
exp2_him_FM <- exp2_him %>% filter(cond=="MismatchMatch")
exp2_him_FF <- exp2_him %>% filter(cond=="MismatchMismatch")

exp2_his_MM <- exp2_his %>% filter(cond=="MatchMatch")
exp2_his_MF <- exp2_his %>% filter(cond=="MatchMismatch")
exp2_his_FM <- exp2_his %>% filter(cond=="MismatchMatch")
exp2_his_FF <- exp2_his %>% filter(cond=="MismatchMismatch")

exp2_him_Matrix_GMME <- c(exp2_him_FM$surp-exp2_him_MM$surp, exp2_him_FF$surp-exp2_him_MF$surp)
exp2_him_Embed_GMME <- c(exp2_him_FF$surp-exp2_him_FM$surp, exp2_him_MF$surp-exp2_him_MM$surp)

exp2_his_Matrix_GMME <- c(exp2_his_FM$surp-exp2_his_MM$surp, exp2_his_FF$surp-exp2_his_MF$surp)
exp2_his_Embed_GMME <- c(exp2_his_FF$surp-exp2_his_FM$surp, exp2_his_MF$surp-exp2_his_MM$surp)

#Get GMME for him for both Matrix and Embed in one dataframe
exp2_him_Matrix <- rbind(exp2_him_FM, exp2_him_FF) %>% select(exp, verbMatrix, verbEmbed, frameNum, cond, pronoun, sent, target, model)
exp2_him_Matrix$contrast = "Matrix"

if(length(exp2_him_Matrix$contrast) != length(exp2_him_Matrix_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp2_him_Matrix$GMME = exp2_him_Matrix_GMME 
}

exp2_him_Embed <- rbind(exp2_him_FF, exp2_him_MF) %>% select(exp, verbMatrix, verbEmbed, frameNum, cond, pronoun, sent, target, model)
exp2_him_Embed$contrast = "Embedded"

if(length(exp2_him_Embed$contrast) != length(exp2_him_Embed_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp2_him_Embed$GMME = exp2_him_Embed_GMME 
}

exp2_him <- rbind(exp2_him_Matrix, exp2_him_Embed)  

#Get GMME for his for both Matrix and Embed in one dataframe
exp2_his_Matrix <- rbind(exp2_his_FM, exp2_his_FF) %>% select(exp, verbMatrix, verbEmbed, frameNum, cond, pronoun, sent, target, model)
exp2_his_Matrix$contrast = "Matrix"

if(length(exp2_his_Matrix$contrast) != length(exp2_his_Matrix_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp2_his_Matrix$GMME = exp2_his_Matrix_GMME 
}

exp2_his_Embed <- rbind(exp2_his_FF, exp2_his_MF) %>% select(exp, verbMatrix, verbEmbed, frameNum, cond, pronoun, sent, target, model)
exp2_his_Embed$contrast = "Embedded"

if(length(exp2_his_Embed$contrast) != length(exp2_his_Embed_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp2_his_Embed$GMME = exp2_his_Embed_GMME 
}

exp2_his <- rbind(exp2_his_Matrix, exp2_his_Embed)  

exp2GMME <- rbind(exp2_him, exp2_his)

### Get summary info and plot
exp2GMME_sum <- exp2GMME %>%
  group_by(contrast, pronoun, model) %>%
  summarise( 
    n=n(),
    mean=mean(GMME),
    sd=sd(GMME)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1))

exp2GMME_sum %>%  filter(!model %in% c('gpt-neo-1.3B', 'gpt-neo-125M', 'gpt2', 'gpt2-medium', 'gpt2-large')) %>% 
  mutate(across(model, factor, levels=c("gpt2-xl","gpt-neo-2.7B", "gpt-j-6B", "gpt3"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "gpt-neo-2.7B"="GPT-Neo", 
                        "gpt-j-6B"="GPT-J", "gpt3"="GPT-3")) %>%
  mutate(pronoun = recode(pronoun, "obj"="Object",
                          "poss"="Poss")) %>%
    ggplot(aes(x=contrast, y=mean, fill=pronoun)) +
  scale_x_discrete(limits = c("Matrix", "Embedded"))+
  labs(y="Gender Mismatch Effect (bits)", x='Position in Sentence',title="Gender Mismatch Effect by Model and Position", fill="Pronoun") +
  geom_bar(position=position_dodge(), stat="identity", alpha=0.7) +
  ylim(-1.5, 5) + theme(text = element_text(size=20)) + 
  scale_fill_manual(values=c('mediumpurple3', 
                             "#E69F00"))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9)  +
  facet_wrap(~model)

ggsave(paste(path,"figures/Exp2.png", sep=""))

####################
#####  Exp 3  ######
####################
exp3 <- read.csv(paste(path, "results/PrincipleB_Exp3_combined.csv", sep=''))

exp3_him <- exp3 %>% filter(pronoun == "obj")
exp3_his <- exp3 %>% filter(pronoun != "obj")

exp3_him_MMM <- exp3_him %>% filter(cond=="MatchMatchMatch")
exp3_him_MMF <- exp3_him %>% filter(cond=="MatchMatchMismatch")
exp3_him_MFM <- exp3_him %>% filter(cond=="MatchMismatchMatch")
exp3_him_MFF <- exp3_him %>% filter(cond=="MatchMismatchMismatch")
exp3_him_FMM <- exp3_him %>% filter(cond=="MismatchMatchMatch")
exp3_him_FMF <- exp3_him %>% filter(cond=="MismatchMatchMismatch")
exp3_him_FFM <- exp3_him %>% filter(cond=="MismatchMismatchMatch")
exp3_him_FFF <- exp3_him %>% filter(cond=="MismatchMismatchMismatch")

exp3_him_Matrix_GMME <- c(exp3_him_FMM$surp-exp3_him_MMM$surp, 
                          exp3_him_FMF$surp-exp3_him_MMF$surp, 
                          exp3_him_FFM$surp-exp3_him_MFM$surp, 
                          exp3_him_FFF$surp-exp3_him_MFF$surp)

exp3_him_Middle_GMME <- c(exp3_him_MFM$surp-exp3_him_MMM$surp, 
                          exp3_him_MFF$surp-exp3_him_MMF$surp, 
                          exp3_him_FFM$surp-exp3_him_FMM$surp, 
                          exp3_him_FFF$surp-exp3_him_FMF$surp)

exp3_him_Embed_GMME <- c(exp3_him_MMF$surp-exp3_him_MMM$surp, 
                          exp3_him_MFF$surp-exp3_him_MFM$surp, 
                          exp3_him_FMF$surp-exp3_him_FMM$surp, 
                          exp3_him_FFF$surp-exp3_him_FFM$surp)

#Get GMME for him for both Matrix and Embed in one dataframe
exp3_him_Matrix <- rbind(exp3_him_FMM, exp3_him_FMF, exp3_him_FFM, exp3_him_FFF) %>% select(exp, verbMatrix, verbEmbed, frameNum, cond, pronoun, sent, target, model)
exp3_him_Matrix$contrast = "Matrix"

if(length(exp3_him_Matrix$contrast) != length(exp3_him_Matrix_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp3_him_Matrix$GMME = exp3_him_Matrix_GMME 
}

exp3_him_Middle <- rbind(exp3_him_MMF, exp3_him_MFF, exp3_him_FMF, exp3_him_FFF) %>% select(exp, verbMatrix, verbEmbed, frameNum, cond, pronoun, sent, target, model)
exp3_him_Middle$contrast = "Middle"

if(length(exp3_him_Middle$contrast) != length(exp3_him_Middle_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp3_him_Middle$GMME = exp3_him_Middle_GMME 
}

exp3_him_Embed <- rbind(exp3_him_MFM, exp3_him_MFF, exp3_him_FFM, exp3_him_FFF) %>% select(exp, verbMatrix, verbEmbed, frameNum, cond, pronoun, sent, target, model)
exp3_him_Embed$contrast = "Embedded"

if(length(exp3_him_Embed$contrast) != length(exp3_him_Embed_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp3_him_Embed$GMME = exp3_him_Embed_GMME 
}

exp3_him <- rbind(exp3_him_Matrix, exp3_him_Middle, exp3_him_Embed)  

exp3_his_MMM <- exp3_his %>% filter(cond=="MatchMatchMatch")
exp3_his_MMF <- exp3_his %>% filter(cond=="MatchMatchMismatch")
exp3_his_MFM <- exp3_his %>% filter(cond=="MatchMismatchMatch")
exp3_his_MFF <- exp3_his %>% filter(cond=="MatchMismatchMismatch")
exp3_his_FMM <- exp3_his %>% filter(cond=="MismatchMatchMatch")
exp3_his_FMF <- exp3_his %>% filter(cond=="MismatchMatchMismatch")
exp3_his_FFM <- exp3_his %>% filter(cond=="MismatchMismatchMatch")
exp3_his_FFF <- exp3_his %>% filter(cond=="MismatchMismatchMismatch")

exp3_his_Matrix_GMME <- c(exp3_his_FMM$surp-exp3_his_MMM$surp, 
                          exp3_his_FMF$surp-exp3_his_MMF$surp, 
                          exp3_his_FFM$surp-exp3_his_MFM$surp, 
                          exp3_his_FFF$surp-exp3_his_MFF$surp)

exp3_his_Middle_GMME <- c(exp3_his_MFM$surp-exp3_his_MMM$surp, 
                          exp3_his_MFF$surp-exp3_his_MMF$surp, 
                          exp3_his_FFM$surp-exp3_his_FMM$surp, 
                          exp3_his_FFF$surp-exp3_his_FMF$surp)

exp3_his_Embed_GMME <- c(exp3_his_MMF$surp-exp3_his_MMM$surp, 
                         exp3_his_MFF$surp-exp3_his_MFM$surp, 
                         exp3_his_FMF$surp-exp3_his_FMM$surp, 
                         exp3_his_FFF$surp-exp3_his_FFM$surp)

#Get GMME for his for both Matrix and Embed in one dataframe
exp3_his_Matrix <- rbind(exp3_his_FMM, exp3_his_FMF, exp3_his_FFM, exp3_his_FFF) %>% select(exp, verbMatrix, verbEmbed, frameNum, cond, pronoun, sent, target, model)
exp3_his_Matrix$contrast = "Matrix"

if(length(exp3_his_Matrix$contrast) != length(exp3_his_Matrix_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp3_his_Matrix$GMME = exp3_his_Matrix_GMME 
}

exp3_his_Middle <- rbind(exp3_his_MMF, exp3_his_MFF, exp3_his_FMF, exp3_his_FFF) %>% select(exp, verbMatrix, verbEmbed, frameNum, cond, pronoun, sent, target, model)
exp3_his_Middle$contrast = "Middle"

if(length(exp3_his_Middle$contrast) != length(exp3_his_Middle_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp3_his_Middle$GMME = exp3_his_Middle_GMME 
}

exp3_his_Embed <- rbind(exp3_his_MFM, exp3_his_MFF, exp3_his_FFM, exp3_his_FFF) %>% select(exp, verbMatrix, verbEmbed, frameNum, cond, pronoun, sent, target, model)
exp3_his_Embed$contrast = "Embedded"

if(length(exp3_his_Embed$contrast) != length(exp3_his_Embed_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp3_his_Embed$GMME = exp3_his_Embed_GMME 
}

exp3_his <- rbind(exp3_his_Matrix, exp3_his_Middle, exp3_his_Embed)  

exp3GMME <- rbind(exp3_him, exp3_his)

### Get summary info and plot
exp3GMME_sum <- exp3GMME %>%
  group_by(contrast, pronoun, model) %>%
  summarise( 
    n=n(),
    mean=mean(GMME),
    sd=sd(GMME)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1))

exp3GMME_sum %>%  filter(!model %in% c('gpt-neo-1.3B', 'gpt-neo-125M', 'gpt2', 'gpt2-medium', 'gpt2-large')) %>% 
  mutate(across(model, factor, levels=c("gpt2-xl","gpt-neo-2.7B", "gpt-j-6B", "gpt3"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "gpt-neo-2.7B"="GPT-Neo", 
                        "gpt-j-6B"="GPT-J", "gpt3"="GPT-3")) %>%
  mutate(pronoun = recode(pronoun, "obj"="Object",
                          "poss"="Poss")) %>%
  ggplot(aes(x=contrast, y=mean, fill=pronoun)) +
  scale_x_discrete(limits = c("Matrix", "Middle", "Embedded"), labels=c("Matrix", "Object", "Embedded"))+
  labs(y="Gender Mismatch Effect (bits)", x='Position in Sentence',title="Gender Mismatch Effect by Model and Position", fill="Pronoun") +
  geom_bar(position=position_dodge(), stat="identity", alpha=0.7) +
  ylim(-1.5, 5) + theme(text = element_text(size=20)) + 
  scale_fill_manual(values=c('mediumpurple3', 
                             "#E69F00"))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9)  +
  facet_wrap(~model)

ggsave(paste(path,"figures/Exp3.png", sep=""))

####################
#####  Exp 5  ######
####################
exp5 <- read.csv(paste(path, "results/PrincipleB_Exp5_combined.csv", sep=''))

exp5_he_Match <- exp5 %>% filter(pronoun == 'he' & contrast=="Match")
exp5_he_Mismatch <- exp5 %>% filter(pronoun == 'he' & contrast!="Match")
exp5_she_Match <- exp5 %>% filter(pronoun != 'he' & contrast=="Match")
exp5_she_Mismatch <- exp5 %>% filter(pronoun != 'he' & contrast!="Match")

exp5_he_GMME <- exp5_he_Mismatch$surp - exp5_he_Match$surp
exp5_she_GMME <- exp5_she_Mismatch$surp - exp5_she_Match$surp

exp5_he <- exp5_he_Mismatch %>% select(item, exp, pronoun, cond, contrast, sent, target, model)
exp5_she <- exp5_she_Mismatch %>% select(item, exp, pronoun, cond, contrast, sent, target, model)

exp5GMME <- rbind(exp5_he, exp5_she)  

exp5GMME$GMME = c(exp5_he_GMME, exp5_she_GMME)

exp5GMME_sum <- exp5GMME %>%
  group_by(model) %>%
  summarise( 
    n=n(),
    mean=mean(GMME),
    sd=sd(GMME)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1))

exp5GMME_sum %>%  filter(!model %in% c('gpt-neo-1.3B', 'gpt-neo-125M', 'gpt2', 'gpt2-medium', 'gpt2-large')) %>%  
  mutate(across(model, factor, levels=c("gpt2-xl","gpt-neo-2.7B", "gpt-j-6B", "gpt3"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "gpt-neo-2.7B"="GPT-Neo", 
                        "gpt-j-6B"="GPT-J", "gpt3"="GPT-3")) %>%
  ggplot(aes(x=model, y=mean)) +
  geom_bar(position=position_dodge(), stat="identity", alpha=0.7, fill="mediumpurple3") +
  ylim(0, 5) + theme(text = element_text(size=20)) +  
  labs(y="Gender Mismatch Effect (bits)", x='Model') +
  labs(y="Gender Mismatch Effect (bits)", x='Model',title="Gender Mismatch Effect for Subject Cataphora") +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9)

ggsave(paste(path,"figures/Exp5.png", sep=""))

####################
#####  Exp 6  ######
####################
exp6 <- read.csv(paste(path, "results/PrincipleB_Exp6_combined.csv", sep=''))

exp6_Match <- exp6 %>% filter(contrast=="Match")
exp6_Mismatch <- exp6 %>% filter(contrast!="Match")

exp6_GMME <- exp6_Mismatch$surp - exp6_Match$surp

exp6GMME <- exp6_Mismatch %>% select(item, cond, adjunct, contrast, sent, target, model)

if(length(exp6GMME$contrast) != length(exp6_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp6GMME$GMME = exp6_GMME 
}

exp6GMME_sum <- exp6GMME %>%
  group_by(adjunct, model) %>%
  summarise( 
    n=n(),
    mean=mean(GMME),
    sd=sd(GMME)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1))

exp6GMME_sum %>%  filter(!model %in% c('gpt-neo-1.3B', 'gpt-neo-125M', 'gpt2', 'gpt2-medium', 'gpt2-large')) %>% 
  mutate(across(model, factor, levels=c("gpt2-xl","gpt-neo-2.7B", "gpt-j-6B", "gpt3"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "gpt-neo-2.7B"="GPT-Neo", 
                        "gpt-j-6B"="GPT-J", "gpt3"="GPT-3")) %>%
  ggplot(aes(x=adjunct, y=mean)) +
  scale_x_discrete(breaks=c("No Constraint", "Constraint"),
                   labels=c("No Principle B", "Principle B"))+
  labs(y="Gender Mismatch Effect (bits)", x='Experiment Condition', title="Gender Mismatch Effect for Object Cataphora") +
  geom_bar(position=position_dodge(), stat="identity",  fill="mediumpurple3",alpha=0.7) +
  ylim(-1.5, 5) + theme(text = element_text(size=20)) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9)  +
  facet_wrap(~model)

ggsave(paste(path,"figures/Exp6.png", sep=""))

####################
#####  Scale  ######
####################

keyExp1 <- expGMME_sum %>% filter(pronoun == 'obj' & exp == 'simple') %>% as.data.frame()
keyExp2 <- exp2GMME_sum %>% filter(pronoun=="obj" & contrast=="Embedded") %>% as.data.frame()
keyExp3 <- exp3GMME_sum %>% filter(pronoun=="obj" & contrast=="Embedded") %>% as.data.frame()
keyExp6 <- exp6GMME_sum %>% filter(adjunct == "Constraint") %>% as.data.frame()

#Add exp to keyExp6 and keyExp2 and keyExp3
keyExp1$exp = "Simple Subject"
keyExp2$exp = "2NP"
keyExp3$exp = "3NP"
keyExp6$exp = "Object Cataphora"

#Select desired columns
keyExp1 <- keyExp1 %>% select(exp, model, mean, ci)
keyExp2 <- keyExp2 %>% select(exp, model, mean, ci)
keyExp3 <- keyExp3 %>% select(exp, model, mean, ci)
keyExp6 <- keyExp6 %>% select(exp, model, mean, ci)

keyExps <- rbind(keyExp1, keyExp2, keyExp3, keyExp6)
keyExps$modelSize <- keyExps$model

#Add model sizes and clean up names
keyExps <- keyExps %>% mutate(modelSize = recode(modelSize, 
                                        "gpt2"=124, 
                                        "gpt2-medium"=335, 
                                        "gpt2-large"=774, 
                                        "gpt2-xl"=1500, 
                                        "gpt-j-6B"=6000, 
                                        "gpt-neo-1.3B"=1300, 
                                        "gpt-neo-125M"=125,
                                        "gpt-neo-2.7B"=2700, 
                                        "gpt3"=175000)) 

keyExps <- keyExps %>%
             mutate(model = recode(model,
                            "gpt2"="GPT-2 Small",
                            "gpt2-medium"="GPT-2 Medium",
                            "gpt2-large"="GPT-2 Large",
                            "gpt2-xl"="GPT-2 XL", 
                            "gpt-j-6B"="GPT-J", 
                            "gpt-neo-1.3B"="GPT-Neo Medium", 
                            "gpt-neo-125M"="GPT-Neo Small",
                            "gpt-neo-2.7B"="GPT-Neo Large", 
                            "gpt3"="GPT-3"))

keyExps %>% 
  mutate(across(exp, factor, levels=c("Simple Subject","2NP", "3NP", "Object Cataphora"))) %>%
  ggplot(aes(x=modelSize, y=abs(mean), label=model)) +
  geom_text_repel(size=4.5)+
  expand_limits(x=100)+
  ylim(0, 1.2) +
  geom_point() + 
  coord_trans(x="log10")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  labs(y="Gender Mismatch Effect (|bits|)", x="Number of Parameters (millons)", title="Gender Mismatch Effect by Model Size and Experiment") + 
  theme(text = element_text(size=20))+
  facet_wrap(~exp, ncol=4)

ggsave(paste(path,"figures/Scale.png", sep=""), width =15)

####################
#####  Counts ######
####################
pileCounts <- read.csv(paste(path, "results/PilePronounCounts.csv", sep=''))

pileCounts %>% 
  mutate(gender = recode(gender, "m"="Masc",
                          "f"="Fem")) %>% filter(!pronoun %in% c("himself", "herself")) %>%
  mutate(across(pronoun, factor, levels=c("him", "his", "her", "her_poss"))) %>%
  mutate(pronoun = recode(pronoun, "her_poss"="her (poss)", 
                          "her"="her (obj)")) %>%
  ggplot(aes(x=position, y=proportion, fill=gender)) +
  theme(text = element_text(size=20)) + 
  scale_fill_manual(values=c('mediumpurple3', 
                             "#E69F00"))+
  scale_x_discrete(limits = c("sole", "first", "last"))+
  labs(y="Antecedent Gender Proportion", x='Noun in Sentence with Pronoun',title="Gendered Antecedents for Pronouns by Position", fill="Antecedent\nGender") +
  geom_bar(stat="identity",alpha=0.7) +
  facet_wrap(~pronoun)

ggsave(paste(path,"figures/PileProps.png", sep=""))

######################
#####  RT Fit    #####
######################

#Load predicted reading times (based on coef in stats.R)
neo_RT <- read.csv(paste(path, "results/RT_neo.csv", sep=''))
j_RT <- read.csv(paste(path, "results/RT_j.csv", sep=""))

#Filter to look at just exp conditions
conditions <- c("CATAMARAN2-FIN-Match",
                "CATAMARAN2-FIN-Misma",
                "CATAMARAN2-OBJ-Match",
                "CATAMARAN2-OBJ-Misma")

neo_RT <- neo_RT %>% filter(cond %in% conditions)
j_RT <- j_RT %>% filter(cond %in% conditions) 

##Neo
FinMatch <- neo_RT %>% filter(cond == "CATAMARAN2-FIN-Match")
FinMatch <- FinMatch[order(FinMatch$item, FinMatch$region),]
FinMismatch <- neo_RT %>% filter(cond == "CATAMARAN2-FIN-Misma")
FinMismatch <- FinMismatch[order(FinMismatch$item, FinMismatch$region),]

FinMatch <- FinMatch %>% filter(region==7)
FinMismatch <- FinMismatch %>% filter(region==7)

NeoFinGMME <- mean(FinMismatch$predRT - FinMatch$predRT)

ObjMatch <- neo_RT %>% filter(cond == "CATAMARAN2-OBJ-Match")
ObjMatch <- ObjMatch[order(ObjMatch$item, ObjMatch$region),]
ObjMismatch <- neo_RT %>% filter(cond == "CATAMARAN2-OBJ-Misma")
ObjMismatch <- ObjMismatch[order(ObjMismatch$item, ObjMismatch$region),]

ObjMatch <- ObjMatch %>% filter(region==7)
ObjMismatch <- ObjMismatch %>% filter(region==7)

NeoObjGMME <- mean(ObjMismatch$predRT - ObjMatch$predRT)

###GPTJ
FinMatch <- j_RT %>% filter(cond == "CATAMARAN2-FIN-Match")
FinMatch <- FinMatch[order(FinMatch$item, FinMatch$region),]
FinMismatch <- j_RT %>% filter(cond == "CATAMARAN2-FIN-Misma")
FinMismatch <- FinMismatch[order(FinMismatch$item, FinMismatch$region),]

FinMatch <- FinMatch %>% filter(region==7)
FinMismatch <- FinMismatch %>% filter(region==7)

jFinGMME <- mean(FinMismatch$predRT - FinMatch$predRT)

ObjMatch <- j_RT %>% filter(cond == "CATAMARAN2-OBJ-Match")
ObjMatch <- ObjMatch[order(ObjMatch$item, ObjMatch$region),]
ObjMismatch <- j_RT %>% filter(cond == "CATAMARAN2-OBJ-Misma")
ObjMismatch <- ObjMismatch[order(ObjMismatch$item, ObjMismatch$region),]

ObjMatch <- ObjMatch %>% filter(region==7)
ObjMismatch <- ObjMismatch %>% filter(region==7)

jObjGMME <- mean(ObjMismatch$predRT - ObjMatch$predRT)

new_data <- data.frame("model"= c("Human", "Human", "GPT-Neo", "GPT-Neo", "GPT-J", "GPT-J"), 
                       "exp" = c("Principle B", "No Principle B", "Principle B", "No Principle B", "Principle B", "No Principle B"), 
                       "RT" = c(-21, 63, NeoObjGMME, NeoFinGMME, jObjGMME, jFinGMME ))

new_data  %>%   mutate(across(model, factor, levels=c("Human","GPT-Neo", "GPT-J"))) %>%
      ggplot(aes(x=exp,y=RT))+
        geom_point(shape = 21,fill = "mediumpurple3", size = 8, stroke = 2)+
        ylim(-30, 70)+
        theme(text = element_text(size=20)) + 
        labs(y="Gender Mismatch Effect RT (ms)",x="Experimental Condition",title='Gender Mismatch Effect for Object Cataphora') + 
        geom_hline(yintercept=0,linetype=2) + 
        facet_wrap(~model)

ggsave(paste(path,"figures/RTComparison.png", sep=""), width =10)
