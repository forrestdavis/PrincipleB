library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyverse)
library(ggrepel)

##Bind
path = '/Users/forrestdavis/Projects/PrincipleB/'

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

exp2GMME <- exp2_him

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
  mutate(pronoun = recode(pronoun, "obj"="Object")) %>%
  ggplot(aes(x=contrast, y=mean)) +
  scale_x_discrete(limits = c("Matrix", "Embedded"))+
  labs(y="Gender Mismatch Effect (bits)", x='Position in Sentence',title="Gender Mismatch Effect by Model and Position", fill="Pronoun") +
  geom_bar(position=position_dodge(), stat="identity", alpha=0.7, fill="mediumpurple3") +
  ylim(-1.5, 5) + theme(text = element_text(size=20)) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9)  +
  facet_wrap(~model)

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

#Just no principle b case (for main slides)

new_data %>% filter(exp != 'Principle B')  %>%   mutate(across(model, factor, levels=c("Human","GPT-Neo", "GPT-J"))) %>%
  ggplot(aes(x=exp,y=RT))+
  geom_point(shape = 21,fill = "mediumpurple3", size = 8, stroke = 2)+
  ylim(-30, 70)+
  theme(text = element_text(size=20)) + 
  labs(y="Gender Mismatch Effect RT (ms)",x="Experimental Condition",title='Gender Mismatch Effect for Object Cataphora') + 
  geom_hline(yintercept=0,linetype=2) + 
  facet_wrap(~model)

#Appendix plot
new_data  %>%   mutate(across(model, factor, levels=c("Human","GPT-Neo", "GPT-J"))) %>%
  ggplot(aes(x=exp,y=RT))+
  geom_point(shape = 21,fill = "mediumpurple3", size = 8, stroke = 2)+
  ylim(-30, 70)+
  theme(text = element_text(size=20)) + 
  labs(y="Gender Mismatch Effect RT (ms)",x="Experimental Condition",title='Gender Mismatch Effect for Object Cataphora') + 
  geom_hline(yintercept=0,linetype=2) + 
  facet_wrap(~model)

exp7 <- read.csv(paste(path, "results/PrincipleB_Exp7_combined.csv", sep=''))

exp7_Match <- exp7 %>% filter(contrast=="Match")
exp7_Mismatch <- exp7 %>% filter(contrast!="Match")

exp7_GMME <- exp7_Mismatch$surp - exp7_Match$surp

exp7GMME <- exp7_Mismatch %>% select(item, cond, adjunct, contrast, sent, target, model)

if(length(exp7GMME$contrast) != length(exp7_GMME)){
  print("Error Stop!!!!!!!!!!")
}else{
  exp7GMME$GMME = exp7_GMME 
}

exp7GMME_sum <- exp7GMME %>%
  group_by(adjunct, model) %>%
  summarise( 
    n=n(),
    mean=mean(GMME),
    sd=sd(GMME)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1))

exp7GMME_sum %>%  filter(!model %in% c('gpt-neo-1.3B', 'gpt-neo-125M', 'gpt2', 'gpt2-medium', 'gpt2-large')) %>% 
  mutate(across(model, factor, levels=c("gpt2-xl","gpt-neo-2.7B", "gpt-j-6B", "gpt3"))) %>%
  mutate(model = recode(model, "gpt2-xl"="GPT-2 XL", 
                        "gpt-neo-2.7B"="GPT-Neo",
                        "gpt-j-6B"="GPT-J", "gpt3"="GPT-3")) %>%
  ggplot(aes(x=adjunct, y=mean)) +
  scale_x_discrete(breaks=c("No Constraint", "Constraint", "Reflexive"),
                   labels=c("No Principle B", "Principle B", "Reflexive"))+
  labs(y="Gender Mismatch Effect (bits)", x='Experiment Condition', title="Gender Mismatch Effect for Object Cataphora") +
  geom_bar(position=position_dodge(), stat="identity",  fill="mediumpurple3",alpha=0.7) +
  ylim(-1.5, 7) + theme(text = element_text(size=20)) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position=position_dodge(.9), width=0.08, colour="black", alpha=0.9, size=.9)  +
  facet_wrap(~model)

ggsave(paste(path,"figures/Exp7.png", sep=""))