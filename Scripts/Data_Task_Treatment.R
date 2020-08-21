######Install and/or load packages######

if(!require(pacman)){install.packages("pacman")} # Install the package in case it was not installed
library(pacman) #This package ("pacman") serves to install and load other packages simultaneously
p_load(readxl, rockchalk, plyr, Hmisc, tidyverse) #Installing and loading the rest of the packages

##### Set data filters #####

filterRTmin = 200 #Exclude responses below 200 ms
filterRTmax = 1500 #Exclude responses above 1500 ms
experimentalblocks = c(1:6) #if necessary, to exclude data after n experimental block

##### Load data file #####

data_raw <- read_excel("./Inputs/Data_ANTI-VeaD.xlsx", sheet = "Raw_Data") # Search from the PROJECT's working directory (the dot)
data_processed = data_raw %>% #data_processed will be the base where the relevant task indexes are included
  distinct(Subject) #Numbers with no repetition, only taking into account, the $Subject column

#####Identify outliers participants######

#Outliers participants#

  #General task: Participants with more than 25% errors in ANTI trials

x = data_raw %>%
  filter (BlockList %in% experimentalblocks, `Vigilancia[Trial]`=="NoVigilancia") %>% 
  group_by(Subject) %>% #This makes summaries to conduct descriptives at the subject's level
  summarise(Target.Error=mean(Target.Error))
outliers_performance = x %>% #This object represents the subjects that have been excluded
  filter (Target.Error > .25) %>% 
  select(Subject) %>% 
  pull() # Make the output a value (single vector), and not a structure of data
n_excluded_bcperformance = length(outliers_performance) # This object is the number of subjects that have been excluded

x = x %>% 
  transmute(Validity_GeneralTask=ifelse(Target.Error > .25, "No","Yes"))
data_processed = cbind(data_processed,x)

  #Irrelevant distractor trials# Participants with more than 25% errors in distractor present condition

x = data_raw %>% 
  filter (BlockList %in% experimentalblocks, `Vigilancia[Trial]`=="DP") %>% 
  group_by(Subject) %>% 
  summarise(Target.Error=mean(Target.Error))
outliers_performance_IDtrials = x %>% #This object represents the subjects that have been excluded
  filter(Target.Error > .25) %>% 
  select(Subject) %>% 
  pull()
n_excluded_bcperformance_IDtrials = length(outliers_performance_IDtrials)# This object is the number of subjects that have been excluded

x = x %>% 
  transmute(Validity_IDtrials=ifelse(Target.Error > .25, "No","Yes"))
data_processed = cbind(data_processed,x)


rm(x)

#####Identify outliers trials######

  #ANTI RT#

x = data_raw %>% 
  filter(!Subject %in% outliers_performance) %>%
  filter(BlockList %in% experimentalblocks, `Vigilancia[Trial]`=="NoVigilancia")
filterRT_percentage_errors = x %>% # Percentage of incorrect trials (outlier subjects excluded)
  summarise(round(100*sum(!Target.ACC == 1)/sum(Target.ACC == 1), digits = 4)) %>% 
  pull()
filterRT_percentage_time = x %>% # Percentage of trials excluded after RT filter (outlier subjects excluded)
  summarise(round(100*sum(!(x$Target.RT > (filterRTmin) & x$Target.RT < filterRTmax))/sum(x$Target.RT > (filterRTmin) & x$Target.RT < filterRTmax), digits=4)) %>% 
  pull()
rm(x)

  #ID RT#

x = data_raw %>% 
  filter(!Subject %in% c(outliers_performance, outliers_performance_IDtrials)) %>%
  filter(BlockList %in% experimentalblocks, `Vigilancia[Trial]`=="DP")
filterRT_percentage_errors_ID = x %>% # Percentage of incorrect trials (outlier subjects excluded)
  summarise(round(100*sum(!Target.ACC == 1)/sum(Target.ACC == 1), digits = 4)) %>% 
  pull()
filterRT_percentage_time_ID = x %>% # Percentage of trials excluded after RT filter (outlier subjects excluded)
  summarise(round(100*sum(!(x$Target.RT > (filterRTmin) & x$Target.RT < filterRTmax))/sum(x$Target.RT > (filterRTmin) & x$Target.RT < filterRTmax), digits=4)) %>% 
  pull()
rm(x)

#### ANTI Trials ####

# RT analysis of ANTI trials #

# Select appropriate trials (Only valid subjects, ANTI trials, only experimental blocks drop errors, filter RT) #

data_rt = data_raw %>% 
  filter(BlockList %in% experimentalblocks, `Vigilancia[Trial]`=="NoVigilancia", Target.ACC==1, Target.RT>filterRTmin, Target.RT<filterRTmax)
  
# Generate data table for RT analyses #

data_rt_table = data_rt %>% # Table with the relevant outcomes for task check
  group_by(Subject, `Tono[Trial]`, `ValidezClave[Trial]`, `Congruency[Trial]`) %>% 
  summarise(Target.RT=mean(Target.RT)) %>% 
  mutate(Condition = paste(`Tono[Trial]`,`ValidezClave[Trial]`,`Congruency[Trial]`, sep = "_"))
data_rt_table = as.data.frame(data_rt_table) %>% 
  reshape(idvar = "Subject", timevar = "Condition", drop = c("Tono[Trial]", "ValidezClave[Trial]", "Congruency[Trial]"), direction = "wide")

# Compute attentional scores #

#Overall

x = as.data.frame(data_rt) %>% 
  group_by(Subject) %>% 
  summarise(Overall_RT=mean(Target.RT))
data_processed=bind_cols(data_processed,x[2])

#Alerting
x = as.data.frame(data_rt) %>% 
  group_by(Subject, `Tono[Trial]`) %>% 
  summarise(Target.RT=mean(Target.RT)) %>%  
  spread(`Tono[Trial]`, Target.RT) %>% 
  mutate(Alerting_RT = notono - tono) %>%
  rename(notono_RT=notono,tono_RT=tono) 
data_rt_table=bind_cols(data_rt_table,x[2:3])
data_processed=bind_cols(data_processed,x[4])

#Orienting
x = as.data.frame(data_rt) %>% 
  group_by(Subject, `ValidezClave[Trial]`) %>% 
  summarise(Target.RT=mean(Target.RT)) %>%  
  spread(`ValidezClave[Trial]`, Target.RT) %>% 
  mutate(Orienting_RT = invalida - valida) %>%
  rename(invalid_RT=invalida,nocue_RT=nocue,valid_RT=valida)
data_rt_table=bind_cols(data_rt_table,x[2:4])
data_processed=bind_cols(data_processed,x[5])

#Congruency
x = as.data.frame(data_rt) %>% 
  group_by(Subject, `Congruency[Trial]`) %>% 
  summarise(Target.RT=mean(Target.RT)) %>%  
  spread(`Congruency[Trial]`, Target.RT) %>% 
  mutate(Congruency_RT = incongruent - congruent) %>%
  rename(incongruent_RT=incongruent,congruent_RT=congruent)
data_rt_table=bind_cols(data_rt_table,x[2:3])
data_processed=bind_cols(data_processed,x[4])

rm(x)

data_rt_table = data_rt_table %>% #Removing outliers from task analyses of ANTI RT trials
  filter(!Subject %in% outliers_performance)

# ACC analysis of ANTI trials #

# Select appropriate trials (ANTI task, NOT drop first block) #

data_acc = data_raw %>% 
  filter(BlockList %in% experimentalblocks, `Vigilancia[Trial]`=="NoVigilancia")

# Generate data table for ACC (percentage errors) analysis #

data_acc_table = data_acc %>% # Table with the relevant outcomes for task check
  group_by(Subject, `Tono[Trial]`, `ValidezClave[Trial]`, `Congruency[Trial]`) %>% 
  summarise(Target.ACC=(1-mean(Target.ACC))*100) %>%
  mutate(Condition = paste(`Tono[Trial]`,`ValidezClave[Trial]`,`Congruency[Trial]`, sep = "_"))
data_acc_table = as.data.frame(data_acc_table) %>%
  reshape(idvar = "Subject", timevar = "Condition", drop = c("Tono[Trial]", "ValidezClave[Trial]", "Congruency[Trial]"), direction = "wide")

# Compute attentional scores #

#Overall
x = as.data.frame(data_acc) %>% 
  group_by(Subject) %>% 
  summarise(Overall_ACC=(1-mean(Target.ACC))*100)
data_processed=bind_cols(data_processed,x[2])

#Alerting
x = as.data.frame(data_acc) %>% 
  group_by(Subject, `Tono[Trial]`) %>% 
  summarise(Target.ACC=(1-mean(Target.ACC))*100) %>%  
  spread(`Tono[Trial]`, Target.ACC) %>% 
  mutate(Alerting_ACC = notono - tono) %>%
  rename(notono_ACC=notono,tono_ACC=tono)
data_acc_table=bind_cols(data_acc_table,x[2:3])
data_processed=bind_cols(data_processed,x[4])

#Orienting
x = as.data.frame(data_acc) %>% 
  group_by(Subject, `ValidezClave[Trial]`) %>% 
  summarise(Target.ACC=(1-mean(Target.ACC))*100) %>%  
  spread(`ValidezClave[Trial]`, Target.ACC) %>% 
  mutate(Orienting_ACC = invalida - valida) %>%
  rename(invalid_ACC=invalida,nocue_ACC=nocue,valid_ACC=valida)
data_acc_table=bind_cols(data_acc_table,x[2:4])
data_processed=bind_cols(data_processed,x[5])

#Congruency
x = as.data.frame(data_acc) %>% 
  group_by(Subject, `Congruency[Trial]`) %>% 
  summarise(Target.ACC=(1-mean(Target.ACC))*100) %>%  
  spread(`Congruency[Trial]`, Target.ACC) %>% 
  mutate(Congruency_ACC = incongruent - congruent) %>%
  rename(incongruent_ACC=incongruent,congruent_ACC=congruent)
data_acc_table=bind_cols(data_acc_table,x[2:3])
data_processed=bind_cols(data_processed,x[4])

rm(x)

data_acc_table = data_acc_table %>% #Removing outliers from task analyses of ANTI percentage errors trials
  filter(!Subject %in% outliers_performance)
  
#### EV Trials ####

# Select appropriate trials (NOT drop first block) #

data_EV = data_raw %>% 
  filter(BlockList %in% experimentalblocks)
data_EV_table = data_EV %>% # Executive Vigilance scores per block for performance over time analyses
  distinct(Subject)

# Define functions for A' and B''#

Aprime = function(Hits_Percentage, FA_Percentage)
{
  ifelse(Hits_Percentage/100 < FA_Percentage/100 | Hits_Percentage/100 == 0,
         0.5,
         0.5 + (((Hits_Percentage/100 - FA_Percentage/100)*(1 + Hits_Percentage/100 - FA_Percentage/100)) / (4*(Hits_Percentage/100)*(1 - (FA_Percentage/100)))))
}

Bprime = function(Hits_Percentage, FA_Percentage)
{
  ifelse(Hits_Percentage/100==0 & FA_Percentage/100==0,
         1,
         ifelse(Hits_Percentage/100==1,
                -1,
                (((Hits_Percentage/100) * (1 - Hits_Percentage/100)) - ((FA_Percentage/100) * (1 - FA_Percentage/100))) / (((Hits_Percentage/100) * (1 - Hits_Percentage/100)) + ((FA_Percentage/100) * (1 - (FA_Percentage/100)))))) 
}

# Compute EV scores #

#Hits (percentage)
x = data_EV %>% 
  filter(`Vigilancia[Trial]` =="VE") %>% 
  group_by(Subject) %>% 
  summarise(Hits_Percentage=mean(Target.ACC)*100)
data_processed=bind_cols(data_processed,x[2])

#False Alarms (percentage)  
x = data_EV %>% 
  filter(Trial_FA_Difficult =="YES") %>% 
  group_by(Subject) %>% 
  summarise(FA_Percentage=mean(FA_Difficult)*100) 
data_processed=bind_cols(data_processed,x[2])

#A' (sensitivity)
data_processed = data_processed %>%
  mutate(Ap=Aprime(Hits_Percentage,FA_Percentage))

#B'' (response bias)
data_processed = data_processed %>%
  mutate(Bp=Bprime(Hits_Percentage,FA_Percentage))

rm(x)

#Calculating EV Slopes#

#Hits slope
x = data_EV %>% 
  filter(`Vigilancia[Trial]` =="VE") %>% 
  group_by(Subject,BlockList) %>% 
  summarise(Hits_Percentage=mean(Target.ACC)*100) %>% 
  spread(BlockList,Hits_Percentage) %>% 
  mutate(Hits_Slope = coef(lm(c(`1`,`2`,`3`,`4`,`5`,`6`) ~ experimentalblocks))[2]) %>% 
  rename_at(vars(`1`:`6`) ,function(x){paste0("EV_Hits_B", x)})
data_EV_table=bind_cols(data_EV_table,x[2:7])
data_processed=bind_cols(data_processed,x[8])

#False alarms slope
x = data_EV %>% 
  filter(Trial_FA_Difficult =="YES") %>% 
  group_by(Subject,BlockList) %>% 
  summarise(FA_Percentage=mean(FA_Difficult)*100) %>% 
  spread(BlockList,FA_Percentage) %>% 
  mutate(FA_Slope = coef(lm(c(`1`,`2`,`3`,`4`,`5`,`6`) ~ experimentalblocks))[2]) %>% 
  rename_at(vars(`1`:`6`) ,function(x){paste0("EV_FA_B", x)})
data_EV_table=bind_cols(data_EV_table,x[2:7])
data_processed=bind_cols(data_processed,x[8])

#A' (sensitivity) slope
x = data_EV %>% 
  group_by(Subject,BlockList) %>% 
  summarise(Hits_Percentage=mean(Target.ACC[which(`Vigilancia[Trial]` =="VE")])*100, FA_Percentage=mean(FA_Difficult[which(Trial_FA_Difficult=="YES")])*100) %>% 
  mutate(Ap=Aprime(Hits_Percentage,FA_Percentage)) %>% 
  select(-Hits_Percentage,-FA_Percentage) %>% 
  spread(BlockList,Ap) %>% 
  mutate(Ap_Slope = coef(lm(c(`1`,`2`,`3`,`4`,`5`,`6`) ~ experimentalblocks))[2]) %>% 
  rename_at(vars(`1`:`6`) ,function(x){paste0("EV_Ap_B", x)})
data_EV_table=bind_cols(data_EV_table,x[2:7])
data_processed=bind_cols(data_processed,x[8])

#B'' (response bias) slope
x = data_EV %>% 
  group_by(Subject,BlockList) %>% 
  summarise(Hits_Percentage=mean(Target.ACC[which(`Vigilancia[Trial]` =="VE")])*100, FA_Percentage=mean(FA_Difficult[which(Trial_FA_Difficult=="YES")])*100) %>% 
  mutate(Bp=Bprime(Hits_Percentage,FA_Percentage)) %>% 
  select(-Hits_Percentage,-FA_Percentage) %>% 
  spread(BlockList,Bp) %>% 
  mutate(Bp_Slope = coef(lm(c(`1`,`2`,`3`,`4`,`5`,`6`) ~ experimentalblocks))[2]) %>% 
  rename_at(vars(`1`:`6`) ,function(x){paste0("EV_Bp_B", x)})
data_EV_table=bind_cols(data_EV_table,x[2:7])
data_processed=bind_cols(data_processed,x[8])

rm(x)

data_EV_table = data_EV_table %>% #Removing outliers from task analyses of EV performance over time
  filter(!Subject %in% outliers_performance)

#### AV Trials ####

# Select appropriate trials (NOT drop first block) #

data_AV = data_raw %>% 
  filter(BlockList %in% experimentalblocks, `Vigilancia[Trial]` =="VA")
data_AV_table = data_AV %>% # Arousal Vigilance scores per block for performance over time analyses
  distinct(Subject)

# Compute AV scores #

#Mean

x = as.data.frame(data_AV) %>% 
  group_by(Subject) %>% 
  summarise(AV_Mean=mean(TargetVA.RT))
data_processed=bind_cols(data_processed,x[2])

#SD
x = as.data.frame(data_AV) %>% 
  group_by(Subject) %>% 
  summarise(AV_SD=sd(TargetVA.RT))
data_processed=bind_cols(data_processed,x[2])

#Percentage lapses

x = as.data.frame(data_AV) %>% 
  group_by(Subject) %>% 
  summarise(AV_Lapses=mean(Lapsus_Juan)*100)
data_processed=bind_cols(data_processed,x[2])

rm(x)

#Calculate AV slopes#

#Mean slope

x = data_AV %>% 
  group_by(Subject,BlockList) %>% 
  summarise(AV_Mean=mean(TargetVA.RT)) %>% 
  spread(BlockList,AV_Mean) %>% 
  mutate(AV_Mean_Slope = coef(lm(c(`1`,`2`,`3`,`4`,`5`,`6`) ~ experimentalblocks))[2]) %>% 
  rename_at(vars(`1`:`6`) ,function(x){paste0("AV_Mean_B", x)})
data_AV_table=bind_cols(data_AV_table,x[2:7])
data_processed=bind_cols(data_processed,x[8])

#SD

x = data_AV %>% 
  group_by(Subject,BlockList) %>% 
  summarise(AV_SD=sd(TargetVA.RT)) %>% 
  spread(BlockList,AV_SD) %>% 
  mutate(AV_SD_Slope = coef(lm(c(`1`,`2`,`3`,`4`,`5`,`6`) ~ experimentalblocks))[2]) %>% 
  rename_at(vars(`1`:`6`) ,function(x){paste0("AV_SD_B", x)})
data_AV_table=bind_cols(data_AV_table,x[2:7])
data_processed=bind_cols(data_processed,x[8])

#Lapses

x = data_AV %>% 
  group_by(Subject,BlockList) %>% 
  summarise(AV_Lapses=mean(Lapsus_Juan)) %>% 
  spread(BlockList,AV_Lapses) %>% 
  mutate(AV_Lapses_Slope = coef(lm(c(`1`,`2`,`3`,`4`,`5`,`6`) ~ experimentalblocks))[2]) %>% 
  rename_at(vars(`1`:`6`) ,function(x){paste0("AV_Lapses_B", x)})
data_AV_table=bind_cols(data_AV_table,x[2:7])
data_processed=bind_cols(data_processed,x[8])

rm(x)

#####Irrelevant distractor######

##RT##

#RT pre-analysis

DT = c("DP", "DA")

data_ID_RT = data_raw[which(data_raw$`Vigilancia[Trial]` %in% DT),]
data_ID_RT = data_ID_RT[which(data_ID_RT$BlockList %in% experimentalblocks),]

filterRT_ID_percentage_errors = round(100*sum(!(data_ID_RT$Target.ACC == 1))/sum(data_ID_RT$Target.ACC == 1), digits=4) # Outliers not removed yet at this point

data_ID_RT = data_ID_RT[which(data_ID_RT$Target.ACC==1),]

filterRT_ID_percentage = round(100*sum(!(data_ID_RT$Target.RT > (filterRTmin) & data_ID_RT$Target.RT < filterRTmax))/sum(data_ID_RT$Target.RT > (filterRTmin) & data_ID_RT$Target.RT < filterRTmax), digits=4) # Outliers not removed yet at this point

data_ID_RT = data_ID_RT[which(data_ID_RT$Target.RT > (filterRTmin) & data_ID_RT$Target.RT < filterRTmax),]

#RT analysis (Z-scores)

data_ID_RT_table = aggregate(Target.RT ~ LugarDistractortoTarget + Subject, data_ID_RT, FUN = mean)
data_ID_RT_table = reshape(data_ID_RT_table, idvar = "Subject", timevar = "LugarDistractortoTarget", direction = "wide")
data_ID_RT_table[is.na(data_ID_RT_table)] = 0
data_ID_RT_table = cbind (data_ID_RT_table, IDZOpposite = (data_ID_RT_table$Target.RT.opposite - data_ID_RT_table$Target.RT.none) / data_ID_RT_table$Target.RT.none)
data_ID_RT_table = cbind (data_ID_RT_table, IDZSame = (data_ID_RT_table$Target.RT.same - data_ID_RT_table$Target.RT.none) / data_ID_RT_table$Target.RT.none)
data_ID_RT_table = cbind (data_ID_RT_table, IDZGeneral = (((data_ID_RT_table$Target.RT.opposite + data_ID_RT_table$Target.RT.same) / 2) - data_ID_RT_table$Target.RT.none) / data_ID_RT_table$Target.RT.none)

##Errors##

#Errors pre-analysis

data_ID_ACC = data_raw[which(data_raw$`Vigilancia[Trial]` %in% DT),]
data_ID_ACC = data_ID_ACC[which(data_ID_ACC$BlockList %in% experimentalblocks),]

#Errors analysis

data_ID_ACC_table = aggregate(Target.ACC ~ LugarDistractortoTarget + Subject, data_ID_ACC, FUN = mean)
data_ID_ACC_table$Target.ACC = 100 * (1 - data_ID_ACC_table$Target.ACC)
data_ID_ACC_table = reshape(data_ID_ACC_table, idvar = "Subject", timevar = "LugarDistractortoTarget", direction = "wide")
data_ID_ACC_table = cbind (data_ID_ACC_table, IDOpposite_Errors = data_ID_ACC_table$Target.ACC.opposite - data_ID_ACC_table$Target.ACC.none)
data_ID_ACC_table = cbind (data_ID_ACC_table, IDSame_Errors = data_ID_ACC_table$Target.ACC.same - data_ID_ACC_table$Target.ACC.none)
data_ID_ACC_table = cbind (data_ID_ACC_table, IDGeneral_Errors = ((data_ID_ACC_table$Target.ACC.opposite + data_ID_ACC_table$Target.ACC.same) / 2) - data_ID_ACC_table$Target.ACC.none)

##### Exclude outliers #####

# 2.5_SD not used in coll-martín_et.al study
#outliers_rt = unique(data_rt_table_wide$Participant[which(abs(scale(data_rt_table_wide$RT_Overall))>filterOutliers)]) # 
#outliers_acc = unique(data_acc_table_wide$Participant[which(abs(scale(data_acc_table_wide$ACC_Overall))>filterOutliers)])
#outliers = outliers_rt
#outliers = append(outliers, outliers_acc)
#outliers = append(outliers, outliers_other)
#outliers = outliers_other
#outliers = outliers[order(outliers)]

outliers_performance = unique(data_acc_table_wide$Subject[which(data_acc_table_wide$ACC_Overall>25)])
outliers_IDtrials_performance = unique(data_ID_ACC_table$Subject[which(data_ID_ACC_table$Target.ACC.none>25 | data_ID_ACC_table$Target.ACC.opposite>25 | data_ID_ACC_table$Target.ACC.same>25)])
outliers_IDtrials_performanceStrict = unique(data_ID_ACC_table$Subject[which(data_ID_ACC_table$Target.ACC.none>10 | data_ID_ACC_table$Target.ACC.opposite>10 | data_ID_ACC_table$Target.ACC.same>10)])

data_rt = data_rt[-which(data_rt$Subject %in% outliers_performance),]
data_rt_table = data_rt_table[-which(data_rt_table$Subject %in% outliers_performance),]
data_rt_table_wide = data_rt_table_wide[-which(data_rt_table_wide$Subject %in% outliers_performance),]

data_acc = data_acc[-which(data_acc$Subject %in% outliers_performance),]
data_acc_table = data_acc_table[-which(data_acc_table$Subject %in% outliers_performance),]
data_acc_table_wide = data_acc_table_wide[-which(data_acc_table_wide$Subject %in% outliers_performance),]

#data_EVig_RT = data_EVig_RT[-which(data_EVig_RT$Subject %in% outliers_performance),]
#data_EVig_RT_table = data_EVig_RT_table[-which(data_EVig_RT_table$Subject %in% outliers_performance),]

data_EVig = data_EVig[-which(data_EVig$Subject %in% outliers_performance),]
data_EVig_table = data_EVig_table[-which(data_EVig_table$Subject %in% outliers_performance),]

data_AVig = data_AVig[-which(data_AVig$Subject %in% outliers_performance),]
data_AVig_table = data_AVig_table[-which(data_AVig_table$Subject %in% outliers_performance),]

data_ID_RT = data_ID_RT[-which(data_ID_RT$Subject %in% outliers_IDtrials_performance),]
data_ID_RT_table = data_ID_RT_table[-which(data_ID_RT_table$Subject %in% outliers_IDtrials_performance),]

data_ID_ACC = data_ID_ACC[-which(data_ID_ACC$Subject %in% outliers_IDtrials_performance),]
data_ID_ACC_table = data_ID_ACC_table[-which(data_ID_ACC_table$Subject %in% outliers_IDtrials_performance),]

##### Reliability analyses ##### 

#Initial version of the script by Mike Lawrence #
#Current version by jroca and fluna #

# Reliability RT scores for ANTI trials #


get_antiRT_scores = function(yy){
  
  xx = aggregate(Target.RT ~ `Tono[Trial]` + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "Tono[Trial]", direction = "wide")
  tone = xx$Target.RT.notono - xx$Target.RT.tono
  
  xx = aggregate(Target.RT ~ `ValidezClave[Trial]` + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "ValidezClave[Trial]", direction = "wide")
  cue = xx$Target.RT.invalida - xx$Target.RT.valida
  
  xx = aggregate(Target.RT ~ `Congruency[Trial]`+ Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "Congruency[Trial]", direction = "wide")
  cong = xx$Target.RT.incongruent - xx$Target.RT.congruent
  
  xx = aggregate(Target.RT ~ Subject, yy, FUN = mean)
  intercept = xx$Target.RT
  
  to_return = data.frame(
    intercept
    , tone
    , cue
    , cong
    , half = ifelse("half"%in%names(yy),yy$half[1],NA)
  )
  row.names(to_return) = NULL
  return(to_return)
}


set.seed(1)
antiRT_rels = ldply(
  .data = 1:1e1
  , .fun = function(iteration){
    scores = ddply(
      .data = data_rt
      , .variables = .(Subject)
      , .fun = function(x){
        x = ddply(
          .data = x
          , .variables = .(`Tono[Trial]`,`ValidezClave[Trial]`,`Congruency[Trial]`)
          , .fun = function(z){
            z$half = ((1:nrow(z))%%2)[order(rnorm(nrow(z)))]
            return(z)
          }
        )
        
        scores_per_half = ddply(
          .data = x
          , .variables = .(half)
          , .fun = get_antiRT_scores
        )
        
        scores_per_half$Subject = x$Subject[1]
        return(scores_per_half)
      }
    )
    
    to_return = data.frame(
      iteration = iteration
      , intercept = with(scores,cor(intercept[half==0],intercept[half==1]))
      , tone = with(scores,cor(tone[half==0],tone[half==1]))
      , cue = with(scores,cor(cue[half==0],cue[half==1]))
      , cong = with(scores,cor(cong[half==0],cong[half==1]))
      , n = nrow(scores)
    )
    return(to_return)
  }
  , .progress = 'time'
)

ldply(
  .data = names(antiRT_rels)[2:ncol(antiRT_rels)]
  , .fun = function(x){
    to_return = data.frame(
      effect = x
    )
    to_return = cbind(
      to_return
      , data.frame(t(quantile(antiRT_rels[,names(antiRT_rels)==x],c(.025,.5, .975))))
    )
    return(to_return)
  }
)

# Reliability ACC scores for ANTI trials #

get_antiACC_scores = function(yy){
  
  xx = aggregate(Target.ACC ~ `Tono[Trial]` + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "Tono[Trial]", direction = "wide")
  tone = xx$Target.ACC.notono - xx$Target.ACC.tono
  
  xx = aggregate(Target.ACC ~ `ValidezClave[Trial]` + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "ValidezClave[Trial]", direction = "wide")
  cue = xx$Target.ACC.invalida - xx$Target.ACC.valida
  
  xx = aggregate(Target.ACC ~ `Congruency[Trial]` + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "Congruency[Trial]", direction = "wide")
  cong = xx$Target.ACC.incongruent - xx$Target.ACC.congruent
  
  xx = aggregate(Target.ACC ~ Subject, yy, FUN = mean)
  intercept = xx$Target.ACC
  
  to_return = data.frame(
    intercept
    , tone
    , cue
    , cong
    , half = ifelse("half"%in%names(yy),yy$half[1],NA)
  )
  row.names(to_return) = NULL
  return(to_return)
}


set.seed(1)
antiACC_rels = ldply(
  .data = 1:1e1
  , .fun = function(iteration){
    scores = ddply(
      .data = data_acc
      , .variables = .(Subject)
      , .fun = function(x){
        x = ddply(
          .data = x
          , .variables = .(`Tono[Trial]`,`ValidezClave[Trial]`,`Congruency[Trial]`)
          , .fun = function(z){
            z$half = ((1:nrow(z))%%2)[order(rnorm(nrow(z)))]
            return(z)
          }
        )
        
        scores_per_half = ddply(
          .data = x
          , .variables = .(half)
          , .fun = get_antiACC_scores
        )
        
        scores_per_half$Subject = x$Subject[1]
        return(scores_per_half)
      }
    )
    
    to_return = data.frame(
      iteration = iteration
      , intercept = with(scores,cor(intercept[half==0],intercept[half==1]))
      , tone = with(scores,cor(tone[half==0],tone[half==1]))
      , cue = with(scores,cor(cue[half==0],cue[half==1]))
      , cong = with(scores,cor(cong[half==0],cong[half==1]))
      , n = nrow(scores)
    )
    return(to_return)
  }
  , .progress = 'time'
)

ldply(
  .data = names(antiACC_rels)[2:ncol(antiACC_rels)]
  , .fun = function(x){
    to_return = data.frame(
      effect = x
    )
    to_return = cbind(
      to_return
      , data.frame(t(quantile(antiACC_rels[,names(antiACC_rels)==x],c(.025,.5, .975))))
    )
    return(to_return)
  }
)

# Reliability Executive Vigilance: mean and SD of RT on hits #
###NOT ACTIVATED IN coll-martin_et.al's edition
#get_ExecutiveVig_RT = function(yy){
  
#  xx = yy[which(yy$Trial_Type=="EV" & yy$Accuracy == 1),]
#  EV_RT = mean(xx$Reaction_Time)
  
#  xx = yy[which(yy$Trial_Type=="EV" & yy$Accuracy == 1),]
#  EV_SD = sd(xx$Reaction_Time)
  
#  to_return = data.frame(
#    EV_RT
#    , EV_SD
#    , half = ifelse("half"%in%names(yy),yy$half[1],NA)
#  )
#  row.names(to_return) = NULL
#  return(to_return)
#}  


#set.seed(1)
#ExecutiveVig_RT_rels = ldply(
#  .data = 1:1e4 
#  , .fun = function(iteration){
#    scores = ddply(
#      .data = data_EVig_RT
#      , .variables = .(Participant)
#      , .fun = function(z){
#        z = ddply(
#          .data = z
#          , .variables = .(Trial_Type)
#          , .fun = function(x){
#            x$half = ((1:nrow(x))%%2)[order(rnorm(nrow(x)))]
#            return(x)
#          }
#        )
        
#        scores_per_half = ddply(
#          .data = z
#          , .variables = .(half)
#          , .fun = get_ExecutiveVig_RT
#        )
        
#        scores_per_half$Participant = z$Participant[1]
#        return(scores_per_half)
#      }
#    )
    
#    to_return = data.frame(
#      iteration = iteration
#      , EV_RT = with(scores,cor(EV_RT[half==0], EV_RT[half==1], use = "complete.obs"))
#      , EV_SD = with(scores,cor(EV_SD[half==0], EV_SD[half==1], use = "complete.obs"))
#      , n = nrow(scores)
#    )
#    return(to_return)
#  }
#  , .progress = 'time'
#)

#ldply(
#  .data = names(ExecutiveVig_RT_rels)[2:ncol(ExecutiveVig_RT_rels)]
#  , .fun = function(x){
#    to_return = data.frame(
#      effect = x
#    )
#    to_return = cbind(
#      to_return
#      , data.frame(t(quantile(ExecutiveVig_RT_rels[,names(ExecutiveVig_RT_rels)==x],c(.025,.5, .975))))
#    )
#    return(to_return)
#  }
#)



# Reliability Executive Vigilance: Hits, False Alarms, #
# and non-parametric (np) indices of sensitivity (A') and response bias (B") #

get_ExecutiveVig_scores = function(yy){
  
  xx = yy[which(yy$`Vigilancia[Trial]`=="VE"),]
  EV_Hrate = mean(xx$Target.ACC)
  
  #xx = yy[which(yy$Trial_Type=="ANTI"),]
  #EV_FArate_Total = mean(xx$FA_Total) #False alarms computed from all noise trials
  
  xx = yy[which(yy$Trial_FA_Difficult=="YES"),]
  EV_FArate_Difficult = mean(xx$FA_Difficult) #False alarms computed from corrected trials
  
  EV_Anp = 
    ifelse(EV_Hrate < EV_FArate_Difficult | EV_Hrate == 0,
           0.5,
           0.5 + (((EV_Hrate - EV_FArate_Difficult)*(1 + EV_Hrate - EV_FArate_Difficult)) / (4*EV_Hrate*(1 - EV_FArate_Difficult))))
  
    
  EV_Bnp = 
    ifelse(EV_Hrate==0 & EV_FArate_Difficult==0,
           1,
           ifelse(EV_Hrate==1,
                  -1,
                  ((EV_Hrate * (1 - EV_Hrate)) - (EV_FArate_Difficult * (1 - EV_FArate_Difficult))) / ((EV_Hrate * (1 - EV_Hrate)) + (EV_FArate_Difficult * (1 - EV_FArate_Difficult)))))
  
  
  
  to_return = data.frame(
    EV_Hrate
#    , EV_FArate_Total
    , EV_FArate_Difficult
    , EV_Anp
    , EV_Bnp
    , half = ifelse("half"%in%names(yy),yy$half[1],NA)
  )
  row.names(to_return) = NULL
  return(to_return)
}  


set.seed(1)
ExecutiveVig_rels = ldply(
  .data = 1:1e1 
  , .fun = function(iteration){
    scores = ddply(
      .data = data_EVig
      , .variables = .(Subject)
      , .fun = function(z){
        z = ddply(
          .data = z
          , .variables = .(`Vigilancia[Trial]`)
          , .fun = function(x){
            x$half = ((1:nrow(x))%%2)[order(rnorm(nrow(x)))]
            return(x)
          }
        )
        
        scores_per_half = ddply(
          .data = z
          , .variables = .(half)
          , .fun = get_ExecutiveVig_scores
        )
        
        scores_per_half$Subject = z$Subject[1]
        return(scores_per_half)
      }
    )
    
    to_return = data.frame(
      iteration = iteration
      , EV_Hrate = with(scores,cor(EV_Hrate[half==0],EV_Hrate[half==1], use = "complete.obs"))
#      , EV_FArate_Total = with(scores,cor(EV_FArate_Total[half==0],EV_FArate_Total[half==1], use = "complete.obs"))
      , EV_FArate_Difficult = with(scores,cor(EV_FArate_Difficult[half==0],EV_FArate_Difficult[half==1], use = "complete.obs"))      
      , EV_Anp = with(scores,cor(EV_Anp[half==0],EV_Anp[half==1], use = "complete.obs"))
      , EV_Bnp = with(scores,cor(EV_Bnp[half==0],EV_Bnp[half==1], use = "complete.obs"))
      , n = nrow(scores)
    )
    return(to_return)
  }
  , .progress = 'time'
)

ldply(
  .data = names(ExecutiveVig_rels)[2:ncol(ExecutiveVig_rels)]
  , .fun = function(x){
    to_return = data.frame(
      effect = x
    )
    to_return = cbind(
      to_return
      , data.frame(t(quantile(ExecutiveVig_rels[,names(ExecutiveVig_rels)==x],c(.025,.5, .975))))
    )
    return(to_return)
  }
)


#############

#EV Slopes

#Hits
get_ExecutiveVig_scores_SL = function(yy)
{
xx = yy[which(yy$`Vigilancia[Trial]`=="VE"),]
for (i in experimentalblocks)
{
  zz = mean(xx$Target.ACC[xx$BlockList==i])
  assign(paste0("EV_Hrate_B",i), zz)
  rm(zz)
}
rBlocks_EV_Hrate = c(EV_Hrate_B1, EV_Hrate_B2, EV_Hrate_B3, EV_Hrate_B4, EV_Hrate_B5, EV_Hrate_B6)
rgSL_EV_Hrate = lm(rBlocks_EV_Hrate ~ experimentalblocks)
rSL_EV_Hrate = coef(rgSL_EV_Hrate)[[2]]

#False alarms 

xx = yy[which(yy$Trial_FA_Difficult=="YES"),]
for (i in experimentalblocks)
{
  zz = mean(xx$FA_Difficult[xx$BlockList==i])
  assign(paste0("EV_FArate_B",i), zz)
  rm(zz) 
}
rBlocks_EV_FArate = c(EV_FArate_B1, EV_FArate_B2, EV_FArate_B3, EV_FArate_B4, EV_FArate_B5, EV_FArate_B6)
rgSL_EV_FArate = lm(rBlocks_EV_FArate ~ experimentalblocks)
rSL_EV_FArate = coef(rgSL_EV_FArate)[[2]]

#A'

for (i in experimentalblocks)
{
  zz =
    ifelse(rBlocks_EV_Hrate[i] < rBlocks_EV_FArate[i] | rBlocks_EV_Hrate[i] == 0,
           0.5,
           0.5 + (((rBlocks_EV_Hrate[i] - rBlocks_EV_FArate[i])*(1 + rBlocks_EV_Hrate[i] - rBlocks_EV_FArate[i])) / (4*rBlocks_EV_Hrate[i]*(1 - rBlocks_EV_FArate[i]))))
  assign(paste0("EV_Aprate_B",i), zz)
  rm(zz)
}
rBlocks_EV_Aprate = c(EV_Aprate_B1, EV_Aprate_B2, EV_Aprate_B3, EV_Aprate_B4, EV_Aprate_B5, EV_Aprate_B6)
rgSL_EV_Aprate = lm(rBlocks_EV_Aprate ~ experimentalblocks)
rSL_EV_Ap = coef(rgSL_EV_Aprate)[[2]]

#B''

for (i in experimentalblocks)
{
  zz =
    ifelse(rBlocks_EV_Hrate[i]==0 & rBlocks_EV_FArate[i]==0,
           1,
           ifelse(rBlocks_EV_Hrate[i]==1,
                  -1,
                  ((rBlocks_EV_Hrate[i] * (1 - rBlocks_EV_Hrate[i])) - (rBlocks_EV_FArate[i] * (1 - rBlocks_EV_FArate[i]))) / ((rBlocks_EV_Hrate[i] * (1 - rBlocks_EV_Hrate[i])) + (rBlocks_EV_FArate[i] * (1 - rBlocks_EV_FArate[i])))))
  assign(paste0("EV_Bprate_B",i), zz)
  rm(zz)
}
rBlocks_EV_Bprate = c(EV_Bprate_B1, EV_Bprate_B2, EV_Bprate_B3, EV_Bprate_B4, EV_Bprate_B5, EV_Bprate_B6)
rgSL_EV_Bprate = lm(rBlocks_EV_Bprate ~ experimentalblocks)
rSL_EV_Bp = coef(rgSL_EV_Bprate)[[2]]

#

  to_return = data.frame(
    rSL_EV_Hrate
    #    , EV_FArate_Total
    , rSL_EV_FArate
    , rSL_EV_Ap
    , rSL_EV_Bp
    , half = ifelse("half"%in%names(yy),yy$half[1],NA)
  )
  row.names(to_return) = NULL
  return(to_return)
}


set.seed(1)
ExecutiveVig_SL_rels = ldply(
  .data = 1:1e1 
  , .fun = function(iteration){
    scores = ddply(
      .data = data_EVig
      , .variables = .(Subject)
      , .fun = function(z){
        z = ddply(
          .data = z
          , .variables = .(`Vigilancia[Trial]`)
          , .fun = function(x){
            x$half = ((1:nrow(x))%%2)[order(rnorm(nrow(x)))]
            return(x)
          }
        )
        
        scores_per_half = ddply(
          .data = z
          , .variables = .(half)
          , .fun = get_ExecutiveVig_scores_SL
        )
        
        scores_per_half$Subject = z$Subject[1]
        return(scores_per_half)
      }
    )
    
    to_return = data.frame(
      iteration = iteration
      , rSL_EV_Hrate = with(scores,cor(rSL_EV_Hrate[half==0],rSL_EV_Hrate[half==1], use = "complete.obs"))
      #      , EV_FArate_Total = with(scores,cor(EV_FArate_Total[half==0],EV_FArate_Total[half==1], use = "complete.obs"))
      , rSL_EV_FArate = with(scores,cor(rSL_EV_FArate[half==0],rSL_EV_FArate[half==1], use = "complete.obs"))      
      , rSL_EV_Ap = with(scores,cor(rSL_EV_Ap[half==0],rSL_EV_Ap[half==1], use = "complete.obs"))
      , rSL_EV_Bp = with(scores,cor(rSL_EV_Bp[half==0],rSL_EV_Bp[half==1], use = "complete.obs"))
      , n = nrow(scores)
    )
    return(to_return)
  }
  , .progress = 'time'
)

ldply(
  .data = names(ExecutiveVig_SL_rels)[2:ncol(ExecutiveVig_SL_rels)]
  , .fun = function(x){
    to_return = data.frame(
      effect = x
    )
    to_return = cbind(
      to_return
      , data.frame(t(quantile(ExecutiveVig_SL_rels[,names(ExecutiveVig_SL_rels)==x],c(.025,.5, .975))))
    )
    return(to_return)
  }
)


# Reliability for Arousal Vigilance scores #

get_ArousalVig_scores = function(yy){
  
  xx = yy[which(yy$`Vigilancia[Trial]`=="VA"),]
  AV_RT = mean(xx$TargetVA.RT)
  
  xx = yy[which(yy$`Vigilancia[Trial]`=="VA"),]
  AV_SD = sd(xx$TargetVA.RT)
  
  xx = yy[which(yy$`Vigilancia[Trial]`=="VA"),]
  AV_Lapsus = mean(xx$Lapsus_Juan)
  
  to_return = data.frame(
    AV_RT
    , AV_SD
    , AV_Lapsus
    , half = ifelse("half"%in%names(yy),yy$half[1],NA)
  )
  row.names(to_return) = NULL
  return(to_return)
}  


set.seed(1)
ArousalVig_rels = ldply(
  .data = 1:1e1 
  , .fun = function(iteration){
    scores = ddply(
      .data = data_AVig
      , .variables = .(Subject)
      , .fun = function(z){
        z = ddply(
          .data = z
          , .variables = .(`Vigilancia[Trial]`)
          , .fun = function(x){
            x$half = ((1:nrow(x))%%2)[order(rnorm(nrow(x)))]
            return(x)
          }
        )
        
        scores_per_half = ddply(
          .data = z
          , .variables = .(half)
          , .fun = get_ArousalVig_scores
        )
        
        scores_per_half$Subject = z$Subject[1]
        return(scores_per_half)
      }
    )
    
    to_return = data.frame(
      iteration = iteration
      , AV_RT = with(scores,cor(AV_RT[half==0], AV_RT[half==1], use = "complete.obs"))
      , AV_SD = with(scores,cor(AV_SD[half==0], AV_SD[half==1], use = "complete.obs"))
      , AV_Lapsus = with(scores,cor(AV_Lapsus[half==0],AV_Lapsus[half==1]))
      , n = nrow(scores)
    )
    return(to_return)
  }
  , .progress = 'time'
)

ldply(
  .data = names(ArousalVig_rels)[2:ncol(ArousalVig_rels)]
  , .fun = function(x){
    to_return = data.frame(
      effect = x
    )
    to_return = cbind(
      to_return
      , data.frame(t(quantile(ArousalVig_rels[,names(ArousalVig_rels)==x],c(.025,.5, .975))))
    )
    return(to_return)
  }
)

#############

#AV Slopes

get_ArousalVig_scores_SL = function(yy)
{
#AV Mean
  xx = yy[which(yy$`Vigilancia[Trial]`=="VA"),]
  for (i in experimentalblocks)
  {
    zz = mean(xx$TargetVA.RT[xx$BlockList==i])
    assign(paste0("AV_Mean_B",i), zz)
    rm(zz)
  }
  rBlocks_AV_Mean = c(AV_Mean_B1, AV_Mean_B2, AV_Mean_B3, AV_Mean_B4, AV_Mean_B5, AV_Mean_B6)
  rgSL_AV_Mean = lm(rBlocks_AV_Mean ~ experimentalblocks)
  rSL_AV_Mean = coef(rgSL_AV_Mean)[[2]]

#AV SD

  xx = yy[which(yy$`Vigilancia[Trial]`=="VA"),]
  for (i in experimentalblocks)
  {
    zz = sd(xx$TargetVA.RT[xx$BlockList==i])
    assign(paste0("AV_SD_B",i), zz)
    rm(zz)
  }
  rBlocks_AV_SD = c(AV_SD_B1, AV_SD_B2, AV_SD_B3, AV_SD_B4, AV_SD_B5, AV_SD_B6)
  rgSL_AV_SD = lm(rBlocks_AV_SD ~ experimentalblocks)
  rSL_AV_SD = coef(rgSL_AV_SD)[[2]]
  
#AV Lapses

  xx = yy[which(yy$`Vigilancia[Trial]`=="VA"),]
  for (i in experimentalblocks)
  {
    zz = mean(xx$Lapsus_Juan[xx$BlockList==i])
    assign(paste0("AV_Lapses_B",i), zz)
    rm(zz)
  }
  rBlocks_AV_Lapses = c(AV_Lapses_B1, AV_Lapses_B2, AV_Lapses_B3, AV_Lapses_B4, AV_Lapses_B5, AV_Lapses_B6)
  rgSL_AV_Lapses = lm(rBlocks_AV_Lapses ~ experimentalblocks)
  rSL_AV_Lapses = coef(rgSL_AV_Lapses)[[2]]
  
  
  to_return = data.frame(
    rSL_AV_Mean
    , rSL_AV_SD
    , rSL_AV_Lapses
    , half = ifelse("half"%in%names(yy),yy$half[1],NA)
  )
  row.names(to_return) = NULL
  return(to_return)
}  


set.seed(1)
ArousalVig_SL_rels = ldply(
  .data = 1:1e1 
  , .fun = function(iteration){
    scores = ddply(
      .data = data_AVig
      , .variables = .(Subject)
      , .fun = function(z){
        z = ddply(
          .data = z
          , .variables = .(`Vigilancia[Trial]`)
          , .fun = function(x){
            x$half = ((1:nrow(x))%%2)[order(rnorm(nrow(x)))]
            return(x)
          }
        )
        
        scores_per_half = ddply(
          .data = z
          , .variables = .(half)
          , .fun = get_ArousalVig_scores_SL
        )
        
        scores_per_half$Subject = z$Subject[1]
        return(scores_per_half)
      }
    )
    
    to_return = data.frame(
      iteration = iteration
      , rSL_AV_Mean = with(scores,cor(rSL_AV_Mean[half==0], rSL_AV_Mean[half==1], use = "complete.obs"))
      , rSL_AV_SD = with(scores,cor(rSL_AV_SD[half==0], rSL_AV_SD[half==1], use = "complete.obs"))
      , rSL_AV_Lapses = with(scores,cor(rSL_AV_Lapses[half==0],rSL_AV_Lapses[half==1]))
      , n = nrow(scores)
    )
    return(to_return)
  }
  , .progress = 'time'
)

ldply(
  .data = names(ArousalVig_SL_rels)[2:ncol(ArousalVig_SL_rels)]
  , .fun = function(x){
    to_return = data.frame(
      effect = x
    )
    to_return = cbind(
      to_return
      , data.frame(t(quantile(ArousalVig_SL_rels[,names(ArousalVig_SL_rels)==x],c(.025,.5, .975))))
    )
    return(to_return)
  }
)

#####Irrelevant distractor######

##RT##

get_ID_RT_scores = function(yy){

  xx = aggregate(Target.RT ~ LugarDistractortoTarget + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "LugarDistractortoTarget", direction = "wide")
  xx[is.na(xx)] = 0
  idzopp = ((xx$Target.RT.opposite - xx$Target.RT.none) / xx$Target.RT.none)
  idzsame = ((xx$Target.RT.same - xx$Target.RT.none) / xx$Target.RT.none)
  idzgen = ((((xx$Target.RT.opposite + xx$Target.RT.same) / 2) - xx$Target.RT.none) / xx$Target.RT.none)
  
  to_return = data.frame(
    idzopp
    , idzsame
    , idzgen
    , half = ifelse("half"%in%names(yy),yy$half[1],NA)
  )
  row.names(to_return) = NULL
  return(to_return)
}


set.seed(1)
ID_RT_rels = ldply(
  .data = 1:1e1
  , .fun = function(iteration){
    scores = ddply(
      .data = data_ID_RT
      , .variables = .(Subject)
      , .fun = function(x){
        x = ddply(
          .data = x
          , .variables = .(LugarDistractortoTarget)
          , .fun = function(z){
            z$half = ((1:nrow(z))%%2)[order(rnorm(nrow(z)))]
            return(z)
          }
        )
        
        scores_per_half = ddply(
          .data = x
          , .variables = .(half)
          , .fun = get_ID_RT_scores
        )
        
        scores_per_half$Subject = x$Subject[1]
        return(scores_per_half)
      }
    )
    
    to_return = data.frame(
      iteration = iteration
      , idzopp = with(scores,cor(idzopp[half==0],idzopp[half==1]))
      , idzsame = with(scores,cor(idzsame[half==0],idzsame[half==1]))
      , idzgen = with(scores,cor(idzgen[half==0],idzgen[half==1]))
      , n = nrow(scores)
    )
    return(to_return)
  }
  , .progress = 'time'
)

ldply(
  .data = names(ID_RT_rels)[2:ncol(ID_RT_rels)]
  , .fun = function(x){
    to_return = data.frame(
      effect = x
    )
    to_return = cbind(
      to_return
      , data.frame(t(quantile(ID_RT_rels[,names(ID_RT_rels)==x],c(.025,.5, .975))))
    )
    return(to_return)
  }
)

##Errors##

get_ID_ACC_scores = function(yy){
  
  xx = aggregate(Target.ACC ~ LugarDistractortoTarget + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "LugarDistractortoTarget", direction = "wide")
  xx[is.na(xx)] = 0
  idzopp = (xx$Target.ACC.opposite - xx$Target.ACC.none)
  idzsame = (xx$Target.ACC.same - xx$Target.ACC.none)
  idzgen = (((xx$Target.ACC.opposite + xx$Target.ACC.same) / 2) - xx$Target.ACC.none)
  
  to_return = data.frame(
    idzopp
    , idzsame
    , idzgen
    , half = ifelse("half"%in%names(yy),yy$half[1],NA)
  )
  row.names(to_return) = NULL
  return(to_return)
}


set.seed(1)
ID_ACC_rels = ldply(
  .data = 1:1e1
  , .fun = function(iteration){
    scores = ddply(
      .data = data_ID_ACC
      , .variables = .(Subject)
      , .fun = function(x){
        x = ddply(
          .data = x
          , .variables = .(LugarDistractortoTarget)
          , .fun = function(z){
            z$half = ((1:nrow(z))%%2)[order(rnorm(nrow(z)))]
            return(z)
          }
        )
        
        scores_per_half = ddply(
          .data = x
          , .variables = .(half)
          , .fun = get_ID_ACC_scores
        )
        
        scores_per_half$Subject = x$Subject[1]
        return(scores_per_half)
      }
    )
    
    to_return = data.frame(
      iteration = iteration
      , idzopp = with(scores,cor(idzopp[half==0],idzopp[half==1]))
      , idzsame = with(scores,cor(idzsame[half==0],idzsame[half==1]))
      , idzgen = with(scores,cor(idzgen[half==0],idzgen[half==1]))
      , n = nrow(scores)
    )
    return(to_return)
  }
  , .progress = 'time'
)

ldply(
  .data = names(ID_ACC_rels)[2:ncol(ID_ACC_rels)]
  , .fun = function(x){
    to_return = data.frame(
      effect = x
    )
    to_return = cbind(
      to_return
      , data.frame(t(quantile(ID_ACC_rels[,names(ID_ACC_rels)==x],c(.025,.5, .975))))
    )
    return(to_return)
  }
)

##### Export data #####
# Remember if data was computed either with standard or online raw data #

# Export reliability scores for the ANTI-Vea task #
write.table(antiRT_rels, "antiRT_rels.csv", sep =";", row.names = FALSE, col.names = TRUE)
write.table(antiACC_rels, "antiACC_rels.csv", sep =";", row.names = FALSE, col.names = TRUE)
#write.table(ExecutiveVig_RT_rels, "ExecutiveVig_RT_rels.csv", sep =";", row.names = FALSE, col.names = TRUE)
write.table(ExecutiveVig_rels, "ExecutiveVig_rels.csv", sep =";", row.names = FALSE, col.names = TRUE)
write.table(ExecutiveVig_SL_rels, "ExecutiveVig_SL_rels.csv", sep =";", row.names = FALSE, col.names = TRUE)
write.table(ArousalVig_rels, "ArousalVig_rels.csv", sep =";", row.names = FALSE, col.names = TRUE)
write.table(ArousalVig_SL_rels, "ArousalVig_SL_rels.csv", sep =";", row.names = FALSE, col.names = TRUE)
write.table(ID_RT_rels, "ID_RT_rels.csv", sep =";", row.names = FALSE, col.names = TRUE)
write.table(ID_ACC_rels, "ID_ACC_rels.csv", sep =";", row.names = FALSE, col.names = TRUE)

#Getting reliability's overall mean, confidence intervals, and Spearman-Brown corrected value

References=c("References:","[1] Parsons, S., Kruijt, A. W., & Fox, E. (2019). Psychological science needs a standard practice of reporting the reliability of cognitive-behavioral measurements. Advances in Methods and Practices in Psychological Science, 2(4), 378-395. https://doi.org/10.1177/2515245919879695","[2] Roca, J., Garcia-Fernandez, P., Castro, C., & Lupiáñez, J. (2018). The moderating effects of vigilance on other components of attentional functioning. Journal of Neuroscience Methods, 308, 151-161. https://doi.org/10.1016/j.jneumeth.2018.07.019","[3] Luna, F. G., Roca, J., Martín-Arévalo, E., & Lupiáñez, J. (2020). Measuring attention and vigilance in the laboratory vs. online: The split-half reliability of the ANTI-Vea [Manuscript in preparation]. Instituto de Investigaciones Psicológicas, Universidad Nacional de Córdoba.")

reliabilityfinal = function(data_file, effect)
{
  
  reliabilityread <- read.csv(data_file, sep = ";", header = TRUE,)
  
  reliabilityoutput = c(paste("Mean split-half =", mean(reliabilityread[[effect]])), paste("LL split-half =", mean(reliabilityread[[effect]]) - 1.96*(sd(reliabilityread[[effect]])/sqrt(length(reliabilityread[[effect]])))), paste("UL split-half =", mean(reliabilityread[[effect]]) + 1.96*(sd(reliabilityread[[effect]])/sqrt(length(reliabilityread[[effect]])))), paste("Mean split-half with Spearman-Brown correction =", (2*mean(reliabilityread[[effect]]))/(1+mean(reliabilityread[[effect]]))),paste("This could be reported as: We estimated the internal consitency of the", effect, "index using a permutation-based split-half approach with 10,000 random splits (for a rationale, see Parsons et al., 2019; see Luna et al., 2020, and Roca et al., 2018, for previous applications of the technique). The (Spearman-Brown corrected) splithalf reliability was rSB =", round((2*mean(reliabilityread[[effect]]))/(1+mean(reliabilityread[[effect]])), digits = 2)))
  cat(reliabilityoutput,sep="\n")
}

#ANTI_RT
reliabilityfinal("antiRT_rels.csv","intercept")
reliabilityfinal("antiRT_rels.csv","tone")
reliabilityfinal("antiRT_rels.csv","cue")
reliabilityfinal("antiRT_rels.csv","cong")
#ANTI_ACC
reliabilityfinal("antiACC_rels.csv","intercept")
reliabilityfinal("antiACC_rels.csv","tone")
reliabilityfinal("antiACC_rels.csv","cue")
reliabilityfinal("antiACC_rels.csv","cong")
#EV_Overall
reliabilityfinal("ExecutiveVig_rels.csv","EV_Hrate")
reliabilityfinal("ExecutiveVig_rels.csv","EV_FArate_Difficult")
reliabilityfinal("ExecutiveVig_rels.csv","EV_Anp")
reliabilityfinal("ExecutiveVig_rels.csv","EV_Bnp")
#EV_Slope
reliabilityfinal("ExecutiveVig_SL_rels.csv","rSL_EV_Hrate")
reliabilityfinal("ExecutiveVig_SL_rels.csv","rSL_EV_FArate")
reliabilityfinal("ExecutiveVig_SL_rels.csv","rSL_EV_Ap")
reliabilityfinal("ExecutiveVig_SL_rels.csv","rSL_EV_Bp")
#AV_Overall
reliabilityfinal("ArousalVig_rels.csv","AV_RT")
reliabilityfinal("ArousalVig_rels.csv","AV_SD")
reliabilityfinal("ArousalVig_rels.csv","AV_Lapsus")
#AV_Slope
reliabilityfinal("ArousalVig_SL_rels.csv","rSL_AV_Mean")
reliabilityfinal("ArousalVig_SL_rels.csv","rSL_AV_SD")
reliabilityfinal("ArousalVig_SL_rels.csv","rSL_AV_Lapses")
#ID_RT
reliabilityfinal("ID_RT_rels.csv","idzopp")
reliabilityfinal("ID_RT_rels.csv","idzsame")
reliabilityfinal("ID_RT_rels.csv","idzgen")
#ID_ACC
reliabilityfinal("ID_ACC_rels.csv","idzopp")
reliabilityfinal("ID_ACC_rels.csv","idzsame")
reliabilityfinal("ID_ACC_rels.csv","idzgen")

cat(References,sep = "\n")
