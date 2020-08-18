######Install and/or load packages######

if(!require(pacman)){install.packages("pacman")}
library(pacman) #This package ("pacman") serves to install and load other packages simultaneously
p_load(readxl, rockchalk, plyr, Hmisc, tidyverse) #Installing and loading the rest of the packages

##### Set data filters #####

filterRTmin = 200 #Exclude responses below 200 ms
filterRTmax = 1500 #Exclude responses above 1500 ms
experimentalblocks = c(1:6) #if necessary, to exclude data after n experimental block

##### Load data file #####

data_raw <- read_excel("./Inputs/Data_ANTI-VeaD.xlsx", sheet = "Raw_Data")
data_processed = data_raw %>% 
  distinct(Subject)

#####Identify outliers participants######

#Outliers participants#

  #General task: Participants with more than 25% errors in ANTI trials

x = data_raw %>%
  filter (BlockList %in% experimentalblocks, `Vigilancia[Trial]`=="NoVigilancia") %>% 
  group_by(Subject) %>% 
  summarise(Target.Error=mean(Target.Error))
outliers_performance = x %>% #This object represents the subjects that have been excluded
  filter (Target.Error > .25) %>% 
  select(Subject) %>% 
  pull()
n_excluded_bcperformance = length(outliers_performance) # This object is the number of subjects that have been excluded

x = x %>% 
  transmute(Validity_GeneralTask=ifelse(Target.Error > .25, "No","Yes"))
data_processed = cbind(data_processed,x)

rm(x)

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

  #Outlier trials#

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

data_rt_table = data_rt %>% 
  group_by(Subject, `Tono[Trial]`, `ValidezClave[Trial]`, `Congruency[Trial]`) %>% 
  summarise(Target.RT=mean(Target.RT)) %>% 
  mutate(Condition = paste(`Tono[Trial]`,`ValidezClave[Trial]`,`Congruency[Trial]`, sep = "_"))
data_rt_table_wide = as.data.frame(data_rt_table) %>%
  reshape(idvar = "Subject", timevar = "Condition", drop = c("Tono[Trial]", "ValidezClave[Trial]", "Congruency[Trial]"), direction = "wide")

# Compute attentional scores #

x = data_rt %>% 
  group_by(Subject, `Tono[Trial]`) %>% 
  summarise(Alerting_RT=mean(Target.RT))
  

x = aggregate(Target.RT ~ `Tono[Trial]` + Subject, data_rt, FUN = mean)
x = reshape(x, idvar = "Subject", timevar = "Tono[Trial]", direction = "wide")
x = cbind (x, RT_A = x$Target.RT.notono - x$Target.RT.tono)
data_rt_table_wide = merge(data_rt_table_wide, subset(x, select = -c(Target.RT.notono, Target.RT.tono)), by = "Subject")

x = aggregate(Target.RT ~ `ValidezClave[Trial]` + Subject, data_rt, FUN = mean)
x = reshape(x, idvar = "Subject", timevar = "ValidezClave[Trial]", direction = "wide") 
x = cbind (x, RT_O = x$Target.RT.invalida - x$Target.RT.valida)
data_rt_table_wide = merge(data_rt_table_wide, subset(x, select = -c(Target.RT.invalida, Target.RT.nocue, Target.RT.valida)), by = "Subject")

x = aggregate(Target.RT ~ `Congruency[Trial]` + Subject, data_rt, FUN = mean)
x = reshape(x, idvar = "Subject", timevar = "Congruency[Trial]", direction = "wide")
x = cbind (x, RT_EC = x$Target.RT.incongruent - x$Target.RT.congruent)
data_rt_table_wide = merge(data_rt_table_wide, subset(x, select = -c(Target.RT.congruent, Target.RT.incongruent)), by = "Subject")

x = aggregate(Target.RT ~ Subject, data_rt, FUN = mean)
data_rt_table_wide = merge(data_rt_table_wide, x, by = "Subject")
names(data_rt_table_wide)[which(names(data_rt_table_wide)=="Target.RT")]="RT_Overall"

rm(x)


# ACC analysis of ANTI trials #

# Select appropriate trials (ANTI task, NOT drop first block) #

data_acc = data_raw[which(data_raw$`Vigilancia[Trial]`=="NoVigilancia"),]
data_acc = data_acc[which(data_acc$BlockList %in% experimentalblocks),]

# Generate data table for ACC analysis #

data_acc_table = aggregate(Target.ACC ~ `Tono[Trial]` + `ValidezClave[Trial]` + `Congruency[Trial]` + Subject, data = data_acc, FUN = mean)
data_acc_table$Target.ACC = 100 * (1 - data_acc_table$Target.ACC)

# Compute attentional scores #

data_acc_table = cbind(data_acc_table, "Condition" = paste(data_acc_table$`Tono[Trial]`, data_acc_table$`ValidezClave[Trial]`, data_acc_table$`Congruency[Trial]`, sep = "_"))
data_acc_table = data_acc_table[order(data_acc_table$Condition),]
data_acc_table = data_acc_table[order(data_acc_table$Subject),]
data_acc_table_wide = reshape(data_acc_table, idvar = "Subject", timevar = "Condition", drop = c("Tono[Trial]", "ValidezClave[Trial]", "Congruency[Trial]"), direction = "wide")

x = aggregate(100 * Target.Error ~ `Tono[Trial]` + Subject, data_acc, FUN = mean)
x = reshape(x, idvar = "Subject", timevar = "Tono[Trial]", direction = "wide")
names(x)[which(names(x)=="100 * Target.Error.notono")]="Target.ACC.notono"
names(x)[which(names(x)=="100 * Target.Error.tono")]="Target.ACC.tono"
x = cbind (x, ACC_A = x$Target.ACC.notono - x$Target.ACC.tono)
data_acc_table_wide = merge(data_acc_table_wide, subset(x, select = -c(Target.ACC.notono, Target.ACC.tono)), by = "Subject")

x = aggregate(100 * Target.Error ~ `ValidezClave[Trial]` + Subject, data_acc, FUN = mean)
x = reshape(x, idvar = "Subject", timevar = "ValidezClave[Trial]", direction = "wide") 
names(x)[which(names(x)=="100 * Target.Error.invalida")]="Target.ACC.invalida"
names(x)[which(names(x)=="100 * Target.Error.nocue")]="Target.ACC.nocue"
names(x)[which(names(x)=="100 * Target.Error.valida")]="Target.ACC.valida"
x = cbind (x, ACC_O = x$Target.ACC.invalida - x$Target.ACC.valida)
data_acc_table_wide = merge(data_acc_table_wide, subset(x, select = -c(Target.ACC.invalida, Target.ACC.nocue, Target.ACC.valida)), by = "Subject")

x = aggregate(100 * Target.Error ~ `Congruency[Trial]` + Subject, data_acc, FUN = mean)
x = reshape(x, idvar = "Subject", timevar = "Congruency[Trial]", direction = "wide") 
names(x)[which(names(x)=="100 * Target.Error.incongruent")]="Target.ACC.incongruent"
names(x)[which(names(x)=="100 * Target.Error.congruent")]="Target.ACC.congruent"
x = cbind (x, ACC_EC = x$Target.ACC.incongruent - x$Target.ACC.congruent)
data_acc_table_wide = merge(data_acc_table_wide, subset(x, select = -c(Target.ACC.congruent, Target.ACC.incongruent)), by = "Subject")

x = aggregate(100 * Target.Error ~ Subject, data_acc, FUN = mean)
data_acc_table_wide = merge(data_acc_table_wide, x, by = "Subject")
names(data_acc_table_wide)[which(names(data_acc_table_wide)=="100 * Target.Error")]="ACC_Overall"

rm(x)


##### Executive Vigilance (EV) #####
###NOT ACTIVATED IN coll-martin_et.al's edition
# EV - Mean Reaction Time and SD on Hits #

# Select appropriate trials (EV task, drop errors, NOT drop first block) #

#data_EVig_RT = data_raw[which(data_raw$`Vigilancia[Trial]`=="VE"),]
#data_EVig_RT = data_EVig_RT[which(data_EVig_RT$BlockList %in% BlockList),]
#data_EVig_RT = data_EVig_RT[which(data_EVig_RT$Target.ACC==1),]


# Filter RT by participant and task (attention: no per condition) #

#filterRT_EV_percentage = round(100*sum(!(data_EVig_RT$Target.RT > (filterRTmin) & data_EVig_RT$Target.RT < filterRTmax))/sum(data_EVig_RT$Target.RT > (filterRTmin) & data_EVig_RT$Target.RT < filterRTmax), digits=4)

#data_EVig_RT = data_EVig_RT[which(data_EVig_RT$Target.RT > (filterRTmin) & data_EVig_RT$Target.RT < filterRTmax),]

#data_EVig_RT_table = unique(subset(data_EVig_RT, select="Subject"))

#Compute mean RT and SD of RT #

#data_EVig_RT_table = merge(x = data_EVig_RT_table, y = aggregate(Target.RT ~ Subject, data = data_EVig_RT[which(data_EVig_RT$`Vigilancia[Trial]` =="VE" & data_EVig_RT$Target.ACC == 1),], FUN = mean), all.x = TRUE, all.y = TRUE, by = "Subject")
#names(data_EVig_RT_table)[names(data_EVig_RT_table)=='Target.RT'] <- "EV_RT"

#data_EVig_RT_table = merge(x = data_EVig_RT_table, y = aggregate(Target.RT ~ Subject, data = data_EVig_RT[which(data_EVig_RT$`Vigilancia[Trial]` =="VE" & data_EVig_RT$Target.ACC == 1),], FUN = sd), all.x = TRUE, all.y = TRUE, by = "Subject")
#names(data_EVig_RT_table)[names(data_EVig_RT_table)=='Target.RT'] <- "EV_SD"




# EV - Signal Detection Theory #

# Select appropriate trials (NOT drop first block) #

data_EVig = data_raw[which(data_raw$BlockList %in% experimentalblocks),]

data_EVig_table = unique(subset(data_EVig, select= "Subject"))

#Compute Hits and False Alarms #
###NOT ACTIVATED IN coll-martin_et.al's edition
#data_EVig_table = merge(x = data_EVig_table, y = aggregate(Target.ACC ~ Subject, data = data_EVig[which(data_EVig$`Vigilancia[Trial]`=="VE" & data_EVig$Target.ACC == 1),], FUN = length), all.x = TRUE, all.y = TRUE, by = "Subject")
#names(data_EVig_table)[names(data_EVig_table)=='Target.ACC'] <- "EV_H"
#data_EVig_table[is.na(data_EVig_table$EV_H), "EV_H" ] = 0


#data_EVig_table = merge(x = data_EVig_table, y = aggregate(FA_Juan ~ Subject, data = data_EVig[which(data_EVig$`Vigilancia[Trial]`=="NoVigilancia" & data_EVig$FA_Juan == 1),], FUN = length), all.x = TRUE, all.y = TRUE, by = "Subject")
#names(data_EVig_table)[names(data_EVig_table)=='FA_Juan'] <- "EV_FA_Total"
#data_EVig_table[is.na(data_EVig_table$EV_FA_Total), "EV_FA_Total" ] = 0


#data_EVig_table = merge(x = data_EVig_table, y = aggregate(FA_Difficult ~ Subject, data = data_EVig[which(data_EVig$Trial_FA_Difficult=="YES" & data_EVig$FA_Difficult == 1),], FUN = length), all.x = TRUE, all.y = TRUE, by = "Subject")
#names(data_EVig_table)[names(data_EVig_table)=='FA_Difficult'] <- "EV_FA_Difficult"
#data_EVig_table[is.na(data_EVig_table$EV_FA_Difficult), "EV_FA_Difficult" ] = 0

#Compute Hits and FA total/difficult rate #

data_EVig_table = merge(x = data_EVig_table, y = aggregate(Target.ACC ~ Subject, data = data_EVig[which(data_EVig$`Vigilancia[Trial]` =="VE"),], FUN = mean), all.x = TRUE, all.y = TRUE, by = "Subject")
names(data_EVig_table)[names(data_EVig_table)=='Target.ACC'] <- "EV_Hrate"

#data_EVig_table = merge(x = data_EVig_table, y = aggregate(100 * FA_Juan ~ Subject, data = data_EVig[which(data_EVig$`Vigilancia[Trial]`=="NoVigilancia"),], FUN = mean), all.x = TRUE, all.y = TRUE, by = "Subject")
#names(data_EVig_table)[names(data_EVig_table)=='100 * FA_Juan'] <- "EV_FArate_Total"

data_EVig_table = merge(x = data_EVig_table, y = aggregate(FA_Difficult ~ Subject, data = data_EVig[which(data_EVig$Trial_FA_Difficult =="YES"),], FUN = mean), all.x = TRUE, all.y = TRUE, by = "Subject")
names(data_EVig_table)[names(data_EVig_table)=='FA_Difficult'] <- "EV_FArate_Difficult"


# Executive Vigilance: A' (sensitivity) and B" (response bias) non-parametric (np) indexes #

data_EVig_table$EV_Anp = 
  ifelse(data_EVig_table$EV_Hrate < data_EVig_table$EV_FArate_Difficult | data_EVig_table$EV_Hrate == 0,
         0.5,
         0.5 + (((data_EVig_table$EV_Hrate - data_EVig_table$EV_FArate_Difficult)*(1 + data_EVig_table$EV_Hrate - data_EVig_table$EV_FArate_Difficult)) / (4*data_EVig_table$EV_Hrate*(1 - data_EVig_table$EV_FArate_Difficult))))

data_EVig_table$EV_Bnp = 
  ifelse(data_EVig_table$EV_Hrate==0 & data_EVig_table$EV_FArate_Difficult==0,
         1,
         ifelse(data_EVig_table$EV_Hrate==1,
                -1,
                ((data_EVig_table$EV_Hrate * (1 - data_EVig_table$EV_Hrate)) - (data_EVig_table$EV_FArate_Difficult * (1 - data_EVig_table$EV_FArate_Difficult))) / ((data_EVig_table$EV_Hrate * (1 - data_EVig_table$EV_Hrate)) + (data_EVig_table$EV_FArate_Difficult * (1 - data_EVig_table$EV_FArate_Difficult)))))


#Calculating EV Slopes#

#Hits

for (i in experimentalblocks)
  {
  data_EVig_table = merge(x = data_EVig_table, y = aggregate(Target.ACC ~ Subject, data = data_EVig[which(data_EVig$`Vigilancia[Trial]` =="VE" & data_EVig$BlockList==i),], FUN = mean), all.x = TRUE, all.y = TRUE, by = "Subject")
  names(data_EVig_table)[names(data_EVig_table)=='Target.ACC'] <- paste0("EV_Hrate_B",i)
}

for (i in 1:length(data_EVig_table$Subject))
  {
  Blocks_EV_Hrate = c(data_EVig_table$EV_Hrate_B1[i], data_EVig_table$EV_Hrate_B2[i], data_EVig_table$EV_Hrate_B3[i], data_EVig_table$EV_Hrate_B4[i], data_EVig_table$EV_Hrate_B5[i], data_EVig_table$EV_Hrate_B6[i])
  SL_EV_Hrate = lm(Blocks_EV_Hrate ~ experimentalblocks)
  data_EVig_table$Slope_EV_Hrate[i] =
    coef(SL_EV_Hrate)[2]
  }

#False alarms

for (i in experimentalblocks)
{
  data_EVig_table = merge(x = data_EVig_table, y = aggregate(FA_Difficult ~ Subject, data = data_EVig[which(data_EVig$Trial_FA_Difficult =="YES" & data_EVig$BlockList==i),], FUN = mean), all.x = TRUE, all.y = TRUE, by = "Subject")
  names(data_EVig_table)[names(data_EVig_table)=='FA_Difficult'] <- paste0("EV_FArate_B",i)
}

for (i in 1:length(data_EVig_table$Subject))
{
  Blocks_EV_FArate = c(data_EVig_table$EV_FArate_B1[i], data_EVig_table$EV_FArate_B2[i], data_EVig_table$EV_FArate_B3[i], data_EVig_table$EV_FArate_B4[i], data_EVig_table$EV_FArate_B5[i], data_EVig_table$EV_FArate_B6[i])
  SL_EV_FArate = lm(Blocks_EV_FArate ~ experimentalblocks)
  data_EVig_table$Slope_EV_FArate[i] =
    coef(SL_EV_FArate)[2]
}

#A'

for (i in experimentalblocks)
{
data_EVig_table[[paste0("EV_Ap_B", i)]] =
  ifelse(data_EVig_table[[paste0("EV_Hrate_B", i)]] < data_EVig_table[[paste0("EV_FArate_B", i)]] | data_EVig_table[[paste0("EV_Hrate_B", i)]] == 0,
         0.5,
         0.5 + (((data_EVig_table[[paste0("EV_Hrate_B", i)]] - data_EVig_table[[paste0("EV_FArate_B", i)]])*(1 + data_EVig_table[[paste0("EV_Hrate_B", i)]] - data_EVig_table[[paste0("EV_FArate_B", i)]])) / (4*data_EVig_table[[paste0("EV_Hrate_B", i)]]*(1 - data_EVig_table[[paste0("EV_FArate_B", i)]]))))
}

for (i in 1:length(data_EVig_table$Subject))
{
  Blocks_EV_Ap = c(data_EVig_table$EV_Ap_B1[i], data_EVig_table$EV_Ap_B2[i], data_EVig_table$EV_Ap_B3[i], data_EVig_table$EV_Ap_B4[i], data_EVig_table$EV_Ap_B5[i], data_EVig_table$EV_Ap_B6[i])
  SL_Ap = lm(Blocks_EV_Ap ~ experimentalblocks)
  data_EVig_table$Slope_Ap[i] =
    coef(SL_Ap)[2]
}

#B''

for (i in experimentalblocks)
{
data_EVig_table[[paste0("EV_Bp_B", i)]] =
  ifelse(data_EVig_table[[paste0("EV_Hrate_B", i)]]==0 & data_EVig_table[[paste0("EV_FArate_B", i)]]==0,
         1,
         ifelse(data_EVig_table[[paste0("EV_Hrate_B", i)]]==1,
                -1,
                ((data_EVig_table[[paste0("EV_Hrate_B", i)]] * (1 - data_EVig_table[[paste0("EV_Hrate_B", i)]])) - (data_EVig_table[[paste0("EV_FArate_B", i)]] * (1 - data_EVig_table[[paste0("EV_FArate_B", i)]]))) / ((data_EVig_table[[paste0("EV_Hrate_B", i)]] * (1 - data_EVig_table[[paste0("EV_Hrate_B", i)]])) + (data_EVig_table[[paste0("EV_FArate_B", i)]] * (1 - data_EVig_table[[paste0("EV_FArate_B", i)]])))))
}
 
for (i in 1:length(data_EVig_table$Subject))
{
Blocks_EV_Bp = c(data_EVig_table$EV_Bp_B1[i], data_EVig_table$EV_Bp_B2[i], data_EVig_table$EV_Bp_B3[i], data_EVig_table$EV_Bp_B4[i], data_EVig_table$EV_Bp_B5[i], data_EVig_table$EV_Bp_B6[i])
SL_Bp = lm(Blocks_EV_Bp ~ experimentalblocks)
data_EVig_table$Slope_Bp[i] =
  coef(SL_Bp)[2]
}

##### Arousal Vigilance (AV) #####

# Select appropriate trials (NOT drop first block) #

data_AVig = data_raw[which(data_raw$`Vigilancia[Trial]` =="VA"),]
data_AVig = data_AVig[which(data_AVig$BlockList %in% experimentalblocks),]

data_AVig_table = unique(subset(data_AVig, select = "Subject"))

# Compute RT, SD of RT, and Lapsus rate #

data_AVig_table = merge(x = data_AVig_table, y = aggregate(TargetVA.RT ~ Subject, data = data_AVig, FUN = mean), all.x = TRUE, all.y = TRUE, by = "Subject")
names(data_AVig_table)[names(data_AVig_table)=='TargetVA.RT'] <- "AV_RT"

data_AVig_table = merge(x = data_AVig_table, y = aggregate(TargetVA.RT ~ Subject, data = data_AVig, FUN = sd), all.x = TRUE, all.y = TRUE, by = "Subject")
names(data_AVig_table)[names(data_AVig_table)=='TargetVA.RT'] <- "AV_SD"

data_AVig_table = merge(x = data_AVig_table, y = aggregate(Lapsus_Juan ~ Subject, data = data_AVig, FUN = mean), all.x = TRUE, all.y = TRUE, by = "Subject")
names(data_AVig_table)[names(data_AVig_table)=='Lapsus_Juan'] <- "AV_Lapsus"

#Calculate slopes#

#RT

for (i in experimentalblocks)
{
data_AVig_table = merge(x = data_AVig_table, y = aggregate(TargetVA.RT ~ Subject, data = data_AVig[which(data_AVig$BlockList==i),], FUN = mean), all.x = TRUE, all.y = TRUE, by = "Subject")
names(data_AVig_table)[names(data_AVig_table)=='TargetVA.RT'] <- paste0("AV_RT_B",i)
}

for (i in 1:length(data_AVig_table$Subject))
{
Blocks_AV_RT = c(data_AVig_table$AV_RT_B1[i], data_AVig_table$AV_RT_B2[i], data_AVig_table$AV_RT_B3[i], data_AVig_table$AV_RT_B4[i], data_AVig_table$AV_RT_B5[i], data_AVig_table$AV_RT_B6[i])
SL_AV_RT = lm(Blocks_AV_RT ~ experimentalblocks)
data_AVig_table$Slope_AV_RT[i] =
  coef(SL_AV_RT)[2]
}

#SD

for (i in experimentalblocks)
{
data_AVig_table = merge(x = data_AVig_table, y = aggregate(TargetVA.RT ~ Subject, data = data_AVig[which(data_AVig$BlockList==i),], FUN = sd), all.x = TRUE, all.y = TRUE, by = "Subject")
names(data_AVig_table)[names(data_AVig_table)=='TargetVA.RT'] <- paste0("AV_SD_B",i)
}

for (i in 1:length(data_AVig_table$Subject))
{
Blocks_AV_SD = c(data_AVig_table$AV_SD_B1[i], data_AVig_table$AV_SD_B2[i], data_AVig_table$AV_SD_B3[i], data_AVig_table$AV_SD_B4[i], data_AVig_table$AV_SD_B5[i], data_AVig_table$AV_SD_B6[i])
SL_AV_SD = lm(Blocks_AV_SD ~ experimentalblocks)
data_AVig_table$Slope_AV_SD[i] =
  coef(SL_AV_SD)[2]
}

#Lapses

for (i in experimentalblocks)
{
data_AVig_table = merge(x = data_AVig_table, y = aggregate(Lapsus_Juan ~ Subject, data = data_AVig[which(data_AVig$BlockList==i),], FUN = mean), all.x = TRUE, all.y = TRUE, by = "Subject")
names(data_AVig_table)[names(data_AVig_table)=='Lapsus_Juan'] <- paste0("AV_Lapsus_B",i)
}

for (i in 1:length(data_AVig_table$Subject))
{
Blocks_AV_Lapsus = c(data_AVig_table$AV_Lapsus_B1[i], data_AVig_table$AV_Lapsus_B2[i], data_AVig_table$AV_Lapsus_B3[i], data_AVig_table$AV_Lapsus_B4[i], data_AVig_table$AV_Lapsus_B5[i], data_AVig_table$AV_Lapsus_B6[i])
SL_AV_Lapsus = lm(Blocks_AV_Lapsus ~ experimentalblocks)
data_AVig_table$Slope_AV_Lapsus[i] =
  coef(SL_AV_Lapsus)[2]
}

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
