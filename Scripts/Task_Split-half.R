#Script adapted from jroca and fluna

######Install and/or load packages######

if(!require(pacman)){install.packages("pacman")} # Install the package in case it was not installed
library(pacman) #This package ("pacman") serves to install and load other packages simultaneously
p_load(readxl, rockchalk, plyr, Hmisc, tidyverse, openxlsx) #Installing and loading the rest of the packages

##### Set data filters #####

filterRTmin = 200 #To exclude responses below 200 ms
filterRTmax = 1500 #To exclude responses above 1500 ms
experimentalblocks = c(1:6) #To exclude data from practice blocks
outliers_performance = c(106, 120, 137, 140)
outliers_performance_IDtrials = c(107, 122, 134, 137, 138, 140, 141, 159, 170, 171, 179, 182, 222)

##### Load data file (excel format!) #####

raw_data = read_excel("./Inputs/Data_ANTI-VeaD.xlsx", sheet = "Raw_Data") # Search from the PROJECT's working directory (the dot)

####Preprocessing database for reliability analyses####

#ANTI RT
rt_data = raw_data %>% 
  filter(!Subject %in% outliers_performance, BlockList %in% experimentalblocks, `Vigilancia[Trial]`=="NoVigilancia", Target.ACC==1, Target.RT>filterRTmin, Target.RT<filterRTmax) %>%
  select(Subject, `Tono[Trial]`, `ValidezClave[Trial]`, `Congruency[Trial]`, Target.RT)

#ANTI ACC
acc_data = raw_data %>% 
  filter(!Subject %in% outliers_performance, BlockList %in% experimentalblocks, `Vigilancia[Trial]`=="NoVigilancia") %>%
  select(Subject, `Tono[Trial]`, `ValidezClave[Trial]`, `Congruency[Trial]`, Target.ACC)

#EV
EV_data = raw_data %>%
  filter(!Subject %in% outliers_performance, BlockList %in% experimentalblocks) %>%
  select(Subject, BlockList, `Vigilancia[Trial]`, Target.ACC, Trial_FA_Difficult, FA_Difficult)
  #Define functions for A' and B''
Aprime = function(EV_Hrate, EV_FArate)
{
  ifelse(EV_Hrate < EV_FArate | EV_Hrate == 0,
         0.5,
         0.5 + (((EV_Hrate - EV_FArate)*(1 + EV_Hrate - EV_FArate)) / (4*(EV_Hrate)*(1 - (EV_FArate)))))
}

Bprime = function(EV_Hrate, EV_FArate)
{
  ifelse(EV_Hrate==0 & EV_FArate==0,
         1,
         ifelse(EV_Hrate==1,
                -1,
                (((EV_Hrate) * (1 - EV_Hrate)) - ((EV_FArate) * (1 - EV_FArate))) / (((EV_Hrate) * (1 - EV_Hrate)) + ((EV_FArate) * (1 - (EV_FArate)))))) 
}  

#AV
AV_data = raw_data %>%
  filter(!Subject %in% outliers_performance, BlockList %in% experimentalblocks, `Vigilancia[Trial]`=="VA") %>%
  select(Subject, BlockList, `Vigilancia[Trial]`, TargetVA.RT, Lapses = Lapsus_Juan)

#ID RT

IDrt_data = raw_data %>% 
  filter(!Subject %in% c(outliers_performance,outliers_performance_IDtrials), BlockList %in% experimentalblocks, `Vigilancia[Trial]` %in% c("DP", "DA"), Target.ACC==1, Target.RT>filterRTmin, Target.RT<filterRTmax) %>%
  select(Subject, `Vigilancia[Trial]`, LugarDistractortoTarget, Target.RT)

#ID ACC

IDacc_data = raw_data %>% 
  filter(!Subject %in% c(outliers_performance,outliers_performance_IDtrials), BlockList %in% experimentalblocks, `Vigilancia[Trial]`%in% c("DP", "DA")) %>%
  select(Subject, `Vigilancia[Trial]`, LugarDistractortoTarget, Target.ACC)


####Reliability analyses####

#ANTI RT trials: Overall, alerting , orienting , and congruency#

get_antiRT_scores = function(yy)
{
  xx = aggregate(Target.RT ~ Subject, yy, FUN = mean)
  overall_RT = xx$Target.RT
  
  xx = aggregate(Target.RT ~ `Tono[Trial]` + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "Tono[Trial]", direction = "wide")
  alerting_RT = xx$Target.RT.notono - xx$Target.RT.tono
  
  xx = aggregate(Target.RT ~ `ValidezClave[Trial]` + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "ValidezClave[Trial]", direction = "wide")
  orienting_RT = xx$Target.RT.invalida - xx$Target.RT.valida
  
  xx = aggregate(Target.RT ~ `Congruency[Trial]`+ Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "Congruency[Trial]", direction = "wide")
  congruency_RT = xx$Target.RT.incongruent - xx$Target.RT.congruent
  
  to_return = data.frame(
    overall_RT
    , alerting_RT
    , orienting_RT
    , congruency_RT
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
      .data = rt_data
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
      , overall_RT = with(scores,cor(overall_RT[half==0],overall_RT[half==1]))
      , alerting_RT = with(scores,cor(alerting_RT[half==0],alerting_RT[half==1]))
      , orienting_RT = with(scores,cor(orienting_RT[half==0],orienting_RT[half==1]))
      , congruency_RT = with(scores,cor(congruency_RT[half==0],congruency_RT[half==1]))
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

#ANTI ACC trials: Overall, alerting , orienting, and congruency#

get_antiACC_scores = function(yy)
{
  xx = aggregate(Target.ACC ~ Subject, yy, FUN = mean)
  overall_ACC = xx$Target.ACC
  
  xx = aggregate(Target.ACC ~ `Tono[Trial]` + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "Tono[Trial]", direction = "wide")
  alerting_ACC = xx$Target.ACC.notono - xx$Target.ACC.tono
  
  xx = aggregate(Target.ACC ~ `ValidezClave[Trial]` + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "ValidezClave[Trial]", direction = "wide")
  orienting_ACC = xx$Target.ACC.invalida - xx$Target.ACC.valida
  
  xx = aggregate(Target.ACC ~ `Congruency[Trial]` + Subject, yy, FUN = mean)
  xx = reshape(xx, idvar = "Subject", timevar = "Congruency[Trial]", direction = "wide")
  congruency_ACC = xx$Target.ACC.incongruent - xx$Target.ACC.congruent
  
  to_return = data.frame(
    overall_ACC
    , alerting_ACC
    , orienting_ACC
    , congruency_ACC
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
      .data = acc_data
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
      , overall_ACC = with(scores,cor(overall_ACC[half==0],overall_ACC[half==1]))
      , alerting_ACC = with(scores,cor(alerting_ACC[half==0],alerting_ACC[half==1]))
      , orienting_ACC = with(scores,cor(orienting_ACC[half==0],orienting_ACC[half==1]))
      , congruency_ACC = with(scores,cor(congruency_ACC[half==0],congruency_ACC[half==1]))
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

#EV trials: hits, false alarms, A'(sensitivity), B''(response bias). Overall performance and performance over time (slope)#

get_ExecutiveVig_scores = function(yy)
{
  xx = yy[which(yy$`Vigilancia[Trial]`=="VE"),]
  EV_Hits = mean(xx$Target.ACC)
  
  x = yy %>% 
    filter(`Vigilancia[Trial]`=="VE")
  EV_Hits_Slope = coef(lm(x$Target.ACC ~ x$BlockList))[2]
  
  xx = yy[which(yy$Trial_FA_Difficult=="YES"),]
  EV_FA = mean(xx$FA_Difficult)
  
  x = yy %>% 
    group_by(BlockList) %>% 
    summarise(FA_B=mean(FA_Difficult[Trial_FA_Difficult=="YES"]))
  EV_FA_Slope = as.numeric(coef(lm(x$FA_B ~ x$BlockList))[2])
  
  EV_Ap = Aprime(EV_Hits, EV_FA)
  
  x = yy %>% 
    group_by(BlockList) %>% 
    summarise(H_B=mean(Target.ACC[`Vigilancia[Trial]`=="VE"]),FA_B=mean(FA_Difficult[Trial_FA_Difficult=="YES"])) %>% 
    mutate(Ap=Aprime(H_B,FA_B))
  EV_Ap_Slope = as.numeric(coef(lm(x$Ap ~ x$BlockList))[2])
  
  EV_Bp = Bprime(EV_Hits, EV_FA)
  
  x = yy %>% 
    group_by(BlockList) %>% 
    summarise(H_B=mean(Target.ACC[`Vigilancia[Trial]`=="VE"]),FA_B=mean(FA_Difficult[Trial_FA_Difficult=="YES"])) %>% 
    mutate(Bp=Bprime(H_B,FA_B))
  EV_Bp_Slope = as.numeric(coef(lm(x$Bp ~ x$BlockList))[2])
  
  to_return = data.frame(
    EV_Hits
    , EV_Hits_Slope
    , EV_FA
    , EV_FA_Slope
    , EV_Ap
    , EV_Ap_Slope
    , EV_Bp
    , EV_Bp_Slope
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
      .data = EV_data
      , .variables = .(Subject)
      , .fun = function(z){
        z = ddply(
          .data = z
          , .variables = .(`Vigilancia[Trial]`,Trial_FA_Difficult, BlockList)
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
      , EV_Hits = with(scores,cor(EV_Hits[half==0],EV_Hits[half==1], use = "complete.obs"))
      , EV_Hits_Slope = with(scores,cor(EV_Hits_Slope[half==0],EV_Hits_Slope[half==1], use = "complete.obs"))
      , EV_FA = with(scores,cor(EV_FA[half==0],EV_FA[half==1], use = "complete.obs"))
      , EV_FA_Slope = with(scores,cor(EV_FA_Slope[half==0],EV_FA_Slope[half==1], use = "complete.obs"))
      , EV_Ap = with(scores,cor(EV_Ap[half==0],EV_Ap[half==1], use = "complete.obs"))
      , EV_Ap_Slope = with(scores,cor(EV_Ap_Slope[half==0],EV_Ap_Slope[half==1], use = "complete.obs"))
      , EV_Bp = with(scores,cor(EV_Bp[half==0],EV_Bp[half==1], use = "complete.obs"))
      , EV_Bp_Slope = with(scores,cor(EV_Bp_Slope[half==0],EV_Bp_Slope[half==1], use = "complete.obs"))
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

#AV trials: RT mean, SD mean, Lapses. Overall performance and performance over time (slope)#

get_ArousalVig_scores = function(yy){
  
  AV_RT = yy %>% 
  summarise(mean(TargetVA.RT)) %>% 
    pull()
  
  AV_RT_Slope = as.numeric(coef(lm(yy$TargetVA.RT ~ yy$BlockList))[2])
  
  AV_SD = yy %>% 
  summarise(sd(TargetVA.RT)) %>% 
    pull()
  
  x = yy %>% 
    group_by(Subject,BlockList) %>% 
    summarise(sd=sd(TargetVA.RT))
  AV_SD_Slope = as.numeric(coef(lm(x$sd ~ x$BlockList))[2])
  
  AV_Lapsus = yy %>% 
    summarise(mean(Lapses)) %>% 
    pull()
  
  AV_Lapsus_Slope = as.numeric(coef(lm(yy$Lapses ~ yy$BlockList))[2])
  
  to_return = data.frame(
    AV_RT
    , AV_RT_Slope
    , AV_SD
    , AV_SD_Slope
    , AV_Lapsus
    , AV_Lapsus_Slope
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
      .data = AV_data
      , .variables = .(Subject)
      , .fun = function(z){
        z = ddply(
          .data = z
          , .variables = .(BlockList)
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
      , AV_RT_Slope = with(scores,cor(AV_RT_Slope[half==0], AV_RT_Slope[half==1], use = "complete.obs"))
      , AV_SD = with(scores,cor(AV_SD[half==0], AV_SD[half==1], use = "complete.obs"))
      , AV_SD_Slope = with(scores,cor(AV_SD_Slope[half==0], AV_SD_Slope[half==1], use = "complete.obs"))
      , AV_Lapsus = with(scores,cor(AV_Lapsus[half==0],AV_Lapsus[half==1]))
      , AV_Lapsus_Slope = with(scores,cor(AV_Lapsus_Slope[half==0],AV_Lapsus_Slope[half==1]))
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

#ID RT trials: ID index z-scores (IDz_RT). Exploratory indexes: ID index z-scores in the same (IDzSame_RT) and the opposite (IDzOpposite_RT) location from the target.

get_ID_RT_scores = function(yy){
  
  x = yy %>% 
    group_by(Subject, `Vigilancia[Trial]`) %>% 
    summarise(RT=mean(Target.RT))
  IDz_RT = (x$RT[x$`Vigilancia[Trial]`=="DP"] - x$RT[x$`Vigilancia[Trial]`=="DA"]) / x$RT[x$`Vigilancia[Trial]`=="DA"]
  
  x = yy %>% 
    group_by(Subject, LugarDistractortoTarget) %>% 
    summarise(RT=mean(Target.RT))
  IDzSame_RT = (x$RT[x$LugarDistractortoTarget=="same"] - x$RT[x$LugarDistractortoTarget=="none"]) / x$RT[x$LugarDistractortoTarget=="none"]
  IDzOpposite_RT = (x$RT[x$LugarDistractortoTarget=="opposite"] - x$RT[x$LugarDistractortoTarget=="none"]) / x$RT[x$LugarDistractortoTarget=="none"]
  
  to_return = data.frame(
    IDz_RT
    , IDzSame_RT
    , IDzOpposite_RT
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
      .data = IDrt_data
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
      , IDz_RT = with(scores,cor(IDz_RT[half==0],IDz_RT[half==1]))
      , IDzSame_RT = with(scores,cor(IDzSame_RT[half==0],IDzSame_RT[half==1]))
      , IDzOpposite_RT = with(scores,cor(IDzOpposite_RT[half==0],IDzOpposite_RT[half==1]))
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

#ID ACC trials: ID index z-scores (IDz_ACC). Exploratory indexes: ID index z-scores in the same (IDzSame_ACC) and the opposite (IDzOpposite_ACC) location from the target.

get_ID_ACC_scores = function(yy){
  
  x = yy %>% 
    group_by(Subject, `Vigilancia[Trial]`) %>% 
    summarise(ACC=mean(Target.ACC))
  IDz_ACC = x$ACC[x$`Vigilancia[Trial]`=="DP"] - x$ACC[x$`Vigilancia[Trial]`=="DA"]
  
  x = yy %>% 
    group_by(Subject, LugarDistractortoTarget) %>% 
    summarise(ACC=mean(Target.ACC))
  IDzSame_ACC = x$ACC[x$LugarDistractortoTarget=="same"] - x$ACC[x$LugarDistractortoTarget=="none"] 
  IDzOpposite_ACC = x$ACC[x$LugarDistractortoTarget=="opposite"] - x$ACC[x$LugarDistractortoTarget=="none"]
  
  to_return = data.frame(
    IDz_ACC
    , IDzSame_ACC
    , IDzOpposite_ACC
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
      .data = IDacc_data
      , .variables = .(Subject)
      , .fun = function(x){
        x = ddply(
          .data = x
          , .variables = .(`Vigilancia[Trial]`, LugarDistractortoTarget)
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
      , IDz_ACC = with(scores,cor(IDz_ACC[half==0],IDz_ACC[half==1]))
      , IDzSame_ACC = with(scores,cor(IDzSame_ACC[half==0],IDzSame_ACC[half==1]))
      , IDzOpposite_ACC = with(scores,cor(IDzOpposite_ACC[half==0],IDzOpposite_ACC[half==1]))
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

#### Obtain reliability's overall mean of iterations, confidence intervals, and Spearman-Brown corrected value ####

reliabilityfinal = function(table_reliability, effect,name)
{
  reliabilityoutput = c(paste0("Permutation-based split-half reliability of ", effect,":"), 
                        paste0("Mean = ",round(mean(table_reliability[[effect]]),digits = 3),", 95% CI[",round(mean(table_reliability[[effect]]) - 1.96*(sd(table_reliability[[effect]])/sqrt(length(table_reliability[[effect]]))),digits = 3),", ",round(mean(table_reliability[[effect]]) + 1.96*(sd(table_reliability[[effect]])/sqrt(length(table_reliability[[effect]]))),digits = 3),"]."),
                        paste0("Mean with Spearman-Brown correction (rSB) = ", round((2*mean(table_reliability[[effect]]))/(1+mean(table_reliability[[effect]])),digits = 3)," (CIs in a large sample of iterations does not virtually vary)."))
  cat(reliabilityoutput,sep="\n")
  assign(name, reliabilityoutput, envir = .GlobalEnv)
  ExampleReporting <<- as.data.frame(matrix(c(paste0("Report example: We estimated the internal consistency of the ", effect, " index using a permutation-based split-half approach with 10,000 random splits (for a rationale, see Parsons et al., 2019; see Luna et al., 2020, and Roca et al., 2018, for previous applications of the technique). The Spearman-Brown corrected splithalf reliability was ", round((2*mean(table_reliability[[effect]]))/(1+mean(table_reliability[[effect]])),digits = 2),"."),"References:","[1] Parsons, S., Kruijt, A. W., & Fox, E. (2019). Psychological science needs a standard practice of reporting the reliability of cognitive-behavioral measurements. Advances in Methods and Practices in Psychological Science, 2(4), 378-395. https://doi.org/10.1177/2515245919879695","[2] Roca, J., Garcia-Fernandez, P., Castro, C., & Lupiáñez, J. (2018). The moderating effects of vigilance on other components of attentional functioning. Journal of Neuroscience Methods, 308, 151-161. https://doi.org/10.1016/j.jneumeth.2018.07.019","[3] Luna, F. G., Roca, J., Martín-Arévalo, E., & Lupiáñez, J. (2020). Measuring attention and vigilance in the laboratory vs. online: The split-half reliability of the ANTI-Vea [Manuscript in preparation]. Instituto de Investigaciones Psicológicas, Universidad Nacional de Córdoba.")),dimnames = NULL)
  
}

#ANTI RT
reliabilityfinal(antiRT_rels,"overall_RT","r.anti_RT_overall")
reliabilityfinal(antiRT_rels,"alerting_RT", "r.anti_RT_alerting")
reliabilityfinal(antiRT_rels,"orienting_RT", "r.anti_RT_orienting")
reliabilityfinal(antiRT_rels,"congruency_RT", "r.anti_RT_congruency")
#ANTI ACC
reliabilityfinal(antiACC_rels,"overall_ACC","r.anti_ACC_overall")
reliabilityfinal(antiACC_rels,"alerting_ACC","r.anti_ACC_alerting")
reliabilityfinal(antiACC_rels,"orienting_ACC","r.anti_ACC_orienting")
reliabilityfinal(antiACC_rels,"congruency_ACC","r.anti_ACC_congruency")
#EV
reliabilityfinal(ExecutiveVig_rels,"EV_Hits","r.EV_Hits")
reliabilityfinal(ExecutiveVig_rels,"EV_Hits_Slope","r.EV_Hits_Slope")
reliabilityfinal(ExecutiveVig_rels,"EV_FA","r.EV_FA")
reliabilityfinal(ExecutiveVig_rels,"EV_FA_Slope","r.EV_FA_Slope")
reliabilityfinal(ExecutiveVig_rels,"EV_Ap","r.EV_Ap")
reliabilityfinal(ExecutiveVig_rels,"EV_Ap_Slope","r.EV_Ap_Slope")
reliabilityfinal(ExecutiveVig_rels,"EV_Bp","r.EV_Bp")
reliabilityfinal(ExecutiveVig_rels,"EV_Bp_Slope","r.EV_Bp_Slope")
#AV
reliabilityfinal(ArousalVig_rels,"AV_RT","r.AV_RT")
reliabilityfinal(ArousalVig_rels,"AV_RT_Slope","r.AV_RT_Slope")
reliabilityfinal(ArousalVig_rels,"AV_SD","r.AV_SD")
reliabilityfinal(ArousalVig_rels,"AV_SD_Slope","r.AV_SD_Slope")
reliabilityfinal(ArousalVig_rels,"AV_Lapsus","r.AV_Lapsus")
reliabilityfinal(ArousalVig_rels,"AV_Lapsus_Slope","r.AV_Lapsus_Slope")
#ID RT
reliabilityfinal(ID_RT_rels,"IDz_RT","r.IDz_RT")
reliabilityfinal(ID_RT_rels,"IDzSame_RT","r.IDzSame_RT")
reliabilityfinal(ID_RT_rels,"IDzOpposite_RT","r.IDzOpposite_RT")
#ID ACC
reliabilityfinal(ID_ACC_rels,"IDz_ACC","r.IDz_ACC")
reliabilityfinal(ID_ACC_rels,"IDzSame_ACC","r.IDzSame_ACC")
reliabilityfinal(ID_ACC_rels,"IDzOpposite_ACC","r.IDzOpposite_ACC")




