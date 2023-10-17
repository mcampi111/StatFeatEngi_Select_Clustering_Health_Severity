library(dplyr)
library(sp)
library(leaflet)
library(lubridate)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(naniar)
library(reticulate)
library(proxy)
library(pracma)
library(reshape2)
library(readxl)
library(janitor)
library(tibble)
library(viridis)
library(scales)
library(blandr)
library(irr)
library(MASS)
library(emg)
library(blandr)
library(Rtsne)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(kernlab)
library(randomForest)
library(e1071)   
library(varImp)
library(cluster)
library(factoextra)
library(magrittr)
library(tsne)
library(rgl)
library(NbClust)
library(RcppAlgos)
library(RoDiCE)
library(boot)
library(caret)
library(twosamples)
library(HDtest)
library(copula)
library(ggpubr)
library(car)
library(psych)
library(GGally)
library(ggcorrplot)
library(vcd)
library(caTools)
library(multcomp)




###############################
# SET DIRECTORY AND READ FILE #
###############################

figs_dir = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\figs\\"

mydir<- "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\data\\"


data_FHIT3 = readRDS( "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\data_FHIT3")



######################
#  STATISTICAL TESTs #
######################

set.seed(42)


data_FHIT3_ctr <- data_FHIT3 %>%
  filter(Type2 == "Control") 

data_FHIT3_tsa <- data_FHIT3 %>%
  filter(Type2 == "ASD") 

data_FHIT3_dys <- data_FHIT3 %>%
  filter(Type2 == "Dyslexia") 

data_FHIT3_adhd <- data_FHIT3 %>%
  filter(Type2 == "ADHD") 


category = unique(data_FHIT3$Type)
combo_category = comboGrid(category, category, repetition = F)


list_black = list(data_FHIT3_tsa,data_FHIT3_dys,
                  data_FHIT3_adhd,data_FHIT3_ctr )
names(list_black) = category



#  2 sample test on the mean 
t_test_mean_diff_bl= as.data.frame(cbind(combo_category,do.call(rbind,lapply(1: dim(combo_category)[1],
                                                                             function(j) do.call(cbind, lapply(1:6, function(i) 
                                                                               round(t.test(list_black[[combo_category[j,1]]][,4+i],
                                                                                            list_black[[combo_category[j,2]]][,4+i],
                                                                                            alternative = "two.sided", 
                                                                                            var.equal = TRUE)$p.value,3) ))))))


t_test_mean_diff_bl = t_test_mean_diff_bl %>% mutate_at(c('V3', 'V4', 'V5', 'V6', 'V7','V8'), as.numeric)
colnames(t_test_mean_diff_bl) <- c("Type1", "Type2",colnames(data_FHIT3_ctr[5:10]))
t_test_mean_diff_bl



#  welch test
w_test_mean_diff_bl= as.data.frame(cbind(combo_category,do.call(rbind,lapply(1: dim(combo_category)[1],
                                                                             function(j) do.call(cbind, lapply(1:6, function(i) 
                                                                               round(t.test(list_black[[combo_category[j,1]]][,4+i],
                                                                                            list_black[[combo_category[j,2]]][,4+i],
                                                                                            alternative = "two.sided", 
                                                                                            var.equal = FALSE)$p.value,3) ))))))


w_test_mean_diff_bl = w_test_mean_diff_bl %>% mutate_at(c('V3', 'V4', 'V5', 'V6', 'V7','V8'), as.numeric)
colnames(w_test_mean_diff_bl) <- c("Type1", "Type2",colnames(data_FHIT3_ctr[5:10]))
w_test_mean_diff_bl

#  variance ratio test
test_var_ratio_bl = as.data.frame(
  cbind(combo_category,
        do.call(rbind,lapply(1: dim(combo_category)[1],
                             function(j) do.call(cbind, lapply(1:6, function(i) 
                               round(var.test(as.numeric(unlist(list_black[[combo_category[j,1]]][,4+i])),
                                              as.numeric(unlist(list_black[[combo_category[j,2]]][,4+i])),
                                              alternative = "two.sided")$p.value,3) ))))))

test_var_ratio_bl = test_var_ratio_bl %>% mutate_at(c('V3', 'V4', 'V5', 'V6', 'V7','V8'), as.numeric)
colnames(test_var_ratio_bl) <- c("Type1", "Type2",colnames(data_FHIT3_ctr[5:10]))
test_var_ratio_bl

# kolmgorov test
test_kolmo_bl = as.data.frame(
  cbind(combo_category,
        do.call(rbind,lapply(1: dim(combo_category)[1],
                             function(j) do.call(cbind, lapply(1:6, function(i) 
                               round(ks.test(as.numeric(unlist(list_black[[combo_category[j,1]]][,4+i])),
                                             as.numeric(unlist(list_black[[combo_category[j,2]]][,4+i])),
                                             alternative = "two.sided")$p.value,3) ))))))

test_kolmo_bl = test_kolmo_bl %>% mutate_at(c('V3', 'V4', 'V5', 'V6', 'V7','V8'), as.numeric)
colnames(test_kolmo_bl) <- c("Type1", "Type2",colnames(data_FHIT3_ctr[5:10]))
test_kolmo_bl


# CMV test
test_cmv_bl = as.data.frame(
  cbind(combo_category,
        do.call(rbind,lapply(1: dim(combo_category)[1],
                             function(j) do.call(cbind, lapply(1:6, function(i) 
                               round(as.numeric(
                                 cvm_test(as.numeric(unlist(list_black[[combo_category[j,1]]][,4+i])),
                                          as.numeric(unlist(list_black[[combo_category[j,2]]][,4+i]))
                                 )[2]),3) ))))))

test_cmv_bl = test_cmv_bl %>% mutate_at(c('V3', 'V4', 'V5', 'V6', 'V7','V8'), as.numeric)
colnames(test_cmv_bl) <- c("Type1", "Type2",colnames(data_FHIT3_ctr[5:10]))
test_cmv_bl


#  group test multi-null tests -->  ANOVA
#1 - test that  that the variance across groups is equal -->NO
bart_test_bl =  as.data.frame(
  cbind(colnames(data_FHIT3[5:10]),
        do.call(rbind,lapply(1:6, function(i)
          (round(data_FHIT3 %$%  bartlett.test(get(
            colnames(data_FHIT3[,4+i])) ~ as.factor(Type))$p.value,
            3)))) ) )
bart_test_bl = bart_test_bl  %>% mutate_at(c('V2'), as.numeric)
colnames(bart_test_bl) = c("variable", "p.value")
bart_test_bl



#2 -ANOVA
anova_black = as.data.frame(
  cbind(colnames(data_FHIT3[5:10]),
        do.call(rbind,lapply(1:6, function(i)
          (round(summary(data_FHIT3 %$% aov( get(
            colnames(data_FHIT3[,4+i]))~ Type))[[1]][5]$`Pr(>F)`[1],7)))) ) )

#2 - TUKEY
tukey_bl = do.call(cbind,lapply(1:6, function(i)
  round(TukeyHSD(data_FHIT3 %$% aov( get(
    colnames(data_FHIT3[,4+i]))~ Type))$Type[,4],3)  ))

colnames(tukey_bl) = colnames(data_FHIT3[5:10])
tukey_bl



# Covariance
cov_test_bl <- data.frame()

variable_names <- colnames(list_black[[1]][5:10])

for (j in 1:dim(combo_category)[1]) {
  category1 <- combo_category[j, 1]
  category2 <- combo_category[j, 2]
  
  p_values <- list()
  
  combination_names <- list()
  
  for (var1 in 1:6) {
    for (var2 in 1:6) {
      p_value <- round(testCov(list_black[[category1]][, 4 + var1],
                               list_black[[category2]][, 4 + var2])[[1]]$p.value, 3)
      p_values <- c(p_values, p_value)
      
      combination_name <- paste(variable_names[var1], variable_names[var2], sep = " - ")
      combination_names <- c(combination_names, combination_name)
    }
  }
  
  p_value_df <- data.frame(matrix(unlist(p_values), nrow = 1))
  colnames(p_value_df) <- combination_names
  
  p_value_df <- cbind(Type1 = category1, Type2 = category2, p_value_df)
  
  cov_test_bl <- rbind(cov_test_bl, p_value_df)
}

rownames(cov_test_bl) <- NULL  

# copula 2 sample testing 
plot_copula = function(string_type, string_typevstype, data){
  
  df = data.frame( group_id =  rep(string_type, 6),
                   desc  = rep(string_typevstype, 6) ,
                   varname = colnames(data)[c(5:10)])
  
  return(df)
  
}

#no need to run anymore
# result_bl_adhd_tsa = coptest.p(as.matrix(data_FHIT3_adhd[,c(5:10)]) ,
#                                as.matrix(data_FHIT3_tsa[,c(5:10)]) ,
#                                nperm = 100,approx = FALSE)
# 
# result_bl_adhd_tsa$tbl
# 
# 
# result_bl_adhd_dys = coptest.p(as.matrix(data_FHIT3_adhd[,c(5:10)]) ,
#                                as.matrix(data_FHIT3_dys[,c(5:10)]) ,
#                                nperm = 100,approx = FALSE)
# 
# result_bl_adhd_dys$tbl
# 
# 
# 
# result_bl_tsa_dys = coptest.p(as.matrix(data_FHIT3_dys[,c(5:10)]) ,
#                               as.matrix(data_FHIT3_tsa[,c(5:10)]) ,
#                               nperm = 100,approx = FALSE)
# 
# result_bl_tsa_dys$tbl

# saveRDS(result_bl_adhd_tsa,file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_tsa.rds" )
# saveRDS(result_bl_adhd_dys, file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_dys.rds"  ) 
# saveRDS(result_bl_tsa_dys, file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_tsa_dys.rds" )

result_bl_adhd_tsa<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_tsa.rds")
result_bl_adhd_dys<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_dys.rds")
result_bl_tsa_dys<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_tsa_dys.rds")


#################
#  COPULA PLOTS #
#################

toplot7 = plot_copula("ADHD", "TSA vs ADHD", data_FHIT3_adhd) #the data to use correspond to the first string
netvis(obj = result_bl_adhd_tsa,ref = toplot7)

toplot8 = plot_copula("TSA", "TSA vs ADHD", data_FHIT3_tsa) #the data to use correspond to the first string
netvis(obj = result_bl_adhd_tsa,ref = toplot8)



toplot9 = plot_copula("ADHD", "ADHD vs DYS", data_FHIT3_adhd) #the data to use correspond to the first string
netvis(obj = result_bl_adhd_dys,ref = toplot9)

toplot10 = plot_copula("DYS", "ADHD vs DYS", data_FHIT3_dys) #the data to use correspond to the first string
netvis(obj = result_bl_adhd_dys,ref = toplot10)



toplot11 = plot_copula("DYS", "TSA vs DYS", data_FHIT3_dys) #the data to use correspond to the first string
netvis(obj = result_bl_tsa_dys,ref = toplot11)

toplot12 = plot_copula("TSA", "TSA vs DYS", data_FHIT3_tsa) #the data to use correspond to the first string
netvis(obj = result_bl_tsa_dys,ref = toplot12)




#################################
#################################
# FEATURE SELECTION & RANKING ###
#################################
#################################

#mean t test
id_m_bl = which(t_test_mean_diff_bl[c(2,3,6),] < 0.1, arr.ind=TRUE)

stat_dec_mean_bl = cbind(do.call(rbind,
                                 lapply(1:dim(id_m_bl)[1], 
                                        function(i) 
                                          t_test_mean_diff_bl[c(2,3,6),][ id_m_bl[i,1],c(1:2) ])),
                         do.call(rbind,
                                 lapply(1:dim(id_m_bl)[1], 
                                        function(i) 
                                          colnames(t_test_mean_diff_bl[c(2,3,6),])[id_m_bl[i,2]] )))


colnames(stat_dec_mean_bl) = c("Type1", "Type2", "Discr_Var")
stat_dec_mean_bl$Test = rep('T-test', dim(stat_dec_mean_bl)[1])
stat_dec_mean_bl$Significance =   rep(0.1, dim(stat_dec_mean_bl)[1])
stat_dec_mean_bl


#mean welch test
id_wm_bl = which(w_test_mean_diff_bl[c(2,3,6),] < 0.1, arr.ind=TRUE)

stat_dec_w_mean_bl = cbind(do.call(rbind,
                                   lapply(1:dim(id_wm_bl)[1], 
                                          function(i) 
                                            w_test_mean_diff_bl[c(2,3,6),][ id_wm_bl[i,1],c(1:2) ])),
                           do.call(rbind,
                                   lapply(1:dim(id_wm_bl)[1], 
                                          function(i) 
                                            colnames(w_test_mean_diff_bl[c(2,3,6),])[id_wm_bl[i,2]] )))


colnames(stat_dec_w_mean_bl) = c("Type1", "Type2", "Discr_Var")
stat_dec_w_mean_bl$Test = rep('Welch', dim(stat_dec_w_mean_bl)[1])
stat_dec_w_mean_bl$Significance =   rep(0.1, dim(stat_dec_w_mean_bl)[1])
stat_dec_w_mean_bl


#Variance test
id_v_bl = which(test_var_ratio_bl[c(2,3,6),] < 0.10, arr.ind=TRUE)

stat_dec_var_bl = cbind(do.call(rbind,
                                lapply(1:dim(id_v_bl)[1], 
                                       function(i) 
                                         test_var_ratio_bl[c(2,3,6),][ id_v_bl[i,1],c(1:2) ])),
                        do.call(rbind,
                                lapply(1:dim(id_v_bl)[1], 
                                       function(i) 
                                         colnames(test_var_ratio_bl[c(2,3,6),])[id_v_bl[i,2]] )))


colnames(stat_dec_var_bl) = c("Type1", "Type2", "Discr_Var")
stat_dec_var_bl$Test = rep('Variance', dim(stat_dec_var_bl)[1])
stat_dec_var_bl$Significance =   rep(0.10, dim(stat_dec_var_bl)[1])
stat_dec_var_bl


#Distribution test
id_d_bl = which(test_kolmo_bl[c(2,3,6),] < 0.1, arr.ind=TRUE)

stat_dec_dist_bl = cbind(do.call(rbind,
                                 lapply(1:dim(id_d_bl)[1], 
                                        function(i) 
                                          test_kolmo_bl[c(2,3,6),][ id_d_bl[i,1],c(1:2) ])),
                         do.call(rbind,
                                 lapply(1:dim(id_d_bl)[1], 
                                        function(i) 
                                          colnames(test_kolmo_bl[c(2,3,6),])[id_d_bl[i,2]] )))


colnames(stat_dec_dist_bl) = c("Type1", "Type2", "Discr_Var")
stat_dec_dist_bl$Test = rep('Kolmogorov', dim(stat_dec_dist_bl)[1])
stat_dec_dist_bl$Significance =   rep(0.1, dim(stat_dec_dist_bl)[1])
stat_dec_dist_bl


#CMV test
id_cmv_bl = which(test_cmv_bl[c(2,3,6),] < 0.1, arr.ind=TRUE)

stat_dec_cmv_bl = cbind(do.call(rbind,
                                lapply(1:dim(id_cmv_bl)[1], 
                                       function(i) 
                                         test_cmv_bl[c(2,3,6),][ id_cmv_bl[i,1],c(1:2) ])),
                        do.call(rbind,
                                lapply(1:dim(id_cmv_bl)[1], 
                                       function(i) 
                                         colnames(test_cmv_bl[c(2,3,6),])[id_cmv_bl[i,2]] )))


colnames(stat_dec_cmv_bl) = c("Type1", "Type2", "Discr_Var")
stat_dec_cmv_bl$Test = rep('CVM', dim(stat_dec_cmv_bl)[1])
stat_dec_cmv_bl$Significance =   rep(0.1, dim(stat_dec_cmv_bl)[1])
stat_dec_cmv_bl

#Bartlett test
id_bart_bl = which(bart_test_bl[c(2,3,6),] < 0.1, arr.ind=TRUE)

stat_dec_bart_bl = do.call(rbind,
                           lapply(1:dim(id_bart_bl)[1], 
                                  function(i) 
                                    bart_test_bl[c(2,3,6),][ id_bart_bl[i,1],c(1:2) ]))


colnames(stat_dec_bart_bl) = c("Discr_Var", "Significance")
stat_dec_bart_bl$Test = rep('Bartlett', dim(stat_dec_bart_bl)[1])
stat_dec_bart_bl$Type1 =   rep("All", dim(stat_dec_bart_bl)[1])
stat_dec_bart_bl$Type2 =  rep("All", dim(stat_dec_bart_bl)[1])
stat_dec_bart_bl

stat_dec_bart_bl <- stat_dec_bart_bl[colnames(stat_dec_cmv_bl)]
stat_dec_bart_bl



#TUKEY test
id_t_bl = which(tukey_bl[c(2,3,6),] < 0.1, arr.ind=TRUE)

stat_dec_tukey_bl = as.data.frame(cbind(do.call(rbind,
                                                lapply(1:dim(id_t_bl)[1], 
                                                       function(i) 
                                                         rownames(tukey_bl[c(2,3,6),])[ id_t_bl[i,1]])),
                                        do.call(rbind,
                                                lapply(1:dim(id_t_bl)[1], 
                                                       function(i) 
                                                         colnames(tukey_bl[c(2,3,6),])[id_t_bl[i,2]] ))) )

colnames(stat_dec_tukey_bl) = c("Combination", "Discr_Var")
stat_dec_tukey_bl$Test = rep('Tukey', dim(stat_dec_tukey_bl)[1])
stat_dec_tukey_bl$Significance =   rep(0.1, dim(stat_dec_tukey_bl)[1])
stat_dec_tukey_bl

new_types_bl = do.call(rbind,lapply(1:dim(stat_dec_tukey_bl)[1], function(i) 
  str_split_fixed(stat_dec_tukey_bl$Combination[i], '-', 2)))
colnames(new_types_bl) = c("Type1","Type2")
stat_dec_tukey_bl = cbind(new_types_bl,stat_dec_tukey_bl[,-1])
stat_dec_tukey_bl


#COVARIANCE test
id_cov_bl = which(cov_test_bl[c(2,3,6),] < 0.1, arr.ind=TRUE)

stat_dec_cov_bl = cbind(do.call(rbind,
                                lapply(1:dim(id_cov_bl)[1], 
                                       function(i) 
                                         cov_test_bl[c(2,3,6),][ id_cov_bl[i,1],c(1:2) ])),
                        do.call(rbind,
                                lapply(1:dim(id_cov_bl)[1], 
                                       function(i) 
                                         colnames(cov_test_bl[c(2,3,6),])[id_cov_bl[i,2]] )))

colnames(stat_dec_cov_bl) = c("Type1", "Type2", "Discr_Var")
stat_dec_cov_bl$Test = rep('Covariance', dim(stat_dec_cov_bl)[1])
stat_dec_cov_bl$Significance =   rep(0.1, dim(stat_dec_cov_bl)[1])
stat_dec_cov_bl


#COPULA test
stat_dec_copula_bl = rbind(do.call(rbind,
                                   lapply(1:length(which(result_bl_adhd_tsa$tbl[,4] < 0.01, arr.ind=TRUE)),function(i)
                                     cbind(cbind("ADHD", "TSA"),
                                           result_bl_adhd_tsa$tbl[which(result_bl_adhd_tsa$tbl[,4] < 0.01, 
                                                                        arr.ind=TRUE),][,1][i]))),
                           
                           do.call(rbind,
                                   lapply(1:length(which(result_bl_tsa_dys$tbl[,4] < 0.01, arr.ind=TRUE)),function(i)
                                     cbind(cbind("DYS", "TSA"),
                                           result_bl_tsa_dys$tbl[which(result_bl_tsa_dys$tbl[,4] < 0.05, 
                                                                       arr.ind=TRUE),][,1][i]))),
                           do.call(rbind,
                                   lapply(1:length(which(result_bl_adhd_dys$tbl[,4] < 0.01, arr.ind=TRUE)),function(i)
                                     cbind(cbind("ADHD","DYS"),
                                           result_bl_adhd_dys$tbl[which(result_bl_adhd_dys$tbl[,4] < 0.01, 
                                                                        arr.ind=TRUE),][,1][i]))) )

colnames(stat_dec_copula_bl)<- c("Type1","Type2", "Discr_Var")
stat_dec_copula_bl = as.data.frame(stat_dec_copula_bl)
stat_dec_copula_bl$Test = rep('Copula', dim(stat_dec_copula_bl)[1])
stat_dec_copula_bl$Significance = c(rep(0.01, length(which(result_bl_adhd_tsa$tbl[,4] < 0.01, arr.ind=TRUE))),
                                    rep(0.01, length(which(result_bl_tsa_dys$tbl[,4] < 0.01, arr.ind=TRUE))),
                                    rep(0.01, length(which(result_bl_adhd_dys$tbl[,4] < 0.01, arr.ind=TRUE))))
stat_dec_copula_bl




############################################################
# FINAL FEATURES BASED ON STATISTICAL TESTS SIGNIFICIANCE #
###########################################################


final_dec_bl = rbind(stat_dec_mean_bl,
                     stat_dec_w_mean_bl,
                     stat_dec_var_bl,
                     stat_dec_dist_bl,
                     stat_dec_cmv_bl,
                     stat_dec_bart_bl,
                     stat_dec_tukey_bl,
                     stat_dec_cov_bl,
                     stat_dec_copula_bl)


final_dec_bl = final_dec_bl %>%
  mutate(Type1_new = if_else(Type1 == 'TSA' & Type2 == 'ADHD', Type2, Type1),
         Type2_new = if_else(Type1 == 'TSA' & Type2 == 'ADHD', Type1, Type2)) %>% 
  mutate(Type1 = Type1_new,Type2 = Type2_new)
final_dec_bl = final_dec_bl[,-c(6,7)]

final_dec_bl = final_dec_bl %>% 
  mutate(
    # Create categories
    Type3 = dplyr::case_when(
      Type1 == "TSA"  ~ "ASD",
      Type1 == "DYS"  ~ "DYS",
      Type1 == "ADHD"  ~ "ADHD",
      Type1 == "CTR"  ~ "Control"
    ),
    # Convert to factor
    Type3 = factor(
      Type3,
      level = c("ASD","DYS", "ADHD", "Control")
    )
  )


final_dec_bl = final_dec_bl %>% 
  mutate(
    # Create categories
    Type4 = dplyr::case_when(
      Type2 == "TSA"  ~ "ASD",
      Type2 == "DYS"  ~ "DYS",
      Type2 == "ADHD"  ~ "ADHD",
      Type2 == "CTR"  ~ "Control"
    ),
    # Convert to factor
    Type4 = factor(
      Type4,
      level = c("ASD","DYS", "ADHD", "Control")
    )
  )


final_dec_bl$Combination = paste(final_dec_bl$Type3,'-',final_dec_bl$Type4)

final_dec_bl2 = final_dec_bl[,-c(1,2,6,7)]
final_dec_bl2
##############
#better format
##############
final_dec_bl2[31,1] = "L. Post. - R. Ant."
final_dec_bl2[32,1] = "R. Post. - R. Ant."
final_dec_bl2[33,1] = "L. Lat. - L. Post."
final_dec_bl2[34,1] = "L. Ant. - R. Ant."
final_dec_bl2[35,1] = "L. Lat. - R. Lat."


final_dec_bl2

final_dec_bl2[13,4] = "ALL"
final_dec_bl2[14,4] = "ALL"
final_dec_bl2[15,4] = "ALL"

final_dec_bl2

#PLOTS FINAL DECISION 

prova2 = final_dec_bl2 %>% complete(Test ,Combination)

pdf( paste(figs_dir, "heat_black.pdf", sep = ""), width = 14, height = 11 )  
ggplot(data = prova2, aes(x = factor(Combination),
                          y = Test, 
                          fill = Significance)) +
  geom_tile(aes(fill = Significance),
            colour = 'black') +
  scale_fill_gradientn("Significance",
                       colours = c("antiquewhite1", "bisque", "darkorange"),
                       breaks=c(0.01,0.05,0.1,0.15),
                       guide = "legend",
                       na.value = "azure") + 
  ggrepel::geom_text_repel(data = prova2,
                           aes(label = Discr_Var),
                           size = 4.4,
                           direction = "y",  
                           box.padding = unit(0.09, "lines"),
                           min.segment.length = 1.9)+
  theme_bw()+  
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="bottom",
        axis.text=element_text(size=15),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15)) + xlab("Disorders Pair Tested")  + ylab("Tests")  
dev.off()

#the same if we consider only one variable for DYS-TSA
ggplot(data = prova2, aes(x = factor(Combination),
                          y = Test, 
                          fill = Significance)) +
  geom_tile(aes(fill = Significance),
            colour = 'black') +
  scale_fill_gradient2("Significance",
                       low = "cyan", mid = "white", high = "blue",breaks=c(0.01,0.05,0.1,0.17),
                       guide = "legend") + 
  geom_text(data = prova2, check_overlap = T,
            aes(label = Discr_Var),
            size = 3)+
  theme_bw()+  
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="right") + xlab("Types")  + xlab("Types")  




hist_vars = final_dec_bl2  %>% group_by( Discr_Var )   %>% 
  count(Combination) %>% 
  mutate(percent = n/sum(n),
         n = n)

pdf( paste(figs_dir, "hist_vars.pdf", sep = ""))  
ggplot(hist_vars,
       aes(x = Discr_Var, y = n, fill = Combination)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = TRUE, vjust = 0.2),
             hjust = 0.6, vjust = 0.1, show.legend = FALSE, size = 8)  +
  #scale_fill_grey() +
  xlab("Discriminant Variable") + ylab("Significant Statistical Tests")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        axis.text=element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15)) +
  scale_fill_manual(values=c("azure3", "lightblue", "cyan", "blue")) 
dev.off()


hist_tests = final_dec_bl2  %>% group_by( Test )   %>% 
  count(Combination) %>% 
  mutate(percent = n/sum(n),
         n = n)

pdf( paste(figs_dir, "hist_tests.pdf", sep = ""))  
ggplot(hist_tests,
       aes(x =  Test, y = n, fill = Combination)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = TRUE),
             vjust = 1,show.legend = FALSE, size = 7)  +
  #scale_fill_grey() +
  xlab("Tests") + ylab("Significant Attributes")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        axis.text=element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15)) +
  scale_fill_manual(values=c("azure3", "lightblue", "cyan", "blue")) 
dev.off()


#######################
#######################
# FEATURE ENGINEERING #
#######################
#######################

final_dec_bl2


#Bootrsap dataset
tosample = as.data.frame(unique(cbind(final_dec_bl2$Discr_Var, final_dec_bl2$Test)))
tosample$Type = c('mean','mean','mean','mean',
                  'var', 'distr','distr','distr','distr',
                  'var_multi', 'var_multi', 'var_multi', 'tuk_multi',
                  "cov_multi", "cov_multi", "cov_multi", "cov_multi", 
                  "cov_multi", "cov_multi", "cov_multi", "cov_multi",
                  "cov_multi", "cov_multi", 
                  "cop_multi", "cop_multi", "cop_multi", "cop_multi", "cop_multi")
tosample =  as.data.frame(unique(cbind(tosample$V1,tosample$Type)))
colnames(tosample) = c("variable", "Type")

tosample = tosample %>% filter(!(Type == "var_multi" & variable %in% variable[Type == "var"]))
tosample = tosample %>% filter(!(Type == "tuk_multi" & variable %in% variable[Type == "mean"]))


var_names_to_exclude <- unique(tosample[tosample$Type %in% c("var", "var_multi"), "variable"])

tosample <- tosample[!(tosample$Type == "cov_multi" &
                         grepl(paste(var_names_to_exclude, collapse = "|"), tosample$variable)), ]

rownames(tosample) <- NULL

tosample$stat = c("meanfun","meanfun","varfun", "datafun", "datafun", "varfun",
                  "varfun", "corrfun", "rank_transformfun", "rank_transformfun",
                  "rank_transformfun","rank_transformfun","rank_transformfun")
tosample$R = c(20,20,20,20,20,20,20,20,20,20,20,20,20)

tosample = tosample[-8,]

tosample

list_cat_child = list(data_FHIT3_adhd,
                      data_FHIT3_ctr,
                      data_FHIT3_dys,
                      data_FHIT3_tsa)

#look at the distribution of the data
df_melted = lapply(1:4, function(i) melt(list_cat_child[[i]][,c(3,5:10)]) )

df_all = do.call(rbind,df_melted) 


ggdensity(df_all, x = "value",
          add = "mean", rug = TRUE,
          color = "variable", fill = "variable",
          facet.by = "variable") + facet_wrap(~Type, ncol = 4) + 
  theme(legend.position = "bottom")


ggdensity(df_all, x = "value",
          add = "mean", rug = TRUE,
          color = "variable", fill = "variable") + facet_grid(variable~Type) + 
  theme(legend.position = "bottom")


#Functions for bootstraping

meanfun <- function(x, d) {
  return(mean(x[d], na.rm=TRUE))
}

varfun <- function(x, d) {
  return(var(x[d], na.rm=TRUE))
}

datafun <- function(data, indices) {
  resampled_data <- data[indices]
  return(resampled_data)
}

rank_transformfun <- function(data, indices) {
  bootstrap_data <- data[indices, ]
  rank_vector <- c(rank(bootstrap_data[, 1]), rank(bootstrap_data[, 2]))
  return(rank_vector)
}


## ## ## ## ##  ## 
## NOT NEEDED  ##
## ## ## ## ## ## 
# corrfun <- function(data, indices) { #not needed
#   bootstrap_data <- data[indices, ]
#   correlation_coefficient <- cor(bootstrap_data[, 1], bootstrap_data[, 2])
#   return(correlation_coefficient)
# }
# 
# 
# ecdffun <- function(data, indices) {  #not needed
#   bootstrap_sample <- data[indices]
#   ecdf_result <- ecdf(bootstrap_sample)
#   return(as.numeric(ecdf_result(data)))
# }


# mle_cov_multi <- function(data) {
#   if (!is.matrix(data)) {
#     data <- as.matrix(data)
#   }
#   
#   cov_matrix <- cov(data, use = "pairwise.complete.obs")
#   
#   if (is.null(dim(cov_matrix)) || any(is.na(cov_matrix)) || nrow(cov_matrix) < 2 || ncol(cov_matrix) < 2) {
#     # Handle cases where cov_matrix is NULL, contains NAs, or is too small
#     return(list(estimate = NA, sd = NA))
#   }
#   
#   return(list(estimate = cov_matrix[1, 2], sd = NA)) # No sd for covariance
# }


###################
# PARAMETRIC BOOT #
##################

# MLE functions
# mle_meanfun <- function(data) {
#   estimate <- mean(data, na.rm = TRUE)
#   sd_estimate <- sd(data, na.rm = TRUE)
#   return(list(estimate = estimate, sd = sd_estimate))
# }
# 
# mle_varfun <- function(data) {
#   estimate <- var(data, na.rm = TRUE)
#   sd_estimate <- sqrt(estimate) # Calculate the standard deviation from variance
#   return(list(estimate = estimate, sd = sd_estimate))
# }
# 
# mle_datafun <- function(data) {
#   return(sample(data, replace = TRUE))
# }
# 
# 
# mle_cop_multi <- function(data) {
#   rank_data <- data
#   rank_data[, 1:2] <- apply(data[, 1:2], 2, rank)
#   return(data.frame(rank_data))  # Return the data frame with ranked statistics
# }


###################
#CASE 1 --> NORMAL#
###################


my.mle_fun<- function(data){
  estimate <- mean(data, na.rm = TRUE)
  sd_estimate <- sd(data, na.rm = TRUE)
  return(list(estimate = estimate, sd = sd_estimate))
}


ran.gen_mle <- function(n, estimate, sd_estimate) {
  # Generate random samples using a normal distribution
  # based on the MLE estimate and standard deviation estimate
  if (is.na(sd_estimate) || sd_estimate <= 0) {
    return(rep(mean_estimate,n))
  } else {
    samples <- rnorm(n, mean = estimate, sd = sd_estimate)
    return(samples)
  }
  
}




custom_boot <- function(data, indices, mle_function, ran.gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))  
  
  mle_estimate <- mle_function(data[indices])
  
  if (is.numeric(mle_estimate)) {
    # Handle cases where mle_estimate is an atomic vector (numeric)
    estimate <- mle_estimate
    sd_estimate <- NA  # No standard deviation available
  } else {
    # Handle cases where mle_estimate is a list with "estimate" and "sd"
    estimate <- mle_estimate$estimate
    sd_estimate <- mle_estimate$sd
  }
  
  resampled_data <- ran.gen_function(length(indices), estimate, sd_estimate)
  
  return(get(stat)(resampled_data))
}



custom_boot2 <- function(data, indices, mle_function, ran.gen_function) {
  if (length(indices) == 0) return(rep(NA, length(data)))  
  
  mle_data <- mle_function(data[indices, ])
  
  if (is.data.frame(mle_data)) {
    # Handle cases where mle_data is a data frame with ranked statistics
    resampled_data1 <- ran.gen_function(length(indices), mle_data$estimate[1], mle_data$sd[1])
    resampled_data2 <- ran.gen_function(length(indices), mle_data$estimate[2], mle_data$sd[2])
    resampled_data <- cbind(resampled_data1, resampled_data2)
  } else if (is.list(mle_data)) {
    # Handle cases where mle_data is a list with "estimate" and "sd"
    resampled_data1 <- ran.gen_function(length(indices), mle_data$estimate, mle_data$sd)
    resampled_data2 <- ran.gen_function(length(indices), mle_data$estimate, mle_data$sd)
    resampled_data <- cbind(resampled_data1, resampled_data2)
  } else if (is.numeric(mle_data)) {
    # Handle cases where mle_data is an atomic vector (numeric)
    resampled_data <- ran.gen_function(length(indices), mle_data, NA)
  } else {
    stop("Unsupported mle_data format")
  }
  
  return(get(stat)(resampled_data))
}

#################
#CASE 2 --> BETA#
#################

beta_mle_fun <- function(data) {
  # Calculate MLE parameters for a beta distribution
  shape1 <- sum(data) + 1
  shape2 <- length(data) - sum(data) + 1
  return(list(shape1 = shape1, shape2 = shape2))
}


ran.gen_mle_beta <- function(n, shape1, shape2) {
  if (is.na(shape2) || is.na(shape1) || shape2 <= 0) {
    return(rep(shape1, n))
  } else {
    samples <- rbeta(n, shape1 = shape1, shape2 = shape2)
    # Scale the resample to the 0-100% range if needed
    scaled_resample <- samples * 100
    return(scaled_resample)
  }
}





#LOOP FOR BOOTSTRAPING


# Initialize an empty list to store bootstrap results
bootstrap_results <- list()

# Iterate through the rows of the tosample data frame
for (i in 1:nrow(tosample)) {
  variable <- tosample$variable[i]
  stat <- tosample$stat[i]
  R <- tosample$R[i]
  type <- tosample$Type[i]  # Get the Type
  
  # Define the appropriate mle_function based on Type and stat
  if (type == "mean" && stat == "meanfun") {
    mle_function <-  beta_mle_fun # my.mle_fun
  } else if (type == "var" && stat == "varfun") {
    mle_function <-  beta_mle_fun # my.mle_fun
  } else if (type == "distr" && stat == "datafun") {
    mle_function <-  beta_mle_fun # my.mle_fun
  } else if (type == "var_multi" && stat == "varfun") {
    mle_function <-  beta_mle_fun # my.mle_fun
  } else if (type == "cop_multi" && stat == "rank_transformfun") {
    mle_function <-  beta_mle_fun # my.mle_fun
  } else {
    stop("Unsupported combination of Type and stat")
  }
  
  # Check if the variable involves subtraction
  if (grepl("-", variable)) {
    # Split the variable into two components
    components <- strsplit(variable, " - ")[[1]]
    
    # Apply bootstrapping to each data frame and store the results for both components
    bootstrap_result <- lapply(list_cat_child, function(df) {
      result <- boot(cbind(df[[components[1]]], df[[components[2]]]),
                     R = R,
                     sim = "parametric", 
                     statistic = custom_boot2,
                     mle_function = mle_function,
                     ran.gen_function = ran.gen_mle_beta, #ran.gen_mle
                     indices = sample(nrow(df), replace = TRUE))$t
      return(list(result))
    })
  } else {
    # Apply bootstrapping to each data frame and store the results for the single variable
    bootstrap_result <- lapply(list_cat_child, function(df) {
      result <- boot(df[[variable]], 
                     R = R,
                     sim = "parametric", 
                     statistic = custom_boot,   
                     mle_function = mle_function,
                     ran.gen_function = ran.gen_mle_beta, #ran.gen_mle
                     indices = sample(nrow(df), replace = TRUE))$t
      return(list(result))
    })
  }
  
  # Check if Type is "distr" or "cop_multi" and extract the first column if true
  if (type %in% c("distr", "cop_multi")) {
    bootstrap_result <- lapply(bootstrap_result, function(result_list) {
      result <- sapply(result_list, function(result_df) result_df[, 1])
      return(list(result))
    })
  }
  
  # Combine the bootstrap results into a single data frame
  bootstrap_results[[i]] <- as.data.frame(do.call(cbind, bootstrap_result))
}



# Combine all the bootstrap results into a final data frame
new_data_param = do.call(rbind,lapply(1:4, function(i) do.call(cbind,sapply(bootstrap_results, "[[", i))))

col_names_newdata <- paste(tosample$variable, tosample$Type, sep = "_")
col_names_newdata <- gsub("[. ]", "_", col_names_newdata)
col_names_newdata <- gsub("[__-]", "", col_names_newdata)

colnames(new_data_param) <- col_names_newdata
new_data_param = as.data.frame(new_data_param)
new_data_param$Type = c(sapply(1:4, function(i) rep(unique(list_cat_child[[i]]$Type), 20 )))



new_data_param = new_data_param %>% 
  mutate(
    # Create categories
    color = dplyr::case_when(
      Type == "TSA"  ~ "pink",
      Type == "DYS"  ~ "green",
      Type == "ADHD"  ~ "yellow",
      Type == "CTR"  ~ "orange"
    ),
    # Convert to factor
    color = factor(
      color,
      level = c("pink","green", "yellow", "orange")
    )
  )


head(new_data_param)

saveRDS(new_data_param, 
        file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\new_data_param.rds")

########################
########################
# non-PARAMETRIC BOOT #
#######################
########################

# Initialize an empty list to store bootstrap results
bootstrap_results <- list()

# Iterate through the rows of the tosample data frame
for (i in 1:nrow(tosample)) {
  variable <- tosample$variable[i]
  stat <- tosample$stat[i]
  R <- tosample$R[i]
  type <- tosample$Type[i]  # Get the Type
  
  # Check if the variable involves subtraction
  if (grepl("-", variable)) {
    # Split the variable into two components
    components <- strsplit(variable, " - ")[[1]]
    
    # Apply bootstrapping to each data frame and store the results for both components
    bootstrap_result <- lapply(list_cat_child, function(df) {
      result <- boot(cbind(df[[components[1]]], df[[components[2]]]), statistic = get(stat), R = R)$t
      return(list(result))
    })
  } else {
    # Apply bootstrapping to each data frame and store the results for the single variable
    bootstrap_result <- lapply(list_cat_child, function(df) {
      result <- boot(df[[variable]], statistic = get(stat), R = R)$t
      return(list(result))
    })
  }
  
  # Check if Type is "distr" or "cop_multi" and extract the first column if true
  if (type %in% c("distr", "cop_multi")) {
    bootstrap_result <- lapply(bootstrap_result, function(result_list) {
      result <- sapply(result_list, function(result_df) result_df[, 1])
      return(list(result))
    })
  }
  
  # Combine the bootstrap results into a single data frame
  bootstrap_results[[i]] <- as.data.frame(do.call(cbind, bootstrap_result))
}


# Combine all the bootstrap results into a final data frame
new_data_nonparm <- do.call(rbind,lapply(1:4, function(i) do.call(cbind,sapply(bootstrap_results, "[[", i))))

col_names_newdata <- paste(tosample$variable, tosample$Type, sep = "_")
col_names_newdata <- gsub("[. ]", "_", col_names_newdata)
col_names_newdata <- gsub("[__-]", "", col_names_newdata)

colnames(new_data_nonparm) <- col_names_newdata
new_data_nonparm = as.data.frame(new_data_nonparm)
new_data_nonparm$Type = c(sapply(1:4, function(i) rep(unique(list_cat_child[[i]]$Type), 20 )))


new_data_nonparm = new_data_nonparm %>% 
  mutate(
    # Create categories
    color = dplyr::case_when(
      Type == "TSA"  ~ "pink",
      Type == "DYS"  ~ "green",
      Type == "ADHD"  ~ "yellow",
      Type == "CTR"  ~ "orange"
    ),
    # Convert to factor
    color = factor(
      color,
      level = c("pink","green", "yellow", "orange")
    )
  )


head(new_data_nonparm)


#new_data_nonparm = new_data_nonparm[,-8] #CHECK HERE !!!!!!  -- removed above!!!
#head(new_data_nonparm)

saveRDS(new_data_nonparm, 
        file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\new_data_nonparam.rds")


#########
# RTSNE #
#########

set.seed(42)

#by experiments
new_data_bl_unique<- new_data_param    # new_data_nonparm
new_data_bl_unique <- unique(new_data_bl_unique)

tsne_bl <- Rtsne(new_data_bl_unique[,c(1:12)], 
                 dims = 2,
                 pca = TRUE,
                 perplexity = 10)

toplot_bl = as.data.frame(tsne_bl$Y)
toplot_bl$color = new_data_bl_unique$color
toplot_bl$Type = new_data_bl_unique$Type
colnames(toplot_bl) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_bl, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))

rm(tsne3d)
tsne3d <- tsne(new_data_bl_unique[,c(1:3)], 
               initial_config = NULL, 
               k = 3, 
               initial_dims = 30, 
               perplexity = 35,
               max_iter = 1000,
               min_cost = 0, 
               epoch_callback = NULL, 
               whiten = TRUE,
               epoch=300)
tsne3d <- cbind(tsne3d,new_data_bl_unique[,1])

par3d(windowRect = c(100, 100, 612, 612))
plot3d(x = tsne3d[,1], 
       y= tsne3d[,2], 
       z= tsne3d[,3], 
       type = "s", 
       radius = 0.5,
       xlab="Dim.1", ylab="Dim.2", zlab="Dim.3", col = new_data_bl_unique$color )
legend3d("topright", legend = c("ASD","DYS","ADHD","CTR"),
         pch = 16, col = c("pink", "yellow", "green", "orange"), cex=0.8)
rgl.snapshot(paste(figs_dir, "bl_tsne_features.png", sep = ""), fmt = 'png')


