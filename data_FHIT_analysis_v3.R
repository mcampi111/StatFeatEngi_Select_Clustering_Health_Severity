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
library(rgl)
library(caret)



###############################
# SET DIRECTORY AND READ FILE #
###############################

figs_dir = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\figs\\"

mydir<- "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\data\\"



data_FHIT<- read_excel(paste(mydir, "dataML.xlsx", sep = ""),
                       sheet = 4, col_names = T) 


################
#DATA wrangling#  
################

data_FHIT2 = data_FHIT

colnames(data_FHIT2)[1:4] <-  data_FHIT2[2,1:4]
data_FHIT2[2,1:4] <- c(NA,NA,NA,NA)
data_FHIT2<- data_FHIT2[,-c(17:52)]
colnames(data_FHIT2)[5:16]<- paste(data_FHIT2[1,], data_FHIT2[2,], sep = "_")[-c(1:4)]
colnames(data_FHIT2)[5:10]<- paste(colnames(data_FHIT2)[5:10], "rf", sep = '_')
colnames(data_FHIT2)[11:16]<- paste(colnames(data_FHIT2)[11:16], "bl", sep = '_')
data_FHIT2<- data_FHIT2[-c(1:2),]


data_FHIT2 = data_FHIT2 %>% 
  mutate_at(c(4:16), as.numeric)

################
#Missing values#  
################

vis_miss(data_FHIT2)

data_FHIT3 <- na.omit(data_FHIT2)
vis_miss(data_FHIT3)


#################
# ADD VARIABLES #  
#################


range(data_FHIT3$Age)

data_FHIT3 = data_FHIT3 %>% 
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      Age > 6 & Age <= 8  ~ "6-8",
      Age > 8 & Age <= 10 ~ "8-10",
      Age > 10 & Age <= 12 ~ "10-12",
      Age > 12             ~ "> 12"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("6-8", "8-10","10-12", "> 12")
    )
  )



data_FHIT3 = data_FHIT3 %>% 
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



data_FHIT3 = data_FHIT3[,c(1:4, 11:18 )]

colnames(data_FHIT3)[5:10] = c("L. Lat.",  "R. Lat.", "L. Post.",  "R. Post.", 
                               "L. Ant.",  "R. Ant.")

data_FHIT3 = data_FHIT3 %>% 
  mutate(
    # Create categories
    Type2 = dplyr::case_when(
      Type == "TSA"  ~ "ASD",
      Type == "DYS"  ~ "Dyslexia",
      Type == "ADHD"  ~ "ADHD",
      Type == "CTR"  ~ "Control"
    ),
    # Convert to factor
    Type2 = factor(
      Type2,
      level = c("ASD","Dyslexia", "ADHD", "Control")
    )
  )



#################
# INITIAL PLOTS #  
#################

#counf ot PEOPLE
data_FHIT3_hist0<- data_FHIT3 %>% 
  count(Type2) %>% 
  mutate(percent = n/sum(n), n = n)

data_FHIT3_hist0$color = c("pink","green", "yellow", "orange")

#counf ot PEOPLE by age
data_FHIT3_hist1<- data_FHIT3 %>% 
  count(Type2,age_group) %>% 
  mutate(percent = n/sum(n), n = n)


#histrograms 
#1) by condition
pdf( paste(figs_dir, "hist_children.pdf", sep = ""))  
ggplot(data_FHIT3_hist0,
       aes(x = Type2, y = n, fill = Type2)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = TRUE),
             vjust = 1,show.legend = FALSE, size = 7)  +
  #scale_fill_grey() +
  xlab("Disorder Type") + ylab("Number of children")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) +
  scale_fill_manual(values=data_FHIT3_hist0$color) 

dev.off()




# BOXPLOTS 

bxpl_data_FHIT3_black = melt(data_FHIT3[,-c(1,2,3,4,11,12)], id.vars = "Type2")

pdf( paste(figs_dir, "box_black.pdf", sep = ""))  
ggplot(bxpl_data_FHIT3_black, 
       aes(x = variable, y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_wrap(~Type2, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        # axis.title.x=element_blank(),
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank(),
        strip.text.x = element_text(size = 14)) + ylab("")  + xlab("") 
dev.off()


#######################
# RTNSE ORIGINAL DATA #
#######################
data_FHIT3_black <- data_FHIT3


set.seed(42)


data_FHIT3_black_unique<- data_FHIT3_black
data_FHIT3_black_unique <- unique(data_FHIT3_black_unique[,c(3,5:10,12)])

tsne_out_data_FHIT3_black <- Rtsne(data_FHIT3_black_unique[,c(2:7)], 
                                   dims = 2,
                                   pca = TRUE,
                                   perplexity = 10)

toplot_FHIT3_black = as.data.frame(tsne_out_data_FHIT3_black$Y)
toplot_FHIT3_black$color = data_FHIT3_black_unique$color
toplot_FHIT3_black$Type = data_FHIT3_black_unique$Type
colnames(toplot_FHIT3_black) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_FHIT3_black, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))



tsne3d <- tsne(data_FHIT3_black_unique[,c(2:7)], 
               initial_config = NULL, 
               k = 3, 
               initial_dims = 30, 
               perplexity = 35,
               max_iter = 1000,
               min_cost = 0, 
               epoch_callback = NULL, 
               whiten = TRUE,
               epoch=300)
tsne3d <- cbind(tsne3d,toplot_FHIT3_black[,1])


par3d(windowRect = c(100, 100, 612, 612))
plot3d(x = tsne3d[,1], 
       y= tsne3d[,2], 
       z= tsne3d[,3], 
       type = "s", 
       radius = 4,
       xlab="Dim.1", ylab="Dim.2", zlab="Dim.3", col = data_FHIT3_black_unique$color )
legend3d("topright", legend = c("ASD","DYS","ADHD","CTR"),
         pch = 16, col = c("pink", "yellow", "green", "orange"), cex=0.8)
rgl.snapshot(paste(figs_dir, "black_tsne.png", sep = ""), fmt = 'png')



######################
#  STATISTICAL TESTs #
######################

data_FHIT3_black_ctr <- data_FHIT3_black %>%
  filter(Type2 == "Control") 

data_FHIT3_black_tsa <- data_FHIT3_black %>%
  filter(Type2 == "ASD") 

data_FHIT3_black_dys <- data_FHIT3_black %>%
  filter(Type2 == "Dyslexia") 

data_FHIT3_black_adhd <- data_FHIT3_black %>%
  filter(Type2 == "ADHD") 


category = unique(data_FHIT3$Type)
combo_category = comboGrid(category, category, repetition = F)


list_black = list(data_FHIT3_black_tsa,data_FHIT3_black_dys,
                  data_FHIT3_black_adhd,data_FHIT3_black_ctr )
names(list_black) = category



#  2 sample test on the mean 
test_mean_diff_bl= as.data.frame(cbind(combo_category,do.call(rbind,lapply(1: dim(combo_category)[1],
                                                                           function(j) do.call(cbind, lapply(1:6, function(i) 
                                                                             round(t.test(list_black[[combo_category[j,1]]][,4+i],
                                                                                          list_black[[combo_category[j,2]]][,4+i],
                                                                                          alternative = "two.sided", 
                                                                                          var.equal = FALSE)$p.value,3) ))))))


test_mean_diff_bl = test_mean_diff_bl %>% mutate_at(c('V3', 'V4', 'V5', 'V6', 'V7','V8'), as.numeric)
colnames(test_mean_diff_bl) <- c("Type1", "Type2",colnames(data_FHIT3_black_ctr[5:10]))
test_mean_diff_bl


#  variance ratio test
test_var_ratio_bl = as.data.frame(
  cbind(combo_category,
        do.call(rbind,lapply(1: dim(combo_category)[1],
                             function(j) do.call(cbind, lapply(1:6, function(i) 
                               round(var.test(as.numeric(unlist(list_black[[combo_category[j,1]]][,4+i])),
                                              as.numeric(unlist(list_black[[combo_category[j,2]]][,4+i])),
                                              alternative = "two.sided")$p.value,3) ))))))

test_var_ratio_bl = test_var_ratio_bl %>% mutate_at(c('V3', 'V4', 'V5', 'V6', 'V7','V8'), as.numeric)
colnames(test_var_ratio_bl) <- c("Type1", "Type2",colnames(data_FHIT3_black_ctr[5:10]))
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
colnames(test_kolmo_bl) <- c("Type1", "Type2",colnames(data_FHIT3_black_ctr[5:10]))
test_kolmo_bl

#  group test multi-null tests -->  ANOVA

#1 - test that  that the variance across groups is equal -->NO
bart_test_bl =  as.data.frame(
  cbind(colnames(data_FHIT3_black[5:10]),
        do.call(rbind,lapply(1:6, function(i)
          (round(data_FHIT3_black %$%  bartlett.test(get(
            colnames(data_FHIT3_black[,4+i])) ~ as.factor(Type))$p.value,
            3)))) ) )
bart_test_bl = bart_test_bl  %>% mutate_at(c('V2'), as.numeric)
colnames(bart_test_bl) = c("variable", "p.value")
bart_test_bl

#2 -ANOVA
anova_black = as.data.frame(
  cbind(colnames(data_FHIT3_black[5:10]),
        do.call(rbind,lapply(1:6, function(i)
          (round(summary(data_FHIT3_black %$% aov( get(
            colnames(data_FHIT3_black[,4+i]))~ Type))[[1]][5]$`Pr(>F)`[1],7)))) ) )

#2 - TUKEY
tukey_bl = do.call(cbind,lapply(1:6, function(i)
  round(TukeyHSD(data_FHIT3_black %$% aov( get(
    colnames(data_FHIT3_black[,4+i]))~ Type))$Type[,4],3)  ))

colnames(tukey_bl) = colnames(data_FHIT3_black[5:10])
tukey_bl


# copula 2 sample testing 
plot_copula = function(string_type, string_typevstype, data){
  
  df = data.frame( group_id =  rep(string_type, 6),
                   desc  = rep(string_typevstype, 6) ,
                   varname = colnames(data)[c(5:10)])
  
  return(df)
  
}

#black - no need to run anymore!!!!
result_bl_adhd_tsa = coptest.p(as.matrix(data_FHIT3_black_adhd[,c(5:10)]) ,
                               as.matrix(data_FHIT3_black_tsa[,c(5:10)]) ,
                               nperm = 100,approx = FALSE)

result_bl_adhd_tsa$tbl


result_bl_adhd_dys = coptest.p(as.matrix(data_FHIT3_black_adhd[,c(5:10)]) ,
                               as.matrix(data_FHIT3_black_dys[,c(5:10)]) ,
                               nperm = 100,approx = FALSE)

result_bl_adhd_dys$tbl



result_bl_tsa_dys = coptest.p(as.matrix(data_FHIT3_black_dys[,c(5:10)]) ,
                              as.matrix(data_FHIT3_black_tsa[,c(5:10)]) ,
                              nperm = 100,approx = FALSE)

result_bl_tsa_dys$tbl

# saveRDS(result_bl_adhd_tsa,file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_tsa.rds" )
# saveRDS(result_bl_adhd_dys, file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_dys.rds"  ) 
# saveRDS(result_bl_tsa_dys, file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_tsa_dys.rds" )

result_bl_adhd_tsa<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_tsa.rds")
result_bl_adhd_dys<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_dys.rds")
result_bl_tsa_dys<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_tsa_dys.rds")



#  COPULA PLOTS

toplot7 = plot_copula("ADHD", "TSA vs ADHD", data_FHIT3_black_adhd) #the data to use correspond to the first string
netvis(obj = result_bl_adhd_tsa,ref = toplot7)

toplot8 = plot_copula("TSA", "TSA vs ADHD", data_FHIT3_black_tsa) #the data to use correspond to the first string
netvis(obj = result_bl_adhd_tsa,ref = toplot8)



toplot9 = plot_copula("ADHD", "ADHD vs DYS", data_FHIT3_black_adhd) #the data to use correspond to the first string
netvis(obj = result_bl_adhd_dys,ref = toplot9)

toplot10 = plot_copula("DYS", "ADHD vs DYS", data_FHIT3_black_dys) #the data to use correspond to the first string
netvis(obj = result_bl_adhd_dys,ref = toplot10)



toplot11 = plot_copula("DYS", "TSA vs DYS", data_FHIT3_black_dys) #the data to use correspond to the first string
netvis(obj = result_bl_tsa_dys,ref = toplot11)

toplot12 = plot_copula("TSA", "TSA vs DYS", data_FHIT3_black_tsa) #the data to use correspond to the first string
netvis(obj = result_bl_tsa_dys,ref = toplot12)




exprs = rbind(as.matrix(data_FHIT3_black_adhd[,c(5:10)]) ,
              as.matrix(data_FHIT3_black_tsa[,c(5:10)]))
grp = c(rep(1,nrow(data_FHIT3_black_adhd[,c(5:10)])),
        rep(2,nrow( as.matrix(data_FHIT3_black_tsa[,c(5:10)]))))
coexvis(obj = result_bl_adhd_tsa,
        exprs = exprs,grp = grp)


exprs = rbind(as.matrix(data_FHIT3_black_adhd[,c(5:10)]) ,
              as.matrix(data_FHIT3_black_dys[,c(5:10)]))
grp = c(rep(1,nrow(data_FHIT3_black_adhd[,c(5:10)])),
        rep(2,nrow( as.matrix(data_FHIT3_black_dys[,c(5:10)]))))
coexvis(obj = result_bl_adhd_dys,
        exprs = exprs,grp = grp)


exprs = rbind(as.matrix(data_FHIT3_black_dys[,c(5:10)]) ,
              as.matrix(data_FHIT3_black_tsa[,c(5:10)]))
grp = c(rep(1,nrow(data_FHIT3_black_dys[,c(5:10)])),
        rep(2,nrow( as.matrix(data_FHIT3_black_tsa[,c(5:10)]))))
coexvis(obj = result_bl_tsa_dys,
        exprs = exprs,grp = grp)

###############################
# FEATURE SELECTION & RANKING #
###############################



#mean test
id_m_bl = which(test_mean_diff_bl[c(2,3,6),] < 0.1, arr.ind=TRUE)

stat_dec_mean_bl = cbind(do.call(rbind,
                                 lapply(1:dim(id_m_bl)[1], 
                                        function(i) 
                                          test_mean_diff_bl[c(2,3,6),][ id_m_bl[i,1],c(1:2) ])),
                         do.call(rbind,
                                 lapply(1:dim(id_m_bl)[1], 
                                        function(i) 
                                          colnames(test_mean_diff_bl[c(2,3,6),])[id_m_bl[i,2]] )))


colnames(stat_dec_mean_bl) = c("Type1", "Type2", "Discr_Var")
stat_dec_mean_bl$Test = rep('mean', dim(stat_dec_mean_bl)[1])
stat_dec_mean_bl$Significance =   rep(0.1, dim(stat_dec_mean_bl)[1])
stat_dec_mean_bl


#Varince test
id_v_bl = which(test_var_ratio_bl[c(2,3,6),] < 0.15, arr.ind=TRUE)

stat_dec_var_bl = cbind(do.call(rbind,
                                lapply(1:dim(id_v_bl)[1], 
                                       function(i) 
                                         test_var_ratio_bl[c(2,3,6),][ id_v_bl[i,1],c(1:2) ])),
                        do.call(rbind,
                                lapply(1:dim(id_v_bl)[1], 
                                       function(i) 
                                         colnames(test_var_ratio_bl[c(2,3,6),])[id_v_bl[i,2]] )))


colnames(stat_dec_var_bl) = c("Type1", "Type2", "Discr_Var")
stat_dec_var_bl$Test = rep('variance', dim(stat_dec_var_bl)[1])
stat_dec_var_bl$Significance =   rep(0.15, dim(stat_dec_var_bl)[1])
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
stat_dec_dist_bl$Test = rep('Distribution', dim(stat_dec_dist_bl)[1])
stat_dec_dist_bl$Significance =   rep(0.1, dim(stat_dec_dist_bl)[1])
stat_dec_dist_bl


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
                     stat_dec_var_bl,
                     stat_dec_dist_bl,
                     stat_dec_tukey_bl,
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
final_dec_bl2[10,1] = "L. Post. - R. Ant."
final_dec_bl2[11,1] = "R. Post. - R. Ant."
final_dec_bl2[12,1] = "L. Lat. - L. Post."
final_dec_bl2[13,1] = "L. Ant. - R. Ant."
final_dec_bl2[14,1] = "L. Lat. - R. Lat."
#final_dec_bl2[15,1] = "Left_A_bl-Right_A_bl"
#final_dec_bl2[16,1] = "Left_L_bl-Right_L_bl"

final_dec_bl2



#PLOTS FINAL DECISION 

prova2 = final_dec_bl2 %>% complete(Test ,Combination)

pdf( paste(figs_dir, "heat_black.pdf", sep = ""), width = 8, height = 5 )  
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
        legend.text=element_text(size=15)) + xlab("Type of Disorder")  + ylab("Tests")  
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




hist_vars = prova2  %>% group_by( Discr_Var )   %>% 
                count(Combination) %>% 
                    mutate(percent = n/sum(n),
                            n = n)
hist_vars =  hist_vars[-c(11,12),]

pdf( paste(figs_dir, "hist_vars.pdf", sep = ""))  
ggplot(hist_vars,
       aes(x = Discr_Var, y = n, fill = Combination)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = TRUE),
             vjust = 1,show.legend = FALSE, size = 7)  +
  #scale_fill_grey() +
  xlab("Discriminant Variable") + ylab("Number of Significant Statistical Tests")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        axis.text=element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15)) +
  scale_fill_manual(values=c("lightblue", "cyan", "blue")) 
dev.off()


hist_tests =prova2  %>% group_by( Test )   %>% 
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
  xlab("Tests") + ylab("Number of Significant Attributes")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom',
        axis.text=element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15)) +
  scale_fill_manual(values=c("lightblue", "cyan", "blue")) 
dev.off()


#######################
# FEATURE ENGINEERING #
#######################


final_dec_bl2

meanfun <- function(x, d) {
  return(mean(x[d], na.rm=TRUE))
}

varfun <- function(x, d) {
  return(var(x[d], na.rm=TRUE))
}


mean_boot_adhd_bl_r_l = boot(data_FHIT3_black_adhd$`R. Lat.`,
                             statistic = meanfun,
                             R=25)
mean_boot_dys_bl_r_l = boot(data_FHIT3_black_dys$`R. Lat.`,
                            statistic = meanfun,
                            R=25)
mean_boot_tsa_bl_r_l = boot(data_FHIT3_black_tsa$`R. Lat.`,
                            statistic = meanfun,
                            R=25)
mean_boot_ctr_bl_r_l = boot(data_FHIT3_black_ctr$`R. Lat.`,
                            statistic = meanfun,
                            R=25)


mean_boot_adhd_bl_l_a = boot(data_FHIT3_black_adhd$`L. Ant.`,
                             statistic = meanfun,
                             R=25)
mean_boot_dys_bl_l_a = boot(data_FHIT3_black_dys$`L. Ant.`,
                            statistic = meanfun,
                            R=25)
mean_boot_tsa_bl_l_a = boot(data_FHIT3_black_tsa$`L. Ant.`,
                            statistic = meanfun,
                            R=25)
mean_boot_ctr_bl_l_a = boot(data_FHIT3_black_ctr$`L. Ant.`,
                            statistic = meanfun,
                            R=25)




var_boot_adhd_bl_l_l = boot(data_FHIT3_black_adhd$`L. Lat.`,
                            statistic = varfun,
                            R=25)
var_boot_dys_bl_l_l = boot(data_FHIT3_black_dys$`L. Lat.`,
                           statistic = varfun,
                           R=25)
var_boot_tsa_bl_l_l = boot(data_FHIT3_black_tsa$`L. Lat.`,
                           statistic = varfun,
                           R=25)
var_boot_ctr_bl_l_l = boot(data_FHIT3_black_ctr$`L. Lat.`,
                           statistic = varfun,
                           R=25)



var_boot_adhd_bl_r_l = boot(data_FHIT3_black_adhd$`R. Lat.`,
                            statistic = varfun,
                            R=25)
var_boot_dys_bl_r_l = boot(data_FHIT3_black_dys$`R. Lat.`,
                           statistic = varfun,
                           R=25)
var_boot_tsa_bl_r_l = boot(data_FHIT3_black_tsa$`R. Lat.`,
                           statistic = varfun,
                           R=25)
var_boot_ctr_bl_r_l = boot(data_FHIT3_black_ctr$`R. Lat.`,
                           statistic = varfun,
                           R=25)


new_data_bl = cbind(rbind(mean_boot_adhd_bl_r_l$t, mean_boot_ctr_bl_r_l$t, mean_boot_dys_bl_r_l$t, mean_boot_tsa_bl_r_l$t),
                    rbind(mean_boot_adhd_bl_l_a$t, mean_boot_ctr_bl_l_a$t, mean_boot_dys_bl_l_a$t, mean_boot_tsa_bl_l_a$t),
                    rbind(var_boot_adhd_bl_l_l$t, var_boot_ctr_bl_l_l$t, var_boot_dys_bl_l_l$t, var_boot_tsa_bl_l_l$t),
                    rbind(var_boot_adhd_bl_r_l$t, var_boot_ctr_bl_r_l$t, var_boot_dys_bl_r_l$t, var_boot_ctr_bl_r_l$t))


colnames(new_data_bl) = c("Mean_boot_R_lat","Mean_boot_L_ant","Var_boot_L_lat","Var_boot_R_lat")

new_data_bl<- as.data.frame(new_data_bl)
new_data_bl$Type = c(rep("ADHD", 25),rep("CTR", 25),rep("DYS", 25),rep("TSA", 25))


new_data_bl = new_data_bl %>% 
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


head(new_data_bl)

#########
# RTSNE #
#########

set.seed(42)

#by experiments
new_data_bl_unique<- new_data_bl
new_data_bl_unique <- unique(new_data_bl_unique)

tsne_bl <- Rtsne(new_data_bl_unique[,c(1:4)], 
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
tsne3d <- tsne(new_data_bl_unique[,c(1:4)], 
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




################
################
## CLUSTERING ##
################
################

###################
## DISSIMILARITY ##
###################

head(new_data_bl)


new_data_bl = new_data_bl %>% 
  mutate(
    # Create categories
    Type2 = dplyr::case_when(
      Type == "TSA"  ~ "ASD",
      Type == "DYS"  ~ "DYS",
      Type == "ADHD"  ~ "ADHD",
      Type == "CTR"  ~ "Control"
    ),
    # Convert to factor
    Type2 = factor(
      Type2,
      level = c("ASD","DYS", "ADHD", "Control")
    )
  )

head(new_data_bl)


data_bl_scaled <- new_data_bl %>% mutate_at(c(1:4), funs(c(scale(.))))


res.dist.bl <- get_dist(data_bl_scaled[,c(1:4)], stand = TRUE, method = "pearson")

fviz_dist(res.dist.bl, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))




#############
## K MEANS ##
#############

fviz_nbclust(data_bl_scaled[,c(1:2)], kmeans, method = "gap_stat")


set.seed(123)


data_bl_kmeans = data_bl_scaled[,c(1:4)]
data_bl_kmeans = as.data.frame(data_bl_kmeans)
rownames(data_bl_kmeans) <- sapply(1:nrow(data_bl_scaled), 
                                   function(i) paste(data_bl_scaled$Type2[i], i, sep = "_"))


km.res.black <- kmeans(data_bl_kmeans, 4, nstart = 25)


fviz_cluster(km.res.black,
             data = data_bl_kmeans,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_bw(), 
             labelsize = 8)

custom_theme <- theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        axis.text=element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15))

fviz_cluster(km.res.black, 
             data = data_bl_kmeans, 
             ellipse.type = "convex",
             labelsize = 0) + 
             scale_color_manual(values = c("pink","orange","green","yellow" )) +
  custom_theme +  
  labs(title = "") +
  scale_shape_manual(values = c(3,17,19,22) ,labels = c("ASD", "Control", "ADHD", "DYS"))



table(km.res.black$cluster,data_bl_scaled$Type2)
table(data_bl_scaled$Type2, km.res.black$cluster)

cluster_labels <- c(  "ADHD", "DYS", "Control","ASD")
cluster_labels <- c("ASD","DYS","ADHD", "Control" )
results_kmeans = as.factor(cluster_labels[km.res.black$cluster])
results_kmeans <- factor(results_kmeans, levels = levels(data_bl_scaled$Type2))

confusionMatrix(data = results_kmeans, reference = data_bl_scaled$Type2)



# True class labels
true_labels <- data_bl_scaled$Type2
# Cluster assignments
cluster_labels <- km.res.black$cluster
# Create a mapping between cluster numbers and class labels
# Replace the numbers with the corresponding class labels
cluster_to_class_mapping <- c("ADHD", "Control", "DYS", "ASD")
cluster_labels_mapped <- factor(cluster_to_class_mapping[cluster_labels],levels = levels(data_bl_scaled$Type2))
# Create the confusion matrix
confusion_matrix <- table(true_labels, cluster_labels_mapped)

# Print the confusion matrix
print(confusion_matrix)
confusionMatrix(data = cluster_labels_mapped, reference = true_labels)


################################
##  HIERARCHICHAL clustering  ##
################################

# Compute hierarchical clustering

data_bl_hier <- as.data.frame(new_data_bl[,c(1:4)])
rownames(data_bl_hier) <- sapply(1:nrow(new_data_bl), 
                                 function(i) paste(new_data_bl$Type2[i], i, sep = "_"))



res.hc.black <- data_bl_hier  %>%  
  scale %>%
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")     



fviz_dend(res.hc.black,
          k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("orange", "green", "pink", "gold"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE ) +custom_theme +  
  labs(title = "")


clusters =cutree(res.hc.black, k=4)
table(data_bl_scaled$Type2,clusters)


# True class labels
true_labels <- data_bl_scaled$Type2
# Cluster assignments
cluster_labels <- clusters
# Create a mapping between cluster numbers and class labels
# Replace the numbers with the corresponding class labels
cluster_to_class_mapping <- c("ADHD", "Control", "DYS", "ASD")
cluster_labels_mapped <- factor(cluster_to_class_mapping[cluster_labels],levels = levels(data_bl_scaled$Type2))
# Create the confusion matrix
confusion_matrix <- table(true_labels, cluster_labels_mapped)

# Print the confusion matrix
print(confusion_matrix)
confusionMatrix(data = cluster_labels_mapped, reference = true_labels)




## Determining the optimal number of clusters ##

res.nbclust.black <- new_data_bl[,c(1:4)] %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 10, 
          method = "complete", index ="all") 



#########
#  PCA  #
#########

corMat_FHIT3_black <- cor(new_data_bl[,c(1:4)])
corrplot(corMat_FHIT3_black, order = "hclust")


res.pca.FHIT3.black <- PCA(data_bl_hier, scale.unit=TRUE, graph = T)


#This line of code will sort the variables the most linked to each PC.
dimdesc(res.pca.FHIT3.black)

summary(res.pca.FHIT3.black)


######################
# Graph of variables #
######################

#Eigenvalues / Variances
fviz_eig(res.pca.FHIT3.black, addlabels = TRUE)

#Quality of representation
var_posture_black <- get_pca_var(res.pca.FHIT3.black)

corrplot(var_posture_black$cos2, is.corr=FALSE)

# The cos2 values are used to estimate the quality of the representation
# The closer a variable is to the circle of correlations, the better its 
# representation on the factor map (and the more important it is to interpret these components)
# Variables that are closed to the center of the plot are less important for the first components


fviz_pca_var(res.pca.FHIT3.black, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


#Contributions of variables to PCs
fviz_contrib(res.pca.FHIT3.black, choice = "var", axes = 1, top = 10)


#######################
# Graph of individuals
#######################

fviz_pca_ind(res.pca.FHIT3.black)



fviz_pca_biplot(res.pca.FHIT3.black, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = new_data_bl$Type,
                col.ind = "black",
                # Color variable by groups
                # col.var = factor(c("sepal", "sepal", "petal", "petal")), #THINK ABOUT THIS
               
                legend.title = list(fill = "Type", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors


custom_theme2 <- theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        axis.text=element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15))



new_data_bl

fviz_pca_biplot(res.pca.FHIT3.black, 
                # Individuals
                geom.ind = "point",
                fill.ind = new_data_bl$Type2, 
                #col.ind = factor(new_data_bl$Type2),
                pointshape = 21, 
                pointsize = 2, 
                palette = c("pink","gold", "green", "orange"),  
                #palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                legend.title = list(fill = "Children", 
                                    color = "Contrib.",
                                    alpha = "Contrib.")
) + custom_theme2 +  
  labs(title = "") 







