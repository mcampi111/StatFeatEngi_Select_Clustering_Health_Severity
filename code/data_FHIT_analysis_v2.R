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



colnames(data_FHIT3)[5:16] = c("Left_L_rf",  "Right_L_rf", "Left_P_rf",  "Right_P_rf", 
                               "Left_A_rf",  "Right_A_rf",  "Left_L_bl", "Right_L_bl",
                               "Left_P_bl",  "Right_P_bl", "Left_A_bl",  "Right_A_bl")




#################
# INITIAL PLOTS #  
#################

#counf ot PEOPLE
data_FHIT3_hist0<- data_FHIT3 %>% 
  count(Type) %>% 
  mutate(percent = n/sum(n), n = n)

#counf ot PEOPLE by age
data_FHIT3_hist1<- data_FHIT3 %>% 
  count(Type,age_group) %>% 
  mutate(percent = n/sum(n), n = n)


#histrograms 
#1) by condition
pdf( paste(figs_dir, "hist_children.pdf", sep = ""))  
ggplot(data_FHIT3_hist0,
       aes(x = Type, y = n, fill = Type)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = TRUE),
             vjust = 1,show.legend = FALSE)  +
  #scale_fill_grey() +
  xlab("Type") + ylab("Number of children")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')

dev.off()



data_FHIT3_hist1$color = c(rep("pink", 4), rep("green", 3),rep("yellow", 4), rep("orange", 4))

#1) by condition and age
ggplot(data_FHIT3_hist1, aes(x = age_group, y = n, fill = Type)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , 
             position = position_stack(reverse = TRUE),
             color = data_FHIT3_hist1$color,
             vjust = 1,
             show.legend = FALSE)  +
  #  scale_fill_grey() +
  xlab("Age groups") + ylab("Number of children") +
  theme(axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')


# BOXPLOTS 

#1) by condition
bxpl_data_FHIT3 = melt(data_FHIT3[,-c(1,2,4,17,18)], id.vars = "Type")

ggplot(bxpl_data_FHIT3, 
       aes(x = variable, y = value, color = variable )) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~Type, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        # axis.title.x=element_blank(),
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank(),
        strip.text.x = element_text(size = 14)) + ylab("")  + xlab("") 

#2) by condition and experiments

bxpl_data_FHIT3_rot_frame = melt(data_FHIT3[,-c(1,2,4,11,12,13,14,15,16,17,18)], id.vars = "Type")


pdf( paste(figs_dir, "box_rot_frame.pdf", sep = ""))  
ggplot(bxpl_data_FHIT3_rot_frame, 
       aes(x = variable, y = value, color = variable )) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~Type, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        # axis.title.x=element_blank(),
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank(),
        strip.text.x = element_text(size = 14)) + ylab("")  + xlab("") 
dev.off()


bxpl_data_FHIT3_black = melt(data_FHIT3[,-c(1,2,4,5,6,7,8,9,10,17,18)], id.vars = "Type")

pdf( paste(figs_dir, "box_black.pdf", sep = ""))  
ggplot(bxpl_data_FHIT3_black, 
       aes(x = variable, y = value, color = variable )) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~Type, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=0.5),
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
data_FHIT3_rotfram <- data_FHIT3[,-c(11:16)]
data_FHIT3_black <- data_FHIT3[,-c(5:10)]


set.seed(42)

#by experiments

data_FHIT3_rotfram_unique<- data_FHIT3_rotfram
data_FHIT3_rotfram_unique <- unique(data_FHIT3_rotfram_unique[,c(3,5:10,12)])

tsne_out_data_FHIT3_rotfram <- Rtsne(data_FHIT3_rotfram_unique[,c(2:7)], 
                                     dims = 2,
                                     pca = TRUE,
                                     perplexity = 10)

toplot_FHIT3_rotfram = as.data.frame(tsne_out_data_FHIT3_rotfram$Y)
toplot_FHIT3_rotfram$color = data_FHIT3_rotfram_unique$color
toplot_FHIT3_rotfram$Type = data_FHIT3_rotfram_unique$Type
colnames(toplot_FHIT3_rotfram) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_FHIT3_rotfram, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))


tsne3d <- tsne(data_FHIT3_rotfram_unique[,c(2:7)], 
               initial_config = NULL, 
               k = 3, 
               initial_dims = 30, 
               perplexity = 35,
               max_iter = 1000,
               min_cost = 0, 
               epoch_callback = NULL, 
               whiten = TRUE,
               epoch=300)
tsne3d <- cbind(tsne3d,data_FHIT3_rotfram_unique[,1])


par3d(windowRect = c(100, 100, 612, 612))
plot3d(x = tsne3d[,1], 
       y= tsne3d[,2], 
       z= tsne3d[,3], 
       type = "s", 
       radius = 0.5,
       xlab="Dim.1", ylab="Dim.2", zlab="Dim.3", col = data_FHIT3_rotfram_unique$color )
#spheres3d(x = m[,1], y= m[,2], z= m[,3], r = 0.2, color = "red", radius = 2)
legend3d("topright", legend = c("ASD","DYS","ADHD","CTR"),
         pch = 16, col = c("pink", "yellow", "green", "orange"), cex=0.8)
rgl.snapshot(paste(figs_dir, "rot_frame_tsne.png", sep = ""), fmt = 'png')







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


rm(tsne3d)

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
       radius = 10.5,
       xlab="Dim.1", ylab="Dim.2", zlab="Dim.3", col = data_FHIT3_black_unique$color )
legend3d("topright", legend = c("ASD","DYS","ADHD","CTR"),
         pch = 16, col = c("pink", "yellow", "green", "orange"), cex=0.8)
rgl.snapshot(paste(figs_dir, "black_tsne.png", sep = ""), fmt = 'png')



#############################
# INITIAL STATISTICAL TESTs #
#############################


data_FHIT3_rotfram_ctr <- data_FHIT3_rotfram %>%
  filter(Type == "CTR") 

data_FHIT3_rotfram_tsa <- data_FHIT3_rotfram %>%
  filter(Type == "TSA") 

data_FHIT3_rotfram_dys <- data_FHIT3_rotfram %>%
  filter(Type == "DYS") 

data_FHIT3_rotfram_adhd <- data_FHIT3_rotfram %>%
  filter(Type == "ADHD") 


data_FHIT3_black_ctr <- data_FHIT3_black %>%
  filter(Type == "CTR") 

data_FHIT3_black_tsa <- data_FHIT3_black %>%
  filter(Type == "TSA") 

data_FHIT3_black_dys <- data_FHIT3_black %>%
  filter(Type == "DYS") 

data_FHIT3_black_adhd <- data_FHIT3_black %>%
  filter(Type == "ADHD") 


category = unique(data_FHIT3$Type)
combo_category = comboGrid(category, category, repetition = F)


list_rot_frame = list(data_FHIT3_rotfram_tsa,data_FHIT3_rotfram_dys,
                      data_FHIT3_rotfram_adhd,data_FHIT3_rotfram_ctr )
names(list_rot_frame) = category


list_black = list(data_FHIT3_black_tsa,data_FHIT3_black_dys,
                  data_FHIT3_black_adhd,data_FHIT3_black_ctr )
names(list_black) = category



#  2 sample test on the mean 
test_mean_diff_rf= as.data.frame(cbind(combo_category,do.call(rbind,lapply(1: dim(combo_category)[1],
                                                                           function(j) do.call(cbind, lapply(1:6, function(i) 
                                                                             round(t.test(list_rot_frame[[combo_category[j,1]]][,4+i],
                                                                                          list_rot_frame[[combo_category[j,2]]][,4+i],
                                                                                          alternative = "two.sided", 
                                                                                          var.equal = FALSE)$p.value,3) ))))))


test_mean_diff_rf = test_mean_diff_rf %>% mutate_at(c('V3', 'V4', 'V5', 'V6', 'V7','V8'), as.numeric)
colnames(test_mean_diff_rf) <- c("Type1", "Type2",colnames(data_FHIT3_rotfram_ctr[5:10]))
test_mean_diff_rf


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
test_var_ratio_rf = as.data.frame(
  cbind(combo_category,
        do.call(rbind,lapply(1: dim(combo_category)[1],
                             function(j) do.call(cbind, lapply(1:6, function(i) 
                               round(var.test(as.numeric(unlist(list_rot_frame[[combo_category[j,1]]][,4+i])),
                                              as.numeric(unlist(list_rot_frame[[combo_category[j,2]]][,4+i])),
                                              alternative = "two.sided")$p.value,3) ))))))

test_var_ratio_rf = test_var_ratio_rf %>% mutate_at(c('V3', 'V4', 'V5', 'V6', 'V7','V8'), as.numeric)
colnames(test_var_ratio_rf) <- c("Type1", "Type2",colnames(data_FHIT3_rotfram_ctr[5:10]))
test_var_ratio_rf

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
test_kolmo_rf = as.data.frame(
  cbind(combo_category,
        do.call(rbind,lapply(1: dim(combo_category)[1],
                             function(j) do.call(cbind, lapply(1:6, function(i) 
                               round(ks.test(as.numeric(unlist(list_rot_frame[[combo_category[j,1]]][,4+i])),
                                             as.numeric(unlist(list_rot_frame[[combo_category[j,2]]][,4+i])),
                                             alternative = "two.sided")$p.value,3) ))))))

test_kolmo_rf = test_kolmo_rf %>% mutate_at(c('V3', 'V4', 'V5', 'V6', 'V7','V8'), as.numeric)
colnames(test_kolmo_rf) <- c("Type1", "Type2",colnames(data_FHIT3_rotfram_ctr[5:10]))
test_kolmo_rf


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
bart_test_rf =  as.data.frame(
  cbind(colnames(data_FHIT3_rotfram[5:10]),
        do.call(rbind,lapply(1:6, function(i)
          (round(data_FHIT3_rotfram %$%  bartlett.test(get(
            colnames(data_FHIT3_rotfram[,4+i])) ~ as.factor(Type))$p.value,
            3)))) ) )
bart_test_rf = bart_test_rf  %>% mutate_at(c('V2'), as.numeric)
colnames(bart_test_rf) = c("variable", "p.value")
bart_test_rf

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
anova_rotfr = as.data.frame(
  cbind(colnames(data_FHIT3_rotfram[5:10]),
        do.call(rbind,lapply(1:6, function(i)
          (round(summary(data_FHIT3_rotfram %$% aov( get(
            colnames(data_FHIT3_rotfram[,4+i]))~ Type))[[1]][5]$`Pr(>F)`[1],7)))) ) )

anova_black = as.data.frame(
  cbind(colnames(data_FHIT3_black[5:10]),
        do.call(rbind,lapply(1:6, function(i)
          (round(summary(data_FHIT3_black %$% aov( get(
            colnames(data_FHIT3_black[,4+i]))~ Type))[[1]][5]$`Pr(>F)`[1],7)))) ) )

#2 - TUKEY
tukey_rf = do.call(cbind,lapply(1:6, function(i)
  round(TukeyHSD(data_FHIT3_rotfram %$% aov( get(
    colnames(data_FHIT3_rotfram[,4+i]))~ Type))$Type[,4],3)  ))

colnames(tukey_rf) = colnames(data_FHIT3_rotfram[5:10])
tukey_rf

tukey_bl = do.call(cbind,lapply(1:6, function(i)
  round(TukeyHSD(data_FHIT3_black %$% aov( get(
    colnames(data_FHIT3_black[,4+i]))~ Type))$Type[,4],3)  ))

colnames(tukey_bl) = colnames(data_FHIT3_black[5:10])
tukey_bl



# copula 2 sample testing 

#rotating frame

#################################################### 1example with ctr for the plots
result_rf_tsa_ctr = coptest.p(as.matrix(data_FHIT3_rotfram_tsa[,c(5:10)]) ,
                              as.matrix(data_FHIT3_rotfram_ctr[,c(5:10)]) ,
                              nperm = 100,approx = FALSE)

result_rf_tsa_ctr$tbl


####################################################
#NO NEED TO RUN ANYMORE!!!
# result_rf_adhd_tsa = coptest.p(as.matrix(data_FHIT3_rotfram_adhd[,c(5:10)]) ,
#                                as.matrix(data_FHIT3_rotfram_tsa[,c(5:10)]) ,
#                                nperm = 100,approx = FALSE)
# 
# result_rf_adhd_tsa$tbl
# 
# 
# result_rf_adhd_dys = coptest.p(as.matrix(data_FHIT3_rotfram_adhd[,c(5:10)]) ,
#                                as.matrix(data_FHIT3_rotfram_dys[,c(5:10)]) ,
#                                nperm = 100,approx = FALSE)
# 
# result_rf_adhd_dys$tbl
# 
# 
# 
# result_rf_tsa_dys = coptest.p(as.matrix(data_FHIT3_rotfram_dys[,c(5:10)]) ,
#                               as.matrix(data_FHIT3_rotfram_tsa[,c(5:10)]) ,
#                               nperm = 100,approx = FALSE)
# 
# result_rf_tsa_dys$tbl


# saveRDS(result_rf_adhd_tsa,file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_rf_adhd_tsa.rds" )
# saveRDS(result_rf_adhd_dys, file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_rf_adhd_dys.rds"  )
# saveRDS(result_rf_tsa_dys, file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_rf_tsa_dys.rds" )

result_rf_adhd_tsa<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_rf_adhd_tsa.rds")
result_rf_adhd_dys<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_rf_adhd_dys.rds")
result_rf_tsa_dys<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_rf_tsa_dys.rds")

plot_copula = function(string_type, string_typevstype, data){
  
  df = data.frame( group_id =  rep(string_type, 6),
                   desc  = rep(string_typevstype, 6) ,
                   varname = colnames(data)[c(5:10)])
  
  return(df)
  
}


toplot1 = plot_copula("ADHD", "TSA vs ADHD", data_FHIT3_rotfram_adhd) #the data to use correspond to the first string
netvis(obj = result_rf_adhd_tsa,ref = toplot1)

toplot2 = plot_copula("TSA", "TSA vs ADHD", data_FHIT3_rotfram_tsa) #the data to use correspond to the first string
netvis(obj = result_rf_adhd_tsa,ref = toplot2)



toplot3 = plot_copula("ADHD", "ADHD vs DYS", data_FHIT3_rotfram_adhd) #the data to use correspond to the first string
netvis(obj = result_rf_adhd_dys,ref = toplot3)

toplot4 = plot_copula("DYS", "ADHD vs DYS", data_FHIT3_rotfram_dys) #the data to use correspond to the first string
netvis(obj = result_rf_adhd_dys,ref = toplot4)



toplot5 = plot_copula("DYS", "TSA vs DYS", data_FHIT3_rotfram_dys) #the data to use correspond to the first string
netvis(obj = result_rf_tsa_dys,ref = toplot5)

toplot6 = plot_copula("TSA", "TSA vs DYS", data_FHIT3_rotfram_tsa) #the data to use correspond to the first string
netvis(obj = result_rf_tsa_dys,ref = toplot6)



#################################  1 example wth control for the plots
exprs = rbind(as.matrix(data_FHIT3_rotfram_tsa[,c(5:10)]) ,
              as.matrix(data_FHIT3_rotfram_ctr[,c(5:10)]))
grp = c(rep(1,nrow(data_FHIT3_rotfram_tsa[,c(5:10)])),
        rep(2,nrow( as.matrix(data_FHIT3_rotfram_ctr[,c(5:10)]))))
coexvis(obj = result_rf_tsa_ctr,
        exprs = exprs,grp = grp)
####################################################

exprs = rbind(as.matrix(data_FHIT3_rotfram_adhd[,c(5:10)]) ,
              as.matrix(data_FHIT3_rotfram_tsa[,c(5:10)]))
grp = c(rep(1,nrow(data_FHIT3_rotfram_adhd[,c(5:10)])),
        rep(2,nrow( as.matrix(data_FHIT3_rotfram_tsa[,c(5:10)]))))
coexvis(obj = result_rf_adhd_tsa,
        exprs = exprs,grp = grp)

exprs = rbind(as.matrix(data_FHIT3_rotfram_adhd[,c(5:10)]) ,
              as.matrix(data_FHIT3_rotfram_dys[,c(5:10)]))
grp = c(rep(1,nrow(data_FHIT3_rotfram_adhd[,c(5:10)])),
        rep(2,nrow( as.matrix(data_FHIT3_rotfram_dys[,c(5:10)]))))
coexvis(obj = result_rf_adhd_dys,
        exprs = exprs,grp = grp)


exprs = rbind(as.matrix(data_FHIT3_rotfram_dys[,c(5:10)]) ,
              as.matrix(data_FHIT3_rotfram_tsa[,c(5:10)]))
grp = c(rep(1,nrow(data_FHIT3_rotfram_dys[,c(5:10)])),
        rep(2,nrow( as.matrix(data_FHIT3_rotfram_tsa[,c(5:10)]))))
coexvis(obj = result_rf_tsa_dys,
        exprs = exprs,grp = grp)


#black - no need to run anymore!!!!
# result_bl_adhd_tsa = coptest.p(as.matrix(data_FHIT3_black_adhd[,c(5:10)]) ,
#                                as.matrix(data_FHIT3_black_tsa[,c(5:10)]) ,
#                                nperm = 100,approx = FALSE)
# 
# result_bl_adhd_tsa$tbl
# 
# 
# result_bl_adhd_dys = coptest.p(as.matrix(data_FHIT3_black_adhd[,c(5:10)]) ,
#                                as.matrix(data_FHIT3_black_dys[,c(5:10)]) ,
#                                nperm = 100,approx = FALSE)
# 
# result_bl_adhd_dys$tbl
# 
# 
# 
# result_bl_tsa_dys = coptest.p(as.matrix(data_FHIT3_black_dys[,c(5:10)]) ,
#                               as.matrix(data_FHIT3_black_tsa[,c(5:10)]) ,
#                               nperm = 100,approx = FALSE)
# 
# result_bl_tsa_dys$tbl


# saveRDS(result_bl_adhd_tsa,file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_tsa.rds" )
# saveRDS(result_bl_adhd_dys, file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_dys.rds"  )
# saveRDS(result_bl_tsa_dys, file = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_tsa_dys.rds" )

result_bl_adhd_tsa<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_tsa.rds")
result_bl_adhd_dys<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_adhd_dys.rds")
result_bl_tsa_dys<- readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\result_bl_tsa_dys.rds")




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

##############################################
# DECISION MAKING BASED ON STATISTICAL TESTs #
##############################################

#5 criteria : 1) mean test, variance test, distribution test, tukey test, copula test


#mean test
id_m_rf = which(test_mean_diff_rf[c(2,3,6),] < 0.1, arr.ind=TRUE)

stat_dec_mean_rf = cbind(do.call(rbind,
                                 lapply(1:dim(id_m_rf)[1], 
                                        function(i) 
                                          test_mean_diff_rf[c(2,3,6),][ id_m_rf[i,1],c(1:2) ])),
                         do.call(rbind,
                                 lapply(1:dim(id_m_rf)[1], 
                                        function(i) 
                                          colnames(test_mean_diff_rf[c(2,3,6),])[id_m_rf[i,2]] )))


colnames(stat_dec_mean_rf) = c("Type1", "Type2", "Discr_Var")
stat_dec_mean_rf$Test = rep('mean', dim(stat_dec_mean_rf)[1])
stat_dec_mean_rf$Significance =   rep(0.1, dim(stat_dec_mean_rf)[1])
stat_dec_mean_rf



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
id_v_rf = which(test_var_ratio_rf[c(2,3,6),] < 0.15, arr.ind=TRUE)

stat_dec_var_rf = cbind(do.call(rbind,
                                lapply(1:dim(id_v_rf)[1], 
                                       function(i) 
                                         test_var_ratio_rf[c(2,3,6),][ id_v_rf[i,1],c(1:2) ])),
                        do.call(rbind,
                                lapply(1:dim(id_v_rf)[1], 
                                       function(i) 
                                         colnames(test_var_ratio_rf[c(2,3,6),])[id_v_rf[i,2]] )))


colnames(stat_dec_var_rf) = c("Type1", "Type2", "Discr_Var")
stat_dec_var_rf$Test = rep('variance', dim(stat_dec_var_rf)[1])
stat_dec_var_rf$Significance =   rep(0.15, dim(stat_dec_var_rf)[1])
stat_dec_var_rf



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
id_d_rf = which(test_kolmo_rf[c(2,3,6),] < 0.1, arr.ind=TRUE)

stat_dec_dist_rf = cbind(do.call(rbind,
                                 lapply(1:dim(id_d_rf)[1], 
                                        function(i) 
                                          test_kolmo_rf[c(2,3,6),][ id_d_rf[i,1],c(1:2) ])),
                         do.call(rbind,
                                 lapply(1:dim(id_d_rf)[1], 
                                        function(i) 
                                          colnames(test_kolmo_rf[c(2,3,6),])[id_d_rf[i,2]] )))


colnames(stat_dec_dist_rf) = c("Type1", "Type2", "Discr_Var")
stat_dec_dist_rf$Test = rep('Distribution', dim(stat_dec_dist_rf)[1])
stat_dec_dist_rf$Significance =   rep(0.1, dim(stat_dec_dist_rf)[1])
stat_dec_dist_rf



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
id_t_rf = which(tukey_rf[c(2,3,6),] < 0.25, arr.ind=TRUE)

stat_dec_tukey_rf = as.data.frame(cbind(do.call(rbind,
                                                lapply(1:dim(id_t_rf)[1], 
                                                       function(i) 
                                                         rownames(tukey_rf[c(2,3,6),])[ id_t_rf[i,1]])),
                                        do.call(rbind,
                                                lapply(1:dim(id_t_rf)[1], 
                                                       function(i) 
                                                         colnames(tukey_rf[c(2,3,6),])[id_t_rf[i,2]] ))) )

colnames(stat_dec_tukey_rf) = c("Combination", "Discr_Var")
stat_dec_tukey_rf$Test = rep('Tukey', dim(stat_dec_tukey_rf)[1])
stat_dec_tukey_rf$Significance =   rep(0.25, dim(stat_dec_tukey_rf)[1])
stat_dec_tukey_rf

new_types_rf = do.call(rbind,lapply(1:dim(stat_dec_tukey_rf)[1], function(i) 
  str_split_fixed(stat_dec_tukey_rf$Combination[i], '-', 2)))
colnames(new_types_rf) = c("Type1","Type2")
stat_dec_tukey_rf = cbind(new_types_rf,stat_dec_tukey_rf[,-1])
stat_dec_tukey_rf

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
stat_dec_copula_rf = rbind(do.call(rbind,
                                   lapply(1:length(which(result_rf_adhd_tsa$tbl[,4] < 0.01, arr.ind=TRUE)),function(i)
                                     cbind(cbind("ADHD", "TSA"),
                                           result_rf_adhd_tsa$tbl[which(result_rf_adhd_tsa$tbl[,4] < 0.01, 
                                                                        arr.ind=TRUE),][,1][i]))),
                           
                           do.call(rbind,
                                   lapply(1:length(which(result_rf_tsa_dys$tbl[,4] < 0.01, arr.ind=TRUE)),function(i)
                                     cbind(cbind("DYS", "TSA"),
                                           result_rf_tsa_dys$tbl[which(result_rf_tsa_dys$tbl[,4] < 0.01, 
                                                                       arr.ind=TRUE),][,1][i]))),
                           do.call(rbind,
                                   lapply(1:length(which(result_rf_adhd_dys$tbl[,4] < 0.01, arr.ind=TRUE)),function(i)
                                     cbind(cbind("ADHD","DYS"),
                                           result_rf_adhd_dys$tbl[which(result_rf_adhd_dys$tbl[,4] < 0.01, 
                                                                        arr.ind=TRUE),][,1][i]))) )

colnames(stat_dec_copula_rf)<- c("Type1","Type2", "Discr_Var")
stat_dec_copula_rf = as.data.frame(stat_dec_copula_rf)
stat_dec_copula_rf$Test = rep('Copula', dim(stat_dec_copula_rf)[1])
stat_dec_copula_rf$Significance = rep(0.01, dim(stat_dec_copula_rf)[1])
stat_dec_copula_rf


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




####################################
# FINAL STATISTICAL TESTS DECISION #
####################################



final_dec_rf = rbind(stat_dec_mean_rf,
                     stat_dec_var_rf,
                     stat_dec_dist_rf,
                     stat_dec_tukey_rf,
                     stat_dec_copula_rf)

final_dec_rf = final_dec_rf %>%
  mutate(Type1_new = if_else(Type1 == 'DYS' & Type2 == 'ADHD', Type2, Type1),
         Type2_new = if_else(Type1 == 'DYS' & Type2 == 'ADHD', Type1, Type2)) %>% 
  mutate(Type1 = Type1_new,Type2 = Type2_new)
final_dec_rf = final_dec_rf[,-c(6,7)]

final_dec_rf$Combination = paste(final_dec_rf$Type1,'-',final_dec_rf$Type2)

final_dec_rf2 = final_dec_rf[,-c(1,2)]
final_dec_rf2
########################
#fix this code tomorrow
final_dec_rf2[6,1] = "Right_P_rf-Right_A_rf"
final_dec_rf2[7,1] = "Left_L_rf-Left_P_rf"
final_dec_rf2[8,1] = "Right_P_rf-Right_A_rf"
final_dec_rf2[9,1] = "Left_L_rf-Left_P_rf"
final_dec_rf2[10,1] = "Right_L_rf-Left_P_rf"
final_dec_rf2[11,1] = "Right_P_rf-Right_A_rf"
final_dec_rf2



#solution 1
ggplot(data = final_dec_rf2, aes(x = factor(Combination),y = Test, fill = Significance)) +
  geom_tile(aes(fill = Significance))+
  geom_text(aes(label = round(Significance,2)), size = 5) +
  theme_bw()+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="none") +
  scale_fill_continuous(low = "cyan", high = "blue") + xlab("Types")

#solution 2
# prova = final_dec_rf2 %>% complete(Test ,Combination)
# 
# ggplot(data = prova, aes(x = factor(Combination),y = Test, fill = Significance)) +
#   geom_tile(position = position_dodge(0), 
#             aes(fill = Significance,  width=4.9, height=1.2),
#             colour = 'black') +
#   scale_fill_gradient2("Significance", limits = c(0.01, 3), 
#                        low = "#1B7837", mid = "white", high = "#762A83")
#   ggrepel::geom_text_repel(aes(label = Discr_Var),
#                            size = 3.5, position = position_dodge(0),
#                            direction = "y",  box.padding = unit(0.04, "lines"))+
#   theme_bw()+  
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=0.5),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         legend.position="bottom") + xlab("Types")  

prova = final_dec_rf2 %>% complete(Test ,Combination)

pdf( paste(figs_dir, "heat_rotframe.pdf", sep = ""), width = 8, height = 5)  
ggplot(data = prova, aes(x = factor(Combination),
                         y = Test, 
                         fill = Significance)) +
  geom_tile(aes(fill = Significance),
            colour = 'black') +
  scale_fill_gradientn("Significance",
                       colours = c("antiquewhite1", "bisque2", "burlywood3", "darkorange"),
                       breaks=c(0.01,0.1,0.15,0.25),
                       guide = "legend",
                       na.value = "azure") + 
  ggrepel::geom_text_repel(data = prova,
                           aes(label = Discr_Var),
                           size = 4,
                           direction = "y",  
                           box.padding = unit(0.09, "lines"),
                           min.segment.length = 1.2)+
  theme_bw()+  
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="bottom") + xlab("Types")  + xlab("Types")  
dev.off()

#the same if we consider only one variable for DYS-TSA
ggplot(data = prova, aes(x = factor(Combination),
                         y = Test, 
                         fill = Significance)) +
  geom_tile(aes(fill = Significance),
            colour = 'black') +
  scale_fill_gradient2("Significance",
                       low = "cyan", mid = "white", high = "blue",
                       breaks=c(0.01,0.1,0.15,0.25),
                       guide = "legend") + 
  geom_text(data = prova, check_overlap = T,
            aes(label = Discr_Var),
            size = 3.5)+
  theme_bw()+  
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="right") + xlab("Types")  + xlab("Types")  



#solution 3
# ggplot(data = final_dec_rf2, aes(x = factor(Discr_Var),y = Test, fill = Significance)) +
#   geom_tile(aes(fill = Significance))+
#   geom_text(aes(label = round(Significance,2)), size = 4, color = 'black') + 
#   facet_wrap(~Combination)+
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.7, hjust=0.5),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         legend.position="none") +
#   scale_fill_continuous(low = "blue", high = "cyan") +
#   xlab("Variables")


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

final_dec_bl$Combination = paste(final_dec_bl$Type1,'-',final_dec_bl$Type2)

final_dec_bl2 = final_dec_bl[,-c(1,2)]
final_dec_bl2
########################
#fix this code tomorrow
########################
final_dec_bl2[10,1] = "Left_L_bl-Left_P_bl"
final_dec_bl2[11,1] = "Left_P_bl-Right_A_bl"
final_dec_bl2[12,1] = "Right_P_bl-Right_A_bl"
final_dec_bl2[13,1] = "Left_A_bl-Right_A_bl"
final_dec_bl2[14,1] = "Left_L_bl-Left_P_bl"
final_dec_bl2[15,1] = "Left_A_bl-Right_A_bl"
final_dec_bl2[16,1] = "Left_L_bl-Right_L_bl"

final_dec_bl2



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
                           size = 3.4,
                           direction = "y",  
                           box.padding = unit(0.09, "lines"),
                           min.segment.length = 1.9)+
  theme_bw()+  
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position="bottom") + xlab("Types")  + xlab("Types")  
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



# you could also try the following test for discrimination:

# - contrast questions - pair the diseases and do the tests
# - features - cluster - for which clustering between them 
# - add spectral clustering to the methods
# - construct cdf, sudo data and then empirical copula of the group 


# # keep this to split the string
# str_split_fixed(result_rf_adhd_tsa$tbl[which(result_rf_adhd_tsa$tbl[,4] < 0.3,
#                                              arr.ind=TRUE),][,1][1],'\\|', 2)


#######################
# FEATURE ENGINEERING #
#######################

#MAKE THIS AUTOMATIC


#############
# ROT FRAME #
#############

final_dec_rf2

meanfun <- function(x, d) {
  return(mean(x[d], na.rm=TRUE))
}

varfun <- function(x, d) {
  return(var(x[d], na.rm=TRUE))
}


mean_boot_adhd_left_l = boot(data_FHIT3_rotfram_adhd$Left_L_rf,
                      statistic = meanfun,
                      R=25)
mean_boot_dys_left_l = boot(data_FHIT3_rotfram_dys$Left_L_rf,
                      statistic = meanfun,
                      R=25)
mean_boot_tsa_left_l = boot(data_FHIT3_rotfram_tsa$Left_L_rf,
                     statistic = meanfun,
                     R=25)
mean_boot_ctr_left_l = boot(data_FHIT3_rotfram_ctr$Left_L_rf,
                     statistic = meanfun,
                     R=25)


mean_boot_adhd_right_a = boot(data_FHIT3_rotfram_adhd$Right_A_rf,
                             statistic = meanfun,
                             R=25)
mean_boot_dys_right_a = boot(data_FHIT3_rotfram_dys$Right_A_rf,
                            statistic = meanfun,
                            R=25)
mean_boot_tsa_right_a = boot(data_FHIT3_rotfram_tsa$Right_A_rf,
                            statistic = meanfun,
                            R=25)
mean_boot_ctr_right_a = boot(data_FHIT3_rotfram_ctr$Right_A_rf,
                            statistic = meanfun,
                            R=25)

var_boot_adhd = boot(data_FHIT3_rotfram_adhd$Left_A_rf,
                      statistic = varfun,
                      R=25)

var_boot_dys = boot(data_FHIT3_rotfram_dys$Left_A_rf,
                     statistic = varfun,
                     R=25)

var_boot_tsa = boot(data_FHIT3_rotfram_tsa$Left_A_rf,
                     statistic = varfun,
                     R=25)

var_boot_ctr = boot(data_FHIT3_rotfram_ctr$Left_A_rf,
                     statistic = varfun,
                     R=25)



new_data_rf = cbind(rbind(mean_boot_adhd_left_l$t, mean_boot_ctr_left_l$t, mean_boot_dys_left_l$t, mean_boot_tsa_left_l$t),
                    rbind(mean_boot_adhd_right_a$t, mean_boot_ctr_right_a$t, mean_boot_dys_right_a$t, mean_boot_tsa_right_a$t),
                    rbind(var_boot_adhd$t, var_boot_ctr$t, var_boot_dys$t, var_boot_tsa$t))


colnames(new_data_rf) = c("Mean_boot_Left_L","Mean_boot_Right_A","Var_boot")

new_data_rf<- as.data.frame(new_data_rf)
new_data_rf$Type = c(rep("ADHD", 25),rep("CTR", 25),rep("DYS", 25),rep("TSA", 25))


new_data_rf = new_data_rf %>% 
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



######
# BL #
######

final_dec_bl2

meanfun <- function(x, d) {
  return(mean(x[d], na.rm=TRUE))
}

varfun <- function(x, d) {
  return(var(x[d], na.rm=TRUE))
}


mean_boot_adhd_bl_r_l = boot(data_FHIT3_black_adhd$Right_L_bl,
                      statistic = meanfun,
                      R=25)
mean_boot_dys_bl_r_l = boot(data_FHIT3_black_dys$Right_L_bl,
                     statistic = meanfun,
                     R=25)
mean_boot_tsa_bl_r_l = boot(data_FHIT3_black_tsa$Right_L_bl,
                     statistic = meanfun,
                     R=25)
mean_boot_ctr_bl_r_l = boot(data_FHIT3_black_ctr$Right_L_bl,
                     statistic = meanfun,
                     R=25)


mean_boot_adhd_bl_l_a = boot(data_FHIT3_black_adhd$Left_A_bl,
                             statistic = meanfun,
                             R=25)
mean_boot_dys_bl_l_a = boot(data_FHIT3_black_dys$Left_A_bl,
                            statistic = meanfun,
                            R=25)
mean_boot_tsa_bl_l_a = boot(data_FHIT3_black_tsa$Left_A_bl,
                            statistic = meanfun,
                            R=25)
mean_boot_ctr_bl_l_a = boot(data_FHIT3_black_ctr$Left_A_bl,
                            statistic = meanfun,
                            R=25)




var_boot_adhd_bl_l_l = boot(data_FHIT3_black_adhd$Left_L_bl,
                     statistic = varfun,
                     R=25)
var_boot_dys_bl_l_l = boot(data_FHIT3_black_dys$Left_L_bl,
                    statistic = varfun,
                    R=25)
var_boot_tsa_bl_l_l = boot(data_FHIT3_black_tsa$Left_L_bl,
                    statistic = varfun,
                    R=25)
var_boot_ctr_bl_l_l = boot(data_FHIT3_black_ctr$Left_L_bl,
                    statistic = varfun,
                    R=25)



var_boot_adhd_bl_r_l = boot(data_FHIT3_black_adhd$Right_L_bl,
                            statistic = varfun,
                            R=25)
var_boot_dys_bl_r_l = boot(data_FHIT3_black_dys$Right_L_bl,
                           statistic = varfun,
                           R=25)
var_boot_tsa_bl_r_l = boot(data_FHIT3_black_tsa$Right_L_bl,
                           statistic = varfun,
                           R=25)
var_boot_ctr_bl_r_l = boot(data_FHIT3_black_ctr$Right_L_bl,
                           statistic = varfun,
                           R=25)


new_data_bl = cbind(rbind(mean_boot_adhd_bl_r_l$t, mean_boot_ctr_bl_r_l$t, mean_boot_dys_bl_r_l$t, mean_boot_tsa_bl_r_l$t),
                    rbind(mean_boot_adhd_bl_l_a$t, mean_boot_ctr_bl_l_a$t, mean_boot_dys_bl_l_a$t, mean_boot_tsa_bl_l_a$t),
                    rbind(var_boot_adhd_bl_l_l$t, var_boot_ctr_bl_l_l$t, var_boot_dys_bl_l_l$t, var_boot_tsa_bl_l_l$t),
                    rbind(var_boot_adhd_bl_r_l$t, var_boot_ctr_bl_r_l$t, var_boot_dys_bl_r_l$t, var_boot_ctr_bl_r_l$t))


colnames(new_data_bl) = c("Mean_boot_r_l","Mean_boot_l_a","Var_boot_l_l","Var_boot_r_l")

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


new_data_rf_unique<- new_data_rf
new_data_rf_unique <- unique(new_data_rf_unique)

tsne_rf <- Rtsne(new_data_rf_unique[,c(1:3)], 
                 dims = 2,
                 pca = TRUE,
                 perplexity = 10)

toplot_rf = as.data.frame(tsne_rf$Y)
toplot_rf$color = new_data_rf_unique$color
toplot_rf$Type = new_data_rf_unique$Type
colnames(toplot_rf) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_rf, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))


tsne3d <- tsne(new_data_rf_unique[,c(1:3)], 
               initial_config = NULL, 
               k = 3, 
               initial_dims = 30, 
               perplexity = 35,
               max_iter = 1000,
               min_cost = 0, 
               epoch_callback = NULL, 
               whiten = TRUE,
               epoch=300)
tsne3d <- cbind(tsne3d,new_data_rf_unique[,1])

par3d(windowRect = c(100, 100, 612, 612))
plot3d(x = tsne3d[,1], 
       y= tsne3d[,2], 
       z= tsne3d[,3], 
       type = "s", 
       radius = 0.5,
       xlab="Dim.1", ylab="Dim.2", zlab="Dim.3", col = new_data_rf_unique$color )
legend3d("topright", legend = c("ASD","DYS","ADHD","CTR"),
         pch = 16, col = c("pink", "yellow", "green", "orange"), cex=0.8)
rgl.snapshot(paste(figs_dir, "rf_tsne_features.png", sep = ""), fmt = 'png')





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

data_rf_scaled <- new_data_rf  %>%  mutate_at(c(1:3), funs(c(scale(.))))
data_bl_scaled <- new_data_bl %>% mutate_at(c(1:4), funs(c(scale(.))))


res.dist.rf <- get_dist(data_rf_scaled[,c(1:3)], stand = TRUE, method = "pearson")
res.dist.bl <- get_dist(data_bl_scaled[,c(1:4)], stand = TRUE, method = "pearson")

fviz_dist(res.dist.rf, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

fviz_dist(res.dist.bl, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))




#############
## K MEANS ##
#############

fviz_nbclust(data_rf_scaled[,c(1:3)], kmeans, method = "gap_stat")
fviz_nbclust(data_bl_scaled[,c(1:2)], kmeans, method = "gap_stat")


set.seed(123)

data_rf_kmeans = data_rf_scaled[,c(1:3)]
data_rf_kmeans = as.data.frame(data_rf_kmeans)
rownames(data_rf_kmeans) <- sapply(1:nrow(data_rf_scaled), 
                                         function(i) paste(data_rf_scaled$Type[i], i, sep = "_"))

data_bl_kmeans = data_bl_scaled[,c(1:4)]
data_bl_kmeans = as.data.frame(data_bl_kmeans)
rownames(data_bl_kmeans) <- sapply(1:nrow(data_bl_scaled), 
                                      function(i) paste(data_bl_scaled$Type[i], i, sep = "_"))


km.res.rotfram <- kmeans(data_rf_kmeans, 4, nstart = 25)
km.res.black <- kmeans(data_bl_kmeans, 4, nstart = 25)


fviz_cluster(km.res.rotfram,
             data = data_rf_kmeans,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(), labelsize = 7)


fviz_cluster(km.res.black,
             data = data_bl_kmeans,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(), labelsize = 7)




################################
##  k-medoids/pam clustering  ##
################################



pam.res.rotatfram <- pam(data_rf_kmeans, 4)
pam.res.black <- pam(data_bl_kmeans, 4)

fviz_cluster(pam.res.rotatfram, 
             labelsize = 7,
             ggtheme = theme_minimal())

fviz_cluster(pam.res.black,
             ggtheme = theme_minimal(), 
             labelsize = 7)



################################
##  HIERARCHICHAL clustering  ##
################################

# Compute hierarchical clustering

data_rf_hier <- as.data.frame(new_data_rf[,c(1:3)])
rownames(data_rf_hier) <- sapply(1:nrow(new_data_rf), 
                                       function(i) paste(new_data_rf$Type[i], i, sep = "_"))

data_bl_hier <- as.data.frame(new_data_bl[,c(1:4)])
rownames(data_bl_hier) <- sapply(1:nrow(new_data_bl), 
                                    function(i) paste(new_data_bl$Type[i], i, sep = "_"))


res.hc.rotfram <- data_rf_hier  %>%  
  scale %>%
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")     

res.hc.black <- data_bl_hier  %>%  
  scale %>%
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")     


fviz_dend(res.hc.rotfram,
          k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE )




fviz_dend(res.hc.black,
          k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE )


############################################
##  Clustering validation and evaluation  ##   -------------------------------------------------------------
############################################


gradient.color <- list(low = "steelblue",  high = "white")

new_data_rf[,c(1:3)] %>%   
  scale()  %>%     # Scale variables
  get_clust_tendency(n = 50, gradient = gradient.color)

new_data_bl[,c(1:4)] %>%   
  scale()  %>%     # Scale variables
  get_clust_tendency(n = 50, gradient = gradient.color)


################################################
## Determining the optimal number of clusters ##
################################################


res.nbclust.rotframe <- new_data_rf[,c(1:3)] %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 10, 
          method = "complete", index ="all") 


res.nbclust.black <- new_data_bl[,c(1:4)] %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 10, 
          method = "complete", index ="all") 






####################
####################
#  linear methods  #
####################
####################

#########################
#  LOGISTIC REGRESSION  #
#########################

#####################
#  FACTOR ANALYSIS  #
#####################

rotframe_factor<- factanal(data_FHIT3_rotfram_scaled[,c(5:10)], factors = 3)

#plot loadings for each factor
plot(
  rotframe_factor$loadings[, 1], 
  rotframe_factor$loadings[, 2],
  xlab = "Factor 1", 
  ylab = "Factor 2", 
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "Factor analysis of prostate data"
)
abline(h = 0, v = 0)

#add column names to each point
text(
  rotframe_factor$loadings[, 1] - 0.08, 
  rotframe_factor$loadings[, 2] + 0.08,
  colnames(data_FHIT3_rotfram_scaled),
  col = "blue"
)


fa.diagram(rotframe_factor)



#########
#  PCA  #
#########


corMat_FHIT3_rotfram <- cor(new_data_rf[,c(1:3)])
corrplot(corMat_FHIT3_rotfram, order = "hclust")

corMat_FHIT3_black <- cor(new_data_bl[,c(1:4)])
corrplot(corMat_FHIT3_black, order = "hclust")


res.pca.FHIT3.rotframe <- PCA(data_rf_hier, scale.unit=TRUE, graph = T)
res.pca.FHIT3.black <- PCA(data_bl_hier, scale.unit=TRUE, graph = T)


#This line of code will sort the variables the most linked to each PC.
dimdesc(res.pca.FHIT3.rotframe)
dimdesc(res.pca.FHIT3.black)

summary(res.pca.FHIT3.rotframe)
summary(res.pca.FHIT3.black)


######################
# Graph of variables #
######################

#Eigenvalues / Variances
fviz_eig(res.pca.FHIT3.rotframe, addlabels = TRUE)
fviz_eig(res.pca.FHIT3.black, addlabels = TRUE)

#Quality of representation
var_FHIT3_rotfram <- get_pca_var(res.pca.FHIT3.rotframe)
var_posture_black <- get_pca_var(res.pca.FHIT3.black)

corrplot(var_FHIT3_rotfram$cos2, is.corr=FALSE) #to check with dimdesc and te one below
corrplot(var_posture_black$cos2, is.corr=FALSE)

# The cos2 values are used to estimate the quality of the representation
# The closer a variable is to the circle of correlations, the better its 
# representation on the factor map (and the more important it is to interpret these components)
# Variables that are closed to the center of the plot are less important for the first components

fviz_pca_var(res.pca.FHIT3.rotframe, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

fviz_pca_var(res.pca.FHIT3.black, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


#Contributions of variables to PCs
fviz_contrib(res.pca.FHIT3.rotframe, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca.FHIT3.black, choice = "var", axes = 1, top = 10)


#######################
# Graph of individuals
#######################

fviz_pca_ind(res.pca.FHIT3.rotframe)
fviz_pca_ind(res.pca.FHIT3.black)



fviz_pca_biplot(res.pca.FHIT3.rotframe, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = new_data_rf$Type,
                col.ind = "black",
                # Color variable by groups
                # col.var = factor(c("sepal", "sepal", "petal", "petal")), #THINK ABOUT THIS
                
                legend.title = list(fill = "Type", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors



fviz_pca_biplot(res.pca.FHIT3.rotframe, 
                # Individuals
                geom.ind = "point",
                fill.ind = new_data_rf$Type, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)



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



fviz_pca_biplot(res.pca.FHIT3.black, 
                # Individuals
                geom.ind = "point",
                fill.ind = new_data_bl$Type, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)


###############
# TSNE OF PCA #  ----- c est shit
###############

ind_pca_FHIT3_rotfram <- get_pca_ind(res.pca.FHIT3.rotframe)

coord_FHIT3_rotfram = ind_pca_FHIT3_rotfram$coord
coord_FHIT3_rotfram = as.data.frame(cbind(as.matrix(coord_FHIT3_rotfram),
                                          data_FHIT3_rotfram$color, 
                                          data_FHIT3_rotfram$Type))
coord_FHIT3_rotfram_unique <- unique(coord_FHIT3_rotfram)
colnames(coord_FHIT3_rotfram_unique)<- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5", "color","Type" )

tsne_out_coord_FHIT3_rotfram <- Rtsne(coord_FHIT3_rotfram_unique[,c(1:5)], 
                                      dims = 2,
                                      pca = TRUE,
                                      perplexity = 10)

toplot_coord_FHIT3_rotfram = as.data.frame(tsne_out_coord_FHIT3_rotfram$Y)
toplot_coord_FHIT3_rotfram$color = coord_FHIT3_rotfram_unique$color
toplot_coord_FHIT3_rotfram$Type = coord_FHIT3_rotfram_unique$Type
colnames(toplot_coord_FHIT3_rotfram) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_coord_FHIT3_rotfram, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))



ind_pca_FHIT3_black <- get_pca_ind(res.pca.FHIT3.black)

coord_FHIT3_black = ind_pca_FHIT3_black$coord
coord_FHIT3_black = as.data.frame(cbind(as.matrix(coord_FHIT3_black),
                                        data_FHIT3_black$color, 
                                        data_FHIT3_black$Type))
coord_FHIT3_black_unique <- unique(coord_FHIT3_black)
colnames(coord_FHIT3_black_unique)<- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5", "color","Type" )

tsne_out_coord_FHIT3_black <- Rtsne(coord_FHIT3_black_unique[,c(1:5)], 
                                    dims = 2,
                                    pca = TRUE,
                                    perplexity = 10)

toplot_coord_FHIT3_black = as.data.frame(tsne_out_coord_FHIT3_black$Y)
toplot_coord_FHIT3_black$color = coord_FHIT3_black_unique$color
toplot_coord_FHIT3_black$Type = coord_FHIT3_black_unique$Type
colnames(toplot_coord_FHIT3_black) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_coord_FHIT3_black, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))


#########################
#########################
#   non-linear methods  #
#########################
#########################

##########
#  KPCA  # -
##########

#plot_kpca https://rdrr.io/github/dswatson/bioplotr/


kpca_rotfram = kpca(as.matrix(data_rf_kmeans),
                    kernel = "rbfdot",
                    kpar = list(sigma = 0.01))

pcv(kpca_rotfram)

to_plot_kpcs_rotfram = as.data.frame(rotated(kpca_rotfram)[,c(1:4)])
colnames(to_plot_kpcs_rotfram) = c("Dim.1", "Dim.2", "Dim.3", "Dim.4")
to_plot_kpcs_rotfram$color = new_data_rf$color
to_plot_kpcs_rotfram$Type = new_data_rf$Type


ggplot(to_plot_kpcs_rotfram, aes(x= Dim.1, y= Dim.2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))


kpca_black = kpca(as.matrix(data_bl_kmeans),
                  kernel = "rbfdot",
                  kpar = list(sigma = 0.01))

pcv(kpca_black)

to_plot_kpcs_black = as.data.frame(rotated(kpca_black)[,c(1:5)])
colnames(to_plot_kpcs_black) = c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5")
to_plot_kpcs_black$color = new_data_bl$color
to_plot_kpcs_black$Type = new_data_bl$Type


ggplot(to_plot_kpcs_black, aes(x= Dim.1, y= Dim.2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))


###################
#  RANDOM FOREST  #
###################

rf_rotframe <-randomForest(as.factor(Type)~.,dat = data_FHIT3_rotfram[c(3,5:10)],
                           ntree=500,
                           importance = T,
                           proximity = T) 



print(rf_rotframe)

#Evaluate variable importance
round(importance(rf_rotframe), 2)
varImpPlot(rf_rotframe)


i_scores <- importance(rf_rotframe)
i_scores <- as.data.frame(i_scores) %>% tibble::rownames_to_column("var") 
i_scores$var<- i_scores$var %>% as.factor()
i_scores_melt = melt(i_scores)
i_scores_melt2 = melt(i_scores[,c(1,6)])


#Plotting the bar and polar charts for comparing variables
i_bar <- ggplot(data = i_scores_melt) + 
  geom_bar(
    stat = "identity",#it leaves the data without count and bin
    mapping = aes(x = var, y=value, fill = variable       ), 
    show.legend = FALSE,
    width = 1
  ) + facet_wrap(~ variable) +
  labs(x = NULL, y = NULL)
i_bar + coord_polar() + theme_minimal()
i_bar + coord_flip() + theme_minimal()


i_bar <- ggplot(data = i_scores_melt2) + 
  geom_bar(
    stat = "identity",#it leaves the data without count and bin
    mapping = aes(x = var, y=value, fill = variable       ), 
    show.legend = FALSE,
    width = 1
  ) + 
  labs(x = NULL, y = NULL)
i_bar + coord_polar() + theme_minimal()
i_bar + coord_flip() + theme_minimal()





rf_black <-randomForest(as.factor(Type)~.,dat = data_FHIT3_black[c(3,5:10)],
                        ntree=500,
                        importance = T,
                        proximity = T) 



print(rf_black)

#Evaluate variable importance
round(importance(rf_black), 2)
varImpPlot(rf_black)


i_scores <- importance(rf_black)
i_scores <- as.data.frame(i_scores) %>% tibble::rownames_to_column("var") 
i_scores$var<- i_scores$var %>% as.factor()
i_scores_melt = melt(i_scores)
i_scores_melt2 = melt(i_scores[,c(1,6)])


#Plotting the bar and polar charts for comparing variables
i_bar <- ggplot(data = i_scores_melt) + 
  geom_bar(
    stat = "identity",#it leaves the data without count and bin
    mapping = aes(x = var, y=value, fill = variable       ), 
    show.legend = FALSE,
    width = 1
  ) + facet_wrap(~ variable) +
  labs(x = NULL, y = NULL)
i_bar + coord_polar() + theme_minimal()
i_bar + coord_flip() + theme_minimal()


i_bar <- ggplot(data = i_scores_melt2) + 
  geom_bar(
    stat = "identity",#it leaves the data without count and bin
    mapping = aes(x = var, y=value, fill = variable       ), 
    show.legend = FALSE,
    width = 1
  ) + 
  labs(x = NULL, y = NULL)
i_bar + coord_polar() + theme_minimal()
i_bar + coord_flip() + theme_minimal()



################
#####  SVM  ####
################

set.seed(314)    

data_rotfram_svm = data_FHIT3_rotfram[c(3,5:10)]

n_rotfram <- nrow(data_FHIT3_rotfram)  
ntrain_rotfram <- round(n_rotfram*0.75) 

tindex_rotfram <- sample(n_rotfram, ntrain_rotfram)   
train_rotfram <- data_rotfram_svm[tindex_rotfram,]   
test_rotfram <- data_rotfram_svm[-tindex_rotfram,] 
svm_rotfram <- svm(as.factor(Type)~., 
                   data=train_rotfram, 
                   method="C-classification",
                   kernel="radial", 
                   gamma=0.01, 
                   cost=100)


summary(svm_rotfram)

# plot(svm_rotfram, 
#      train_rotfram,
#      Left_Lateral_rot_frame ~ Right_Lateral_rot_frame,
#      slice=list(Left_Post_rot_frame=16, Right_Post_rot_frame=4))

prediction_rotfram <- predict(svm_rotfram, test_rotfram)
xtab_rotfram <- table(test_rotfram$Type, prediction_rotfram)
xtab_rotfram

sum(diag(xtab_rotfram)) / nrow(test_rotfram)


plot(cmdscale(dist(data_rotfram_svm[,-1])),
     col = as.integer(as.factor(unlist(data_rotfram_svm[,1]))),
     pch = c("o","+")[1:150 %in% svm_rotfram$index + 1])


#feature importance



data_black_svm = data_FHIT3_black[c(3,5:10)]

n_black <- nrow(data_FHIT3_black)  
ntrain_black <- round(n_black*0.75) 

tindex_black <- sample(n_black, ntrain_black)   
train_black <- data_black_svm[tindex_black,]   
test_black <- data_black_svm[-tindex_black,] 
svm_black <- svm(as.factor(Type)~., 
                 data=train_black, 
                 method="C-classification",
                 kernel="radial", 
                 gamma=0.01, 
                 cost=100)


summary(svm_black)

# plot(svm_black, 
#      train_black,
#      Left_Lateral_rot_frame ~ Right_Lateral_rot_frame,
#      slice=list(Left_Post_rot_frame=16, Right_Post_rot_frame=4))

prediction_black <- predict(svm_black, test_black)
xtab_black <- table(test_black$Type, prediction_black)
xtab_black

sum(diag(xtab_black)) / nrow(test_black)


plot(cmdscale(dist(data_black_svm[,-1])),
     col = as.integer(as.factor(unlist(data_black_svm[,1]))),
     pch = c("o","+")[1:150 %in% svm_black$index + 1])


#feature importance







