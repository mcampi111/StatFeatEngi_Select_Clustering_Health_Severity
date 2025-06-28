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
library(NbClust)
library(RcppAlgos)
library(boot)
library(caret)
library(twosamples)
library(RoDiCE)
library(doParallel)

#############
# FUNCTIONS #
#############

myfviz_silhouette <- function (sil.obj, var.col, label = FALSE, print.summary = TRUE, ...) {
  if (inherits(sil.obj, c("eclust", "hcut", "pam", "clara", 
                          "fanny"))) {
    df <- as.data.frame(sil.obj$silinfo$widths, stringsAsFactors = TRUE)
  }
  else if (inherits(sil.obj, "silhouette")) 
    df <- as.data.frame(sil.obj[, 1:3], stringsAsFactors = TRUE)
  else stop("Don't support an oject of class ", class(sil.obj))
  df <- df[order(df$cluster, -df$sil_width), ]
  if (!is.null(rownames(df))) 
    df$name <- factor(rownames(df), levels = rownames(df))
  else df$name <- as.factor(1:nrow(df))
  df$cluster <- as.factor(df$cluster)
  df$var_col <- var.col
  mapping <- aes_string(x = "name", y = "sil_width", color = "var_col", 
                        fill = "var_col")
  p <- ggplot(df, mapping) + geom_bar(stat = "identity") + 
    labs(y = "Silhouette width Si", x = "", title = paste0("Clusters silhouette plot ", 
                                                           "\n Average silhouette width: ", round(mean(df$sil_width), 
                                                                                                  2))) + ggplot2::ylim(c(NA, 1)) + geom_hline(yintercept = mean(df$sil_width), 
                                                                                                                                              linetype = "dashed", color = "red")
  p <- ggpubr::ggpar(p, ...)
  if (!label) 
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  else if (label) 
    p <- p + theme(axis.text.x = element_text(angle = 45))
  ave <- tapply(df$sil_width, df$cluster, mean)
  n <- tapply(df$cluster, df$cluster, length)
  sil.sum <- data.frame(cluster = names(ave), size = n, ave.sil.width = round(ave, 
                                                                              2), stringsAsFactors = TRUE)
  if (print.summary) 
    print(sil.sum)
  p
}


###############################
# SET DIRECTORY AND READ FILE #
###############################

main_dir = '/Users/mcampi/Desktop/Spin_ML/code/'

figs_dir = paste(main_dir, "/figs/", sep ='')

mydir<- paste(main_dir, "/data/", sep ='')

data_ampl2 = readRDS( file= paste(mydir, "data_ampl2bis.rds", sep = ""))


################
#DATA wrangling#  
################

data_ampl2 <- data_ampl2[data_ampl2$AGE <= 89, ]
data_ampl2 <- data_ampl2[data_ampl2$AGE >= 45, ]

data_ampl2$age_group2 <- cut(data_ampl2$AGE, breaks = seq(0, 100, by = 5), 
                             labels = FALSE,
                             include.lowest = TRUE)
data_ampl2$age_group2 <- factor(paste0((data_ampl2$age_group2 - 1) * 5, "-", 
                                       (data_ampl2$age_group2) * 5))

data_ampl2$age_group3 <- cut(data_ampl2$AGE, breaks = seq(0, 100, by = 1), 
                             labels = FALSE,
                             include.lowest = TRUE)
data_ampl2$age_group3 <- factor(paste0((data_ampl2$age_group3 - 1) , "-", 
                                       (data_ampl2$age_group3) ))


data_ampl2$age_group1 <- cut(data_ampl2$AGE, breaks = seq(0, 100, by = 10), 
                             labels = FALSE,
                             include.lowest = TRUE)
data_ampl2$age_group1 <- factor(paste0((data_ampl2$age_group1 - 1) * 10, "-", 
                                       (data_ampl2$age_group1) * 10))



#################
# ADD VARIABLES #  
#################


data_ampl2 = data_ampl2 %>% 
  mutate(
    # Create categories
    color = dplyr::case_when(
      Classification == "Slight"  ~ "#D0F0C0",
      Classification == "Mild"  ~ "#A1D99B",
      Classification == "Moderate"  ~ "#31A354",
      Classification == "Moderately severe"  ~ "#006837",
      Classification == "Severe"  ~ "#004529"
    ),
    # Convert to factor
    color = factor(
      color,
      level = c("#D0F0C0","#A1D99B","#31A354","#006837","#004529") 
    )
  )



#################
# INITIAL PLOTS #  
#################

#counf ot PEOPLE
data_ampl2_hist0<- data_ampl2 %>% 
  ungroup() %>%
  count(Classification) %>% 
  mutate(percent = n/sum(n), n = n)


color_mapping <- setNames(c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
                          unique(data_ampl2_hist0$Classification))

data_ampl2_hist0 <- data_ampl2_hist0 %>%
  mutate(color = color_mapping[Classification])


#counf ot PEOPLE by age
data_ampl2_hist1<- data_ampl2 %>% 
  ungroup() %>%
  count(Classification ,age_group2) %>% 
  mutate(percent = n/sum(n), n = n)

color_mapping <- setNames(c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
                          unique(data_ampl2_hist1$Classification))

data_ampl2_hist1 <- data_ampl2_hist1 %>%
  mutate(color = color_mapping[Classification])




#histrograms 
#1) by condition
pdf( paste(figs_dir, "hist_people_hl.pdf", sep = ""))  
ggplot(data_ampl2_hist0,
       aes(x = Classification, y = n, fill = Classification)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = TRUE),
             vjust = 0.5,show.legend = FALSE, size = 7)  +
  #scale_fill_grey() +
  xlab("Hearing Loss") + ylab("Number of People")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) +
  scale_fill_manual(values=data_ampl2_hist0$color) 

dev.off()

#2) by condition + age
pdf( paste(figs_dir, "hist_people_hl_age.pdf", sep = ""), width = 12, height = 7) 
ggplot(data_ampl2_hist1,
       aes(x = Classification, y = n, fill = Classification)) + 
  geom_col(position = position_stack(reverse = TRUE)) + 
  facet_wrap(~age_group2, ncol = 5, scales = "free_y") +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = FALSE),
             vjust = 0.5,show.legend = FALSE, size = 3)  +
  #scale_fill_grey() +
  xlab("Hearing Loss") + ylab("Number of People")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white')) +
  scale_fill_manual(values=data_ampl2_hist0$color) 

dev.off()



# BOXPLOTS 
#by conditions
bxpl_data_ampl2 = melt(data_ampl2[,c(4,7:28,30,31)],
                       id.vars = "Classification")

bxpl_data_ampl2 <- bxpl_data_ampl2 %>%
  mutate(Test_Type = case_when(
    grepl("_L", variable) ~ "Audiogram_L",
    grepl("_R", variable) ~ "Audiogram_R",
    TRUE ~ "Speech_Tests"
  ))

pdf( paste(figs_dir, "box_audio_left_hl.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2, Test_Type == "Audiogram_L"), 
       aes(x = factor(str_extract(variable, "[0-9]+"), 
                      levels = c("125", "250", "500", "750", "1000", 
                                 "1500", "2000", "3000", "4000", 
                                 "6000", "8000")), 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_wrap(~Classification, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Frequency Left Audiogram") 
dev.off()



pdf( paste(figs_dir, "box_audio_right_hl.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2, Test_Type == "Audiogram_R"), 
       aes(x = factor(str_extract(variable, "[0-9]+"), 
                      levels = c("125", "250", "500", "750", "1000", 
                                 "1500", "2000", "3000", "4000", 
                                 "6000", "8000")), 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_wrap(~Classification, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Frequency Rigth Audiogram") 
dev.off()


pdf( paste(figs_dir, "box_speech_hl.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2, Test_Type == "Speech_Tests"), 
       aes(x = variable, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_wrap(~Classification, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Speech Tests") 
dev.off()


#by conditions and age
bxpl_data_ampl2_age = melt(data_ampl2[,c(4,7:28,30,31,35)],
                       id.vars = c("Classification", "age_group2"))

bxpl_data_ampl2_age <- bxpl_data_ampl2_age %>%
  mutate(Test_Type = case_when(
    grepl("_L", variable) ~ "Audiogram_L",
    grepl("_R", variable) ~ "Audiogram_R",
    TRUE ~ "Speech_Tests"
  ))


pdf( paste(figs_dir, "box_audio_left_hl_age.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2_age, Test_Type == "Audiogram_L"), 
       aes(x = factor(str_extract(variable, "[0-9]+"), 
                      levels = c("125", "250", "500", "750", "1000", 
                                 "1500", "2000", "3000", "4000", 
                                 "6000", "8000")), 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid(Classification~age_group2, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Frequency Left Audiogram") 
dev.off()

#Alternative 1
pdf( paste(figs_dir, "box_audio_left_hl_age.pdf", sep = ""), width = 17, height = 7)  
ggplot(data = subset(bxpl_data_ampl2_age, Test_Type == "Audiogram_L"), 
       aes(x = age_group2, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid(Classification ~ factor(str_extract(variable, "[0-9]+"), 
                                     levels = c("125", "250", "500", "750", "1000", 
                                                "1500", "2000", "3000", "4000", 
                                                "6000", "8000")),
             scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Age groups") 

dev.off()

#Alternative 2
pdf( paste(figs_dir, "box_audio_left_hl_age.pdf", sep = ""), width = 17, height = 10)  
ggplot(data = subset(bxpl_data_ampl2_age, Test_Type == "Audiogram_L"), 
       aes(x = Classification, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid( factor(str_extract(variable, "[0-9]+"), 
              levels = c("125", "250", "500", "750", "1000", 
               "1500", "2000", "3000", "4000", 
               "6000", "8000"))~ age_group2 ,
             scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Hearing Loss Category") 
dev.off()




pdf( paste(figs_dir, "box_audio_right_hl_age.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2_age, Test_Type == "Audiogram_R"), 
       aes(x = factor(str_extract(variable, "[0-9]+"), 
                      levels = c("125", "250", "500", "750", "1000", 
                                 "1500", "2000", "3000", "4000", 
                                 "6000", "8000")), 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid(Classification~age_group2, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Frequency Right Audiogram") 
dev.off()

#Alternative
ggplot(data = subset(bxpl_data_ampl2_age, Test_Type == "Audiogram_R"), 
       aes(x = age_group2, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid(Classification ~ factor(str_extract(variable, "[0-9]+"), 
                                   levels = c("125", "250", "500", "750", "1000", 
                                              "1500", "2000", "3000", "4000", 
                                              "6000", "8000")),
             scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Age groups") 



pdf( paste(figs_dir, "box_speech_tests_hl_age.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2_age, Test_Type == "Speech_Tests"), 
       aes(x = variable, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid(Classification~age_group2, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Speech_Tests") 
dev.off()


#Alternative
bxpl_data_ampl2_age2= bxpl_data_ampl2_age

bxpl_data_ampl2_age2 = within(bxpl_data_ampl2_age2, {
  Test_Type2 <- ifelse(grepl("SNR", variable), "SNR", 
                      ifelse(grepl("SRT", variable), "SRT", "Other"))
})

pdf( paste(figs_dir, "box_speech_tests_hl_age2.pdf", sep = ""))  
ggplot(data = subset(bxpl_data_ampl2_age2, Test_Type == "Speech_Tests"), 
       aes(x = age_group2, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid(Classification~Test_Type2, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Age Groups") 

dev.off()


bxpl_data_ampl2_age2 <- bxpl_data_ampl2_age2 %>%
  mutate(Test_Type2 = recode(Test_Type2, 
                             "SNR" = "Speech-in-noise",
                             "SRT" = "Speech-in-quiet"))


pdf( paste(figs_dir, "box_speech_tests_hl_age.pdf", sep = ""), width = 17, height = 10)  
ggplot(data = subset(bxpl_data_ampl2_age2, Test_Type == "Speech_Tests"), 
       aes(x = Classification, 
           y = value, color = variable , fill=variable)) +
  geom_violin( ) + 
  stat_summary(fun.y=mean, geom="point", shape=23, size=2, col = "blue",
               fill = "blue") + 
  facet_grid(Test_Type2~age_group2, scales="free_y") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 15, angle = 90, vjust = 0.7, hjust=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.6, 
                                   size = 10),
        axis.text=element_text(size=10), 
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white') ) + 
  ylab("")  + xlab("Age Groups") 
dev.off()
#######################
# RTNSE ORIGINAL DATA #
#######################


set.seed(42)


data_ampl2_left = data_ampl2[,c(4,7:17)]
data_ampl2_right  = data_ampl2[,c(4,18:28)]
data_ampl2_speech = data_ampl2[,c(4,30,31)]

#LEFT
data_ampl2_left_unique<- unique(data_ampl2_left)

tsne_left <- Rtsne(data_ampl2_left_unique, 
                   dims = 2, pca = TRUE, perplexity = 10)

data_ampl2_left_unique$Classification  <- factor(data_ampl2_left_unique$Classification, 
                                         levels = c("Slight", "Mild", "Moderate",
                                                     "Moderately severe", "Severe"))

color_mapping <- setNames(c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
                          levels(data_ampl2_left_unique$Classification))

data_ampl2_left_unique <- data_ampl2_left_unique %>%
  mutate(color = color_mapping[Classification])

toplot_left = as.data.frame(tsne_left$Y)
toplot_left$color = data_ampl2_left_unique$color
toplot_left$Classification = data_ampl2_left_unique$Classification
colnames(toplot_left) = c("Y1", "Y2", "color", "Classification")
toplot_left$color <- as.factor(toplot_left$color)

pdf( paste(figs_dir, "tsne_left_audio.pdf", sep = ""), 
     width = 12, height = 10)  
ggplot(toplot_left, aes(x= Y1, y= Y2, color = Classification )) +
  geom_point(size = 3 )  +
  scale_color_manual(values = c(Slight = "#D0F0C0", Mild = "#A1D99B", Moderate = "#31A354", 
                                `Moderately severe` = "#006837", Severe = "#004529")) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))
dev.off()


#RIGHT
data_ampl2_right_unique<- unique(data_ampl2_right)

tsne_right <- Rtsne(data_ampl2_right_unique, 
                   dims = 2, pca = TRUE, perplexity = 10)

data_ampl2_right_unique$Classification  <- factor(data_ampl2_right_unique$Classification, 
                                                 levels = c("Slight", "Mild", "Moderate",
                                                            "Moderately severe", "Severe"))

color_mapping <- setNames(c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
                          levels(data_ampl2_right_unique$Classification))

data_ampl2_right_unique <- data_ampl2_right_unique %>%
  mutate(color = color_mapping[Classification])

toplot_right = as.data.frame(tsne_right$Y)
toplot_right$color = data_ampl2_right_unique$color
toplot_right$Classification = data_ampl2_right_unique$Classification
colnames(toplot_right) = c("Y1", "Y2", "color", "Classification")
toplot_right$color <- as.factor(toplot_right$color)


ggplot(toplot_right, aes(x= Y1, y= Y2, color = Classification )) +
  geom_point(size = 3 )  +
  scale_color_manual(values = c(Slight = "#D0F0C0", Mild = "#A1D99B", Moderate = "#31A354", 
                                `Moderately severe` = "#006837", Severe = "#004529")) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))

#SPEECH Tests
data_ampl2_speech_unique<- unique(data_ampl2_speech)

tsne_speech <- Rtsne(data_ampl2_speech_unique, 
                    dims = 2, pca = TRUE, perplexity = 10)

data_ampl2_speech_unique$Classification  <- factor(data_ampl2_speech_unique$Classification, 
                                                  levels = c("Slight", "Mild", "Moderate",
                                                             "Moderately severe", "Severe"))

color_mapping <- setNames(c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
                          levels(data_ampl2_speech_unique$Classification))

data_ampl2_speech_unique <- data_ampl2_speech_unique %>%
  mutate(color = color_mapping[Classification])

toplot_speech = as.data.frame(tsne_speech$Y)
toplot_speech$color = data_ampl2_speech_unique$color
toplot_speech$Classification = data_ampl2_speech_unique$Classification
colnames(toplot_speech) = c("Y1", "Y2", "color", "Classification")
toplot_speech$color <- as.factor(toplot_speech$color)


pdf( paste(figs_dir, "tsne_speech.pdf", sep = ""), 
     width = 12, height = 10)  
ggplot(toplot_speech, aes(x= Y1, y= Y2, color = Classification )) +
  geom_point(size = 3 )  +
  scale_color_manual(values = c(Slight = "#D0F0C0", Mild = "#A1D99B", Moderate = "#31A354", 
                                `Moderately severe` = "#006837", Severe = "#004529")) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))
dev.off()




######################
#  STATISTICAL TESTs #
######################

data_stats_test = data_ampl2[,c(4,35,37,7:28,30,31)] #Change the age group cause there are too many

data_stats_test2 = data_stats_test
data_stats_test2$Classification2 <- paste(data_stats_test2$age_group1,  #age_group2
                                          data_stats_test2$Classification, sep = ".")

data_ampl2_slight <- data_stats_test %>%
  filter(Classification == "Slight")  %>% group_by(age_group1) %>% group_split() 

data_ampl2_mild <- data_stats_test %>%
  filter(Classification == "Mild") %>% group_by(age_group1) %>% group_split() 

data_ampl2_moder <- data_stats_test %>%
  filter(Classification == "Moderate") %>% group_by(age_group1) %>% group_split() 

data_ampl2_modersev <- data_stats_test %>%
  filter(Classification == "Moderately severe") %>% group_by(age_group1) %>% group_split() 

data_ampl2_sev <- data_stats_test %>%
  filter(Classification == "Severe") %>% group_by(age_group1) %>% group_split() 


category_hl = levels(data_ampl2$Classification)
category_age = levels(data_ampl2$age_group1)

names_list_combo = levels(interaction(data_ampl2$age_group1,
                                      data_ampl2$Classification))

generate_pairs <- function(category_age) {
  age_group_categories <- paste(category_age, category_hl, sep = ".")
  pairs <- combn(age_group_categories, 2, simplify = FALSE)
  return(pairs)
}

combo_category <- lapply(category_age, generate_pairs)
combo_category <- unlist(combo_category, recursive = FALSE)
combo_category = as.data.frame(do.call(rbind,combo_category))


list_cat = list(data_ampl2_slight,
                data_ampl2_mild,
                data_ampl2_moder,
                data_ampl2_modersev,
                data_ampl2_sev)

list_cat <- flatten(list_cat)


names(list_cat) = names_list_combo

#  2 sample test on the mean 
t_test_mean_diff = as.data.frame(cbind(combo_category,do.call(rbind,lapply(1: dim(combo_category)[1],
                                                                           function(j) do.call(cbind, lapply(1:24, function(i) 
                                                                             round(t.test(list_cat[[combo_category[j,1]]][,3+i],
                                                                                          list_cat[[combo_category[j,2]]][,3+i],
                                                                                          alternative = "two.sided", 
                                                                                          var.equal = TRUE)$p.value,3) ))))))


t_test_mean_diff = t_test_mean_diff %>% 
  mutate_at(3:26, as.numeric)

colnames(t_test_mean_diff) <- c("Classif1", "Classif2",
                                colnames(data_stats_test[4:27]))
t_test_mean_diff


#  welch test
w_test_mean_diff= as.data.frame(cbind(combo_category,
                                      do.call(rbind, lapply(1:dim(combo_category)[1], function(j) {
                                        do.call(cbind, lapply(1:24, function(i) {
                                          # Perform the t-test if both groups have more than 1 observation
                                          if (nrow(list_cat[[combo_category[j, 1]]]) > 1 &&
                                              nrow(list_cat[[combo_category[j, 2]]]) > 1) {
                                            round(t.test(list_cat[[combo_category[j, 1]]][, 3 + i],
                                                         list_cat[[combo_category[j, 2]]][, 3 + i],
                                                         alternative = "two.sided",
                                                         var.equal = FALSE)$p.value, 3)
                                          } else {
                                            # If one of the groups has less than 2 observations, return NA
                                            NA
                                          }
                                        }))
                                      }))))

w_test_mean_diff = w_test_mean_diff %>% 
  mutate_at(3:26, as.numeric)
colnames(w_test_mean_diff) <- c("Classif1", "Classif2",
                                   colnames(data_stats_test[4:27]))
w_test_mean_diff

#  variance ratio test
test_var_ratio = as.data.frame(
  cbind(combo_category,
        do.call(rbind, lapply(1:dim(combo_category)[1], function(j) {
          do.call(cbind, lapply(1:24, function(i) {
            # Perform the variance test if both groups have more than 1 observation
            if (length(unlist(list_cat[[combo_category[j, 1]]][, 3 + i])) > 1 &&
                length(unlist(list_cat[[combo_category[j, 2]]][, 3 + i])) > 1) {
              round(var.test(as.numeric(unlist(list_cat[[combo_category[j, 1]]][, 3 + i])),
                             as.numeric(unlist(list_cat[[combo_category[j, 2]]][, 3 + i])),
                             alternative = "two.sided")$p.value, 3)
            } else {
              # If one of the groups has less than 2 observations, return NA
              NA
            }
          }))
        }))))

test_var_ratio = test_var_ratio %>% 
  mutate_at(3:26, as.numeric)
colnames(test_var_ratio) <-c("Classif1", "Classif2",
                                colnames(data_stats_test[4:27]))
test_var_ratio

# kolmgorov test
test_kolmo = as.data.frame(
  cbind(combo_category,
        do.call(rbind, lapply(1:dim(combo_category)[1], function(j) {
          do.call(cbind, lapply(1:24, function(i) {
            # Perform the Kolmogorov-Smirnov test if both groups have more than 1 observation
            if (length(unlist(list_cat[[combo_category[j, 1]]][, 3 + i])) > 1 &&
                length(unlist(list_cat[[combo_category[j, 2]]][, 3 + i])) > 1) {
              round(ks.test(as.numeric(unlist(list_cat[[combo_category[j, 1]]][, 3 + i])),
                            as.numeric(unlist(list_cat[[combo_category[j, 2]]][, 3 + i])),
                            alternative = "two.sided")$p.value, 3)
            } else {
              # If one of the groups has less than 2 observations, return NA
              NA
            }
          }))
        }))))


test_kolmo = test_kolmo %>% 
  mutate_at(3:26, as.numeric)
colnames(test_kolmo) <- c("Classif1", "Classif2",
                             colnames(data_stats_test[4:27]))
test_kolmo


# CMV test
test_cmv = as.data.frame(
  cbind(combo_category,
        do.call(rbind, lapply(1:dim(combo_category)[1], function(j) {
          do.call(cbind, lapply(1:24, function(i) {
            # Perform the Cramer-von Mises test if both groups have more than 1 observation
            if (length(unlist(list_cat[[combo_category[j, 1]]][, 3 + i])) > 1 &&
                length(unlist(list_cat[[combo_category[j, 2]]][, 3 + i])) > 1) {
              round(as.numeric(
                cvm_test(as.numeric(unlist(list_cat[[combo_category[j, 1]]][, 3 + i])),
                         as.numeric(unlist(list_cat[[combo_category[j, 2]]][, 3 + i]))
                )[2]), 3)
            } else {
              # If one of the groups has less than 2 observations, return NA
              NA
            }
          }))
        }))))

test_cmv = test_cmv %>%
  mutate_at(3:26, as.numeric)
colnames(test_cmv) <- c("Classif1", "Classif2",
                           colnames(data_stats_test[4:27]))
test_cmv


#  group test multi-null tests -->  ANOVA
#1 - test that  that the variance across groups is equal -->NO
bart_test =   as.data.frame(
  cbind(colnames(data_stats_test2[4:27]),
        do.call(rbind, lapply(1:24, function(i) {
          # Extract the variable name
          var_name <- colnames(data_stats_test2)[3 + i]
          
          # Filter out groups with less than 2 observations
          filtered_data <- data_stats_test2 %>%
            group_by(Classification2) %>%
            filter(n() >= 2)
          
          # Perform Bartlett test
          round(bartlett.test(get(var_name) ~ as.factor(Classification2),
                              data = filtered_data)$p.value, 3)
        }))
  )
)
bart_test = bart_test  %>% 
  mutate_at(c('V2'), as.numeric)
colnames(bart_test) = c("variable", "p.value")
bart_test



#2 -ANOVA
anova_test = as.data.frame(
  cbind(colnames(data_stats_test2[4:27]),
        do.call(rbind,lapply(1:24, function(i)
          (round(summary(data_stats_test2 %$% aov( get(
            colnames(data_stats_test2[,3+i]))~ Classification2))[[1]][5]$`Pr(>F)`[1],7)))) ) )

#2 - TUKEY
tukey_test = do.call(cbind,lapply(1:24, function(i)
  round(TukeyHSD(data_stats_test2 %$% aov( get(
    colnames(data_stats_test2[,3+i]))~ Classification2))$Classification2[,4],3)  ))

colnames(tukey_test) = colnames(data_stats_test[4:27])
tukey_test


library(sLED)
# Covariance
cov_test = as.data.frame(
  cbind(combo_category,
        do.call(rbind, lapply(1:dim(combo_category)[1], function(j) {
          # Filter the data for each group
          group1 <- data_stats_test2[,c(4:27,28)] %>%
            filter(Classification2 == combo_category[j, 1]) %>%
            dplyr::select(-Classification2)
          group2 <- data_stats_test2[,c(4:27,28)] %>%
            filter(Classification2 == combo_category[j, 2]) %>%
            dplyr::select(-Classification2)
          
          print(j)
          
          # Check if both groups have at least 2 observations
          if (nrow(group1) >= 2 && nrow(group2) >= 2) {
            # Perform the test if both groups have sufficient observations
            #round(testCov(group1, group2, alpha = 0.05, 
            #              method = "HD", J = 100)$p.value, 3)
            round(sLED(group1, group2, npermute = 100)$pVal, 3)
          } else {
            # If one of the groups has less than 2 observations, return NA
            NA
          }
        }))
  )
)

cov_test = cov_test %>% mutate_at(3, as.numeric)
colnames(cov_test) <- c("Classif1", "Classif2", 
                        "Significance")
cov_test



##########################
# copula 2 sample testing#
##########################
plot_copula = function(string_type, string_typevstype, data){
  
  df = data.frame( group_id =  rep(string_type, 6),
                   desc  = rep(string_typevstype, 6) ,
                   varname = colnames(data)[c(5:10)])
  
  return(df)
  
}


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!NO NEED TO RUN THESE ANYMORE!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#Normally running
#cop_test_left <- list()

# Loop through each combo_category
# for (j in 1:dim(combo_category)[1]) { #
#   
#   # Filter the data for each group based on combo_category
#   group1 <- subset(data_stats_test2, 
#                    Classification2 == combo_category[j, 1], 
#                    select = c(4:14, 26:28))[,-14]
#   group2 <- subset(data_stats_test2, 
#                    Classification2 == combo_category[j, 2],
#                    select = c(4:14, 26:28))[,-14]
#   
#   
#   
#   # Check if both groups have at least 2 observations
#   if (nrow(group1) >= 2 && nrow(group2) >= 2) {
#     # Perform the test if both groups have sufficient observations
#     p_value <- coptest.p(as.matrix(group1), as.matrix(group2),
#                          nperm = 20, approx = FALSE)$tbl
#     # Append the result to the cop_test_left data frame
#     cop_test_left[[j]] <- p_value
#   } else {
#     # If one of the groups has less than 2 observations, return NA
#     cop_test_left[[j]] <- NA
#   }
# }


# Run in parallel
# Define the function to run the coptest with error handling and consistent return type
# Initialize the list to store the results
# cop_test_left <- list()
# cop_test_right <- list()
# 
# 
# # Define the function to run the copula test
# run_coptest <- function(j) {
#   tryCatch({
#     group1 <- subset(data_stats_test2,
#                      Classification2 == combo_category[j, 1],
#                      select = c(4:14, 26:28))[,-14] #select = c(5:25, 26:28))[,-14]
#     group2 <- subset(data_stats_test2,
#                      Classification2 == combo_category[j, 2],
#                      select = c(4:14, 26:28))[,-14] #select = c(15:25, 26:28))[,-14]
# 
#     if (nrow(group1) >= 2 && nrow(group2) >= 2) {
#       result <- coptest.p(as.matrix(group1), as.matrix(group2),
#                           nperm = 20, approx = FALSE)$tbl
#       return(result)
#     } else {
#       return(NA)
#     }
#   }, error = function(e) {
#     message("Error in run_coptest: ", e)
#     return(NA)
#   })
# }
# 
# # # Determine the number of cores to use
# num_cores <- 4  # Or a specific number like 6 or 8
# 
# # Run in chunks of 10
# chunk_size <- 10
# num_chunks <- ceiling(nrow(combo_category) / chunk_size)
# 
# # # Measure the total time taken for all chunks
# total_time_taken <- system.time({
#   for (i in 1:num_chunks) {
#     start_index <- (i - 1) * chunk_size + 1
#     end_index <- min(i * chunk_size, nrow(combo_category))
# 
#     # Create a cluster
#     cl <- makeCluster(num_cores)
#     on.exit(stopCluster(cl), add = TRUE)
# 
#     # Export necessary objects to the cluster
#     clusterExport(cl, c("data_stats_test2", 
#                         "combo_category", 
#                         "coptest.p",
#                         "run_coptest"))
# 
#     # Run the parallel computation for the current chunk
#     chunk_results <- parLapply(cl, start_index:end_index, run_coptest)
# 
#     # Stop the cluster
#     stopCluster(cl)
# 
#     # Combine the results
#     #cop_test_right <- c(cop_test_right, chunk_results)
#     cop_test_left <- c(cop_test_left, chunk_results)
#     
# 
#     # Print progress
#     cat(sprintf("Processed chunk %d out of %d\n", i, num_chunks))
# 
#     # Pause for user input to continue
#     if (i < num_chunks) {
#       readline(prompt = "Press Enter to continue to the next chunk...")
#     }
#   }
# })
# 
# rm(chunk_results, cl)
# 
# 
# saveRDS(cop_test_left,
#         file = "/Users/mcampi/Desktop/Spin_ML/code/Rfiles/cop_test_left_10y_pta.rds" )
# 
# saveRDS(cop_test_right,
#         file = "/Users/mcampi/Desktop/Spin_ML/code/Rfiles/cop_test_right_10y_pta.rds" )
# 

cop_test_left<- readRDS("/Users/mcampi/Desktop/Spin_ML/code/Rfiles/cop_test_left_10y_pta.rds")
cop_test_right<- readRDS("/Users/mcampi/Desktop/Spin_ML/code/Rfiles/cop_test_right_10y_pta.rds")



for (i in 1:dim(combo_category)[1]) {
  
  cop_test_left[[i]] = as.data.frame(cop_test_left[[i]])
  cop_test_left[[i]]$contrast1 <- combo_category[i,1]
  cop_test_left[[i]]$contrast2 <- combo_category[i,2]
  
}


for (i in 1:dim(combo_category)[1]) {
  
  cop_test_right[[i]] = as.data.frame(cop_test_right[[i]])
  cop_test_right[[i]]$contrast1 <- combo_category[i,1]
  cop_test_right[[i]]$contrast2 <- combo_category[i,2]
  
}

cop_test_left_filtered = Filter(function(df) ncol(df) >= 6, cop_test_left)
cop_test_right_filtered = Filter(function(df) ncol(df) >= 6, cop_test_right)


cop_test_left_filtered = do.call(rbind, cop_test_left_filtered)
cop_test_right_filtered = do.call(rbind, cop_test_right_filtered)


#################
#  COPULA PLOTS # - work on this for the paper
#################

toplot7 = plot_copula("ADHD", "TSA vs ADHD", 
                      data_FHIT3_black_adhd) #the data to use correspond to the first string
netvis(obj = result_bl_adhd_tsa,ref = toplot7)

toplot8 = plot_copula("TSA", "TSA vs ADHD", data_FHIT3_black_tsa) #the data to use correspond to the first string
netvis(obj = result_bl_adhd_tsa,ref = toplot8)




#################################
#################################
# FEATURE SELECTION & RANKING ###
#################################
#################################

#mean t test
id_m = which(t_test_mean_diff[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_mean = cbind(do.call(rbind,
                                 lapply(1:dim(id_m)[1], 
                                        function(i) 
                                          t_test_mean_diff[-c(1,2),][ id_m[i,1],c(1:2) ])),
                         do.call(rbind,
                                 lapply(1:dim(id_m)[1], 
                                        function(i) 
                                          colnames(t_test_mean_diff[-c(1,2),])[id_m[i,2]] )))


colnames(stat_dec_mean) = c("Classif1", "Classif2", "Discr_Var")
stat_dec_mean$Test = rep('T-test', dim(stat_dec_mean)[1])
stat_dec_mean$Significance =   rep(0.1, dim(stat_dec_mean)[1])
stat_dec_mean


#mean welch test
id_wm = which(w_test_mean_diff[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_w_mean = cbind(do.call(rbind,
                                 lapply(1:dim(id_wm)[1], 
                                        function(i) 
                                          w_test_mean_diff[-c(1,2),][ id_wm[i,1],c(1:2) ])),
                         do.call(rbind,
                                 lapply(1:dim(id_wm)[1], 
                                        function(i) 
                                          colnames(w_test_mean_diff[-c(1,2),])[id_wm[i,2]] )))


colnames(stat_dec_w_mean) = c("Classif1", "Classif2", "Discr_Var")
stat_dec_w_mean$Test = rep('Welch', dim(stat_dec_w_mean)[1])
stat_dec_w_mean$Significance =   rep(0.1, dim(stat_dec_w_mean)[1])
stat_dec_w_mean


#Variance test
id_v = which(test_var_ratio[-c(1,2),] < 0.10, arr.ind=TRUE)

stat_dec_var = cbind(do.call(rbind,
                                lapply(1:dim(id_v)[1], 
                                       function(i) 
                                         test_var_ratio[-c(1,2),][ id_v[i,1],c(1:2) ])),
                        do.call(rbind,
                                lapply(1:dim(id_v)[1], 
                                       function(i) 
                                         colnames(test_var_ratio[-c(1,2),])[id_v[i,2]] )))


colnames(stat_dec_var) = c("Classif1", "Classif2", "Discr_Var")
stat_dec_var$Test = rep('Variance', dim(stat_dec_var)[1])
stat_dec_var$Significance =   rep(0.10, dim(stat_dec_var)[1])
stat_dec_var


#Distribution test
id_d = which(test_kolmo[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_dist = cbind(do.call(rbind,
                                 lapply(1:dim(id_d)[1], 
                                        function(i) 
                                          test_kolmo[-c(1,2),][ id_d[i,1],c(1:2) ])),
                         do.call(rbind,
                                 lapply(1:dim(id_d)[1], 
                                        function(i) 
                                          colnames(test_kolmo[-c(1,2),])[id_d[i,2]] )))


colnames(stat_dec_dist) = c("Classif1", "Classif2", "Discr_Var")
stat_dec_dist$Test = rep('Kolmogorov', dim(stat_dec_dist)[1])
stat_dec_dist$Significance =   rep(0.1, dim(stat_dec_dist)[1])
stat_dec_dist


#CMV test
id_cmv = which(test_cmv[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_cmv = cbind(do.call(rbind,
                                 lapply(1:dim(id_cmv)[1], 
                                        function(i) 
                                          test_cmv[-c(1,2),][ id_cmv[i,1],c(1:2) ])),
                         do.call(rbind,
                                 lapply(1:dim(id_cmv)[1], 
                                        function(i) 
                                          colnames(test_cmv[-c(1,2),])[id_cmv[i,2]] )))


colnames(stat_dec_cmv) = c("Classif1", "Classif2", "Discr_Var")
stat_dec_cmv$Test = rep('CMV', dim(stat_dec_cmv)[1])
stat_dec_cmv$Significance =   rep(0.1, dim(stat_dec_cmv)[1])
stat_dec_cmv

#Bartlett test
id_bart_bl = which(bart_test[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_bart = do.call(rbind, lapply(1:dim(id_bart_bl)[1], 
                        function(i) bart_test[-c(1,2),][ id_bart_bl[i,1],c(1:2) ]))


colnames(stat_dec_bart) = c("Discr_Var", "Significance")
stat_dec_bart$Test = rep('Bartlett', dim(stat_dec_bart)[1])
stat_dec_bart$Classif1 =   rep("All", dim(stat_dec_bart)[1])
stat_dec_bart$Classif2 =  rep("All", dim(stat_dec_bart)[1])
stat_dec_bart

stat_dec_bart <- stat_dec_bart[colnames(stat_dec_cmv)]
stat_dec_bart



#TUKEY test
id_t = which(tukey_test[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_tukey = as.data.frame(cbind(do.call(rbind,
                                                lapply(1:dim(id_t)[1], 
                                                       function(i) 
                                                         rownames(tukey_test[-c(1,2),])[ id_t[i,1]])),
                                        do.call(rbind,
                                                lapply(1:dim(id_t)[1], 
                                                       function(i) 
                                                         colnames(tukey_test[-c(1,2),])[id_t[i,2]] ))) )

colnames(stat_dec_tukey) = c("Combination", "Discr_Var")
stat_dec_tukey$Test = rep('Tukey', dim(stat_dec_tukey)[1])
stat_dec_tukey$Significance =   rep(0.1, dim(stat_dec_tukey)[1])
stat_dec_tukey


split_at_second_occurrence <- function(string, delimiter) {
  # Find positions of all occurrences of the delimiter
  positions <- gregexpr(delimiter, string)[[1]]
  
  # If there are at least two occurrences, split at the second one
  if (length(positions) >= 2) {
    split_position <- positions[2]
    part1 <- substr(string, 1, split_position - 1)
    part2 <- substr(string, split_position + 1, nchar(string))
    return(c(part1, part2))
  } else {
    # If there are less than two occurrences, return the original string
    return(c(string, NA))
  }
}

# Apply the function to the sample data
new_types <- do.call(rbind,lapply(1:dim(stat_dec_tukey)[1], function(i) 
  split_at_second_occurrence(stat_dec_tukey$Combination[i], "-")))
colnames(new_types) = c("Classif1","Classif2")
stat_dec_tukey = cbind(new_types,stat_dec_tukey[,-1])
stat_dec_tukey


#COVARIANCE test
id_cov = which(cov_test[-c(1,2),] < 0.1, arr.ind=TRUE)

stat_dec_cov = do.call(rbind,
                           lapply(1:dim(id_cov)[1], 
                                  function(i) 
                                    cov_test[-c(1,2),][ id_cov[i,1],c(1:2) ]))

stat_dec_cov$Discr_Var = rep('NA', dim(stat_dec_cov)[1])
stat_dec_cov$Test = rep('Covariance', dim(stat_dec_cov)[1])
stat_dec_cov$Significance =   rep(0.1, dim(stat_dec_cov)[1])
stat_dec_cov


#COPULA test
id_cop_left = which(cop_test_left_filtered[,-c(1,2,4,5,6)] < 0.1, arr.ind=TRUE)
id_cop_right = which(cop_test_right_filtered[,-c(1,2,4,5,6)] < 0.1, arr.ind=TRUE)


stat_dec_copula_left = do.call(rbind,
                            lapply(1:length(id_cop_left), 
                                 function(i) 
                                   cop_test_left_filtered[ id_cop_left[i],c(5,6,1) ]))

stat_dec_copula_right = do.call(rbind,
                                lapply(1:length(id_cop_right), 
                                       function(i) 
                                         cop_test_right_filtered[ id_cop_right[i],c(5,6,1) ]))


colnames(stat_dec_copula_left)<- c("Classif1","Classif2", "Discr_Var")
colnames(stat_dec_copula_right)<- c("Classif1","Classif2", "Discr_Var")

stat_dec_copula_left = as.data.frame(stat_dec_copula_left)
stat_dec_copula_right = as.data.frame(stat_dec_copula_right)

stat_dec_copula_left$Test = rep('Copula', dim(stat_dec_copula_left)[1])
stat_dec_copula_right$Test = rep('Copula', dim(stat_dec_copula_right)[1])


stat_dec_copula_left$Significance =  rep(0.1, dim(stat_dec_copula_left)[1])
stat_dec_copula_right$Significance =  rep(0.1, dim(stat_dec_copula_right)[1])
  


############################################################
# FINAL FEATURES BASED ON STATISTICAL TESTS SIGNIFICIANCE #
###########################################################


final_dec = rbind(stat_dec_mean, stat_dec_w_mean,
                  stat_dec_var, stat_dec_dist,
                  stat_dec_cmv,  stat_dec_bart,
                  stat_dec_tukey,  stat_dec_cov,
                  stat_dec_copula_left, stat_dec_copula_right)

final_dec_left =  final_dec %>%
            filter(!grepl("_R", Discr_Var))

final_dec_left = final_dec_left %>%
  mutate(AgeGroup1 = sub("\\..*", "", Classif1),
         AgeGroup2 = sub("\\..*", "", Classif2),
         HearingLossDegree1 = sub(".*\\.", "", Classif1),
         HearingLossDegree2 = sub(".*\\.", "", Classif2))


final_dec_left$Combination_age = paste(final_dec_left$AgeGroup1 ,'-',final_dec_left$AgeGroup2)
final_dec_left$Combination_HLdegree = paste(final_dec_left$HearingLossDegree1,'-',final_dec_left$HearingLossDegree2)

severity_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")

standardize_combination <- function(combination) {
  # Split the combination into two parts
  parts <- str_split(combination, " - ", simplify = TRUE)
  # Order the parts based on severity levels
  ordered_parts <- sort(parts, index.return = TRUE, decreasing = FALSE, method = "radix")
  # Join the ordered parts back together
  standardized_combination <- paste(ordered_parts$x, collapse = " - ")
  return(standardized_combination)
}

# Apply the standardization to the Combination_HLdegree column
final_dec_left <- final_dec_left %>%
  mutate(Combination_HLdegree = sapply(Combination_HLdegree, standardize_combination))


unique(final_dec_left$Combination_HLdegree)

remove_identical_combinations <- function(combination) {
  # Split the combination into two parts
  parts <- str_split(combination, " - ", simplify = TRUE)
  # Check if the two parts are identical
  return(parts[1] != parts[2])
}

# Apply the function to filter the dataset
filtered_final_dec_left <- final_dec_left %>%
  filter(sapply(Combination_HLdegree, remove_identical_combinations))

unique(filtered_final_dec_left$Combination_HLdegree)

filtered_final_dec_left <- filtered_final_dec_left %>%
  mutate(Discr_Var = str_replace_all(Discr_Var, "FREQ_", ""))

filtered_final_dec_left <- filtered_final_dec_left %>%
  mutate(Discr_Var = str_replace_all(Discr_Var, "_L", ""))

######################
#PLOTS FINAL DECISION#
######################

colnames(filtered_final_dec_left)

filtered_age <- filtered_final_dec_left %>% 
  filter(AgeGroup1 != AgeGroup2)

filtered_hl_nocopula <- filtered_final_dec_left %>% 
  filter(HearingLossDegree1 != HearingLossDegree2, 
         Test != "Copula")

filtered_hl_onlycopula <- filtered_final_dec_left %>% 
  filter(HearingLossDegree1 != HearingLossDegree2, #AgeGroup1 != AgeGroup2,
         Test == "Copula")

desired_order <- c("125", "250", "500", "750", "1000", 
                   "2000", "3000", "4000", "6000", "8000", 
                   "SNR", "SRT", "NA")


hist_vars_age = filtered_age  %>% group_by( Discr_Var,
                                            Combination_HLdegree)   %>% 
                count(Test) %>% 
                    mutate(percent = n/sum(n),
                            n = n)

hist_vars_age <- hist_vars_age %>%
  mutate(Discr_Var = factor(Discr_Var, levels = desired_order))


hist_vars_hl_nocopula = filtered_hl_nocopula  %>% group_by( Discr_Var,
                                                            Combination_HLdegree)   %>% 
  count(Test) %>% 
  mutate(percent = n/sum(n),
         n = n)

hist_vars_hl_nocopula <- hist_vars_hl_nocopula %>%
  mutate(Discr_Var = factor(Discr_Var, levels = desired_order))



hist_vars_hl_onlycopula = filtered_hl_onlycopula  %>% group_by( Discr_Var,Combination_HLdegree)   %>% 
  count(Test) %>% 
  mutate(percent = n/sum(n),
         n = n)


ggplot(hist_vars_age, 
       aes(x = Discr_Var,
           y = n,
           fill = Combination_HLdegree)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  facet_grid(~Test, scales = 'free_y') +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15)) +
  ylab("# Statistical Siginificant Test") +xlab("Discriminant Variables")


ggplot(hist_vars_hl_nocopula, 
       aes(x = Discr_Var, y = n, 
           fill = Combination_HLdegree)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  facet_grid(Combination_HLdegree~Test, scales = 'free_y') +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15)) +
  ylab("# Statistical Siginificant Test") +xlab("Discriminant Variables")


ggplot(hist_vars_hl_onlycopula, 
       aes(x = Discr_Var, y = n, fill = Combination_HLdegree)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + facet_wrap(~Test, scales = 'free_y') +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(color = 'black'),
        strip.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=15, angle = 90),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=15)) 



#######################
#######################
# FEATURE ENGINEERING #
#######################
#######################

prova = final_dec_left %>%
  distinct(Discr_Var, Test)

prova=  final_dec_left %>%
  distinct(Discr_Var, Test, .keep_all = TRUE)

final_dec_left_feat =  final_dec_left %>% 
  filter(Test != "Bartlett", 
         Test != "Welch",
         Test != "Covariance",
         Test != "CMV",
         Test != "Tukey")

unique(final_dec_left_feat$Test)


final_dec_left_feat <- final_dec_left_feat %>%
  mutate(Feat_Type = case_when(
    Test %in% c("T-test") ~ "Mean",
    Test == "Variance" ~ "Variance",
    Test == "Kolmogorov" ~ "Distribution",
    Test == "Copula" ~ "Rank",
    TRUE ~ NA_character_  # Optional: Handle any unexpected values
  ))


#In the next round i will have to fix this!!!!
#CAN I USE THIS WITHOUT BOOTSTRAPING????????

# calculate_mean_variance_distr <- function(df, ...) {
#   columns <- list(...)
#   result <- lapply(df[, unlist(columns), drop = FALSE], function(column) {
#     list(
#       Mean = mean(column, na.rm = TRUE), 
#       Variance = var(column, na.rm = TRUE),
#       ECDF = ecdf(column)(column),
#       Rank = rank(column)
#     )
#   })
#   return(result)
# }
# 
# feat_left <- lapply(list_cat, calculate_mean_variance_distr, columns = c(4:14, 26, 27))

# extract_statistics <- function(feature) {
#   c(mean = feature$Mean, variance = feature$Variance, 
#     ECDF = feature$ECDF, rank = feature$Rank)
# }
# 
# lapply(1:length(feat_left), function(i)
#   colnames(t(sapply(feat_left[[i]], function(x) unlist(extract_statistics(x))))))
# 
# feat_left2 = lapply(1:length(feat_left), function(i)
#   as.vector(t(t(sapply(feat_left[[i]], function(x) 
#     unlist(extract_statistics(x)))))) )
# 


mystatistic <- function(data, indices) {
  # Subset the original data using the indices
  boot_sample <- data[indices]
  
  # Compute the statistics of interest on the bootstrap sample
  boot_mean <- mean(boot_sample, na.rm = TRUE)
  boot_var <- var(boot_sample, na.rm = TRUE)
  
  # Compute ECDF and sample 10 values
  ecdf_vals <- ecdf(boot_sample)(boot_sample)
  boot_ecdf <- sample(ecdf_vals, 10, replace = TRUE)
  
  # Compute ranks and sample 10 values
  rank_vals <- rank(boot_sample)
  boot_rank <- sample(rank_vals, 10, replace = TRUE)
  
  # Combine results into a single vector
  return(c(boot_mean, boot_var, boot_ecdf, boot_rank))
}



bootstrap_columns <- function(df, columns, R = 20) {
  results <- lapply(columns, function(col) {
    boot_result <- boot(df[[col]], mystatistic, R = R)
    list(
      Means = boot_result$t[, 1],
      Variances = boot_result$t[, 2],
      ECDFs = boot_result$t[, 3:12],
      Ranks = boot_result$t[, 13:22]
    )
  })
  names(results) <- columns
  return(results)
}

# Apply the function to each data frame in list_cat
columns_to_bootstrap <- colnames(list_cat[[1]])[c(4:14, 26, 27)]

bootstrap_results <- lapply(list_cat, function(df) {
  bootstrap_columns(df, columns_to_bootstrap)
})

extract_statistics <- function(feature) {
  list(mean = feature$Mean, 
       variance = feature$Variance,
        ECDF = feature$ECDF, 
       rank = feature$Rank)
}

feat_left = lapply(1:length(bootstrap_results),function(i)
  sapply(bootstrap_results[[i]], function(x) extract_statistics(x)))

feat_left = lapply(1:length(bootstrap_results),function(i)
  cbind (do.call(cbind,feat_left[[i]][c(seq(1, by = 4, length.out = 13))]),
         do.call(cbind,feat_left[[i]][c(seq(2, by = 4, length.out = 13))]),
         do.call(cbind,feat_left[[i]][c(seq(3, by = 4, length.out = 13))]),
         do.call(cbind,feat_left[[i]][c(seq(4, by = 4, length.out = 13))]) ) )


cols_name = c( "125","250","500","750","1000","1500","2000","3000","4000","6000",
               "8000","SRT","SNR")

col_means = sapply(cols_name, paste, ".mean", sep = "")
col_var = sapply(cols_name, paste, ".variance", sep = "")
col_ecdf = as.vector(sapply(cols_name, paste, ".ECDF", sapply(1:10,function(i) i ), sep= ""))
col_rank = as.vector(sapply(cols_name, paste, ".rank", sapply(1:10,function(i) i ), sep= ""))



new_colnames <- c(col_means, col_var, col_ecdf, col_rank)


for (i in 1:length(feat_left)) {
  
  feat_left[[i]] =  data.frame(feat_left[[i]])
  colnames(feat_left[[i]]) = new_colnames
  feat_left[[i]]$HL = sapply(strsplit(names_list_combo[i], "\\."), "[[", 2)
  feat_left[[i]]$age_group = sapply(strsplit(names_list_combo[i], "\\."), "[[", 1)
}


feat_left = do.call(rbind,feat_left)

colnames(feat_left)

ordered_col_names <- c("HL", "age_group", colnames(feat_left)[-c(287,288)])

# Reorder the columns in feat_left dataframe
feat_left <- feat_left[, ordered_col_names]

colnames(feat_left)


#mean 3:13 14:15 variance  16:26 27:28 ecdf 29:138  139:158  rank 159:268  269:288
feat_left2 = feat_left[,c(1,3:13)]
feat_left2 = feat_left[,c(1,14:15)] 
feat_left2 = cbind(feat_left[,c(1,2:13)], feat_left[,c(14:15)] )

feat_left2 = feat_left[,c(1,16:26 )]
feat_left2 = feat_left[,c(1,27:28)] 

feat_left2 = cbind(feat_left[,c(1,14:15)], feat_left[,c(269:288)] ) #speech mean and var


feat_left2 = feat_left[,c(1,29:138)]
feat_left2 = feat_left[,c(1,139:158)] 

feat_left2 = feat_left[,c(1,159:268)]
feat_left2 = feat_left[,c(1,269:288)] 




#########
# RTSNE #
#########

set.seed(42)

#by experiments
feat_left_unique_audio_hl<- feat_left2  


feat_left_unique_audio_hl = drop_na(feat_left_unique_audio_hl)
feat_left_unique_audio_hl <- unique(feat_left_unique_audio_hl)

feat_left_unique_audio_hl = feat_left_unique_audio_hl %>%
  group_by(across(2:ncol(feat_left_unique_audio_hl))) %>% 
  slice(1) %>%
  ungroup()

tsne_feat_left <-Rtsne(feat_left_unique_audio_hl[,c(2:ncol(feat_left_unique_audio_hl))],
                 dims = 2,
                 pca = TRUE,
                 perplexity = 5)

toplot_feat_left = as.data.frame(tsne_feat_left$Y)
hl_levels <- c("Slight", "Mild", "Moderate", "Moderately severe", "Severe")
feat_left_unique_audio_hl$HL <- factor(feat_left_unique_audio_hl$HL, levels = hl_levels)
color_mapping <- setNames(c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
                                           levels(feat_left_unique_audio_hl$HL))
feat_left_unique_audio_hl <- feat_left_unique_audio_hl %>%
  mutate(color = color_mapping[HL])

toplot_feat_left$color = feat_left_unique_audio_hl$color
toplot_feat_left$color <- as.factor(toplot_feat_left$color)
toplot_feat_left$Type = feat_left_unique_audio_hl$HL
colnames(toplot_feat_left) = c("Y1", "Y2", "color", "HL")

ggplot(toplot_feat_left, aes(x= Y1, y= Y2, color = HL )) +
  geom_point(size = 3 )  +
  scale_color_manual(values = c(Slight = "#D0F0C0", Mild = "#A1D99B", Moderate = "#31A354", 
                                `Moderately severe` = "#006837", Severe = "#004529")) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))




################
################
## CLUSTERING ##
################
################

#mean 2:13 14:15 variance  16:26 27:28 ecdf 29:138  139:158  rank 159:268  269:288
feat_left2 = feat_left[,c(1,3:13)]
feat_left2 = feat_left[,c(1,14:15)] 
feat_left2 = cbind(feat_left[,c(1,3:13)], feat_left[,c(14:15)] )

feat_left2 = feat_left[,c(1,16:26 )]
feat_left2 = feat_left[,c(1,27:28)] 

feat_left2 = cbind(feat_left[,c(1,14:15)], feat_left[,c(269:288)] ) #speech mean and var


feat_left2 = feat_left[,c(1,29:138)]
feat_left2 = feat_left[,c(1,139:158)] 

feat_left2 = feat_left[,c(1,159:268)]
feat_left2 = feat_left[,c(1,269:288)] 



feat_left2 = data_ampl2[,c(4,30:31)]

#############
## K MEANS ##
#############


set.seed(123)


data_feat_left2_scaled = feat_left2 %>%
  mutate_at(c(2:(ncol(feat_left2)
                 -c(1))), 
            funs(c(scale(.)))) 


data_kmeans = as.data.frame(data_feat_left2_scaled[,c(2:ncol(data_feat_left2_scaled))])
rownames(data_kmeans) <- sapply(1:nrow(data_feat_left2_scaled), 
                                   function(i) paste(data_feat_left2_scaled$HL[i],
                                                     i, sep = "_"))

data_kmeans = data_kmeans[complete.cases(data_kmeans), ]

km.res <- kmeans(data_kmeans, 5, nstart = 25)

fviz_cluster(km.res,
             data = data_kmeans,
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

fviz_cluster(km.res,
             data = data_kmeans, 
             ellipse.type = "convex",
             labelsize = 0,
             ggtheme = theme_bw()) +
             custom_theme +   
             scale_color_manual(values = c("#D0F0C0", "#A1D99B","#31A354","#006837","#004529")) +
             labs(title = "") +
             scale_shape_manual(values = c(3,17,19,22, 25) ,
                                labels = c("Slight", "Mild", "Mod", 
                                           "Mod Sev", "Sev"))


#confusion matrix
table(km.res$cluster,data_feat_left2_scaled$HL)
table(data_feat_left2_scaled$HL, km.res$cluster)



# True class labels
true_labels <-  factor(data_feat_left2_scaled$HL,levels = hl_levels)
# Cluster assignments
cluster_labels <- km.res$cluster
# Create a mapping between cluster numbers and class labels
# Replace the numbers with the corresponding class labels
cluster_to_class_mapping <- c("Slight", "Mild", "Moderate","Moderately severe", "Severe")
cluster_labels_mapped <- factor(cluster_to_class_mapping[cluster_labels],levels = hl_levels)
# Create the confusion matrix
confusion_matrix <- table(true_labels, cluster_labels_mapped)

# Print the confusion matrix
print(confusion_matrix)
confusionMatrix(data = cluster_labels_mapped, reference = true_labels)




################################
##  HIERARCHICHAL clustering  ## # Compute hierarchical clustering
################################

data_hier <- as.data.frame(data_feat_left2_scaled[,c(2:ncol(data_feat_left2_scaled))])
rownames(data_hier) <- sapply(1:nrow(data_feat_left2_scaled), 
                   function(i) paste(data_feat_left2_scaled$HL[i], i, sep = "_"))



res.hc = eclust(data_hier, "hclust", k = 5)

fviz_dend(res.hc,
          k = 5, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#D0F0C0","#A1D99B","#31A354","#006837","#004529"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE ) +custom_theme +  
  labs(title = "")



#confusion matrix
clusters =cutree(res.hc, k=5)
table(data_feat_left2_scaled$HL,clusters)


# True class labels
true_labels <- data_feat_left2_scaled$HL
# Cluster assignments
cluster_labels <- clusters
# Create a mapping between cluster numbers and class labels
# Replace the numbers with the corresponding class labels
cluster_to_class_mapping <- c("Slight", "Mild", "Moderate",
                              "Moderately severe", "Severe")
cluster_labels_mapped <- factor(cluster_to_class_mapping[cluster_labels],
                                levels = c("Slight", "Mild",
                                           "Moderate","Moderately severe", 
                                           "Severe"))

true_labels = factor(true_labels, levels = c("Slight", "Mild",
                                             "Moderate","Moderately severe", 
                                             "Severe"))
# Create the confusion matrix
confusion_matrix <- table(true_labels, cluster_labels_mapped)

# Print the confusion matrix
print(confusion_matrix)
confusionMatrix(data = cluster_labels_mapped, 
                reference = true_labels)

silhouette(cluster_labels, dist(data_bl_scaled[,c(1:3)])). #FIX THIS AND THEN DO THE REST up



