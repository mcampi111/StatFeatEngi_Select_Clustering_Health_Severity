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

###############################
# SET DIRECTORY AND READ FILE #
###############################


mydir<- "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\data\\"

data_EM<- read_excel(paste(mydir, "dataML.xlsx", sep = ""),
                      sheet = 1, col_names = T) 


data_posture<- read_excel(paste(mydir, "dataML.xlsx", sep = ""),
                          sheet = 2, col_names = T) 


data_dual_task<- read_excel(paste(mydir, "dataML.xlsx", sep = ""),
                            sheet = 3, col_names = T) 



data_FHIT<- read_excel(paste(mydir, "dataML.xlsx", sep = ""),
                            sheet = 4, col_names = T) 


################
#DATA wrangling#  
################

data_EM2 = data_EM %>%
  row_to_names(row_number = 1) %>% clean_names()

colnames(data_EM2)[5:9] <- paste(colnames(data_EM2)[5:9], "gap", sep = '_')
colnames(data_EM2)[10:14] <- paste(colnames(data_EM2)[10:14], "step", sep = '_')
colnames(data_EM2)[15:19] <- paste(colnames(data_EM2)[15:19], "overlap", sep = '_')
colnames(data_EM2)[20:21] <- paste(colnames(data_EM2)[20:21], "anti_s", sep = '_')
colnames(data_EM2)[22:23] <- paste(colnames(data_EM2)[22:23], "pursuit", sep = '_')
colnames(data_EM2)[24] <- paste(colnames(data_EM2)[24], "fix", sep = '_')
colnames(data_EM2)[25] <- paste(colnames(data_EM2)[25], "fix_dis", sep = '_')
colnames(data_EM2)[26:29] <- paste(colnames(data_EM2)[26:29], "ados", sep = '_')
colnames(data_EM2)[30:33] <- paste(colnames(data_EM2)[30:33], "adi_r", sep = '_')

data_EM2<- data_EM2[,-c(34:41)]

data_FHIT2 = data_FHIT

colnames(data_FHIT2)[1:4] <-  data_FHIT2[2,1:4]
data_FHIT2[2,1:4] <- c(NA,NA,NA,NA)
data_FHIT2<- data_FHIT2[,-c(15:52)]
colnames(data_FHIT2)[5:14]<- paste(data_FHIT2[1,], data_FHIT2[2,], sep = "_")[-c(1:4)]
colnames(data_FHIT2)[5:10]<- paste(colnames(data_FHIT2)[5:10], "rot_frame", sep = '_')
colnames(data_FHIT2)[11:14]<- paste(colnames(data_FHIT2)[11:14], "black", sep = '_')
data_FHIT2<- data_FHIT2[-c(1:2),]


data_FHIT2 = data_FHIT2 %>% 
               mutate_at(c(4:14), as.numeric)
  
################
#Missing values#  
################

vis_miss(data_EM2)   #cannot use this  
vis_miss(data_FHIT2) #this is good
vis_miss(data_posture) # this --> ask if what I did is good
vis_miss(data_dual_task) #this is good


data_FHIT3 <- na.omit(data_FHIT2)
vis_miss(data_FHIT3)

data_dual_task2<- na.omit(data_dual_task)
vis_miss(data_dual_task2)

#I kill the columns with NA so to retain as many variables as possible but this might not be the best solution
data_posture2<- data_posture[,-c(17:20)]
vis_miss(data_posture2)
data_posture2 <- data_posture2[,-c(7,10,13,16)] 
vis_miss(data_posture2)
data_posture2<- na.omit(data_posture2)
vis_miss(data_posture2)


#################
# ADD VARIABLES #  
#################

range(data_FHIT3$Age)
range(data_dual_task2$Ages)
range(data_posture2$Ages)


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



data_dual_task2 = data_dual_task2 %>% 
  mutate(
    # Create categories
     age_group = dplyr::case_when(
      Ages > 6 & Ages <= 8  ~ "6-8",
      Ages > 8 & Ages <= 10 ~ "8-10",
      Ages > 10 & Ages <= 12 ~ "10-12",
      Ages > 12             ~ "> 12"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("6-8", "8-10","10-12", "> 12")
    )
  )



data_posture2 = data_posture2 %>% 
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      Ages <= 6  ~ "< 6",
      Ages > 6 & Ages <= 8  ~ "6-8",
      Ages > 8 & Ages <= 10 ~ "8-10",
      Ages > 10 & Ages <= 12 ~ "10-12",
      Ages > 12             ~ "> 12"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("< 6","6-8", "8-10","10-12", "> 12")
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


data_dual_task2 = data_dual_task2 %>% 
  mutate(
    # Create categories
    color = dplyr::case_when(
      Type == "TSA"  ~ "pink",
      Type == "DYS"  ~ "green",
      Type == "TDAH"  ~ "yellow",
      Type == "CTR"  ~ "orange"
    ),
    # Convert to factor
    color = factor(
      color,
      level = c("pink","green", "yellow", "orange")
    )
  )


colnames(data_dual_task2) <- c("Prenom_nom",
                             "Study",
                             "Type",
                             "Ages",
                             "L_step_Assis",
                             "L_mem_A",
                             "percent_errors_in_memA",
                             "FIX_N_sac_A",
                             "FIX_N_sac_P",
                             "L_step_Platform",
                             "L_mem_P",
                             "percent_errors_P",
                             "Sur_step",
                             "Sur_mem",
                             "Sur_fix",
                             "Vit_step",
                             "Vit_mem",
                             "Vit_fix",
                             "IIP_step",
                             "IIP_mem",
                             "IIP_fix",
                             "age_group",
                             "color")

data_posture2 = data_posture2 %>% 
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


colnames(data_posture2) <- c("Prenom_nom",
                             "Study",
                             "Type",
                             "Ages",
                             "Surface_YO_Stable_cm2",
                             "Surf_YF_Stable",
                             "Surf_YO_Instable",
                             "S_YF_I",
                             "Vitesse_YO_Stable_mm_s",
                             "V_YF_S",
                             "V_YO_I",
                             "V_YF_I",
                             "age_group",
                             "color")

##############
# FINAL DATA #  
##############


final_data<- list(data_FHIT3, data_dual_task2, data_posture2)


#################
# INITIAL PLOTS #  
#################



#counf ot PEOPLE
data_FHIT3_hist0<- data_FHIT3 %>% 
                    count(Type) %>% 
                      mutate(percent = n/sum(n), n = n)

data_dual_task2_hist0<- data_dual_task2 %>% 
                          count(Type) %>% 
                           mutate(percent = n/sum(n), n = n)

data_posture2_hist0<- data_posture2 %>% 
                        count(Type) %>% 
                           mutate(percent = n/sum(n), n = n)



#counf ot PEOPLE by age
data_FHIT3_hist1<- data_FHIT3 %>% 
                   count(Type,age_group) %>% 
                    mutate(percent = n/sum(n), n = n)

data_dual_task2_hist1<- data_dual_task2 %>% 
                         count(Type,age_group) %>% 
                           mutate(percent = n/sum(n), n = n)

data_posture2_hist1<- data_posture2 %>% 
                        count(Type,age_group) %>% 
                           mutate(percent = n/sum(n), n = n)




#histrograms 

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


ggplot(data_dual_task2_hist0,
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


ggplot(data_posture2_hist0,
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


# histogram by age

label_group1 = c("6-8", "8-10", "10-12",">12")

label_group2 = c("<6", "6-8", "8-10", "10-12",">12")


data_FHIT3_hist1$color = c(rep("pink", 4), rep("green", 3),rep("yellow", 4), rep("orange", 4))
data_dual_task2_hist1$color = c(rep("pink", 4), rep("green", 4),rep("yellow", 3),rep("orange", 4))
data_posture2_hist1$color = c(rep("pink", 4), rep("green", 5),rep("yellow", 3),rep("orange", 3))


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

ggplot(data_dual_task2_hist1, aes(x = age_group, y = n, fill = Type)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , 
             position = position_stack(reverse = TRUE),
             color = data_dual_task2_hist1$color,
             vjust = 1,
             show.legend = FALSE)  +
  #  scale_fill_grey() +
  xlab("Age groups") + ylab("Number of children") +
  theme(axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')

ggplot(data_posture2_hist1, aes(x = age_group, y = n, fill = Type)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , 
             position = position_stack(reverse = TRUE),
             color = data_posture2_hist1$color,
             vjust = 1,
             show.legend = FALSE)  +
  #  scale_fill_grey() +
  xlab("Age groups") + ylab("Number of children") +
  theme(axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')




# BOXPLOTS variables  (no age)

bxpl_data_FHIT3 = melt(data_FHIT3[,-c(1,2,4,15,16)], id.vars = "Type")

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



bxpl_data_dual_task2_group1 = melt(data_dual_task2[,-c(1,2,4,7:9,12:22,23)], id.vars = "Type")
bxpl_data_dual_task2_group2 = melt(data_dual_task2[,-c(1,2,4,5,6,10,11,22,23)], id.vars = "Type")


ggplot(bxpl_data_dual_task2_group1, 
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


ggplot(bxpl_data_dual_task2_group2, 
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



bxpl_data_posture2 = melt(data_posture2[,-c(1,2,4,13,14)], id.vars = "Type")

ggplot(bxpl_data_posture2, 
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






# BOXPLOTS variables  (age) (NOT USEFUL I THINK - TOASK)

#########
# RTSNE #
#########

set.seed(42) # Sets seed for reproducibility

#FHIT3

data_FHIT3_unique<- data_FHIT3
data_FHIT3_unique <- unique(data_FHIT3_unique[,c(3,5:14,16)])

tsne_out_data_FHIT3 <- Rtsne(data_FHIT3_unique[,c(2:11)], 
                             dims = 2,
                             pca = TRUE,
                             perplexity = 10)

toplot_FHIT3 = as.data.frame(tsne_out_data_FHIT3$Y)
toplot_FHIT3$color = data_FHIT3_unique$color
toplot_FHIT3$Type = data_FHIT3_unique$Type
colnames(toplot_FHIT3) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_FHIT3, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))



#dual task

data_dual_task_unique<- data_dual_task2
data_dual_task_unique <- unique(data_dual_task_unique[,c(3,5:21,23)])

tsne_out_data_dual_task <- Rtsne(data_dual_task_unique[,c(2:18)], 
                             dims = 2,
                             pca = TRUE,
                             perplexity = 10)

toplot_2task = as.data.frame(tsne_out_data_dual_task$Y)
toplot_2task$color = data_dual_task_unique$color
toplot_2task$Type = data_dual_task_unique$Type
colnames(toplot_2task) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_2task, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))


#posture

data_posture_unique<- data_posture2
data_posture_unique <- unique(data_posture_unique[,c(3,5:12,14)])

tsne_out_data_posture <- Rtsne(data_posture_unique[,c(2:9)], 
                                 dims = 2,
                                 pca = TRUE,
                                 perplexity = 10)

toplot_posture = as.data.frame(tsne_out_data_posture$Y)
toplot_posture$color = data_posture_unique$color
toplot_posture$Type = data_posture_unique$Type
colnames(toplot_posture) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_posture, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))

####################
####################
#  linear methods  #
####################
####################

#########
#  PCA  #
#########

data_FHIT3.scale = scale(data_FHIT3[,c(5:14)],center=TRUE,scale=TRUE)
corMat_FHIT3 <- cor(data_FHIT3.scale)
corrplot(corMat_FHIT3, order = "hclust")


data_posture2.scale = scale(data_posture2[,c(5:12)],center=TRUE,scale=TRUE)
corMat_posture2 <- cor(data_posture2.scale)
corrplot(corMat_posture2, order = "hclust")


data_dual_task.scale = scale(data_dual_task2[,c(5:21)],center=TRUE,scale=TRUE)
corMat_2taks <- cor(data_dual_task.scale)
corrplot(corMat_2taks, order = "hclust")


res.pca.FHIT3 <- PCA(data_FHIT3[,c(5:14)], scale.unit=TRUE, graph = T)
res.pca.posture <- PCA(data_posture2[,c(5:12)], scale.unit=TRUE, graph = T)
res.pca.2task <- PCA(data_dual_task2[,c(5:21)], scale.unit=TRUE, graph = T)

#This line of code will sort the variables the most linked to each PC.
dimdesc(res.pca.FHIT3)
dimdesc(res.pca.posture)
dimdesc(res.pca.2task)


summary(res.pca.FHIT3)
summary(res.pca.posture)
summary(res.pca.2task)


######################
# Graph of variables #
######################

#Eigenvalues / Variances
fviz_eig(res.pca.FHIT3, addlabels = TRUE, ylim = c(0, 50))
fviz_eig(res.pca.posture, addlabels = TRUE, ylim = c(0, 50))
fviz_eig(res.pca.2task, addlabels = TRUE, ylim = c(0, 50))


#Quality of representation
var_FHIT3 <- get_pca_var(res.pca.FHIT3)
var_posture <- get_pca_var(res.pca.posture)
var_2task <- get_pca_var(res.pca.2task)

corrplot(var_FHIT3$cos2, is.corr=FALSE) #to check with dimdesc and te one below
corrplot(var_posture$cos2, is.corr=FALSE)
corrplot(var_2task$cos2, is.corr=FALSE)


# The cos2 values are used to estimate the quality of the representation
# The closer a variable is to the circle of correlations, the better its 
# representation on the factor map (and the more important it is to interpret these components)
# Variables that are closed to the center of the plot are less important for the first components

fviz_pca_var(res.pca.FHIT3, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
             )

fviz_pca_var(res.pca.posture, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

fviz_pca_var(res.pca.2task, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


#Contributions of variables to PCs
fviz_contrib(res.pca.FHIT3, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca.posture, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca.2task, choice = "var", axes = 1, top = 10)


#######################
# Graph of individuals
#######################

fviz_pca_ind(res.pca.FHIT3)
fviz_pca_ind(res.pca.posture)
fviz_pca_ind(res.pca.2task)



#color them by type _ add color to the main dataset and do it again 
ind.FHIT3<- fviz_pca_ind(res.pca.FHIT3,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = data_FHIT3$Type, # color by groups
             palette = unique(data_FHIT3$color),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

ind.FHIT3

ggpubr::ggpar(ind.FHIT3,
              title = "Principal Component Analysis",
              subtitle = "FHIT3 data set",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Type", legend.position = "top",
              ggtheme = theme_bw()  #, palette = "jco"
)

#ALTERNATIVES
fviz_pca_biplot(res.pca.FHIT3, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = data_FHIT3$Type,
                col.ind = "black",
                # Color variable by groups
                # col.var = factor(c("sepal", "sepal", "petal", "petal")), #THINK ABOUT THIS
                
                legend.title = list(fill = "Type", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors

fviz_pca_biplot(res.pca.FHIT3, 
                # Individuals
                geom.ind = "point",
                fill.ind = data_FHIT3$Type, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)






ind.posture<- fviz_pca_ind(res.pca.posture,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = data_posture2$Type, # color by groups
             palette = unique(data_FHIT3$color),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

ind.posture

ggpubr::ggpar(ind.posture,
              title = "Principal Component Analysis",
              subtitle = "Posture data set",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Type", legend.position = "top",
              ggtheme = theme_bw()  #, palette = "jco"
)

#ALTERNATIVEs
fviz_pca_biplot(res.pca.posture, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = data_posture2$Type,
                col.ind = "black",
                # Color variable by groups
                # col.var = factor(c("sepal", "sepal", "petal", "petal")), #THINK ABOUT THIS
                
                legend.title = list(fill = "Type", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors


fviz_pca_biplot(res.pca.posture, 
                # Individuals
                geom.ind = "point",
                fill.ind = data_posture2$Type, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)





ind.2task <- fviz_pca_ind(res.pca.2task,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = data_dual_task2$Type, # color by groups
             palette = unique(data_FHIT3$color),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

ind.2task


ggpubr::ggpar(ind.2task,
              title = "Principal Component Analysis",
              subtitle = "2 tasks data set",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Type", legend.position = "top",
              ggtheme = theme_bw()  #, palette = "jco"
)


#ALTERNATIVE
fviz_pca_biplot(res.pca.2task, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = data_dual_task2$Type,
                col.ind = "black",
                # Color variable by groups
                # col.var = factor(c("sepal", "sepal", "petal", "petal")), #THINK ABOUT THIS
                
                legend.title = list(fill = "Type", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors


fviz_pca_biplot(res.pca.2task, 
                # Individuals
                geom.ind = "point",
                fill.ind = data_dual_task2$Type, col.ind = "black",
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
# TSNE OF PCA #
###############

ind_pca_FHIT3 <- get_pca_ind(res.pca.FHIT3)

coord_FHIT3 = ind_pca_FHIT3$coord
coord_FHIT3 = as.data.frame(cbind(as.matrix(coord_FHIT3), data_FHIT3$color, data_FHIT3$Type))
coord_FHIT3_unique <- unique(coord_FHIT3)
colnames(coord_FHIT3_unique)<- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5", "color","Type" )

tsne_out_coord_FHIT3 <- Rtsne(coord_FHIT3_unique[,c(1:5)], 
                             dims = 2,
                             pca = TRUE,
                             perplexity = 10)

toplot_coord_FHIT3 = as.data.frame(tsne_out_data_FHIT3$Y)
toplot_coord_FHIT3$color = coord_FHIT3_unique$color
toplot_coord_FHIT3$Type = coord_FHIT3_unique$Type
colnames(toplot_coord_FHIT3) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_coord_FHIT3, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))




ind_pca_posture <- get_pca_ind(res.pca.posture)

coord_posture = ind_pca_posture$coord
coord_posture = as.data.frame(cbind(as.matrix(coord_posture), data_posture2$color, data_posture2$Type))
coord_posture_unique <- unique(coord_posture)
colnames(coord_posture_unique)<- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5", "color","Type" )

tsne_out_coord_posture <- Rtsne(coord_posture_unique[,c(1:5)], 
                              dims = 2,
                              pca = TRUE,
                              perplexity = 10)

toplot_coord_posture = as.data.frame(tsne_out_coord_posture$Y)
toplot_coord_posture$color = coord_posture_unique$color
toplot_coord_posture$Type = coord_posture_unique$Type
colnames(toplot_coord_posture) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_coord_posture, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))



ind_pca_2task <- get_pca_ind(res.pca.2task)

coord_2task = ind_pca_2task$coord
coord_2task = as.data.frame(cbind(as.matrix(coord_2task), data_dual_task2$color, data_dual_task2$Type))
coord_2task_unique <- unique(coord_2task)
colnames(coord_2task_unique)<- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5", "color","Type" )

tsne_out_coord_2task <- Rtsne(coord_2task_unique[,c(1:5)], 
                                dims = 2,
                                pca = TRUE,
                                perplexity = 10)

toplot_coord_2task = as.data.frame(tsne_out_coord_2task$Y)
toplot_coord_2task$color = coord_2task_unique$color
toplot_coord_2task$Type = coord_2task_unique$Type
colnames(toplot_coord_2task) = c("Y1", "Y2", "color", "Type")

ggplot(toplot_coord_2task, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))


##########
#  KPCA  # - the R package works better
##########

# pd <- import("pandas")
# np <- import("numpy")
# sk_dec<-  import("sklearn.decomposition")
# 
# gamma_vec = c(0.01, 0.5, 1, 5, 10)
# 
# #gamma_vec = c(0.0001, 0.001, 0.01)
# 
# 
# alfa_vec = c(0.01, 0.1, 1, 10, 100)
# 
# #alfa_vec = c(100, 1000, 5000)
# 
# 
# 
# kpca_function<- function(df, kernel, hyper1, hyper2, ncomp){
#   
#   df_mat<- as.matrix(df)
#   kpca_machine<- sk_dec$KernelPCA( kernel= kernel,
#                                    fit_inverse_transform= "True",  
#                                    gamma= hyper1, 
#                                    n_components = np$int(ncomp),
#                                    alpha = hyper2)
#   
#   
#   X_kpca<- kpca_machine$fit_transform( df_mat )
#   X_back<- kpca_machine$inverse_transform(X_kpca)
#   
#   X_list<- list(as.matrix(X_kpca), as.matrix(X_back))
#   names(X_list)<- c("X_kpca", "X_back")
#   
#   return(X_list)
#   
# }
# 
# 
# 
# 
# 
# 
# X_kp_bck_FHIT =  lapply(1:length(alfa_vec), function (h) 
#                    lapply(1:length(gamma_vec), function(i)
#                         kpca_function(df = as.matrix(data_FHIT3.scale), #  data_FHIT3[,c(5:14)]
#                                       kernel = "rbf", 
#                                       hyper1 = gamma_vec[i],
#                                       hyper2 = alfa_vec[h], 
#                                       ncomp = 5  ) ))
# 
# X_kp_bck_posture =  lapply(1:length(alfa_vec), function (h) 
#                    lapply(1:length(gamma_vec), function(i)
#                          kpca_function(df = as.matrix(data_posture2.scale), # data_posture2[,c(5:12)]
#                                        kernel = "rbf", 
#                                        hyper1 = gamma_vec[i],
#                                        hyper2 = alfa_vec[h], 
#                                        ncomp = 5  ) ))
# 
# 
# X_kp_bck_2task =  lapply(1:length(alfa_vec), function (h) 
#                    lapply(1:length(gamma_vec), function(i)
#                             kpca_function(df = as.matrix(data_dual_task.scale), # data_dual_task2[,c(5:21)]
#                                           kernel = "rbf", 
#                                           hyper1 = gamma_vec[i],
#                                           hyper2 = alfa_vec[h], 
#                                           ncomp = 5  ) ))
# 

#######################################
# MSE distances - Euclidean distances #   
#######################################


# dist_kpca_FHIT3<- lapply(1:length(alfa_vec), function(h)
#                     lapply(1:length(gamma_vec), function(i) 
#                         sapply(1:nrow(X_kp_bck_FHIT[[h]][[i]]$X_back), function(k) 
#                            dist( rbind(X_kp_bck_FHIT[[h]][[i]]$X_back[k,],
#                                        as.matrix(data_FHIT3.scale) ) ) )))
#  
# dist_kpca_posture<- lapply(1:length(alfa_vec), function(h)
#                     lapply(1:length(gamma_vec), function(i) 
#                        sapply(1:nrow(X_kp_bck_posture[[h]][[i]]$X_back), function(k) 
#                          dist( rbind(X_kp_bck_posture[[h]][[i]]$X_back[k,],
#                                as.matrix(data_posture2.scale) ) ) )))
# 
# dist_kpca_2task<- lapply(1:length(alfa_vec), function(h)
#                       lapply(1:length(gamma_vec), function(i) 
#                         sapply(1:nrow(X_kp_bck_2task[[h]][[i]]$X_back), function(k) 
#                                  dist( rbind(X_kp_bck_2task[[h]][[i]]$X_back[k,],
#                                        as.matrix(data_dual_task.scale) ) ) )))
# 


#################
#final distances#
#################

# d_final_FHIT3 =lapply(1:length(alfa_vec), function(h)
#                  lapply(1:length(gamma_vec), function(i) 
#                      sum(dist_kpca_FHIT3[[h]][[i]])/nrow(X_kp_bck_FHIT[[h]][[i]]$X_back)   ))
# 
# l_d_final_FHIT3<- do.call(rbind, d_final_FHIT3)
# 
# rownames(l_d_final_FHIT3)<- alfa_vec
# colnames(l_d_final_FHIT3)<- gamma_vec
# 
# mat_d_final_FHIT3 = matrix(as.numeric(l_d_final_FHIT3),5,5)
# 
# 
# 
# d_final_posture =lapply(1:length(alfa_vec), function(h)
#   lapply(1:length(gamma_vec), function(i) 
#     sum(dist_kpca_posture[[h]][[i]])/nrow(X_kp_bck_posture[[h]][[i]]$X_back)   ))
# 
# l_d_final_posture<- do.call(rbind, d_final_posture)
# 
# rownames(l_d_final_posture)<- alfa_vec
# colnames(l_d_final_posture)<- gamma_vec
# 
# mat_d_final_posture = matrix(as.numeric(l_d_final_posture),5,5)
# 
# 
# 
# d_final_2task =lapply(1:length(alfa_vec), function(h)
#   lapply(1:length(gamma_vec), function(i) 
#     sum(dist_kpca_2task[[h]][[i]])/nrow(X_kp_bck_2task[[h]][[i]]$X_back)   ))
# 
# l_d_final_2task<- do.call(rbind, d_final_2task)
# 
# rownames(l_d_final_2task)<- alfa_vec
# colnames(l_d_final_2task)<- gamma_vec
# 
# mat_d_final_2task = matrix(as.numeric(l_d_final_2task),5,5)
# 

###################################
# Select the optimal kpca indices #
###################################

# optimal_kpca_FHIT3 = X_kp_bck_FHIT[[which(mat_d_final_FHIT3 == min(mat_d_final_FHIT3), arr.ind = TRUE)[2]]][[which(mat_d_final_FHIT3 == min(mat_d_final_FHIT3), arr.ind = TRUE)[1] ]]
# optimal_kpca_posture = X_kp_bck_posture[[which(mat_d_final_posture == min(mat_d_final_posture), arr.ind = TRUE)[2]]][[which(mat_d_final_posture == min(mat_d_final_posture), arr.ind = TRUE)[1] ]]
# optimal_kpca_2task = X_kp_bck_2task[[which(mat_d_final_2task == min(mat_d_final_2task), arr.ind = TRUE)[2]]][[which(mat_d_final_2task == min(mat_d_final_2task), arr.ind = TRUE)[1] ]]
# 
# kpcs_FHIT3 = optimal_kpca_FHIT3$X_kpca
# kpcs_posture = optimal_kpca_posture$X_kpca
# kpcs_2task =optimal_kpca_2task$X_kpca



r_kpca_FHIT3 = kpca(data_FHIT3.scale,
                    kernel = "rbfdot",
                    kpar = list(sigma = 0.01))

pcv(r_kpca_FHIT3)

to_plot_kpcs_r_FRHIT3 = as.data.frame(rotated(r_kpca_FHIT3)[,c(1:5)])
colnames(to_plot_kpcs_r_FRHIT3) = c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5")
to_plot_kpcs_r_FRHIT3$color = data_FHIT3$color
to_plot_kpcs_r_FRHIT3$Type = data_FHIT3$Type


ggplot(to_plot_kpcs_r_FRHIT3, aes(x= Dim.1, y= Dim.2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))




r_kpca_posture = kpca(data_posture2.scale,
                    kernel = "rbfdot",
                    kpar = list(sigma = 0.01))

pcv(r_kpca_posture)

to_plot_kpcs_r_posture = as.data.frame(rotated(r_kpca_posture)[,c(1:5)])
colnames(to_plot_kpcs_r_posture) = c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5")
to_plot_kpcs_r_posture$color = data_posture2$color
to_plot_kpcs_r_posture$Type = data_posture2$Type


ggplot(to_plot_kpcs_r_posture, aes(x= Dim.1, y= Dim.2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))



r_kpca_2task = kpca(data_dual_task.scale,
                      kernel = "rbfdot",
                      kpar = list(sigma = 0.01))

pcv(r_kpca_2task)

to_plot_kpcs_r_2task = as.data.frame(rotated(r_kpca_2task)[,c(1:5)])
colnames(to_plot_kpcs_r_2task) = c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5")
to_plot_kpcs_r_2task$color = data_dual_task2$color
to_plot_kpcs_r_2task$Type = data_dual_task2$Type


ggplot(to_plot_kpcs_r_2task, aes(x= Dim.1, y= Dim.2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))



################
# TSNE OF kPCA #
################
# 
# 
# coord_kpca_FHIT3 = rotated(r_kpca_FHIT3)[,c(1:5)] #kpcs_FHIT3
# coord_kpca_FHIT3 = as.data.frame(cbind(as.matrix(coord_kpca_FHIT3), data_FHIT3$color, data_FHIT3$Type))
# coord_kpca_FHIT3_unique <- unique(coord_kpca_FHIT3)
# colnames(coord_kpca_FHIT3_unique)<- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5", "color","Type" )
# 
# tsne_out_kpca_FHIT3 <- Rtsne(coord_kpca_FHIT3_unique[,c(1:5)], 
#                               dims = 2,
#                               pca = TRUE,
#                               perplexity = 10)
# 
# toplot_kpca_FHIT3 = as.data.frame(tsne_out_kpca_FHIT3$Y)
# toplot_kpca_FHIT3$color = coord_kpca_FHIT3_unique$color
# toplot_kpca_FHIT3$Type = coord_kpca_FHIT3_unique$Type
# colnames(toplot_kpca_FHIT3) = c("Y1", "Y2", "color", "Type")
# 
# ggplot(toplot_kpca_FHIT3, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
#   geom_point(size = 3) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   theme(legend.position="bottom",
#         strip.text.x = element_text(size = 15))
# 
# 
# 
# 
# coord_kpca_posture = rotated(r_kpca_posture)[,c(1:5)]#kpcs_posture
# coord_kpca_posture = as.data.frame(cbind(as.matrix(coord_kpca_posture), data_posture2$color, data_posture2$Type))
# coord_kpca_posture_unique <- unique(coord_kpca_posture)
# colnames(coord_kpca_posture_unique)<- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5", "color","Type" )
# 
# tsne_out_kpca_posture <- Rtsne(unique(coord_kpca_posture_unique[,c(1:5)]), 
#                                 dims = 2,
#                                 pca = TRUE,
#                                 perplexity = 20)
# 
# toplot_kpca_posture = as.data.frame(tsne_out_kpca_posture$Y)
# toplot_kpca_posture$color = coord_kpca_posture_unique$color[-144]
# toplot_kpca_posture$Type = coord_kpca_posture_unique$Type[-144]
# colnames(toplot_kpca_posture) = c("Y1", "Y2", "color", "Type")
# 
# ggplot(toplot_kpca_posture, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
#   geom_point(size = 3) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   theme(legend.position="bottom",
#         strip.text.x = element_text(size = 15))
# 
# 
# 
# 
# coord_kpca_2task = rotated(r_kpca_2task)[,c(1:5)]#kpcs_2task
# coord_kpca_2task = as.data.frame(cbind(as.matrix(coord_kpca_2task), data_dual_task2$color, data_dual_task2$Type))
# coord_kpca_2task_unique <- unique(coord_kpca_2task)
# colnames(coord_kpca_2task_unique)<- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5", "color","Type" )
# 
# tsne_out_kpca_2task <- Rtsne(coord_kpca_2task_unique[,c(1:5)], 
#                              dims = 2,
#                              pca = TRUE,
#                              perplexity = 10)
# 
# toplot_kpca_2task = as.data.frame(tsne_out_kpca_2task$Y)
# toplot_kpca_2task$color = coord_kpca_2task_unique$color
# toplot_kpca_2task$Type = coord_kpca_2task_unique$Type
# colnames(toplot_kpca_2task) = c("Y1", "Y2", "color", "Type")
# 
# ggplot(toplot_kpca_2task, aes(x= Y1, y= Y2, color = Type, shape = Type )) +
#   geom_point(size = 3) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank()) +
#   theme(legend.position="bottom",
#         strip.text.x = element_text(size = 15))
# 
# 
# 

###################
#  RANDOM FOREST  #
###################


rf_FHIT3 <-randomForest(as.factor(Type)~.,data=data_FHIT3[c(3,5:14)],
                  ntree=500,
                  importance = T,
                  proximity = T) 



print(rf_FHIT3)

#Evaluate variable importance
round(importance(rf_FHIT3), 2)
varImpPlot(rf_FHIT3)


i_scores <- importance(rf_FHIT3)
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








data_posture3 = data_posture2

rf_posture <-randomForest(as.factor(Type)~.,data=data_posture3[c(3,5:12)],
                          ntree=500,
                          importance = T,
                          proximity = T) 



print(rf_posture)

#Evaluate variable importance
round(importance(rf_posture), 2)
varImpPlot(rf_posture)

i_scores <- importance(rf_posture)
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









rf_2task <-randomForest(as.factor(Type)~.,data=data_dual_task2[c(3,5:21)],
                        ntree=500,
                        importance = T,
                        proximity = T) 



print(rf_2task)

#Evaluate variable importance
round(importance(rf_2task), 2)
varImpPlot(rf_2task)





################
################
#####  SVM  ####
################
################

set.seed(314)    

data_FHIT3_svm = data_FHIT3[c(3,5:14)]

n_FHIT3 <- nrow(data_FHIT3)  
ntrain_FHIT3 <- round(n_FHIT3*0.75) 

tindex_FHIT3 <- sample(n_FHIT3, ntrain_FHIT3)   
train_FHIT3 <- data_FHIT3_svm[tindex_FHIT3,]   
test_FHIT3 <- data_FHIT3_svm[-tindex_FHIT3,] 
svm_FHIT3 <- svm(as.factor(Type)~., 
                 data=train_FHIT3, 
                 method="C-classification",
                 kernel="radial", 
                 gamma=0.01, 
                 cost=100)


summary(svm_FHIT3)

plot(svm_FHIT3, 
     train_FHIT3,
     Left_Lateral_rot_frame ~ Right_Lateral_rot_frame,
     slice=list(Left_Post_rot_frame=16, Right_Post_rot_frame=4))

prediction_FHIT3 <- predict(svm_FHIT3, test_FHIT3)
xtab_FHIT3 <- table(test_FHIT3$Type, prediction_FHIT3)
xtab_FHIT3

sum(diag(xtab_FHIT3)) / nrow(test_FHIT3)


plot(cmdscale(dist(data_FHIT3_svm[,-1])),
     col = as.integer(as.factor(unlist(data_FHIT3_svm[,1]))),
     pch = c("o","+")[1:150 %in% svm_FHIT3$index + 1])


#feature importance





data_posture_svm = data_posture3[c(3,5:12)]

n_posture <- nrow(data_posture3)  
ntrain_posture <- round(n_posture*0.75) 
tindex_posture <- sample(n_posture, ntrain_posture)   
train_posture <- data_posture_svm[tindex_posture,]   
test_posture <- data_posture_svm[-tindex_posture,] 
svm_posture <- svm(as.factor(Type)~., 
                 data=train_posture, 
                 method="C-classification",
                 kernel="radial", 
                 gamma=0.1, 
                 cost=10)


summary(svm_posture)

plot(svm_posture, 
     train_posture,
     V5 ~ V6,
     slice=list(V1=3, V2=4))

prediction_posture <- predict(svm_posture, test_posture)
xtab_posture <- table(test_posture$Type, prediction_posture)
xtab_posture

sum(diag(xtab_posture)) / nrow(test_posture)


plot(cmdscale(dist(data_posture_svm[,-1])),
     col = as.integer(as.factor(unlist(data_posture_svm[,1]))),
     pch = c("o","+")[1:150 %in% svm_posture$index + 1])


#feature importance





data_2task_svm = data_dual_task2[c(3,5:21)]

n_2task <- nrow(data_2task_svm)  
ntrain_2task <- round(n_posture*0.75) 
tindex_2task <- sample(n_posture, ntrain_2task)   
train_2task <- data_posture_svm[tindex_2task,]   
test_2task <- data_posture_svm[-tindex_2task,] 
svm_2task <- svm(as.factor(Type)~., 
                   data=train_2task, 
                   method="C-classification",
                   kernel="radial", 
                   gamma=0.001, 
                   cost=10)


summary(svm_2task)

plot(svm_2task, 
     train_2task,
     V5 ~ V6,
     slice=list(V1=3, V2=4))

prediction_2task <- predict(svm_2task, test_2task)
xtab_2task <- table(test_2task$Type, prediction_2task)
xtab_2task

sum(diag(xtab_2task)) / nrow(test_2task)


plot(cmdscale(dist(data_2task_svm[,-1])),
     col = as.integer(as.factor(unlist(data_2task_svm[,1]))),
     pch = c("o","+")[1:150 %in% svm_2task$index + 1])


#feature importance


