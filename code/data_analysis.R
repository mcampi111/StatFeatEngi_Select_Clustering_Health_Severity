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
library(twosamples)
library(HDtest)


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


saveRDS(data_FHIT3, file= "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\data_FHIT3")

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

