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
library(lasso2)


###############################
# SET DIRECTORY AND READ FILE #
###############################

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
colnames(data_FHIT2)[5:10]<- paste(colnames(data_FHIT2)[5:10], "rot_frame", sep = '_')
colnames(data_FHIT2)[11:16]<- paste(colnames(data_FHIT2)[11:16], "black", sep = '_')
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


bxpl_data_FHIT3_black = melt(data_FHIT3[,-c(1,2,4,5,6,7,8,9,10,17,18)], id.vars = "Type")

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


#  DONT CENTER THE DATA

# each component individually 
#  2 sample test on the mean  adhd vs ctr, dys vs ctr, tsa vs ctr ALL by components
#  variance ratio test  -- same settings as abouve
#  kolmgorov test-- same settings as above
#  group test - test jointly as adhd vs ctr and same - multi-null tests
#  manova tests - sequence of tests

# contrast questions - pair the diseases and do the tests

# features - cluster - for which clustering between them 

# add spectral clustering to the methods

# construct cdf, sudo data and then empirical copula of the group 

# copula 2 sample testing -- test on sunday 



#########
# RTSNE #
#########

set.seed(42)

#by experiments

data_FHIT3_rotfram <- data_FHIT3[,-c(11:16)]
data_FHIT3_black <- data_FHIT3[,-c(5:10)]

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
plot3d(x = tsne3d[,1], 
       y= tsne3d[,2], 
       z= tsne3d[,3], 
       type = "s", 
       radius = 0.5,
       xlab="", ylab="", zlab="", col = data_FHIT3_rotfram_unique$color )
#spheres3d(x = m[,1], y= m[,2], z= m[,3], r = 0.2, color = "red", radius = 2)
#legend3d("topright", legend = c("Malign", "Benign",), pch = 16, col = c("red", "black"), cex=0.8)


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
plot3d(x = tsne3d[,1], 
       y= tsne3d[,2], 
       z= tsne3d[,3], 
       type = "s", 
       radius = 1,
       xlab="", ylab="", zlab="", col = data_FHIT3_rotfram_unique$color )



################
################
## CLUSTERING ##
################
################

###################
## DISSIMILARITY ##
###################

data_FHIT3_rotfram_scaled <- data_FHIT3_rotfram  %>%  mutate_at(c(5:10), funs(c(scale(.))))
data_FHIT3_black_scaled <- data_FHIT3_black %>% mutate_at(c(5:10), funs(c(scale(.))))


res.dist.rotfram <- get_dist(data_FHIT3_rotfram_scaled[,c(5:10)], stand = TRUE, method = "pearson")
res.dist.black <- get_dist(data_FHIT3_black_scaled[,c(5:10)], stand = TRUE, method = "pearson")

fviz_dist(res.dist.rotfram, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

fviz_dist(res.dist.black, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))




#############
## K MEANS ##
#############

fviz_nbclust(data_FHIT3_rotfram_scaled[,c(5:10)], kmeans, method = "gap_stat")
fviz_nbclust(data_FHIT3_black_scaled[,c(5:10)], kmeans, method = "gap_stat")


set.seed(123)

data_rto_fram_kmeans = data_FHIT3_rotfram_scaled[,c(5:10)]
data_rto_fram_kmeans = as.data.frame(data_rto_fram_kmeans)
rownames(data_rto_fram_kmeans) <- sapply(1:nrow(data_FHIT3_rotfram_scaled), 
                                         function(i) paste(data_FHIT3_rotfram_scaled$Type[i], i, sep = "_"))

data_black_kmeans = data_FHIT3_black_scaled[,c(5:10)]
data_black_kmeans = as.data.frame(data_black_kmeans)
rownames(data_black_kmeans) <- sapply(1:nrow(data_FHIT3_black_scaled), 
                                         function(i) paste(data_FHIT3_black_scaled$Type[i], i, sep = "_"))


km.res.rotfram <- kmeans(data_rto_fram_kmeans, 4, nstart = 25)
km.res.black <- kmeans(data_black_kmeans, 4, nstart = 25)


fviz_cluster(km.res.rotfram,
             data = data_rto_fram_kmeans,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(), labelsize = 7)


fviz_cluster(km.res.black,
             data = data_black_kmeans,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(), labelsize = 7)




################################
##  k-medoids/pam clustering  ##
################################



pam.res.rotatfram <- pam(data_rto_fram_kmeans, 4)
pam.res.black <- pam(data_black_kmeans, 4)

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

data_rto_fram_hier <- as.data.frame(data_FHIT3_rotfram[,c(5:10)])
rownames(data_rto_fram_hier) <- sapply(1:nrow(data_FHIT3_rotfram), 
                                         function(i) paste(data_FHIT3_rotfram$Type[i], i, sep = "_"))

data_black_hier <- as.data.frame(data_FHIT3_black[,c(5:10)])
rownames(data_black_hier) <- sapply(1:nrow(data_FHIT3_black), 
                                       function(i) paste(data_FHIT3_black$Type[i], i, sep = "_"))


res.hc.rotfram <- data_rto_fram_hier  %>%  
                  scale %>%
                  dist(method = "euclidean") %>% 
                  hclust(method = "ward.D2")     

res.hc.black <- data_black_hier  %>%  
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
##  Clustering validation and evaluation  ##
############################################


gradient.color <- list(low = "steelblue",  high = "white")

data_FHIT3_rotfram[,c(5:10)] %>%   
                    scale()  %>%     # Scale variables
                    get_clust_tendency(n = 50, gradient = gradient.color)

data_FHIT3_black[,c(5:10)] %>%   
                    scale()  %>%     # Scale variables
                    get_clust_tendency(n = 50, gradient = gradient.color)


################################################
## Determining the optimal number of clusters ##
################################################


res.nbclust.rotframe <- data_FHIT3_rotfram[,c(5:10)] %>%
                        scale() %>%
                        NbClust(distance = "euclidean",
                        min.nc = 2, max.nc = 10, 
                        method = "complete", index ="all") 


res.nbclust.black <- data_FHIT3_black[,c(5:10)] %>%
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


corMat_FHIT3_rotfram <- cor(data_FHIT3_rotfram_scaled[,c(5:10)])
corrplot(corMat_FHIT3_rotfram, order = "hclust")

corMat_FHIT3_black <- cor(data_FHIT3_black_scaled[,c(5:10)])
corrplot(corMat_FHIT3_black, order = "hclust")


res.pca.FHIT3.rotframe <- PCA(data_rto_fram_hier, scale.unit=TRUE, graph = T)
res.pca.FHIT3.black <- PCA(data_black_hier, scale.unit=TRUE, graph = T)


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
                fill.ind = data_FHIT3_rotfram$Type,
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
                fill.ind = data_FHIT3_rotfram$Type, col.ind = "black",
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
                fill.ind = data_FHIT3_rotfram$Type,
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
                fill.ind = data_FHIT3_rotfram$Type, col.ind = "black",
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


kpca_rotfram = kpca(as.matrix(data_rto_fram_kmeans),
                    kernel = "rbfdot",
                    kpar = list(sigma = 0.01))

pcv(kpca_rotfram)

to_plot_kpcs_rotfram = as.data.frame(rotated(kpca_rotfram)[,c(1:5)])
colnames(to_plot_kpcs_rotfram) = c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5")
to_plot_kpcs_rotfram$color = data_FHIT3_rotfram$color
to_plot_kpcs_rotfram$Type = data_FHIT3_rotfram$Type


ggplot(to_plot_kpcs_rotfram, aes(x= Dim.1, y= Dim.2, color = Type, shape = Type )) +
  geom_point(size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="bottom",
        strip.text.x = element_text(size = 15))


kpca_black = kpca(as.matrix(data_black_kmeans),
                  kernel = "rbfdot",
                  kpar = list(sigma = 0.01))

pcv(kpca_black)

to_plot_kpcs_black = as.data.frame(rotated(kpca_black)[,c(1:5)])
colnames(to_plot_kpcs_black) = c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5")
to_plot_kpcs_black$color = data_FHIT3_black$color
to_plot_kpcs_black$Type = data_FHIT3_black$Type


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







