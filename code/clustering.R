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
library(gridExtra)
library(kableExtra)

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

figs_dir = "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\figs\\"

mydir<- "C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\data\\"


new_data_parm = readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\new_data_param_500.rds")
new_data_nonparm = readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\new_data_nonparam.rds")


new_data_param_unscreen = readRDS("C:\\Users\\mcampi\\Desktop\\Simo_MariaPia\\code\\Rfiles\\new_unscreen_param_beta.rds")
new_data_parm = new_data_param_unscreen

features = c('mean', 'var', 'distr', 'cop')

combo_category1= t(combn(features, 1))
combo_category2 = t(combn(features, 2))
combo_category3 = t(combn(features, 3))
combo_category4 = t(combn(features, 4))

combo_category = list(combo_category1,combo_category2,
                      combo_category3, combo_category4)

datacluster_param = vector("list", length(combo_category))

# Loop through each entry in the combo_category list
for (i in 1:length(combo_category)) {
  combination_matrix <- combo_category[[i]]
  
  # Loop through each row of the matrix (each combination)
  for (j in 1:nrow(combination_matrix)) {
    combination <- as.character(combination_matrix[j, ])
    
    # Select columns that contain the specified categories in the combination
    datacluster_param[[i]][[j]] <- new_data_parm[, grep(paste(combination, collapse = "|"), names(new_data_parm))]
    
  }
}


for (i in 1:length(datacluster_param)) { for (j in 1:length(datacluster_param[[i]])) {  
  datacluster_param[[i]][[j]]$Type = new_data_parm$Type  }}

for (i in 1:length(datacluster_param)) { for (j in 1:length(datacluster_param[[i]])) {       
  
  datacluster_param[[i]][[j]] = datacluster_param[[i]][[j]] %>% 
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
  
}}


datacluster_nonparam = vector("list", length(combo_category))


# Loop through each entry in the combo_category list
for (i in 1:length(combo_category)) {
  combination_matrix <- combo_category[[i]]
  
  # Loop through each row of the matrix (each combination)
  for (j in 1:nrow(combination_matrix)) {
    combination <- as.character(combination_matrix[j, ])
    
    # Select columns that contain the specified categories in the combination
    datacluster_nonparam[[i]][[j]] <- new_data_nonparm[, grep(paste(combination, collapse = "|"), names(new_data_parm))]
    
  }
}


for (i in 1:length(datacluster_nonparam)) { for (j in 1:length(datacluster_nonparam[[i]])) {  
  datacluster_nonparam[[i]][[j]]$Type = new_data_nonparm$Type  }}

for (i in 1:length(datacluster_nonparam)) { for (j in 1:length(datacluster_nonparam[[i]])) {       
  
  datacluster_nonparam[[i]][[j]] = datacluster_nonparam[[i]][[j]] %>% 
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
  
}}

################
################
## CLUSTERING ##
################
################


datacluster_param_scaled <- lapply(1:length(datacluster_param), function(i)
  lapply(1:length(datacluster_param[[i]]), function(j)
    datacluster_param[[i]][[j]] %>%
      mutate_at(c(1:(ncol(datacluster_param[[i]][[j]])
                     -2)), 
                funs(c(scale(.)))) ))


datacluster_nonparam_scaled <- lapply(1:length(datacluster_nonparam), function(i)
  lapply(1:length(datacluster_nonparam[[i]]), function(j)
    datacluster_nonparam[[i]][[j]] %>%
      mutate_at(c(1:(ncol(datacluster_nonparam[[i]][[j]])
                     -2)), 
                funs(c(scale(.)))) ))

res.dist.p <- get_dist(as.data.frame(datacluster_param_scaled[[4]])[,c(1:12)], 
                       stand = TRUE, method = "pearson") 
res.dist.np <- get_dist(as.data.frame(datacluster_nonparam_scaled[[4]])[,c(1:12)],
                        stand = TRUE, method = "pearson") 

fviz_dist(res.dist.p, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

fviz_dist(res.dist.np, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))



res.dist.p <- get_dist(as.data.frame(datacluster_param_scaled[[3]][[1]])[,c(1:7)], 
                       stand = TRUE, method = "pearson") 
res.dist.np <- get_dist(as.data.frame(datacluster_nonparam_scaled[[3]][[1]])[,c(1:7)],
                        stand = TRUE, method = "pearson") 

fviz_dist(res.dist.p, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

fviz_dist(res.dist.np, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#############
## K MEANS ##
#############

set.seed(123)

datakmeans_param = lapply(1:length(datacluster_param), function(i)
  lapply(1:length(datacluster_param[[i]]), function(j) 
    as.data.frame(datacluster_param_scaled[[i]][[j]][, c(1:(ncol(datacluster_param_scaled[[i]][[j]])-2) )]) ))

datakmeans_nonparam = lapply(1:length(datacluster_nonparam), function(i)
  lapply(1:length(datacluster_nonparam[[i]]), function(j) 
    as.data.frame(datacluster_nonparam_scaled[[i]][[j]][, c(1:(ncol(datacluster_nonparam_scaled[[i]][[j]])-2) )]) ))


for (i in 1:length(datacluster_param)) {
  for (j in 1:length(datacluster_param[[i]])) {
    
    rownames(datakmeans_param[[i]][[j]]) <- sapply(1:nrow(datakmeans_param[[i]][[j]]), 
                                                   function(h) paste(datacluster_param_scaled[[i]][[j]]$Type2[h], h, sep = "_")) 
  }
}



for (i in 1:length(datacluster_nonparam)) {
  for (j in 1:length(datacluster_nonparam[[i]])) {
    
    rownames(datakmeans_nonparam[[i]][[j]]) <- sapply(1:nrow(datakmeans_nonparam[[i]][[j]]), 
                                                      function(h) paste(datacluster_nonparam_scaled[[i]][[j]]$Type2[h], h, sep = "_")) 
  }
}



km.res.param <- lapply(1:length(datacluster_param), function(i)
  lapply(1:length(datacluster_param[[i]]), function(j) 
    kmeans(datakmeans_param[[i]][[j]], 4, nstart = 25) ))


fviz_cluster(km.res.param[[1]][[1]],
             data = datakmeans_param[[1]][[1]],
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

fviz_cluster(km.res.param[[1]][[1]],
             data = datakmeans_param[[1]][[1]], 
             ellipse.type = "convex",
             labelsize = 0) + 
  scale_color_manual(values = c("pink","orange","green","yellow" )) +
  custom_theme +  
   labs(title = "") #+
  # scale_shape_manual(values = c(3,17,19,22) ,labels = c("ASD", "Control", "ADHD", "DYS"))

custom_theme <- theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the combinations of km.res.param and datakmeans_param
for (i in 1:length(km.res.param)) {
  for (j in 1:length(km.res.param[[i]])) {
    # Create a ggplot object for each combination
    p <- fviz_cluster(
      km.res.param[[i]][[j]],
      data = datakmeans_param[[i]][[j]],
      ellipse.type = "convex",
      # palette = "jco",
      ggtheme = theme_bw(), 
      labelsize = 8) +
      custom_theme +  
      labs(title = "") + 
      scale_color_manual(values = c("pink","orange","green","yellow" )) 
    
    # Store the ggplot object in the list
    plot_list[[length(plot_list) + 1]] <- p
  }
}


grid.arrange(grobs = plot_list, ncol = 4)


# # Initialize an empty list to store ggplot objects
# plot_list <- list()
# 
# # Define a color mapping for Type2 values
# color_mapping <- c("Control" = "orange", "DYS" = "yellow", "ADHD" = "green", "ASD" = "pink")
# 
# # Loop through the combinations of km.res.param and datakmeans_param
# for (i in 1:length(km.res.param)) {
#   for (j in 1:length(km.res.param[[i]])) {
#     # Create a ggplot object for each combination
#     p <- fviz_cluster(
#       km.res.param[[i]][[j]],
#       data = datakmeans_param[[i]][[j]],
#       ellipse.type = "convex",
#       ggtheme = theme_bw(),
#       labelsize = 8,
#       aes(fill = Type2)
#     ) +
#       scale_fill_manual(values = color_mapping) +
#       custom_theme +
#       labs(title = "") +
#       scale_shape_manual(labels = c("ASD", "Control", "ADHD", "DYS"))
#     
#     # Store the ggplot object in the list
#     plot_list[[length(plot_list) + 1]] <- p
#   }
# }


km.res.nonparam <- lapply(1:length(datacluster_nonparam), function(i)
  lapply(1:length(datacluster_nonparam[[i]]), function(j) 
    kmeans(datakmeans_nonparam[[i]][[j]], 4, nstart = 25) ))


fviz_cluster(km.res.nonparam[[1]][[1]],
             data = datakmeans_nonparam[[1]][[1]],
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_bw(), 
             labelsize = 8)

fviz_cluster(km.res.nonparam[[1]][[1]],
             data = datakmeans_nonparam[[1]][[1]], 
             ellipse.type = "convex",
             labelsize = 0) + 
  scale_color_manual(values = c("pink","orange","green","yellow" )) +
  custom_theme +  
  labs(title = "") #+
  #scale_shape_manual(values = c(3,17,19,22) ,labels = c("ASD", "Control", "ADHD", "DYS"))


# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the combinations of km.res.param and datakmeans_param
for (i in 1:length(km.res.nonparam)) {
  for (j in 1:length(km.res.nonparam[[i]])) {
    # Create a ggplot object for each combination
    p <- fviz_cluster(
      km.res.nonparam[[i]][[j]],
      data = datakmeans_nonparam[[i]][[j]],
      ellipse.type = "convex",
      # palette = "jco",
      ggtheme = theme_bw(), 
      labelsize = 8) +
      custom_theme +  
      labs(title = "") + 
      scale_color_manual(values = c("pink","orange","green","yellow" ))
    
    # Store the ggplot object in the list
    plot_list[[length(plot_list) + 1]] <- p
  }
}


grid.arrange(grobs = plot_list, ncol = 4)


#####################
# SILHOUETTE SCORES #
#####################

cluster_colors = c('pink', 'yellow', 'orange','green')


sil_kmeans_param <- lapply(1:length(datacluster_param), function(i)
  lapply(1:length(datacluster_param[[i]]), function(j)
    silhouette(km.res.param[[i]][[j]]$cluster, 
               dist(datakmeans_param[[i]][[j]])) ))

for (i in 1:length(datacluster_param)) {
  for (j in 1:length(datacluster_param[[i]])) {
    
    rownames(sil_kmeans_param[[i]][[j]]) <- sapply(1:nrow(datacluster_param[[i]][[j]]), 
                                                   function(h) paste(datacluster_param[[i]][[j]]$Type2[h], h, sep = "_")) 
  }
}




sil_kmeans_nonparam <- lapply(1:length(datacluster_nonparam), function(i)
  lapply(1:length(datacluster_nonparam[[i]]), function(j)
    silhouette(km.res.nonparam[[i]][[j]]$cluster, 
               dist(datakmeans_nonparam[[i]][[j]])) ))

for (i in 1:length(datacluster_nonparam)) {
  for (j in 1:length(datacluster_nonparam[[i]])) {
    
    rownames(sil_kmeans_nonparam[[i]][[j]]) <- sapply(1:nrow(datacluster_nonparam[[i]][[j]]), 
                                                      function(h) paste(datacluster_nonparam[[i]][[j]]$Type2[h], h, sep = "_")) 
  }
}


pdf( paste(figs_dir, "sil_kmeans_param.pdf", sep = ""), width = 12, height = 9 )  
fviz_silhouette(sil_kmeans_param[[1]][[1]], palette = cluster_colors) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        axis.text=element_text(size=5, angle = 90))
dev.off()


pdf( paste(figs_dir, "sil_kmeans_nonparam.pdf", sep = ""), width = 12, height = 9 )  
fviz_silhouette(sil_kmeans_nonparam[[1]][[1]], palette = cluster_colors) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        axis.text=element_text(size=5, angle = 90))
dev.off()



avg_sil_param = lapply(1:length(datacluster_param), function(i)
  lapply(1:length(datacluster_param[[i]]), function(j) 
    mean(sil_kmeans_param[[i]][[j]][,3]) ))

result_overall_param = rbind(do.call(rbind, avg_sil_param[[1]]), do.call(rbind, avg_sil_param[[2]]),
      do.call(rbind, avg_sil_param[[3]]), do.call(rbind, avg_sil_param[[4]]) )

kable(result_overall_param, format = "latex", digit = 3)

do.call(rbind, do.call(c, lapply(1:length(datacluster_param), function(i)
          lapply(1:length(datacluster_param[[i]]), function(j) 
            as.data.frame(fviz_silhouette(sil_kmeans_param[[i]][[j]])$data %>% 
              group_by(cluster) %>% 
                summarise(size = n(), 
                  ave.sil.width=round(mean(sil_width), 2))) )) ) )

#capture.output(fviz_silhouette(sil_kmeans_param[[i]][[j]]))




avg_sil_nonparam = lapply(1:length(datacluster_nonparam), function(i)
  lapply(1:length(datacluster_nonparam[[i]]), function(j) 
    mean(sil_kmeans_nonparam[[i]][[j]][,3]) ))


result_overall_nonparam = rbind(do.call(rbind, avg_sil_nonparam[[1]]), do.call(rbind, avg_sil_nonparam[[2]]),
      do.call(rbind, avg_sil_nonparam[[3]]), do.call(rbind, avg_sil_nonparam[[4]]) )

kable(result_overall_nonparam, format = "latex", digit = 3)


################################
################################
##  HIERARCHICHAL clustering  ## 
################################
################################

param_hier <- datakmeans_param
nonparam_hier <- datakmeans_nonparam


res.hc.param = lapply(1:length(datacluster_param), function(i)
                   lapply(1:length(datacluster_param[[i]]), function(j) 
                       eclust(param_hier[[i]][[j]], "hclust", k = 4) ))

fviz_dend(res.hc.param[[1]][[1]],
          k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("orange", "green", "pink", "gold"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE ) +custom_theme +  
  labs(title = "")



res.hc.nonparam = lapply(1:length(datacluster_param), function(i)
                    lapply(1:length(datacluster_param[[i]]), function(j) 
                       eclust(nonparam_hier[[i]][[j]], "hclust", k = 4) ))

fviz_dend(res.hc.nonparam[[1]][[1]],
          k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("orange", "green", "pink", "gold"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE ) +custom_theme +  
  labs(title = "")



# Initialize an empty list to store ggplot objects
plot_list <- list()

# Loop through the combinations of km.res.param and datakmeans_param
for (i in 1:length(res.hc.param)) {
  for (j in 1:length(res.hc.param[[i]])) {
    # Create a ggplot object for each combination
    p <- fviz_dend(res.hc.param[[i]][[j]], #res.hc.nonparam
                   k = 4, # Cut in four groups
                   cex = 0.5, # label size
                   k_colors = c("orange", "green", "pink", "gold"),
                   color_labels_by_k = TRUE, # color labels by groups
                   rect = TRUE ) +custom_theme +  
      labs(title = "")
    
    # Store the ggplot object in the list
    plot_list[[length(plot_list) + 1]] <- p
  }
}


grid.arrange(grobs = plot_list, ncol = 4)






####################
#SILHOUETTE SCORES##
####################


sil_hier_param <- lapply(1:length(datacluster_param), function(i)
                    lapply(1:length(datacluster_param[[i]]), function(j)
                        silhouette(res.hc.param[[i]][[j]]$cluster, 
                              dist(datakmeans_param[[i]][[j]])) ))

for (i in 1:length(datacluster_param)) {
  for (j in 1:length(datacluster_param[[i]])) {
    
    rownames(sil_hier_param[[i]][[j]]) <- sapply(1:nrow(datacluster_param[[i]][[j]]), 
                                                   function(h) paste(datacluster_param[[i]][[j]]$Type2[h], h, sep = "_")) 
  }
}




sil_hier_nonparam <- lapply(1:length(datacluster_nonparam), function(i)
  lapply(1:length(datacluster_nonparam[[i]]), function(j)
    silhouette(res.hc.nonparam[[i]][[j]]$cluster, 
               dist(datakmeans_nonparam[[i]][[j]])) ))

for (i in 1:length(datacluster_nonparam)) {
  for (j in 1:length(datacluster_nonparam[[i]])) {
    
    rownames(sil_hier_nonparam[[i]][[j]]) <- sapply(1:nrow(datacluster_nonparam[[i]][[j]]), 
                                                      function(h) paste(datacluster_nonparam[[i]][[j]]$Type2[h], h, sep = "_")) 
  }
}




pdf( paste(figs_dir, "sil_hclust_param.pdf", sep = ""), width = 12, height = 9 )  
fviz_silhouette(res.hc.param[[1]][[1]], palette = c("green", "yellow","pink","orange" ) ) + theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        axis.text=element_text(size=5, angle = 90)) #+ 
dev.off() 
# scale_color_manual(values = c("pink","orange","green","yellow" ),
#                    labels = c("ASD", "Control", "ADHD", "DYS")) 


pdf( paste(figs_dir, "sil_hclust_nonparam.pdf", sep = ""), width = 12, height = 9 )  
fviz_silhouette(res.hc.nonparam[[1]][[1]], palette = c("green", "yellow","pink","orange" ) ) + theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right',
        axis.text=element_text(size=5, angle = 90)) #+ 
dev.off() 



avg_sil_param_hier = lapply(1:length(datacluster_param), function(i)
  lapply(1:length(datacluster_param[[i]]), function(j) 
    mean(sil_hier_param[[i]][[j]][,3]) ))

result_overall_param_hier = rbind(do.call(rbind, avg_sil_param_hier[[1]]), do.call(rbind, avg_sil_param_hier[[2]]),
                             do.call(rbind, avg_sil_param_hier[[3]]), do.call(rbind, avg_sil_param_hier[[4]]) )


kable(result_overall_param_hier, format = "latex", digit = 3)



avg_sil_nonparam_hier = lapply(1:length(datacluster_nonparam), function(i)
  lapply(1:length(datacluster_nonparam[[i]]), function(j) 
    mean(sil_hier_nonparam[[i]][[j]][,3]) ))

result_overall_nonparam_hier = rbind(do.call(rbind, avg_sil_nonparam_hier[[1]]), 
                                     do.call(rbind, avg_sil_nonparam_hier[[2]]),
                                  do.call(rbind, avg_sil_nonparam_hier[[3]]), 
                                  do.call(rbind, avg_sil_nonparam_hier[[4]]) )


kable(result_overall_nonparam_hier, format = "latex", digit = 3)



#########
#  PCA  #
#########

corMat_param <- cor(param_scaled[,c(1:12)])
corrplot(corMat_param, order = "hclust")

corMat_nonparam <- cor(nonparam_scaled[,c(1:12)])
corrplot(corMat_nonparam, order = "hclust")



res.pca.param <- PCA(param_hierarhical, scale.unit=TRUE, graph = T)
res.pca.nonparam <- PCA(nonparam_hierarhical, scale.unit=TRUE, graph = T)


#This line of code will sort the variables the most linked to each PC.
dimdesc(res.pca.param)
dimdesc(res.pca.nonparam)


summary(res.pca.param)
summary(res.pca.nonparam)


######################
# Graph of variables #
######################

#Eigenvalues / Variances
fviz_eig(res.pca.param, addlabels = TRUE)
fviz_eig(res.pca.nonparam, addlabels = TRUE)


#Quality of representation
var_param <- get_pca_var(res.pca.param)
var_nonparam <- get_pca_var(res.pca.nonparam)


corrplot(var_param$cos2, is.corr=FALSE)
corrplot(var_nonparam$cos2, is.corr=FALSE)

# The cos2 values are used to estimate the quality of the representation
# The closer a variable is to the circle of correlations, the better its 
# representation on the factor map (and the more important it is to interpret these components)
# Variables that are closed to the center of the plot are less important for the first components


fviz_pca_var(res.pca.param, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE )

fviz_pca_var(res.pca.nonparam, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE )


#Contributions of variables to PCs
fviz_contrib(res.pca.param, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca.nonparam, choice = "var", axes = 2, top = 10)


#######################
# Graph of individuals
#######################

fviz_pca_ind(res.pca.param)
fviz_pca_ind(res.pca.nonparam)



fviz_pca_biplot(res.pca.param, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = param_scaled$Type,
                col.ind = "black",
                # Color variable by groups
                # col.var = factor(c("sepal", "sepal", "petal", "petal")), #THINK ABOUT THIS
                
                legend.title = list(fill="Type", color="Clusters"),
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




fviz_pca_biplot(res.pca.param, 
                # Individuals
                geom.ind = "point",
                fill.ind = param_scaled$Type2, 
                #col.ind = factor(new_data_bl$Type2),
                pointshape = 21, 
                pointsize = 2, 
                palette = c("pink","gold", "green", "orange"),  
                #palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib",
                col.var = "contrib",
                gradient.cols = "RdYlBu",
                legend.title = list(fill = "Children", 
                                    color = "Contrib.",
                                    alpha = "Contrib.")
) + custom_theme2 +  
  labs(title = "") 





fviz_pca_biplot(res.pca.nonparam, 
                # Individuals
                geom.ind = "point",
                fill.ind = param_scaled$Type2, 
                #col.ind = factor(new_data_bl$Type2),
                pointshape = 21, 
                pointsize = 2, 
                palette = c("pink","gold", "green", "orange"),  
                #palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib",
                col.var = "contrib",
                gradient.cols = "RdYlBu",
                legend.title = list(fill = "Children", 
                                    color = "Contrib.",
                                    alpha = "Contrib.")
) + custom_theme2 +  
  labs(title = "") 


