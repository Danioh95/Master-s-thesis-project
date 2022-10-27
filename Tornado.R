library(tidyverse)

# load the file containing data into the "Environment", i.e. the Workspace
tornado_df %>%   
as_tibble() %>%  # have a look at its content
  sample_frac(1) # to make it easy to select random rows from a table

# delete NA values
tornado_df_nao <- na.omit(tornado_df)

# make the dataframe "unsupervised" by extracting a subset that does not contain the class indication
unsup_tornado_df <- tornado_df_nao %>%
  select(-class)

# Have a look at the unsupervised dataframe to check the need for preprocessing
unsup_tornado_df

# Scale variable, as a result of scale analysis
scaled_unsup_tornado_df <- unsup_tornado_df %>%
  scale()

# Notice that the scale() function returns a matrix with attributes:
scaled_unsup_tornado_df <- scaled_unsup_tornado_df %>%
  as_tibble()

# Attributes can be viewed by the attributes() function, and possibly stored in a variable
attribs <- attributes(scaled_unsup_tornado_df)

means <- attribs$`scaled:center`
stdevs <- attribs$`scaled:scale`

# Check if observations are clusterable 
library(clustertend)
# hopkins(unsup_tornado_df, n = 0.01 * nrow(unsup_tornado_df))$H # First we use the unscaled df
hopkins(scaled_unsup_tornado_df, n = 0.3 * nrow(scaled_unsup_tornado_df))$H # Then we use the unscaled df for comparison

# Observations are clusterable

# K-MEANS
# To choose the optimal number of clusters to run the kmeans with, we know that we can use the elbow curve. 
# We need, therefore, create a vector containing the Total WithinSS for all values of k we're interested in, say 1 to 10. 
# The values will be stored in the tot_withinss variable.
# Note: kmeans will work on "non-dataframe" objects just like 'scaled_unsup_tornado_df', which is a matrix. 
# Indeed, if you type "?kmeans" in the console, you will see that the default type of input object is a "numeric *matrix* of data".

k <- 1:10 # number of clusters to compare
tot_withinss <- map_dbl(k, function(x) {
  set.seed(123)
  km <- kmeans(scaled_unsup_tornado_df, centers = x, nstart = 1)
  km$tot.withinss
})

# The "map_dbl" belongs to the "map" set of functions included in the "purrr" library (one of the libraries loaded when you load the "tidyverse" library). 
# It maps (applies) the function in the second argument to each element of the first argument (a vector, in this case) and it is used when the mapped function returns a single numeric value. 

# the way we did, that is by defining the function to be applied within the map_dbl statement itself

# ELBOW CURVE
elbow_df <- tibble(k = k, tot_withinss = tot_withinss)
elbow_df

# There comes the plot: we first store it in a variable and then call (draw) it
elbow_curve <- ggplot(elbow_df, aes(k, tot_withinss)) +
  geom_point(size = 3) +
  geom_line() +
  theme_bw()+
  geom_label(aes(label = round(tot_withinss, 1)),
             nudge_y=1000) +
  scale_x_continuous(breaks = elbow_df$k) +
  labs(x = "k (number of clusters)", y = "Total Within Sum of Squares") +
  ggtitle(label = "Elbow curve")

elbow_curve

# We may also use the silhouette analysis as an additional means to choose the best number of clusters.

# The optimal number of clusters can be chosen by comparing the average silhouette width of all observations at the candidate values of k. To do that, we can create a function that:
# 1) calculates the km model with k = x centroids
# 2) uses the silhouette function to obtain the vector of the silhouette widths
# 3) summarizes the values to obtain the mean silhouette coefficient for that clustering output

library(cluster)

overall_silhouette_width <- function(x) {
  set.seed(123)
  km <- kmeans(scaled_unsup_tornado_df, centers = x, nstart = 1)
  sil <- silhouette(km$cluster, dist(scaled_unsup_tornado_df))
  mean_sil_width <- mean(sil[, "sil_width"])
  return(mean_sil_width)
}

overall_silhouette_width(3)

k <- 2:10
mean_sil_width_df <- map_dbl(k, overall_silhouette_width) %>% # you know how map_dbl works, don't you?
  tibble(k = k, mean_sil_width = .) %>% # create the tibble directly from the output of the previous function
  bind_rows(c(k = 1, mean_sil_width = 0)) %>% # add a row for k = 1 (silhouette() can't be run with k = 1)
  arrange(k) # sort by ascending k

# We are now ready to plot the silhouette curve:
silhouette_analysis <- ggplot(mean_sil_width_df, aes(x = k, y = mean_sil_width)) +
  geom_line() +
  geom_segment(aes(
    x = mean_sil_width_df %>% filter(mean_sil_width == max(mean_sil_width)) %>% .$k,
    y = -Inf,
    xend = mean_sil_width_df %>% filter(mean_sil_width == max(mean_sil_width)) %>% .$k,
    yend = max(mean_sil_width)
  ),
  linetype = "dashed"
  ) +
  geom_label(aes(label = round(mean_sil_width, 3))) +
  scale_x_continuous(
    breaks = mean_sil_width_df$k,
    minor_breaks = NULL
  ) +
  labs(x = "k (number of clusters)", y = "Mean Silhouette Width") +
  ggtitle(label = "Silhouette curve") +
  theme_bw()

silhouette_analysis

# Let's load another useful library to draw plots side by side
library(cowplot)
plot_grid(elbow_curve, silhouette_analysis)

# NB CLUST
library(NbClust)
NbClust(data=scaled_unsup_tornado_df, diss=NULL, distance="euclidean", min.nc=2, max.nc=10,
        method="kmeans", index="all", alphaBeale=0.1)
# NUMERO OTTIMO DI CLUSTER = 3

# K MEANS
k <- 3
set.seed(123)
tornado_km <- kmeans(scaled_unsup_tornado_df, centers = k, nstart = 1)

# Hint: look at the ratio between the betweenss and the totss: what can you tell?
{tornado_km$betweenss / tornado_km$totss * 100} %>%
  round(2) %>%
  paste0("BSS/TSS = ", ., "%")

# We can now update the original tornado_df with the cluster assignment returned by the  kmeans clusters.
tornado_df_km <- scaled_unsup_tornado_df %>%
  mutate(km.cluster = tornado_km$cluster,
         class = tornado_df_nao$class)

# Remember that the kernel_df was a "supervised" dataset? It already contains the "target" class of each observation. We may therefore use one of the supervised methods to verify how good our clustering technique has performed.
# But before we do that, we can create a "confusion matrix", an empirical way to check corrispondence between classes and clusters:

table(tornado_df_km$class, tornado_df_km$km.cluster)

# Aggiungiamo la colonna al df scaled_unsup_tornado_df su cui abbiamo fatto il Kmeans
scaled_unsup_tornado_df_km <- scaled_unsup_tornado_df %>%
  as_tibble() %>%
  mutate(
    class = tornado_df_nao$class,
    km.cluster = tornado_km$cluster
  ) %>%
  arrange(km.cluster)

# Recall that the dataframe has 4 features, 1 true class assignment column, 1 kmeans class assignment column, 1 silhouette width class. With ggplot we can't represent more that two feature on the plot. We will represent some combinations
library(viridis)
ggplot(scaled_unsup_tornado_df_km, aes(slon, elon, color = factor(km.cluster))) +
  geom_point(size = 1) +
  theme_bw()+
  ggtitle(label = "K-Means")

ggplot(scaled_unsup_tornado_df_km, aes(stn, slon, color = factor(km.cluster))) +
  geom_point(size = 1) +
  theme_bw()+
  ggtitle(label = "K-Means")

ggplot(scaled_unsup_tornado_df_km, aes(stn, elon, color = factor(km.cluster))) +
  geom_point(size = 1) +
  theme_bw()+
  ggtitle(label = "K-Means")

ggplot(scaled_unsup_tornado_df_km, aes(stn, elat, color = factor(km.cluster))) +
  geom_point(size = 1) +
  theme_bw()+
  ggtitle(label = "K-Means")

ggplot(scaled_unsup_tornado_df_km, aes(elon, elat, color = factor(km.cluster))) +
  geom_point(size = 1) +
  theme_bw()+
  ggtitle(label = "K-Means")

ggplot(scaled_unsup_tornado_df_km, aes(slon, elat, color = factor(km.cluster))) +
  geom_point(size = 1) +
  theme_bw()+
  ggtitle(label = "K-Means")

library(GGally)
ggpairs(scaled_unsup_tornado_df,
        columns = 1:4,
        diag = list(
          mapping = aes(color = factor(km.cluster), alpha = 0.3)
        ),
        upper = "blank",
        lower = list(
          continous = "points",
          mapping = aes(color = factor(km.cluster))
        )
) +
  theme_bw()


# As an objective measure we can now use Entropy
entropy_df_km <- scaled_unsup_tornado_df_km %>%
  count(km.cluster, class, name = "mij") %>%
  group_by(km.cluster) %>%
  mutate(
    mi = sum(mij),
    pij = mij / mi
  )

entropy_df_km

cluster_entropy_km <- entropy_df_km %>%
  summarise(
    e = -sum(pij * log2(pij))
  )

cluster_entropy_km

E_km <- entropy_df_km %>%
  select(-c(class, pij)) %>% # we don't need those quantities anymore
  ungroup() %>% # needed for the following mutate()
  mutate(
    m = sum(mij) # this line could have been replaced by m = nrow(kernel_df) without ungrouping at line 253 above
  ) %>%
  select(-mij) %>% # we need mij no more
  group_by(km.cluster) %>%
  filter(row_number() == 1) %>%
  left_join(cluster_entropy_km, by = "km.cluster") %>%
  ungroup() %>%
  summarise(E = sum(mi / m * e)) %>%
  as.numeric()

round(E_km, 3)

# PRECISION
precision_df_km <- scaled_unsup_tornado_df_km %>%
  count(km.cluster, class, name = "mij") %>%
  group_by(km.cluster) %>%
  mutate(
    mi = sum(mij),
    pij = mij / mi
  )

precision_df_km

# PURITY
purity_cluster_km <- precision_df_km %>%
  summarise(
    pi = max(pij)
  )
purity_cluster_km

#overall purity
P_km <- precision_df_km %>% # remember that precision_df is grouped by km.cluster
  distinct(km.cluster, mi) %>% 
  left_join(purity_cluster_km, by = "km.cluster") %>%
  ungroup() %>%
  summarise(
    P = sum(mi/sum(mi)*pi)
  ) %>%
  as.numeric()
P_km

# RECALL
recall_df_km <- scaled_unsup_tornado_df_km %>%
  count(class, name = "mj") %>%
  right_join(precision_df_km, by = "class") %>%
  select(km.cluster, class, mij, mj) %>%
  mutate(
    recall = mij/mj
  )
recall_df_km

# F-MEASURE
F_measure_km <- precision_df_km %>%
  select(km.cluster, class, pij) %>%
  left_join(recall_df_km) %>%
  select(-c(mij,mj)) %>%
  mutate(
    F_measure = 2*pij*recall/(pij+recall)
  )
F_measure_km

# Similarity Matrix
set.seed(123)

tornado_distance <- scaled_unsup_tornado_df_km %>%
  select(-c(km.cluster,class)) %>%
  dist() %>%
  as.matrix()

tornado_similarity <- 1 - (tornado_distance - min(tornado_distance))/(max(tornado_distance)-min(tornado_distance))

df <- as.data.frame(tornado_similarity) %>%
  rownames_to_column("row") %>%
  gather(key = "col", value = "similarity", 2:ncol(.)) %>%
  mutate_at(vars(row,col),as.numeric)

ggplot(df, aes(x = col, y = row)) +
  geom_tile(aes(fill = similarity)) +
  scale_fill_gradient2(name = "similarity") +
  theme_bw() +
  labs(x = "Point", y = "Point")

tornado_incidence <- scaled_unsup_tornado_df_km %>%
  select(km.cluster) %>%
  rownames_to_column() %>%
  group_by(km.cluster) %>%
  nest() %>%
  mutate(
    matr = map(data, function(x) matrix(rep.int(1, times = nrow(x)^2), nrow = nrow(x)))
  )

library(Matrix)
?bdiag
tornado_incidence <- as.matrix(bdiag(tornado_incidence$matr))

colnames(tornado_incidence) <- as.character(1:nrow(unsup_tornado_df))
cor(c(tornado_similarity),c(tornado_incidence)) # come creare la incidence matrix

#HIERARCHICAL COMPLETE

unsup_tornado_hclust <- scaled_unsup_tornado_df %>%
  select(stn:elon)

unsup_tornado_hclust

dist_tornado <- dist(unsup_tornado_hclust, method = 'euclidean')
hc_tornado_complete <- hclust(dist_tornado, method = "complete")

k_opt_comp <- NbClust(data = unsup_tornado_hclust, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10,
                      method = "complete", index = "silhouette", alphaBeale = 0.1)
k_opt_comp
# K=2 silhouette=0.7649 molto distante dalle altre

plot(hc_tornado_complete, labels=FALSE)

library(dplyr)
tornado_assignments_comp <- cutree(hc_tornado_complete, k = 2)
tornado_clustered_comp <- mutate(unsup_tornado_hclust_comp, cluster = tornado_assignments_comp)%>%
  arrange(cluster)
tornado_clustered_comp

cophenetic_matrix_comp <- cophenetic(hc_tornado_complete)
cophenetic_matrix_comp %>%
  round(3)

CPCC.complete <- cor(c(dist_tornado), c(cophenetic_matrix_comp)) %>%
  round(3)
CPCC.complete  # 0.531

# HIERARCHICAL SINGLE

hc_tornado_single <- hclust(dist_tornado, method = "single")

k_opt_single <- NbClust(data = unsup_tornado_hclust, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10,
                        method = "single", index = "silhouette", alphaBeale = 0.1)
k_opt_single 
# K=2 valore silhouette di pochissimo maggiore (+0.0012) rispetto al caso K=3
# per tutti gli indici : 10 dicono 2 (la metà)

plot(hc_tornado_single, labels=FALSE)%>%


tornado_assignments_single <- cutree(hc_tornado_single, k = 3)
tornado_clustered_single <- mutate(unsup_tornado_hclust, cluster = tornado_assignments_single)%>%
  arrange(cluster)
tornado_clustered_single

cophenetic_matrix_single <- cophenetic(hc_tornado_single)
cophenetic_matrix_single %>%
  round(3)

CPCC.single <- cor(c(dist_tornado), c(cophenetic_matrix_single)) %>%
  round(3)
CPCC.single # 0.493


# HIERARCHICAL AVERAGE

hc_tornado_av <- hclust(dist_tornado, method = "average")

k_opt_av <- NbClust(data = unsup_tornado_hclust, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10,
                    method = "average", index = "silhouette", alphaBeale = 0.1)
k_opt_av
# k=2 di poco più alto rispetto a k=3

plot(hc_tornado_av, labels=FALSE) 

tornado_assignments_av <- cutree(hc_tornado_av, k = 2)
tornado_clustered_av <- mutate(unsup_tornado_hclust, cluster = tornado_assignments_av)%>%
  arrange(cluster)
tornado_clustered_av

cophenetic_matrix_av <- cophenetic(hc_tornado_av)
cophenetic_matrix_av %>%
  round(3)

CPCC.av <- cor(c(dist_tornado), c(cophenetic_matrix_av)) %>%
  round(3)
CPCC.av # 0.747 (anche nel caso k=5)

# HIERARCHICAL CENTROID

hc_tornado_cent <- hclust(dist_tornado, method = "centroid")

k_opt_cent <- NbClust(data = unsup_tornado_hclust, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10,
                    method = "centroid", index = "silhouette", alphaBeale = 0.1)
k_opt_cent
# K=2 di poco superiore a k=3

plot(hc_tornado_cent, labels=FALSE) 

tornado_assignments_cent <- cutree(hc_tornado_cent, k = 2)
tornado_clustered_cent <- mutate(unsup_tornado_hclust, cluster = tornado_assignments_cent)%>%
  arrange(cluster)
tornado_clustered_cent

cophenetic_matrix_cent <- cophenetic(hc_tornado_cent)
cophenetic_matrix_cent %>%
  round(3)

CPCC.cent <- cor(c(dist_tornado), c(cophenetic_matrix_cent)) %>%
  round(3)
CPCC.cent # 0.714

# HIERARCHICAL WARD

hc_tornado_ward <- hclust(dist_tornado, method = "ward.D2")

k_opt_ward <- NbClust(data = unsup_tornado_hclust, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10,
                      method = "ward.D2", index = "silhouette", alphaBeale = 0.1)
k_opt_ward
# K=4 ma è di pochissimo superiore a k=3 e k=2

plot(hc_tornado_ward, labels=FALSE) 

tornado_assignments_ward <- cutree(hc_tornado_ward, k = 4)
tornado_clustered_ward <- mutate(unsup_tornado_hclust, cluster = tornado_assignments_ward)%>%
  arrange(cluster)
tornado_clustered_ward

cophenetic_matrix_ward <- cophenetic(hc_tornado_ward)
cophenetic_matrix_ward %>%
  round(3)

CPCC.ward <- cor(c(dist_tornado), c(cophenetic_matrix_ward)) %>%
  round(3)
CPCC.ward # 0.467

# VALIDITY
tornado_df_h <- scaled_unsup_tornado_df %>%
  mutate(h.cluster = tornado_clustered_av$cluster,
         class = tornado_df_nao$class)

# Remember that the kernel_df was a "supervised" dataset? It already contains the "target" class of each observation. We may therefore use one of the supervised methods to verify how good our clustering technique has performed.
# But before we do that, we can create a "confusion matrix", an empirical way to check corrispondence between classes and clusters:

table(tornado_df_h$class, tornado_df_h$h.cluster)
# As an objective measure we can now use Entropy
entropy_df_h <- tornado_df_h %>%
  count(h.cluster, class, name = "mij") %>%
  group_by(h.cluster) %>%
  mutate(
    mi = sum(mij),
    pij = mij / mi
  )

entropy_df_h

cluster_entropy_h <- entropy_df_h %>%
  summarise(
    e = -sum(pij * log2(pij))
  )

cluster_entropy_h

E_h <- entropy_df_h %>%
  select(-c(class, pij)) %>% # we don't need those quantities anymore
  ungroup() %>% # needed for the following mutate()
  mutate(
    m = sum(mij) # this line could have been replaced by m = nrow(kernel_df) without ungrouping at line 253 above
  ) %>%
  select(-mij) %>% # we need mij no more
  group_by(h.cluster) %>%
  filter(row_number() == 1) %>%
  left_join(cluster_entropy_h, by = "h.cluster") %>%
  ungroup() %>%
  summarise(E = sum(mi / m * e)) %>%
  as.numeric()

round(E_h, 3)

# PRECISION
precision_df_h <- tornado_df_h %>%
  count(h.cluster, class, name = "mij") %>%
  group_by(h.cluster) %>%
  mutate(
    mi = sum(mij),
    pij = mij / mi
  )

precision_df_h

# PURITY
purity_cluster_h <- precision_df_h %>%
  summarise(
    pi = max(pij)
  )
purity_cluster_h

#overall purity
P_h <- precision_df_h %>% # remember that precision_df is grouped by km.cluster
  distinct(h.cluster, mi) %>% 
  left_join(purity_cluster_h, by = "h.cluster") %>%
  ungroup() %>%
  summarise(
    P = sum(mi/sum(mi)*pi)
  ) %>%
  as.numeric()
P_h

# RECALL
recall_df_h <- tornado_df_h %>%
  count(class, name = "mj") %>%
  right_join(precision_df_h, by = "class") %>%
  select(h.cluster, class, mij, mj) %>%
  mutate(
    recall = mij/mj
  )
recall_df_h

# F-MEASURE
F_measure_h <- precision_df_h %>%
  select(h.cluster, class, pij) %>%
  left_join(recall_df_h) %>%
  select(-c(mij,mj)) %>%
  mutate(
    F_measure = 2*pij*recall/(pij+recall)
  )
F_measure_h

# Similarity Matrix
set.seed(123)

tornado_distance_h <- tornado_df_h %>%
  select(-c(h.cluster,class)) %>%
  dist() %>%
  as.matrix()

tornado_similarity_h <- 1 - (tornado_distance_h - min(tornado_distance_h))/(max(tornado_distance_h)-min(tornado_distance_h))

df_h <- as.data.frame(tornado_similarity_h) %>%
  rownames_to_column("row") %>%
  gather(key = "col", value = "similarity", 2:ncol(.)) %>%
  mutate_at(vars(row,col),as.numeric)

ggplot(df_h, aes(x = col, y = row)) +
  geom_tile(aes(fill = similarity)) +
  scale_fill_gradient2(name = "similarity") +
  theme_bw() +
  labs(x = "Point", y = "Point")


tornado_incidence_h <- tornado_df_h %>%
  select(h.cluster) %>%
  rownames_to_column() %>%
  group_by(h.cluster) %>%
  nest() %>%
  mutate(
    matr = map(data, function(x) matrix(rep.int(1, times = nrow(x)^2), nrow = nrow(x)))
  )

library(Matrix)
?bdiag
tornado_incidence_h <- as.matrix(bdiag(tornado_incidence_h$matr))

colnames(tornado_incidence_h) <- as.character(1:nrow(unsup_tornado_df))
cor(c(tornado_similarity_h),c(tornado_incidence_h)) # come creare la incidence matrix

# DBSCAN
library(dbscan)
library(fpc)
library(factoextra)
library(zoom)

dbscan::kNNdistplot(scaled_unsup_tornado_df, k = 5)%>%
  in.zoom()
abline(h = 0.47, lty = 2)

set.seed(123)
db <- fpc::dbscan(scaled_unsup_tornado_df, eps = 0.47, MinPts = 5)

fviz_cluster(db, data = scaled_unsup_tornado_df, choose.vars=c("slon", "elat"), stand = TRUE,
             ellipse = TRUE, show.clust.cent = TRUE,
             geom = "point", palette = "jco", ggtheme = theme_classic())

#VALIDITA' DB

tornado_df_db <- scaled_unsup_tornado_df %>%
  mutate(db.cluster = db$cluster,
         class = tornado_df_nao$class)

# Remember that the kernel_df was a "supervised" dataset? It already contains the "target" class of each observation. We may therefore use one of the supervised methods to verify how good our clustering technique has performed.
# But before we do that, we can create a "confusion matrix", an empirical way to check corrispondence between classes and clusters:

table(tornado_df_db$class, tornado_df_db$db.cluster)
# As an objective measure we can now use Entropy
entropy_df_db <- tornado_df_db %>%
  count(db.cluster, class, name = "mij") %>%
  group_by(db.cluster) %>%
  mutate(
    mi = sum(mij),
    pij = mij / mi
  )

entropy_df_db

cluster_entropy_db <- entropy_df_db %>%
  summarise(
    e = -sum(pij * log2(pij))
  )

cluster_entropy_db

E_db <- entropy_df_db %>%
  select(-c(class, pij)) %>% # we don't need those quantities anymore
  ungroup() %>% # needed for the following mutate()
  mutate(
    m = sum(mij) # this line could have been replaced by m = nrow(kernel_df) without ungrouping at line 253 above
  ) %>%
  select(-mij) %>% # we need mij no more
  group_by(db.cluster) %>%
  filter(row_number() == 1) %>%
  left_join(cluster_entropy_db, by = "db.cluster") %>%
  ungroup() %>%
  summarise(E = sum(mi / m * e)) %>%
  as.numeric()

round(E_db, 3)

# PRECISION
precision_df_db <- tornado_df_db %>%
  count(db.cluster, class, name = "mij") %>%
  group_by(db.cluster) %>%
  mutate(
    mi = sum(mij),
    pij = mij / mi
  )

precision_df_db

# PURITY
purity_cluster_db <- precision_df_db %>%
  summarise(
    pi = max(pij)
  )
purity_cluster_db

#overall purity
P_db <- precision_df_db %>% # remember that precision_df is grouped by km.cluster
  distinct(db.cluster, mi) %>% 
  left_join(purity_cluster_db, by = "db.cluster") %>%
  ungroup() %>%
  summarise(
    P = sum(mi/sum(mi)*pi)
  ) %>%
  as.numeric()
P_db

# RECALL
recall_df_db <- tornado_df_db %>%
  count(class, name = "mj") %>%
  right_join(precision_df_db, by = "class") %>%
  select(db.cluster, class, mij, mj) %>%
  mutate(
    recall = mij/mj
  )
recall_df_db

# F-MEASURE
F_measure_db <- precision_df_db %>%
  select(db.cluster, class, pij) %>%
  left_join(recall_df_db) %>%
  select(-c(mij,mj)) %>%
  mutate(
    F_measure = 2*pij*recall/(pij+recall)
  )
F_measure_db

# Similarity Matrix
set.seed(123)

tornado_distance_db <- tornado_df_db %>%
  select(-c(db.cluster,class)) %>%
  dist() %>%
  as.matrix()

tornado_similarity_db <- 1 - (tornado_distance_db - min(tornado_distance_db))/(max(tornado_distance_db)-min(tornado_distance_db))

df_db <- as.data.frame(tornado_similarity_db) %>%
  rownames_to_column("row") %>%
  gather(key = "col", value = "similarity", 2:ncol(.)) %>%
  mutate_at(vars(row,col),as.numeric)

ggplot(df_db, aes(x = col, y = row)) +
  geom_tile(aes(fill = similarity)) +
  scale_fill_gradient2(name = "similarity") +
  theme_bw() +
  labs(x = "Point", y = "Point")

tornado_incidence_db <- tornado_df_db %>%
  select(db.cluster) %>%
  rownames_to_column() %>%
  group_by(db.cluster) %>%
  nest() %>%
  mutate(
    matr = map(data, function(x) matrix(rep.int(1, times = nrow(x)^2), nrow = nrow(x)))
  )

library(Matrix)
?bdiag
tornado_incidence_db <- as.matrix(bdiag(tornado_incidence_db$matr))

colnames(tornado_incidence_db) <- as.character(1:nrow(unsup_tornado_df))
cor(c(tornado_similarity_db),c(tornado_incidence_db)) # come creare la incidence matrix