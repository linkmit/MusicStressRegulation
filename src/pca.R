library(tidyverse)
library(factoextra)
library(ggplot2)
library(PCAtest)

data <- read_csv("../data/pca_dataset.csv", col_types = cols())
df <- subset(data, select = -strategy)

#### pca ####
pca <- prcomp(df, scale = TRUE)
summary(pca)

#### track pca ####
df_track_features_plus_pca <-
  bind_cols(data, pca$x)

write_csv(df_track_features_plus_pca, "../outputs/df_track_features_plus_pca.csv")

#### scree plot ####
eig_plot <- fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50)) +
  ggtitle(NULL)

ggsave("../outputs/figs/scree_plot.jpeg", eig_plot, width = 6, height = 4, dpi = 800)

#### pca test ####
pca_test_res <- PCAtest(df, varcorr = T, plot = T)

#### loadings ####
pca_loadings <- as.data.frame(pca$rotation) %>%
  rownames_to_column(var = "Feature") %>%
  pivot_longer(cols = -Feature, names_to = "PrincipalComponent", values_to = "Loading") 

pca_1and2 <- pca_loadings %>%
  filter(PrincipalComponent %in% c("PC1", "PC2"))

write_csv(pca_1and2, "../outputs/pca_loadings.csv")

#### pca scores for strategies ####
df_strategy_pca_scores <- df_track_features_plus_pca |>
  select(strategy, PC1, PC2) |>
  group_by(strategy) |>
  summarise(PC1 = mean(PC1),
            PC2 = mean(PC2)
  ) %>%
  as.data.frame()

write.csv(df_strategy_pca_scores, "../outputs/strategy_pca_scores.csv", row.names = TRUE)

#### biplot - features & strategies ####
# include all features that have a significant correlation with PC1, PC2, and PC3
var_names <- rownames(get_pca_var(pca)$coord)
idx_pc1 <- c(6, 7, 11, 14, 28, 34, 35, 40, 41, 42, 55, 57)
idx_pc2 <- c(3, 6, 12, 13, 14, 15, 16, 18, 19, 23, 29, 30, 31, 33, 34, 35, 36, 37, 40, 41, 46, 47, 57,58)
idx_pc3 <- c(26, 30, 36, 56, 59, 61, 63, 64, 65, 66)


# PC1 vs PC2
idx_12 <- unique(c(idx_pc1, idx_pc2))
selected_vars_12 <- var_names[idx_all]

p <- fviz_pca_var(
  pca,
  axes = c(1,2),
  select.var = list(name = selected_vars_12_,
  repel = TRUE,
  labelsize = 2,
  max.overlaps = Inf
)

df_strategy_pca_scores_2 <- df_strategy_pca_scores |> select(- strategy)
rownames(df_strategy_pca_scores_2) <- df_strategy_pca_scores$strategy

fviz_add(
  p, 
  df_strategy_pca_scores_2, 
  map = "symbiplot", 
  color = "blue", 
  labelsize = 3,
  repel = TRUE
) +
  ggtitle(NULL)

ggsave("../outputs/biplot_pc1_vs_pc2.jpeg", width = 6, height = 6, dpi = 800, scale = 1.3)

# PC2 vs PC3
idx_all_23 <- unique(c(idx_pc2, idx_pc3))
selected_vars_23 <- var_names[idx_all_23]

p23 <- fviz_pca_var(
  pca,
  axes = c(2, 3),                      
  select.var = list(name = selected_vars_23),
  repel = TRUE,
  labelsize = 2,
  max.overlaps = Inf
)

fviz_add(
  p23,
  df_strategy_pca_scores_2,
  map = "symbiplot",
  color = "blue",
  labelsize = 3,
  repel = TRUE
) +
  ggtitle(NULL)

ggsave("../outputs/biplot_pc2_vs_pc3.jpeg", width = 6, height = 6, dpi = 800, scale = 1.3)

# PC1 vs PC3
idx_all_13 <- unique(c(idx_pc1, idx_pc3))
selected_vars_13 <- var_names[idx_all_13]

# Plot PC1 vs PC3
p13 <- fviz_pca_var(
  pca,
  axes = c(1, 3),
  select.var = list(name = selected_vars_13),
  repel = TRUE,
  labelsize = 2,
  max.overlaps = Inf
)

# Add strategy centroids
fviz_add(
  p13,
  df_strategy_pca_scores_2,
  map = "symbiplot",
  color = "blue",
  labelsize = 3,
  repel = TRUE
) +
  ggtitle(NULL)

ggsave("../outputs/biplot_pc1_vs_pc3.jpeg", width = 6, height = 6, dpi = 800, scale = 1.3)