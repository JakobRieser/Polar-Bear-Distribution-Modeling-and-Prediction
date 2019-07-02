#' with ggcorr:
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")

ggcorr(
  cm,
  method = c("pairwise", "pearson"),
  cor_matrix = NULL,
  nbreaks = NULL,
  digits = 2,
  name = "p",
  low = "#3B9AB2",
  mid = "#EEEEEE",
  high = "#F21A00",
  midpoint = 0,
  palette = NULL,
  geom = "tile",
  min_size = 2,
  max_size = 6,
  label = T,
  label_alpha = FALSE,
  label_color = "gray20",
  label_round = 1,
  label_size = 3.5,
  limits = TRUE,
  drop = !limits,
  layout.exp = ,
  legend.position = "right",
  legend.size = 9)

ggcorr(
  cm_biooracle,
  method = c("pairwise", "pearson"),
  cor_matrix = NULL,
  nbreaks = NULL,
  digits = 2,
  name = "p",
  low = "#3B9AB2",
  mid = "#EEEEEE",
  high = "#F21A00",
  midpoint = 0,
  palette = NULL,
  geom = "tile",
  min_size = 2,
  max_size = 6,
  label = T,
  label_alpha = FALSE,
  label_color = "gray20",
  label_round = 1,
  label_size = 3.5,
  limits = TRUE,
  drop = !limits,
  layout.exp = ,
  legend.position = "right",
  legend.size = 9)











#' with corrplot:
install.packages("corrplot")
library(corrplot)

corrplot(cm, method = "circle")
