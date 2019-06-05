ggcorr(
  cm[, -1],
  name = expression(rho),
  geom = "circle",
  max_size = 10,
  min_size = 2,
  size = 3,
  hjust = 0.75,
  nbreaks = 6,
  angle = -45,
  palette = "PuOr" # colorblind safe, photocopy-able
)


source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")





ggcorr(cm, method = c("pairwise", "pearson"), cor_matrix = NULL,
       nbreaks = NULL, digits = 2, name = "p", low = "#3B9AB2",
       mid = "#EEEEEE", high = "#F21A00", midpoint = 0, palette = NULL,
       geom = "tile", min_size = 2, max_size = 6, label = FALSE,
       label_alpha = FALSE, label_color = "black", label_round = 1,
       label_size = 2, limits = c(-1, 1), drop = is.null(limits) ||
         identical(limits, FALSE), layout.exp = 0, legend.position = "right",
       legend.size = 9)


ggcorr(
  cm,
  method = c("pairwise", "pearson"),
  cor_matrix = NULL,
  nbreaks = NULL,
  digits = 2,
  name = "",
  low = "#3B9AB2",
  mid = "#EEEEEE",
  high = "#F21A00",
  midpoint = 0,
  palette = NULL,
  geom = "tile",
  min_size = 2,
  max_size = 6,
  label = FALSE,
  label_alpha = FALSE,
  label_color = "black",
  label_round = 1,
  label_size = 4,
  limits = TRUE,
  drop = !limits,
  layout.exp = 0,
  legend.position = "right",
  legend.size = 9)
  