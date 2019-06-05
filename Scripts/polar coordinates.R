coordinates(gammap)
gammap

polar <- CRS("+init=epsg:3995")

polar

crs(gammap) <- 

plot(gammap)

gammap_df <- as.data.frame(gammap, xy=TRUE)

summary(gammap_df)
gammap_df

x_lines <- seq(-120,180, by = 60)

ggplot() +
  geom_raster(data = gammap_df, aes(x = x, y = y), fill = "grey", colour = "black", alpha = 0.8) +
  
  # Convert to polar coordinates
  coord_map("ortho", orientation = c(90, 0, 0)) +
  scale_y_continuous(breaks = seq(45, 90, by = 5), labels = NULL) +
  
  # Removes Axes and labels
  scale_x_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  
  # Adds labels
  geom_text(aes(x = 180, y = seq(55, 85, by = 10), hjust = -0.2, label = paste0(seq(55, 85, by = 10), "°N"))) +
  geom_text(aes(x = x_lines, y = 39, label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W"))) +
  
  # Adds axes
  geom_hline(aes(yintercept = 45), size = 1)  +
  geom_segment(aes(y = 45, yend = 90, x = x_lines, xend = x_lines), linetype = "dashed") +
  
  # Change theme to remove axes and ticks
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                        colour = "black"),
        axis.ticks=element_blank()) +
  labs(caption = "Designed by Mikey Harper")

summary(Ursus_maritimus.df)
