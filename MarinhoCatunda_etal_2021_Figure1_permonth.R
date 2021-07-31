library(gridExtra)
Fig1 = grid.arrange(Fig1a, Fig1b, Fig1c +
               annotation_custom(
                 grob = Fig1c_legend,
                 xmin = "Nov",
                 xmax = "Nov",
                 ymin = 13,
                 ymax = 14
               ), nrow = 3)

ggsave("MarinhoCatunda_etal_2021_Figure1_permonth.tiff", Fig1, height = 21, width = 14, units = "cm")
