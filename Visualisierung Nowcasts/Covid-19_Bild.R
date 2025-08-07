#install.packages("png")
library(cowplot)
library(png)
library(grid)

# PNG einlesen
img1 <- readPNG("C:\\Users\\felix\\Desktop\\Uni\\BA\\Nowcasts\\00-04_früh_True_1.png")
img2 <- readPNG("C:\\Users\\felix\\Desktop\\Uni\\BA\\Nowcasts\\00-04_früh_True_2.png")
img3=readPNG("C:\\Users\\felix\\Desktop\\Uni\\BA\\Nowcasts\\00-04_früh_True_3.png")
img4=readPNG("C:\\Users\\felix\\Desktop\\Uni\\BA\\Nowcasts\\00-04_früh_10_1.png")
img5=readPNG("C:\\Users\\felix\\Desktop\\Uni\\BA\\Nowcasts\\00-04_früh_10_2.png")
img6=readPNG("C:\\Users\\felix\\Desktop\\Uni\\BA\\Nowcasts\\00-04_früh_10_3.png")
# In Raster-Grob umwandeln
g1 <- rasterGrob(img1, interpolate = TRUE)
g2 <- rasterGrob(img2, interpolate = TRUE)
g3=rasterGrob(img3, interpolate = TRUE)
g4=rasterGrob(img4, interpolate = TRUE)
g5=rasterGrob(img5, interpolate = TRUE)
g6=rasterGrob(img6, interpolate = TRUE)
# Nebeneinander anzeigen
plot_grid(g1,g4, g2,g5,g3,g6, ncol = 2,nrow = 3)
