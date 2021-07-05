p <- c("imager", "here","dplyr","purrr","broom","ggplot2","stats","base")
sapply(p, require, character.only = TRUE)  

# Reconstructing Images Using PCA
library(imager)
library(here)
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)



img <- load.image(here("/home/oscar/Imágenes/1979-breznev-y-honecker.jpg"))
str(img)

fpath <- system.file('/home/oscar/Imágenes/1979-breznev-y-honecker.jpg',package='imager') 
im <- load.image(fpath)
plot(im)

dim(img)



library(imager)


file <- system.file('/home/oscar/Imágenes/1979-breznev-y-honecker.jpg',package='imager')
#system.file gives the full path for a file that ships with a R package
#if you already have the full path to the file you want to load just run:
#im <- load.image("/somedirectory/myfile.png")
im <- load.image(file)

plot(im)

