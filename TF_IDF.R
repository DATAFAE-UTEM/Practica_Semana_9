#=================================================================================================#
                                           #    TF-IDF   #
#=================================================================================================#
install.packages("tm")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readr")

# librerias
library(tm)
library(ggplot2)
library(tidyverse)
library(readr)

# Data_text es un dataframe que se crea en ela archivo Pdf-to-text de la semana 7

# se crea un corpus con el texto limpio
corpu <- VCorpus(VectorSource(Data_text$Texto_limpio))

# cantidad de archivos
length(corpu)

# stopwprds personalizadas
myStopwords <- c( stopwords("spanish"),"Carlos","Pavez","Tolosa","sbif","sefialo","dofia")

# TDM aplicando la ponderación TF-IDF en lugar de la frecuencia del término
tdm <- TermDocumentMatrix(corpu,
                         control = list(weighting = weightTfIdf,
                                        stopwords = myStopwords,
                                        removePunctuation = T,
                                        removeNumbers = T))
tdm
inspect(tdm)

# frecuencia con la que aparecen los términos sumando el contenido de todos los términos (es decir, filas)
freq <- rowSums(as.matrix(tdm))
head(freq,10)
tail(freq,10)

# Trazar las frecuencias ordenadas
plot(sort(freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")

# 10 terminos mas frecuentes
tail(sort(freq),n=10)

# Términos más frecuentes y sus frecuencias en un diagrama de barras.
high.freq <- tail(sort(freq),n=10)
hfp.df <- as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df)

ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() +
  xlab("Terms") + ylab("Frequency") +
    ggtitle("Term frequencies")
#correlacion coseno, matris tf_idf

