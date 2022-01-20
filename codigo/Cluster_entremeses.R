##########################
##OPERACIONES QUE REALIZA
# 1. Cargar texto
# 2. Limpiar el texto 
# 3. TF-IDF: Eliminamos palabras demasiado raras y demasiado frecuentes
# 4. Nubes de palabras
# 5. Cluster
# 6. REDES: https://yuasaavedraco.github.io/Docs/Redes_Neuronales_con_R.html
##########################
# Establecer directorio
my_dir <-"~/Desktop/ENTREMESES/pdf"
# Cargar librerías
library(tm)
library(wordcloud2)
#Cargar textos
files <- list.files(path = my_dir, pattern = "pdf$")           
files # listamos los archivos con formato pdf
#A partir de los documentos en pdf, necesitaremos generar un Corpus, esto se hace de la siguiente manera
setwd(my_dir)
corp <- Corpus(URISource(files, encoding = "latin1"),  
               readerControl = list(reader = readPDF, language = "es-419"))
# summary(corp) 
ndocs <- length(corp)
ndocs # número de documentos leídos

# Limpiamos
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, content_transformer(removePunctuation))
corp <- tm_map(corp, content_transformer(removeNumbers))
corp <- tm_map(corp, removeWords, stopwords("spanish"))
mywords <- c("la", "el", "a", "de")
corp <- tm_map(corp, removeWords, mywords)
corp <- tm_map(corp, stripWhitespace)

#convertir el Corpus de cada documento en una tabla: eliminamos aquellas
# palabras que aparecen menos del 1%
# nos quedamos sólo con las palabras de entre 4 y 15 caracteres
# eliminamos también aquellas que aprecen en más del 50% 
# Ignorar palabras extrañas
minTermFreq <- ceiling(ndocs*0.1)
# Ignorar palabras muy comunes
maxTermFreq <- floor(ndocs*0.5)
dtm <- DocumentTermMatrix(corp,
                          control = list(
                            language = "es-419",
                            wordLengths = c(4, 15),
                            bounds = list(global = c(minTermFreq, maxTermFreq))
                          ))
inspect(dtm)

#tabla de contingencia que tuviera palabras que aparecen en 
# aproximadamente 30% de los documentos.
dtm2 <- removeSparseTerms(dtm, 0.7)
inspect(dtm2)

# Si quisiéramos ver toda la tabla, hacemos lo siguiente:
M <- as.matrix(dtm)
o <- order(sM <- colSums(M), decreasing = TRUE)
write.csv(M[,o], paste0(my_dir, "DTM.csv"), fileEncoding = "UTF-8")


# Generamos una nube de palabras para los documentos descartanto las que parezcan menos de 15 veces: (palabras, 
# frecuencia de las palabras). 
mywords <- data.frame(words = names(sM), freq = as.numeric(sM))
mywords2 <- mywords[mywords$freq > 25,]
wordcloud2(mywords2, fontFamily = "serif", 
           backgroundColor = "white", shape = 'pentagon', size = 0.4)

# cambiamos el punto de corte a 10 palabras.
mywords3 <- data.frame(words = names(M["Dub_EP_carcel.pdf", ]), freq = as.numeric(M["Dub_EP_carcel.pdf", ]))
mywords4 <- mywords3[mywords3$freq > 3,]
wordcloud2(mywords4, fontFamily = "serif", 
           backgroundColor = "black", shape = 'star', size = 0.4)


#agrupamiento jerárquico, fijando alguno de los criterios de enlace

distMatrix <- dist(M, method = "manhattan")
groups <- hclust(distMatrix, method = "ward.D")
plot(groups, main = "Dendograma de Entremeses SdO", cex=0.9, hang=-1, 
     xlab = "", ylab = "Altura")
rect.hclust(groups, k = 5, border="blue")

