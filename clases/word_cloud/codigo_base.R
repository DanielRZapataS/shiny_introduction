library(shiny)
library(wordcloud)
library(RColorBrewer)


stop_worlds <-
  read.csv(
    "clases/word_cloud/stopwords-es.txt",
    encoding = "UTF-8",
    stringsAsFactors = F
  )

stop_worlds <- as.character(stop_worlds$X0)

texto <-
  readLines(sprintf("./clases/word_cloud/libros/%s.txt", "elquijote"),
            encoding = "UTF-8")

procesar_palabras <- function(descripcion){
  descripcion <- gsub("[[:punct:]]", " ", descripcion)
  descripcion <- gsub("[0-9]", " ", descripcion)
  descripcion <- trimws(descripcion)
  descripcion <- unlist(strsplit(descripcion, " "))
  descripcion <- descripcion[descripcion != ""]
  descripcion <- tolower(descripcion)
}

librocorpus <- procesar_palabras(texto)
tabla <- as.data.frame(table(librocorpus))
tabla <- tabla[order(tabla$Freq, decreasing = T),]
tabla <- tabla[!(tabla$librocorpus %in% stop_worlds),]

wordcloud(tabla$librocorpus, tabla$Freq, max.words = 100, colors =  brewer.pal(12, "Paired"))

total_palabras <- length(librocorpus)
palabras_unicas <- length(unique(librocorpus))
palabbras_especiales <- dim(tabla)[1]


# encontrar inicio de un libro 

texto <-
  readLines(sprintf("./clases/word_cloud/libros/%s.txt", "calderon"),
            encoding = "UTF-8")
for(i in 1:length(texto)){
  cat(i, "\n")
  if(substr(texto[i], 1, 12) == "*** START OF"){
    primera_linea <- i
    break()
  }
  
}



