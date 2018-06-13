Equilibrio
# Wordcloud

install.packages(c('tm', 'SnowballC', 'wordcloud', 'topicmodels'))
install.packages('RTextTools')
libs=c('tm', 'SnowballC', 'wordcloud','proxy','RTextTools')
# Load the libraries
lapply(libs, require, character.only=TRUE)

# Remove libraries list
rm(libs)

# Save each item in the corpus as a document
# Name each element on b.corpus
names(mydf_melt)=paste("document",1:length(unique(mydf_melt)),sep="")
# Export each list as a document
for (i in 1:length(unique(mydf_melt))) {
  write.csv(mydf_melt[i], file=paste0("C:/Users/SergioMC/Documents/Equilibrio/Research/corpus/",names(mydf_melt)[i], ".txt"))
}

# Next we load the data into the R Corpus Data Structure.
path= "C:/Users/SergioMC/Documents/Equilibrio/Research"

dir = DirSource(paste(path,"/corpus/",sep=""))

e.corpus = Corpus(dir)
summary(e.corpus)



# Normalization step
e.corpus = tm_map(e.corpus, content_transformer(tolower))
test_words = c("mmm", "y", "mmmm", "document","osea","pues","sino", "entonces","después", "ahí", "así", "ósea","creo","vez","cosa","digamos","siempre", "solo", "cosas","tan","tal","ahi","nunca","darme", "cuenta")
e.corpus = tm_map(e.corpus, removeWords, c(test_words, stopwords(kind = "spanish")))
e.corpus = tm_map(e.corpus, removePunctuation)
e.corpus = tm_map(e.corpus,content_transformer(gsub),pattern = "x\ndocument\\d", replacement = "  ")
e.corpus = tm_map(e.corpus, removeNumbers)
e.corpus =  tm_map(e.corpus, stripWhitespace)
e.corpus  = tm_map(e.corpus, content_transformer(iconv),from="UTF-8", to='ASCII//TRANSLIT')

# The next step is to create a Document-Term Matrix (DTM) with tfIDF. 
e.dtm.tfidf = DocumentTermMatrix(e.corpus,control=list(weighting = weightTfIdf))

# body.dtm.tfidf = as.matrix(body.dtm.tfidf)
e.dtm.tfidf = removeSparseTerms(e.dtm.tfidf, 0.99999)

e=as.matrix(e.dtm.tfidf)
freq = data.frame(sort(colSums(as.matrix(e.dtm.tfidf)), decreasing=TRUE))

wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"),scale = c(3,.6))






# The next step is to create a Document-Term Matrix (DTM). 

# First, create a document term matrix
bdocs = length(e.corpus)

# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- bdocs * 0.01

# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- bdocs * .5


review_dtm=DocumentTermMatrix(e.corpus, control= list(bounds = list(global = minTermFreq)))

review_dtm = weightTfIdf(review_dtm, normalize = TRUE)
dtm.matrix = as.matrix(review_dtm)

wordcloud(colnames(dtm.matrix), dtm.matrix[9,], max.words = 50,colors=brewer.pal(1, "Dark2"))

# freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
# 
# review_dtm_tfidf <- DocumentTermMatrix(e.corpus, control = list(weighting = weightTfIdf))
# review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.95)
# freq = data.frame(sort(colSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
# wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))


m=as.matrix(e.dtm.tfidf)

distMatrix <- dist(m, method="euclidean")
groups <- hclust(distMatrix,method="ward.D")
plot(groups, cex=0.9, hang=-1)
rect.hclust(groups, k=5)


table(output$cluster)






tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()





path= "C:/Users/SergioMC/Documents/Equilibrio/Research"

dir = DirSource(paste(path,"/corpus/",sep=""), encoding = "UTF-8")

e.corpus = Corpus(dir)
summary(e.corpus)


# First, create a document term matrix
bdocs <- length(e.corpus)

# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
minTermFreq <- bdocs * 0.01
# ignore overly common words i.e. terms that appear in more than 50% of the documents
maxTermFreq <- bdocs * .5
bodydtm = DocumentTermMatrix(e.corpus,
                             control = list(
                               stopwords = TRUE, 
                               stopwords("es"),
                               wordLengths=c(4, 15),
                               removePunctuation = T,
                               removeNumbers = T,
                               #stemming = T,
                               bounds = list(global = c(minTermFreq, maxTermFreq))
                             ))
bodydtm.matrix = as.matrix(bodydtm)

bodydtm = weightTfIdf(bodydtm, normalize = TRUE)
bodydtm.matrix = as.matrix(bodydtm)

wordcloud(colnames(bodydtm.matrix), bodydtm.matrix, max.words = 200)

sp.stopwords1=c("_", "a", "actualmente", "acuerdo","iba","abajo","document", "adelante", "ademas", "además", "adrede", "afirmó", "agregó", "ahi", "ahora", "ahí", "al", "algo", "alguna", "algunas", "alguno", "algunos", "algún", "alli", "allí", "alrededor", "ambos", "ampleamos", "antano", "antaño", "ante", "anterior", "antes", "apenas", "aproximadamente", "aquel", "aquella", "aquellas", "aquello", "aquellos", "aqui", "aquél", "aquélla", "aquéllas", "aquéllos", "aquí", "arriba", "arribaabajo", "aseguró", "asi", "así")
sp.stopwords2=c("un", "una", "unas", "uno", "unos", "usa", "usais", "usamos", "usan", "usar", "usas", "uso", "usted", "ustedes", "v", "va", "vais", "valor", "vamos", "van", "varias", "varios", "vaya", "veces", "ver", "verdad", "verdadera", "verdadero", "vez", "vosotras", "vosotros", "voy", "vuestra", "vuestras", "vuestro", "vuestros", "w", "x", "y", "ya", "yo", "z", "él", "éramos", "ésa", "ésas", "ése", "ésos", "ésta", "éstas", "éste", "éstos", "última", "últimas", "último", "últimos")
sp.stopwords3=c("atras", "aun", "aunque", "ayer", "añadió", "aún", "b", "bajo", "bastante", "bien", "breve", "buen", "buena", "buenas", "bueno", "buenos", "c", "cada", "casi", "cerca", "cierta", "ciertas", "cierto", "ciertos", "cinco", "claro", "comentó", "como", "con", "conmigo", "conocer", "conseguimos", "conseguir", "considera", "consideró", "consigo", "consigue", "consiguen", "consigues", "contigo", "contra", "cosas", "creo", "cual", "cuales", "cualquier", "cuando", "cuanta", "cuantas")
sp.stopwords4=c("cuanto", "cuantos", "cuatro", "cuenta", "cuál", "cuáles", "cuándo", "cuánta", "cuántas", "cuánto", "cuántos", "cómo", "d", "da", "dado", "dan", "dar", "de", "debajo", "debe", "deben", "debido", "decir", "dejó", "del", "delante", "demasiado", "demás", "dentro", "deprisa", "desde", "despacio", "despues", "después", "detras", "detrás", "dia", "dias", "dice", "dicen", "dicho", "dieron", "diferente", "diferentes", "dijeron", "dijo", "dio", "donde", "dos", "durante", "día", "días", "dónde", "e", "ejemplo", "el", "ella", "ellas", "ello", "ellos", "embargo", "empleais", "emplean", "emplear", "empleas", "empleo", "en", "encima", "encuentra", "enfrente", "enseguida", "entonces", "entre", "era", "erais", "eramos", "eran", "eras", "eres", "es", "esa", "esas", "ese", "eso", "esos", "esta", "estaba", "estabais", "estaban", "estabas", "estad", "estada", "estadas", "estado", "estados", "estais", "estamos", "estan", "estando", "estar", "estaremos", "estará", "estarán", "estarás", "estaré", "estaréis", "estaría", "estaríais", "estaríamos", "estarían", "estarías", "estas", "este", "estemos", "esto", "estos", "estoy", "estuve", "estuviera", "estuvierais", "estuvieran", "estuvieras", "estuvieron", "estuviese", "estuvieseis", "estuviesen", "estuvieses", "estuvimos", "estuviste", "estuvisteis", "estuviéramos", "estuviésemos", "estuvo", "está", "estábamos", "estáis", "están", "estás", "esté", "estéis", "estén", "estés", "ex", "excepto", "existe", "existen", "explicó", "expresó", "f", "fin", "final", "fue", "fuera", "fuerais", "fueran", "fueras", "fueron", "fuese", "fueseis", "fuesen", "fueses", "fui", "fuimos", "fuiste", "fuisteis", "fuéramos", "fuésemos", "g", "general", "gran", "grandes", "gueno", "h", "ha", "haber", "habia", "habida", "habidas", "habido", "habidos", "habiendo", "habla", "hablan", "habremos", "habrá", "habrán", "habrás", "habré", "habréis", "habría", "habríais", "habríamos", "habrían", "habrías", "habéis", "había", "habíais", "habíamos", "habían", "habías", "hace", "haceis", "hacemos", "hacen", "hacer", "hacerlo", "haces", "hacia", "haciendo", "hago", "han", "has", "hasta", "hay", "haya", "hayamos", "hayan", "hayas", "hayáis", "he", "hecho", "hemos", "hicieron", "hizo", "horas", "hoy", "hube", "hubiera", "hubierais", "hubieran", "hubieras", "hubieron", "hubiese", "hubieseis", "hubiesen", "hubieses", "hubimos", "hubiste", "hubisteis", "hubiéramos", "hubiésemos", "hubo", "i", "igual", "incluso", "indicó", "informo", "informó", "intenta", "intentais", "intentamos", "intentan", "intentar", "intentas", "intento", "ir", "j", "junto", "k", "l", "la", "lado", "largo", "las", "le", "lejos", "les", "llegó", "lleva", "llevar", "lo", "los", "luego", "lugar", "m", "mal", "manera", "manifestó", "mas", "mayor", "me", "mediante", "medio", "mejor", "mencionó", "menos", "menudo", "mi", "mia", "mias", "mientras", "mio", "mios", "mis", "misma", "mismas", "mismo", "mismos", "modo", "momento", "mucha", "muchas", "mucho", "muchos", "muy", "más", "mí", "mía", "mías", "mío", "míos", "n", "nada", "nadie", "ni", "ninguna", "ningunas", "ninguno", "ningunos", "ningún", "no", "nos", "nosotras", "nosotros", "nuestra", "nuestras", "nuestro", "nuestros", "nueva", "nuevas", "nuevo", "nuevos", "nunca", "o", "ocho", "os", "otra", "otras", "otro", "otros", "p", "pais", "para", "parece", "parte", "partir", "pasada", "pasado", "paìs", "peor", "pero", "pesar", "poca", "pocas", "poco", "pocos", "podeis", "podemos", "poder", "podria", "podriais", "podriamos", "podrian", "podrias", "podrá", "podrán", "podría", "podrían", "poner", "por", "por qué", "porque", "posible", "primer", "primera", "primero", "primeros", "principalmente", "pronto", "propia", "propias", "propio", "propios", "proximo", "próximo", "próximos", "pudo", "pueda", "puede", "pueden", "puedo", "pues", "q", "qeu", "que", "quedó", "queremos", "quien", "quienes", "quiere", "quiza", "quizas", "quizá", "quizás", "quién", "quiénes", "qué", "r", "raras", "realizado", "realizar", "realizó", "repente", "respecto", "s", "sabe", "sabeis", "sabemos", "saben", "saber", "sabes", "sal", "salvo", "se", "sea", "seamos", "sean", "seas", "segun", "segunda", "segundo", "según", "seis", "ser", "sera", "seremos", "será", "serán", "serás", "seré", "seréis", "sería", "seríais", "seríamos", "serían", "serías", "seáis", "señaló", "si", "sido", "siempre", "siendo", "siete", "sigue", "siguiente", "sin", "sino", "sobre", "sois", "sola", "solamente", "solas", "solo", "solos", "somos", "son", "soy", "soyos", "su", "supuesto", "sus", "suya", "suyas", "suyo", "suyos", "sé", "sí", "sólo", "t", "tal", "tambien", "también", "tampoco", "tan", "tanto", "tarde", "te", "temprano", "tendremos", "tendrá", "tendrán", "tendrás", "tendré", "tendréis", "tendría", "tendríais", "tendríamos", "tendrían", "tendrías", "tened", "teneis", "tenemos", "tener", "tenga", "tengamos", "tengan", "tengas", "tengo", "tengáis", "tenida", "tenidas", "tenido", "tenidos", "teniendo", "tenéis", "tenía", "teníais", "teníamos", "tenían", "tenías", "tercera", "ti", "tiempo", "tiene", "tienen", "tienes", "toda", "todas", "todavia", "todavía", "todo", "todos", "total", "trabaja", "trabajais", "trabajamos", "trabajan", "trabajar", "trabajas", "trabajo", "tras", "trata", "través", "tres", "tu", "tus", "tuve", "tuviera", "tuvierais", "tuvieran", "tuvieras", "tuvieron", "tuviese", "tuvieseis", "tuviesen", "tuvieses", "tuvimos", "tuviste", "tuvisteis", "tuviéramos", "tuviésemos", "tuvo", "tuya", "tuyas", "tuyo", "tuyos", "tú", "u", "ultimo")
sp.stopwords=c(sp.stopwords1,sp.stopwords2,sp.stopwords3,sp.stopwords4)
rm(sp.stopwords1,sp.stopwords2,sp.stopwords3,sp.stopwords4)
