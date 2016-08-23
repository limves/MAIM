# Session 2,  C??digo en base a la presentaci??n de Alberto

# |A A B!d
set1 <-c("a", "b", "c")
set2 <-c("c", "d", "e")

intersect(set1, set2)
union(set1, set2)

jaccard.sim <- function(x,y)
{
  return(legend(intersect(x,y)) / legend(union(x,y)))
}

jaccard.sim(set1, set2)
# si los dos conuntos jakart mide 1, si son diferentes mide 0
# oTRaS formas de medir la simitud, a atra vez de coceno

# Cosine Similarity
# producto punto (de dos vectores a y b da un vector escalor)
# de dos conjuntos se jenera una medida unica... todas las instancias de un vector se multiplica
# v1 = (1*,5*,3*)

# |A A B!d
v1 <-c(1, 5, 3)
# v2 <-c(1, 5, 3) # v2 <-c(3, 7, 9)
v2 <-c(3, 7, 9)

sum (v1+v2)

cusine.sim <- function(x, y){
  return (sum(x*y)/(sqrt(sum(x*x)) * sqrt(sum(y*y))))
}

cosine.sim(v1, v2)




# 1	Filtrado Colaborativo para minar preferencias (basado en items)

#??2.- usa la funci??n read.csv para cargar los datos. 

music.usage <- read.csv("/temp/lastfm-matrix-germany.csv")
summary(music.usage)
str(music.usage)
music.usage

ncol(music.usage[1,1:10])

# 3.- Visualiza algunos datos del dataset. 

select.artists <- function(df)
{
  df <- df[-1]
  artists.names <- colnames(df)
  artists <- NULL
  for (i in 1:ncol(df))
  {
    if (df[,i] == 1)
      artists <- c(artists, artists.names[i])
  }
  artists
}
select.artists(music.usage[1,])
# 4.- Elimina la columna de usuarios

music.usage.nu <- music.usage[,!(names(music.usage) %in% c("user")) ]
music.usage.nu[1,1:10]

# 5.- Crea una funci??n para calcular la similitud Coseno. 
# ya se creo arriva

# 6.- Crea una matriz para guardar la similitud Coseno entre dos pares de canciones
music.usage.sim <- matrix (NA, nrow=ncol(music.usage.nu),
                          ncol= ncol(music.usage.nu),
                          dimnames= list(colnames(music.usage.nu), colnames(music.usage.nu) ))

music.usage.sim[1:5,1:5]

as.matrix(music.usage.nu[1])

# 7.- Calcula todas las distancias, despliega los primeros resultados. 

cusine.sim <- function(x, y){
  return (sum(x*y)/(sqrt(sum(x*x)) * sqrt(sum(y*y))))
}

for (i in 1:ncol(music.usage.nu)) {
  for (j in 1:ncol(music.usage.nu))
    music.usage.sim[i,j] <- cusine.sim(as.matrix(music.usage.nu[i]), as.matrix(music.usage.nu[j]))
}
music.usage.sim
  
# 8.- Crea una matriz para guardar los artistas m??s semejantes entre s??
music.usage.similar.artists <- matrix(NA, nrow = ncol (music.usage.sim), ncol=11, dimnames =list(colnames(music.usage.sim)))
music.usage.similar.artists

# poner los artistas que son mas semejantes.

#??9.- Encuentra a los 10 vecinos m??s cercanos y puebla la matriz creada anteriormente. 
# Crea un ciclo para procesar todos los artistas, 
#??ordena la matriz de similitud por artista, 
#??obt??n los primeros 11 valores y aplica la transpuesta de la matriz. 
music.usage.sim <- data.frame(music.usage.sim)
music.usage.similar.artists <- matrix(NA, nrow=ncol(music.usage.sim), ncol=11, 
                                      dimnames=list(colnames(music.usage.sim)))

head(n=11, rownames(music.usage.sim[order(music.usage.sim[,1],decreasing=T),][1]))
music.usage.similar.artists[]
order(music.usage.sim[,1],decreasing=T) 

for (i in 1:ncol(music.usage.sim))
{
  music.usage.similar.artists[i,] <- t(head(n=11, 
                                            rownames(music.usage.sim[order(music.usage.sim[,i],decreasing=T),][i])))
}

music.usage.similar.artists[1,1:10]

# 10.- Qu?? artistas son los m??s similares a los beatles?
music.usage.similar.artists
music.usage.similar.artists <- data.frame(music.usage.similar.artists)
music.usage.similar.artists <- music.usage.similar.artists[-1]
music.usage.similar.artists["the.beatles",]

# Segunda parte 

# second point 2
# By user
user.data <- matrix(NA, nrow = nrow(music.usage), ncol=ncol(music.usage) -1,
                    dimnames = list (music.usage$user, colnames(music.usage[-1])))

user.data
# point 3, 
user.data[1:5, 1:5]
# point 4
rownames(user.data)[1]
# five point 
music.usage[music.usage$user=="1","the.killers"]
# six
top.artists <- head(n=6, music.usage.sim[order(music.usage.sim[,"the.killers"],decreasing=T),]["the.killers"])
rownames(top.artists)
auxnames <-rownames(top.artists)
top.artists[,1]
# seven 
# history of user
top.artists.history <- music.usage[,c("user",auxnames)]
# Todo:

# eigth sum

user1.history <- head(music.usage[music.usage$user=="1",c]) ### FALTA... LO PUSO ARRIVA
score.recommendation <- function (history, similarities){
  return (sum(history*similarities)/sum(similarities))
}


for (i in  1:ncol(user.data)){ # users
  for (j in 1:ncol(user.data)){ # artists
    user <- rownames(user.data)[i]
    artist <- colnames(user.data)[j]
    
    # five point 
    if (music.usage[music.usage$user==user,artist] == 1)
    {
      user.data[i,j] <- ""
    }
    else 
    {
      top.artists <- head(n=6, music.usage.sim[order(music.usage.sim[,artist],decreasing=T),][artist])
      top.artists.names <- rownames(top.artist)
      top.artists.sim <- top.artist[,1]
      top.artists.names <- top.artists.names[-1]
      top.artists.sim <-top.artists.sim[-1]
      
      # history of user
      top.artists.history <- music.usage [music.usage$user,c("user", top.artists.names)][-1]
      
      # 
      user.data[i,j] <- score.recommendation(similarities = top.artists.sim, )
      score.recommendation # lo perdi...

    }
  }
}

