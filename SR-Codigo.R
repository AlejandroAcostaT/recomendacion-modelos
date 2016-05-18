#####################################
#           Bibliotecas
#####################################
if(!require("arules")){
  install.packages("arules")
}
if(!require("arulesViz")){
  install.packages("arulesViz")
}

library("arules")
library("arulesViz")

#####################################
#         Parte 1 - articulos
#####################################

#Lectura de la etrada
periodico = read.csv("periodico.csv", stringsAsFactors = FALSE)

items = periodico$articles

#Se crean dos listas para el preprocesamiento de la data

temas = c("deportes", "politica", "variedades", "internacional",
          "nacionales", "sucesos", "comunidad", "negocios", "opinion")

articulos = c("articulo1", "articulo2", "articulo3", "articulo4",
             "articulo5", "articulo6", "articulo7", "articulo8",
             "articulo9")

##Preprocesamiento de los datos
#Ciclo para preprocesar la data
arts = c()

for (item in items) {
  x = gsub('[^[:alnum:] ]','' ,toString(item))
  x = (as.numeric(unlist(unlist(strsplit(x, "[^0-9]+")))))
  art = paste(toString(temas[((x[2]-1)/9)+1]),
              toString(articulos[((x[2]-1)%%9)+1])
              , sep="/")
  if(length(x)>2){
    for(i in (3:length(x))){
      art = paste(art, 
                  paste(toString(temas[((x[i]-1)/9)+1]),
                        toString(articulos[((x[i]-1)%%9)+1])
                        , sep="/"),
                  sep=",")
    }
  }
  arts = c(arts, art)
}

##Buscar numero de Bots:
l = length(periodico[,1])

bots = c()
times = c()
index = c()
for(i in (1:l)){
  secs = difftime(periodico$exit[i],periodico$entry[i], units = "secs")
  if(secs <= 20){
    bots = c(bots, i)
  }else{
    times = c(times, secs)
    index = c(index, i)
  }
}

length(bots)
arts=arts[-bots]
# Crear archivo con las transacciones
write(arts, file="transacciones")

##Detectar tipos de usuarios

##Usando arules para las transacciones
transacciones = read.transactions("transacciones", format="basket", sep=",")
unlink("transacciones")

# Top 10 transacciones en el dataset
itemFrequencyPlot(transacciones,topN=10,type="absolute")
top10 = sort(table(arts), decreasing = T)[1:10]
top10 = data.frame(top10)

#Obtener las reglas
rules <- apriori(transacciones, parameter = list(supp = 0.000022, conf = 0.85, maxlen=5))

#Funcion de recomendacion de un articulo
#rules son las reglas
#trans son los articulos leidos
recomendacion = function(rules, trans){
  articulo = sort(subset(rules, subset = lhs %in% trans),decreasing=TRUE,by="confidence")
  articulo = inspect(unique(articulo@rhs))
  l=c()
  for(i in (1:length(articulo[,1]))){
    l=c(l,gsub('[^[:alnum:]/]','' ,toString(articulo[i,])))
  }
  
  l=l[!l %in% trans]
  
  return (l[1])
}

#Prueba de la funcion
#siempre dejar rules
#respetar formato de entrada de los articulos (solo las palabras sin los '{}')
rec = recomendacion(rules, c("nacionales/articulo6", "deportes/articulo4", "deportes/articulo1"))

##Top 10 de tiempos
pair = data.frame(times, index)

#Visitas con menor tiempo de estadia
menor = pair[order(pair$times),]
id = menor$index[1:10]
segundos = menor$times[1:10]

transaccion = arts[id[1:10]]
menorTiempo = data.frame(id, transaccion, segundos)

#Visitas con mayor tiempo de estadia
mayor = pair[rev(order(pair$times)),]
id = mayor$index[1:10]
segundos = mayor$times[1:10]

transaccion = arts[id[1:10]]
mayorTiempo = data.frame(id, transaccion, segundos)

#####################################
#         Parte 2 - ROC
#####################################

generate_ROC = function(scores, real, target){
  # Generar curva
  real = real[order(scores, decreasing = T)]
  scores = sort(scores, decreasing = T)
  
  t = table(real)
  l = length(real)
  P = t[[target]]
  N = l - P
  FP = 0
  TP = 0
  Rx = c()
  Ry = c()
  fPrev = -Inf
  i = 1
  j = 1
  labs = c()
  while(i<=length(real)){
    if(scores[i] != fPrev){
     Rx[j] <- FP/N
     Ry[j] <- TP/P
     labs[j] = fPrev
     fPrev = scores[i]
     j = j + 1
    }
    if(real[i]==target){
      TP = TP + 1
    }else{
      FP = FP + 1
    }
    i = i + 1
  }
  Rx[j] <- FP/N
  Ry[j] <- TP/P
  labs[j] = 1.0
  plot(Rx, Ry, main = "ROC Curve", xlim = c(-0.005, 1.005), ylim = c(-0.05, 1.05), xlab = "FP-RATE", ylab = "TP-Rate", pch=16, col="red")
  lines(Rx, Ry, lty=5)
  lines(c(-2,2),c(-2,2), lty=5, col=16)
  text(Rx+0.025, Ry+0.025, labels = labs)
}

generate_ROC(scores, real, target)

  