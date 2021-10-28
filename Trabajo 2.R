library(readxl)
BASE <- read_excel("~/Archivos/Juan Andres/Universidad/20212/ESTADISTICA EXPLORATORIA/DATA/Recaudo_Vehicular_ANI.xlsx")
View(BASE)

medidas = function(x){
  Mín = min(x)
  Máx = max(x)
  Prom = mean(x)
  Med = median(x)
  DE = sd(x)
  CV = DE/Prom*100
  res = c(Mín, Máx, Prom, Med, DE, CV)
  names(res) = c("Mín", "Máx", "Prom", "Med", "DE", "CV")
  res
}

pp = function(){
  
  Año = as.integer(readline("Ingrese el Año: "))
  
  RecaudoDiario = BASE$Recaudo/BASE$Días
  BASE = cbind(BASE, RecaudoDiario)
  
  Ano = split(BASE, f = (BASE$Año))
  
  Ano2014= as.data.frame(Ano$`2014`)
  Ano2015= as.data.frame(Ano$`2015`)
  Ano2016= as.data.frame(Ano$`2016`)
  Ano2017= as.data.frame(Ano$`2017`)
  Ano2018= as.data.frame(Ano$`2018`)
  Ano2019= as.data.frame(Ano$`2019`)
  Ano2020= as.data.frame(Ano$`2020`)
  Ano2021= as.data.frame(Ano$`2021`)
  
  if (Año==2014){
    
    by(Ano2014$RecaudoDiario,Ano2014$Mes,function(x) medidas(x))
  } else {
    if (Año==2015){
      
      by(Ano2015$RecaudoDiario,Ano2015$Mes,function(x) medidas(x))
    } else {
      if (Año==2016){
        
        by(Ano2016$RecaudoDiario,Ano2016$Mes,function(x) medidas(x))
      } else {
        if (Año==2017){
          
          by(Ano2017$RecaudoDiario,Ano2017$Mes,function(x) medidas(x))
        } else {
          if (Año==2018){
            
            by(Ano2018$RecaudoDiario,Ano2018$Mes,function(x) medidas(x))
          } else {
            if (Año==2019){
              
              by(Ano2019$RecaudoDiario,Ano2019$Mes,function(x) medidas(x))
            } else {
              if (Año==2020){
                
                by(Ano2020$RecaudoDiario,Ano2020$Mes,function(x) medidas(x))
              } else {
                if (Año==2021){
                  
                  by(Ano2021$RecaudoDiario,Ano2021$Mes,function(x) medidas(x))
                } else {
                  print("Rectifique El Año")
                }
              }
            }
          }
        }
      }
    }
  }
}

pp()

sp = function(){
  
  RecaudoDiario = BASE$Recaudo/BASE$Días
  BASE = cbind(BASE, RecaudoDiario)
  
  ReVSAño = by(BASE$Recaudo, BASE$Año, mean)
  RDVSAño = by(BASE$RecaudoDiario, BASE$Año, mean)
  
  plot(ReVSAño,
       main="Promedio Anual De Recaudo",
       xlab= "Años",
       ylab= "Promedio Recaudo")
  
  plot(RDVSAño,
       main="Promedio Anual De Recaudo Diario",
       xlab= "Años",
       ylab= "Promedio Recaudo Diario")
}

sp()

tp = function(){
  
  Dias = BASE$Días
  Años = (BASE$Año)
  
  BASE$Recaudo.cortes <- cut(BASE$Recaudo,
                             breaks = 3)
  table(BASE$Recaudo.cortes, BASE$Días, BASE$Año)
  
}

tp()