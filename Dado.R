dado = function(){
  a=0
  n = as.integer(readline("Ingrese El Numero De Lanzamientos Del Dado: ")) 
  
  pares = numeric(0)
  impares = (numeric(0))
  
  for (i in 1:n){
    ran = sample(1:6, 1)
    a = a + 1
    if (ran%%2 ==0){
      pares[a]=ran
    } else {
      impares[a]=ran
    }
  }
  
  sumpares = sum(na.omit(pares))
  sumimpares = sum(na.omit(impares))
  resultado = sumpares - sumimpares
  
  print(sumpares)
  print(sumimpares)
  print(resultado)
  ifelse(resultado>0, "Ganador" , "Perdedor")
}

dado2 = function(){
  
  o=1
  
  
  for (i in o){
    
    num1 = matrix(numeric(0), nrow = 100, ncol = 1)
    sumanum=0
    a=0
    plot(x = 1,                 
         xlab = "Lanzamientos", 
         ylab = "Valor Suma Resultado Lanzamientos",
         xlim = c(0, 35), 
         ylim = c(0, 100),
         main = "Lanzamiento De Dados Hasta 100",
         type = "n")
    
    while (sumanum < 100){
      
      rannum = sample(1:6, 1)
      a = a + 1
      num1[a,]=rannum
      o=o+1
      sumanum= sum(na.omit(num1))
      points(x = o,
             y = sumanum,
             pch = 20)
      
    }
    
    print(sumanum)
    print(o)
    
  }
}

dado()
dado2()
