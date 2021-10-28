# Ctr + Shift + Enter para correrlo todo =)

#1
pa1 <- function(a1,r,n) {
  
  PG = numeric(0)
  a=0
  i=0
  o=1
  PG[o] = a1
  n=n-1
  
  for (i in 1:n){
    
    i = i + 1
    o = o + 1
    a = a1 * r^(i-1)
    PG[o] = a
    PG
    
  }
  
  PGsum1 = sum(PG)
  print("Progresion geometrica")
  print(PG)
  print("Suma Progresion geometrica")
  print(PGsum1)
  
}

#2
pa2 <- function(a1,r,n) {
  
  num = (r^n) - 1
  den = 1 / (r - 1)
  Sn = a1 * num * den
  print("Suma Progresion geometrica Metodo 2")
  print(Sn)
  
}

#3
MnInscts <- function() {
  BASE = InsectSprays
  PI = by(BASE$count, BASE$spray, mean)
  PI
}

#4
CntInscts <- function(){
  BASE = InsectSprays
  table(BASE$spray, BASE$count>5)
}


pa1(2,3,20)
pa2(2,3,20)

MnInscts()
CntInscts()

#True = El insecticida Supera los 5 Insectos

