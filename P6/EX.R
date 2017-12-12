#vacuna
pv<-runif #constantes
a$estado<-"R"
pv<-0.03
if (a$estado=R){
  break()
}

#Probabilidad de curarse se tiene que comparar con un if si la es mayor la probabilildad de curarse
#que paralelizaste, como y porque tambien que la funcion sea la misma no romperla
#si es mayor se cura desde el incio(al aparcer como infectado)
#variar pi para ver que le pasa a los maximos
#variar el efecto que tiene la dimension de la vacuna
#vacunar el 10%, 20%, etc. de la poblacion
#al paralelizar se mide el tiempo y se compara 
#se hace una prueba estadistica para comprobar que hay una diferencia
#tener los 3 estados, estado infectado recuperado y ni uno de los dos
#de los no infectados se vacuna una parte y otra parte se puede infectar
if (runif(1) < pv) {
  e <- "R"
}
if (runif(1) < pi&& e!="R") {
  e <- "I"
}
#l es el límite de la vecindad
#v es el numero de pasos que tienen que pasar para terminar la matriz
#contagios ind y verlos en globales como vectores logicos