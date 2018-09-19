install.packages("UsingR")
install.packages("HistData")
require(UsingR)
require(HistData)
require(ggplot2)
#################################################problema 1#####################################################
a)
datos<-galton
modelopadrehijo<-lm(datos$child~datos$parent)
summary(modelopadrehijo)
#con un 95 % de confianza se rechaza la hipotesis nula bo=y^, lo cual indica que hay una relacion de 64.63%(pendiente) 
#entre la altura del padre y la del hijo
ggplot(datos,aes(datos$parent,datos$child))+geom_point()+
  geom_smooth(method = "lm")
#b)
#La relacion entre padres e hijos tiene un punto de partida en comun, que es 23.94, en este caso lo que quiere
#decir es que el valor de la altura esta determinado por este valor (ordenada al origen) mas un cambio (pendiente)
#el cual se interpreta como que por cada unidad que aumente la medida del papa, aumentara la del hijo en  .6463
#unidades respectivamente, que considerando los datos empieza desde 64
#c)
#predecir la altura del hijo cuyo padre mide 80 pulgadas
#el resultado arroja que el hijo podria medir 75.64473 pulgadas, lo cual suena congruente, tomando en cuenta las 
#pruebas de la regresion no estan muy relacionadas las variables y el modelo explica la relacion en un 20.96% por 
#tanto no es confiable
#?
#recta de regresion = 23.94153+0.64629x
recta<-23.94153+(0.64629*galton$parent)
error<-galton$child-recta
mediaerror<-mean(error)
sy<-sum(error-mediaerror)^2
syest<-sqrt(sy)
#################################################problema 2 #####################################################
modelogascaba<-lm(mtcars$mpg~mtcars$hp)
ggplot(mtcars,aes(mtcars$hp,mtcars$mpg))+geom_point()+geom_smooth(method = "lm")
#coeficientes
#La ordenada al origen nos dice que el maximo rendimiento de combustible posible es de 30.09 galones aprox,
#sin embargo conforme va aumentando el caballaje en una unidad el rendimieno de combustible disminuye en .06823 
#unidades.
#Probando la hipotesis nula tenemos que los p-value son menores al .05 establecido, por lo tanto la hipotesis de
#que no hay relacion entre los caballos de fuerza y el consumo de combustible se rechaza.
#Para 111 caballos de fuerza el consumo de combustible sera de 22.52533













