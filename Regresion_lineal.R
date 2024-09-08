#Paquetes necesarios para la práctica: 

install.packages("ISLR")
library(ISLR)
install.packages("GGally") #Para ggpairs. 
library(GGally)
install.packages("leaps")
library(leaps)
install.packages("lmtest")
library(lmtest)
install.packages("car")
library("car")
install.packages("corrplot")
library(corrplot)
install.packages("olsrr")
library("olsrr")



head(carseats)
attach(Carseats); 
names(Carseats);




                                                               #MODELOS:

#TIPO 1: Con variables predictoras cuantitativas y cualitativas:

    #Modelo 1: Completo:

#Construimos el modelo utilizando todos los predictores y la variable respuesta Sales: 
RLM.Completo_1 = lm(Sales~., data=Carseats)
summary(RLM.Completo_1)
anova(RLM.Completo_1)

#Comparamos gráficos para cada variable dependiente y correlaciones (esto servirá para la discusión del
#modelo 2, para los modelos 2.3 y 2.4): 
ggpairs(Carseats, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

#VARIANZA CONSTANTE: 

bptest(RLM.Completo_1) #Prueba de Braunch-Pagan.

ncvTest(RLM.Completo_1) #Non-constant Variance Score Test.

#Colinealidad:
vif(RLM.Completo_1)

#INDEPENDENCIA RESIDUOS: ¿Autocorrelación?
dwtest(RLM.Completo_1)

#EXTRAYENDO OUTLIERS Y NORMALIDAD GRÁFICAMENTE: residuos vs valores ajustados (fifted values), residuos estandarizados frente a leverage:
par(mfrow=c(2, 2))
plot(RLM.Completo_1, col='deepskyblue4', pch=19)



BC_2 <- boxCox(RLM.Completo_1, family="yjPower", plotit = TRUE)


lambda <- BC_2$x[which.max(BC_2$y)]
lambda
"""
[1] 0.989899   #El valor exacto de lambda. 
"""

potencia <- yjPower(Sales, lambda)

RLM.Completo_1_transf <- lm(potencia ~ ., data = Carseats)
RLM.Completo_1_transf


summary(RLM.Completo_1_transf)
anova(RLM.Completo_1_transf)

par(mfrow=c(2, 2))
plot(RLM.Completo_1_transf, col='deepskyblue4', pch=19)









    #Modelo 2:


#MÉTODO FORWARD: Este método inicia con el modelo vacío y 
#busca el mejor modelo con una, luego dos, luego tres
#variables y así sucesivamente. 
RLM.Vacio <- lm(Sales~1, data=Carseats)
summary(RLM.Vacio)

RLM.Forward <- step(RLM.Vacio, scope=
                      list(lower=RLM.Vacio, upper=RLM.Completo),
                    direction = "forward") 
summary(RLM.Forward)


#Modelo con todas las variables explicativas:
RLM.Completo = lm(Sales~., data=Carseats)
summary(RLM.Completo)

#MÉTODO BACKWARD: Este inicia con el modelo completo. 


RLM.Backward <- step(RLM.Completo, scope=
                       list(lower=RLM.Vacio, upper=RLM.Completo),
                     direction = "backward") 
summary(RLM.Backward)



#MÉTODO STEPWISE: Este inicia con el modelo vacío. Este método nos quitaría
#las variables no significativas (a diferencia de los otros dos métodos). 


RLM.Stepwise <- step(RLM.Vacio, scope=
                       list(lower=RLM.Vacio, upper=RLM.Completo),
                     direction = "both") 
summary(RLM.Stepwise)
anova(RLM.Stepwise)





#Veamos qué obtenemos con regsubsets para todas las variables explicativas: 
modelos_mejoresCompleto <- regsubsets(Sales ~., data=Carseats, nbest=3, nvmax=7)
summary(modelos_mejoresCompleto)$which



#El Cp de Mallows compara la precisión y el sesgo del modelo completo con modelos que incluyen solo algunos predictores:

modelo_completo <- lm(Sales ~ ., data = Carseats)
modelo_reducido <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats)
summary(modelo_reducido)
anova(modelo_reducido)
ols_mallows_cp(modelo_reducido, modelo_completo)



par(mfrow=c(1, 2))
plot(modelos_mejoresCompleto, scale="adjr2", main=expression(R[Adj]^2))
plot(modelos_mejoresCompleto, scale="bic", main="BIC")





                                #Mejores variables guiándonos por el BIC (ANALÍTICAMENTE):
library(leaps)
summary(modelos_mejoresCompleto)$bic #Lista de todos los BIC
"""
 [1] -103.36220  -76.26634  -18.17982 -235.90366 -159.18848 -134.66784 -373.71021 -288.89418 -288.74490 -468.52957 -449.96052 -441.40171 -566.50696
[14] -559.56850 -549.98260 -711.31065 -608.13362 -607.03462 -774.30360 -707.37999 -706.95421
"""

which(summary(modelos_mejoresCompleto)$bic == min(summary(modelos_mejoresCompleto)$bic)) #Tomar el modelo con menor BIC.
"""
[1] 19 
"""

summary(modelos_mejoresCompleto)$which[19, ] #¿Cuál es el modelo 19?
"""
(Intercept)       CompPrice          Income     Advertising      Population           Price   ShelveLocGood ShelveLocMedium             Age 
           TRUE            TRUE            TRUE            TRUE           FALSE            TRUE            TRUE            TRUE            TRUE 
      Education        UrbanYes           USYes 
          FALSE           FALSE           FALSE
"""




                                    #Mejores variables guiándonos por el R^2 ajustado:

summary(modelos_mejoresCompleto)$adjr2
which(summary(modelos_mejoresCompleto)$adjr2 == max(summary(modelos_mejoresCompleto)$adjr2)) #Tomar el modelo con mayor adjr2.
summary(modelos_mejoresCompleto)$which[19, ]




                                                  #Correlación de predictores: 
#Correlación: 
install.packages("ISLR")
library(ISLR)
install.packages("GGally")
library(GGally)
ggpairs(modelo_reducido_final, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")  #REVISAR CORRELACIÓN COMPRICE Y PRICE.



                                          #Colinealidad o multicolinealidad entre predictores: 
modelo_reducido_final <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data=Carseats)

#Análisis de Inflación de Varianza (VIF): Se calcula un VIF por cada variable explicativa:

library(car)
vif(modelo_reducido_final)


                                            #Autocorrelación con Durbin-Watson: Veamos la INDEPENDENCIA DE ERRORES:

library(lmtest)
dwtest(modelo_reducido_final, alternative = "two.sided")

"""

	Durbin-Watson test

data:  modelo_reducido
DW = 1.9882, p-value = 0.4523  #DW está lejos de 0, siendo un valor MUY cercano a 2, y el p-valor es mayor que 0.05, luego hay independencia. No hay autocorrrelación entre los errores. 
alternative hypothesis: true autocorrelation is greater than 0
"""







                                                          #Comprobación Hipótesis:

#VARIANZA CONSTANTE: 

library(lmtest)
bptest(modelo_reducido) #Prueba de Braunch-Pagan

"""
H_{0}: Los errores tienen varianza constante (homoceodasticidad)
H_{1}: Los errores tienen NO varianza constante (NO hay homoceodasticidad)
	studentized Breusch-Pagan test

data:  modelo_reducido
BP = 4.7418, df = 7, p-value = 0.6914 #p-valor grande, luego aceptamosla hipótesis nula: Hay homoceodasticidad. 

Si el p-valor es menor que 0.05, entonces hay evidencias para decir que NO se cumple la homocedasticidad de los  
e_{i}.
 
"""
ncvTest(modelo_reducido)

"""
Non-constant Variance Score Test 
Variance formula: ~ fitted.values 
Chisquare = 0.1768675, Df = 1, p = 0.67408   #El p-valor es mayor que 0.05, aceptamos homoceodasticidad. 
"""




#NORMALIDAD DE LOS RESIDUOS:
modelo_reducido_residuos <- modelo_reducido$residuals
modelo_reducido_residuos


shapiro.test(modelo_reducido_residuos) #Ya vimos normalidad en algunos datos en ggpairs (foto del plot grande con muchas gráficas). 

"""
	Shapiro-Wilk normality test

data:  modelo_reducido_residuos
W = 0.99724, p-value = 0.7439  #El p-valor es mayor que 0.05, aceptamos normalidad de los residuos.
"""

ks.test(modelo_reducido_residuos, pnorm)

"""
	One-sample Kolmogorov-Smirnov test

data:  modelo_reducido_residuos
D = 0.026871, p-value = 0.9349  #El p-valor es holgadamente mayor que 0.05, aceptamos normalidad de los residuos.
alternative hypothesis: two-sided
"""

qqnorm(modelo_reducido_residuos) #La normalidad de los residuos implicaría normalidad en los Y. 
qqline(modelo_reducido_residuos)

#Gráficos más completo:
par(mfrow=c(2, 2))
plot(modelo_reducido, col='deepskyblue4', pch=19)

crPlots(modelo_reducido)

#Linealidad: 
mean(modelo_reducido_residuos)
"""
[1] 7.729413e-17
"""

                                                    #DETECCIÓN DE OUTLIERS: 
#Veamos cuales de los outliers son INFLUYENTES:  

tabla_influencias <- influence.measures(modelo_reducido) #Cook D_{i}, DFITS_{i}, DFBETAS_{i}.

#La tabla anterior proporciona una tabla de 400 observaciones, vamos a escoger de todas ellas
#las que son outliers y, además, cumplen los criterios para ser INFLUYENTES (Cook, 
#DFITS, DFBETAS).



#Observaciones extremas respecto a Y=Sales:
modelo_reducido <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats)
plot(predict(modelo_reducido), rstudent(modelo_reducido))

Carseats$VD <- rstudent(modelo_reducido)
which(Carseats$VD >= 2*6/400) #Sacamos aquellas observaciones con alto leverage (NO tienen por qué ser influyentes)



#2a forma: Cálculo con la teoría: El valor crítico de Bonferroni es t_{1-alpha/2n;n-p-1}=t_{1-0.05/2;400-6-1}=1.966019

CriticBonferroni <- qt(1-0.05/2,393)#valor crítico de Bonferroni. ¡OJO! ¡¡¡Inestable para n grande!!!!(Apuntes Rosa).
CriticBonferroni

sum(abs(rstudent(modelo_reducido))>CriticBonferroni)
"""
[1] 16  #I.e, 16 valores satisfacen la desigualdad.
"""

which.max(abs(rstudent(modelo_reducido)))
"""
358   #¿Cuál de ellos es el residuo studentizado más grande en valor absoluto? El 358
"""

#Comprobación del rstudent utilizando outlierTest: 
install.packages("car")
library("car")

outlierTest(modelo_reducido)

"""
          No Studentized residuals with Bonferroni p < 0.05
          Largest |rstudent|:
              rstudent unadjusted p-value Bonferroni p
          358  3.34075         0.00091592      0.36637         #La observación 358. 
          """








#Observaciones extremas respecto a las X:


#La F de tablas se calcula con qt: t_{0.05;n-p}=t_{0.5;6;400-6}=411.1304
F_tabla <- qt(0.5,6,400-6)
F_tabla
obs_cook <- cooks.distance(modelo_reducido)
which(obs_cook>F_tabla)

obs_dffits <- dffits(modelo_reducido)
which(abs(obs_dffits) > 2*sqrt(6/400)) #Quiero los influyentes solo, aquelloes que abs(dffits)>2*sqrt(p/n)


obs_dfbetas <- dfbetas(modelo_reducido)
which(abs(obs_dfbetas) > 2/sqrt(400))





influenceIndexPlot(modelo_reducido) #Valores hat, Bonferroni, Cook, Student estandarizado.
#Ver plot_diagnostico_reducido.png
influencePlot(modelo_reducido)  #VISTA ANALÍTICA DE LAS INFLUENCIAS. ALTERNATIVA A influenceIndexPlot. 









#Modelo 2.1. Quitar observaciones 357 y 358:

obs.out <- c(357,358) #observaciones a quitar
Carseats_obs_out <- Carseats[-obs.out,1:8] 

Carseats_obs_out_reducido <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats_obs_out)



par(mfrow=c(2, 2))
plot(Carseats_obs_out_reducido, col='deepskyblue4', pch=19)


bptest(Carseats_obs_out_reducido) #Prueba de Braunch-Pagan. #p-valor grande, luego aceptamosla hipótesis nula: Hay homoceodasticidad. 
"""
	studentized Breusch-Pagan test

data:  Carseats_obs_out_reducido
BP = 7.1492, df = 7, p-value = 0.4135
"""
ncvTest(Carseats_obs_out_reducido) #p-valor grande, luego aceptamosla hipótesis nula: Hay homoceodasticidad. 
"""
Non-constant Variance Score Test 
Variance formula: ~ fitted.values 
Chisquare = 0.4508098, Df = 1, p = 0.50195
"""

dwtest(Carseats_obs_out_reducido)
"""
	Durbin-Watson test

data:  Carseats_obs_out_reducido
DW = 1.9341, p-value = 0.2547
alternative hypothesis: true autocorrelation is greater than 0
"""


par(mfrow=c(1, 2))
plot(Carseats_obs_out_reducido, scale="adjr2", main=expression(R[Adj]^2))
plot(Carseats_obs_out_reducido, scale="bic", main="BIC")


vif(Carseats_obs_out_reducido) #Los valores VIF_{j} son pequeños, luego no hay colinealidad. Entre 0 y 1 bien, poco más de uno algo de colinealidad. 

summary(Carseats_obs_out_reducido)



Carseats_obs_out_reducido_residuos <- Carseats_obs_out_reducido$residuals
Carseats_obs_out_reducido_residuos


shapiro.test(Carseats_obs_out_reducido_residuos) 

"""
	Shapiro-Wilk normality test

data:  Carseats_obs_out_reducido_residuos
W = 0.99673, p-value = 0.6029  #El p-valor es mayor que 0.05, aceptamos normalidad de los residuos.
"""

ks.test(Carseats_obs_out_reducido_residuos, pnorm)

"""
	One-sample Kolmogorov-Smirnov test

data:  Carseats_obs_out_reducido_residuos
D = 0.022621, p-value = 0.897  #El p-valor es holgadamente mayor que 0.05, aceptamos normalidad de los residuos.
alternative hypothesis: two-sided
"""

library("olsrr")
Carseats_obs_out_completo <- lm(Sales ~ ., data = Carseats_obs_out)
Carseats_obs_out_reducido <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats_obs_out)
ols_mallows_cp(Carseats_obs_out_reducido, Carseats_obs_out_completo)

"""
[1] 5.546158
"""








#Modelo 2.2. Quitar observaciones 298 y 208:

obs.out_1 <- c(298,208) #observaciones a quitar
Carseats_obs_out_1 <- Carseats[-obs.out_1,1:8] 

Carseats_obs_out_reducido_1 <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats_obs_out_1)



par(mfrow=c(2, 2))
plot(Carseats_obs_out_reducido_1, col='deepskyblue4', pch=19)


bptest(Carseats_obs_out_reducido_1) #Prueba de Braunch-Pagan. #p-valor grande, luego aceptamosla hipótesis nula: Hay homoceodasticidad. 
"""
	studentized Breusch-Pagan test

data:  Carseats_obs_out_reducido_1
BP = 2.3938, df = 7, p-value = 0.9349
"""
ncvTest(Carseats_obs_out_reducido_1) #p-valor grande, luego aceptamosla hipótesis nula: Hay homoceodasticidad. 
"""
Non-constant Variance Score Test 
Variance formula: ~ fitted.values 
Chisquare = 0.001873212, Df = 1, p = 0.96548
"""

dwtest(Carseats_obs_out_reducido_1)
"""
	Durbin-Watson test

data:  Carseats_obs_out_reducido_1
DW = 2.0224, p-value = 0.5885
alternative hypothesis: true autocorrelation is greater than 0
"""


par(mfrow=c(1, 2))
plot(Carseats_obs_out_reducido_1, scale="adjr2", main=expression(R[Adj]^2))
plot(Carseats_obs_out_reducido_1, scale="bic", main="BIC")


vif(Carseats_obs_out_reducido_1) #Los valores VIF_{j} son pequeños, luego no hay colinealidad. Entre 0 y 1 bien, poco más de uno algo de colinealidad. 

summary(Carseats_obs_out_reducido_1)



Carseats_obs_out_reducido_residuos_1 <- Carseats_obs_out_reducido_1$residuals
Carseats_obs_out_reducido_residuos_1


shapiro.test(Carseats_obs_out_reducido_residuos_1) 

"""
	Shapiro-Wilk normality test

data:  Carseats_obs_out_reducido_residuos
W = 0.99647, p-value = 0.6305 #El p-valor es mayor que 0.05, aceptamos normalidad de los residuos.
"""

ks.test(Carseats_obs_out_reducido_residuos_1, pnorm)

"""
	One-sample Kolmogorov-Smirnov test

data:  Carseats_obs_out_reducido_residuos
D = 0.022742, p-value = 0.9862  #El p-valor es holgadamente mayor que 0.05, aceptamos normalidad de los residuos.
alternative hypothesis: two-sided
"""

library("olsrr")
Carseats_obs_out_completo_1 <- lm(Sales ~ ., data = Carseats_obs_out_1)
Carseats_obs_out_reducido_1 <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats_obs_out_1)
ols_mallows_cp(Carseats_obs_out_reducido_1, Carseats_obs_out_completo_1)

"""
[1] 5.546158
"""

AIC(Carseats_obs_out_reducido_1)
BIC(Carseats_obs_out_reducido_1)








#Modelo 2.3. Quitar observaciones 43 y 211:

obs.out_2 <- c(43,211) #observaciones a quitar
Carseats_obs_out_2 <- Carseats[-obs.out_2,1:8] 

Carseats_obs_out_reducido_2 <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats_obs_out_2)

summary(Carseats_obs_out_reducido_2)

par(mfrow=c(2, 2))
plot(Carseats_obs_out_reducido_2, col='deepskyblue4', pch=19)


bptest(Carseats_obs_out_reducido_2) #Prueba de Braunch-Pagan. #p-valor grande, luego aceptamosla hipótesis nula: Hay homoceodasticidad. 
"""
	studentized Breusch-Pagan test

data:  Carseats_obs_out_reducido_2
BP = 4.7387, df = 7, p-value = 0.6918
"""
ncvTest(Carseats_obs_out_reducido_2) #p-valor grande, luego aceptamosla hipótesis nula: Hay homoceodasticidad. 
"""
Non-constant Variance Score Test 
Variance formula: ~ fitted.values 
Chisquare = 0.1889362, Df = 1, p = 0.6638
"""

dwtest(Carseats_obs_out_reducido_2)
"""
	Durbin-Watson test

data:  Carseats_obs_out_reducido_2
DW = 1.9976, p-value = 0.4902
alternative hypothesis: true autocorrelation is greater than 0
"""


par(mfrow=c(1, 2))
plot(Carseats_obs_out_reducido_2, scale="adjr2", main=expression(R[Adj]^2))
plot(Carseats_obs_out_reducido_2, scale="bic", main="BIC")


vif(Carseats_obs_out_reducido_2) #Los valores VIF_{j} son pequeños, luego no hay colinealidad. Entre 0 y 1 bien, poco más de uno algo de colinealidad. 

summary(Carseats_obs_out_reducido_2)



Carseats_obs_out_reducido_residuos_2 <- Carseats_obs_out_reducido_2$residuals
Carseats_obs_out_reducido_residuos_2

mean(Carseats_obs_out_reducido_residuos_2)

shapiro.test(Carseats_obs_out_reducido_residuos_2) 

"""
	Shapiro-Wilk normality test

data:  Carseats_obs_out_reducido_residuos
W = 0.99716, p-value = 0.7243 #El p-valor es mayor que 0.05, aceptamos normalidad de los residuos.
"""

ks.test(Carseats_obs_out_reducido_residuos_2, pnorm)

"""
	One-sample Kolmogorov-Smirnov test

data:  Carseats_obs_out_reducido_residuos
D = 0.027662, p-value = 0.9209  #El p-valor es holgadamente mayor que 0.05, aceptamos normalidad de los residuos.
alternative hypothesis: two-sided
"""

library("olsrr")
Carseats_obs_out_completo_2 <- lm(Sales ~ ., data = Carseats_obs_out_2)
Carseats_obs_out_reducido_2 <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats_obs_out_2)
ols_mallows_cp(Carseats_obs_out_reducido_2, Carseats_obs_out_completo_2)

"""
[1] 5.589415
"""

AIC(Carseats_obs_out_reducido_2)
BIC(Carseats_obs_out_reducido_2)

















ggpairs(Carseats, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")







#Modelo 2.4: Quitamos CompPrice: 


modelo_reducido_final_pruebas_1 <- lm(Sales ~ ShelveLoc + Price + Advertising + Age + Income, data=Carseats)
summary(modelo_reducido_final_pruebas_1)




"""
Call:
lm(formula = Sales ~ ShelveLoc + Price + Advertising + Age + 
    Income, data = Carseats)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.7968 -0.9912 -0.1068  0.9670  4.2404 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)     13.400594   0.545299  24.575  < 2e-16 ***
ShelveLocGood    4.875602   0.230253  21.175  < 2e-16 ***
ShelveLocMedium  2.004566   0.189280  10.590  < 2e-16 ***
Price           -0.060559   0.003285 -18.436  < 2e-16 ***
Advertising      0.105665   0.011642   9.076  < 2e-16 ***
Age             -0.049826   0.004790 -10.401  < 2e-16 ***
Income           0.013550   0.002771   4.891 1.47e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.539 on 393 degrees of freedom
Multiple R-squared:  0.7074,	Adjusted R-squared:  0.7029   #bajan la R^2 ajustada, la F-statistics.
F-statistic: 158.3 on 6 and 393 DF,  p-value: < 2.2e-16
"""

library("olsrr")
modelo_completo <- lm(Sales ~ ., data = Carseats)
ols_mallows_cp(modelo_reducido_final_pruebas_1, modelo_completo) #Número horrible: 37347065. Muy mal ajustado.

AIC(modelo_reducido_final_pruebas_1)
BIC(modelo_reducido_final_pruebas_1)

library(lmtest)
dwtest(modelo_reducido_final_pruebas_1, alternative = "two.sided")
"""
	Durbin-Watson test

data:  modelo_reducido_final_pruebas_1
DW = 1.9296, p-value = 0.4789                                  #NO autocorrelación :D
alternative hypothesis: true autocorrelation is not 0   
"""

#NORMALIDAD DE LOS RESIDUOS:
modelo_reducido_final_pruebas_residuos_1 <- modelo_reducido_final_pruebas_1$residuals
modelo_reducido_final_pruebas_residuos_1


shapiro.test(modelo_reducido_final_pruebas_residuos_1) #El p-valor es mayor que 0.05, aceptamos normalidad de los residuos.

"""
	Shapiro-Wilk normality test

data:  modelo_reducido_final_pruebas_residuos_1
W = 0.99459, p-value = 0.1717           #El p-valor es mayor que 0.05, aceptamos normalidad de los residuos.

"""
ks.test(modelo_reducido_final_pruebas_residuos_1, pnorm)

"""
	One-sample Kolmogorov-Smirnov test

data:  modelo_reducido_final_pruebas_residuos_1
D = 0.10061, p-value = 0.0006087                      #No se acepta la normalidad de los residuos.
alternative hypothesis: two-sided
"""


qqnorm(modelo_reducido_final_pruebas_residuos_1) #No se acepta la normalidad de los residuos. 
qqline(modelo_reducido_final_pruebas_residuos_1)


#Homoceodasticidad:

library(lmtest)
bptest(modelo_reducido_final_pruebas_1) #Prueba de Braunch-Pagan

"""

	studentized Breusch-Pagan test

data:  modelo_reducido
BP = 1.4535, df = 6, p-value = 0.9625 #El p-valor es mayor que 0.05, aceptamos homoceodasticidad.
"""

ncvTest(modelo_reducido_final_pruebas_1)
"""
Non-constant Variance Score Test 
Variance formula: ~ fitted.values 
Chisquare = 0.01949957, Df = 1, p = 0.88894 #El p-valor es mayor que 0.05, aceptamos homoceodasticidad. 
"""

library(car)
vif(modelo_reducido_final_pruebas_1)
"""
                GVIF Df GVIF^(1/(2*Df))
ShelveLoc   1.014783  2        1.003676
Price       1.018505  1        1.009210
Advertising 1.009404  1        1.004691
Age         1.014095  1        1.007023
Income      1.012469  1        1.006215
"""


par(mfrow=c(2, 2))
plot(modelo_reducido_final_pruebas_1, col='deepskyblue4', pch=19)


influenceIndexPlot(modelo_reducido_final_pruebas_1)


#PRUEBAS:
#Quitamos observaciones 42, 26
obs.out <- c(26, 144, 51, 107) #observaciones a quitar
Carseats_obs_out_prueba <- Carseats[-obs.out,1:8] 

Carseats_obs_out_prueba_1 <- lm(Sales ~ ShelveLoc + Price + Advertising + Age + Income, data = Carseats_obs_out_prueba)


par(mfrow=c(2, 2))
plot(Carseats_obs_out_prueba_1, col='deepskyblue4', pch=19)

summary(Carseats_obs_out_prueba_1)






















                                                     #OPCIÓN 2: Sin Price


modelo_reducido_final_pruebas_2 <- lm(Sales ~ ShelveLoc + Advertising + Age + Income + CompPrice, data=Carseats)
summary(modelo_reducido_final_pruebas_2)
anova(modelo_reducido_final_pruebas_2)
"""
Call:
lm(formula = Sales ~ ShelveLoc + CompPrice + Advertising + Age + 
    Income, data = Carseats)

Residuals:
    Min      1Q  Median      3Q     Max 
-6.2818 -1.4829 -0.1108  1.4570  5.9593 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      4.898750   1.039363   4.713 3.39e-06 ***
ShelveLocGood    4.663241   0.313864  14.858  < 2e-16 ***
ShelveLocMedium  1.911349   0.258159   7.404 8.13e-13 ***
CompPrice        0.007220   0.006917   1.044    0.297       #CompPrice se vuelve no significativo.
Advertising      0.096389   0.015864   6.076 2.92e-09 ***
Age             -0.040049   0.006532  -6.131 2.13e-09 ***
Income           0.016806   0.003784   4.442 1.16e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.099 on 393 degrees of freedom
Multiple R-squared:  0.4558,	Adjusted R-squared:  0.4475  #Además baja todo. ES UNA MIERDA DE OPCIÓN. 
F-statistic: 54.86 on 6 and 393 DF,  p-value: < 2.2e-16
"""



#Quitamos CompPrice:
modelo_reducido_final_pruebas_2_final <- lm(Sales ~ ShelveLoc + Advertising + Age + Income, data=Carseats)
summary(modelo_reducido_final_pruebas_2_final)
"""
Call:
lm(formula = Sales ~ ShelveLoc + Advertising + Age + Income, 
    data = Carseats)

Residuals:
    Min      1Q  Median      3Q     Max 
-6.3079 -1.4777 -0.0678  1.4416  6.0422 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      5.854636   0.491421  11.914  < 2e-16 ***
ShelveLocGood    4.675320   0.313686  14.904  < 2e-16 ***
ShelveLocMedium  1.919267   0.258076   7.437 6.50e-13 ***
Advertising      0.096022   0.015862   6.053 3.30e-09 ***
Age             -0.040744   0.006499  -6.270 9.52e-10 ***
Income           0.016500   0.003773   4.374 1.57e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.099 on 394 degrees of freedom
Multiple R-squared:  0.4543,	Adjusted R-squared:  0.4474      
F-statistic:  65.6 on 5 and 394 DF,  p-value: < 2.2e-16
"""

par(mfrow=c(1, 2))
plot(modelo_reducido_final_pruebas_2_final, scale="adjr2", main=expression(R[Adj]^2))
plot(modelo_reducido_final_pruebas_2_final, scale="bic", main="BIC")


library("olsrr")
modelo_completo <- lm(Sales ~ ., data = Carseats)
ols_mallows_cp(modelo_reducido_final_pruebas_2_final, modelo_completo) #Número horrible: 69455584. Muy mal ajustado.




library(lmtest)
dwtest(modelo_reducido_final_pruebas_2_final, alternative = "two.sided")

"""
	Durbin-Watson test

data:  modelo_reducido_final_pruebas_2_final
DW = 1.9023, p-value = 0.3255            #El p-valor es mayor que 0.05, aceptamos normalidad de los residuos.
alternative hypothesis: true autocorrelation is not 0
"""



#NORMALIDAD DE LOS RESIDUOS:
modelo_reducido_final_pruebas_2_final_residuos <- modelo_reducido_final_pruebas_2_final$residuals
modelo_reducido_final_pruebas_2_final_residuos


shapiro.test(modelo_reducido_final_pruebas_2_final_residuos) #El p-valor es mayor que 0.05, aceptamos normalidad de los residuos.
#Si DW está lejos de 0, siendo un valor MUY cercano a 2, y el p-valor es mayor que 0.05, luego hay independencia. No hay autocorrrelación entre los errores. 

"""
	Shapiro-Wilk normality test

data:  modelo_reducido_final_pruebas_2_final_residuos
W = 0.99698, p-value = 0.6702        #El p-valor es mayor que 0.05, aceptamos normalidad de los residuos.

"""
ks.test(modelo_reducido_final_pruebas_2_final_residuos, pnorm)

"""
	One-sample Kolmogorov-Smirnov test

data:  modelo_reducido_final_pruebas_2_final_residuos
D = 0.18583, p-value = 2.01e-12                #NO se acepta la normalidad de los residuos. 
alternative hypothesis: two-sided

"""


qqnorm(modelo_reducido_final_pruebas_2_final_residuos) #No se acepta la normalidad de los residuos. 
qqline(modelo_reducido_final_pruebas_2_final_residuos)


#Homoceodasticidad:

library(lmtest)
bptest(modelo_reducido_final_pruebas_2_final) #Prueba de Braunch-Pagan

"""

	studentized Breusch-Pagan test

data:  modelo_reducido_final_pruebas_2_final
BP = 7.0565, df = 5, p-value = 0.2165      #El p-valor es mayor que 0.05, aceptamos homoceodasticidad.
"""
library(car)
ncvTest(modelo_reducido_final_pruebas_2_final)
"""
Non-constant Variance Score Test 
Variance formula: ~ fitted.values 
Chisquare = 0.3248429, Df = 1, p = 0.56871 #El p-valor es mayor que 0.05, aceptamos homoceodasticidad. 
"""

library(car)
vif(modelo_reducido_final_pruebas_2_final)
"""
                GVIF Df GVIF^(1/(2*Df))
ShelveLoc   1.012514  2        1.003114
Advertising 1.007367  1        1.003676
Age         1.003371  1        1.001684
Income      1.009094  1        1.004537
"""

kappa(modelo_reducido_final_pruebas_2_final) #INDICA FUERTE COLINEALIDAD: 379.0389


par(mfrow=c(2, 2))
plot(modelo_reducido_final_pruebas_2_final, col='deepskyblue4', pch=19)







#TRANSFORMACIÓN 
#Me da error, me dice que la variable respuesta debe ser positiva, esto es porque hay un dato, el 175, cuyo valor es y=0.00
#que es el causante del problema, pero no se si hay más. Para solucionarlo, vamos a añadir una constante a la variable 
#respuesta Sales para poder aplicar boxcox, el problema es que al sumar una constante, no sabemos exactamente qué cantidad
#sumar sin afectar a las demás pruebas, lo que causa problemas futuros. Por tanto, recurrimos a las Transformaciones Yeo-Johnson:


BC <- boxCox(modelo_reducido_final_pruebas_2_final, family="yjPower", plotit = TRUE)


lambda <- BC$x[which.max(BC$y)]
lambda
"""
[1] 0.9494949   #El valor exacto de lambda. 
"""

modelo_reducido_final_pruebas_2_transformado <- yjPower(Sales, lambda)

modelo_reducido_final_pruebas_2_2_transformado <- lm(modelo_reducido_final_pruebas_2_transformado ~ ShelveLoc + Price + Advertising + Age + Income, data = Carseats)
modelo_reducido_final_pruebas_2_2_transformado


summary(modelo_reducido_final_pruebas_2_2_transformado)

"""
Call:
lm(formula = modelo_reducido_final_pruebas_2_transformado ~ ShelveLoc + 
    Price + Advertising + Age + Income, data = Carseats)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.6111 -0.8757 -0.0962  0.8771  3.8408 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)     12.260335   0.491160  24.962  < 2e-16 ***
ShelveLocGood    4.377784   0.207393  21.109  < 2e-16 ***
ShelveLocMedium  1.816476   0.170488  10.655  < 2e-16 ***
Price           -0.054473   0.002959 -18.411  < 2e-16 ***
Advertising      0.094977   0.010486   9.057  < 2e-16 ***
Age             -0.044827   0.004315 -10.390  < 2e-16 ***
Income           0.012219   0.002496   4.896 1.43e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.386 on 393 degrees of freedom
Multiple R-squared:  0.7063,	Adjusted R-squared:  0.7019 
F-statistic: 157.5 on 6 and 393 DF,  p-value: < 2.2e-16

"""


#Colinealidad o multicolinealidad entre predictores: 

#Análisis de Inflación de Varianza (VIF): Se calcula un VIF por cada variable explicativa:

library(car)
vif(modelo_reducido_final_pruebas_2_2_transformado) #Los valores VIF_{j} son pequeños, luego no hay colinealidad. Entre 0 y 1 bien, poco más de uno algo de colinealidad. 


#Autocorrelación con Durbin-Watson: Veamos la INDEPENDENCIA DE ERRORES:
library(lmtest)
dwtest(modelo_reducido_final_pruebas_2_2_transformado, alternative = "two.sided")

"""
Durbin-Watson test

data:  modelo_reducido_final_pruebas_2_2_transformado
DW = 1.9294, p-value = 0.4779
alternative hypothesis: true autocorrelation is not 0
"""


influenceIndexPlot(modelo_reducido_final_pruebas_2_2_transformado) #Valores hat, Bonferroni, Cook, Student estandarizado.
#Ver plot_diagnostico_reducido.png
influencePlot(modelo_reducido_final_pruebas_2_2_transformado)  #VISTA ANALÍTICA DE LAS INFLUENCIAS. ALTERNATIVA A influenceIndexPlot. 

par(mfrow=c(2, 2))
plot(modelo_reducido_final_pruebas_2_2_transformado, col='deepskyblue4', pch=19)


library(lmtest)
bptest(modelo_reducido) #Prueba de Braunch-Pagan. #p-valor grande, luego aceptamos la hipótesis nula: Hay homoceodasticidad. 

ncvTest(modelo_reducido) #p-valor grande, luego aceptamos la hipótesis nula: Hay homoceodasticidad. 

#NORMALIDAD DE LOS RESIDUOS:
modelo_reducido_final_pruebas_2_2_transformado_residuos <- modelo_reducido_final_pruebas_2_2_transformado$residuals
modelo_reducido_final_pruebas_2_2_transformado_residuos

ks.test(modelo_reducido_final_pruebas_2_2_transformado_residuos, pnorm)
"""
	One-sample Kolmogorov-Smirnov test

data:  modelo_reducido_final_pruebas_2_2_transformado_residuos
D = 0.081426, p-value = 0.009942                #Mejora considerablemente el p-valor, pero sigue siendo insuficiente. 
alternative hypothesis: two-sided
"""









#TRATAMIENTO DE LAS VARIABLES CUALITATIVAS DEL MODELO 2: 


"""
La idea es hacer para cada una de las categorías de la variable cualitativa
establecer una variable binaria.

Transformamos la variable categórica en variable dummy, y luego analizamos el
efecto de esa variable categórica sobre Sales.
"""


modelo_complete <- lm(Sales ~ Price + CompPrice + Advertising + 
                        Age + Income + Population + Education, data = Carseats)
summary(modelo_complete)


"""
Call:
lm(formula = Sales ~ Price + CompPrice + Advertising + Age + 
    Income + Population + Education, data = Carseats)

Residuals:
    Min      1Q  Median      3Q     Max 
-5.0598 -1.3515 -0.1739  1.1331  4.8304 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  7.7076934  1.1176260   6.896 2.15e-11 ***
Price       -0.0925226  0.0050521 -18.314  < 2e-16 ***
CompPrice    0.0939149  0.0078395  11.980  < 2e-16 ***
Advertising  0.1308637  0.0151219   8.654  < 2e-16 ***
Age         -0.0449743  0.0060083  -7.485 4.75e-13 ***
Income       0.0128717  0.0034757   3.703 0.000243 ***
Population  -0.0001239  0.0006877  -0.180 0.857092     #Los signos negativos en la columna Estimate denotan que influyen (para mal)
Education   -0.0399844  0.0371257  -1.077 0.282142     #esas variables sobre Sales, es decir, estaría asociado con una disminución
---                                                    # en Sales.
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.929 on 392 degrees of freedom
Multiple R-squared:  0.5417,	Adjusted R-squared:  0.5335 
F-statistic: 66.18 on 7 and 392 DF,  p-value: < 2.2e-16
"""

#ShelveLoc tiene 3 niveles, pero vamos a considerar solo ShelveLocGood y ShelveLocMedium que infiere R y que resumen muy bien ShelveLoc:


#Interpretamos esas variables porque son las significativas para el modelo, Urban y US 
#quedan descartadas. Dado que la variable cualitativa ShelveLoc tiene 3 niveles, se ne-
#cesitarán 2 variables dummys para resumir la variable cualitativa.

#CASO 1: Todas las variables cuantitativas: 


ventas <- 7.7076934 + (-0.0925226)*Price + 0.0939149*CompPrice + 0.1308637*Advertising 
+ (-0.0449743)*Age + 0.0128717*Income + (-0.0001239)*Population + (-0.0399844)*Education

#Quitamos Population y Education al no ser significativas para el modelo: 
modelo_complete2 <- lm(Sales ~ Price + CompPrice + Advertising + 
                         Age + Income, data = Carseats)
summary(modelo_complete2)

"""
Call:
lm(formula = Sales ~ Price + CompPrice + Advertising + Age + 
    Income, data = Carseats)

Residuals:
    Min      1Q  Median      3Q     Max 
-4.9071 -1.3081 -0.1892  1.1495  4.6980 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  7.109190   0.943940   7.531 3.46e-13 ***
Price       -0.092543   0.005044 -18.347  < 2e-16 ***
CompPrice    0.093904   0.007792  12.051  < 2e-16 ***
Advertising  0.130611   0.014572   8.963  < 2e-16 ***
Age         -0.044971   0.005994  -7.503 4.20e-13 ***
Income       0.013092   0.003465   3.779 0.000182 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.927 on 394 degrees of freedom
Multiple R-squared:  0.5403,	Adjusted R-squared:  0.5345         #Mejora un poco el R^2, y además menos variables, luego mejorará AIC y BIC.
F-statistic: 92.62 on 5 and 394 DF,  p-value: < 2.2e-16     #Además mejora el F-statistic haciéndolo más grande. 
"""


ventas2 <- 7.7076934 + (-0.0925226)*Price + 0.0939149*CompPrice + 0.1308637*Advertising 
+ (-0.0449743)*Age + 0.0128717*Income



#Incluimos ShelveLoc: MEJORA LA R^2 y la F-statistic:

modelo_complete3 <- lm(Sales ~ Price + CompPrice + Advertising + 
                         Age + Income + Population + Education + ShelveLoc, data = Carseats)
summary(modelo_complete3)

"""
Call:
lm(formula = Sales ~ Price + CompPrice + Advertising + Age + 
    Income + Population + Education + ShelveLoc, data = Carseats)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.8403 -0.6846  0.0151  0.6702  3.3481 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)      5.6472014  0.5973864   9.453  < 2e-16 ***
Price           -0.0953854  0.0026726 -35.690  < 2e-16 ***
CompPrice        0.0929439  0.0041451  22.422  < 2e-16 ***
Advertising      0.1141447  0.0080123  14.246  < 2e-16 ***
Age             -0.0459891  0.0031817 -14.454  < 2e-16 ***
Income           0.0157321  0.0018426   8.538 3.09e-16 ***
Population       0.0002632  0.0003641   0.723    0.470    
Education       -0.0197355  0.0196387  -1.005    0.316    
ShelveLocGood    4.8350741  0.1527240  31.659  < 2e-16 ***
ShelveLocMedium  1.9553465  0.1255836  15.570  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.02 on 390 degrees of freedom
Multiple R-squared:  0.8725,	Adjusted R-squared:  0.8696 
F-statistic: 296.6 on 9 and 390 DF,  p-value: < 2.2e-16
"""
ventas3 <- 5.6472014 + (-0.0953854)*Price + 0.0929439*CompPrice + 0.1141447*Advertising
+ (-0.0459891)*Age + 0.0157321*Income + 4.8350741*ShelveLocGood + 1.9553465*ShelveLocMedium

#NOTA IMPORTANTE: Los p-valores utilizando summary PARA VARIABLES CUALITATIVAS pueden proporcionarnos conclusiones erróneas, es por 
#ello que utilizaremos la función anova(nuestro_modelo) en lugar de summary() para comprobar la significancia de las 
#variables cualitativas:
modelo_complete3 <- lm(Sales ~ Price + CompPrice + Advertising + 
                         Age + Income + Population + ShelveLoc, data = Carseats)
anova(modelo_complete3)

"""
Analysis of Variance Table

Response: Sales
             Df  Sum Sq Mean Sq  F value    Pr(>F)    
Price         1  630.03  630.03 605.7673 < 2.2e-16 ***
CompPrice     1  508.69  508.69 489.1028 < 2.2e-16 ***
Advertising   1  315.78  315.78 303.6165 < 2.2e-16 ***
Age           1  211.86  211.86 203.7020 < 2.2e-16 ***
Income        1   53.02   53.02  50.9742 4.609e-12 ***
Population    1    0.02    0.02   0.0185   0.89201    
ShelveLoc     2 1052.94  526.47 506.1956 < 2.2e-16 ***   #Se ve que es significativa esa variable cualitativa.
Residuals   390  405.62    1.04                       
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Como el p-valor es menor que 0.05, se concluye que hay evidencias para rechazar H_{0}, 
es decir, ShelveLoc es significativa. Otra forma para confirmarlo:




modelo_reducido_1 <- lm(Sales ~ Price + CompPrice + Advertising + 
                         Age + Income, data = Carseats)


modelo_complete4 <- lm(Sales ~ Price + CompPrice + Advertising + 
                         Age + Income + ShelveLoc, data = Carseats)

require(MASS)
data()
anova(modelo_reducido_1, modelo_complete4) #MÁS RECOMENDABLE.

"""


#como Population y Education NO son estadísticamente significativas al ser su p-valor mayor que 0.05,
#los quitamos y generamos el nuevo modelo:
modelo_complete4 <- lm(Sales ~ Price + CompPrice + Advertising + 
                         Age + Income + ShelveLoc, data = Carseats)
summary(modelo_complete4)

"""
Call:
lm(formula = Sales ~ Price + CompPrice + Advertising + Age + 
    Income + ShelveLoc, data = Carseats)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.7728 -0.6954  0.0282  0.6732  3.3292 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      5.475226   0.505005   10.84   <2e-16 ***
Price           -0.095319   0.002670  -35.70   <2e-16 ***
CompPrice        0.092571   0.004123   22.45   <2e-16 ***
Advertising      0.115903   0.007724   15.01   <2e-16 ***
Age             -0.046128   0.003177  -14.52   <2e-16 ***
Income           0.015785   0.001838    8.59   <2e-16 ***
ShelveLocGood    4.835675   0.152499   31.71   <2e-16 *** #Las dummys son significativas, luego hay que interpretarlas. 
ShelveLocMedium  1.951993   0.125375   15.57   <2e-16 *** #Las dummys son significativas, luego hay que interpretarlas.
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.019 on 392 degrees of freedom
Multiple R-squared:  0.872,	Adjusted R-squared:  0.8697  #Mejora mucho más el R^2 ajustado.
F-statistic: 381.4 on 7 and 392 DF,  p-value: < 2.2e-16   #Mejora mucho más el F-statistic. 
"""


#CASO 2: Recta de regresión tomando ShelveLoc: 

ventas_general <- 5.475226 + (-0.095319 )*Price + 0.092571*CompPrice + 0.115903*Advertising
+ (-0.046128)*Age + 0.015785*Income + 4.835675*ShelveLocGood + 1.951993*ShelveLocMedium

#Siguiendo la teoría, tenemos una variable cuantitativa: ShelveLoc y dos dummys:  D1 = ShelveLocGood y D2= ShelveLocGoodMedium:
#Sería Y = \beta_{0} + \beta{1}x_{1} + \beta{2}D_{1} + \beta{3}x_{1}D_{1} + \beta{4}D_{2} + \beta{5}x_{1}D_{2} + \varepsilon



#Modelo de los que toman ShelveLocGood. Luego ShelveLocGood = 1 y ShelveLocGoodMedium=0:
ventas_general_1 <- 5.475226 + (-0.095319 )*Price + 0.092571*CompPrice + 0.115903*Advertising
+ (-0.046128)*Age + 0.015785*Income + 4.835675*1 + 1.951993*0

#Así, el intercepto queda como: 5.475226 + 4.835675 = 10.3109, luego la ecuación final es: 
ventas_general_1_final <- 10.3109 + (-0.095319 )*Price + 0.092571*CompPrice + 0.115903*Advertising
+ (-0.046128)*Age + 0.015785*Income 




#Modelo de los que toman ShelveLocGoodMedium. Luego ShelveLocGood = 0 y ShelveLocGoodMedium=1:
ventas_general_2 <- 5.475226 + (-0.095319 )*Price + 0.092571*CompPrice + 0.115903*Advertising
+ (-0.046128)*Age + 0.015785*Income + 4.835675*0 + 1.951993*1

#Así, el intercepto queda como: 5.475226 + 1.951993 = 7.427219, luego la ecuación final es: 
ventas_general_2_final <- 7.427219 + (-0.095319 )*Price + 0.092571*CompPrice + 0.115903*Advertising
+ (-0.046128)*Age + 0.015785*Income 



#Modelo sin ShelveLoc. Luego ShelveLocGood = 0 y ShelveLocGoodMedium=0:

ventas_general_3 <- 5.475226 + (-0.095319 )*Price + 0.092571*CompPrice + 0.115903*Advertising
+ (-0.046128)*Age + 0.015785*Income + 4.835675*0 + 1.951993*0

#Así, el intercepto queda como: 5.475226 + 1.951993 = 7.427219, luego la ecuación final es:
ventas_general_3 <- 5.475226 + (-0.095319 )*Price + 0.092571*CompPrice + 0.115903*Advertising
+ (-0.046128)*Age + 0.015785*Income



"""
Se observa que solo cambia el intercepto y además en forma creciente, 
luego tomando esas dos dummys, ¡¡estamos vendiendo más que sin tomarlas!!
En concreto, tomando ShelveLocGood vemos un incremento hasta 10 y con
ShelveLocMedium vemos otro incremento, aunque menor, de hasta 7 ventas más.
"""



#PREGUNTA 1: ¿Son iguales las rectas? 


modelo_reducido_1 <- lm(Sales ~ Price + CompPrice + Advertising + 
                          Age + Income, data = Carseats)


modelo_complete4 <- lm(Sales ~ Price + CompPrice + Advertising + 
                         Age + Income + ShelveLoc, data = Carseats)

require(MASS)
data("Carseats")
anova(modelo_reducido_1, modelo_complete4)


"""
Analysis of Variance Table

Model 1: Sales ~ Price + CompPrice + Advertising + Age + Income
Model 2: Sales ~ Price + CompPrice + Advertising + Age + Income + ShelveLoc
  Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
1    394 1462.90                                  
2    392  407.39  2    1055.5 507.82 < 2.2e-16 ***   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


Como el p-valor es aproximadamente 0, se concluye que hay evidencias para rechazar H_{0}, 
es decir, las rectas NO son iguales. 
"""





"""
Ahora bien, quizá el estudio del intercepto no sea suficiente, pues si se diera el caso de que
las rectas tienen misma pendiente, significaría que 
ShelveLocGood y ShelveLocMedium es igual de válido para las demás variables eplicativas. Por 
poner un ejemplo más sencillo,  si tuviéramos
un modelo donde hay alumnos y alumnos_tercera_matrícula y una variable cualitativa seminario_Álgebra y variable explicativa
que tome o no ese curso. Queremos ver como se relacionan las variables anteriores con las calificaciones
en la asignatura Ecuaciones Algebraicas:
Si tuviéramos en nuestro modelo pendientes iguales (supongamos que con mejoría de calificación), eso significaría que 
el efecto positivo de los alumnos que  toman el curso (la pendiente) sería igual para todo tipo de alumnos, 
pero esto quizá no sea tan realista pues alumnos_tercera_matrícula puede que tengan más experiencia en la asignatura que
los que toman el curso. Por tanto, quizá sea más realista un modelo con rectas secantes, esperando así que el curso 
tenga más efecto positivo sobre alumnos (primera matrícula que toman el curso) y menos sobre alumnos_tercera_matricula.

"""


#PREGUNTA 2: ¿Son las pendientes iguales?

anova(modelo_complete4)

"""
Analysis of Variance Table

Response: Sales
             Df  Sum Sq Mean Sq F value    Pr(>F)    
Price         1  630.03  630.03 606.234 < 2.2e-16 ***
CompPrice     1  508.69  508.69 489.480 < 2.2e-16 ***
Advertising   1  315.78  315.78 303.851 < 2.2e-16 ***
Age           1  211.86  211.86 203.859 < 2.2e-16 ***
Income        1   53.02   53.02  51.014 4.494e-12 ***
ShelveLoc     2 1055.51  527.76 507.822 < 2.2e-16 ***  #El p-valor es menor que 0.05, luego las dos rectas no tienen la misma pendiente.
Residuals   392  407.39    1.04                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

"""





#PREGUNTA 3: ¿Son los interteptos iguales? Se respondió en CASO 2.
#Otra forma sería utilizando el ANOVA y viendo el p-valor.










































####

#VALIDACION DEL MODELO 2:
#x) Crear y validar modelos de predicción para Sales usando los datos de Carseats.

install.packages("ISLR")
library(ISLR)
install.packages("leaps")
library(leaps)
install.packages("PASWR")
library(PASWR) 

modelo_reducido <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats)
summary(modelo_reducido)
set.seed(5) #semilla
train <- sample (c(TRUE, FALSE), size=nrow(Carseats[1:6]),
                 replace=TRUE, prob=c(0.70,0.30)) #conjunto de entenamiento
prop.table(table(train))#calcula los percentiles en train
test <- (!train)
prop.table(table(test)) #calcula los percentiles en test

#Usando la aproximación al conjunto de validación elegir el mejor modelo de
#regresión obtenido con la función regsubsets() cuando Sales es la variable
#respuesta y ShelveLoc, Price, Compprice, Advertising, Age e Income las variables explicativas. Usar
#la semilla set.seed=5 y dividir los datos disponibles en conjunto de entrenamiento
#y tst donde aproximadamente el 70% de los datos disponibles se usan
#para el entrenamiento y el resto se usa para el conjunto de test.

model.exh <- regsubsets(Sales ~ShelveLoc + Price + CompPrice + Advertising + Age + Income , data = Carseats[train, 1:8] , method= "exhaustive")
summary(model.exh) #todos los modelos posibles para los `predictores
#vamos a calcular el error del conjunto de validación para el  mejor modelo de entre los
#obtenidos antes
predict.regsubsets <- function(object, newdata, id,...){
  form <-as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}
val.errors <- rep(NA,7)
Y <- Carseats[test,]$Sales
for (i in 1:7){
  Yhat <- predict.regsubsets (model.exh, newdata=Carseats[test,], id=i)
  val.errors[i] <- mean((Y-Yhat)^2)
}
val.errors
coef(model.exh, which.min(val.errors))


#otra forma
regfit.best <-regsubsets(Sales~ShelveLoc + Price + CompPrice + Advertising + Age + Income, data=Carseats[train, 1:8])
coef( regfit.best, which.min(val.errors))

#Usando validación cruzada dejando uno fuera elegir el mejor modelo de regresi
#ón de entre los que obtenemos con la función regsubsets cuando cuando
#SALES es la variable respuesta y ShelveLoc, Price, CompPrice, Advertising, Age e Income son  las variables
#explicativas.
##LOOCV

modelo_reducido <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats)
summary(modelo_reducido)

n <- nrow(Carseats)
k <- n #número de grupos igual a n
set.seed(5)
folds <- sample(x=1:k, size =nrow(Carseats), replace = FALSE)
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL,paste(1:7)))
for (j in 1:k){
  best.fit <- regsubsets(Sales~ShelveLoc + Price + CompPrice + Advertising + Age + Income, data=Carseats[folds !=j,])#cojemos datos del conjunto de entrenamiento
  for (i in 1:7){
    pred <- predict.regsubsets(best.fit, newdata=Carseats[folds==j,], id=i)#datos de test
    cv.errors[j,i] <- mean((Carseats$Sales[folds == j]-pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)#calcula la media de los betas_i
mean.cv.errors

#x3)Usando validación cruzada con 5 grupos elegir el mejor modelo de regresi´on de
#entre los que obtenemos con la funci´on regsubsets cuando cuando Sales es la
#variable respuesta y ShelveLoc, Price, CompPrice, Advertising, Age, e Income las variables explicativas.
## VALIDACIÓN CRUZADA k-folds
modelo_reducido <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats)
summary(modelo_reducido)
n <- nrow(Carseats)
k <- 10 #número de grupos igual a n
set.seed(5)
folds <- sample(x=1:k, size =nrow(Carseats), replace = FALSE)
cv.errors <- matrix(NA, k, 7, dimnames = list(NULL,paste(1:7)))
for (j in 1:k){
  best.fit <- regsubsets(Sales~ShelveLoc + Price + CompPrice + Advertising + Age + Income, data=Carseats[folds !=j,])
  for (i in 1:7){
    pred <- predict.regsubsets(best.fit, newdata=Carseats[folds==j,], id=i)
    cv.errors[j,i] <- mean((Carseats$Sales[folds == j]-pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
coef(regfit.best, which.min(mean.cv.errors))
install.packages("car")
library(car)

#comprobación grafica
model.cv <- lm(Sales~ShelveLoc + Price + CompPrice + Advertising + Age + Income, data=Carseats)
summary(model.cv)
plot(lm(Sales~ShelveLoc + Price + CompPrice + Advertising + Age + Income, data=Carseats))
plot(lm(Sales~ShelveLoc + Price + CompPrice + Advertising + Age + Income, data=Carseats), which=c(1,2))
residualPlot(model.cv)
influenceIndexPlot(model.cv)
####


###
#VALIDACION SIMPLE: CON TODAS LAS VARIABLES EXPLICATIVAS:



install.packages("ISLR")
library(ISLR)
install.packages("leaps")
library(leaps)
install.packages("PASWR")
library(PASWR) 

modelo_reducido <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats)
summary(modelo_reducido)
set.seed(5) #semilla
train <- sample (c(TRUE, FALSE), size=nrow(Carseats[1:6]),
                 replace=TRUE, prob=c(0.70,0.30)) #conjunto de entenamiento
prop.table(table(train))#calcula los percentiles en train
test <- (!train)
prop.table(table(test)) #calcula los percentiles en test

#Usando la aproximación al conjunto de validación elegir el mejor modelo de
#regresión obtenido con la función regsubsets() cuando Sales es la variable
#respuesta y ShelveLoc, Price, Compprice, Advertising, Age e Income las variables explicativas. Usar
#la semilla set.seed=5 y dividir los datos disponibles en conjunto de entrenamiento
#y tst donde aproximadamente el 70% de los datos disponibles se usan
#para el entrenamiento y el resto se usa para el conjunto de test.

model.exh <- regsubsets(Sales ~. , data = Carseats[train, 1:8] , method= "exhaustive")
summary(model.exh) #todos los modelos posibles para los `predictores
#vamos a calcular el error del conjunto de validación para el  mejor modelo de entre los
#obtenidos antes
predict.regsubsets <- function(object, newdata, id,...){
  form <-as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}
val.errors <- rep(NA,8)
Y <- Carseats[test,]$Sales
for (i in 1:8){
  Yhat <- predict.regsubsets (model.exh, newdata=Carseats[test,], id=i)
  val.errors[i] <- mean((Y-Yhat)^2)
}
val.errors
coef(model.exh, which.min(val.errors))


#otra forma
regfit.best <-regsubsets(Sales~., data=Carseats[train, 1:8])
coef( regfit.best, which.min(val.errors))

#Usando validación cruzada dejando uno fuera elegir el mejor modelo de regresi
#ón de entre los que obtenemos con la función regsubsets cuando cuando
#SALES es la variable respuesta y ShelveLoc, Price, CompPrice, Advertising, Age e Income son  las variables
#explicativas.
##LOOCV

modelo_reducido <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats)
summary(modelo_reducido)

n <- nrow(Carseats)
k <- n #número de grupos igual a n
set.seed(5)
folds <- sample(x=1:k, size =nrow(Carseats), replace = FALSE)
cv.errors <- matrix(NA, k, 8, dimnames = list(NULL,paste(1:8)))
for (j in 1:k){
  best.fit <- regsubsets(Sales~., data=Carseats[folds !=j,]) #cogemos datos del conjunto de entrenamiento
  for (i in 1:8){
    pred <- predict.regsubsets(best.fit, newdata=Carseats[folds==j,], id=i)#datos de test
    cv.errors[j,i] <- mean((Carseats$Sales[folds == j]-pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)#calcula la media de los betas_i
mean.cv.errors
coef( regfit.best, which.min(mean.cv.errors))
#x3)Usando validación cruzada con 5 grupos elegir el mejor modelo de regresión de
#entre los que obtenemos con la función regsubsets cuando cuando Sales es la
#variable respuesta y ShelveLoc, Price, CompPrice, Advertising, Age, e Income las variables explicativas.
## VALIDACIÓN CRUZADA k-folds
modelo_reducido <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats)
summary(modelo_reducido)
n <- nrow(Carseats)
k <- 10 #número de grupos igual a n
set.seed(5)
folds <- sample(x=1:k, size =nrow(Carseats), replace = FALSE)
cv.errors <- matrix(NA, k, 8, dimnames = list(NULL,paste(1:8)))
for (j in 1:k){
  best.fit <- regsubsets(Sales~., data=Carseats[folds !=j,])
  for (i in 1:8){
    pred <- predict.regsubsets(best.fit, newdata=Carseats[folds==j,], id=i)
    cv.errors[j,i] <- mean((Carseats$Sales[folds == j]-pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
coef(regfit.best, which.min(mean.cv.errors))
install.packages("car")
library(car)

#comprobación grafica
model.cv <- lm(Sales~ShelveLoc + Price + CompPrice + Advertising + Age + Income, data=Carseats)
summary(model.cv)
plot(lm(Sales~ShelveLoc + Price + CompPrice + Advertising + Age + Income, data=Carseats))
plot(lm(Sales~ShelveLoc + Price + CompPrice + Advertising + Age + Income, data=Carseats), which=c(1,2))
residualPlot(model.cv)
influenceIndexPlot(model.cv)
####


#Estimación para la respuesta media para nuevos valores: 

modelo_reducido <- lm(Sales ~ ShelveLoc + Price + CompPrice + Advertising + Age + Income, data = Carseats)
summary(modelo_reducido)


nuevos_datos <- data.frame(ShelveLoc, Price=180, CompPrice=150, Advertising=10, Age=60, Income=120)
prediccion_datos <- predict(object=modelo_reducido, newdata=nuevos_datos)
View(prediccion_datos)


#Calculamos el IC al 95% para E[Y|ShelveLoc + Price + CompPrice + Advertising + Age + Income]
#para el nuevo conjunto de datos siguiente: 

#Intervalo para la respuesta media: 
nuevos_datos <- data.frame(ShelveLoc, Price=180, CompPrice=150, Advertising=10, Age=60, Income=120)
prediccion_media <- predict(object=modelo_reducido, newdata=nuevos_datos, interval="confidence", level=0.95)

#Intervalo de predicción: 
nuevos_datos <- data.frame(ShelveLoc, Price=180, CompPrice=150, Advertising=10, Age=60, Income=120)
prediccion_datos <- predict(object=modelo_reducido, newdata=nuevos_datos, interval="prediction", level=0.95)
prediccion_datos

nuevos_datos_2 <- cbind(nuevos_datos, prediccion_datos)

