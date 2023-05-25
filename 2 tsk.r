#Ерохов Юрий, Д-А 131
# Ерохов Ю.М — создайте модель множественной линейной регрессии ночных потоков
#углекислого газа за осенний период 2013 года по данным измерений методом турбулентной пульсации

setwd("F:/R2z")

library(tidyverse)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)


eddypro = read_csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"),
                   comment=c("["))

eddypro = eddypro[-1, ]

eddypro = select(eddypro, -(roll))

names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]","_")

#Отфильтруем по заданию данные только за осенний период. С начала сентября (244) по конец августа (336)
eddypro = filter(eddypro,DOY >= 244 & DOY < 336)

#Отфильтруем данные по заданию только за ночное время 
eddypro = filter(eddypro, daytime == FALSE)

#Преобразуем переменные типа char в факторы
eddypro = eddypro %>% mutate_if(is.character, as.factor)

# нам необходимо получить все переменные были типа numeric, 
# воспользуемся функциями saplly и is.numeric
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
str(eddypro_numeric)

# посмотрим коэф корреляции 
cor_eddy = cor(eddypro_numeric)
str(cor_eddy)

# НАдо убрать NA
# Посчитаем сколько всего и удалим
# воспользуемся функцией summarise_all и sum
na_cor_eddy = eddypro_numeric %>% summarise_all(~sum(is.na(.x)))
navect = na_cor_eddy[1,]%>% as.integer()

# посмотрим у каких переменных кол-во NA превышает 30
names(eddypro_numeric)[navect>30]

# исключим все na
eddypro_numeric = na.exclude(eddypro_numeric)

# снова посчитаем коэф корреляции
cor_eddy = cor(eddypro_numeric)
cor_eddy = data.frame(cor_eddy)

#найдем коэф детерминации для нашей зависимой переменной
cor_vars=cor_eddy$co2_flux^2
names(cor_vars)=names(cor_eddy)

# значимые коэффициенты, в которых коэф детерминации более 0,12
cor_vars=cor_vars[cor_vars>0.12]

# значимые переменные
names(cor_vars)%>% na.exclude()

## МНОЖЕСТВЕННАЯ РЕГРЕССИЯ

# Построим модель по известным нам значимым переменным 
mod1 = lm(data = eddypro_numeric, co2_flux ~ DOY + h2o_molar_density + h2o_mole_fraction
          + h2o_mixing_ratio + sonic_temperature + air_temperature + air_pressure + air_density 
          + air_heat_capacity + air_molar_volume + water_vapor_density + e + es +  specific_humidity
          + Tdew + un_co2_flux + w_slash_co2_cov + h2o...126 + h2o...128 + flowrate)

# коэф/дов интервал/остатки
coef(mod1)
resid(mod1)
confint(mod1)

# посмотрим р-значения по модели
summary(mod1)
# коэффициент детерминации = 0,9484

# ДА
anova(mod1)

## Построим графиик нормального распределения:
plot(mod1,2)
# в целом данные распределены нормально
# Построим график наблюдаемых значений от предсказанных значений
plot(mod1$fitted.values, eddypro_numeric$co2_flux)
# Добавим линию у = х
abline(a=0, b=1, col = "purple")
# почти все точки лежат на прямой,
# значит модель хорошо оценивает данные дневных потоков СО2

# Построим график зависимости остатков от наблюдаемых значений 
plot(eddypro_numeric$co2_flux,mod1$residuals)
# Для поиска коэффициентов для линии зададим модель, связывающую остатки и CO2
mo1 = lm(mod1$residuals ~ eddypro_numeric$co2_flux)
abline(a = mo1$coefficients[1], b = mo1$coefficients[2],col = "purple")
# наблюдается зависимость остатков от наблюдаемых значений 

#МОДЕЛЬ 2 
# построим аналогичную модель при этом будем искать зависимость между переменными 
# второго порядка 
mod2 = lm(data = eddypro_numeric, co2_flux~(DOY + h2o_molar_density + h2o_mole_fraction
                                            + h2o_mixing_ratio + sonic_temperature + air_temperature + air_pressure + air_density 
                                            + air_heat_capacity + air_molar_volume + water_vapor_density + e + es +  specific_humidity
                                            + Tdew + un_co2_flux + w_slash_co2_cov + h2o...126 + h2o...128 + flowrate)^2)

# помострим коэффициенты, остатки и доверительный интервал 
coef(mod2)
resid(mod2)
confint(mod2)

#P-значения по модели
summary(mod2)
# коэффициент детерминации = 0,9592

#ДА
anova(mod2)

# из дисперс анализа понятно какие переменные не значимые

#график на нормальной веротяностной бумаге:
plot(mod2,2) 
# график наблюдаемых значений от предсказанных значений
plot(mod2$fitted.values, eddypro_numeric$co2_flux)
# Добавим линию у=х
abline(a=0, b=1, col="red")
# график остатков от набоюдаемых значений 
plot(eddypro_numeric$co2_flux,mod2$residuals)
# Для поиска коэффициентов для линии зададим модель, связывающую остатки и CO2
mo2=lm(mod2$residuals~eddypro_numeric$co2_flux)
abline(a=mo2$coefficients[1],b=mo2$coefficients[2],col="red")

#МОДЕЛЬ 3 
# так как незначимые 1 порядка могут иметь связи с перемнными 2-го порядка, мы их не искоючаем

mod3 = lm (data = eddypro_numeric, co2_flux ~ ((DOY + h2o_molar_density + h2o_mole_fraction
                                                + h2o_mixing_ratio + sonic_temperature + air_temperature + air_pressure + air_density 
                                                + air_heat_capacity + air_molar_volume + water_vapor_density + e + es +  specific_humidity
                                                + Tdew + un_co2_flux + w_slash_co2_cov + h2o...126 + h2o...128 + flowrate)^2) - e - es - flowrate 
           - DOY:h2o_molar_density - DOY:h2o_mole_fraction - DOY:h2o_mixing_ratio - DOY:sonic_temperature - DOY:air_temperature - DOY:air_pressure
           - DOY:air_density - DOY:air_heat_capacity - DOY:air_molar_volume - DOY:water_vapor_density - DOY:e -DOY:es - DOY:Tdew - DOY:un_co2_flux
           - DOY:h2o...126 - DOY:h2o...128 - DOY:flowrate - h2o_molar_density:h2o_mole_fraction - h2o_molar_density:h2o_mixing_ratio - h2o_molar_density:sonic_temperature
           - h2o_molar_density:air_pressure - h2o_molar_density:air_density - h2o_molar_density:air_heat_capacity - h2o_molar_density:air_molar_volume
           - h2o_molar_density:water_vapor_density - h2o_molar_density:e - h2o_molar_density:un_co2_flux - h2o_molar_density:w_slash_co2_cov - h2o_molar_density:h2o...128
           - h2o_molar_density:flowrate - h2o_mole_fraction:water_vapor_density - h2o_mole_fraction:e - h2o_mole_fraction:es - h2o_mole_fraction:Tdew
           - h2o_mole_fraction:un_co2_flux - h2o_mole_fraction:flowrate - h2o_mixing_ratio:water_vapor_density - h2o_mixing_ratio:e - h2o_mixing_ratio:es
           - h2o_mixing_ratio:un_co2_flux - h2o_mixing_ratio:w_slash_co2_cov - h2o_mixing_ratio:flowrate - sonic_temperature:air_temperature
           - sonic_temperature:e - sonic_temperature:es - sonic_temperature:es - sonic_temperature:un_co2_flux - sonic_temperature:w_slash_co2_cov 
           - sonic_temperature:h2o...126 - sonic_temperature:flowrate - air_temperature:un_co2_flux - air_pressure:air_density - air_pressure:water_vapor_density
           - air_pressure:un_co2_flux - air_pressure:h2o...126 - air_pressure:flowrate - air_density:es - air_density:un_co2_flux - air_heat_capacity:es
           - air_molar_volume:es - water_vapor_density:e - water_vapor_density:es - water_vapor_density:un_co2_flux - air_heat_capacity:es - air_molar_volume:es 
           - water_vapor_density:e - water_vapor_density:es - water_vapor_density:un_co2_flux - water_vapor_density:h2o...126 - water_vapor_density:flowrate
           - e:es - e:un_co2_flux - e:flowrate - es:Tdew - es:un_co2_flux - es:h2o...126 - es:h2o...128 - es:flowrate - Tdew:un_co2_flux - Tdew:w_slash_co2_cov
           - Tdew:flowrate - un_co2_flux:w_slash_co2_cov - un_co2_flux:h2o...126 - un_co2_flux:h2o...128 - w_slash_co2_cov:h2o...126 - w_slash_co2_cov:flowrate 
           - h2o...126:flowrate)


#Коэффициенты, остаткиБ доверительный интервал
coef(mod3)
resid(mod3)
confint(mod3)

#P-значения по модели
summary(mod3)
# коефф 0,9549,это меньше чем в моде 2 
# мод два более полная 

#исперси
anova(mod3)
#Графиик на нормальной веротяностной бумаге :
plot(mod3,2) 
# график 
plot(mod3$fitted.values, eddypro_numeric$co2_flux)
# добавим линию у=х
abline(a=0, b=1, col="green")
# остатки 
plot(eddypro_numeric$co2_flux,mod3$residuals)
# новая модель
mo3=lm(mod3$residuals~eddypro_numeric$co2_flux)
abline(a=mo3$coefficients[1],b=mo3$coefficients[2],col="green")

#МОДЕЛЬ 4
# незначимые переменные убрать

mod4 = lm (data = eddypro_numeric, co2_flux ~ ((DOY + h2o_molar_density + h2o_mole_fraction
                                                 + h2o_mixing_ratio + sonic_temperature + air_temperature + air_pressure + air_density 
                                                 + air_heat_capacity + air_molar_volume + water_vapor_density + e + es +  specific_humidity
                                                 + Tdew + un_co2_flux + w_slash_co2_cov + h2o...126 + h2o...128 + flowrate)^2) - e - es - flowrate 
                                               - DOY:h2o_molar_density - DOY:h2o_mole_fraction - DOY:h2o_mixing_ratio - DOY:sonic_temperature - DOY:air_temperature - DOY:air_pressure
                                               - DOY:air_density - DOY:air_heat_capacity - DOY:air_molar_volume - DOY:water_vapor_density - DOY:e -DOY:es - DOY:Tdew - DOY:un_co2_flux
                                               - DOY:h2o...126 - DOY:h2o...128 - DOY:flowrate - h2o_molar_density:h2o_mole_fraction - h2o_molar_density:h2o_mixing_ratio - h2o_molar_density:sonic_temperature
                                               - h2o_molar_density:air_pressure - h2o_molar_density:air_density - h2o_molar_density:air_heat_capacity - h2o_molar_density:air_molar_volume
                                               - h2o_molar_density:water_vapor_density - h2o_molar_density:e - h2o_molar_density:un_co2_flux - h2o_molar_density:w_slash_co2_cov - h2o_molar_density:h2o...128
                                               - h2o_molar_density:flowrate - h2o_mole_fraction:water_vapor_density - h2o_mole_fraction:e - h2o_mole_fraction:es - h2o_mole_fraction:Tdew
                                               - h2o_mole_fraction:un_co2_flux - h2o_mole_fraction:flowrate - h2o_mixing_ratio:water_vapor_density - h2o_mixing_ratio:e - h2o_mixing_ratio:es
                                               - h2o_mixing_ratio:un_co2_flux - h2o_mixing_ratio:w_slash_co2_cov - h2o_mixing_ratio:flowrate - sonic_temperature:air_temperature
                                               - sonic_temperature:e - sonic_temperature:es - sonic_temperature:es - sonic_temperature:un_co2_flux - sonic_temperature:w_slash_co2_cov 
                                               - sonic_temperature:h2o...126 - sonic_temperature:flowrate - air_temperature:un_co2_flux - air_pressure:air_density - air_pressure:water_vapor_density
                                               - air_pressure:un_co2_flux - air_pressure:h2o...126 - air_pressure:flowrate - air_density:es - air_density:un_co2_flux - air_heat_capacity:es
                                               - air_molar_volume:es - water_vapor_density:e - water_vapor_density:es - water_vapor_density:un_co2_flux - air_heat_capacity:es - air_molar_volume:es 
                                               - water_vapor_density:e - water_vapor_density:es - water_vapor_density:un_co2_flux - water_vapor_density:h2o...126 - water_vapor_density:flowrate
                                               - e:es - e:un_co2_flux - e:flowrate - es:Tdew - es:un_co2_flux - es:h2o...126 - es:h2o...128 - es:flowrate - Tdew:un_co2_flux - Tdew:w_slash_co2_cov
                                               - Tdew:flowrate - un_co2_flux:w_slash_co2_cov - un_co2_flux:h2o...126 - un_co2_flux:h2o...128 - w_slash_co2_cov:h2o...126 - w_slash_co2_cov:flowrate 
                                               - h2o...126:flowrate - DOY:specific_humidity - DOY:w_slash_co2_cov - h2o_molar_density:air_temperature - h2o_molar_density:specific_humidity - h2o_molar_density:Tdew
                                               - h2o_molar_density:h2o...126 - h2o_mole_fraction:sonic_temperature - h2o_mole_fraction:air_pressure - h2o_mole_fraction:air_heat_capacity - h2o_mole_fraction:air_molar_volume
                                               - h2o_mole_fraction:w_slash_co2_cov - h2o_mole_fraction:h2o...126 - h2o_mole_fraction:h2o...128 - h2o_mixing_ratio:air_pressure - sonic_temperature:air_molar_volume 
                                               - h2o_mole_fraction:air_heat_capacity - h2o_mole_fraction:w_slash_co2_cov - h2o_mole_fraction:h2o...126 - h2o_mole_fraction:h2o...128 - h2o_mixing_ratio:air_pressure
                                               - sonic_temperature:air_molar_volume - air_temperature:e - air_temperature:w_slash_co2_cov - air_temperature:h2o...126 - air_temperature:flowrate - air_pressure:e - air_pressure:es
                                               - air_density:water_vapor_density - air_density:w_slash_co2_cov - air_density:flowrate - air_heat_capacity:w_slash_co2_cov - air_heat_capacity:flowrate - air_molar_volume:w_slash_co2_cov
                                               - air_molar_volume:h2o...126 - air_molar_volume:flowrate - water_vapor_density:specific_humidity - e:w_slash_co2_cov - es:specific_humidity 
                                               - es:w_slash_co2_cov - specific_humidity:flowrate - un_co2_flux:flowrate - w_slash_co2_cov:h2o...128)
#Коэффициенты, остатки, доверительный интервал
coef(mod4)
resid(mod4)
confint(mod4)

#P-значения по модели
summary(mod4)
# коеффициент детерминации = 0.9525 - меньше чем во второй и 3 модели
# таким образом,  более полная модель 2 лучше отражает данные

#Дисперсионный анализ
anova(mod4)
#Графиик на нормальной веротяностной бумаге :
plot(mod4,2) 
# Построим график наблюдаемых значений от предсказанных значений
plot(mod4$fitted.values, eddypro_numeric$co2_flux)
# Добавим линию у=х
abline(a=0, b=1, col="blue")
# Построим график остатков от набоюдаемых значений 
plot(eddypro_numeric$co2_flux,mod4$residuals)
# Для поиска коэффициентов для линии зададим модель, связывающую остатки и CO2
mo4=lm(mod4$residuals~eddypro_numeric$co2_flux)
abline(a=mo4$coefficients[1],b=mo4$coefficients[2],col="blue")

#МОДЕЛЬ 5
# на основании ДА 4 модели уберем незначимые переменные

mod5 = lm (data = eddypro_numeric, co2_flux~ ((DOY + h2o_molar_density + h2o_mole_fraction
                                              + h2o_mixing_ratio + sonic_temperature + air_temperature + air_pressure + air_density 
                                              + air_heat_capacity + air_molar_volume + water_vapor_density + e + es +  specific_humidity
                                              + Tdew + un_co2_flux + w_slash_co2_cov + h2o...126 + h2o...128 + flowrate)^2) - e - es - flowrate 
- DOY:h2o_molar_density - DOY:h2o_mole_fraction - DOY:h2o_mixing_ratio - DOY:sonic_temperature - DOY:air_temperature - DOY:air_pressure
- DOY:air_density - DOY:air_heat_capacity - DOY:air_molar_volume - DOY:water_vapor_density - DOY:e -DOY:es - DOY:Tdew - DOY:un_co2_flux
- DOY:h2o...126 - DOY:h2o...128 - DOY:flowrate - h2o_molar_density:h2o_mole_fraction - h2o_molar_density:h2o_mixing_ratio - h2o_molar_density:sonic_temperature
- h2o_molar_density:air_pressure - h2o_molar_density:air_density - h2o_molar_density:air_heat_capacity - h2o_molar_density:air_molar_volume
- h2o_molar_density:water_vapor_density - h2o_molar_density:e - h2o_molar_density:un_co2_flux - h2o_molar_density:w_slash_co2_cov - h2o_molar_density:h2o...128
- h2o_molar_density:flowrate - h2o_mole_fraction:water_vapor_density - h2o_mole_fraction:e - h2o_mole_fraction:es - h2o_mole_fraction:Tdew
- h2o_mole_fraction:un_co2_flux - h2o_mole_fraction:flowrate - h2o_mixing_ratio:water_vapor_density - h2o_mixing_ratio:e - h2o_mixing_ratio:es
- h2o_mixing_ratio:un_co2_flux - h2o_mixing_ratio:w_slash_co2_cov - h2o_mixing_ratio:flowrate - sonic_temperature:air_temperature
- sonic_temperature:e - sonic_temperature:es - sonic_temperature:es - sonic_temperature:un_co2_flux - sonic_temperature:w_slash_co2_cov 
- sonic_temperature:h2o...126 - sonic_temperature:flowrate - air_temperature:un_co2_flux - air_pressure:air_density - air_pressure:water_vapor_density
- air_pressure:un_co2_flux - air_pressure:h2o...126 - air_pressure:flowrate - air_density:es - air_density:un_co2_flux - air_heat_capacity:es
- air_molar_volume:es - water_vapor_density:e - water_vapor_density:es - water_vapor_density:un_co2_flux - air_heat_capacity:es - air_molar_volume:es 
- water_vapor_density:e - water_vapor_density:es - water_vapor_density:un_co2_flux - water_vapor_density:h2o...126 - water_vapor_density:flowrate
- e:es - e:un_co2_flux - e:flowrate - es:Tdew - es:un_co2_flux - es:h2o...126 - es:h2o...128 - es:flowrate - Tdew:un_co2_flux - Tdew:w_slash_co2_cov
- Tdew:flowrate - un_co2_flux:w_slash_co2_cov - un_co2_flux:h2o...126 - un_co2_flux:h2o...128 - w_slash_co2_cov:h2o...126 - w_slash_co2_cov:flowrate 
- h2o...126:flowrate - DOY:specific_humidity - DOY:w_slash_co2_cov - h2o_molar_density:air_temperature - h2o_molar_density:specific_humidity - h2o_molar_density:Tdew
- h2o_molar_density:h2o...126 - h2o_mole_fraction:sonic_temperature - h2o_mole_fraction:air_pressure - h2o_mole_fraction:air_heat_capacity - h2o_mole_fraction:air_molar_volume
- h2o_mole_fraction:w_slash_co2_cov - h2o_mole_fraction:h2o...126 - h2o_mole_fraction:h2o...128 - h2o_mixing_ratio:air_pressure - sonic_temperature:air_molar_volume 
- h2o_mole_fraction:air_heat_capacity - h2o_mole_fraction:w_slash_co2_cov - h2o_mole_fraction:h2o...126 - h2o_mole_fraction:h2o...128 - h2o_mixing_ratio:air_pressure
- sonic_temperature:air_molar_volume - air_temperature:e - air_temperature:w_slash_co2_cov - air_temperature:h2o...126 - air_temperature:flowrate - air_pressure:e - air_pressure:es
- air_density:water_vapor_density - air_density:w_slash_co2_cov - air_density:flowrate - air_heat_capacity:w_slash_co2_cov - air_heat_capacity:flowrate - air_molar_volume:w_slash_co2_cov
- air_molar_volume:h2o...126 - air_molar_volume:flowrate - water_vapor_density:specific_humidity - e:w_slash_co2_cov - es:specific_humidity 
- es:w_slash_co2_cov - specific_humidity:flowrate - un_co2_flux:flowrate - w_slash_co2_cov:h2o...128 - h2o_mole_fraction:air_temperature - h2o_mole_fraction:air_density - h2o_mixing_ratio:air_heat_capacity
- h2o_mixing_ratio:air_molar_volume - h2o_mixing_ratio:h2o...126 - h2o_mixing_ratio:h2o...128 - sonic_temperature:air_pressure - sonic_temperature:water_vapor_density - sonic_temperature:Tdew 
- air_temperature:air_molar_volume - air_temperature:es - air_pressure:Tdew - air_pressure:w_slash_co2_cov - air_density:h2o...126 - air_heat_capacity:un_co2_flux - water_vapor_density:w_slash_co2_cov 
-water_vapor_density:h2o...128 - e:specific_humidity - e:h2o...126 - specific_humidity:un_co2_flux - specific_humidity:h2o...126 - Tdew:h2o...126 - h2o...128:flowrate)  

#Коэффициенты, остатки и довертельный интервал 
coef(mod5)
resid(mod5)
confint(mod5)

#P-значения по модели
summary(mod5)
# коеффициент детерминации = 0.9513 - меньше чем предыдущие
# таким образом, более полная модель 2 лучше отражает данные

#ДА
anova(mod5)
#Графиик на нормальной веротяностной бумаге :
plot(mod5,2) 
# Построим график наблюдаемых значений от предсказанных значений
plot(mod5$fitted.values, eddypro_numeric$co2_flux)
# Добавим линию у=х
abline(a=0, b=1, col="orange")
# Построим график остатков от набоюдаемых значений 
plot(eddypro_numeric$co2_flux,mod5$residuals)
# Для поиска коэффициентов для линии зададим модель, связывающую остатки и CO2
mo5=lm(mod5$residuals~eddypro_numeric$co2_flux)
abline(a=mo5$coefficients[1],b=mo5$coefficients[2],col="orange")


#МОДЕЛЬ 6
# на основании ДА 5 модели уберем незначимые переменные
mod6 = lm (data = eddypro_numeric, co2_flux~ ((DOY + h2o_molar_density + h2o_mole_fraction
                                              + h2o_mixing_ratio + sonic_temperature + air_temperature + air_pressure + air_density 
                                              + air_heat_capacity + air_molar_volume + water_vapor_density + e + es +  specific_humidity
                                              + Tdew + un_co2_flux + w_slash_co2_cov + h2o...126 + h2o...128 + flowrate)^2) - e - es - flowrate 
- DOY:h2o_molar_density - DOY:h2o_mole_fraction - DOY:h2o_mixing_ratio - DOY:sonic_temperature - DOY:air_temperature - DOY:air_pressure
- DOY:air_density - DOY:air_heat_capacity - DOY:air_molar_volume - DOY:water_vapor_density - DOY:e -DOY:es - DOY:Tdew - DOY:un_co2_flux
- DOY:h2o...126 - DOY:h2o...128 - DOY:flowrate - h2o_molar_density:h2o_mole_fraction - h2o_molar_density:h2o_mixing_ratio - h2o_molar_density:sonic_temperature
- h2o_molar_density:air_pressure - h2o_molar_density:air_density - h2o_molar_density:air_heat_capacity - h2o_molar_density:air_molar_volume
- h2o_molar_density:water_vapor_density - h2o_molar_density:e - h2o_molar_density:un_co2_flux - h2o_molar_density:w_slash_co2_cov - h2o_molar_density:h2o...128
- h2o_molar_density:flowrate - h2o_mole_fraction:water_vapor_density - h2o_mole_fraction:e - h2o_mole_fraction:es - h2o_mole_fraction:Tdew
- h2o_mole_fraction:un_co2_flux - h2o_mole_fraction:flowrate - h2o_mixing_ratio:water_vapor_density - h2o_mixing_ratio:e - h2o_mixing_ratio:es
- h2o_mixing_ratio:un_co2_flux - h2o_mixing_ratio:w_slash_co2_cov - h2o_mixing_ratio:flowrate - sonic_temperature:air_temperature
- sonic_temperature:e - sonic_temperature:es - sonic_temperature:es - sonic_temperature:un_co2_flux - sonic_temperature:w_slash_co2_cov 
- sonic_temperature:h2o...126 - sonic_temperature:flowrate - air_temperature:un_co2_flux - air_pressure:air_density - air_pressure:water_vapor_density
- air_pressure:un_co2_flux - air_pressure:h2o...126 - air_pressure:flowrate - air_density:es - air_density:un_co2_flux - air_heat_capacity:es
- air_molar_volume:es - water_vapor_density:e - water_vapor_density:es - water_vapor_density:un_co2_flux - air_heat_capacity:es - air_molar_volume:es 
- water_vapor_density:e - water_vapor_density:es - water_vapor_density:un_co2_flux - water_vapor_density:h2o...126 - water_vapor_density:flowrate
- e:es - e:un_co2_flux - e:flowrate - es:Tdew - es:un_co2_flux - es:h2o...126 - es:h2o...128 - es:flowrate - Tdew:un_co2_flux - Tdew:w_slash_co2_cov
- Tdew:flowrate - un_co2_flux:w_slash_co2_cov - un_co2_flux:h2o...126 - un_co2_flux:h2o...128 - w_slash_co2_cov:h2o...126 - w_slash_co2_cov:flowrate 
- h2o...126:flowrate - DOY:specific_humidity - DOY:w_slash_co2_cov - h2o_molar_density:air_temperature - h2o_molar_density:specific_humidity - h2o_molar_density:Tdew
- h2o_molar_density:h2o...126 - h2o_mole_fraction:sonic_temperature - h2o_mole_fraction:air_pressure - h2o_mole_fraction:air_heat_capacity - h2o_mole_fraction:air_molar_volume
- h2o_mole_fraction:w_slash_co2_cov - h2o_mole_fraction:h2o...126 - h2o_mole_fraction:h2o...128 - h2o_mixing_ratio:air_pressure - sonic_temperature:air_molar_volume 
- h2o_mole_fraction:air_heat_capacity - h2o_mole_fraction:w_slash_co2_cov - h2o_mole_fraction:h2o...126 - h2o_mole_fraction:h2o...128 - h2o_mixing_ratio:air_pressure
- sonic_temperature:air_molar_volume - air_temperature:e - air_temperature:w_slash_co2_cov - air_temperature:h2o...126 - air_temperature:flowrate - air_pressure:e - air_pressure:es
- air_density:water_vapor_density - air_density:w_slash_co2_cov - air_density:flowrate - air_heat_capacity:w_slash_co2_cov - air_heat_capacity:flowrate - air_molar_volume:w_slash_co2_cov
- air_molar_volume:h2o...126 - air_molar_volume:flowrate - water_vapor_density:specific_humidity - e:w_slash_co2_cov - es:specific_humidity 
- es:w_slash_co2_cov - specific_humidity:flowrate - un_co2_flux:flowrate - w_slash_co2_cov:h2o...128 - h2o_mole_fraction:air_temperature - h2o_mole_fraction:air_density - h2o_mixing_ratio:air_heat_capacity
- h2o_mixing_ratio:air_molar_volume - h2o_mixing_ratio:h2o...126 - h2o_mixing_ratio:h2o...128 - sonic_temperature:air_pressure - sonic_temperature:water_vapor_density - sonic_temperature:Tdew 
- air_temperature:air_molar_volume - air_temperature:es - air_pressure:Tdew - air_pressure:w_slash_co2_cov - air_density:h2o...126 - air_heat_capacity:un_co2_flux - water_vapor_density:w_slash_co2_cov 
-water_vapor_density:h2o...128 - e:specific_humidity - e:h2o...126 - specific_humidity:un_co2_flux - specific_humidity:h2o...126 - Tdew:h2o...126 - h2o...128:flowrate - h2o_mixing_ratio:sonic_temperature - h2o_mixing_ratio:air_density
- air_temperature:air_pressure - air_temperature:water_vapor_density - air_temperature:Tdew - air_density:e - air_density:Tdew - air_density:e - air_density:Tdew - air_heat_capacity:water_vapor_density
- air_heat_capacity:h2o...126 - air_molar_volume:un_co2_flux - e:Tdew - e:h2o...128 - specific_humidity:w_slash_co2_cov - specific_humidity:h2o...128 - h2o...126:h2o...128) 

#Коэффициенты, остатки и доверительный интервал 
coef(mod6)
resid(mod6)
confint(mod6)

#P-значения по модели
summary(mod6)
# коеффициент детерминации = 0.9504
# таким образом, более полная модель 2 лучше отражает данные

#ДА
anova(mod6)
#Графиик на нормальной веротяностной бумаге :
plot(mod6,2) 
# Построим график наблюдаемых значений от предсказанных значений
plot(mod6$fitted.values, eddypro_numeric$co2_flux)
# Добавим линию у=х
abline(a=0, b=1, col="yellow")
# Построим график остатков от набоюдаемых значений 
plot(eddypro_numeric$co2_flux,mod6$residuals)
# Для поиска коэффициентов для линии зададим модель, связывающую остатки и CO2
mo6=lm(mod6$residuals~eddypro_numeric$co2_flux)
abline(a=mo6$coefficients[1],b=mo6$coefficients[2],col="yellow")

# Отмечается ухудщение модели с каждым разом, лучше всего использовать 2 модель
# мод 2 наилучше отражает данные R^2=0.9592
# данные распеределены нормально
