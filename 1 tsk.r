#Ерохов Юрий Михайлович. Д-А 131. 
# регион 79 рассчитать урожайность пшеницы
#в 2013 году, взяв для рассчета средние суммы активных температур 
#за текущий год, с 14 ближайших метеостанций но убирая из расчета активных температур дни с температурой выше 30 градусов
# 79 регион - Еврейская автономная область Столица биробиджан 

#проверим рабочую дирректорию 
getwd()
#работа с пакетами + проверка библиотек 
library(tidyverse)
library(rnoaa)
library(lubridate)
#скачиваем станции для работы 
station_data = ghcnd_stations()
station_data
write.csv(station_data, file = "stations.csv")
station_data = read.csv("stations.csv")

#После получения списка станций, получим список станций ближайщих к столице региона и координами его столицы
evr = data.frame(id = "EVR" , latitude = 48.789917, longitude = 132.924750)
evr_around = meteo_nearby_stations(lat_lon_df = evr, station_data = station_data,
                                      limit = 14, var = c("TAVG"),
                                      year_min = 2013, year_max = 2013)
#проверить точно координаты 

evr_around #evr_around это список единственным элементом которого является таблица
# содержащая идентификаторыметеостанций отсортированных по их 
# удалленности от нашей столицы 
# первым элементом как раз и будет нужный нам идентификатор 
#отфильтруем станции по расстоянию 
evr_id = evr_around[["EVR"]][["id"]][1]
summary(evr_id)

#выбираем целиком первый объект из списка
evr_table = evr_around[[1]]
summary(evr_table)

# в таблице evr_table оказалось 14 объектов ранжированых по расстоянию от Владивастока
#сформируем список необходимых станций
evr_stations = evr_table
str(evr_stations)

#выводим индетификаторы отфильтрованных метиостанций
evr_stations$id

#создаем цикл, в котором бы скачивались нужные данные для всех метеостанций
#создадим объект, куда скачаем все данные всех метеостанций
all_evr_data = meteo_tidy_ghcnd(stationid = evr_id)
summary(all_evr_data)

#Создаем объект куда скачаем все данные всех метеостанций(колличество)
all_evr_meteodata = data.frame()
#Создаем цикл для метеостанций
stations_names = evr_stations$id
stations_names = stations_names[1:14] 

for (sname in stations_names)
{ one_meteo = meteo_tidy_ghcnd( stationid = sname,
                              date_min = "2013-01-01",
                              date_max = "2013-12-31")
station_vars = names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }
  
  one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_evr_meteodata=rbind(all_evr_meteodata, one_meteo)}


#Записываем полученные результаты
write.csv(all_evr_meteodata,"all_evr_meteodata.csv")
#считываем данные 
all_evr_meteodata=read.csv("all_evr_meteodata.csv")
#смотрим, что получилось
str(all_evr_meteodata)

#добавим год, месяц, день
all_evr_meteodata = all_evr_meteodata %>% mutate(year=year(date), 
                                              month=month(date), 
                                                 day=day(date))
#Превратим NA в 0 и где tavg<5>30 наш главный рассчет 
all_evr_meteodata[is.na(all_evr_meteodata$tavg),"tavg"] = 0
all_evr_meteodata[all_evr_meteodata$tavg<5, "tavg"] = 0
all_evr_meteodata[all_evr_meteodata$tavg>30, "tavg"] = 0
summary(all_evr_meteodata)

#Сгрупируем метеостанции по id, месяцам и годам и проссумируем температуру по этим группа, 
#затем сгруппируем данные по месяцам и найдем среднее по месяцам для всех метеостанций
group_meteodata = all_evr_meteodata %>% group_by(id,year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))


#Введем значения для рассчета по формлуе
y = 1.0 #коэффициент для экспозиции склона будем условно считать что все склоны супер идеально ровные 
afi = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)#константа, из табл.1

bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)#константа, из табл.1
di = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)#отношение числа i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце из табл.1
Kf = 300 #  коэффициент использования ФАР посевом
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  коэффициент «Сумма частей основной и побочной продукции
Ej = 25 #   коэффициент «Стандартная влажность культуры

#Оассчитай коэфы
#Расчитаем Fi 
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)

#Расчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))

#Расчитаем урожай
Uroj = (sum(sumT_month$Yi)) 
Uroj 

#Результат 16,93 ц/га

