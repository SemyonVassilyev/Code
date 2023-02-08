library("readxl")
library("AER")
d <- read_excel("dataR.xlsx")
table(d)

#ПОЯСНЕНИЕ ПЕРЕМЕННЫХ
#gni = GNI per capita growth (annual %)
#lfad = Labor force with advanced education, %
#adr = Age dependency ratio (% of working-age population)
#gcf = Gross capital formation (annual % growth)
#rifs = Renewable internal freshwater resources per capita, m3
#tnrr = Total natural resources rents (% of GDP)
#dummy = Дамми на богатство


m1 <- lm(gni ~ lfad + adr + gcf + rifs
        + tnrr + dummy, data = d)
summary(m1)
resettest(m1, data = d , power=2)
#эта модель показалась слишком скучной и простой
#поэтому в новую модель попробовали добавить  dummy*gcf
#чтобы посмотреть на влияние прироста капитала в записимости от (не)бедноти страны

m2 <- lm(gni ~ lfad + adr + gcf + rifs
         + tnrr + dummy + I(dummy*gcf), data = d)
summary(m2)
resettest(m2, data = d , power=2)
#уже интереснее, но после РЕСЕТ-теста стало ясно, что не хвататет квадратичных признаков

#ниже идет наша итоговая
m3 <- lm(gni ~ lfad + adr + gcf + I(gcf^2) + rifs
         + tnrr + dummy + I(dummy*gcf), data = d)
summary(m3)
resettest(m3, data = d , power=2)

#здесь по итогу получили хорошие результаты на RESET-тесте (при всех разумных альфа норм спецификация)
#но этому результату предшествовал долгий перебор для каждой переменной