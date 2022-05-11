#Пакеты
library(readxl)
library(dplyr)
library(tidyr)
library(plm)
library(stargazer)
library(sandwich)
library(lmtest)
library(car)
library(ggplot2)
library(openintro) 
library(ivpack)
library(corrplot)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(stargazer)
library(car)
library(pscl)
library(WeightIt)
library(GLDEX)
library(ExPanDaR)
library(Matching)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(ggdendro)


# ПОДГОТОВКА ДАННЫХ 

#Импорт файлов

Data_1.1 <- read_excel("R_files/1.1.xlsx")
Data_2.1 <- read_excel("R_files/2.1.xlsx")
Data_3.1 <- read_excel("R_files/3.1.xlsx")
Data_4.1 <- read_excel("R_files/4.1.xlsx")
Data_5.1 <- read_excel("R_files/5.1.xlsx")
Data_6.1 <- read_excel("R_files/6.1.xlsx")
Data_7.1 <- read_excel("R_files/7.1.xlsx")
Data_8.1 <- read_excel("R_files/8.1.xlsx")
Data_9.1 <- read_excel("R_files/9.1.xlsx")
Data_10.1 <- read_excel("R_files/10.1.xlsx")
Data_11.1 <- read_excel("R_files/11.1.xlsx")
Data_12.1 <- read_excel("R_files/12.1.xlsx")
Data_13.1 <- read_excel("R_files/13.1.xlsx")
Data_14.1 <- read_excel("R_files/14.1.xlsx")
Data_15.1 <- read_excel("R_files/15.1.xlsx")
Data_16.1 <- read_excel("R_files/16.1.xlsx")
Data_1.2 <- read_excel("R_files/1.2.xlsx")
Data_2.2 <- read_excel("R_files/2.2.xlsx")
Data_3.2 <- read_excel("R_files/3.2.xlsx")
Data_4.2 <- read_excel("R_files/4.2.xlsx")
Data_5.2 <- read_excel("R_files/5.2.xlsx")
Data_6.2 <- read_excel("R_files/6.2.xlsx")
Data_7.2 <- read_excel("R_files/7.2.xlsx")
Data_8.2 <- read_excel("R_files/8.2.xlsx")
Data_9.2 <- read_excel("R_files/9.2.xlsx")
Data_10.2 <- read_excel("R_files/10.2.xlsx")
Data_11.2 <- read_excel("R_files/11.2.xlsx")
Data_12.2 <- read_excel("R_files/12.2.xlsx")
Data_13.2 <- read_excel("R_files/13.2.xlsx")
Data_14.2 <- read_excel("R_files/14.2.xlsx")
Data_15.2 <- read_excel("R_files/15.2.xlsx")
Data_16.2 <- read_excel("R_files/16.2.xlsx")

# Объединяем данные по годам

Data_1 <- merge(Data_1.1, Data_1.2, by.x = 2, by.y = 2)
Data_2 <- merge(Data_2.1, Data_2.2, by.x = 2, by.y = 2)
Data_3 <- merge(Data_3.1, Data_3.2, by.x = 2, by.y = 2)
Data_4 <- merge(Data_4.1, Data_4.2, by.x = 2, by.y = 2)
Data_5 <- merge(Data_5.1, Data_5.2, by.x = 2, by.y = 2)
Data_6 <- merge(Data_6.1, Data_6.2, by.x = 2, by.y = 2)
Data_7 <- merge(Data_7.1, Data_7.2, by.x = 2, by.y = 2)
Data_8 <- merge(Data_8.1, Data_8.2, by.x = 2, by.y = 2)
Data_9 <- merge(Data_9.1, Data_9.2, by.x = 2, by.y = 2)
Data_10 <- merge(Data_10.1, Data_10.2, by.x = 2, by.y = 2)
Data_11 <- merge(Data_11.1, Data_11.2, by.x = 2, by.y = 2)
Data_12 <- merge(Data_12.1, Data_12.2, by.x = 2, by.y = 2)
Data_13 <- merge(Data_13.1, Data_13.2, by.x = 2, by.y = 2)
Data_14 <- merge(Data_14.1, Data_14.2, by.x = 2, by.y = 2)
Data_15 <- merge(Data_15.1, Data_15.2, by.x = 2, by.y = 2)
Data_16 <- merge(Data_16.1, Data_16.2, by.x = 2, by.y = 2)

# Меняем заголовки

names <- c('i','code','age', 'status', 'NWC_14', 'NWC_15', 'NWC_16', 'TAS_14', 'TAS_15', 'TAS_16', 'REV_14', 'REV_15', 'REV_16', 'NPR_14', 'NPR_15', 'NPR_16', 'DSO_14', 'DSO_15', 'DSO_16', 'LEV_14', 'LEV_15', 'LEV_16', 'CCE_14', 'CCE_15', 'CCE_16', "ROA_14", 'ROA_15', 'ROA_16', 'ROE_14', 'ROE_15', 'ROE_16', 'FLQ_14', 'FLQ_15', 'FLQ_16', 'ALQ_14', 'ALQ_15', 'ALQ_16', 'n2', 'age2', 'status2', 'NWC_17', 'NWC_18', 'NWC_19','NWC_20', 'NWC_21', 'TAS_17', 'TAS_18', 'TAS_19','TAS_20', 'TAS_21', 'REV_17', 'REV_18', 'REV_19','REV_20', 'REV_21', 'NPR_17', 'NPR_18', 'NPR_19', 'NPR_20', 'NPR_21', 'DSO_17', 'DSO_18', 'DSO_19','DSO_20', 'DSO_21', 'LEV_17', 'LEV_18', 'LEV_19', 'LEV_20', 'LEV_21', 'CCE_17', 'CCE_18', 'CCE_19', 'CCE_20', 'CCE_21', "ROA_17", 'ROA_18', 'ROA_19',"ROA_20", 'ROA_21', 'ROE_17', 'ROE_18', 'ROE_19','ROE_20', 'ROE_21', 'FLQ_17', 'FLQ_18', 'FLQ_19', 'FLQ_20', 'FLQ_21', 'ALQ_17', 'ALQ_18', 'ALQ_19', 'ALQ_20', 'ALQ_21')

names(Data_1) <- names
names(Data_2) <- names
names(Data_3) <- names
names(Data_4) <- names
names(Data_5) <- names
names(Data_6) <- names
names(Data_7) <- names
names(Data_8) <- names
names(Data_9) <- names
names(Data_10) <- names
names(Data_11) <- names
names(Data_12) <- names
names(Data_13) <- names
names(Data_14) <- names
names(Data_15) <- names
names(Data_16) <- names

# Индексы для отраслей
Data_1$ind <- rep(1, dim(Data_1)[1])
Data_2$ind <- rep(2, dim(Data_2)[1])
Data_3$ind <- rep(3, dim(Data_3)[1])
Data_4$ind <- rep(4, dim(Data_4)[1])
Data_5$ind <- rep(5, dim(Data_5)[1])
Data_6$ind <- rep(6, dim(Data_6)[1])
Data_7$ind <- rep(7, dim(Data_7)[1])
Data_8$ind <- rep(8, dim(Data_8)[1])
Data_9$ind <- rep(9, dim(Data_9)[1])
Data_10$ind <- rep(10, dim(Data_10)[1])
Data_11$ind <- rep(11, dim(Data_11)[1])
Data_12$ind <- rep(12, dim(Data_12)[1])
Data_13$ind <- rep(13, dim(Data_13)[1])
Data_14$ind <- rep(14, dim(Data_14)[1])
Data_15$ind <- rep(15, dim(Data_15)[1])
Data_16$ind <- rep(16, dim(Data_16)[1])

# Объединяем в одну таблицу и удаляем пропуски
Data <- rbind(Data_1, Data_2, Data_3, Data_4, Data_5, Data_6, Data_7, Data_8 ,Data_9, Data_10,Data_11, Data_12, Data_13,Data_14, Data_15,Data_16)
Data <- na.omit(Data)

Data$GRO_15 <- Data$REV_15/Data$REV_14*100-100
Data$GRO_16 <- Data$REV_16/Data$REV_15*100-100
Data$GRO_17 <- Data$REV_17/Data$REV_16*100-100
Data$GRO_18 <- Data$REV_18/Data$REV_17*100-100
Data$GRO_19 <- Data$REV_19/Data$REV_18*100-100
Data$GRO_20 <- Data$REV_20/Data$REV_19*100-100
Data$GRO_21 <- Data$REV_21/Data$REV_20*100-100

# Немного меняем порядок столбцов для дальнейшего преобразования

Data$industry <- Data$ind
Data <- Data[-which( colnames(Data)=="ind")]
Data <- dplyr::select(Data, -n2, -age2)

# Чистим выбросы
a1_15 <- quantile(Data$NWC_15, 0.01)
a2_15 <- quantile(Data$NWC_15, 0.98)
a1_16 <- quantile(Data$NWC_16, 0.01)
a2_16 <- quantile(Data$NWC_16, 0.98)
a1_17 <- quantile(Data$NWC_17, 0.01)
a2_17 <- quantile(Data$NWC_17, 0.98)
a1_18 <- quantile(Data$NWC_18, 0.01)
a2_18 <- quantile(Data$NWC_18, 0.98)
a1_19 <- quantile(Data$NWC_19, 0.01)
a2_19 <- quantile(Data$NWC_19, 0.98)
a1_20 <- quantile(Data$NWC_20, 0.01)
a2_20 <- quantile(Data$NWC_20, 0.98)
a1_21 <- quantile(Data$NWC_21, 0.01)
a2_21 <- quantile(Data$NWC_21, 0.98)

b1_15 <- quantile(Data$TAS_15, 0.01)
b2_15 <- quantile(Data$TAS_15, 0.98)
b1_16 <- quantile(Data$TAS_16, 0.01)
b2_16 <- quantile(Data$TAS_16, 0.98)
b1_17 <- quantile(Data$TAS_17, 0.01)
b2_17 <- quantile(Data$TAS_17, 0.98)
b1_18 <- quantile(Data$TAS_18, 0.01)
b2_18 <- quantile(Data$TAS_18, 0.98)
b1_19 <- quantile(Data$TAS_19, 0.01)
b2_19 <- quantile(Data$TAS_19, 0.98)
b1_20 <- quantile(Data$TAS_20, 0.01)
b2_20 <- quantile(Data$TAS_20, 0.98)
b1_21 <- quantile(Data$TAS_21, 0.01)
b2_21 <- quantile(Data$TAS_21, 0.98)

c1_15 <- quantile(Data$REV_15, 0.01)
c2_15 <- quantile(Data$REV_15, 0.99)
c1_16 <- quantile(Data$REV_16, 0.01)
c2_16 <- quantile(Data$REV_16, 0.99)
c1_17 <- quantile(Data$REV_17, 0.01)
c2_17 <- quantile(Data$REV_17, 0.99)
c1_18 <- quantile(Data$REV_18, 0.01)
c2_18 <- quantile(Data$REV_18, 0.99)
c1_19 <- quantile(Data$REV_19, 0.01)
c2_19 <- quantile(Data$REV_19, 0.99)
c1_20 <- quantile(Data$REV_20, 0.01)
c2_20 <- quantile(Data$REV_20, 0.99)
c1_21 <- quantile(Data$REV_21, 0.01)
c2_21 <- quantile(Data$REV_21, 0.99)

d1_15 <- quantile(Data$NPR_15, 0.015)
d2_15 <- quantile(Data$NPR_15, 0.98)
d1_16 <- quantile(Data$NPR_16, 0.015)
d2_16 <- quantile(Data$NPR_16, 0.98)
d1_17 <- quantile(Data$NPR_17, 0.015)
d2_17 <- quantile(Data$NPR_17, 0.98)
d1_18 <- quantile(Data$NPR_18, 0.015)
d2_18 <- quantile(Data$NPR_18, 0.98)
d1_19 <- quantile(Data$NPR_19, 0.015)
d2_19 <- quantile(Data$NPR_19, 0.98)
d1_20 <- quantile(Data$NPR_20, 0.015)
d2_20 <- quantile(Data$NPR_20, 0.98)
d1_21 <- quantile(Data$NPR_21, 0.015)
d2_21 <- quantile(Data$NPR_21, 0.98)

e1_15 <- quantile(Data$DSO_15, 0.01)
e2_15 <- quantile(Data$DSO_15, 0.99)
e1_16 <- quantile(Data$DSO_16, 0.01)
e2_16 <- quantile(Data$DSO_16, 0.99)
e1_17 <- quantile(Data$DSO_17, 0.01)
e2_17 <- quantile(Data$DSO_17, 0.99)
e1_18 <- quantile(Data$DSO_18, 0.01)
e2_18 <- quantile(Data$DSO_18, 0.99)
e1_19 <- quantile(Data$DSO_19, 0.01)
e2_19 <- quantile(Data$DSO_19, 0.99)
e1_20 <- quantile(Data$DSO_20, 0.01)
e2_20 <- quantile(Data$DSO_20, 0.99)
e1_21 <- quantile(Data$DSO_21, 0.01)
e2_21 <- quantile(Data$DSO_21, 0.99)

f1_15 <- quantile(Data$LEV_15, 0.01)
f2_15 <- quantile(Data$LEV_15, 0.99)
f1_16 <- quantile(Data$LEV_16, 0.01)
f2_16 <- quantile(Data$LEV_16, 0.99)
f1_17 <- quantile(Data$LEV_17, 0.01)
f2_17 <- quantile(Data$LEV_17, 0.99)
f1_18 <- quantile(Data$LEV_18, 0.01)
f2_18 <- quantile(Data$LEV_18, 0.99)
f1_19 <- quantile(Data$LEV_19, 0.01)
f2_19 <- quantile(Data$LEV_19, 0.99)
f1_20 <- quantile(Data$LEV_20, 0.01)
f2_20 <- quantile(Data$LEV_20, 0.99)
f1_21 <- quantile(Data$LEV_21, 0.01)
f2_21 <- quantile(Data$LEV_21, 0.99)

g1_15 <- quantile(Data$CCE_15, 0.01)
g2_15 <- quantile(Data$CCE_15, 0.99)
g1_16 <- quantile(Data$CCE_16, 0.01)
g2_16 <- quantile(Data$CCE_16, 0.99)
g1_17 <- quantile(Data$CCE_17, 0.01)
g2_17 <- quantile(Data$CCE_17, 0.99)
g1_18 <- quantile(Data$CCE_18, 0.01)
g2_18 <- quantile(Data$CCE_18, 0.99)
g1_19 <- quantile(Data$CCE_19, 0.01)
g2_19 <- quantile(Data$CCE_19, 0.99)
g1_20 <- quantile(Data$CCE_20, 0.01)
g2_20 <- quantile(Data$CCE_20, 0.99)
g1_21 <- quantile(Data$CCE_21, 0.01)
g2_21 <- quantile(Data$CCE_21, 0.99)

h1_15 <- quantile(Data$ROA_15, 0.01)
h2_15 <- quantile(Data$ROA_15, 0.99)
h1_16 <- quantile(Data$ROA_16, 0.01)
h2_16 <- quantile(Data$ROA_16, 0.99)
h1_17 <- quantile(Data$ROA_17, 0.01)
h2_17 <- quantile(Data$ROA_17, 0.99)
h1_18 <- quantile(Data$ROA_18, 0.01)
h2_18 <- quantile(Data$ROA_18, 0.99)
h1_19 <- quantile(Data$ROA_19, 0.01)
h2_19 <- quantile(Data$ROA_19, 0.99)
h1_20 <- quantile(Data$ROA_20, 0.01)
h2_20 <- quantile(Data$ROA_20, 0.99)
h1_21 <- quantile(Data$ROA_21, 0.01)
h2_21 <- quantile(Data$ROA_21, 0.99)

i1_15 <- quantile(Data$ROE_15, 0.01)
i2_15 <- quantile(Data$ROE_15, 0.99)
i1_16 <- quantile(Data$ROE_16, 0.01)
i2_16 <- quantile(Data$ROE_16, 0.99)
i1_17 <- quantile(Data$ROE_17, 0.01)
i2_17 <- quantile(Data$ROE_17, 0.99)
i1_18 <- quantile(Data$ROE_18, 0.01)
i2_18 <- quantile(Data$ROE_18, 0.99)
i1_19 <- quantile(Data$ROE_19, 0.01)
i2_19 <- quantile(Data$ROE_19, 0.99)
i1_20 <- quantile(Data$ROE_20, 0.01)
i2_20 <- quantile(Data$ROE_20, 0.99)
i1_21 <- quantile(Data$ROE_21, 0.01)
i2_21 <- quantile(Data$ROE_21, 0.99)

j1_15 <- quantile(Data$FLQ_15, 0.01)
j2_15 <- quantile(Data$FLQ_15, 0.99)
j1_16 <- quantile(Data$FLQ_16, 0.01)
j2_16 <- quantile(Data$FLQ_16, 0.99)
j1_17 <- quantile(Data$FLQ_17, 0.01)
j2_17 <- quantile(Data$FLQ_17, 0.99)
j1_18 <- quantile(Data$FLQ_18, 0.01)
j2_18 <- quantile(Data$FLQ_18, 0.99)
j1_19 <- quantile(Data$FLQ_19, 0.01)
j2_19 <- quantile(Data$FLQ_19, 0.99)
j1_20 <- quantile(Data$FLQ_20, 0.01)
j2_20 <- quantile(Data$FLQ_20, 0.99)
j1_21 <- quantile(Data$FLQ_21, 0.01)
j2_21 <- quantile(Data$FLQ_21, 0.99)

k1_15 <- quantile(Data$ALQ_15, 0.01)
k2_15 <- quantile(Data$ALQ_15, 0.99)
k1_16 <- quantile(Data$ALQ_16, 0.01)
k2_16 <- quantile(Data$ALQ_16, 0.99)
k1_17 <- quantile(Data$ALQ_17, 0.01)
k2_17 <- quantile(Data$ALQ_17, 0.99)
k1_18 <- quantile(Data$ALQ_18, 0.01)
k2_18 <- quantile(Data$ALQ_18, 0.99)
k1_19 <- quantile(Data$ALQ_19, 0.01)
k2_19 <- quantile(Data$ALQ_19, 0.99)
k1_20 <- quantile(Data$ALQ_20, 0.01)
k2_20 <- quantile(Data$ALQ_20, 0.99)
k1_21 <- quantile(Data$ALQ_21, 0.01)
k2_21 <- quantile(Data$ALQ_21, 0.99)

Data <- filter(Data, NWC_15 > a1_15 & NWC_15 < a2_15)
Data <- filter(Data, NWC_16 > a1_16 & NWC_16 < a2_16)
Data <- filter(Data, NWC_17 > a1_17 & NWC_17 < a2_17)
Data <- filter(Data, NWC_18 > a1_18 & NWC_18 < a2_18)
Data <- filter(Data, NWC_19 > a1_19 & NWC_19 < a2_19)
Data <- filter(Data, NWC_20 > a1_20 & NWC_20 < a2_20)
Data <- filter(Data, NWC_21 > a1_21 & NWC_21 < a2_21)

Data <- filter(Data, TAS_15 > b1_15 & TAS_15 < b2_15)
Data <- filter(Data, TAS_16 > b1_16 & TAS_16 < b2_16)
Data <- filter(Data, TAS_17 > b1_17 & TAS_17 < b2_17)
Data <- filter(Data, TAS_18 > b1_18 & TAS_18 < b2_18)
Data <- filter(Data, TAS_19 > b1_19 & TAS_19 < b2_19)
Data <- filter(Data, TAS_20 > b1_20 & TAS_20 < b2_20)
Data <- filter(Data, TAS_21 > b1_21 & TAS_21 < b2_21)

Data <- filter(Data, REV_15 > c1_15 & REV_15 < c2_15)
Data <- filter(Data, REV_16 > c1_16 & REV_16 < c2_16)
Data <- filter(Data, REV_17 > c1_17 & REV_17 < c2_17)
Data <- filter(Data, REV_18 > c1_18 & REV_18 < c2_18)
Data <- filter(Data, REV_19 > c1_19 & REV_19 < c2_19)
Data <- filter(Data, REV_20 > c1_20 & REV_20 < c2_20)
Data <- filter(Data, REV_21 > c1_21 & REV_21 < c2_21)

Data <- filter(Data, NPR_15 > d1_15 & NPR_15 < d2_15)
Data <- filter(Data, NPR_16 > d1_16 & NPR_16 < d2_16)
Data <- filter(Data, NPR_17 > d1_17 & NPR_17 < d2_17)
Data <- filter(Data, NPR_18 > d1_18 & NPR_18 < d2_18)
Data <- filter(Data, NPR_19 > d1_19 & NPR_19 < d2_19)
Data <- filter(Data, NPR_20 > d1_20 & NPR_20 < d2_20)
Data <- filter(Data, NPR_21 > d1_21 & NPR_21 < d2_21)

Data <- filter(Data, DSO_15 > e1_15 & DSO_15 < e2_15)
Data <- filter(Data, DSO_16 > e1_16 & DSO_16 < e2_16)
Data <- filter(Data, DSO_17 > e1_17 & DSO_17 < e2_17)
Data <- filter(Data, DSO_18 > e1_18 & DSO_18 < e2_18)
Data <- filter(Data, DSO_19 > e1_19 & DSO_19 < e2_19)
Data <- filter(Data, DSO_20 > e1_20 & DSO_20 < e2_20)
Data <- filter(Data, DSO_21 > e1_21 & DSO_21 < e2_21)

Data <- filter(Data, LEV_15 > f1_15 & LEV_15 < f2_15)
Data <- filter(Data, LEV_16 > f1_16 & LEV_16 < f2_16)
Data <- filter(Data, LEV_17 > f1_17 & LEV_17 < f2_17)
Data <- filter(Data, LEV_18 > f1_18 & LEV_18 < f2_18)
Data <- filter(Data, LEV_19 > f1_19 & LEV_19 < f2_19)
Data <- filter(Data, LEV_20 > f1_20 & LEV_20 < f2_20)
Data <- filter(Data, LEV_21 > f1_21 & LEV_21 < f2_21)

Data <- filter(Data, CCE_15 > g1_15 & CCE_15 < g2_15)
Data <- filter(Data, CCE_16 > g1_16 & CCE_16 < g2_16)
Data <- filter(Data, CCE_17 > g1_17 & CCE_17 < g2_17)
Data <- filter(Data, CCE_18 > g1_18 & CCE_18 < g2_18)
Data <- filter(Data, CCE_19 > g1_19 & CCE_19 < g2_19)
Data <- filter(Data, CCE_20 > g1_20 & CCE_20 < g2_20)
Data <- filter(Data, CCE_21 > g1_21 & CCE_21 < g2_21)

Data <- filter(Data, ROA_15 > h1_15 & ROA_15 < h2_15)
Data <- filter(Data, ROA_16 > h1_16 & ROA_16 < h2_16)
Data <- filter(Data, ROA_17 > h1_17 & ROA_17 < h2_17)
Data <- filter(Data, ROA_18 > h1_18 & ROA_18 < h2_18)
Data <- filter(Data, ROA_19 > h1_19 & ROA_19 < h2_19)
Data <- filter(Data, ROA_20 > h1_20 & ROA_20 < h2_20)
Data <- filter(Data, ROA_21 > h1_21 & ROA_21 < h2_21)

Data <- filter(Data, ROE_15 > i1_15 & ROE_15 < i2_15)
Data <- filter(Data, ROE_16 > i1_16 & ROE_16 < i2_16)
Data <- filter(Data, ROE_17 > i1_17 & ROE_17 < i2_17)
Data <- filter(Data, ROE_18 > i1_18 & ROE_18 < i2_18)
Data <- filter(Data, ROE_19 > i1_19 & ROE_19 < i2_19)
Data <- filter(Data, ROE_20 > i1_20 & ROE_20 < i2_20)
Data <- filter(Data, ROE_21 > i1_21 & ROE_21 < i2_21)

Data <- filter(Data, FLQ_15 > j1_15 & FLQ_15 < j2_15)
Data <- filter(Data, FLQ_16 > j1_16 & FLQ_16 < j2_16)
Data <- filter(Data, FLQ_17 > j1_17 & FLQ_17 < j2_17)
Data <- filter(Data, FLQ_18 > j1_18 & FLQ_18 < j2_18)
Data <- filter(Data, FLQ_19 > j1_19 & FLQ_19 < j2_19)
Data <- filter(Data, FLQ_20 > j1_20 & FLQ_20 < j2_20)
Data <- filter(Data, FLQ_21 > j1_21 & FLQ_21 < j2_21)

Data <- filter(Data, ALQ_15 > k1_15 & ALQ_15 < k2_15)
Data <- filter(Data, ALQ_16 > k1_16 & ALQ_16 < k2_16)
Data <- filter(Data, ALQ_17 > k1_17 & ALQ_17 < k2_17)
Data <- filter(Data, ALQ_18 > k1_18 & ALQ_18 < k2_18)
Data <- filter(Data, ALQ_19 > k1_19 & ALQ_19 < k2_19)
Data <- filter(Data, ALQ_20 > k1_20 & ALQ_20 < k2_20)
Data <- filter(Data, ALQ_21 > k1_21 & ALQ_21 < k2_21)

Data <- filter(Data, GRO_15 > -80 & GRO_15 < 300)
Data <- filter(Data, GRO_16 > -80 & GRO_16 < 300)
Data <- filter(Data, GRO_17 > -80 & GRO_17 < 300)
Data <- filter(Data, GRO_18 > -80 & GRO_18 < 300)
Data <- filter(Data, GRO_19 > -80 & GRO_19 < 300)
Data <- filter(Data, GRO_20 > -80 & GRO_20 < 300)
Data <- filter(Data, GRO_21 > -80 & GRO_21 < 300)

# Индексы для компаний
Data$i <- c(1:(dim(Data)[1]))

# Преобразование для удобства работы с панельными данными
data <- Data %>% pivot_longer(
  cols = NWC_14:GRO_21,
  names_to = c(".value", "Year"),
  names_pattern = "(.*)_(..)"
)

data <- na.omit(data)

#Добавляем факторные переменные и логарифмы где это нужно

data$Year <- as.factor(data$Year)
data$i <- as.factor(data$i)
data$status <- as.factor(data$status)
data$industry<- as.factor(data$industry)
data$age<- as.factor(data$age)
data$covid <- as.factor(ifelse(data$Year == 20 |data$Year == 21 , 1, 0))
data$PR <- as.factor(ifelse(data$NPR > median(data$NPR), 1, 0))
data$REV <- log(data$REV)
data$SIZ <- log(data$TAS)


# ПЕРВИЧНЫЙ АНАЛИЗ ДАННЫХ

#Графики

#для медианных значений и квантилей
ggplot(data, aes(x=Year, y = ROA, fill=Year)) + geom_boxplot(outlier.shape = NA) + xlab("") + ylab("ROA") + labs(fill = "Год") + scale_y_continuous(limits = c(-0.25, 0.5)) + theme(text = element_text(size = 20))
ggplot(data, aes(x=Year, y = ROE, fill=Year)) + geom_boxplot(outlier.shape = NA) + xlab("") + ylab("ROE") + labs(fill = "Год") + scale_y_continuous(limits = c(-0.5, 1)) + theme(text = element_text(size = 20))
ggplot(filter(data, NWC > quantile(data$NWC, 0.1) & NWC < quantile(data$NWC, 0.9)), aes(x=Year, y = NWC, fill=Year)) + geom_boxplot(outlier.shape = NA) + xlab("") + ylab("NWC") + labs(fill = "Год")+ theme(text = element_text(size = 20)) + scale_y_continuous(limits = c(1500000, 60000000))
ggplot(data, aes(x=Year, y = REV, fill=Year)) + geom_boxplot(outlier.shape = NA) + xlab("") + ylab("ln(Revenue)") + labs(fill = "Год") + theme(text = element_text(size = 20)) + scale_y_continuous(limits = c(13, 23))
ggplot(data, aes(x=Year, y = DSO, fill=Year)) + geom_boxplot(outlier.shape = NA) + xlab("") + ylab("DSO") + labs(fill = "Год") + theme(text = element_text(size = 20)) + scale_y_continuous(limits = c(0, 230))
ggplot(data, aes(x=Year, y = LEV, fill=Year)) + geom_boxplot(outlier.shape = NA) + xlab("") + ylab("L/E") + labs(fill = "Год")+ theme(text = element_text(size = 20)) + scale_y_continuous(limits = c(0, 2.5))
ggplot(data, aes(x=Year, y = CCE, fill=Year)) + geom_boxplot() + xlab("") + ylab("Equity Ratio") + labs(fill = "Год")+ theme(text = element_text(size = 20))
ggplot(data, aes(x=Year, y = FLQ, fill=Year)) + geom_boxplot(outlier.shape = NA) + xlab("") + ylab("Quick Ratio") + labs(fill = "Год")+ theme(text = element_text(size = 20)) + scale_y_continuous(limits = c(0, 5))
ggplot(data, aes(x=Year, y = ALQ, fill=Year)) + geom_boxplot(outlier.shape = NA) + xlab("") + ylab("Cash Ratio") + labs(fill = "Год")+ theme(text = element_text(size = 20)) + scale_y_continuous(limits = c(0, 1.25))
ggplot(data, aes(x=Year, y = GRO, fill=Year)) + geom_boxplot(outlier.shape = NA) + xlab("") + ylab("Growth") + labs(fill = "Год")+ theme(text = element_text(size = 20)) + scale_y_continuous(limits = c(-60, 80))

#для средних
means <- data %>% group_by(Year) %>% summarise(ROA = mean(ROA), ROE = mean(ROE), NWC = mean(NWC), SIZ = mean(SIZ), REV = mean(REV), FLQ = mean(FLQ), ALQ = mean(ALQ), GRO = mean(GRO), DSO = mean(DSO), CCE = mean(CCE), LEV = mean(LEV))

ggplot(means, aes(x=Year, y = ROA, fill=Year)) + geom_col() + xlab("") + ylab("mean ROA") + labs(fill = "Год") + scale_y_continuous(limits = c(0, 0.2)) + theme(text = element_text(size = 20))
ggplot(means, aes(x=Year, y = ROE, fill=Year))+ geom_col() + xlab("") + ylab("mean ROE") + labs(fill = "Год") + scale_y_continuous(limits = c(0, 0.4)) + theme(text = element_text(size = 20))
ggplot(means, aes(x=Year, y = NWC, fill=Year)) + geom_col() + xlab("") + ylab("mean NWC") + labs(fill = "Год")+ theme(text = element_text(size = 20))
ggplot(means, aes(x=Year, y = REV, fill=Year)) + geom_col() + xlab("") + ylab("mean ln(Revenue)") + labs(fill = "Год") + theme(text = element_text(size = 20))
ggplot(means, aes(x=Year, y = DSO, fill=Year)) + geom_col() + xlab("") + ylab("mean DSO") + labs(fill = "Год") + theme(text = element_text(size = 20))
ggplot(means, aes(x=Year, y = LEV, fill=Year)) + geom_col() + xlab("") + ylab("mean L/E") + labs(fill = "Год")+ theme(text = element_text(size = 20))
ggplot(means, aes(x=Year, y = CCE, fill=Year)) + geom_col() + xlab("") + ylab("mean Equity Ratio") + labs(fill = "Год")+ theme(text = element_text(size = 20))
ggplot(means, aes(x=Year, y = FLQ, fill=Year)) + geom_col() + xlab("") + ylab("mean Quick Ratio") + labs(fill = "Год")+ theme(text = element_text(size = 20)) + scale_y_continuous(limits = c(0, 3))
ggplot(means, aes(x=Year, y = ALQ, fill=Year)) + geom_col() + xlab("") + ylab("mean Cash Ratio") + labs(fill = "Год")+ theme(text = element_text(size = 20)) + scale_y_continuous(limits = c(0, 0.8))
ggplot(means, aes(x=Year, y = GRO, fill=Year)) + geom_col() + xlab("") + ylab("mean Growth") + labs(fill = "Год")+ theme(text = element_text(size = 20))
ggplot(means, aes(x=Year, y = SIZ, fill=Year)) + geom_col() + xlab("") + ylab("mean Size") + labs(fill = "Год")+ theme(text = element_text(size = 20))

#Корреляционный анализ
Data_f <- dplyr::select(data, ROA, ROE, SIZ, REV, LEV, GRO, DSO,CCE, ALQ, FLQ, NWC, NPR)
data_f <- na.omit(Data_f)
names(data_f) <- c('ROA', 'ROE', 'Ln(Assets)', 'Ln(Revenue)', 'L/E', 'Growth', 'DSO','Equity Ratio', 'Cash Ratio', 'Quick Ratio', 'NWC', 'Net Profit')

corrplot(cor(data_f), addCoef.col = TRUE, addgrid.col = TRUE)

corr_data <- data_f
cor_data <- round(cor(corr_data),2)
head(cor_data)
summary(cor_data)
melted_cor <- melt(cor_data)
head(melted_cor)
ggheatmap <- ggplot(melted_cor, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()
ggheatmap +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(2, 0),
    legend.direction = "vertical")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#ggplot(data = data_f, mapping = aes(ROE,ROA)) + geom_point(alpha = 0.25) + theme_light() +  geom_smooth(col = "blue")
#ggplot(data = data_f, mapping = aes(industry,ROA)) + geom_point(alpha = 0.25) + theme_light() +  geom_smooth(col = "blue")

# data frame для моделей
data_1 <- dplyr::select(data, ROA, ROE, SIZ, LEV, GRO, REV, DSO, CCE, FLQ, ALQ, PR, NWC, industry, covid, i, Year)
data_1 <- na.omit(data_1)


# МОДЕЛИ

# Фиктивная перерменная для 20 года
# обычный МНК
mod1 <- plm(ROA ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry + covid, data = data_1, index = c("i","Year"), model="pooling")

mod <- lm(ROA ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry + covid, data = data_1)
summary(mod)
bptest(mod_1)
resettest(mod_1, power = 2)
# Степени не пропущены
vif(mod_1)

#  Модель с фиксированными эффектами для i
mod2 <- plm(ROA ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry + covid, data = data_1, index = c("i","Year"), model="within", effect="individual")
summary(mod2)
#  Модель со случайными эффектами
mod3 <- plm(ROA ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry + covid, data = data_1, index = c("i","Year"), model="random")
summary(mod3)

vif(mod1)
#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами:
pooltest(mod1,mod2) # Модель с фиксированными эффектами лучше
#тест Бреуша-Пагана, сравнивающий pooled и RE
plmtest(mod1,effect="twoways",type="bp") # Лучше pooled
# Сопоставим три регрессии в одной табличке, используя корректные стандартные ошибки
# Но сначала настроим ошибки
clse = function(reg){ 
  # index(reg, "id") returns the id or entity variable vector 
  G = length(unique(index(reg,"id")))
  N = length(index(reg,"id"))
  dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

summary(as.factor(Data$industry))

stargazer(mod1, mod2, mod3,  
          se=list(clse(mod1),clse(mod2),clse(mod3)), 
          title="Panel regressions, clustered SEs", 
          column.labels=c("Pooled OLS", "State FE", "St-Yr FE"), 
          digits=3, type="html", out = "table.doc")


# Предположение о неоднородности в зависимости от отрасли
data_1$covI <- as.factor(as.numeric(as.character(data_1$covid))*as.numeric(as.character(data_1$industry)))
summary(data_1$covI)


# обычный МНК
mod1 <- plm(ROA ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry+ covid + covI, data = data_1, index = c("i","Year"), model="pooling")
summary(mod1)

#  Модель с фиксированными эффектами для i
mod2 <- plm(ROA ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry + covid + covI, data = data_1, index = c("i","Year"), model="within", effect="individual")
summary(mod2)
#  Модель со случайными эффектами
mod3 <- plm(ROA ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry + covid+ covI, data = data_1, index = c("i","Year"), model="random")
summary(mod3)

#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами:
pooltest(mod1,mod2) # Модель с фиксированными эффектами лучше
#тест Бреуша-Пагана, сравнивающий pooled и RE
plmtest(mod1,effect="twoways",type="bp") # Лучше pooled

stargazer(mod1, mod2, mod3,  
          se=list(clse(mod1),clse(mod2),clse(mod3)), 
          title="Panel regressions, clustered SEs", 
          column.labels=c("Pooled OLS", "State FE", "St-Yr FE"), 
          digits=3, type="html", out = "table.doc")


# Регрессии с произведениями

data_1$covSIZ <- as.numeric(as.character(data_1$covid))*data_1$SIZ
data_1$covLEV <- as.numeric(as.character(data_1$covid))*data_1$LEV
data_1$covGRO <- as.numeric(as.character(data_1$covid))*data_1$GRO
data_1$covREV <- as.numeric(as.character(data_1$covid))*data_1$REV
data_1$covDSO <- as.numeric(as.character(data_1$covid))*data_1$DSO
data_1$covCCE <- as.numeric(as.character(data_1$covid))*data_1$CCE
data_1$covFLQ <- as.numeric(as.character(data_1$covid))*data_1$FLQ
data_1$covALQ <- as.numeric(as.character(data_1$covid))*data_1$ALQ

# обычный МНК
mod1 <- plm(ROA ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + covSIZ + covLEV + covGRO + covDSO + covCCE + covFLQ + covALQ + industry + covid, data = data_1, index = c("i","Year"), model="pooling")
summary(mod1)
#  Модель с фиксированными эффектами для i
mod2 <- plm(ROA ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + covSIZ + covLEV  + covGRO + covDSO + covCCE + covFLQ + covALQ + industry + covid, data = data_1, index = c("i","Year"), model="within", effect="individual")
summary(mod2)
#  Модель со случайными эффектами
mod3 <- plm(ROA ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + covSIZ + covLEV  + covGRO + covDSO + covCCE + covFLQ + covALQ + industry + covid, data = data_1, index = c("i","Year"), model="random")
summary(mod3)

#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами:
pooltest(mod1,mod2) # Модель с фиксированными эффектами лучше
#тест Бреуша-Пагана, сравнивающий pooled и RE
plmtest(mod1,effect="twoways",type="bp") # Лучше pooled

stargazer(mod1, mod2, mod3,  
          se=list(clse(mod1),clse(mod2),clse(mod3)), 
          title="Panel regressions, clustered SEs", 
          column.labels=c("Pooled OLS", "State FE", "St-Yr FE"), 
          digits=3, type="html", out = "table.doc")

# Логит модель
data_log <- data
data_log <- filter(data_log, Year!=21)
summary(data_log)
data_log$cov <- as.factor(ifelse(data_log$Year == 20, 1, 0))
m_logit <- glm(cov ~ ROA + SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + NPR, data = data_log, family = binomial(link = "logit"))
summary(m_logit)
table(data_1$PR)
hitmiss(m_logit)

lrtest(m_logit)

plotROC(actuals = data_log$cov, fitted(m_logit))

stargazer(m_logit, 
          title="Logit model, clustered SEs", type="text", out = "table1.doc", digits=3)


# ПРОГНОЗИРОВАНИЕ

data<-pdata.frame(data,index=c("i","Year"))
data_l<-transform(data, l_ROA=Lag(ROA,-1))

data_l <- na.omit(data_l)
summary(data)
data_l <- dplyr::select(data_l, ROA, ROE, SIZ, LEV, GRO, DSO, CCE, ALQ, FLQ, NWC, NPR, Year, i, industry)
summary(data_l)

data_l$Year <- as.character(data_l$Year)
data_lm <- filter(data_l, Year=='18'|Year=='17'|Year=='16'|Year=='15')
data_lm$Year <- as.factor(data_lm$Year)
summary(data_lm)

mod <- plm(ROA ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry, data = data_lm, index = c("i","Year"), model="pooling")
summary(mod)

stargazer(mod, 
          title="Logit model, clustered SEs", type="html", 
          df=FALSE, digits=3, out = "table.doc")

data_p <- dplyr::select(filter(data_l, Year==19), -i, -ROA, -NPR, -Year)
data_p$ROA_predicted <- predict(mod, newdata = data_p)
P <- data_p %>% group_by(industry) %>% summarise(ROA_predicted = mean(ROA_predicted))


Data_P <- Data %>% group_by(industry) %>% summarise(ROA_20 = mean(ROA_20))
Data_P$ROA_21 <- P$ROA_predicted

Data_p <- Data_P %>% pivot_longer(
  cols = ROA_20:ROA_21,
  names_to = c(".value", "v"),
  names_pattern = "(.*)_(..)"
)
View(Data_p)
Data_p$Predict <- as.factor(ifelse(Data_p$v == 21, 1, 0))
Data_p <- dplyr::select(Data_p, -v)

# график сопоставления ожидаемой и реальной ROA по отраслям
ggplot(data = Data_p, mapping = aes(industry,ROA)) +labs(x = 'Отрасли', y = 'Рентабельность активов') + theme(text = element_text(size = 20)) + geom_point(aes(color = Predict), size =2) + geom_line(aes(color = Predict), size = 1.5) + scale_x_continuous(breaks = seq(1, 17, 1))
Data_P$diff <- Data_P$ROA_20-Data_P$ROA_21
# На сколько средняя ожидаемая ROA по отраслям отличается от реальной
ggplot(data = Data_P, aes(industry,diff)) + scale_x_continuous(breaks = seq(1, 16, 1)) +labs(x = 'Отрасли', y = 'Разность прогнозируемой и реальной рентабельности')+ theme(text = element_text(size = 20)) + geom_bar(stat="identity", fill = 'cadetblue2')


#ТЕСТЫ НА НАДЕЖНОСТЬ

# Альтернативная зависимая переменная

# Фиктивная перерменная для 20 года
# обычный МНК
mod1 <- plm(ROE ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry + covid, data = data_1, index = c("i","Year"), model="pooling")
summary(mod1)
#  Модель с фиксированными эффектами для i
mod2 <- plm(ROE ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry + covid, data = data_1, index = c("i","Year"), model="within", effect="individual")
summary(mod2)
#  Модель со случайными эффектами
mod3 <- plm(ROE ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry + covid, data = data_1, index = c("i","Year"), model="random")
summary(mod3)
#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами:
pooltest(mod1,mod2) # Модель с фиксированными эффектами лучше
#тест Бреуша-Пагана, сравнивающий pooled и RE
plmtest(mod1,effect="twoways",type="bp") # Лучше pooled
# Сопоставим три регрессии в одной табличке, используя корректные стандартные ошибки

stargazer(mod1, mod2, mod3,  
          se=list(clse(mod1),clse(mod2),clse(mod3)), 
          title="Panel regressions, clustered SEs", 
          column.labels=c("Pooled OLS", "State FE", "St-Yr FE"), 
          digits=3, type="html", out = "table.doc")

# Предположение о неоднородности в зависимости от отрасли
data_1$covI <- as.factor(as.numeric(as.character(data_1$covid))*as.numeric(as.character(data_1$industry)))

# обычный МНК
mod1 <- plm(ROE ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry+ covid + covI, data = data_1, index = c("i","Year"), model="pooling")
summary(mod1)
#  Модель с фиксированными эффектами для i
mod2 <- plm(ROE ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry + covid + covI, data = data_1, index = c("i","Year"), model="within", effect="individual")
summary(mod2)
#  Модель со случайными эффектами
mod3 <- plm(ROE ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + industry + covid+ covI, data = data_1, index = c("i","Year"), model="random")
summary(mod3)

#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами:
pooltest(mod1,mod2) # Модель с фиксированными эффектами лучше
#тест Бреуша-Пагана, сравнивающий pooled и RE
plmtest(mod1,effect="twoways",type="bp") # Лучше pooled

stargazer(mod1, mod2, mod3,  
          se=list(clse(mod1),clse(mod2),clse(mod3)), 
          title="Panel regressions, clustered SEs", 
          column.labels=c("Pooled OLS", "State FE", "St-Yr FE"), 
          digits=3, type="html", out = "table.doc")

# Регрессии с произведениями

data_1$covSIZ <- as.numeric(as.character(data_1$covid))*data_1$SIZ
data_1$covLEV <- as.numeric(as.character(data_1$covid))*data_1$LEV
data_1$covGRO <- as.numeric(as.character(data_1$covid))*data_1$GRO
data_1$covREV <- as.numeric(as.character(data_1$covid))*data_1$REV
data_1$covDSO <- as.numeric(as.character(data_1$covid))*data_1$DSO
data_1$covCCE <- as.numeric(as.character(data_1$covid))*data_1$CCE
data_1$covFLQ <- as.numeric(as.character(data_1$covid))*data_1$FLQ
data_1$covALQ <- as.numeric(as.character(data_1$covid))*data_1$ALQ

# обычный МНК
mod1 <- plm(ROE ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + covSIZ + covLEV + covGRO + covDSO + covCCE + covFLQ + covALQ + industry + covid, data = data_1, index = c("i","Year"), model="pooling")
summary(mod1)
#  Модель с фиксированными эффектами для i
mod2 <- plm(ROE ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + covSIZ + covLEV + covGRO + covDSO + covCCE + covFLQ + covALQ + industry + covid, data = data_1, index = c("i","Year"), model="within", effect="individual")
summary(mod2)
#  Модель со случайными эффектами
mod3 <- plm(ROE ~ SIZ + LEV + GRO + DSO + CCE + FLQ + ALQ + covSIZ + covLEV + covGRO + covDSO + covCCE + covFLQ + covALQ + industry + covid, data = data_1, index = c("i","Year"), model="random")
summary(mod3)

#Тест для сравнения pooled регрессии и регрессии с фиксированными эффектами:
pooltest(mod1,mod2) # Модель с фиксированными эффектами лучше
#тест Бреуша-Пагана, сравнивающий pooled и RE
plmtest(mod1,effect="twoways",type="bp") # Лучше pooled

stargazer(mod1, mod2, mod3,  
          se=list(clse(mod1),clse(mod2),clse(mod3)), 
          title="Panel regressions, clustered SEs", 
          column.labels=c("Pooled OLS", "State FE", "St-Yr FE"), 
          digits=3, type="html", out = "table.doc")

# Мэтчинг

X <- cbind(data_1$SIZ, data_1$LEV, data_1$GRO, data_1$DSO, data_1$CCE, data_1$FLQ, data_1$ALQ)
X <- as.matrix(X)
Mod <- Match(Y = data_1$ROA, X = X, Tr = (as.numeric(as.character(data_1$covid))), Weight = 2, version = "fast")
summary(Mod)

data1 <- filter(data_1, industry ==1)
data2 <- filter(data_1, industry ==2)
data3 <- filter(data_1, industry ==3)
data4 <- filter(data_1, industry ==4)
data5 <- filter(data_1, industry ==5)
data6 <- filter(data_1, industry ==6)
data7 <- filter(data_1, industry ==7)
data8 <- filter(data_1, industry ==8)
data9 <- filter(data_1, industry ==9)
data10 <- filter(data_1, industry ==10)
data11 <- filter(data_1, industry ==11)
data12 <- filter(data_1, industry ==12)
data13 <- filter(data_1, industry ==13)
data14 <- filter(data_1, industry ==14)
data15 <- filter(data_1, industry ==15)
data16 <- filter(data_1, industry ==16)

X1 <- as.matrix(cbind(data1$SIZ, data1$LEV, data1$GRO, data1$DSO, data1$CCE, data1$FLQ, data1$ALQ))
X2 <- as.matrix(cbind(data2$SIZ, data2$LEV, data2$GRO, data2$DSO, data2$CCE, data2$FLQ, data2$ALQ))
X3 <- as.matrix(cbind(data3$SIZ, data3$LEV, data3$GRO, data3$DSO, data3$CCE, data3$FLQ, data3$ALQ))
X4 <- as.matrix(cbind(data4$SIZ, data4$LEV, data4$GRO, data4$DSO, data4$CCE, data4$FLQ, data4$ALQ))
X5 <- as.matrix(cbind(data5$SIZ, data5$LEV, data5$GRO, data5$DSO, data5$CCE, data5$FLQ, data5$ALQ))
X6 <- as.matrix(cbind(data6$SIZ, data6$LEV, data6$GRO, data6$DSO, data6$CCE, data6$FLQ, data6$ALQ))
X7 <- as.matrix(cbind(data7$SIZ, data7$LEV, data7$GRO, data7$DSO, data7$CCE, data7$FLQ, data7$ALQ))
X8 <- as.matrix(cbind(data8$SIZ, data8$LEV, data8$GRO, data8$DSO, data8$CCE, data8$FLQ, data8$ALQ))
X9 <- as.matrix(cbind(data9$SIZ, data9$LEV, data9$GRO, data9$DSO, data9$CCE, data9$FLQ, data9$ALQ))
X10 <- as.matrix(cbind(data10$SIZ, data10$LEV, data10$GRO, data10$DSO, data10$CCE, data10$FLQ, data10$ALQ))
X11 <- as.matrix(cbind(data11$SIZ, data11$LEV, data11$GRO, data11$DSO, data11$CCE, data11$FLQ, data11$ALQ))
X12 <- as.matrix(cbind(data12$SIZ, data12$LEV, data12$GRO, data12$DSO, data12$CCE, data12$FLQ, data12$ALQ))
X13 <- as.matrix(cbind(data13$SIZ, data13$LEV, data13$GRO, data13$DSO, data13$CCE, data13$FLQ, data13$ALQ))
X14 <- as.matrix(cbind(data14$SIZ, data14$LEV, data14$GRO, data14$DSO, data14$CCE, data14$FLQ, data14$ALQ))
X15 <- as.matrix(cbind(data15$SIZ, data15$LEV, data15$GRO, data15$DSO, data15$CCE, data15$FLQ, data15$ALQ))
X16 <- as.matrix(cbind(data16$SIZ, data16$LEV, data16$GRO, data16$DSO, data16$CCE, data16$FLQ, data16$ALQ))

Mod1 <- Match(Y = data1$ROA, X = X1, Tr = (as.numeric(as.character(data1$covid))), Weight = 2, version = "fast")
Mod2 <- Match(Y = data2$ROA, X = X2, Tr = (as.numeric(as.character(data2$covid))), Weight = 2, version = "fast")
Mod3 <- Match(Y = data3$ROA, X = X3, Tr = (as.numeric(as.character(data3$covid))), Weight = 2, version = "fast")
Mod4 <- Match(Y = data4$ROA, X = X4, Tr = (as.numeric(as.character(data4$covid))), Weight = 2, version = "fast")
Mod5 <- Match(Y = data5$ROA, X = X5, Tr = (as.numeric(as.character(data5$covid))), Weight = 2, version = "fast")
Mod6 <- Match(Y = data6$ROA, X = X6, Tr = (as.numeric(as.character(data6$covid))), Weight = 2, version = "fast")
Mod7 <- Match(Y = data7$ROA, X = X7, Tr = (as.numeric(as.character(data7$covid))), Weight = 2, version = "fast")
Mod8 <- Match(Y = data8$ROA, X = X8, Tr = (as.numeric(as.character(data8$covid))), Weight = 2, version = "fast")
Mod9 <- Match(Y = data9$ROA, X = X9, Tr = (as.numeric(as.character(data9$covid))), Weight = 2, version = "fast")
Mod10 <- Match(Y = data10$ROA, X = X10, Tr = (as.numeric(as.character(data10$covid))), Weight = 2, version = "fast")
Mod11 <- Match(Y = data11$ROA, X = X11, Tr = (as.numeric(as.character(data11$covid))), Weight = 2, version = "fast")
Mod12 <- Match(Y = data12$ROA, X = X12, Tr = (as.numeric(as.character(data12$covid))), Weight = 2, version = "fast")
Mod13 <- Match(Y = data13$ROA, X = X13, Tr = (as.numeric(as.character(data13$covid))), Weight = 2, version = "fast")
Mod14 <- Match(Y = data14$ROA, X = X14, Tr = (as.numeric(as.character(data14$covid))), Weight = 2, version = "fast")
Mod15 <- Match(Y = data15$ROA, X = X15, Tr = (as.numeric(as.character(data15$covid))), Weight = 2, version = "fast")
Mod16 <- Match(Y = data16$ROA, X = X16, Tr = (as.numeric(as.character(data16$covid))), Weight = 2, version = "fast")

summary(Mod1)
summary(Mod2)
summary(Mod3)
summary(Mod4)
summary(Mod5)
summary(Mod6)
summary(Mod7)
summary(Mod8)
summary(Mod9)
summary(Mod10)
summary(Mod11)
summary(Mod12)
summary(Mod13)
summary(Mod14)
summary(Mod15)
summary(Mod16)

industry <- c(1:16)
estimate <- c(Mod1$est,Mod2$est, Mod3$est, Mod4$est, Mod5$est, Mod6$est, Mod7$est, Mod8$est, Mod9$est, Mod10$est, Mod11$est, Mod12$est, Mod13$est,Mod14$est,Mod15$est,Mod16$est)
match <- data.frame(industry, estimate)
match$p <- c(0.7,1,0.4,1,1,1,0.4,1,1,1,1,1,1,0.4,1,1)
ggplot(data = match, aes(industry,estimate)) + scale_x_continuous(breaks = seq(1, 16, 1)) +labs(x = 'Отрасли', y = 'Оценки изменения ROA, полученные мэтчингом')+ theme(text = element_text(size = 20)) + geom_bar(stat="identity", alpha = match$p, fill = 'cadetblue2')+ scale_y_continuous(limits = c(-0.05, 0.01))


#КЛАСТЕРИЗАЦИЯ

Data_l <- as.data.frame(c(1:dim(Data)))
Data_l$ROA <- Data$ROA_20-Data$ROA_19
Data_l$ROE <- Data$ROE_20-Data$ROE_19
Data_l$NWC <- Data$NWC_20-Data$NWC_19
Data_l$REV <- Data$REV_20-Data$REV_19
Data_l$DSO <- Data$DSO_20-Data$DSO_19
Data_l$NPR <- Data$NPR_20-Data$NPR_19
Data_l$CCE <- Data$CCE_20-Data$CCE_19
Data_l$LEV <- Data$LEV_20-Data$LEV_19
Data_l$FLQ <- Data$FLQ_20-Data$FLQ_19
Data_l$ALQ <- Data$ALQ_20-Data$ALQ_19
Data_l$GRO <- Data$GRO_20-Data$GRO_19
Data_l$TAS <- Data$TAS_20-Data$TAS_19
Data_l$industry <- Data$industry

summary(Data_l)
k <- 7
Data_l3 <- dplyr:: filter(Data_l,ROA< 0.0449*k, ROA>-0.0386*k, ROE< 0.078900*k, ROE>-0.085400*k, NWC > -4000*k, NWC < 7368000*k, REV<1.162e+07*k, REV>-5.012e+06*k, DSO<19.000*k, DSO>-8.000*k, NPR<2582000*k, NPR>-1068000*k, CCE>-0.041700*k, CCE< 0.055400*k, LEV> -0.20460*k, LEV< 0.12930*k, FLQ > -0.2405*k, FLQ <  0.4954*k, ALQ > -0.0774*k, ALQ<0.2177*k, GRO>-24.043*k, GRO<15.909*k, TAS>-9.930e+05*k, TAS < 1.241e+07*k)
Data_l2 <- dplyr::select(Data_l3, ROA, ROE, TAS, GRO, REV, NPR, NWC, DSO, CCE, LEV, FLQ, ALQ)
names(Data_l2) <- c('ROA', 'ROE','Total Assets','Growth',  'Revenue', 'Net Profit', 'NWC', 'DSO', 'Equity Ratio', 'L/E', 'Quick Ratio', 'Cash Ratio')
mod <- hclust(dist(scale(Data_l2)))
flexclust::barchart(mod, scale(Data_l2), k = 3)
Data_l3$clust <- cutree(mod, 3)
summary(Data_l3)
Data_l3$industry <- as.factor(Data_l3$industry) 
Data_l3$clust <- as.factor(Data_l3$clust)
summary(Data_l3$clust)
clust_1 <- filter(Data_l3, clust ==1)
clust_2 <- filter(Data_l3, clust ==2)
clust_3 <- filter(Data_l3, clust ==3)
summary(clust_1$industry)
summary(clust_2$industry)
summary(clust_3$industry)

ggplot(clust_1, aes(x=industry)) + geom_bar(fill = 'cadetblue2')+labs(x = 'Отрасли', y = 'Число компаний',title = 'Cluster 1')+ theme(text = element_text(size = 20))
ggplot(clust_2, aes(x=industry)) + geom_bar(fill = 'cadetblue2')+labs(x = 'Отрасли', y = 'Число компаний',title = 'Cluster 2')+ theme(text = element_text(size = 20))
ggplot(clust_3, aes(x=industry)) + geom_bar(fill = 'cadetblue2')+labs(x = 'Отрасли', y = 'Число компаний',title = 'Cluster 3')+ theme(text = element_text(size = 20))

ggplot(Data_l3, aes(x=industry)) + geom_bar(fill = 'cadetblue2')+labs(x = 'Отрасли', y = 'Число компаний')+ theme(text = element_text(size = 20))

i_1 <- filter(Data_l3, industry ==1)
i_2 <- filter(Data_l3, industry ==2)
i_3 <- filter(Data_l3, industry ==3)
i_4 <- filter(Data_l3, industry ==4)
i_5 <- filter(Data_l3, industry ==5)
i_6 <- filter(Data_l3, industry ==6)
i_7 <- filter(Data_l3, industry ==7)
i_8 <- filter(Data_l3, industry ==8)
i_9 <- filter(Data_l3, industry ==9)
i_10 <- filter(Data_l3, industry ==10)
i_11 <- filter(Data_l3, industry ==11)
i_12 <- filter(Data_l3, industry ==12)
i_13 <- filter(Data_l3, industry ==13)
i_14 <- filter(Data_l3, industry ==14)
i_15 <- filter(Data_l3, industry ==15)
i_16 <- filter(Data_l3, industry ==16)

summary(i_1$clust)
summary(i_2$clust)
summary(i_3$clust)
summary(i_2$clust)
summary(i_3$clust)
summary(i_4$clust)
summary(i_5$clust)
summary(i_6$clust)
summary(i_7$clust)
summary(i_8$clust)
summary(i_9$clust)
summary(i_10$clust)
summary(i_11$clust)
summary(i_12$clust)
summary(i_13$clust)
summary(i_14$clust)
summary(i_15$clust)
summary(i_16$clust)
