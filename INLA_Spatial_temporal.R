##########
setwd("E:/ResearchWork2")
library(dplyr)
library(INLA)
library(rgdal)
#library(rgdal)
library(leaflet)
library(spdep)
#library(INLA)
library(sp)
PHU_boundary <- rgdal::readOGR(dsn="F:/Research Work/COVID-2019/Canada/ON/Ministry_of_Health_Public_Health_Unit_Boundary-shp",layer="Ministry_of_Health_Public_Health_Unit_Boundary")


data1 <- lst[[1]]@data
data2 <- lst[[2]]@data
colnames(data2)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data2 <- data2[,c(1:7,9,10,8,11:14)]
data3 <- lst[[3]]@data
data4 <- lst[[4]]@data
colnames(data4)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data4 <- data4[,c(1:7,9,10,8,11:14)]

data5 <- lst[[5]]@data
data6 <- lst[[6]]@data
colnames(data6)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data6 <- data6[,c(1:7,9,10,8,11:14)]

data7 <- lst[[7]]@data
data8 <- lst[[8]]@data
colnames(data8)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data8 <- data8[,c(1:7,9,10,8,11:14)]

data9 <- lst[[9]]@data
data10 <- lst[[10]]@data
colnames(data10)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data10 <- data10[,c(1:7,9,10,8,11:14)]

data11 <- lst[[11]]@data
data12 <- lst[[12]]@data
colnames(data12)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data12 <- data12[,c(1:7,9,10,8,11:14)]

data13 <- lst[[13]]@data
data14 <- lst[[14]]@data
colnames(data14)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data14 <- data14[,c(1:7,9,10,8,11:14)]

data15 <- lst[[15]]@data
data16 <- lst[[16]]@data
colnames(data16)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data16 <- data16[,c(1:7,9,10,8,11:14)]

data17 <- lst[[17]]@data
data18 <- lst[[18]]@data
colnames(data18)[5:6] <-  c("Age_Adjusted_Rate","Raw_Rate")
data18 <- data18[,c(1:7,9,10,8,11:14)]

data19 <- lst[[19]]@data
data20 <- lst[[20]]@data
colnames(data20)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data20 <- data20[,c(1:7,9,10,8,11:14)]

data21 <- lst[[21]]@data
data22 <- lst[[22]]@data
colnames(data22)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data22 <- data22[,c(1:7,9,10,8,11:14)]

data23 <- lst[[23]]@data
data24 <- lst[[24]]@data
colnames(data24)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data24 <- data24[,c(1:7,9,10,8,11:14)]

data25 <- lst[[25]]@data
data26 <- lst[[26]]@data
colnames(data26)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data26 <- data26[,c(1:7,9,10,8,11:14)]

data27 <- lst[[27]]@data
data28 <- lst[[28]]@data
colnames(data28)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data28 <- data28[,c(1:7,9,10,8,11:14)]

data29 <- lst[[29]]@data
data30 <- lst[[30]]@data
colnames(data30)[5:6] <- c("Age_Adjusted_Rate","Raw_Rate")
data30 <- data30[,c(1:7,9,10,8,11:14)]

data31 <- lst[[31]]@data

covid_data <- rbind(data1,data2,data3,data4,data5,data6,data7,
                    data8,data9,data10,data11,data12,data13,data14,
                    data15,data16,data17,data18,data19,data20,data21,
                    data22,data23,data24,data25,data26,data27,data28,
                    data29,data30,data31)

write.csv(covid_data,"F:/Research Work/COVID-2019/Canada/ON/data_preparation/covid_data.csv")

covid_data <- read.csv("F:/Research Work/COVID-2019/Canada/ON/data_preparation/covid_data.csv",sep=",",header = TRUE)
covid_data_1 <- subset(covid_data,PHU_ID == 2226)
ID1 <- rep(1,nrow(covid_data_1))
for(i in 2:34){
  ID1 <- c(ID1,rep(i,31))
}

date_num_str_int <- rep(c(1:31),34)
date_num_unstr_int <- rep(c(1:31),34)

date_num_str <- rep(c(1:31),34)
date_num_unstr <- rep(c(1:31),34)

region_date_num <- c(1:1054)

covid_data$ID1 <- ID1
covid_data$date_num_str_int <- date_num_str_int
covid_data$date_num_unstr_int <- date_num_unstr_int
covid_data$date_num_str <- date_num_str
covid_data$date_num_unstr <- date_num_unstr
covid_data$region_date_num <- region_date_num
covid_data$region <- ID1
census_PHU <- read.csv("F:/Research Work/COVID-2019/Canada/ON/data_preparation/2016census_PHU4.csv",header = TRUE,sep = ",")

census_PHU_ad <- read.csv("F:/Research Work/COVID-2019/Canada/ON/data_preparation/census_PHU_adjusted.csv",header = TRUE,sep = ",")
covid_census <- merge(covid_data,census_PHU_ad,by.x = "HRUID20181",by.y = "GEO_CODE")
covid_data_co <- merge(covid_data,census_PHU,by.x = "HRUID20181",by.y = "GEO_CODE")
covid_dataa <- subset(covid_data_co,Date == "2020-03-31")
PHU_covid_dataa <- merge(PHU_boundary,covid_dataa,by.x = "PHU_ID",by.y = "PHU_ID")
# PHU_covid_dataa <- merge(PHU_boundary,covid_dataa,by.x = "PHU_ID",by.y = "PHU_ID")

nb_ON1 <- poly2nb(PHU_covid_dataa)
head(nb_ON1)
nb2INLA("map.adj1", nb_ON1)
g_ON1 <- inla.read.graph(filename = "map.adj1")




formula <- cases_new~f(region,model = "bym",graph = g_ON1)+
                      f(date_num_str,model = "rw1")+
                      f(date_num_unstr,model = "iid")
modd <- inla(formula1,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE))


formula2 <- cases_new~f(region,model = "bym",graph = g_ON1)+
                      f(date_num_str,model = "rw1")+
                      f(date_num_unstr,model = "iid")+
                      f(region_date_num,model = "iid")
modd2 <- inla(formula2,family = "poisson",data=covid_data,
             E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE))

formula3 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
                        f(date_num_str,model = "rw1")+
                        f(date_num_unstr,model = "iid")+
                        f(ID1,model = "iid",group = date_num_str_int,
                          control.group = list(model="rw1"))

modd3 <- inla(formula3,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE))

formula4 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
                        f(date_num_str, model="rw1") +
                        f(date_num_unstr, model="iid") +
                        f(date_num_unstr_int, model="iid", group=ID1,
                          control.group=list(model="besag", graph=g_ON1))
modd4 <- inla(formula4,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE))

formula5 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
                        f(date_num_str, model="rw1") +
                        f(date_num_unstr, model="iid") +
                        f(ID1, 
                          model="besag", graph=g_ON1, 
                          group=date_num_str_int,
                          control.group=list(model="rw1"))
modd5 <- inla(formula5,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE))


formula6 <- cases_new~f(region,model = "bym",graph = g_ON1)+
                      f(date_num_str,model = "rw2")+
                      f(date_num_unstr,model = "iid")
modd6 <- inla(formula1,family = "poisson",data=covid_data,
             E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))
# episilon = modd6$marginals.random$region
# RR = inla.tmarginal(function(x) exp(x), episilon)
ll <- inla.tmarginal(function(x) exp(x),
               modd6$marginals.random$date_num_str$index.1)
e1 <- inla.emarginal(function(x) x, ll)
l2 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_str$index.2)
e2 <- inla.emarginal(function(x) x, l2)
l3 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_str$index.3)
e3 <- inla.emarginal(function(x) x, l3)
l4 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_str$index.4)
e4 <- inla.emarginal(function(x) x, l4)
l5 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_str$index.5)
e5 <- inla.emarginal(function(x) x, l5)
l6 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_str$index.6)
e6 <- inla.emarginal(function(x) x, l6)
l7 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_str$index.7)
e7 <- inla.emarginal(function(x) x, l7)
l8 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_str$index.8)
e8 <- inla.emarginal(function(x) x, l8)
l9 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_str$index.9)
e9 <- inla.emarginal(function(x) x, l9)
l10 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_str$index.10)
e10 <- inla.emarginal(function(x) x, l11)
l11 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.11)
e11 <- inla.emarginal(function(x) x, l11)
l12 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.12)
e12 <- inla.emarginal(function(x) x, l12)
l13 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.13)
e13 <- inla.emarginal(function(x) x, l13)
l14 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.14)
e14 <- inla.emarginal(function(x) x, l14)
l15 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.15)
e15 <- inla.emarginal(function(x) x, l15)
l16 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.16)
e16 <- inla.emarginal(function(x) x, l16)
l17 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.17)
e17 <- inla.emarginal(function(x) x, l17)
l18 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.18)
e18 <- inla.emarginal(function(x) x, l18)
l19 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.19)
e19 <- inla.emarginal(function(x) x, l19)
l20 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.20)
e20 <- inla.emarginal(function(x) x, l20)
l21 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.21)
e21 <- inla.emarginal(function(x) x, l21)
l22 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.22)
e22 <- inla.emarginal(function(x) x, l22)
l23 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.23)
e23 <- inla.emarginal(function(x) x, l23)
l24 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.24)
e24 <- inla.emarginal(function(x) x, l24)
l25 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.25)
e25 <- inla.emarginal(function(x) x, l25)
l26 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.26)
e26 <- inla.emarginal(function(x) x, l26)
l27 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.27)
e27 <- inla.emarginal(function(x) x, l27)
l28 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.28)
e28 <- inla.emarginal(function(x) x, l28)
l29 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.29)
e29 <- inla.emarginal(function(x) x, l29)
l30 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.30)
e30 <- inla.emarginal(function(x) x, l30)


l31 <- inla.tmarginal(function(x) exp(x),
                      modd6$marginals.random$date_num_str$index.31)
e31 <- inla.emarginal(function(x) x, l31)
############
s1 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_unstr$index.1)
inla.emarginal(function(x) x, s1)
s31 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_unstr$index.31)
############
inla.emarginal(function(x) x, s31)
r1 <- inla.tmarginal(function(x) exp(x),
               (modd6$marginals.random$region$index.1+modd6$marginals.random$region$index.35))
inla.emarginal(function(x) x, r1)
RR_date_str <- rep(0,31)
RR_date_str[1] <- e1
RR_date_str[2] <- e2
RR_date_str[3] <- e3
RR_date_str[4] <- e4
RR_date_str[5] <- e5
RR_date_str[6] <- e6
RR_date_str[7] <- e7
RR_date_str[8] <- e8
RR_date_str[9] <- e9
RR_date_str[10] <- e10
RR_date_str[11] <- e11
RR_date_str[12] <- e12
RR_date_str[13] <- e13
RR_date_str[14] <- e14
RR_date_str[15] <- e15
RR_date_str[16] <- e16
RR_date_str[17] <- e17
RR_date_str[18] <- e18
RR_date_str[19] <- e19
RR_date_str[20] <- e20
RR_date_str[21] <- e21
RR_date_str[22] <- e22
RR_date_str[23] <- e23
RR_date_str[24] <- e24
RR_date_str[25] <- e25
RR_date_str[26] <- e26
RR_date_str[27] <- e27
RR_date_str[28] <- e28
RR_date_str[29] <- e29
RR_date_str[30] <- e30
RR_date_str[31] <- e31
Date <- covid_data_1$Date
Date_RR_datestr <- data.frame(Date,RR_date_str)
write.csv(Date_RR_datestr,"F:/Research Work/COVID-2019/Canada/ON/data_preparation/Date_RR_datestr.csv")
##############
sl <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_unstr$index.1)
rs1 <- inla.emarginal(function(x) x, sl)
s2 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_unstr$index.2)
rs2 <- inla.emarginal(function(x) x, s2)
s3 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_unstr$index.3)
rs3 <- inla.emarginal(function(x) x, s3)
s4 <- inla.tmarginal(function(x) exp(x),
                     modd6$marginals.random$date_num_unstr$index.4)
rs4 <- inla.emarginal(function(x) x, s4)
RR_dateunstr <- rep(0,31)
llst <- list(NULL)
length(llst) <- 31
for(i in 1:31){
  llst[[i]] <- inla.tmarginal(function(x) exp(x),
                              modd6$marginals.random$date_num_unstr[[i]])
}
for(i in 1:31){
  RR_dateunstr[i] <- inla.emarginal(function(x) x,llst[[i]])
}
Date_RR_datestr$RR_date_unstr <- RR_dateunstr
colnames(Date_RR_datestr) <- c("Date","Structured","Unstructured")
# Date_RR_datestr2 <- Date_RR_datestr %>%
#   gather(Structured, Unstructured, key = "type", value = "relative risk") %>%
#   mutate("type" = factor(
#     "type", levels = c("Structured", "Unstructured"),
#     labels = c("Structured", "Unstructured")))
# p <- ggplot(data = Date_RR_datestr2, mapping = aes(
#   x = Date, y = "relative risk", color = "type"))
# p + geom_line()
Date_RR_datestr$Date <- as.Date(Date_RR_datestr$Date)
g <- ggplot()+geom_line(aes(x=Date,y=Structured),data=Date_RR_datestr,color = "grey40")
Date_RR_datestr2 <- gather(Date_RR_datestr,key = "type",value = "relative_risk","Structured","Unstructured")
Date_RR_datestr2[,2] <- as.factor(Date_RR_datestr2[,2])

g <- ggplot(Date_RR_datestr2,aes(x=Date,y=relative_risk,color=type,group=type))+
  geom_line()+
  labs(x = "", y= "", titles = "Infectied Risks at the bi-week level (Model2)")+
  theme_bw()
  
  





write.csv(Date_RR_datestr,"F:/Research Work/COVID-2019/Canada/ON/data_preparation/Date_RR_datestr.csv")
library(ggplot2)
library(tidyr)
# inla <- unlist(lapply(modd6$marginals.random$date_num_str, function(X){
#   inla.tmarginal(function(X) exp(X))
# }))
###############
formula7 <- cases_new~f(region,model = "bym",graph = g_ON1)+
                      f(date_num_str,model = "rw2")+
                      f(date_num_unstr,model = "iid")+
                      f(region_date_num,model = "iid")
modd7 <- inla(formula2,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula8 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
                        f(date_num_str,model = "rw2")+
                        f(date_num_unstr,model = "iid")+
                        f(ID1,model = "iid",group = date_num_str_int,
                          control.group = list(model="rw2"))

modd8 <- inla(formula8,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula9 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
                        f(date_num_str, model="rw2") +
                        f(date_num_unstr, model="iid") +
                        f(date_num_unstr_int, model="iid", group=ID1,
                          control.group=list(model="besag", graph=g_ON1))
modd9 <- inla(formula9,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula10 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
                         f(date_num_str, model="rw2") +
                         f(date_num_unstr, model="iid") +
                         f(ID1, model="besag", graph=g_ON1, 
                           group=date_num_str_int,control.group=list(model="rw2"))
modd10 <- inla(formula10,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

lst_spatialtemporalmodels <- list(NULL)
length(lst_spatialtemporalmodels) <- 5
lst_spatialtemporalmodels[[1]] <- modd6
lst_spatialtemporalmodels[[2]] <- modd7
lst_spatialtemporalmodels[[3]] <- modd8
lst_spatialtemporalmodels[[4]] <- modd9
lst_spatialtemporalmodels[[5]] <- modd10

saveRDS(lst_spatialtemporalmodels,"F:/Research Work/COVID-2019/Canada/ON/data_preparation/lst_spatialtemporalmodels.RDS")
RR_datestr8 <- rep(0,31)
llst8 <- list(NULL)
length(llst8) <- 31
for(i in 1:31){
  llst8[[i]] <- inla.tmarginal(function(x) exp(x),
                              modd8$marginals.random$date_num_str[[i]])
}
for(i in 1:31){
  RR_datestr8[i] <- inla.emarginal(function(x) x,llst8[[i]])
}

RR_dateunstr8 <- rep(0,31)
llstun8 <- list(NULL)
length(llstun8) <- 31
for(i in 1:31){
  llstun8[[i]] <- inla.tmarginal(function(x) exp(x),
                               modd8$marginals.random$date_num_unstr[[i]])
}
for(i in 1:31){
  RR_dateunstr8[i] <- inla.emarginal(function(x) x,llstun8[[i]])
}
Date_RR_date8 <- data.frame(Date,RR_datestr8,RR_dateunstr8)
write.csv(Date_RR_date8,"F:/Research Work/COVID-2019/Canada/ON/data_preparation/Date_RR_date8.csv")

#######
RR_datestr10 <- rep(0,31)
llst10 <- list(NULL)
length(llst8) <- 31
for(i in 1:31){
  llst10[[i]] <- inla.tmarginal(function(x) exp(x),
                               modd10$marginals.random$date_num_str[[i]])
}
for(i in 1:31){
  RR_datestr10[i] <- inla.emarginal(function(x) x,llst10[[i]])
}

RR_dateunstr10 <- rep(0,31)
llstun10 <- list(NULL)
length(llstun10) <- 31
for(i in 1:31){
  llstun10[[i]] <- inla.tmarginal(function(x) exp(x),
                                 modd10$marginals.random$date_num_unstr[[i]])
}
for(i in 1:31){
  RR_dateunstr10[i] <- inla.emarginal(function(x) x,llstun10[[i]])
}
Date_RR_date10 <- data.frame(Date,RR_datestr10,RR_dateunstr10)
write.csv(Date_RR_date10,"F:/Research Work/COVID-2019/Canada/ON/data_preparation/Date_RR_date10.csv")

###########

formula11 <- cases_new~Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")
modd11 <- inla(formula11,family = "poisson",data=covid_data_co,
             E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


formula22 <- cases_new~Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")+
  f(region_date_num,model = "iid")
modd22 <- inla(formula22,family = "poisson",data=covid_data_co,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula33 <- cases_new ~ Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")+
  f(ID1,model = "iid",group = date_num_str_int,
    control.group = list(model="rw2"))

modd33 <- inla(formula33,family = "poisson",data=covid_data_co,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula44 <- cases_new ~ Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw2") +
  f(date_num_unstr, model="iid") +
  f(date_num_unstr_int, model="iid", group=ID1,
    control.group=list(model="besag", graph=g_ON1))
modd44 <- inla(formula44,family = "poisson",data=covid_data_co,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula55 <- cases_new ~ Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw2") +
  f(date_num_unstr, model="iid") +
  f(ID1, model="besag", graph=g_ON1, 
    group=date_num_str_int,
    control.group=list(model="rw2"))
modd55 <- inla(formula55,family = "poisson",data=covid_data_co,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


# formula66 <- cases_new~Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
#   f(region,model = "bym",graph = g_ON1)+
#   f(date_num_str,model = "rw2")+
#   f(date_num_unstr,model = "iid")
# modd66 <- inla(formula66,family = "poisson",data=covid_data_co,
#               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula111 <- cases_new~Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "ar1")+
  f(date_num_unstr,model = "iid")
modd111 <- inla(formula111,family = "poisson",data=covid_data_co,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


formula222 <- cases_new~Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "ar1")+
  f(date_num_unstr,model = "iid")+
  f(region_date_num,model = "iid")
modd222 <- inla(formula222,family = "poisson",data=covid_data_co,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula333 <- cases_new ~ Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "ar1")+
  f(date_num_unstr,model = "iid")+
  f(ID1,model = "iid",group = date_num_str_int,
    control.group = list(model="ar1"))

modd333 <- inla(formula333,family = "poisson",data=covid_data_co,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula444 <- cases_new ~ Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="ar1") +
  f(date_num_unstr, model="iid") +
  f(date_num_unstr_int, model="iid", group=ID1,
    control.group=list(model="besag", graph=g_ON1))
modd444 <- inla(formula444,family = "poisson",data=covid_data_co,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula555 <- cases_new ~ Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="ar1") +
  f(date_num_unstr, model="iid") +
  f(ID1, model="besag", graph=g_ON1, 
    group=date_num_str_int,
    control.group=list(model="ar1"))
modd555 <- inla(formula555,family = "poisson",data=covid_data_co,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))
#############
formula1111 <- cases_new~Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw1")+
  f(date_num_unstr,model = "iid")
modd1111 <- inla(formula1111,family = "poisson",data=covid_data_co,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


formula2222 <- cases_new~Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw1")+
  f(date_num_unstr,model = "iid")+
  f(region_date_num,model = "iid")
modd2222 <- inla(formula2222,family = "poisson",data=covid_data_co,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula3333 <- cases_new ~ Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw1")+
  f(date_num_unstr,model = "iid")+
  f(ID1,model = "iid",group = date_num_str_int,
    control.group = list(model="rw1"))

modd3333 <- inla(formula3333,family = "poisson",data=covid_data_co,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula4444 <- cases_new ~ Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw1") +
  f(date_num_unstr, model="iid") +
  f(date_num_unstr_int, model="iid", group=ID1,
    control.group=list(model="besag", graph=g_ON1))
modd4444 <- inla(formula4444,family = "poisson",data=covid_data_co,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula5555 <- cases_new ~ Apartment+Other_attached+X0_17_Years+X18_64_Years+X65_years+Average_household_size+Self_employed+Population_density+Health_occupations+X65_years2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw1") +
  f(date_num_unstr, model="iid") +
  f(ID1, model="besag", graph=g_ON1, 
    group=date_num_str_int,
    control.group=list(model="rw1"))
modd5555 <- inla(formula5555,family = "poisson",data=covid_data_co,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


formula1_rw2 <- cases_new~f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")
modd_rw2 <- inla(formula1_rw2,family = "poisson",data=covid_data,
             E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


formula2_rw2 <- cases_new~f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")+
  f(region_date_num,model = "iid")
modd2_rw2 <- inla(formula2_rw2,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula3_rw2 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")+
  f(ID1,model = "iid",group = date_num_str_int,
    control.group = list(model="rw2"))

modd3_rw2 <- inla(formula3_rw2,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula4_rw2 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw2") +
  f(date_num_unstr, model="iid") +
  f(date_num_unstr_int, model="iid", group=ID1,
    control.group=list(model="besag", graph=g_ON1))
modd4_rw2 <- inla(formula4_rw2,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula5_rw2 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw2") +
  f(date_num_unstr, model="iid") +
  f(ID1, 
    model="besag", graph=g_ON1, 
    group=date_num_str_int,
    control.group=list(model="rw2"))
modd5_rw2 <- inla(formula5_rw2,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


###############
covid_censuss <- subset(covid_census,Date == "2020-03-31")
PHU_covid_census <- merge(PHU_boundary,covid_censuss,by.x = "PHU_ID",by.y = "PHU_ID")
# PHU_covid_dataa <- merge(PHU_boundary,covid_dataa,by.x = "PHU_ID",by.y = "PHU_ID")

nb_ON1 <- poly2nb(PHU_covid_census)
head(nb_ON1)
nb2INLA("map.adj1", nb_ON1)
g_ON1 <- inla.read.graph(filename = "map.adj1")

############
formulas1 <- cases_new~Apartment_Proportion+Other_Proportion+Apartment2_Proportion+Average_Household_size+Low_income+X0_17Years+X18_65Years+X65Years_over+Self_employed_Proportion+Population_Density+Health_occupations_proportion+X65_Years._over+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")
modds1 <- inla(formulas1,family = "poisson",data=covid_census,
                 E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


formulas2 <- cases_new~Apartment_Proportion+Other_Proportion+Apartment2_Proportion+Average_Household_size+Low_income+X0_17Years+X18_65Years+X65Years_over+Self_employed_Proportion+Population_Density+Health_occupations_proportion+X65_Years._over+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")+
  f(region_date_num,model = "iid")
modds2 <- inla(formulas2,family = "poisson",data=covid_census,
                 E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formulas3 <- cases_new ~ Apartment_Proportion+Other_Proportion+Apartment2_Proportion+Average_Household_size+Low_income+X0_17Years+X18_65Years+X65Years_over+Self_employed_Proportion+Population_Density+Health_occupations_proportion+X65_Years._over+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")+
  f(ID1,model = "iid",group = date_num_str_int,
    control.group = list(model="rw2"))

modds3 <- inla(formulas3,family = "poisson",data=covid_census,
                 E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formulas4 <- cases_new ~ Apartment_Proportion+Other_Proportion+Apartment2_Proportion+Average_Household_size+Low_income+X0_17Years+X18_65Years+X65Years_over+Self_employed_Proportion+Population_Density+Health_occupations_proportion+X65_Years._over+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw2") +
  f(date_num_unstr, model="iid") +
  f(date_num_unstr_int, model="iid", group=ID1,
    control.group=list(model="besag", graph=g_ON1))
modds4 <- inla(formulas4,family = "poisson",data=covid_census,
                 E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formulas5 <- cases_new ~ Apartment_Proportion+Other_Proportion+Apartment2_Proportion+Average_Household_size+Low_income+X0_17Years+X18_65Years+X65Years_over+Self_employed_Proportion+Population_Density+Health_occupations_proportion+X65_Years._over+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw2") +
  f(date_num_unstr, model="iid") +
  f(ID1, model="besag", graph=g_ON1, 
    group=date_num_str_int,
    control.group=list(model="rw2"))
modds5 <- inla(formulas5,family = "poisson",data=covid_census,
                 E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


