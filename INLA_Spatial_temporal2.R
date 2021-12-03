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

########
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
########
census_PHU_ad <- read.csv("F:/Research Work/COVID-2019/Canada/ON/data_preparation/census_PHU_adjusted.csv",header = TRUE,sep = ",")
covid_census <- merge(covid_data,census_PHU_ad,by.x = "HRUID20181",by.y = "GEO_CODE")
#########
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


lst_spt2 <- list(NULL)
length(lst_spt2) <- 5
lst_spt2[[1]] <- modds1
lst_spt2[[2]] <- modds2
lst_spt2[[3]] <- modds3
lst_spt2[[4]] <- modds4
lst_spt2[[5]] <- modds5
############
library(grDevices)
coefficients <- c("beta1","beta2","beta3","beta4","beta5",
                  "beta6","beta7","beta8","beta9","beta10",
                  "beta11","beta12")

effects_fixed1 <- modds1$summary.fixed[2:13,]
effects_fixed1$coefficients <- coefficients


p1 <- ggplot(data = effects_fixed1, mapping = aes(
  x = coefficients,
  y = mean ))+
  geom_point(size = 1.2) +
  geom_pointrange(mapping = aes(
    ymin = mean - sd,
    ymax = mean + sd)) +
  theme_bw()+
  labs(x = "Coefficients",
       y = "Estimated coefficient value")

ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/coefficients1.pdf", p1)

effects_fixed2 <- modds2$summary.fixed[2:13,]
effects_fixed2$coefficients <- coefficients


p2 <- ggplot(data = effects_fixed2, mapping = aes(
  x = coefficients,
  y = mean ))+
  geom_point(size = 1.2) +
  geom_pointrange(mapping = aes(
    ymin = mean - sd,
    ymax = mean + sd)) +
  theme_bw()+
  labs(x = "Coefficients",
       y = "Estimated coefficient value")
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/coefficients2.pdf", p2)


effects_fixed3 <- modds3$summary.fixed[2:13,]
effects_fixed3$coefficients <- coefficients


p3 <- ggplot(data = effects_fixed3, mapping = aes(
  x = coefficients,
  y = mean ))+
  geom_point(size = 1.2) +
  geom_pointrange(mapping = aes(
    ymin = mean - sd,
    ymax = mean + sd)) +
  theme_bw()+
  labs(x = "Coefficients",
       y = "Estimated coefficient value")
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/coefficients3.pdf", p3)

effects_fixed4 <- modds4$summary.fixed[2:13,]
effects_fixed4$coefficients <- coefficients


p4 <- ggplot(data = effects_fixed4, mapping = aes(
  x = coefficients,
  y = mean ))+
  geom_point(size = 1.2) +
  geom_pointrange(mapping = aes(
    ymin = mean - sd,
    ymax = mean + sd)) +
  theme_bw()+
  labs(x = "Coefficients",
       y = "Estimated coefficient value")
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/coefficients4.pdf", p4)

effects_fixed5 <- modds5$summary.fixed[2:13,]
effects_fixed5$coefficients <- coefficients


p5 <- ggplot(data = effects_fixed5, mapping = aes(
  x = coefficients,
  y = mean ))+
  geom_point(size = 1.2) +
  geom_pointrange(mapping = aes(
    ymin = mean - sd,
    ymax = mean + sd)) +
  theme_bw()+
  labs(x = "Coefficients",
       y = "Estimated coefficient value")

ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/coefficients5.pdf", p5)

##############
datestr9 <- rep(0,31)
llst9 <- list(NULL)
length(llst9) <- 31
for(i in 1:31){
  llst9[[i]] <- inla.tmarginal(function(x) exp(x),
                               lst_spt2[[2]]$marginals.random$date_num_str[[i]])
}
for(i in 1:31){
  datestr9[i] <- inla.emarginal(function(x) x,llst9[[i]])
}

dateunstr9 <- rep(0,31)
llstun9 <- list(NULL)
length(llstun9) <- 31
for(i in 1:31){
  llstun9[[i]] <- inla.tmarginal(function(x) exp(x),
                                 lst_spt2[[2]]$marginals.random$date_num_unstr[[i]])
}
for(i in 1:31){
  dateunstr9[i] <- inla.emarginal(function(x) x,llstun9[[i]])
}
Date <- covid_data_1$Date

temporal9 <- data.frame(Date,datestr9,dateunstr9)
colnames(temporal9) <- c("Date","Structured","Unstructured")
library(reshape)
library(ggplot2)
library(scales)
melt_temporal9 <- melt(temporal9, id = "Date")
colnames(melt_temporal9) <- c("Date","type","value")
melt_temporal9$Date <- as.Date(melt_temporal9$Date)
melt_temporal9$Date <- as.POSIXct(melt_temporal9$Date)
p1 <- ggplot(data = melt_temporal9, aes(x=Date,y=value,group=type,color=type,shape=type))+
  geom_line()+
  xlab("Date")+
  ylab("")+
  theme_bw()+
  theme(legend.position = c(0.105,0.895))+
  scale_x_datetime(breaks = date_breaks("45 days"),labels = date_format("%m/%d"))
ggsave(p1, file='F:/Research Work/COVID-2019/Canada/ON/data_preparation/p1.pdf', width=12, height=10)

#####
datestr9 <- rep(0,31)
llst9 <- list(NULL)
length(llst9) <- 31
for(i in 1:31){
  llst9[[i]] <- inla.tmarginal(function(x) exp(x),
                               modds3$marginals.random$date_num_str[[i]])
}
for(i in 1:31){
  datestr9[i] <- inla.emarginal(function(x) x,llst9[[i]])
}

dateunstr9 <- rep(0,31)
llstun9 <- list(NULL)
length(llstun9) <- 31
for(i in 1:31){
  llstun9[[i]] <- inla.tmarginal(function(x) exp(x),
                                 modds3$marginals.random$date_num_unstr[[i]])
}
for(i in 1:31){
  dateunstr9[i] <- inla.emarginal(function(x) x,llstun9[[i]])
}
Date <- covid_data_1$Date

temporal9 <- data.frame(Date,datestr9,dateunstr9)
colnames(temporal9) <- c("Date","Structured","Unstructured")
library(reshape)
library(ggplot2)
library(scales)
melt_temporal9 <- melt(temporal9, id = "Date")
colnames(melt_temporal9) <- c("Date","type","value")
melt_temporal9$Date <- as.Date(melt_temporal9$Date)
write.csv(melt_temporal9,"E:/ResearchWork2/Shinyappdata/Bayesian/temporal_model9.csv")
# melt_temporal9$Date <- as.POSIXct(melt_temporal9$Date)
# p1 <- ggplot(data = melt_temporal9, aes(x=Date,y=value,group=type,color=type,shape=type))+
#   geom_line()+
#   xlab("Date")+
#   ylab("")+
#   theme_bw()+
#   theme(legend.position = c(0.105,0.895))+
#   scale_x_datetime(breaks = date_breaks("45 days"),labels = date_format("%m/%d"))
# ggsave(p1, file='F:/Research Work/COVID-2019/Canada/ON/data_preparation/p1.pdf', width=12, height=10)

covid_col = "#cc4c02"
covid_other_col = "#662506"
g3 = ggplot(melt_temporal9, aes(x=Date,y=value,group=type,color=type,shape=type)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
  # geom_bar(position="stack", stat="identity") + 
  ylab("Temporal Effect") + xlab("Date") + theme_bw() + 
  scale_colour_manual(values=c(covid_col,covid_other_col)) +
  theme(legend.title = element_blank(), legend.position = c(0.105,0.895), plot.title = element_text(size=10), 
        plot.margin = margin(5, 12, 5, 5))
g3

######
formula <- cases_new~f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")
modd <- inla(formula,family = "poisson",data=covid_data,
             E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


formula2 <- cases_new~f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")+
  f(region_date_num,model = "iid")
modd2 <- inla(formula2,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula3 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")+
  f(ID1,model = "iid",group = date_num_str_int,
    control.group = list(model="rw2"))

modd3 <- inla(formula3,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula4 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw2") +
  f(date_num_unstr, model="iid") +
  f(date_num_unstr_int, model="iid", group=ID1,
    control.group=list(model="besag", graph=g_ON1))
modd4 <- inla(formula4,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formula5 <- cases_new ~ f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw2") +
  f(date_num_unstr, model="iid") +
  f(ID1, 
    model="besag", graph=g_ON1, 
    group=date_num_str_int,
    control.group=list(model="rw2"))
modd5 <- inla(formula5,family = "poisson",data=covid_data,
              E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


###############
datestr5 <- rep(0,31)
llst5 <- list(NULL)
length(llst5) <- 31
for(i in 1:31){
  llst5[[i]] <- inla.tmarginal(function(x) exp(x),
                               modd3$marginals.random$date_num_str[[i]])
}
for(i in 1:31){
  datestr5[i] <- inla.emarginal(function(x) x,llst5[[i]])
}

dateunstr5 <- rep(0,31)
llstun5 <- list(NULL)
length(llstun5) <- 31
for(i in 1:31){
  llstun5[[i]] <- inla.tmarginal(function(x) exp(x),
                                 modd3$marginals.random$date_num_unstr[[i]])
}
for(i in 1:31){
  dateunstr5[i] <- inla.emarginal(function(x) x,llstun5[[i]])
}
Date <- covid_data_1$Date
temporal5 <- data.frame(Date,datestr5,dateunstr5)
colnames(temporal5) <- c("Date","Structured","Unstructured")
library(reshape)
library(ggplot2)
library(scales)
melt_temporal5 <- melt(temporal5, id = "Date")
colnames(melt_temporal5) <- c("Date","type","value")
melt_temporal5$Date <- as.Date(melt_temporal5$Date)
melt_temporal5$Date <- as.POSIXct(melt_temporal5$Date)
p1 <- ggplot(data = melt_temporal5, aes(x=Date,y=value,group=type,color=type,shape=type))+
  geom_line()+
  xlab("Date")+
  ylab("")+
  theme_bw()+
  theme(legend.position = c(0.105,0.895))+
  scale_x_datetime(breaks = date_breaks("45 days"),labels = date_format("%m/%d"))
ggsave(p1, file='F:/Research Work/COVID-2019/Canada/ON/data_preparation/p2.pdf', width=12, height=10)

###########
datestr <- rep(0,31)
llst <- list(NULL)
length(llst) <- 31
for(i in 1:31){
  llst[[i]] <- inla.tmarginal(function(x) exp(x),
                               modd$marginals.random$date_num_str[[i]])
}
for(i in 1:31){
  datestr[i] <- inla.emarginal(function(x) x,llst[[i]])
}

dateunstr <- rep(0,31)
llstun <- list(NULL)
length(llstun) <- 31
for(i in 1:31){
  llstun[[i]] <- inla.tmarginal(function(x) exp(x),
                                 modds1$marginals.random$date_num_unstr[[i]])
}
for(i in 1:31){
  dateunstr[i] <- inla.emarginal(function(x) x,llstun[[i]])
}
Date <- covid_data_1$Date
temporal <- data.frame(Date,datestr,dateunstr)
colnames(temporal) <- c("Date","Structured","Unstructured")
library(reshape)
library(ggplot2)
library(scales)
melt_temporal <- melt(temporal, id = "Date")
colnames(melt_temporal) <- c("Date","type","value")
melt_temporal$Date <- as.Date(melt_temporal$Date)
melt_temporal$Date <- as.POSIXct(melt_temporal$Date)
p3 <- ggplot(data = melt_temporal, aes(x=Date,y=value,group=type,color=type,shape=type))+
  geom_line()+
  xlab("Date")+
  ylab("")+
  theme_bw()+
  theme(legend.position = c(0.105,0.895))+
  scale_x_datetime(breaks = date_breaks("45 days"),labels = date_format("%m/%d"))
ggsave(p1, file='F:/Research Work/COVID-2019/Canada/ON/data_preparation/p2.pdf', width=12, height=10)

#########
datestr11 <- rep(0,31)
llst11 <- list(NULL)
length(llst11) <- 31
for(i in 1:31){
  llst11[[i]] <- inla.tmarginal(function(x) exp(x),
                              modds1$marginals.random$date_num_str[[i]])
}
for(i in 1:31){
  datestr11[i] <- inla.emarginal(function(x) x,llst11[[i]])
}

dateunstr11 <- rep(0,31)
llstun11 <- list(NULL)
length(llstun11) <- 31
for(i in 1:31){
  llstun11[[i]] <- inla.tmarginal(function(x) exp(x),
                                modds1$marginals.random$date_num_unstr[[i]])
}
for(i in 1:31){
  dateunstr11[i] <- inla.emarginal(function(x) x,llstun11[[i]])
}
Date <- covid_data_1$Date
temporal11 <- data.frame(Date,datestr11,dateunstr11)
colnames(temporal11) <- c("Date","Structured","Unstructured")
library(reshape)
library(ggplot2)
library(scales)
melt_temporal11 <- melt(temporal11, id = "Date")
colnames(melt_temporal11) <- c("Date","type","value")
melt_temporal11$Date <- as.Date(melt_temporal11$Date)
melt_temporal11$Date <- as.POSIXct(melt_temporal11$Date)
p4 <- ggplot(data = melt_temporal11, aes(x=Date,y=value,group=type,color=type,shape=type))+
  geom_line()+
  xlab("Date")+
  ylab("")+
  theme_bw()+
  theme(legend.position = c(0.105,0.895))+
  scale_x_datetime(breaks = date_breaks("45 days"),labels = date_format("%m/%d"))
ggsave(p4, file='F:/Research Work/COVID-2019/Canada/ON/data_preparation/p4.pdf', width=12, height=10)

post.var <- inla.tmarginal(function(x) exp(x), 
                           modds3$marginals.random$region$index.1)
View(modds3$summary.fitted.values)
fitted_values <- modds3$summary.fitted.values
covid_census_result <- cbind(covid_census,fitted_values)
write.csv(covid_census_result,"E:/ResearchWork2/Shinyappdata/Bayesian/covid_cum.csv")
covid_census_result1 <- subset(covid_census_result, Date == "2020-03-31")

covid_census_resultt <- list(NULL)
length(covid_census_resultt) <- 31
for(i in 1:31){
  covid_census_resultt[[i]] <- merge(PHU_boundary,(subset(covid_census_result, Date == Date[i])),by.x = "PHU_ID","PHU_ID")
}
library(sf)
covid_census_resultt1 <- st_as_sf(covid_census_resultt[[1]])
plot(covid_census_resultt1["mean"], 
     main = "Philadelphia homicide density per square km", 
     breaks = "quantile")
library(RColorBrewer)
pal <- brewer.pal(10, "Spectral") # we select 7 colors from the palette
class(pal)
plot(covid_census_resultt1["mean"], 
     main = "Philadelphia homicide density per square km", 
     breaks = c(0,10,30,50,100,200,500,1000,2000,4000,6500), 
     pal = pal)
library(classInt)
breaks_qt <- classIntervals(covid_census_result$mean, n = 11, style = "quantile")
br <- breaks_qt$brks 
offs <- 0.0000001 
br[1] <- br[1] - offs 
br[length(br)] <- br[length(br)] + offs 
# categoreis for choropleth map
covid_census_resultt[[1]]$mean_bracket <- cut(covid_census_resultt[[1]]$mean, br)
# plot
library(sp)
spplot(covid_census_resultt[[1]], "mean_bracket", col.regions=pal, main = "Philadelphia homicide density per square km")
library(tigris)
library(tidyverse)
#install.packages("vroom")
library(vroom)
library(sp)
library(sf)
library(tigris)
library(leaflet)
library(htmlwidgets)
PHU_boundary <- st_read("F:/Research Work/COVID-2019/Canada/ON/Ministry_of_Health_Public_Health_Unit_Boundary-shp/Ministry_of_Health_Public_Health_Unit_Boundary.shp")

all_data_census <- geo_join(PHU_boundary,covid_census_result,'PHU_ID','PHU_ID',how = "inner")
saveRDS(all_data_census,"F:/Research Work/COVID-2019/Canada/ON/data_preparation/all_data_census.RDS")
all_data_census <- readRDS("F:/Research Work/COVID-2019/Canada/ON/data_preparation/all_data_census.RDS")
library(tmap)
tm_shape(all_data_census) +
  tm_borders() +
  tm_facets(by = "Date")


l_dw <- tm_shape(all_data_census) + tm_fill("mean", breaks=c(0,10,30,50,100,300,500,1000,2000,4000,6000,8000), Palette = "Reds", title = "Estimated Infected Risk \n (cases per 100,000)") +
  tm_borders(alpha=.4) +
  tm_facets(by = "Date")+
  tm_layout(legend.text.size = 0.76,legend.title.size = 1, frame = FALSE, legend.outside = TRUE,
            legend.show = TRUE, legend.hist.height = 2)
tmap_save(l_dw,"F:/Research Work/COVID-2019/Canada/ON/data_preparation/tmap1_1.pdf")

library(ggplot2)
library(latex2exp)
library(berryFunctions)

plot1 <- ggplot(as.data.frame(modds3$marginals.fixed$Apartment_Proportion)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(pi, "(", betaG5, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot1.pdf", plot1)

plot2 <- ggplot(as.data.frame(modds3$marginals.fixed$Other_Proportion)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(pi, "(", betaG4, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot2.pdf", plot2)

plot3 <- ggplot(as.data.frame(modds3$marginals.fixed$Apartment2_Proportion)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(pi, "(", betaG3, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot3.pdf", plot3)

plot4 <- ggplot(as.data.frame(modds3$marginals.fixed$Average_Household_size)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(pi, "(", betaE2, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot4.pdf", plot4)




plot5 <- ggplot(as.data.frame(modds3$marginals.fixed$Low_income)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(pi, "(", betaE3, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot5.pdf", plot5)


plot6 <- ggplot(as.data.frame(modds3$marginals.fixed$X0_17Years)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(pi, "(", betaG6, " | ", bold(y), ")")))

coefficients <- c("beta1","beta2","beta3","beta4","beta5",
                  "beta6","beta7","beta8","beta9","beta10",
                  "beta11","beta12")

effects_fixed <- modds3$summary.fixed[2:13,]
effects_fixed$coefficients <- coefficients


p <- ggplot(data = effects_fixed, mapping = aes(
  x = coefficients,
  y = mean ))+
 geom_point(size = 1.2) +
  geom_pointrange(mapping = aes(
    ymin = mean - sd,
    ymax = mean + sd)) +
  theme_bw()+
  labs(x = "Coefficients",
       y = "Estimated coefficient value")

ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/coefficients.pdf", p)

Jan21 <- st_read("F:/Research Work/COVID-2019/Canada/ON/ATP/polygons/smoothed_Jan21/smoothed_polygons.shp")
tm_shape(Jan21) + tm_fill("local_risk", breaks=c(0,10,30,50,100,300,500,1000,2000,4000,6000,8000), Palette = "Reds", title = "Estimated Infected Risk \n (cases per 100,000)") +
  tm_borders(alpha=.4) +
  tm_layout(legend.text.size = 0.4,legend.title.size = 1, frame = FALSE, legend.outside = TRUE,
            legend.show = TRUE, legend.hist.height = 2)
Jan21_polygon <- st_read("F:/Research Work/COVID-2019/Canada/ON/ATA/smoothed_ata_Jan/Jan_220/smoothed.shp")
tm_shape(Jan21_polygon) + tm_fill("reg.est", breaks=c(0,10,30,50,100,300,500,1000,2000,4000,6000,8000), Palette = "Reds", title = "Estimated Infected Risk \n (cases per 100,000)") +
  tm_borders(alpha=.4) +
  tm_layout(legend.text.size = 0.4,legend.title.size = 1, frame = FALSE, legend.outside = TRUE,
            legend.show = TRUE, legend.hist.height = 2)
