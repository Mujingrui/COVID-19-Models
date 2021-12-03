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
covid_biweekly_census2 <- subset(covid_biweekly_census,covid_biweekly_census$Date>="2020-03-31")
interaction <- covid_data2[,15:24]
interaction <- interaction[1:1054,]
covid_biweekly_census2_interaction <- cbind(covid_biweekly_census2,interaction)
#####
covid_data2 <- read.csv("F:/Research Work/COVID-2019/Canada/ON/data_preparation/covid_data2.csv",sep=",",header = TRUE)
covid_data2 <- covid_data2[1:1054,]
covid_data2_1 <- subset(covid_data2,PHU_ID == 2226)
ID1 <- rep(1,nrow(covid_data2_1))
for(i in 2:34){
  ID1 <- c(ID1,rep(i,31))
}

date_num_str_int <- rep(c(1:31),34)
date_num_unstr_int <- rep(c(1:31),34)

date_num_str <- rep(c(1:31),34)
date_num_unstr <- rep(c(1:31),34)

region_date_num <- c(1:1054)

covid_data2$ID1 <- ID1
covid_data2$date_num_str_int <- date_num_str_int
covid_data2$date_num_unstr_int <- date_num_unstr_int
covid_data2$date_num_str <- date_num_str
covid_data2$date_num_unstr <- date_num_unstr
covid_data2$region_date_num <- region_date_num
covid_data2$region <- ID1

covid_biweekly_census2_interaction$ID1 <- ID1
covid_biweekly_census2_interaction$date_num_str_int <- date_num_str_int
covid_biweekly_census2_interaction$date_num_unstr_int <- date_num_unstr_int
covid_biweekly_census2_interaction$date_num_str <- date_num_str
covid_biweekly_census2_interaction$date_num_unstr <- date_num_unstr
covid_biweekly_census2_interaction$region_date_num <- region_date_num
covid_biweekly_census2_interaction$region <- ID1
covid_biweekly_census2_interaction$cases_new <- round(covid_biweekly_census2_interaction$Age_Adjusted_Rate1*(covid_biweekly_census2_interaction$Population2019/100000),0)

census_PHU_ad <- read.csv("F:/Research Work/COVID-2019/Canada/ON/data_preparation/census_PHU_adjusted.csv",header = TRUE,sep = ",")
covid_census2 <- merge(covid_data2,census_PHU_ad,by.x = "HRUID20181",by.y = "GEO_CODE")
#########
covid_censuss2 <- subset(covid_census2,Date == "2020-03-31")
PHU_covid_census2 <- merge(PHU_boundary,covid_censuss2,by.x = "PHU_ID",by.y = "PHU_ID")
# PHU_covid_dataa <- merge(PHU_boundary,covid_dataa,by.x = "PHU_ID",by.y = "PHU_ID")

nb_ON1 <- poly2nb(PHU_covid_census2)
head(nb_ON1)
nb2INLA("map.adj1", nb_ON1)
g_ON1 <- inla.read.graph(filename = "map.adj1")

###########
formulas11 <- cases_new~Apartment_Proportion+Other_Proportion+Apartment2_Proportion+Average_Household_size+Low_income+X0_17Years+X18_65Years+X65Years_over+Self_employed_Proportion+Population_Density+Health_occupations_proportion+X65_Years._over+
  G1+G2+G3+G4+G5+E1+E2+E3+B1+B2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")
modds11 <- inla(formulas11,family = "poisson",data=covid_census2,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))
modds11_biweekly <- inla(formulas11,family = "poisson",data=covid_biweekly_census2_interaction,
                E = Population2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


formulas22 <- cases_new~Apartment_Proportion+Other_Proportion+Apartment2_Proportion+Average_Household_size+Low_income+X0_17Years+X18_65Years+X65Years_over+Self_employed_Proportion+Population_Density+Health_occupations_proportion+X65_Years._over+
  G1+G2+G3+G4+G5+E1+E2+E3+B1+B2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")+
  f(region_date_num,model = "iid")
modds22 <- inla(formulas22,family = "poisson",data=covid_census2,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))
modds22_biweekly <- inla(formulas22,family = "poisson",data=covid_biweekly_census2_interaction,
                         E = Population2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formulas33 <- cases_new ~ Apartment_Proportion+Other_Proportion+Apartment2_Proportion+Average_Household_size+Low_income+X0_17Years+X18_65Years+X65Years_over+Self_employed_Proportion+Population_Density+Health_occupations_proportion+X65_Years._over+
  G1+G2+G3+G4+G5+E1+E2+E3+B1+B2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str,model = "rw2")+
  f(date_num_unstr,model = "iid")+
  f(ID1,model = "iid",group = date_num_str_int,
    control.group = list(model="rw2"))

modds33 <- inla(formulas33,family = "poisson",data=covid_census2,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))
modds33_biweekly <- inla(formulas33,family = "poisson",data=covid_biweekly_census2_interaction,
                         E = Population2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))

formulas44 <- cases_new ~ Apartment_Proportion+Other_Proportion+Apartment2_Proportion+Average_Household_size+Low_income+X0_17Years+X18_65Years+X65Years_over+Self_employed_Proportion+Population_Density+Health_occupations_proportion+X65_Years._over+
  G1+G2+G3+G4+G5+E1+E2+E3+B1+B2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw2") +
  f(date_num_unstr, model="iid") +
  f(date_num_unstr_int, model="iid", group=ID1,
    control.group=list(model="besag", graph=g_ON1))
modds44 <- inla(formulas44,family = "poisson",data=covid_census2,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))
modds44_biweekly <- inla(formulas44,family = "poisson",data=covid_biweekly_census2_interaction,
                         E = Population2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))




formulas55 <- cases_new ~ Apartment_Proportion+Other_Proportion+Apartment2_Proportion+Average_Household_size+Low_income+X0_17Years+X18_65Years+X65Years_over+Self_employed_Proportion+Population_Density+Health_occupations_proportion+X65_Years._over+
  G1+G2+G3+G4+G5+E1+E2+E3+B1+B2+
  f(region,model = "bym",graph = g_ON1)+
  f(date_num_str, model="rw2") +
  f(date_num_unstr, model="iid") +
  f(ID1, model="besag", graph=g_ON1, 
    group=date_num_str_int,
    control.group=list(model="rw2"))
modds55 <- inla(formulas55,family = "poisson",data=covid_census2,
               E = POP2019/100000, control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE,cpo = TRUE,waic = TRUE))


# lst_spt2 <- list(NULL)
# length(lst_spt2) <- 5
# lst_spt2[[1]] <- modds1
# lst_spt2[[2]] <- modds2
# lst_spt2[[3]] <- modds3
# lst_spt2[[4]] <- modds4
# lst_spt2[[5]] <- modds5

coefficients <- c("G1","G2","G3","G4","G5",
                  "E1","E2","E3","B1","B2")

effects_fixed1 <- modds11$summary.fixed[14:23,]
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

ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/coefficients11.pdf", p1)

effects_fixed2 <- modds22$summary.fixed[14:23,]
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

ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/coefficients22.pdf", p2)

effects_fixed3 <- modds33$summary.fixed[14:23,]
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

ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/coefficients33.pdf", p3)

effects_fixed4 <- modds44$summary.fixed[14:23,]
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

ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/coefficients44.pdf", p4)

effects_fixed5 <- modds55$summary.fixed[14:23,]
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

ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/coefficients55.pdf", p5)


##########
plot1 <- ggplot(as.data.frame(modds33$marginals.fixed$G1)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(p, "(", betaG1, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot11.pdf", plot1)


plot2 <- ggplot(as.data.frame(modds33$marginals.fixed$G2)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(p, "(", betaG2, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot22.pdf", plot2)

plot3 <- ggplot(as.data.frame(modds33$marginals.fixed$G3)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(p, "(", betaG3, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot33.pdf", plot3)


plot4 <- ggplot(as.data.frame(modds33$marginals.fixed$G4)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(p, "(", betaG1, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot44.pdf", plot4)


plot5 <- ggplot(as.data.frame(modds33$marginals.fixed$G5)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(p, "(", betaG5, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot55.pdf", plot5)


plot6 <- ggplot(as.data.frame(modds33$marginals.fixed$E1)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(p, "(", betaE1, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot66.pdf", plot6)


plot1 <- ggplot(as.data.frame(modds33$marginals.fixed$G1)) + 
  geom_line(aes(x = x, y = y)) +
  theme_bw()+
  ylab (expression(paste(p, "(", betaG1, " | ", bold(y), ")")))
ggsave(file="F:/Research Work/COVID-2019/Canada/ON/data_preparation/plot11.pdf", plot1)





