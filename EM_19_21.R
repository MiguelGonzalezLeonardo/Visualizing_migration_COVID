

setwd("D:\\ARTICULO7_CovidRural\\Graficos")


library(foreign)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(dplyr)
library(ggrepel)
library(viridis)

#Data
data <- read.csv(file = 'EM_19_21.csv', sep = ',')


#Orden
data$Type<- factor(data$Type, levels = c("Internal migration",
                                         "International migration",
                                         "Total"))

data$Year <- as.factor(data$Year)
data$Year <- factor(data$Year, levels = c("2019",
                                         "2020",
                                         "2021"))

data$Province <- as.factor(data$Province)
levels(data$Province)
data$Province <- recode(data$Province, "Albacete"="Albacete",
                        "Alicante"="Alicante",
                        "Almeria"="Almería",
                        "Alava"="Álava",
                        "Asturias"="Asturias",
                        "Avila"="Ávila",
                        "Badajoz"="Badajoz",
                        "Baleares"="Baleares",
                        "Barcelona"="Barcelona",
                        "Bizkaia"="Bizkaia",
                        "Burgos"="Burgos",
                        "Caceres"="Cáceres",
                        "Cadiz"="Cádiz",
                        "Cantabria"="Cantabria",
                        "Castellon"="Castellón",
                        "Ciudad Real"="Ciudad Real",
                        "Cordoba"="Córdoba",
                        "A Coruna"="A Coruña",
                        "Cuenca"="Cuenca",
                        "Gipuzkoa"="Gipuzkoa",
                        "Girona"="Girona",
                        "Granada"="Granada",
                        "Guadalajara"="Guadalajara",
                        "Huelva"="Huelva",
                        "Huesca"="Huesca",
                        "Jaen"="Jaén",
                        "Leon"="León",
                        "Lleida"="Lleida",
                        "Lugo"="Lugo",
                        "Madrid"="Madrid",
                        "Malaga"="Málaga",
                        "Murcia"="Murcia",
                        "Navarra"="Navarra",
                        "Ourense"="Ourense",
                        "Palencia"="Palencia",
                        "Las Palmas"="Las Palmas",
                        "Pontevedra"="Pontevedra",
                        "La Rioja"="La Rioja",
                        "Salamanca"="Salamanca",
                        "Tenerife"="Tenerife",
                        "Segovia"="Segovia",
                        "Sevilla"="Sevilla",
                        "Soria"="Soria",
                        "Tarragona"="Tarragona",
                        "Teruel"="Teruel",
                        "Toledo"="Toledo",
                        "Valencia"="Valencia",
                        "Valladolid"="Valladolid",
                        "Zamora"="Zamora",
                        "Zaragoza"="Zaragoza")


data$Dens <- recode(data$Province,"Soria"="1",
                    "Teruel"="1",
                    "Cuenca"="1",
                    "Huesca"="1",
                    "Zamora"="1",
                    "Cáceres"="1",
                    "Guadalajara"="1",
                    "Palencia"="1",
                    "Ávila"="1",
                    "Segovia"="1",
                    "Burgos"="2",
                    "Ciudad Real"="2",
                    "Albacete"="2",
                    "Salamanca"="2",
                    "Badajoz"="2",
                    "León"="2",
                    "Lugo"="2",
                    "Lleida"="2",
                    "Ourense"="2",
                    "Toledo"="2",
                    "Jaén"="3",
                    "Huelva"="3",
                    "Zaragoza"="3",
                    "Córdoba"="3",
                    "Navarra"="3",
                    "La Rioja"="3",
                    "Valladolid"="3",
                    "Granada"="3",
                    "Almería"="3",
                    "Castellón"="3",
                    "Asturias"="4",
                    "Álava"="4",
                    "Cantabria"="4",
                    "Girona"="4",
                    "Tarragona"="4",
                    "Murcia"="4",
                    "Sevilla"="4",
                    "A Coruña"="4",
                    "Cádiz"="4",
                    "Pontevedra"="4",
                    "Málaga"="5",
                    "Baleares"="5",
                    "Valencia"="5",
                    "Las Palmas"="5",
                    "Tenerife"="5",
                    "Alicante"="5",
                    "Gipuzkoa"="5",
                    "Bizkaia"="5",
                    "Barcelona"="5",
                    "Madrid"="5")

data$Dens <- factor(data$Dens, levels = c( "5", "4", "3", "2", "1"))

data$Dens <- recode(data$Dens, "1"="Q1 (< 24 hab. / Km²)",
                    "2"="Q2 (24 to 46 hab. / Km²)",
                    "3"="Q3 (46 to 91 hab. / Km²)",
                    "4"="Q4 (91 to 214 hab. / Km²)",
                    "5"="Q5 (> 214 hab. / Km²)")


data$Province<- factor(data$Province, levels = c("Soria",
                                                 "Teruel",
                                                 "Cuenca",
                                                 "Huesca",
                                                 "Zamora",
                                                 "Cáceres",
                                                 "Guadalajara",
                                                 "Palencia",
                                                 "Ávila",
                                                 "Segovia",
                                                 "Burgos",
                                                 "Ciudad Real",
                                                 "Albacete",
                                                 "Salamanca",
                                                 "Badajoz",
                                                 "León",
                                                 "Lugo",
                                                 "Lleida",
                                                 "Ourense",
                                                 "Toledo",
                                                 "Jaén",
                                                 "Huelva",
                                                 "Zaragoza",
                                                 "Córdoba",
                                                 "Navarra",
                                                 "La Rioja",
                                                 "Valladolid",
                                                 "Granada",
                                                 "Almería",
                                                 "Castellón",
                                                 "Asturias",
                                                 "Álava",
                                                 "Cantabria",
                                                 "Girona",
                                                 "Tarragona",
                                                 "Murcia",
                                                 "Sevilla",
                                                 "A Coruña",
                                                 "Cádiz",
                                                 "Pontevedra",
                                                 "Málaga",
                                                 "Baleares",
                                                 "Valencia",
                                                 "Las Palmas",
                                                 "Tenerife",
                                                 "Alicante",
                                                 "Gipuzkoa",
                                                 "Bizkaia",
                                                 "Barcelona",
                                                 "Madrid"))





#Plot
bmp(file="EM_19_21.bmp", width = 8, height = 11, units = 'in', res = 300)

ggplot(data, aes(Value, Province)) +
  
  #facet_grid(Dens ~ Type, scales="free_y")
  #facet_wrap(.~Type)+
  facet_grid(vars(Dens), vars(Type), scales="free_y")+
  
  geom_line(aes(group = Province), size=0.4, color="gray20",linetype="solid") +
  
  geom_point(aes(colour=Year), size = 2.3, alpha=0.8, pch=16) +
  
  #scale_color_manual(values=c("#619CFF", "#F8766D", "#00BA38"))+
  scale_colour_viridis_d(option = "plasma")+
  
  #coord_flip()+
  
  scale_x_continuous(limits = c(-5, 18.5), breaks =seq(-5,15,5))+
  
  geom_vline(xintercept = 0, size=0.5, colour ="grey20", linetype="dashed")+
  
  labs(x="Net migration rates (‰)",y = NULL) +
  
  #geom_text_repel(data = data, aes(color = Year, label = round(Value, 2)),
                  #size = 2.35,point.padding = 0.5, force = 1, segment.size  = 0.2) +
  
  theme_bw()+
  
  theme(text = element_text(size = 11.5, face="plain"),
        
        axis.title.x = element_text(vjust=-1,size=9, face="plain"),
        strip.background = element_rect(color="grey15",size = 0.35),
        
        panel.border = element_rect(color="grey15",size = 0.35),
        panel.grid.major.x = element_line(colour = "grey70", size=0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey70", size=0.2),
        panel.grid.minor.y = element_blank(),
        
        axis.text.x = element_text(colour = "grey10", size = 9, hjust = 0.5, face="plain"),
        axis.text.y = element_text(colour = "grey10", size = 9, hjust =1, vjust = 0.5, face="plain"),
        axis.ticks = element_line(color="grey15", size = 0.5),
        
        legend.title = element_blank (),
        legend.text = element_text(colour="black", size = 9.5, face="plain"),
        legend.position="bottom",
        legend.direction = "horizontal",
        #legend.justification=c(1,0),
        legend.key.width=unit(.5,"cm"),
        legend.key.height=unit(.5,"cm"))

dev.off()
