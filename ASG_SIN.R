###############################################################
# Empleo formal en Sinaloa, abril 2021.
###############################################################

# Directorio
wd <- "C:/Users/Victor/Desktop/jzepeda.org/economic indicators/employment/IMSS_data"
setwd(wd)

# Cargamos librerías
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)

# Importamos base de datos
ASG <- read.csv("asg-2021-04-30.csv", sep = "|")

# Filtramos para conocer el número total de asegurados
ASG %>%
  filter(cve_entidad == 25) %>%
  summarise(sum(ta))

# Filtramos para conocer el número total de asegurados por sector económico
data1 <- ASG %>%
  filter(cve_entidad == 25) %>%
  filter(sector_economico_1 != 0) %>%
  group_by(sector_economico_1) %>%
  summarise(sum(ta))
data1

# Cambiamos el nombre de los datos de la variable "sector económico 1"
sector_economico <- c("0" = "Agricultura",
                      "1" = "Industrias extractivas",
                      "3" = "Industrias de la \n transformación",
                      "4" = "Industria de la construcción",
                      "5" = "Industria eléctrica",
                      "6" = "Comercio",
                      "7" = "Transportes y \n comunicaciones",
                      "8" = "Servicios para empresas",
                      "9" = "Servicios sociales \n y comunales")

# Cambiamos el tipo de dato de la variable "sector económico 1"
# Para ello, convertimos los datos a "character" y lo agregamos al data frame
data1$sector_economico_1 <- as.character(sector_economico[data1$sector_economico_1])
data1

# Renombramos las variables del data frame "data1"
data1 <- rename(data1, "Sector" = "sector_economico_1", "Asegurados" = "sum(ta)")
data1

# Graficamos
b <- ggplot(data1, aes(x = reorder(Sector, Asegurados), 
                       y = Asegurados, 
                       fill = Sector)) + 
  geom_col(width = 0.55) + 
  geom_label(aes(label = Asegurados), 
             size = 3.00,
             hjust = 0.5,
             vjust = 0.5,
             color = "black",
             fill = "white") + 
  labs(title = "Empleo formal en Sinaloa por sector económico, abril 2021.",
       subtitle = "Cotizantes en el IMSS.",
       x = "Sector económico",
       y = "Cotizantes",
       caption = "Elaborado por @eljuanzepeda con información del IMSS.") + 
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(face = "bold")) + 
  ylim(0, 150000) +
  coord_flip()
b

ggsave(filename = "ASG_Sector.pdf",
       plot = last_plot(),
       units = c("in"),
       width = 7.50,
       height = 4.50)