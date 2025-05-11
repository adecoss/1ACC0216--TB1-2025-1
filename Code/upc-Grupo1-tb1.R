#Limpiar
rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

#Libreriras
library(ggplot2)
library(tidyverse)
library(lubridate)
library(VIM)
library(mlr)

#Carga De Datos
setwd("C:/Users/CHAMPUX/Downloads/TB1 DATOS/Data") #TU DIRECTORIO
hotel_data <- read.csv('hotel_bookings (original).csv', header= TRUE, stringsAsFactors = FALSE, sep=',',dec='.')
View(hotel_data)

head(hotel_data)
summary(hotel_data)
str(hotel_data)
names(hotel_data)

# Algunas Variables categoricas clave
table(hotel_data$hotel)
table(hotel_data$is_canceled)
table(hotel_data$customer_type)
table(hotel_data$reservation_status)
table(hotel_data$meal)
table(hotel_data$children)
table(hotel_data$babies)
table(hotel_data$required_car_parking_spaces)
table(hotel_data$country)

# Convertir variables categóricas
hotel_data$hotel <- as.factor(hotel_data$hotel)
hotel_data$is_canceled <- as.factor(hotel_data$is_canceled)
hotel_data$arrival_date_month <- as.factor(hotel_data$arrival_date_month)
hotel_data$meal <- as.factor(hotel_data$meal)
hotel_data$customer_type <- as.factor(hotel_data$customer_type)

#Median/Mean ADR
mean(hotel_data$adr)
median(hotel_data$adr)

# Distribución de reservas por tipo de hotel
ggplot(hotel_data, aes(x = hotel)) +
  geom_bar() +
  ggtitle("Distribución por tipo de hotel")

# Cancelaciones por tipo de cliente
ggplot(hotel_data, aes(x = customer_type, fill = is_canceled)) +
  geom_bar(position = "dodge") +
  ggtitle("Cancelaciones por tipo de cliente")

# Histograma del tiempo de anticipación de la reserva
ggplot(hotel_data, aes(x = lead_time)) +
  geom_histogram(bins = 30) +
  ggtitle("Distribución del tiempo de reserva anticipada")


#--------------------------------------------------------------------------------
#DATOS FALTATNTES
#--------------------------------------------------------------------------------

#Convertir los NULL en el dataset a NA
hotel_data[hotel_data == "NULL"] <- NA 

#Ver datos Faltantes
aggr(hotel_data,numbers=T,sortVar=T)

# Ver cuantos valores NA hay por columna
colSums(is.na(hotel_data))

#Reemplazamos los datos faltantes en children por la mediana
mediana_children <- median(hotel_data$children, na.rm = TRUE)
hotel_data$children[is.na(hotel_data$children)] <- mediana_children

#Para Country, reconocemos NA como una nueva clase

#hotel_data$country[is.na(hotel_data$country)] <- "Desconocido"

# O reemplazamos por la MODA

hotel_data$country <- as.factor(hotel_data$country)

imputacion_country <- impute(hotel_data[, "country", drop = FALSE],
                             classes = list(factor = imputeMode()),
                             dummy.classes = "factor",
                             dummy.type = "numeric")

hotel_data$country <- imputacion_country$data$country

# Company y Agent tienen demasiados NA, los categorizamos como "Desconocido"
hotel_data$company[is.na(hotel_data$company)] <- "Desconocido"
hotel_data$agent[is.na(hotel_data$agent)] <- "Desconocido"

#Ver datos Faltantes x2
aggr(hotel_data,numbers=T,sortVar=T)

# Ver cuantos valores NA hay por columna x2
colSums(is.na(hotel_data))

#--------------------------------------------------------------------------------
#VALORES ATIPICOS
#--------------------------------------------------------------------------------

#Histograma ADR
h1<-ggplot(data = hotel_data,
        mapping = aes(x = adr)) +
  geom_histogram(aes(y=..density..),
                 bins = 15,
                 position = 'identity',
                 alpha = 0.8,
                 color="white",
                 fill="lightskyblue3") +
  stat_function(fun = dnorm,
                args = list(mean = mean(hotel_data$adr),
                            sd = sd(hotel_data$adr)))+
  labs(title = 'Histograma ADR',
       x = 'ADR',
       y = 'conteos',
       subtitle = 'Detectar valores atípicos',
       caption = 'Fuente:ADR')
h1

#Boxlot ADR
h2<-ggplot(hotel_data, aes(x =adr)) +
  geom_boxplot(fill="steelblue") +
  labs(title = "Boxplot ADR",
  )+
  theme_classic()
h2

#Visualizamos los registros con los valores atípicos identificados
outliers<-boxplot(hotel_data$adr,plot=FALSE)$out
outliers

#--------------------------------------------------------------------------------

# Calcular IQR: Rango intercuartílico
Q1 <- quantile(hotel_data$adr, 0.25, na.rm = TRUE)
Q3 <- quantile(hotel_data$adr, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Definir límites para outliers
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

# Identificar datos atípicos
outliers <- hotel_data$adr[hotel_data$adr < limite_inferior | hotel_data$adr > limite_superior
]
outliers

#--------------------------------------------------------------------------------

# Calcular el Z-score para la variable 'adr'
adr_mean <- mean(hotel_data$adr)
adr_sd <- sd(hotel_data$adr)

#Calculamos z_scores
z_scores <- (hotel_data$adr - adr_mean) / adr_sd

# Identificar datos atípicos(Z-scores mayores a 3 o menores a -3)
outliers <- hotel_data$adr[abs(z_scores) > 3]
outliers


#Capping/Winsorization

# Definir límites del 1% y 99%
lower_bound <- quantile(hotel_data$adr, 0.01)
upper_bound <- quantile(hotel_data$adr, 0.99)

#Aplicar límites
hotel_data$adr_capped <- ifelse(hotel_data$adr < lower_bound, lower_bound,
                         ifelse(hotel_data$adr > upper_bound, upper_bound, hotel_data$adr))
#Visualizaremos la variable
h3<-par(mfrow = c(1,2))
boxplot(hotel_data$adr, main = "ADR con outliers",col = 3)
boxplot(hotel_data$adr_capped, main = "ADR sin outliers",col=2)
h3

h4<-ggplot(hotel_data, aes(x =adr_capped)) +
  geom_boxplot(fill="steelblue") +
  labs(title = "Boxplot",
  )+
  theme_classic()
h4

#-------------------------------------------------------------------------------------------
#VISUALIZACION DE DATOS
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#1. ¿Cuántas reservas se realizan por tipo de hotel? ¿Que tipo de hotel prefiere la gente?
#-------------------------------------------------------------------------------------------

table(hotel_data$hotel)

#Contar la cantidad de hoteles
hotel_counts <- hotel_data %>%
  count(hotel)

#Grafico Barras Todas las Reservas por tipo de Hotel
p1 <- ggplot(hotel_counts, aes(x = hotel, y = n, fill = hotel)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3, size = 4) +
  labs(title = "Reservas por Tipo de Hotel",
       x = "Tipo de Hotel", y = "Cantidad de Reservas") +
  theme_minimal()

p1

#Grafico Barras Todas las Reservas por tipo de Hotel considerando cancelaciones
p2<-hotel_data %>%
  group_by(hotel, is_canceled) %>%
  summarise(total_reservas = n()) %>%
  mutate(estado = ifelse(is_canceled == 1, "Cancelada", "No cancelada")) %>%
ggplot(aes(x = hotel, y = total_reservas, fill = estado)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = total_reservas),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3.5) +
  labs(title = "Reservas y cancelaciones por tipo de hotel",
       x = "Tipo de hotel", y = "Cantidad de reservas", fill = "Estado") +
  theme_minimal()
p2


#-------------------------------------------------------------------------------------------
#2. ¿Está aumentando la demanda con el tiempo?
#-------------------------------------------------------------------------------------------

# Combianmos el year, month de llegada para hacer fecha_mes
hotel_data <- hotel_data %>%
  mutate(month_num = match(arrival_date_month, month.name),
         fecha_mes = make_date(year = arrival_date_year,
                               month = month_num,
                               day = 1))

#Reservas por Mes
reserva_mensual <- hotel_data %>%
  group_by(fecha_mes) %>%
  summarise(reservas = n(), .groups = "drop")

p3<-ggplot(reserva_mensual, aes(x = fecha_mes, y = reservas)) +
  geom_line(color = "darkblue") +
  geom_point() +
  labs(title = "Tendencia mensual de reservas en el tiempo",
       x = "Mes y Año", y = "Cantidad de Reservas") +
  theme_minimal()
p3

#Reservars por mes, por tipo de hotel
reserva_mensual_hotel <- hotel_data %>%
  group_by(fecha_mes, hotel) %>%
  summarise(reservas = n(), .groups = "drop")

p4<-ggplot(reserva_mensual_hotel, aes(x = fecha_mes, y = reservas, color = hotel)) +
  geom_line() +
  geom_point() +
  labs(title = "Tendencia mensual de reservas por tipo de hotel",
       x = "Mes y Año", y = "Cantidad de Reservas", color = "Tipo de Hotel") +
  theme_minimal()
p4



#-------------------------------------------------------------------------------------------
#3. ¿Cuáles son las temporadas de reservas (alta, media, baja)?
#-------------------------------------------------------------------------------------------

#Cantidad de reservas por Mes (En total, todoa los años)
p5<-hotel_data %>%
  count(arrival_date_month) %>%
  mutate(arrival_date_month = factor(arrival_date_month, 
                                     levels = month.name)) %>%
  
ggplot(aes(x = arrival_date_month, y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  labs(title = "Reservas por mes", x = "Mes", y = "Número de reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p5


#Usando terciles, asignamois las temporadas
# Calcular reservas por mes, en una nueva variable
reservas_por_mes <- hotel_data %>%
  count(arrival_date_month) %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name))

# Calcular terciles para definir las categorías
terciles <- quantile(reservas_por_mes$n, probs = c(1/3, 2/3))

# Categorizar en temporadas
reservas_por_mes <- reservas_por_mes %>%
  mutate(temporada = case_when(
    n <= terciles[1] ~ "Baja",
    n <= terciles[2] ~ "Media",
    TRUE ~ "Alta"
  ))

# Graficar
p6<- ggplot(reservas_por_mes, aes(x = arrival_date_month, y = n, fill = temporada)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  labs(title = "Reservas por mes y temporada",
       x = "Mes", y = "Número de reservas", fill = "Temporada") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6


#-------------------------------------------------------------------------------------------
#4. ¿Cuál es la duración promedio de las estancias por tipo de hotel? 
#-------------------------------------------------------------------------------------------

#Sumamos los dias de semana con los fines
total_stay = hotel_data$stays_in_weekend_nights + hotel_data$stays_in_week_nights
mean(total_stay)

p7 <- hotel_data %>%
  mutate(total_stay = stays_in_weekend_nights + stays_in_week_nights) %>%
  group_by(hotel) %>%
  summarise(promedio_estancia = mean(total_stay), .groups = "drop") %>%
ggplot(aes(x = hotel, y = promedio_estancia, fill = hotel)) +
  geom_col() +
  geom_text(aes(label = round(promedio_estancia, 1)), vjust = -0.5, size = 4) +
  labs(title = "Duración promedio de estancia por tipo de hotel", y = "Días", x = "Tipo de hotel") +
  theme_minimal()
p7

#-------------------------------------------------------------------------------------------
#5. ¿Cuántas reservas incluyen niños y/o bebés?
#-------------------------------------------------------------------------------------------

#Separamos los casos de Niños, Bebes, Niño+Bebe.
p8<-hotel_data %>%
  mutate(tipo_reserva = case_when(
    children > 0 & babies > 0 ~ "Niños y Bebés",
    children > 0 & babies == 0 ~ "Solo Niños",
    children == 0 & babies > 0 ~ "Solo Bebés",
    TRUE ~ "Sin Niños/Bebés"
  )) %>%
  count(tipo_reserva) %>%
ggplot(aes(x = reorder(tipo_reserva, -n), y = n, fill = tipo_reserva)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  labs(title = "Reservas por tipo de presencia infantil",
       x = "Tipo de Reserva", y = "Cantidad de Reservas") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")
p8

#-------------------------------------------------------------------------------------------
#6. ¿Es importante contar con espacios de estacionamiento?
#-------------------------------------------------------------------------------------------

table(hotel_data$required_car_parking_spaces)

parking_data <- hotel_data %>%
  count(required_car_parking_spaces) %>%
  arrange(desc(n)) %>%
  mutate(required_car_parking_spaces = factor(required_car_parking_spaces, levels = unique(required_car_parking_spaces)))

p9<- ggplot(parking_data, aes(x = required_car_parking_spaces, y = n)) +
  geom_col(fill = "green") +
  geom_text(aes(label = n), vjust = -0.3, size = 4) +
  labs(title = "Reservas por cantidad de espacios de estacionamiento requeridos",
       x = "Espacios requeridos", y = "Cantidad de reservas") +
  theme_minimal()
p9


#-------------------------------------------------------------------------------------------
#7. ¿En que meses del año se producen mas cancelaciones de reservas?
#-------------------------------------------------------------------------------------------

#Cantidad
p10 <- hotel_data %>%
  filter(is_canceled == 1) %>%
  count(arrival_date_month) %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name)) %>%
ggplot(aes(x = arrival_date_month, y = n)) +
  geom_col(fill = "red") +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
  labs(title = "Cantidad de cancelaciones por mes", x = "Mes", y = "Cantidad de cancelaciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p10

#Tasa
p11<-hotel_data %>%
  group_by(arrival_date_month, is_canceled) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)) %>%
  filter(is_canceled == 1) %>%
  ungroup() %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name)) %>%
  
ggplot(aes(x = arrival_date_month, y = percentage)) +
  geom_col(fill = "red") +
  geom_text(aes(label = paste0(round(percentage*100,1),"%")), vjust = -0.5) +
  labs(title = "Tasa de cancelación por mes",
       subtitle = "Mayor tasa de cancelación por mes",
       x = "Mes", 
       y = "Tasa de cancelación") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p11

#-------------------------------------------------------------------------------------------
#8. Cual es la distribucion de comidas elejidas en las reservas?
#-------------------------------------------------------------------------------------------

meal_data <- hotel_data %>%
  count(meal) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1),
         etiqueta = paste0(meal, "\n", porcentaje, "%"))

p12<-ggplot(meal_data, aes(x = "", y = n, fill = meal)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Distribución de tipos de comida reservada",
       fill = "Tipo de comida") +
  theme_void()
p12


#Eliminamos ciertas columnas que ya no nos sirven, 
hotel_data_final <- hotel_data %>%
  select(-c(arrival_date_year))

#Nuevo Dataset
write.csv(hotel_data,'hotel_data NEW.csv', row.names = FALSE)