setwd("C:/Users/Usuario/Desktop/Facultad/7 Septimo Semestre 2023/Estadística Descriptiva/Proyecto Final")

pokemon <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-10/pokemon.csv")
library(ggplot2)
library(ggthemes)
library(GGally)
library(tidyverse)
library(ggrepel)
library(fmsb)
library(gridExtra)
library(extrafont)
font_import()
fonts()
loadfonts(device = "win", quiet = TRUE)

###### FRECUENCIA DE POKEMON POR TIPO (ver color) ####
ttipo1 <- names(sort(table(pokemon$tipo_1), decreasing = FALSE)) 
pokemon$tipo_1_ordenado <- factor(pokemon$tipo_1, levels = ttipo1)

# Paleta de colores de Pokémon 
colores_tipos <- c("Agua" = "#6890F0", "Normal" = "#A8A878", "Césped" = "#78C850", 
                   "Bicho" = "#A8B820", "Psíquico" = "#F85888", "Fuego" = "#F08030", 
                   "Roca" = "#B8A038", "Eléctrico" = "#F8D030", "Tierra" = "#E0C068", 
                   "Fantasma" = "#705898", "Dragón" = "#7038F8", "Oscuro" = "#705848", 
                   "Veneno" = "#A040A0", "Luchador" = "#C03028", "Acero" = "#B8B8D0", 
                   "Hielo" = "#98D8D8", "Hada" = "#EE99AC", "Volador" = "#A890F0")

ggplot(pokemon, aes(y = tipo_1_ordenado, fill = tipo_1)) +
  geom_bar(stat = "count") +
  scale_fill_manual(values = colores_tipos) +
  labs(title = "Frecuencia de Pokémon por Tipo", 
       x = NULL, 
       y = NULL) +
  theme_minimal() +
  theme(
    text = element_text(family = "Pokemon GB"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  ) +
  guides(fill = FALSE)


##### #POKEMON LEGENDARIOS Y NO LEGEN - BOXPLOT ####
box_plot_attr <- select(pokemon, tipo_1, es_legendario, puntos_vida, defensa, ataque, fuerza_especial_ataque, fuerza_especial_defensa, velocidad)
box_plot_attr_leg <- filter(box_plot_attr, es_legendario == "VERDADERO")
box_plot_attr_nor <- filter(box_plot_attr, es_legendario == "FALSO")
box_plot_attr_leg_long <- gather(box_plot_attr_leg, attribute, value, -c(tipo_1, es_legendario))
box_plot_attr_nor_long <- gather(box_plot_attr_nor, attribute, value, -c(tipo_1, es_legendario))

bp_leg <- ggplot(data = box_plot_attr_leg_long, aes(attribute, value)) +
  geom_boxplot(fill = "gold") +
  ggtitle("Pokemon Legendarios") +
  labs(x = NULL, y = NULL) + theme_minimal() +
  theme(axis.text.y = element_text(color = "#333333", family = "Pokemon GB"),
        axis.text.x = element_text(color = "#333333"),
        plot.title = element_text(hjust = 0.5, color = "#333333", family = "Pokemon GB"),
        axis.title = element_text(color = "#333333"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

bp_nor <- ggplot(data = box_plot_attr_nor_long, aes(attribute, value)) +
  geom_boxplot(fill = "#E2BF65") +
  ggtitle("Pokemon Normales") +
  labs(x = NULL, y = NULL) +
  theme(axis.text.y = element_text(color = "#333333", family = "Pokemon GB"),
        axis.text.x = element_text(color = "#333333"),
        plot.title = element_text(hjust = 0.5, color = "#333333", family = "Pokemon GB"),
        axis.title = element_text(color = "#333333"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

grid.arrange(bp_leg, bp_nor, ncol = 2)


###### ¿Como afecta la generacion de cada pokemon a los puntos totales? ####

ggplot(pokemon, aes(x = factor(generacion), y = total, 
                    fill = factor(generacion))) + 
  geom_boxplot() + 
  labs(title="Puntos Totales por Generación", 
       y = "Puntos Totales", x = "Generación", fill = "Generaciones")+
  geom_label_repel(data = subset(pokemon, total >= 780),
                   aes(label = nombre_traducido), box.padding = 0.3, point.padding = 0.5) +
  guides(fill = "none")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(family = "Pokemon GB"))+
  scale_fill_manual(values=c("#ef476f","#f78c6b" ,"#ffd166","#06d6a0","#118ab2","#073b4c"))


#### Radar comparando 3 casos atipicos:

vec_mewtwoX <- c(130, 190, 100, 154, 100, 106)
vec_mewtwoY <- c(140, 150, 70, 194, 120, 106)
vec_rayquazu <- c(115, 180, 100, 180, 100, 105)
df4 <- data.frame(rbind(rep(200, 6), rep(0, 6),
                        +                         +                         +                        vec_mewtwoX, vec_mewtwoY, vec_rayquazu))
colnames(df4) <- paste(c("Velocidad", "Ataque", "Defensa", "F. Especial Ataque", "F. Especial Defensa", "Puntos de Vida"))
areas <- c(rgb(0.5137, 0.4824, 0.6118, 0.25),
           rgb(0.0667, 0.5412, 0.698, 0.25),
           rgb(0.1608, 0.3216, 0.2549, 0.25))

radarchart(df4,
           cglty = 1,       # Tipo de línea del grid
           cglcol = "gray", # Color líneas grid
           pcol = c(rgb(0.5137, 0.4824, 0.6118, 1),
                    rgb(0.0667, 0.5412, 0.698, 1),
                    rgb(0.1608, 0.3216, 0.2549, 1)),      # Color para cada línea
           plwd = 2,        # Ancho para cada línea
           plty = 1,        # Tipos de línea
           title = "Comparación de los tres casos atípicos", 
           pfcol = areas)   # Color de las áreas 
legend("topright",
       legend = c("Mewtwo Mega X", "Mewtwo Mega Y", "Rayquaza Mega"),
       bty = "n", pch = 20, col = areas,
       text.col = "grey25", pt.cex = 2)

## PUNTOS TOTALES POR GENERACION PARA 4 TIPOS POKÉMON 
evo_filtrado <- pokemon %>% filter("NA"!= nivel_evolucion)
evo_filtrado1 <- evo_filtrado %>% 
  filter(tipo_1 %in% c("Césped", "Normal", "Agua", "Roca"))

ggplot(evo_filtrado1, aes(x = factor(tipo_1), y = total, fill = factor(tipo_1))) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ generacion) +
  labs(x = NULL, y = NULL, fill = "Generacion", title = "Puntos totales por generación para 4 tipos Pokémon") +
  scale_y_continuous(labels = function(n) format(n, scientific = FALSE)) +
  theme(axis.text.y = element_text(color = "#333333", family = "Pokemon GB"),
        axis.text.x = element_text(color = "#333333", face = "bold"),
        plot.title = element_text(hjust = 0.5, family = "Pokemon GB")) +
  guides(fill = "none") +
  scale_fill_manual(values = c("#6390F0", "#7AC74C", "#A8A77A", "#B6A136"))


###### ¿Existe una relacion entre los puntos de defensa y los de ataque? ####

Tipo.1v3 <- pokemon %>% filter(tipo_1 %in% c("Césped", "Normal", "Agua", "Roca")) %>% select(ataque,defensa,tipo_1)

ggpairs(Tipo.1v3, title= "Correlación entre puntos de ataque y de defensa", ggplot2::aes(colour=tipo_1)) +
  scale_color_manual(values=c("#6390F0","#7AC74C","#A8A77A", "#B6A136")) + theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(hjust = 0.5, family = "Pokemon GB")
  )


#Dispersion con la recta de regresion para 4 tipos:
Tipo.1v2 <- pokemon %>% filter(tipo_1 %in% c("Césped", "Normal", "Agua", "Roca"))

ggplot(Tipo.1v2, aes(x = ataque, y = defensa, color = factor(tipo_1))) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm") +
  labs(color = "Tipo de Pokémon", title = "Relacion entre Defensa y Ataque",x = "Ataque", y = "Defensa")+ 
  scale_color_manual(values=c("#6390F0","#7AC74C","#A8A77A", "#B6A136"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(family = "Pokemon GB"))



