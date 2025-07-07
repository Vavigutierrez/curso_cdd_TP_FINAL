# =============================================================================
# ANÁLISIS EXPLORATORIO DE DATOS - ESTABLECIMIENTOS PRODUCTIVOS ARGENTINOS
# =============================================================================
# Objetivo: Explorar patrones en establecimientos productivos argentinos

# Este script demuestra un análisis exploratorio completo utilizando
# el dataset de establecimientos productivos del CEP XXI, siguiendo
# la metodología enseñada en clase.
#
# OPTIMIZACIÓN PARA DATASET GRANDE (1.36M observaciones):
# - Utiliza muestras representativas para operaciones computacionalmente costosas
# - Mantiene análisis estadísticos completos en todo el dataset
# - Optimiza visualizaciones sin perder representatividad

# =============================================================================
# 1. CARGAR PAQUETES NECESARIOS
# =============================================================================

# Instalar automáticamente paquetes que faltan
paquetes_necesarios <- c("tidyverse", "ggplot2", "dplyr", "skimr", "corrplot", "GGally", "gridExtra", "scales", "RColorBrewer")
paquetes_faltantes <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[,"Package"])]
if(length(paquetes_faltantes)) install.packages(paquetes_faltantes)

# Paquetes básicos de Tidyverse
library(tidyverse)     # Para manipulación y visualización de datos
library(ggplot2)       # Gráficos (incluido en tidyverse)
library(dplyr)         # Manipulación de datos (incluido en tidyverse)

# Paquetes específicos para EDA
library(skimr)         # Para estadísticas descriptivas mejoradas
library(corrplot)      # Para visualización de correlaciones
library(GGally)        # Para gráficos multivariados
library(gridExtra)     # Para organizar múltiples gráficos

# Paquetes para mapas y escalas
library(scales)        # Para formateo de números y porcentajes
library(RColorBrewer)  # Paletas de colores

# Configuración inicial
options(scipen = 999)  # Evitar notación científica
set.seed(42)          # Para reproducibilidad

# =============================================================================
# 2. DEFINIR RUTAS Y CARGAR DATOS
# =============================================================================

# Definir rutas
in_input <- "input"
out_graficos <- file.path("output", "graphs")
out_stats <- file.path("output", "statistics")

# Crear directorios si no existen
dir.create(out_graficos, showWarnings = FALSE, recursive = TRUE)

cat("=== ANÁLISIS EXPLORATORIO DE DATOS ===\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Cargar dataset limpio e integrado
establecimientos <- readRDS(file.path(in_input, "establecimientos_completo.rds"))

cat("✓ Datos cargados:", nrow(establecimientos), "establecimientos\n")
cat("✓ Variables disponibles:", ncol(establecimientos), "\n\n")

# =============================================================================
# 3. ENTENDER EL CONTEXTO Y ESTRUCTURA DE LOS DATOS
# =============================================================================

cat("3. EXPLORANDO LA ESTRUCTURA DE LOS DATOS\n")
cat("----------------------------------------\n")

# Examinar las primeras filas
print("Primeras filas del dataset:")
head(establecimientos)

# Estructura del dataset
print("Estructura del conjunto de datos:")
glimpse(establecimientos)

# Información contextual sobre las variables
cat("
VARIABLES DEL DATASET DE ESTABLECIMIENTOS PRODUCTIVOS:
- cuit, sucursal: Identificadores anónimos del establecimiento
- anio: Año de referencia (2021 o 2022)
- lat, lon: Coordenadas geográficas (WGS84)
- clae6: Código de actividad económica (6 dígitos)
- provincia_id, in_departamentos: Códigos geográficos
- quintil: Quintil de exportación (0=no exporta, 1-5=niveles de exportación)
- empleo: Categoría de tamaño de empleo (ej: 'a. 1-9', 'b. 10-49')
- proporcion_mujeres: Proporción de empleadas mujeres [0-1]
- categoria_tamaño: Micro, Pequeña, Mediana, Grande
- es_exportador: Indicador binario de exportación
- provincia, departamento: Nombres geográficos
- clae6_desc, letra_desc: Descripciones de actividad económica
- region: Clasificación regional (Centro, Cuyo, NOA, NEA, Patagonia)
")

# =============================================================================
# 4. EVALUACIÓN DE LA CALIDAD DE LOS DATOS
# =============================================================================

cat("4. EVALUACIÓN DE CALIDAD DE DATOS\n")
cat("----------------------------------\n")

# Verificar valores faltantes
print("Valores faltantes por columna:")
valores_faltantes <- colSums(is.na(establecimientos))
print(valores_faltantes[valores_faltantes > 0])

# Verificar duplicados
print("Número de filas duplicadas:")
print(sum(duplicated(establecimientos)))

# Estadísticas resumidas para variables numéricas
variables_numericas <- c("proporcion_mujeres", "empleo_numerico", "quintil", "lat", "lon")
print("Estadísticas resumidas para variables numéricas:")
summary(establecimientos[variables_numericas])

# Resumen detallado con skimr
print("Informe detallado con skimr:")
skim(establecimientos)

# =============================================================================
# 5. ANÁLISIS UNIVARIANTE: VARIABLES CATEGÓRICAS
# =============================================================================

cat("\n5. ANÁLISIS UNIVARIANTE - VARIABLES CATEGÓRICAS\n")
cat("------------------------------------------------\n")

# Función para analizar variables categóricas
analizar_categorica <- function(data, variable, titulo) {
  # Tabla de frecuencias
  tabla_freq <- table(data[[variable]])
  tabla_prop <- prop.table(tabla_freq) * 100
  
  cat("\nDistribución de", titulo, ":\n")
  print(tabla_freq)
  cat("Porcentajes:\n")
  print(round(tabla_prop, 2))
  
  # Gráfico de barras
  p <- ggplot(data, aes(x = .data[[variable]])) +
    geom_bar(fill = "steelblue", alpha = 0.7) +
    labs(title = paste("Distribución de", titulo),
         x = titulo,
         y = "Número de Establecimientos") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  
  # Guardar gráfico
  ggsave(file.path(out_graficos, paste0("barras_", gsub(" ", "_", tolower(titulo)), ".png")), 
         p, width = 10, height = 6, dpi = 300)
  
  return(tabla_freq)
}

# Analizar distribución por región
freq_region <- analizar_categorica(establecimientos, "region", "Región")

# Analizar distribución por tamaño de empresa
freq_tamaño <- analizar_categorica(establecimientos, "categoria_tamaño", "Tamaño de Empresa")

# Analizar distribución de exportadores
freq_exportador <- analizar_categorica(establecimientos, "es_exportador", "Estado Exportador")

# =============================================================================
# 6. ANÁLISIS UNIVARIANTE: VARIABLES NUMÉRICAS
# =============================================================================

cat("\n6. ANÁLISIS UNIVARIANTE - VARIABLES NUMÉRICAS\n")
cat("----------------------------------------------\n")

# Función para crear gráficos univariados completos
crear_graficos_univariados <- function(data, variable, titulo) {
  # Histograma
  p1 <- ggplot(data, aes(x = .data[[variable]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
    labs(title = paste("Histograma de", titulo)) +
    theme_minimal()
  
  # Densidad
  p2 <- ggplot(data, aes(x = .data[[variable]])) +
    geom_density(fill = "steelblue", alpha = 0.5) +
    labs(title = paste("Densidad de", titulo)) +
    theme_minimal()
  
  # Boxplot
  p3 <- ggplot(data, aes(y = .data[[variable]])) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    labs(title = paste("Boxplot de", titulo), x = "") +
    theme_minimal()
  
  # Gráfico combinado
  plot_combined <- grid.arrange(p1, p2, p3, ncol = 3)
  
  # Guardar gráfico
  ggsave(file.path(out_graficos, paste0("univariado_", gsub(" ", "_", tolower(titulo)), ".png")), 
         plot_combined, width = 15, height = 5, dpi = 300)
  
  # Calcular estadísticas descriptivas
  stats <- data %>%
    summarise(
      media = mean(.data[[variable]], na.rm = TRUE),
      mediana = median(.data[[variable]], na.rm = TRUE),
      desv_std = sd(.data[[variable]], na.rm = TRUE),
      q1 = quantile(.data[[variable]], 0.25, na.rm = TRUE),
      q3 = quantile(.data[[variable]], 0.75, na.rm = TRUE),
      minimo = min(.data[[variable]], na.rm = TRUE),
      maximo = max(.data[[variable]], na.rm = TRUE)
    )
  
  cat("\nEstadísticas para", titulo, ":\n")
  cat("Media:", round(stats$media, 3), "\n")
  cat("Mediana:", round(stats$mediana, 3), "\n")
  cat("Desviación estándar:", round(stats$desv_std, 3), "\n")
  cat("Q1:", round(stats$q1, 3), "\n")
  cat("Q3:", round(stats$q3, 3), "\n")
  cat("Rango:", round(stats$minimo, 3), "-", round(stats$maximo, 3), "\n")
  
  return(stats)
}

# Analizar proporción de mujeres
stats_mujeres <- crear_graficos_univariados(establecimientos, "proporcion_mujeres", "Proporción de Mujeres")

# Analizar tamaño de empleo numérico
stats_empleo <- crear_graficos_univariados(establecimientos, "empleo_numerico", "Tamaño de Empleo")

# Analizar quintil de exportación
stats_quintil <- crear_graficos_univariados(establecimientos, "quintil", "Quintil de Exportación")

# =============================================================================
# 7. ANÁLISIS BIVARIANTE
# =============================================================================

cat("\n7. ANÁLISIS BIVARIANTE\n")
cat("-----------------------\n")

# Relación entre tamaño de empresa y proporción de mujeres
p_tamaño_genero <- ggplot(establecimientos, aes(x = categoria_tamaño, y = proporcion_mujeres, 
                                                fill = categoria_tamaño)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Proporción de Mujeres por Tamaño de Empresa",
       x = "Tamaño de Empresa",
       y = "Proporción de Mujeres") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_tamaño_genero)
ggsave(file.path(out_graficos, "boxplot_tamaño_genero.png"), 
       p_tamaño_genero, width = 10, height = 6, dpi = 300)

# Relación entre exportación y proporción de mujeres
p_export_genero <- ggplot(establecimientos, aes(x = factor(es_exportador), y = proporcion_mujeres, 
                                                fill = factor(es_exportador))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Proporción de Mujeres según Estado Exportador",
       x = "Es Exportador",
       y = "Proporción de Mujeres") +
  scale_x_discrete(labels = c("No", "Sí")) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_export_genero)
ggsave(file.path(out_graficos, "boxplot_exportador_genero.png"), 
       p_export_genero, width = 8, height = 6, dpi = 300)

# Scatter plot: tamaño de empleo vs proporción de mujeres (usando muestra para optimización)
cat("Creando scatter plot con muestra optimizada para rendimiento...\n")
set.seed(42)
muestra_scatter <- min(50000, nrow(establecimientos))  # Limitar a 50K puntos
establecimientos_muestra <- establecimientos %>% 
  sample_n(muestra_scatter)

p_scatter_empleo <- ggplot(establecimientos_muestra, aes(x = empleo_numerico, y = proporcion_mujeres)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 0.8) +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  labs(title = "Relación entre Tamaño de Empleo y Proporción de Mujeres",
       subtitle = paste("Muestra representativa de", format(muestra_scatter, big.mark = ","), "establecimientos"),
       x = "Tamaño de Empleo (numérico)",
       y = "Proporción de Mujeres") +
  theme_minimal()

print(p_scatter_empleo)
ggsave(file.path(out_graficos, "scatter_empleo_genero.png"), 
       p_scatter_empleo, width = 10, height = 6, dpi = 300)

# Análisis por región
p_region_genero <- ggplot(establecimientos, aes(x = region, y = proporcion_mujeres, fill = region)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Proporción de Mujeres por Región",
       x = "Región",
       y = "Proporción de Mujeres") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_region_genero)
ggsave(file.path(out_graficos, "boxplot_region_genero.png"), 
       p_region_genero, width = 10, height = 6, dpi = 300)

# =============================================================================
# 8. ANÁLISIS DE CORRELACIONES
# =============================================================================

cat("\n8. ANÁLISIS DE CORRELACIONES\n")
cat("-----------------------------\n")

# Seleccionar variables numéricas para correlación
variables_numericas <- establecimientos %>%
  select(proporcion_mujeres, empleo_numerico, quintil, lat, lon) %>%
  filter(complete.cases(.))

# Calcular matriz de correlación
cor_matrix <- cor(variables_numericas)
print("Matriz de correlación:")
print(round(cor_matrix, 3))

# Visualizar matriz de correlación
png(file.path(out_graficos, "matriz_correlacion.png"), 
    width = 800, height = 600, res = 150)
corrplot.mixed(cor_matrix,  
               tl.col = "black", tl.srt = 45, 
               title = "Matriz de Correlación - Variables Numéricas")
dev.off()

# Identificar correlaciones más fuertes con proporción de mujeres
cor_con_mujeres <- cor_matrix[, "proporcion_mujeres"]
cor_con_mujeres <- cor_con_mujeres[order(abs(cor_con_mujeres), decreasing = TRUE)]
cor_con_mujeres <- cor_con_mujeres[cor_con_mujeres != 1]  # Excluir autocorrelación

print("Variables más correlacionadas con proporción de mujeres:")
print(round(cor_con_mujeres, 3))

# =============================================================================
# 9. ANÁLISIS MULTIVARIANTE
# =============================================================================

cat("\n9. ANÁLISIS MULTIVARIANTE\n")
cat("--------------------------\n")

# Gráfico de pares para variables clave (usando muestra para optimización)
cat("Creando gráfico de pares con muestra optimizada...\n")
variables_clave <- c("proporcion_mujeres", "empleo_numerico", "quintil")
set.seed(42)
muestra_pairs <- min(10000, nrow(establecimientos))  # Limitar a 10K puntos para ggpairs
establecimientos_pairs <- establecimientos %>% 
  sample_n(muestra_pairs)

p_pairs <- GGally::ggpairs(
  establecimientos_pairs[, variables_clave],
  title = paste("Gráfico de Pares - Variables Clave (muestra:", format(muestra_pairs, big.mark = ","), "obs.)")
)

print(p_pairs)
ggsave(file.path(out_graficos, "grafico_pares.png"), 
       p_pairs, width = 12, height = 10, dpi = 300)

# =============================================================================
# 10. ANÁLISIS POR GRUPOS DE INTERÉS
# =============================================================================

cat("\n10. ANÁLISIS POR GRUPOS DE INTERÉS\n")
cat("-----------------------------------\n")

# Crear grupos basados en la proporción de mujeres
establecimientos <- establecimientos %>%
  mutate(grupo_genero = case_when(
    proporcion_mujeres < 0.3 ~ "Predominio Masculino",
    proporcion_mujeres < 0.7 ~ "Composición Mixta",
    TRUE ~ "Predominio Femenino"
  ) %>% factor(levels = c("Predominio Masculino", "Composición Mixta", "Predominio Femenino")))

# Distribución de grupos de género
tabla_genero <- table(establecimientos$grupo_genero)
print("Distribución por composición de género:")
print(tabla_genero)
print("Porcentajes:")
print(round(prop.table(tabla_genero) * 100, 2))

# Análisis de exportación por grupo de género
tabla_cruzada <- table(establecimientos$grupo_genero, establecimientos$es_exportador)
print("Tabla cruzada: Composición de género vs Exportación:")
print(tabla_cruzada)
print("Porcentajes por fila:")
print(round(prop.table(tabla_cruzada, 1) * 100, 2))

# Gráfico de composición de género por región
p_genero_region <- ggplot(establecimientos, aes(x = region, fill = grupo_genero)) +
  geom_bar(position = "fill", alpha = 0.8) +
  labs(title = "Composición de Género por Región",
       x = "Región",
       y = "Proporción",
       fill = "Composición de Género") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_genero_region)
ggsave(file.path(out_graficos, "composicion_genero_region.png"), 
       p_genero_region, width = 12, height = 6, dpi = 300)

# =============================================================================
# 11. ANÁLISIS SECTORIAL
# =============================================================================

cat("\n11. ANÁLISIS SECTORIAL\n")
cat("-----------------------\n")

# Top 10 sectores por número de establecimientos
top_sectores <- establecimientos %>%
  count(letra_desc, sort = TRUE) %>%
  slice_head(n = 10)

print("Top 10 sectores por número de establecimientos:")
print(top_sectores)

# Gráfico de top sectores
p_top_sectores <- ggplot(top_sectores, aes(x = reorder(letra_desc, n), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top 10 Sectores por Número de Establecimientos",
       x = "Sector Económico",
       y = "Número de Establecimientos") +
  theme_minimal()

print(p_top_sectores)
ggsave(file.path(out_graficos, "top_sectores.png"), 
       p_top_sectores, width = 12, height = 8, dpi = 300)

# Proporción de mujeres por sector (top 10)
genero_por_sector <- establecimientos %>%
  filter(letra_desc %in% top_sectores$letra_desc) %>%
  group_by(letra_desc) %>%
  summarise(
    establecimientos = n(),
    prop_mujeres_promedio = mean(proporcion_mujeres, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(prop_mujeres_promedio))

print("Proporción promedio de mujeres por sector (Top 10):")
print(genero_por_sector)

# Gráfico de composición de género por sectores
cat("\nCreando gráfico de composición de género por sectores...\n")

# Promedio nacional de participación femenina
promedio_nacional <- mean(establecimientos$proporcion_mujeres, na.rm = TRUE)

# Análisis extendido por sector (todos los sectores con suficientes datos)
estadisticas_sector_completo <- establecimientos %>%
  filter(!is.na(proporcion_mujeres), !is.na(letra_desc)) %>%
  group_by(letra_desc) %>%
  summarise(
    n_establecimientos = n(),
    participacion_media = mean(proporcion_mujeres, na.rm = TRUE),
    participacion_mediana = median(proporcion_mujeres, na.rm = TRUE),
    desviacion_estandar = sd(proporcion_mujeres, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Filtrar sectores con al menos 1000 establecimientos para representatividad
  filter(n_establecimientos >= 1000) %>%
  # Ordenar por participación media descendente
  arrange(desc(participacion_media)) %>%
  # Agregar diferencia con respecto al promedio nacional
  mutate(
    diferencia_nacional = participacion_media - promedio_nacional,
    sector_corto = str_trunc(letra_desc, 35, "right")
  )

# Gráfico principal de composición de género
p_composicion_genero_sectores <- estadisticas_sector_completo %>%
  ggplot(aes(x = reorder(sector_corto, participacion_media), 
             y = participacion_media)) +
  # Barras coloreadas según posición relativa al promedio
  geom_col(aes(fill = participacion_media > promedio_nacional), 
           alpha = 0.8, width = 0.7) +
  # Línea de promedio nacional
  geom_hline(yintercept = promedio_nacional, 
             color = "red", linetype = "dashed", size = 1.2) +
  # Anotación del promedio nacional
  annotate("text", 
           x = nrow(estadisticas_sector_completo) * 0.75, 
           y = promedio_nacional + 0.02, 
           label = paste("Promedio Nacional:", sprintf("%.1f%%", promedio_nacional * 100)),
           color = "red", size = 4, fontface = "bold") +
  # Etiquetas en las barras
  geom_text(aes(label = sprintf("%.1f%%", participacion_media * 100)),
            hjust = -0.1, size = 3) +
  # Coordenadas volteadas para barras horizontales
  coord_flip() +
  # Escalas y colores
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, max(estadisticas_sector_completo$participacion_media) * 1.1)) +
  scale_fill_manual(values = c("FALSE" = "#E74C3C", "TRUE" = "#27AE60"),
                    labels = c("Debajo del promedio", "Encima del promedio")) +
  # Títulos y etiquetas
  labs(
    title = "Composición de Género en Establecimientos Productivos Argentinos",
    subtitle = paste("Participación femenina por sector económico vs. promedio nacional",
                     sprintf("(%.1f%%)", promedio_nacional * 100)),
    x = "Sector Económico",
    y = "Participación Femenina (%)",
    fill = "Posición relativa:",
    caption = "Fuente: CEP XXI - Establecimientos Productivos 2021-2022\nSectores con al menos 1,000 establecimientos"
  ) +
  # Tema
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_composicion_genero_sectores)
ggsave(file.path(out_graficos, "composicion_genero_sectores.png"), 
       p_composicion_genero_sectores, width = 16, height = 12, dpi = 300)

# Gráfico de variabilidad por sectores (boxplot)
cat("Creando gráfico de variabilidad por sectores...\n")

# Preparar datos para boxplot de variabilidad
datos_variabilidad <- establecimientos %>%
  filter(!is.na(proporcion_mujeres), !is.na(letra_desc)) %>%
  # Mantener solo sectores principales
  group_by(letra_desc) %>%
  filter(n() >= 1000) %>%
  ungroup() %>%
  # Agregar categorización
  mutate(
    sector_corto = str_trunc(letra_desc, 30, "right"),
    sector_corto = fct_reorder(sector_corto, proporcion_mujeres, .fun = mean)
  )

p_variabilidad_sectores <- datos_variabilidad %>%
  ggplot(aes(x = sector_corto, y = proporcion_mujeres)) +
  geom_boxplot(aes(fill = sector_corto), alpha = 0.7, outlier.alpha = 0.3) +
  geom_hline(yintercept = promedio_nacional, 
             color = "red", linetype = "dashed", size = 1) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_viridis_d() +
  labs(
    title = "Variabilidad de Participación Femenina por Sector",
    subtitle = "Distribución de la proporción de mujeres en establecimientos",
    x = "Sector Económico",
    y = "Proporción de Mujeres",
    caption = "Fuente: CEP XXI - Establecimientos Productivos 2021-2022\nLínea roja: promedio nacional"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    axis.text.y = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_variabilidad_sectores)
ggsave(file.path(out_graficos, "variabilidad_genero_sectores.png"), 
       p_variabilidad_sectores, width = 14, height = 10, dpi = 300)

# Guardar tabla de estadísticas sectoriales
write_csv(estadisticas_sector_completo, file.path(out_stats, "composicion_genero_sectores.csv"))

# Mostrar estadísticas clave
cat("\n📊 ESTADÍSTICAS CLAVE DE COMPOSICIÓN DE GÉNERO:\n")
cat("===============================================\n")
cat("🎯 Promedio nacional de participación femenina:", sprintf("%.1f%%", promedio_nacional * 100), "\n")

# Sector con mayor participación femenina
sector_mayor <- estadisticas_sector_completo %>% slice_max(participacion_media, n = 1)
cat("🏆 Sector con MAYOR participación femenina:\n")
cat("   ", sector_mayor$letra_desc, ":", sprintf("%.1f%%", sector_mayor$participacion_media * 100), "\n")

# Sector con menor participación femenina
sector_menor <- estadisticas_sector_completo %>% slice_min(participacion_media, n = 1)
cat("📉 Sector con MENOR participación femenina:\n")
cat("   ", sector_menor$letra_desc, ":", sprintf("%.1f%%", sector_menor$participacion_media * 100), "\n")

# Rango de variación
rango_variacion <- max(estadisticas_sector_completo$participacion_media) - min(estadisticas_sector_completo$participacion_media)
cat("📊 Rango de variación entre sectores:", sprintf("%.1f puntos porcentuales", rango_variacion * 100), "\n")

# Sectores por encima y debajo del promedio
sectores_encima <- sum(estadisticas_sector_completo$participacion_media > promedio_nacional)
sectores_debajo <- sum(estadisticas_sector_completo$participacion_media <= promedio_nacional)
cat("⬆️ Sectores por encima del promedio nacional:", sectores_encima, "\n")
cat("⬇️ Sectores por debajo del promedio nacional:", sectores_debajo, "\n\n")

# =============================================================================
# 12. ANÁLISIS DE CLUSTERING SECTORIAL
# =============================================================================

cat("\n12. ANÁLISIS DE CLUSTERING SECTORIAL\n")
cat("-------------------------------------\n")

# Cargar paquetes adicionales para clustering
suppressMessages({
  library(cluster)
  library(factoextra)
  library(FactoMineR)
  library(ggrepel)
  library(tibble)
})

cat("¿Existen clusters de sectores con patrones distintivos de género?\n\n")

# Crear dataset agregado por sector
sectores_genero_base <- establecimientos %>%
  group_by(letra, letra_desc) %>%
  summarise(
    establecimientos = n(),
    prop_mujeres_promedio = mean(proporcion_mujeres, na.rm = TRUE),
    prop_mujeres_mediana = median(proporcion_mujeres, na.rm = TRUE),
    tamaño_promedio = mean(empleo_numerico, na.rm = TRUE),
    tasa_exportadores = mean(quintil > 0, na.rm = TRUE),
    prop_pequeñas = mean(categoria_tamaño == "Pequeña", na.rm = TRUE),
    distribucion_geografica = n_distinct(provincia),
    .groups = 'drop'
  ) %>%
  filter(establecimientos >= 1000)

cat("Sectores analizados:", nrow(sectores_genero_base), "\n")

# Preparación de datos para clustering
cat("\nPreparación de datos para clustering:\n")

# Seleccionar variables con varianza
vars_con_varianza <- sapply(sectores_genero_base[,-c(1,2)], function(x) sd(x, na.rm = TRUE)) > 0
sectores_genero <- sectores_genero_base[, c(TRUE, TRUE, vars_con_varianza)]
cat("- Variables con varianza:", sum(vars_con_varianza), "de", length(vars_con_varianza), "\n")

# Verificar y limpiar datos
datos_cluster_sectores <- sectores_genero[,-c(1,2)] %>%
  mutate(across(everything(), ~ifelse(is.na(.) | is.infinite(.), 0, .)))

# Estandarizar datos (requisito para k-means)
datos_cluster_sectores <- scale(datos_cluster_sectores)
rownames(datos_cluster_sectores) <- make.unique(sectores_genero$letra_desc)

cat("- Datos escalados - NA:", sum(is.na(datos_cluster_sectores)), "\n")
cat("- Datos escalados - Inf:", sum(is.infinite(datos_cluster_sectores)), "\n")

# Determinar k óptimo
cat("\nEvaluación del número óptimo de clusters (k)...\n")

# Método del codo (WSS - within sum of squares)
p_elbow <- fviz_nbclust(
  datos_cluster_sectores,
  kmeans,
  method = "wss",
  k.max = 10,
  nstart = 25
) + labs(title = "Método del Codo (WSS)", x = "Número de clusters (k)", y = "WSS total")

print(p_elbow)
ggsave(file.path(out_graficos, "clustering_metodo_codo.png"), 
       p_elbow, width = 10, height = 6, dpi = 300)

# Método de la Silhouette
p_silhouette <- fviz_nbclust(
  datos_cluster_sectores,
  kmeans,
  method = "silhouette",
  k.max = 10,
  nstart = 25
) + labs(title = "Método de Silhouette", x = "Número de clusters (k)", y = "Silhouette promedio")

print(p_silhouette)
ggsave(file.path(out_graficos, "clustering_metodo_silhouette.png"), 
       p_silhouette, width = 10, height = 6, dpi = 300)

# Clustering final
k_sectores <- 3  # Basado en interpretabilidad de negocio
kmeans_sectores <- kmeans(datos_cluster_sectores, centers = k_sectores, nstart = 25)

# Análisis Silhouette detallado
sil_sectores_detallado <- cluster::silhouette(kmeans_sectores$cluster, dist(datos_cluster_sectores))
sil_promedio_sectores <- mean(sil_sectores_detallado[, "sil_width"])

cat("\n=== MÉTRICAS DE VALIDACIÓN ===\n\n")

# 1. Silhouette Score promedio
cat("1. Silhouette Score promedio:", round(sil_promedio_sectores, 3), "\n")

# Interpretación de calidad del clustering
if(sil_promedio_sectores > 0.7) {
  interpretacion_sil <- "✅ Clustering EXCELENTE (>0.7)"
} else if(sil_promedio_sectores > 0.5) {
  interpretacion_sil <- "✅ Clustering de BUENA calidad (0.5-0.7)"
} else if(sil_promedio_sectores > 0.25) {
  interpretacion_sil <- "⚠️ Clustering de calidad RAZONABLE (0.25-0.5)"
} else {
  interpretacion_sil <- "❌ Clustering de BAJA calidad (<0.25)"
}
cat("   Interpretación:", interpretacion_sil, "\n")

# 2. Within Sum of Squares
cat("2. Within Sum of Squares:", round(kmeans_sectores$tot.withinss, 2), "\n")

# 3. Between Sum of Squares / Total Sum of Squares
bss_tss_ratio_sectores <- kmeans_sectores$betweenss / kmeans_sectores$totss
cat("3. BSS/TSS ratio:", round(bss_tss_ratio_sectores, 3), "\n")
cat("   (Mayor es mejor - indica separación entre clusters)\n")

# 4. Varianza explicada
variabilidad_explicada_sectores <- (kmeans_sectores$betweenss / kmeans_sectores$totss) * 100
cat("4. Varianza explicada:", round(variabilidad_explicada_sectores, 1), "%\n")

# Visualización del silhouette plot detallado
p_silhouette_detallado <- fviz_silhouette(sil_sectores_detallado) +
  labs(title = "Análisis Silhouette Detallado - Clustering Sectorial",
       subtitle = paste("Silhouette Score promedio:", round(sil_promedio_sectores, 3))) +
  theme_minimal()

print(p_silhouette_detallado)
ggsave(file.path(out_graficos, "kmeans_silhouette_detallado_sectores.png"), 
       p_silhouette_detallado, width = 12, height = 8, dpi = 300)

# Agregar asignaciones de cluster al dataset original
sectores_con_clusters <- sectores_genero %>%
  mutate(cluster = as.factor(kmeans_sectores$cluster))

# Estadísticas por cluster
cat("\nESTADÍSTICAS POR CLUSTER:\n")
estadisticas_cluster_sectores <- sectores_con_clusters %>%
  group_by(cluster) %>%
  summarise(
    n_sectores = n(),
    establecimientos_total = sum(establecimientos),
    prop_mujeres_promedio = round(mean(prop_mujeres_promedio), 3),
    prop_mujeres_mediana = round(median(prop_mujeres_promedio), 3),
    tasa_exportadores_promedio = round(mean(tasa_exportadores), 3),
    tamaño_promedio_sectores = round(mean(tamaño_promedio), 1),
    prop_pequeñas_promedio = round(mean(prop_pequeñas), 1),
    .groups = 'drop'
  )

print(estadisticas_cluster_sectores)

# Análisis detallado por cluster
cat("\nANÁLISIS DETALLADO POR CLUSTER:\n")
cat("===============================\n")

for(i in 1:k_sectores) {
  cat("\n📊 CLUSTER", i, "- ANÁLISIS DETALLADO\n")
  cat(rep("-", 40), "\n")
  
  # Sectores en este cluster
  sectores_cluster <- sectores_con_clusters %>%
    filter(cluster == i) %>%
    arrange(desc(prop_mujeres_promedio)) %>%
    select(letra_desc, establecimientos, prop_mujeres_promedio, tasa_exportadores, tamaño_promedio)
  
  cat("Sectores incluidos:\n")
  print(sectores_cluster)
  
  # Características principales
  avg_fem <- mean(sectores_cluster$prop_mujeres_promedio)
  avg_export <- mean(sectores_cluster$tasa_exportadores)
  avg_tamaño <- mean(sectores_cluster$tamaño_promedio)
  total_establecimientos <- sum(sectores_cluster$establecimientos)
  
  cat("\nCaracterísticas principales:\n")
  cat("• Participación femenina promedio:", round(avg_fem, 3), "\n")
  cat("• Tasa de exportadores promedio:", round(avg_export, 3), "%\n")
  cat("• Tamaño promedio de empresas:", round(avg_tamaño, 1), "empleados\n")
  cat("• Total de establecimientos:", format(total_establecimientos, big.mark = ","), "\n")
}

# Visualización comparativa de clusters
# Calcular top sectores para mostrar etiquetas
top_sectores_cluster <- sectores_con_clusters %>%
  top_n(15, establecimientos)

# Gráfico mejorado
p_clusters_interpretacion <- sectores_con_clusters %>%
  ggplot(aes(x = prop_mujeres_promedio, y = tasa_exportadores,
             color = cluster, size = establecimientos)) +
  geom_point(alpha = 0.8) +
  geom_text_repel(
    data = top_sectores_cluster,
    aes(label = letra_desc),
    size = 2.8, force = 3, max.overlaps = Inf,
    show.legend = FALSE
  ) +
  # Agregar centroides
  geom_point(data = as.data.frame(kmeans_sectores$centers) %>%
               rownames_to_column("cluster") %>%
               mutate(cluster = as.factor(cluster)),
             aes(x = prop_mujeres_promedio, y = tasa_exportadores),
             color = "black", size = 6, shape = 4, stroke = 2, show.legend = FALSE) +
  labs(
    title = "Interpretación de Clusters Sectoriales",
    subtitle = paste("K-means con K =", k_sectores, "| Centroides marcados con ✕"),
    x = "Participación Femenina Promedio",
    y = "Tasa de Exportadores (%)",
    color = "Cluster",
    size = "Establecimientos"
  ) +
  scale_color_manual(values = c("red", "blue", "green")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 0.3)) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_clusters_interpretacion)
ggsave(file.path(out_graficos, "clusters_sectoriales_interpretacion.png"), 
       p_clusters_interpretacion, width = 14, height = 10, dpi = 300)

# Guardar resultados detallados
write_csv(estadisticas_cluster_sectores, 
          file.path(out_stats, "caracteristicas_clusters_sectores.csv"))

# Conclusiones del clustering
cat("\n🎯 CONCLUSIONES DEL CLUSTERING:\n")
cat("✅ K-means identificó", k_sectores, "grupos naturales de sectores económicos\n")
cat("✅ Silhouette score:", round(sil_promedio_sectores, 3), "-", interpretacion_sil, "\n")
cat("✅ Varianza explicada:", round(variabilidad_explicada_sectores, 1), "% (BSS/TSS =", round(bss_tss_ratio_sectores, 3), ")\n")

# =============================================================================
# 13. RESUMEN ESTADÍSTICO FINAL
# =============================================================================

cat("\n13. RESUMEN ESTADÍSTICO FINAL\n")
cat("------------------------------\n")

# Crear resumen ejecutivo
resumen_ejecutivo <- establecimientos %>%
  summarise(
    total_establecimientos = n(),
    regiones = n_distinct(region),
    provincias = n_distinct(provincia),
    sectores_economicos = n_distinct(letra_desc),
    proporcion_mujeres_promedio = round(mean(proporcion_mujeres, na.rm = TRUE), 3),
    proporcion_mujeres_mediana = round(median(proporcion_mujeres, na.rm = TRUE), 3),
    porcentaje_exportadores = round(mean(es_exportador, na.rm = TRUE) * 100, 2),
    tamaño_promedio_empleo = round(mean(empleo_numerico, na.rm = TRUE), 1)
  )

print("RESUMEN EJECUTIVO DEL ANÁLISIS:")
print(resumen_ejecutivo)

# Guardar resumen
write_csv(resumen_ejecutivo, file.path(out_stats, "resumen_ejecutivo_eda.csv"))

# Guardar insights clave
insights_clave <- data.frame(
  metrica = c("Total establecimientos", "Región más concentrada", 
              "Participación femenina promedio", "Tasa de exportadores",
              "Sector más numeroso"),
  valor = c(
    format(nrow(establecimientos), big.mark = "."),
    names(sort(table(establecimientos$region), decreasing = TRUE))[1],
    paste0(round(mean(establecimientos$proporcion_mujeres, na.rm = TRUE) * 100, 1), "%"),
    paste0(round(mean(establecimientos$es_exportador) * 100, 1), "%"),
    top_sectores$letra_desc[1]
  )
)

print("\nINSIGHTS CLAVE:")
print(insights_clave)

write_csv(insights_clave, file.path(out_stats, "insights_clave.csv"))

cat("\n=== ANÁLISIS EXPLORATORIO COMPLETADO ===\n")
cat("Gráficos generados en:", out_graficos, "\n")
cat("Estadísticas guardadas en:", out_stats, "\n")
cat("Siguiente paso: Ejecutar 04_ml_modeling.R\n")