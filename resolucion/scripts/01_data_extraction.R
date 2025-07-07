# =============================================================================
# EXTRACCIÓN Y VALIDACIÓN DE DATOS - ESTABLECIMIENTOS PRODUCTIVOS ARGENTINOS
# =============================================================================
# Objetivo: Cargar y validar datos de establecimientos productivos del CEP XXI

# =============================================================================
# 1. CARGAR PAQUETES NECESARIOS
# =============================================================================

library(tidyverse)  # Para manipulación de datos y visualización
library(readr)      # Para lectura eficiente de archivos CSV
library(skimr)      # Para estadísticas descriptivas mejoradas

# Configuración inicial
options(scipen = 999)  # Evitar notación científica
set.seed(42)          # Para reproducibilidad

# =============================================================================
# 2. DEFINIR RUTAS DE TRABAJO
# =============================================================================

# Definir rutas de entrada y salida
in_datos <- "raw"
out_input <- "input"
out_stats <- file.path("output", "statistics")

# Crear directorios si no existen
dir.create(out_input, showWarnings = FALSE, recursive = TRUE)
dir.create(out_stats, showWarnings = FALSE, recursive = TRUE)

cat("=== CARGA Y VALIDACIÓN DE DATOS ===\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# =============================================================================
# 3. FUNCIONES AUXILIARES PARA CARGA DE DATOS
# =============================================================================

# Función para cargar datos con múltiples encodings
cargar_csv_seguro <- function(archivo, encoding = "UTF-8") {
  ruta_completa <- file.path(in_datos, archivo)
  
  # Verificar que el archivo existe
  if (!file.exists(ruta_completa)) {
    stop("Archivo no encontrado: ", archivo)
  }
  
  cat("Cargando:", archivo, "...")
  
  # Intentar cargar con diferentes encodings
  tryCatch({
    datos <- read_csv(ruta_completa, locale = locale(encoding = encoding), 
                     show_col_types = FALSE)
    cat(" ✓ Exitoso (", nrow(datos), "filas,", ncol(datos), "columnas)\n")
    return(datos)
  }, error = function(e) {
    cat(" Error con", encoding, ". Probando latin1...\n")
    tryCatch({
      datos <- read_csv(ruta_completa, locale = locale(encoding = "latin1"), 
                       show_col_types = FALSE)
      cat(" ✓ Exitoso con latin1 (", nrow(datos), "filas,", ncol(datos), "columnas)\n")
      return(datos)
    }, error = function(e2) {
      stop("Error al cargar ", archivo, ": ", e2$message)
    })
  })
}

# Función para validar estructura de datos
validar_estructura <- function(datos, columnas_esperadas, nombre_dataset) {
  cat("\n--- Validando", nombre_dataset, "---\n")
  
  columnas_faltantes <- setdiff(columnas_esperadas, names(datos))
  columnas_extra <- setdiff(names(datos), columnas_esperadas)
  
  if (length(columnas_faltantes) > 0) {
    cat("⚠ Columnas faltantes:", paste(columnas_faltantes, collapse = ", "), "\n")
  }
  
  if (length(columnas_extra) > 0) {
    cat("ℹ Columnas adicionales:", paste(columnas_extra, collapse = ", "), "\n")
  }
  
  if (length(columnas_faltantes) == 0) {
    cat("✓ Estructura validada correctamente\n")
  }
  
  return(length(columnas_faltantes) == 0)
}

# =============================================================================
# 4. CARGAR ARCHIVOS DE DATOS
# =============================================================================

cat("4. CARGANDO ARCHIVOS DE DATOS\n")
cat("------------------------------\n")

# Definir archivos a cargar
archivos <- list(
  actividades = "actividades_establecimientos.csv",
  departamentos = "codigo_departamento_provincia.csv",
  depto_actividad = "depto_actividad_genero.csv",
  establecimientos = "distribucion_establecimientos_productivos_sexo.csv"
)

# Cargar todos los archivos
datos <- map(archivos, cargar_csv_seguro)

# Mostrar dimensiones de cada dataset
cat("\nResumen de datos cargados:\n")
for (nombre in names(datos)) {
  cat(sprintf("- %-15s: %d filas × %d columnas\n", 
              nombre, nrow(datos[[nombre]]), ncol(datos[[nombre]])))
}

# =============================================================================
# 5. VALIDACIÓN DE ESTRUCTURA DE DATOS
# =============================================================================

cat("\n5. VALIDACIÓN DE ESTRUCTURA\n")
cat("----------------------------\n")

# Definir columnas esperadas para cada dataset
esquemas <- list(
  actividades = c("clae6", "clae2", "letra", "clae6_desc", "clae2_desc", "letra_desc"),
  departamentos = c("provincia_id", "in_departamentos", "departamento", "provincia"),
  depto_actividad = c("anio", "in_departamentos", "departamento", "provincia_id", 
                     "provincia", "clae6", "clae2", "letra", "genero", "Empleo", 
                     "Establecimientos", "empresas_exportadoras"),
  establecimientos = c("cuit", "sucursal", "anio", "lat", "lon", "clae6", 
                      "in_departamentos", "provincia_id", "quintil", "empleo", 
                      "proporcion_mujeres")
)

# Validar estructura de cada dataset
validaciones <- map2_lgl(datos, esquemas, ~ validar_estructura(.x, .y, .y))

if (all(validaciones)) {
  cat("\n✓ Todas las validaciones de estructura exitosas\n")
} else {
  cat("\n⚠ Algunas validaciones fallaron\n")
}

# =============================================================================
# 6. ANÁLISIS DE CALIDAD DE DATOS
# =============================================================================

cat("\n6. ANÁLISIS DE CALIDAD DE DATOS\n")
cat("--------------------------------\n")

# Función para analizar calidad
analizar_calidad <- function(datos, nombre) {
  cat("\n--- Calidad de", nombre, "---\n")
  
  # Estadísticas básicas
  cat("Dimensiones:", nrow(datos), "×", ncol(datos), "\n")
  
  # Valores faltantes
  valores_na <- datos %>%
    summarise_all(~ sum(is.na(.))) %>%
    pivot_longer(everything(), names_to = "columna", values_to = "na_count") %>%
    filter(na_count > 0) %>%
    mutate(porcentaje_na = round(na_count / nrow(datos) * 100, 2))
  
  if (nrow(valores_na) > 0) {
    cat("Valores faltantes encontrados:\n")
    print(valores_na)
  } else {
    cat("✓ No hay valores faltantes\n")
  }
  
  # Duplicados
  duplicados <- sum(duplicated(datos))
  cat("Filas duplicadas:", duplicados, 
      sprintf("(%.2f%%)\n", duplicados/nrow(datos)*100))
  
  # Guardar resumen detallado
  resumen_archivo <- file.path(out_stats, paste0("calidad_", nombre, ".txt"))
  sink(resumen_archivo)
  cat("=== RESUMEN DE CALIDAD DE DATOS ===\n")
  cat("Dataset:", nombre, "\n")
  cat("Fecha:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  print(skim(datos))
  sink()
  
  return(valores_na)
}

# Analizar calidad de cada dataset
calidad_resultados <- map2(datos, names(datos), analizar_calidad)

# =============================================================================
# 7. VALIDACIONES CRUZADAS ENTRE DATASETS
# =============================================================================

cat("\n7. VALIDACIONES CRUZADAS\n")
cat("------------------------\n")

# Verificar integridad referencial
cat("Verificando códigos de actividad económica...\n")
actividades_en_establecimientos <- unique(datos$establecimientos$clae6)
actividades_validas <- unique(datos$actividades$clae6)
actividades_faltantes <- setdiff(actividades_en_establecimientos, actividades_validas)

if (length(actividades_faltantes) > 0) {
  cat("⚠ Códigos de actividad en establecimientos no encontrados en catálogo:\n")
  cat("Cantidad:", length(actividades_faltantes), "\n")
  cat("Primeros 10:", head(actividades_faltantes, 10), "\n")
} else {
  cat("✓ Todos los códigos de actividad son válidos\n")
}

# Verificar códigos de departamento
cat("\nVerificando códigos de departamento...\n")
departamentos_en_establecimientos <- datos$establecimientos %>%
  select(provincia_id, in_departamentos) %>%
  distinct()

departamentos_validos <- datos$departamentos %>%
  select(provincia_id, in_departamentos) %>%
  distinct()

departamentos_faltantes <- anti_join(departamentos_en_establecimientos, 
                                   departamentos_validos,
                                   by = c("provincia_id", "in_departamentos"))

if (nrow(departamentos_faltantes) > 0) {
  cat("⚠ Códigos de departamento en establecimientos no encontrados:\n")
  cat("Cantidad:", nrow(departamentos_faltantes), "\n")
} else {
  cat("✓ Todos los códigos de departamento son válidos\n")
}

# =============================================================================
# 8. VALIDACIÓN DE COORDENADAS GEOGRÁFICAS
# =============================================================================

cat("\n8. VALIDACIÓN GEOGRÁFICA\n")
cat("------------------------\n")

# Definir límites aproximados de Argentina
limites_argentina <- list(
  lat_min = -55, lat_max = -21.5,
  lon_min = -74, lon_max = -53.5
)

# Validar coordenadas
coordenadas_validas <- datos$establecimientos %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(
    lat_valida = lat >= limites_argentina$lat_min & lat <= limites_argentina$lat_max,
    lon_valida = lon >= limites_argentina$lon_min & lon <= limites_argentina$lon_max,
    coordenada_valida = lat_valida & lon_valida
  )

cat("Validación de coordenadas:\n")
cat("Total establecimientos con coordenadas:", nrow(coordenadas_validas), "\n")
cat("Coordenadas válidas:", sum(coordenadas_validas$coordenada_valida), 
    sprintf("(%.2f%%)\n", sum(coordenadas_validas$coordenada_valida)/nrow(coordenadas_validas)*100))
cat("Latitudes inválidas:", sum(!coordenadas_validas$lat_valida), "\n")
cat("Longitudes inválidas:", sum(!coordenadas_validas$lon_valida), "\n")

# =============================================================================
# 9. ANÁLISIS TEMPORAL
# =============================================================================

cat("\n9. ANÁLISIS TEMPORAL\n")
cat("--------------------\n")

# Analizar distribución por años
anios_establecimientos <- table(datos$establecimientos$anio)
anios_depto_actividad <- table(datos$depto_actividad$anio)

cat("Distribución temporal - Establecimientos:\n")
print(anios_establecimientos)

cat("\nDistribución temporal - Departamento-Actividad:\n")
print(anios_depto_actividad)

# Verificar consistencia temporal
if (all(datos$establecimientos$anio %in% c(2021, 2022)) && 
    all(datos$depto_actividad$anio %in% c(2021, 2022))) {
  cat("✓ Datos temporales consistentes (2021-2022)\n")
} else {
  cat("⚠ Inconsistencias en datos temporales\n")
}

# =============================================================================
# 10. GUARDAR DATOS VALIDADOS
# =============================================================================

cat("\n10. GUARDANDO DATOS VALIDADOS\n")
cat("------------------------------\n")

# Guardar cada dataset validado
iwalk(datos, ~ {
  archivo_salida <- file.path(out_input, paste0(.y, "_validado.rds"))
  saveRDS(.x, archivo_salida)
  cat("✓ Guardado:", archivo_salida, "\n")
})

# Crear resumen de validación
resumen_validacion <- data.frame(
  Dataset = names(datos),
  Filas = map_dbl(datos, nrow),
  Columnas = map_dbl(datos, ncol),
  Valores_Faltantes = map_dbl(calidad_resultados, ~ sum(.x$na_count)),
  Duplicados = map_dbl(datos, ~ sum(duplicated(.)))
)

# Guardar resumen
write_csv(resumen_validacion, file.path(out_stats, "resumen_validacion.csv"))

# =============================================================================
# 11. RESUMEN FINAL
# =============================================================================

cat("\n=== RESUMEN DE EXTRACCIÓN DE DATOS ===\n")
cat("Datos procesados:\n")
print(resumen_validacion)

cat("\nArchivos generados:\n")
cat("- Datos validados en:", out_input, "\n")
cat("- Reportes de calidad en:", out_stats, "\n")

cat("\n✓ Extracción de datos completada exitosamente\n")
cat("Siguiente paso: Ejecutar 02_data_cleaning.R\n")