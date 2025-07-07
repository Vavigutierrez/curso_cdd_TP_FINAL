# =============================================================================
# LIMPIEZA Y TRANSFORMACIÓN DE DATOS - ESTABLECIMIENTOS PRODUCTIVOS ARGENTINOS
# =============================================================================
# Objetivo: Limpiar y estandarizar datos de establecimientos productivos
#
# VERSIÓN REVISADA: Filtros menos agresivos para reducir pérdida de datos
# - CLAE6: Acepta códigos de 5 y 6 dígitos
# - Sin filtros geográficos estrictos
# - Validación más permisiva

# =============================================================================
# 1. CARGAR PAQUETES NECESARIOS
# =============================================================================

library(tidyverse)  # Para manipulación de datos y visualización
library(lubridate)  # Para manejo de fechas
library(stringr)    # Para manipulación de texto

# Configuración inicial
options(scipen = 999)  # Evitar notación científica
set.seed(42)          # Para reproducibilidad

# =============================================================================
# 2. DEFINIR RUTAS DE TRABAJO
# =============================================================================

# Definir rutas de entrada y salida
in_input <- "input"
out_input <- "input"
out_stats <- file.path("output", "statistics")

cat("=== LIMPIEZA Y TRANSFORMACIÓN DE DATOS ===\n")
cat("Inicio:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# =============================================================================
# 3. FUNCIONES AUXILIARES PARA LIMPIEZA
# =============================================================================

# Función para limpiar columnas de texto
limpiar_texto <- function(datos, columnas) {
  datos %>%
    mutate(across(all_of(columnas), ~ str_trim(str_squish(.))))
}

# Función para estandarizar categorías
estandarizar_categorica <- function(datos, columna) {
  datos %>%
    mutate(!!columna := str_to_title(!!sym(columna)),
           !!columna := str_replace_all(!!sym(columna), "\\s+", " "),
           !!columna := factor(!!sym(columna)))
}

# Función para crear log de limpieza
crear_log_limpieza <- function(etapa, descripcion, antes, despues) {
  data.frame(
    etapa = etapa,
    descripcion = descripcion,
    registros_antes = antes,
    registros_despues = despues,
    registros_removidos = antes - despues,
    porcentaje_removido = round((antes - despues) / antes * 100, 2),
    timestamp = Sys.time()
  )
}

# Inicializar log de limpieza
log_limpieza <- data.frame()

# =============================================================================
# 4. CARGAR DATOS VALIDADOS
# =============================================================================

cat("4. CARGANDO DATOS VALIDADOS\n")
cat("----------------------------\n")

# Cargar todos los datasets validados
actividades <- readRDS(file.path(in_input, "actividades_validado.rds"))
departamentos <- readRDS(file.path(in_input, "departamentos_validado.rds"))
depto_actividad <- readRDS(file.path(in_input, "depto_actividad_validado.rds"))
establecimientos <- readRDS(file.path(in_input, "establecimientos_validado.rds"))

cat("✓ Actividades cargadas:", nrow(actividades), "registros\n")
cat("✓ Departamentos cargados:", nrow(departamentos), "registros\n")
cat("✓ Depto-Actividad cargado:", nrow(depto_actividad), "registros\n")
cat("✓ Establecimientos cargados:", nrow(establecimientos), "registros\n")

# Guardar conteos originales
conteos_originales <- list(
  actividades = nrow(actividades),
  departamentos = nrow(departamentos),
  depto_actividad = nrow(depto_actividad),
  establecimientos = nrow(establecimientos)
)

# =============================================================================
# 5. LIMPIAR DATASET DE ACTIVIDADES
# =============================================================================

cat("\n5. LIMPIANDO ACTIVIDADES ECONÓMICAS\n")
cat("------------------------------------\n")

# Limpiar y estandarizar actividades
actividades_limpio <- actividades %>%
  # Limpiar columnas de texto
  limpiar_texto(c("clae6_desc", "clae2_desc", "letra_desc")) %>%
  # Arreglar problemas de encoding
  mutate(
    clae6_desc = str_replace_all(clae6_desc, "\\s+", " "),
    clae2_desc = str_replace_all(clae2_desc, "\\s+", " "),
    letra_desc = str_replace_all(letra_desc, "\\s+", " ")
  ) %>%
  # Asegurar tipos correctos
  mutate(
    clae6 = as.integer(clae6),
    clae2 = as.integer(clae2),
    letra = as.character(letra)
  ) %>%
  # Eliminar duplicados por clave primaria
  distinct(clae6, .keep_all = TRUE) %>%
  # Filtrar registros válidos (menos restrictivo)
  filter(
    !is.na(clae6),
    !is.na(clae2),
    !is.na(letra),
    nchar(as.character(clae6)) %in% c(5, 6),  # CLAE6 puede tener 5 o 6 dígitos
    clae2 > 0,
    letra %in% LETTERS[1:21]  # Secciones ISIC válidas A-U
  )

# Registrar limpieza
log_limpieza <- bind_rows(log_limpieza, 
  crear_log_limpieza("Actividades", "Limpieza texto y eliminación duplicados", 
                     conteos_originales$actividades, nrow(actividades_limpio)))

cat("Resultado limpieza actividades:\n")
cat("- Registros originales:", conteos_originales$actividades, "\n")
cat("- Registros después limpieza:", nrow(actividades_limpio), "\n")
cat("- Registros removidos:", conteos_originales$actividades - nrow(actividades_limpio), "\n")

# =============================================================================
# 6. LIMPIAR DATASET DE DEPARTAMENTOS
# =============================================================================

cat("\n6. LIMPIANDO DEPARTAMENTOS\n")
cat("--------------------------\n")

departamentos_limpio <- departamentos %>%
  # Limpiar columnas de texto
  limpiar_texto(c("departamento", "provincia")) %>%
  # Estandarizar nombres de provincias
  mutate(
    provincia = case_when(
      str_detect(provincia, "^Buenos Aires$") ~ "Buenos Aires",
      str_detect(provincia, "^CABA$|^Ciudad|^Capital") ~ "CABA", 
      str_detect(provincia, "^Cordoba$|^Córdoba$") ~ "Córdoba",
      str_detect(provincia, "^Entre Rios$") ~ "Entre Ríos",
      str_detect(provincia, "^Neuquen$") ~ "Neuquén",
      str_detect(provincia, "^Rio Negro$") ~ "Río Negro",
      str_detect(provincia, "^Santa Fe$") ~ "Santa Fe",
      str_detect(provincia, "^Tucuman$") ~ "Tucumán",
      TRUE ~ provincia
    )
  ) %>%
  # Asegurar tipos correctos
  mutate(
    provincia_id = as.integer(provincia_id),
    in_departamentos = as.integer(in_departamentos)
  ) %>%
  # Eliminar duplicados
  distinct(provincia_id, in_departamentos, .keep_all = TRUE) %>%
  # Filtrar registros válidos
  filter(
    !is.na(provincia_id),
    !is.na(in_departamentos),
    provincia_id > 0,
    in_departamentos > 0,
    !is.na(departamento),
    !is.na(provincia)
  )

# Registrar limpieza
log_limpieza <- bind_rows(log_limpieza,
  crear_log_limpieza("Departamentos", "Estandarización texto y validación",
                     conteos_originales$departamentos, nrow(departamentos_limpio)))

cat("Resultado limpieza departamentos:\n")
cat("- Registros originales:", conteos_originales$departamentos, "\n")
cat("- Registros después limpieza:", nrow(departamentos_limpio), "\n")
cat("- Registros removidos:", conteos_originales$departamentos - nrow(departamentos_limpio), "\n")

# =============================================================================
# 7. LIMPIAR DATASET DEPARTAMENTO-ACTIVIDAD-GÉNERO
# =============================================================================

cat("\n7. LIMPIANDO DATOS DEPTO-ACTIVIDAD-GÉNERO\n")
cat("------------------------------------------\n")

depto_actividad_limpio <- depto_actividad %>%
  # Limpiar columnas de texto
  limpiar_texto(c("departamento", "provincia", "genero")) %>%
  # Estandarizar categorías de género
  mutate(
    genero = case_when(
      str_detect(str_to_lower(genero), "^varones?$|^hombres?$|^masculino$|^m$") ~ "Varones",
      str_detect(str_to_lower(genero), "^mujeres?$|^femenino$|^f$") ~ "Mujeres",
      TRUE ~ genero
    )
  ) %>%
  # Asegurar tipos correctos
  mutate(
    anio = as.integer(anio),
    provincia_id = as.integer(provincia_id),
    in_departamentos = as.integer(in_departamentos),
    clae6 = as.integer(clae6),
    clae2 = as.integer(clae2),
    Empleo = as.integer(Empleo),
    Establecimientos = as.integer(Establecimientos),
    empresas_exportadoras = as.integer(empresas_exportadoras)
  ) %>%
  # Filtrar registros válidos (menos restrictivo)
  filter(
    anio %in% c(2021, 2022),
    !is.na(provincia_id) & provincia_id > 0,
    !is.na(in_departamentos) & in_departamentos > 0,
    !is.na(clae6) & nchar(as.character(clae6)) %in% c(5, 6),  # CLAE6 flexible
    genero %in% c("Varones", "Mujeres"),
    !is.na(Empleo) & Empleo >= 0,
    !is.na(Establecimientos) & Establecimientos >= 0
  ) %>%
  # Crear variables derivadas
  mutate(
    empleo_por_establecimiento = ifelse(Establecimientos > 0, 
                                       Empleo / Establecimientos, 0),
    proporcion_exportadoras = ifelse(Establecimientos > 0,
                                   empresas_exportadoras / Establecimientos, 0)
  )

# Registrar limpieza  
log_limpieza <- bind_rows(log_limpieza,
  crear_log_limpieza("Depto-Actividad-Género", "Estandarización y validación",
                     conteos_originales$depto_actividad, nrow(depto_actividad_limpio)))

cat("Resultado limpieza depto-actividad-género:\n")
cat("- Registros originales:", conteos_originales$depto_actividad, "\n")
cat("- Registros después limpieza:", nrow(depto_actividad_limpio), "\n")
cat("- Registros removidos:", conteos_originales$depto_actividad - nrow(depto_actividad_limpio), "\n")

# =============================================================================
# 8. LIMPIAR DATASET DE ESTABLECIMIENTOS
# =============================================================================

cat("\n8. LIMPIANDO ESTABLECIMIENTOS\n")
cat("------------------------------\n")

# Definir categorías de empleo válidas
categorias_empleo <- c("a. 1-9", "b. 10-49", "c. 50-99", "d. 100-199", 
                      "e. 200-499", "f. 500 y más")

establecimientos_limpio <- establecimientos %>%
  # Asegurar tipos correctos
  mutate(
    anio = as.integer(anio),
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    clae6 = as.integer(clae6),
    provincia_id = as.integer(provincia_id),
    in_departamentos = as.integer(in_departamentos),
    quintil = as.integer(quintil),
    proporcion_mujeres = as.numeric(proporcion_mujeres)
  ) %>%
  # Validar campos esenciales (sin filtros geográficos estrictos)
  filter(
    anio %in% c(2021, 2022),
    !is.na(clae6) & nchar(as.character(clae6)) %in% c(5, 6),  # CLAE6 flexible
    !is.na(provincia_id) & provincia_id > 0,
    !is.na(in_departamentos) & in_departamentos > 0,
    empleo %in% categorias_empleo,
    !is.na(proporcion_mujeres) & proporcion_mujeres >= 0 & proporcion_mujeres <= 1,
    quintil >= 0 & quintil <= 5
  ) %>%
  # Crear variables derivadas
  mutate(
    # Tamaño numérico de empleo (punto medio de categorías)
    empleo_numerico = case_when(
      empleo == "a. 1-9" ~ 5,
      empleo == "b. 10-49" ~ 29.5,
      empleo == "c. 50-99" ~ 74.5,
      empleo == "d. 100-199" ~ 149.5,
      empleo == "e. 200-499" ~ 349.5,
      empleo == "f. 500 y más" ~ 750,
      TRUE ~ NA_real_
    ),
    # Categorías de tamaño
    categoria_tamaño = case_when(
      empleo %in% c("a. 1-9") ~ "Micro",
      empleo %in% c("b. 10-49") ~ "Pequeña",
      empleo %in% c("c. 50-99", "d. 100-199") ~ "Mediana",
      empleo %in% c("e. 200-499", "f. 500 y más") ~ "Grande",
      TRUE ~ "Desconocido"
    ),
    # Estado exportador
    es_exportador = quintil > 0,
    categoria_exportacion = case_when(
      quintil == 0 ~ "No exporta",
      quintil == 1 ~ "Exportador bajo",
      quintil %in% 2:3 ~ "Exportador medio",
      quintil %in% 4:5 ~ "Exportador alto",
      TRUE ~ "Desconocido"
    ),
    # Composición de género
    composicion_genero = case_when(
      proporcion_mujeres < 0.3 ~ "Predominio masculino",
      proporcion_mujeres > 0.7 ~ "Predominio femenino", 
      TRUE ~ "Composición mixta"
    )
  ) %>%
  # Eliminar duplicados potenciales
  distinct(cuit, sucursal, anio, .keep_all = TRUE)

# Registrar limpieza
log_limpieza <- bind_rows(log_limpieza,
  crear_log_limpieza("Establecimientos", "Validación coordenadas y estandarización",
                     conteos_originales$establecimientos, nrow(establecimientos_limpio)))

cat("Resultado limpieza establecimientos:\n")
cat("- Registros originales:", conteos_originales$establecimientos, "\n")
cat("- Registros después limpieza:", nrow(establecimientos_limpio), "\n")
cat("- Registros removidos:", conteos_originales$establecimientos - nrow(establecimientos_limpio), "\n")

# =============================================================================
# 9. VALIDAR CONSISTENCIA ENTRE DATASETS
# =============================================================================

cat("\n9. VALIDANDO CONSISTENCIA ENTRE DATASETS\n")
cat("-----------------------------------------\n")

# Verificar integridad referencial después de limpieza
cat("Verificando códigos de actividad...\n")
actividades_en_establecimientos <- unique(establecimientos_limpio$clae6)
actividades_validas <- unique(actividades_limpio$clae6)
actividades_faltantes <- setdiff(actividades_en_establecimientos, actividades_validas)

if (length(actividades_faltantes) > 0) {
  cat("⚠ Encontrados", length(actividades_faltantes), "códigos de actividad no válidos\n")
  # Filtrar establecimientos con códigos inválidos
  establecimientos_limpio <- establecimientos_limpio %>%
    filter(clae6 %in% actividades_validas)
  cat("✓ Removidos establecimientos con códigos inválidos\n")
} else {
  cat("✓ Todos los códigos de actividad son válidos\n")
}

# Verificar códigos de departamento
cat("Verificando códigos de departamento...\n")
establecimientos_deptos <- establecimientos_limpio %>% 
  select(provincia_id, in_departamentos) %>% 
  distinct()

departamentos_validos <- departamentos_limpio %>%
  select(provincia_id, in_departamentos) %>%
  distinct()

deptos_invalidos <- anti_join(establecimientos_deptos, departamentos_validos,
                             by = c("provincia_id", "in_departamentos"))

if (nrow(deptos_invalidos) > 0) {
  cat("⚠ Encontrados", nrow(deptos_invalidos), "códigos de departamento inválidos\n")
  # Filtrar establecimientos con códigos inválidos
  establecimientos_limpio <- establecimientos_limpio %>%
    inner_join(departamentos_validos, by = c("provincia_id", "in_departamentos"))
  cat("✓ Removidos establecimientos con códigos de departamento inválidos\n")
} else {
  cat("✓ Todos los códigos de departamento son válidos\n")
}

# =============================================================================
# 10. CREAR DATASET INTEGRADO
# =============================================================================

cat("\n10. CREANDO DATASET INTEGRADO\n")
cat("------------------------------\n")

# Crear dataset principal combinando todas las tablas
establecimientos_completo <- establecimientos_limpio %>%
  # Unir con actividades
  left_join(actividades_limpio %>% 
            select(clae6, clae6_desc, clae2, clae2_desc, letra, letra_desc),
            by = "clae6") %>%
  # Unir con departamentos
  left_join(departamentos_limpio,
            by = c("provincia_id", "in_departamentos")) %>%
  # Agregar clasificación regional
  mutate(
    region = case_when(
      provincia %in% c("CABA", "Buenos Aires") ~ "Centro",
      provincia %in% c("Córdoba", "Entre Ríos", "Santa Fe") ~ "Centro",
      provincia %in% c("Mendoza", "San Juan", "San Luis", "La Rioja") ~ "Cuyo",
      provincia %in% c("Catamarca", "Jujuy", "Salta", "Santiago del Estero", "Tucumán") ~ "NOA",
      provincia %in% c("Chaco", "Corrientes", "Formosa", "Misiones") ~ "NEA",
      provincia %in% c("Chubut", "La Pampa", "Neuquén", "Río Negro", "Santa Cruz", "Tierra del Fuego") ~ "Patagonia",
      TRUE ~ "Otras"
    )
  )

cat("✓ Dataset integrado creado con", nrow(establecimientos_completo), "establecimientos\n")
cat("✓ Dataset incluye", ncol(establecimientos_completo), "variables\n")

# =============================================================================
# 11. GUARDAR DATASETS LIMPIOS
# =============================================================================

cat("\n11. GUARDANDO DATASETS LIMPIOS\n")
cat("-------------------------------\n")

# Guardar datasets individuales limpios
saveRDS(actividades_limpio, file.path(out_input, "actividades_limpio.rds"))
saveRDS(departamentos_limpio, file.path(out_input, "departamentos_limpio.rds"))
saveRDS(depto_actividad_limpio, file.path(out_input, "depto_actividad_limpio.rds"))
saveRDS(establecimientos_limpio, file.path(out_input, "establecimientos_limpio.rds"))

# Guardar dataset integrado
saveRDS(establecimientos_completo, file.path(out_input, "establecimientos_completo.rds"))

cat("✓ Actividades limpias guardadas\n")
cat("✓ Departamentos limpios guardados\n")
cat("✓ Depto-actividad-género limpio guardado\n")
cat("✓ Establecimientos limpios guardados\n")
cat("✓ Dataset integrado guardado\n")

# Guardar log de limpieza
write_csv(log_limpieza, file.path(out_stats, "log_limpieza.csv"))
cat("✓ Log de limpieza guardado\n")

# =============================================================================
# 12. GENERAR RESUMEN DE LIMPIEZA
# =============================================================================

cat("\n12. GENERANDO RESUMEN DE LIMPIEZA\n")
cat("----------------------------------\n")

# Crear tabla resumen
resumen_limpieza <- data.frame(
  Dataset = c("Actividades", "Departamentos", "Depto-Actividad-Género", "Establecimientos"),
  Registros_Originales = c(conteos_originales$actividades, conteos_originales$departamentos,
                          conteos_originales$depto_actividad, conteos_originales$establecimientos),
  Registros_Limpios = c(nrow(actividades_limpio), nrow(departamentos_limpio),
                       nrow(depto_actividad_limpio), nrow(establecimientos_limpio)),
  Tasa_Retencion = round(c(nrow(actividades_limpio)/conteos_originales$actividades,
                          nrow(departamentos_limpio)/conteos_originales$departamentos,
                          nrow(depto_actividad_limpio)/conteos_originales$depto_actividad,
                          nrow(establecimientos_limpio)/conteos_originales$establecimientos) * 100, 2)
)

print(resumen_limpieza)

# Métricas de calidad del dataset integrado
metricas_calidad <- establecimientos_completo %>%
  summarise(
    total_establecimientos = n(),
    provincias = n_distinct(provincia),
    departamentos = n_distinct(in_departamentos),
    actividades_economicas = n_distinct(clae6),
    años = n_distinct(anio),
    proporcion_mujeres_promedio = round(mean(proporcion_mujeres, na.rm = TRUE), 3),
    exportadores = sum(es_exportador),
    tasa_exportadores = round(mean(es_exportador) * 100, 2)
  )

cat("\nMétricas de Calidad del Dataset Integrado:\n")
print(metricas_calidad)

# Guardar resúmenes
write_csv(resumen_limpieza, file.path(out_stats, "resumen_limpieza.csv"))
write_csv(metricas_calidad, file.path(out_stats, "metricas_calidad.csv"))

cat("\n=== LIMPIEZA DE DATOS COMPLETADA ===\n")
cat("Datasets listos para análisis exploratorio\n")
cat("Siguiente paso: Ejecutar 03_eda.R\n")