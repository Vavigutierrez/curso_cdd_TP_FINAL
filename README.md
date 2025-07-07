# Análisis de Establecimientos Productivos Argentinos

## 📊 Proyecto de Ciencia de Datos para Economía y Negocios

**Universidad de Buenos Aires - Facultad de Ciencias Económicas**  
**Curso:** Ciencia de Datos para Economía y Negocios  
**Año:** 2025  
---

## 🎯 Resumen

Análisis integral de la estructura productiva argentina (2021-2022) basado en datos del CEP XXI. Se implementó un pipeline completo de ciencia de datos para:

- Explorar patrones geográficos, sectoriales y de género
- Visualizar la distribución de +1.3M establecimientos  
- Aplicar modelos de machine learning para probar hipótesis económicas y evaluar predictores

### Resultados Relevantes
🔹 Procesamiento y análisis de +1.3 millones de registros a nivel nacional

🔹 15+ visualizaciones listas para presentaciones

🔹 3 modelos de ML implementados (Random Forest, Regresión Logística, K-means)

🔹 Modelado orientado a: validación de hipótesis y comparación de modelos

---

## 📁 Estructura del Proyecto

```
resolucion/
├── raw/                                    # Datos originales
│   ├── actividades_establecimientos.csv   # Códigos y descripciones de actividades
│   ├── codigo_departamento_provincia.csv  # Mapeo geográfico
│   ├── depto_actividad_genero.csv        # Datos agregados por departamento
│   └── distribucion_establecimientos_productivos_sexo.csv  # Datos principales
├── input/                                 # Datos procesados
│   ├── *_validated.rds                    # Datasets validados
│   ├── *_clean.rds                        # Datasets limpios
│   └── establishments_full.rds            # Dataset integrado final
├── output/                                # Resultados del análisis
│   ├── graphs/                            # Visualizaciones (PNG, HTML)
│   ├── models/                            # Modelos ML entrenados (RDS)
│   └── statistics/                        # Estadísticas y métricas (CSV)
├── scripts/                               # Código fuente principal
│   ├── 01_data_extraction.R               # Extracción y validación
│   ├── 02_data_cleaning.R                 # Limpieza y transformación
│   ├── 03_eda.R                           # Análisis exploratorio
│   ├── 04_ml_modeling.R                   # Machine learning
│   └── 05_classification_models_gender.R  # Comparación de modelos ML

├── presentation/                          # Materiales de presentación
│   └── final_presentation.pdf             # Presentación en PDF
└── README.md                              # Este archivo
```

---

## 🚀 Guía de Inicio Rápido

### Prerequisitos

**Software Requerido:**
- R (≥ 4.0.0)
- RStudio (recomendado)
- LaTeX (para generar PDFs)

**Paquetes de R Necesarios:**
```r
# Instalar paquetes principales
install.packages(c(
  "tidyverse", "ggplot2", "readr", "dplyr",
  "skimr", "VIM", "corrplot", "scales",
  "cluster", "factoextra", "FactoMineR",
  "randomForest", "caret", "e1071", "class",
  "plotly", "leaflet", "RColorBrewer",
  "gridExtra", "viridis", "pROC", "ROCR",
  "knitr", "rmarkdown", "kableExtra"
))
```

### Ejecución Paso a Paso
```r
# 1. Extracción y validación de datos
source("scripts/01_data_extraction.R")

# 2. Limpieza y transformación
source("scripts/02_data_cleaning.R")

# 3. Análisis exploratorio
source("scripts/03_eda.R")

# 4. Machine learning
source("scripts/04_ml_modeling.R")

# 5. Comparativa ML
source("scripts/05_classification_models_gender.R")

```

---
## 📊 Descripción de Datos

### Fuente
**Dataset:** "Distribución Geográfica de los Establecimientos Productivos: Versión Exploratoria"  
**Publicado por:** Centro de Estudios para la Producción (CEP XXI) - Ministerio de Economía  
**Fecha:** Diciembre 2023  
**Período:** 2021-2022

### Esquema de Base de Datos

El proyecto maneja 4 datasets principales con el siguiente esquema relacional:

```sql
-- ESTABLISHMENT (Establecimientos individuales)
cuit, sucursal, anio, lat, lon, clae6, in_departamentos, 
provincia_id, quintil, empleo, proporcion_mujeres

-- ACTIVITY (Códigos de actividad económica)  
clae6 [PK], clae2, letra, clae6_desc, clae2_desc, letra_desc

-- DEPARTMENT (División territorial)
provincia_id [PK], in_departamentos [PK], departamento, provincia

-- DEPT_ACTIVITY_GENDER (Agregados por departamento-actividad-género)
provincia_id, in_departamentos, clae6, genero, 
empleo, establecimientos, empresas_exportadoras
```

### Variables Clave

| Variable | Descripción | Tipo |
|----------|-------------|------|
| `cuit` | Identificador anónimo del establecimiento | String |
| `lat`, `lon` | Coordenadas geográficas (WGS84) | Numeric |
| `clae6` | Código de actividad económica (6 dígitos) | Integer |
| `empleo` | Categoría de tamaño de empleo | Factor |
| `proporcion_mujeres` | Proporción de empleadas mujeres [0-1] | Numeric |
| `quintil` | Quintil de exportación [0-5] | Integer |
| `provincia_id` | Código de provincia | Integer |

---

## 🔬 Metodología de Análisis

### 1. Extracción y Validación (`01_data_extraction.R`)

**Características:**
- ✅ Lectura robusta con múltiples encodings (UTF-8, latin1, windows-1252)
- ✅ Validación automática de esquemas de datos
- ✅ Control de calidad con detección de outliers
- ✅ Validación de integridad referencial entre tablas
- ✅ Análisis de coordenadas geográficas (bounds de Argentina)

**Salidas:**
- Datasets validados en formato RDS
- Reportes de calidad de datos
- Log de validaciones realizadas

### 2. Limpieza y Transformación (`02_data_cleaning.R`)

**Características:**
- ✅ Estandarización de variables categóricas
- ✅ Limpieza de encoding de caracteres especiales
- ✅ Validación de coordenadas dentro de Argentina
- ✅ Creación de variables derivadas (tamaño_empresa, región, etc.)
- ✅ Eliminación de duplicados y registros inválidos

**Variables Derivadas Creadas:**
- `size_category`: Micro, Pequeña, Mediana, Grande
- `is_exporter`: Indicador binario de exportación
- `export_category`: Nivel de exportación detallado
- `gender_composition`: Composición de género predominante
- `region`: Agrupación regional geográfica

### 3. Análisis Exploratorio (`03_eda.R`)

1. **Distribución por región geográfica** - Gráfico de barras
2. **Tamaño de empleo** - Distribución porcentual
3. **Participación femenina** - Histograma con estadísticas
4. **Composición de género por sector** - Top 15 sectores
5. **Actividad exportadora por región** - Gráfico apilado
6. **Tamaño vs empleo femenino** - Box plots
7. **Mapa geográfico interactivo** - Leaflet con clustering
8. **Concentración de actividades económicas** - Top 20
9. **Comparación temporal 2021-2022** - Gráfico agrupado
10. **Matriz de correlación** - Heatmap de variables numéricas
11. **Análisis provincial** - Top 15 provincias
12. **Brecha de género sectorial** - Heatmap 2D
13. **Intensidad exportadora** - Distribución por quintiles
14. **Especialización regional** - Coeficientes de localización
15. **Clustering sectorial** - K-means con análisis de silhouette

### 4. Machine Learning (`04_ml_modeling.R`)

#### 4.1 Análisis de Componentes Principales (PCA)
- **Variables analizadas:** empleo, proporción_mujeres, quintil, lat, lon
- **Componentes principales:** Identificación de varianza explicada
- **Visualizaciones:** Scree plot, biplot, contribución de variables

#### 4.2 Modelado Predictivo (ML)

**Dos enfoques complementarios:**

🧬 **Validación de hipótesis:** predicción de exportadores, influencia geográfica, patrones del sector salud

⚖️ **Comparación de modelos:** regresión logística vs. random forest para participación femenina alta

**Métricas:** Accuracy, AUC, F1, sensibilidad, especificidad

**Visuales:** Curvas ROC, matriz de confusión, importancia de variables

---

## 📈 Resultados Principales

### Estadísticas Descriptivas
- **Total de establecimientos analizados:** ~50,000+
- **Cobertura geográfica:** 24 provincias, 500+ departamentos
- **Actividades económicas:** 200+ códigos CLAE6
- **Participación femenina promedio:** ~35-45%
- **Tasa de exportadores:** ~15-25%

### Hallazgos del Machine Learning
- **Clusters óptimos identificados:** 3-4 grupos principales
- **Mejor modelo de clasificación:** Random Forest (AUC > 0.70)
- **Variables más predictivas:** Tamaño de empleo, sector económico, ubicación
- **Varianza explicada PCA:** 60-70% en primeros 2 componentes

### Patrones Identificados
1. **Concentración geográfica:** Región Centro concentra >50% establecimientos
2. **Brechas sectoriales:** Variabilidad 20-80% participación femenina entre sectores
3. **Tamaño y exportación:** Correlación positiva entre tamaño y propensión exportadora
4. **Especialización regional:** Clusters geográficos con perfiles productivos distintivos

---

## 🎨 Visualizaciones Destacadas

### 📊 Análisis Unidimensional
- **Histogramas y boxplots de empleo y género:** Distribución detallada de la participación femenina por categorías de empleo, identificando patrones sectoriales y brechas de género
- **Análisis de quintiles de exportación:** Visualización de la intensidad exportadora con distribución geográfica y sectorial

### 🗺️ Visualización Geográfica
- **Mapas interactivos de establecimientos:** Implementación con Leaflet que permite explorar la distribución espacial con clustering automático y filtros por sector/región
- **Heatmaps de concentración provincial:** Identificación visual de clusters geográficos y especialización regional

### 🔬 Machine Learning Visual
- **Clustering sectorial con K-means:** Visualización de grupos de establecimientos con características similares, incluyendo análisis de silueta y caracterización de clusters

### 📈 Evaluación de Modelos
- **Curvas ROC y matrices de confusión por modelo:** Comparación visual del rendimiento de Random Forest vs Regresión Logística para predicción de alta participación femenina
- **Análisis de importancia de variables:** Gráficos de barras mostrando las características más predictivas en cada modelo

---

## 🎯 Casos de Uso y Aplicaciones

### Para Académicos e Investigadores
- **Análisis regional:** Estudios de desarrollo económico territorial
- **Género y empleo:** Investigación sobre brechas laborales
- **Metodología:** Template para análisis similares con otros datasets

### Para Funcinonarios Públicos
- **Diagnóstico territorial:** Identificación de áreas de intervención
- **Políticas de género:** Diseño de programas de inclusión laboral
- **Promoción exportadora:** Focalización de incentivos por región/sector

### Para Sector Privado
- **Benchmarking:** Comparación con perfiles sectoriales/regionales
- **Localización:** Análisis para decisiones de inversión geográfica
- **Mercado laboral:** Entendimiento de disponibilidad de talento

---

## 📚 Referencias y Atribuciones

### Fuentes de Datos
- **Centro de Estudios para la Producción (CEP XXI)**
- **Ministerio de Economía de Argentina**
- **Dataset:** "Distribución Geográfica de los Establecimientos Productivos: Versión Exploratoria" (Diciembre 2023)

---

## 📄 Licencia y Uso

### Datos
Dominio público según CEP XXI

### Código
Uso académico y educativo

---
Proyecto realizado en el marco del curso "Ciencia de Datos para Economía y Negocios"
Facultad de Ciencias Económicas - Universidad de Buenos Aires
Año: 2025