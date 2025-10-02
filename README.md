# economia-compitacional
Material de Economía Computacional (Especialización en Econometría). Scripts en R, Python y otros; prácticas reproducibles.

## Cómo ejecutar

En R o RStudio, ubicándote en la **raíz** del repositorio, corre:

```r
source("R/analisis_impacto.R", echo = TRUE)

## Requisitos

- **R ≥ 4.2**  
- *(Opcional)* **RStudio**  
- **Paquetes usados**: dplyr, tidyr, ggplot2, knitr, broom, AER, lmtest, sandwich, MatchIt, rdrobust, rddensity.  
  > Los scripts intentan **instalar automáticamente** cualquier paquete faltante desde CRAN.

## Datos

- Carpeta esperada: `data/`
- Archivos necesarios:
  - `impact_dataset.csv` — base principal.
  - `impact_dataset_dictionary.csv` — diccionario de variables.
- Cómo obtenerlos:
  - Descarga los archivos desde el enlace que comparta el docente y colócalos en `data/` **sin cambiar los nombres**.
- Buenas prácticas:
  - No subir datos sensibles ni archivos > 50 MB al repositorio.
- Comprobación rápida en R:
  ```r
  stopifnot(file.exists("data/impact_dataset.csv"))
  read.csv("data/impact_dataset.csv", nrows = 3)
