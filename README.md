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

[![Launch Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/orinconl/economia-compitacional/HEAD?urlpath=lab)
## Ejecutar en la nube (Binder: JupyterLab con Python y R)

[![Launch Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/orinconl/economia-compitacional/HEAD?urlpath=lab)

> Abre JupyterLab en tu navegador con entornos de **Python 3** y **R** (gracias a `r-irkernel`).

### Paso a paso (R)
1. Haz clic en el botón **Launch Binder**.
2. En el **Launcher** de JupyterLab elige **R** (R kernel) → *Notebook*.
3. En la primera celda, verifica y carga los datos:
   ```r
   stopifnot(file.exists("data/impact_dataset.csv"))
   panel <- read.csv("data/impact_dataset.csv")
   head(panel)
