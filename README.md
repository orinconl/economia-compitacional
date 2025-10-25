# economia-compitacional

Repositorio de la asignatura **Economía Computacional** (Especialización en Econometría).
Incluye materiales en **R** y **Python**, datos de práctica y guías para ejecutar el contenido
tanto **localmente** como **en la nube** (Binder/JupyterLab).
## Estructura del repositorio

```text
.
├─ R/                 # Scripts en R para el análisis principal
├─ python/            # Notebooks y scripts en Python (analítica y soporte)
├─ data/              # Archivos de datos (no sensibles)
├─ docs/              # Material de apoyo
├─ .binder/           # Configuración para ejecutar en la nube
├─ README.md          # Guía de uso
└─ LICENSE            # Licencia


- **R/**: contendrá los scripts que el curso ejecutará en clase.  
- **python/**: alternativas y utilidades en Python para quien prefiera ese entorno.  
- **data/**: aquí van los archivos .csv y otros insumos que se cargarán desde los scripts.  
- **.binder/**: permite abrir el repo en la nube con R y Python, sin instalar nada.  
- **docs/**: material complementario para consulta.

> Nota: evita subir datos personales o archivos mayores a ~50 MB.

```


[![Launch Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/orinconl/economia-compitacional/HEAD?urlpath=lab)

1) Haz clic en **Launch Binder** para abrir **JupyterLab** en el navegador.  
2) En el **Launcher** elige **R → Notebook** o **Python 3 → Notebook**.  
3) En la primera celda, verifica y carga la data:

**R**
```r
stopifnot(file.exists("data/impact_dataset.csv"))
panel <- read.csv("data/impact_dataset.csv")
head(panel)

[Haz clic aquí para abrir el ejercicio en Binder](https://mybinder.org/v2/gh/orinconl/economia-compitacional/main?labpath=notebooks%2Fejercicio_finanzas.ipynb)

