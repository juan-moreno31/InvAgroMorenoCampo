# Reformas agrarias y titulación de baldíos en Colombia (1901–2013)
## Código final: corte transversal municipal (CNA 2014) y cruce de censos 1960–2014

[![R](https://img.shields.io/badge/R-4.3%2B-blue.svg)](https://www.r-project.org/)

Juan Diego Moreno Sánchez (Universidad Externado de Colombia) · Jacobo Campo Robledo (Universidad Católica de Colombia)

Este repositorio contiene **únicamente los scripts finales** (R y Python), la bibliografía y el borrador del paper de dos productos:

1. **Paper municipal** — *Equalizing Volume, Fragile Legacy: Public Land Titling and Land Inequality in Colombia, 1901–2013*. Corte transversal de 2014 con efectos fijos de departamento sobre el Censo Nacional Agropecuario (CNA 2014), con errores de Conley y *wild cluster bootstrap*, regresión cuantílica y persistencia frente al Censo Agropecuario de 1960 (carpeta `modelo_final/`).
2. **Presentación del IX Congreso de Economía Colombiana** (Universidad de los Andes, 1 de octubre de 2026) — *Reformas agrarias y producto agropecuario en Colombia: un análisis cliométrico (1925–2012)*. Cruce municipal Censo Agropecuario de 1960 – CNA 2014: tabla micro, persistencia y placebo (carpeta `scripts/`).

> [!IMPORTANT]
> **Aviso de confidencialidad (Universidad de los Andes – CEDE).**
> En cumplimiento del acuerdo de confidencialidad (NDA) suscrito con el Centro de Estudios sobre Desarrollo Económico (CEDE) de la Universidad de los Andes, **queda prohibida la inclusión y subida de cualquier archivo con microdatos, registros o cifras de adjudicaciones de tierras**. Por eso el repositorio **no contiene bases de datos, tablas de resultados ni gráficas**: el `.gitignore` ignora todo por defecto y solo habilita archivos `.R` y `.py`, este README, `Paper_work/referencias.bib` y el borrador del paper (`Paper_work/PaperAgro_v5.qmd`, con su estilo APA y plantilla), cuyas cifras agregadas provienen de documentos ya publicados.
>
> En consecuencia, el código **no es reproducible para terceros** sin autorización formal y acceso institucional a las fuentes del CEDE. Se publica con fines de auditoría metodológica y revisión.

---

## Estructura

```text
AgroMorenoCampo/
├── README.md
├── .gitignore                          # Lista blanca: scripts, bibliografía y borrador del paper
├── Paper_work/
│   ├── PaperAgro_v5.qmd                # Borrador del paper (Quarto → Word)
│   ├── referencias.bib                 # Bibliografía del paper (BibTeX)
│   ├── apa.csl                         # Estilo de citas APA 7
│   └── plantilla_lup.docx              # Plantilla de Word (Land Use Policy)
│
├── modelo_final/                       # Paper municipal (CNA 2014): versión final
│   ├── 01_construir_base.R             # Base municipal: CNA 2014, adjudicaciones, Censo 1960, CNMH, URT
│   ├── 02_modelo_final.R               # MCO con EF de departamento; clúster, Conley y wild cluster bootstrap
│   ├── 03_mecanismo_vs_regimen.R       # Antigüedad de la titulación frente a régimen legal
│   ├── 04_cuantilica.R                 # Regresión cuantílica y percentiles del tamaño de UPA
│   └── 05_persistencia_1960.R          # Antigüedad posterior a 1960 y municipios de frontera
│
├── CensoAgropecuario1960/CensoDigital/
│   └── 01_merge_censo_1960.R           # Consolida la digitalización del Censo Agropecuario de 1960
│
└── scripts/
    ├── clean_urt_data.py               # Solicitudes de restitución (URT) por municipio
    ├── 02_cna_municipal/
    │   ├── 01_corte_transversal.R      # Extracción CNA 2014 y cruce con adjudicaciones
    │   ├── 02_panel_conflicto.R        # Panel municipal de conflicto y agregados de adjudicación
    │   └── 07_replica_tabla_micro.R    # Réplica de la tabla micro de la presentación
    └── 04_cruce_censos/
        ├── 01_merge_1960_2014.R        # Panel de cortes Censo 1960 – CNA 2014
        ├── 03_gini_hato_1960.R         # Concentración del hato ganadero en 1960
        ├── 03_tratamiento_periodos.R   # Adjudicación por periodo histórico
        ├── 08_replica_persistencia_placebo.R  # Persistencia y placebo
        ├── 09_muestras_mapa.R          # Muestras del mapa
        ├── 09_mapa_muestras.py         # Mapa de muestras
        └── plot_periodos.R             # Gráfica de periodos históricos
```

## Ejecución (entorno autorizado)

Todos los scripts se ejecutan con el directorio de trabajo en la raíz del proyecto y leen insumos locales que **no** forman parte del repositorio (`datos/`, `Bases-datos/`, `CensoNacionalAgro/`, `CensoAgropecuario1960/`).

```powershell
# 1. Construcción de insumos
Rscript CensoAgropecuario1960/CensoDigital/01_merge_censo_1960.R
python  scripts/clean_urt_data.py
Rscript scripts/02_cna_municipal/01_corte_transversal.R
Rscript scripts/02_cna_municipal/02_panel_conflicto.R
Rscript scripts/04_cruce_censos/01_merge_1960_2014.R
Rscript scripts/04_cruce_censos/03_gini_hato_1960.R
Rscript scripts/04_cruce_censos/03_tratamiento_periodos.R

# 2. Paper municipal (en orden)
Rscript modelo_final/01_construir_base.R
Rscript modelo_final/02_modelo_final.R
Rscript modelo_final/03_mecanismo_vs_regimen.R
Rscript modelo_final/04_cuantilica.R
Rscript modelo_final/05_persistencia_1960.R

# 3. Cruce municipal de la presentación
Rscript scripts/02_cna_municipal/07_replica_tabla_micro.R
Rscript scripts/04_cruce_censos/08_replica_persistencia_placebo.R
Rscript scripts/04_cruce_censos/plot_periodos.R
Rscript scripts/04_cruce_censos/09_muestras_mapa.R
python  scripts/04_cruce_censos/09_mapa_muestras.py
```

Paquetes de R: `data.table`, `stringr`, `readxl`, `fixest`, `quantreg`, `sandwich`, `lmtest`, `ggplot2`.
Python: `pandas`, `matplotlib`, `shapely`.

## Referencias metodológicas

* Cameron, A. C., Gelbach, J. B., & Miller, D. L. (2008). Bootstrap-based improvements for inference with clustered errors. *The Review of Economics and Statistics*, 90(3), 414–427.
* Conley, T. G. (1999). GMM estimation with cross sectional dependence. *Journal of Econometrics*, 92(1), 1–45.
* Koenker, R., & Bassett, G. (1978). Regression quantiles. *Econometrica*, 46(1), 33–50.
* MacKinnon, J. G., & Webb, M. D. (2017). Wild bootstrap inference for wildly different cluster sizes. *Journal of Applied Econometrics*, 32(2), 233–254.
* Villaveces Niño, J., & Sánchez, F. (2014). *Tendencias históricas y regionales de la adjudicación de baldíos en Colombia 1900–2012* (Documento CEDE 2014-20). Universidad de los Andes.

La bibliografía completa del paper está en `Paper_work/referencias.bib`.
