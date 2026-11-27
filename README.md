# Respuesta ecohidrológica simulada del río Liucura

Proyecto final del curso Visualización de Datos Medioambientales, AGP3141.

**Autora:** Josefina Ovando León  
**Institución:** Pontificia Universidad Católica de Chile

## Objetivo

Comparar el caudal simulado mediante SWAT+ al utilizar tres productos de
cobertura del suelo —CONAF, MapBiomas y CLDynamic- y contrastarlo con
observaciones de la estación Río Liucura en Liucura, disponibles en CAMELS-CL.

El análisis incluye RMSE, NSE, PBIAS y KGE. Posteriormente se examina el
desempeño de CONAF para meses de caudal bajo, intermedio y alto. También se
describe la estacionalidad de sedimentos observados en el río Trancura y
nitrato observado en el río Pucón en Balseadero Quelhue.

## Organización

- `datos/`: archivos de entrada.
- `scripts/`: lectura de datos, cálculos y figuras.
- `output/`: tablas generadas.
- `figuras/`: gráficos exportados.
- `analisis.qmd`: informe reproducible.
- `index.qmd`: portada del sitio.
- `informe/`: documentación de datos y métodos.
- `docs/`: sitio web generado por Quarto.

## Reproducir el informe

1. Instalar R y Quarto.
2. Abrir `swat-liucura.Rproj` en RStudio.
3. Ejecutar `renv::restore()` en la consola de R.
4. Ejecutar `quarto render --to html` en la terminal.
5. Abrir `docs/index.html`.

Los scripts se ejecutan automáticamente al generar el informe.


## Fuentes

- [CAMELS-CL](https://www.cr2.cl/camels-cl/)
- [Dirección General de Aguas](https://dga.mop.gob.cl/)
- [Documentación de SWAT+](https://swatplus.gitbook.io/io-docs/)
