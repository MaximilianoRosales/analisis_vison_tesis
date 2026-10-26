# ¿Dónde se encuentra el Visón? Proximidad a centros ganaderos productivos como determinante de la presencia o ausencia de Visón Americano 

Trabajo final del curso **Visualización de datos medioambientales**, Magíster en Recursos Naturales, Pontificia Universidad Católica de Chile.

> ⚠️ **Los datos de este repositorio son simulados.** Todavía no se recogen datos de terreno de la tesis, así que se generó una base "tipo" con la estructura esperada. **No son resultados ecológicos reales**.

## 1. Contexto

Este proyecto se basa en mi tesis de magíster, que estudia la presencia/ausencia de visón americano (*Neovison vison*) en riberas y su relación con la distancia a centros productivos.

- **Área de estudio:** desde las proximidades del Lago Villarrica hasta la cordillera.
- **Comunas:** Villarrica, Pucón y Curarrehue.
- **Ríos:** Trancura, Liucura, Pucón y sus afluentes, más el Toltén aguas abajo del lago.
- **Métodos de detección:** huellas, fecas, cámaras trampa y avistamiento.
- **Coordenadas:** UTM huso 19S (EPSG:32719).

## 2. Pregunta de visualización

¿Cómo se distribuyen las detecciones de visón a lo largo de los ríos del área de estudio, y cómo varían con la distancia a centros productivos?

## 3. Estructura del repositorio

```
.
├── README.md
├── data/
│   ├── raw/          # datos originales (simulados).
│   └── processed/    # datos limpios, generados por los scripts
├── scripts/          # código R, numerado en el orden de ejecución
├── figures/          # gráficos generados por el código
└── docs/             # diccionario de datos y notas
```

| Carpeta | Contenido |
|---|---|
| `data/raw` | Base simulada de detecciones |
| `data/processed` | Datos validados y transformados (factores, unidades, versión espacial). |
| `scripts` | `01_simular_datos.R` → `02_limpieza.R` → `03_visualizacion.R` |
| `figures` | Figuras finales en PNG. |
| `docs` | Diccionario de variables y notas metodológicas. |


## 4. Datos

Cada fila es un sitio de muestreo en una ribera. Variables principales:

| Variable | Descripción |
|---|---|
| `id_sitio` | Identificador único del sitio |
| `rio` | Río donde está el sitio |
| `comuna` | Comuna |
| `este_utm`, `norte_utm` | Coordenadas UTM 19S (m) |
| `distancia_centro_productivo_m` | Distancia al centro productivo más cercano (m) |
| `metodo` | Método de detección |
| `fecha` | Fecha del registro |
| `presencia` | 1 = presencia de visón, 0 = ausencia |

El diccionario completo irá en `docs/diccionario_datos.md`.


## 5. Resultados

Las figuras se guardan en `figures/`. *(Agregar aquí 2 o 3 imágenes con una línea de interpretación cada una cuando estén listas.)*
