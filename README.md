# ¿Dónde se encuentra el Visón? Proximidad a centros ganaderos productivos como determinante de la presencia o ausencia de Visón Americano

Trabajo final del curso **Visualización de datos medioambientales** (profesora: Sara Acevedo), Magíster en Recursos Naturales, Pontificia Universidad Católica de Chile.

> ⚠️ **Los datos de este repositorio son simulados con ayuda de Claude (Opus 5.5 - Alto).** Todavía no se recogen datos de terreno de la tesis, así que se generó una base "tipo" con la estructura esperada. **No son resultados ecológicos reales**.

## 1. Contexto del estudio

Este proyecto se basa en mi tesis de magíster, que estudia la presencia/ausencia de visón americano (*Neovison vison*) en riberas y su relación con la distancia a centros productivos.

![](figures/iconos/neogale_vison.jpg){fig-align="center"}

- **Área de estudio:** desde las proximidades del Lago Villarrica hasta la cordillera.
- **Comunas:** Villarrica, Pucón y Curarrehue.
- **Ríos:** Trancura, Liucura, Pucón y sus afluentes, más el Toltén aguas abajo del lago.
- **Métodos de detección:** huellas, fecas, cámaras trampa y avistamiento.
- **Coordenadas:** UTM huso 19S (EPSG:32719).

## 2. Pregunta de investigación

¿Cómo se distribuyen las detecciones de visón a lo largo de los ríos del área de estudio, y cómo varían con la distancia a centros productivos?

## 3. Estructura del repositorio

```         
.
├── README.md
├── data/
│   ├── raw/                     # datos originales (simulados).
│   └── intermedio/    
│   └── clean/                   # datos limpios, generados por los scripts
│   └── simulation/
├── scripts/                     # código R, numerado en el orden de ejecución
│   └── 00_preparar_capas.r/
│   └── 01_simular_centros_productivos.r/
│   └── 02_simular_muestreo.r/
│   └── 03_limpiar_datos.r/
│   └── ejecutar_todo.r/
│   └── funciones/               # carpeta con funciones
├── figures/                     # gráficos generados por el código
└── docs/
    └── diccionario_datos.csv/.  # diccionario de datos y notas
```

## 4. Diccionario de los datos

## Ver el diccionario completo

```{r}
dicc <- read_csv("documentos/diccionario_datos.csv", show_col_types = FALSE)
kable(dicc[, c("variable", "categoria", "descripcion", "tipo", "unidad", "nivel")],
      col.names = c("Variable", "Categoría", "Descripción", "Tipo", "Unidad", "Nivel"))
```

También está disponible como archivo: [`documentos/diccionario_datos.csv`](https://github.com/MaximilianoRosales/analisis_vison_tesis/blob/main/documentos/diccionario_datos.csv).

# 

## 
