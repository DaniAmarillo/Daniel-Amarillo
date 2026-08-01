#------------------------------------------------------------
# Medición de memoria y tiempos del buscador
#------------------------------------------------------------

# Carga los RDS una sola vez, informa dimensiones y memoria, mide preparación
# temporal y consultas repetidas, y verifica que consultar no altera archivos.

# Medir recursos sin modificar los objetos operativos.
medir_rendimiento_busqueda <- function(
    archivo_modelo,
    archivo_funciones,
    archivo_preparacion,
    ruta_sqlite,
    tabla_principal = "papers",
    consultas,
    repeticiones = 25L,
    n_resultados = 5L,
    limite_memoria_gb = 16
) {
  if (!file.exists(archivo_modelo)) {
    stop("No se encontró el modelo de búsqueda.")
  }

  if (!file.exists(archivo_funciones)) {
    stop("No se encontraron las funciones de búsqueda.")
  }

  if (!file.exists(archivo_preparacion)) {
    stop("No se encontró la función de preparación de índices.")
  }

  # Convertir y validar el número de repeticiones.
  repeticiones <- as.integer(repeticiones)

  if (repeticiones < 1L) {
    stop("El número de repeticiones debe ser positivo.")
  }

  if (!n_resultados %in% c(5L, 10L, 20L)) {
    stop("El número de resultados debe ser 5, 10 o 20.")
  }

  #----------------------------------------------------------
  # 1. Cargar una sola vez los objetos usados por las consultas
  #----------------------------------------------------------

  # Registrar huellas antes de consultar.
  md5_antes <- tools::md5sum(
    c(
      modelo = archivo_modelo,
      funciones = archivo_funciones
    )
  )

  gc()

  # Cronometrar la carga de los RDS.
  tiempo_carga <- system.time({
    # Recargar un objeto previamente serializado.
    modelo <- readRDS(archivo_modelo) # Cargar matrices, metadatos y parámetros
    # Recargar un objeto previamente serializado.
    funciones <- readRDS(archivo_funciones) # Cargar funciones TF-IDF y LSA
  })

  if (
    !is.environment(funciones) ||
      !all(
        c("buscar_tfidf", "buscar_lsa") %in%
          ls(funciones)
      )
  ) {
    stop("Las funciones cargadas no tienen la estructura esperada.")
  }

  #----------------------------------------------------------
  # 2. Dimensiones original y reducida
  #----------------------------------------------------------

  # Contar documentos de la representación original.
  documentos_tfidf <- as.integer(
    nrow(modelo$matriz_tfidf)
  ) # Número de artículos en la representación original

  # Contar documentos de la representación reducida.
  documentos_lsa <- as.integer(
    nrow(modelo$matriz_lsa)
  ) # Número de artículos en la representación reducida

  # Contar columnas TF-IDF.
  terminos_originales <- as.integer(
    ncol(modelo$matriz_tfidf)
  ) # Cantidad de términos de la matriz TF-IDF

  # Contar componentes LSA.
  componentes_lsa <- as.integer(
    ncol(modelo$matriz_lsa)
  ) # Cantidad de componentes conservados en LSA

  # Reunir dimensiones para validarlas.
  dimensiones_validas <- c(
    documentos_tfidf,
    documentos_lsa,
    terminos_originales,
    componentes_lsa
  )

  if (
    length(dimensiones_validas) != 4L ||
      any(is.na(dimensiones_validas)) ||
      any(dimensiones_validas <= 0L)
  ) {
    stop(
      "No fue posible identificar correctamente las dimensiones del modelo."
    )
  }

  # Construir la tabla comparativa de dimensiones.
  dimensiones_busqueda <- tibble::tribble(
    ~representacion, ~documentos, ~columnas, ~celdas,
    "TF-IDF",
    documentos_tfidf,
    terminos_originales,
    as.double(documentos_tfidf) *
      as.double(terminos_originales),
    "LSA",
    documentos_lsa,
    componentes_lsa,
    as.double(documentos_lsa) *
      as.double(componentes_lsa)
  ) # Crear exactamente una fila por estrategia

  if (
    nrow(dimensiones_busqueda) != 2L ||
      any(
        dimensiones_busqueda$representacion !=
          c("TF-IDF", "LSA")
      )
  ) {
    stop(
      "La tabla de dimensiones no quedó construida correctamente."
    )
  }

  # Calcular reducción de columnas y celdas.
  reduccion_dimension <- tibble::tibble(
    indicador = c(
      "Documentos representados",
      "Términos de la dimensión original",
      "Componentes de la dimensión reducida",
      "Reducción de columnas (%)",
      "Reducción de celdas (%)"
    ),
    valor = c(
      documentos_tfidf,
      terminos_originales,
      componentes_lsa,
      100 * (
        1 -
          componentes_lsa /
          terminos_originales
      ),
      100 * (
        1 -
          (
            nrow(modelo$matriz_lsa) *
              componentes_lsa
          ) /
          (
            documentos_tfidf *
              terminos_originales
          )
      )
    )
  )

  #----------------------------------------------------------
  # 3. Tamaño de objetos con object.size
  #----------------------------------------------------------

  # Reunir objetos medidos con object.size().
  objetos_memoria <- list(
    "Matriz TF-IDF dispersa" =
      modelo$dtm_tfidf,
    "Matriz TF-IDF densa" =
      modelo$matriz_tfidf,
    "Matriz LSA reducida" =
      modelo$matriz_lsa,
    "Vectores de términos LSA" =
      modelo$svd$vectores_terminos,
    "Modelo completo cargado" =
      modelo,
    "Funciones cargadas" =
      funciones
  )

  # Convertir tamaños a bytes, MB y GB.
  memoria_objetos <- tibble::tibble(
    objeto = names(objetos_memoria),
    bytes = vapply(
      objetos_memoria,
      function(objeto) {
        as.numeric(
          # Medir memoria sin crear una copia adicional.
          utils::object.size(objeto)
        )
      },
      numeric(1)
    )
  ) |>
    dplyr::mutate(
      megabytes = bytes / 1024^2,
      gigabytes = bytes / 1024^3
    )

  # Medir la matriz TF-IDF densa.
  bytes_tfidf_densa <- as.numeric(
    # Medir memoria sin crear una copia adicional.
    utils::object.size(
      modelo$matriz_tfidf
    )
  )

  # Medir la matriz LSA.
  bytes_lsa <- as.numeric(
    # Medir memoria sin crear una copia adicional.
    utils::object.size(
      modelo$matriz_lsa
    )
  )

  # Sumar memoria del modelo y las funciones.
  memoria_cargada_bytes <-
    as.numeric(
      # Medir memoria sin crear una copia adicional.
      utils::object.size(modelo)
    ) +
    as.numeric(
      # Medir memoria sin crear una copia adicional.
      utils::object.size(funciones)
    )

  # Comparar memoria y límite de 16 GB.
  resumen_memoria <- tibble::tibble(
    indicador = c(
      "Memoria matriz TF-IDF densa (MB)",
      "Memoria matriz LSA (MB)",
      "Reducción de memoria entre matrices (%)",
      "Memoria total de objetos cargados (GB)",
      "Límite establecido (GB)",
      "Porcentaje del límite utilizado",
      "Cumple el límite de memoria"
    ),
    valor = c(
      bytes_tfidf_densa / 1024^2,
      bytes_lsa / 1024^2,
      100 * (
        1 -
          bytes_lsa /
          bytes_tfidf_densa
      ),
      memoria_cargada_bytes / 1024^3,
      limite_memoria_gb,
      100 * (
        memoria_cargada_bytes /
          (
            limite_memoria_gb *
              1024^3
          )
      ),
      memoria_cargada_bytes <
        limite_memoria_gb *
          1024^3
    )
  )

  #----------------------------------------------------------
  # 4. Tiempo de preparación completa
  #----------------------------------------------------------

  # Aislar la función usada para medir preparación.
  entorno_preparacion <- new.env(
    parent = globalenv()
  )

  # Cargar el script auxiliar requerido.
  sys.source(
    archivo_preparacion,
    envir = entorno_preparacion
  )

  if (
    !exists(
      "preparar_indices_busqueda",
      envir = entorno_preparacion,
      mode = "function"
    )
  ) {
    stop("No fue posible cargar la función de preparación.")
  }

  # Guardar la preparación en un archivo temporal.
  archivo_modelo_temporal <- tempfile(
    fileext = ".rds"
  )

  # Guardar funciones en un archivo temporal.
  archivo_funciones_temporal <- tempfile(
    fileext = ".rds"
  )

  # Garantizar la limpieza o cierre al terminar la función.
  on.exit(
    unlink(
      c(
        archivo_modelo_temporal,
        archivo_funciones_temporal
      )
    ),
    add = TRUE
  )

  gc()

  # Cronometrar una reconstrucción temporal completa.
  tiempo_preparacion <- system.time({
    entorno_preparacion$
      preparar_indices_busqueda(
        ruta_sqlite = ruta_sqlite,
        tabla_principal = tabla_principal,
        archivo_modelo =
          archivo_modelo_temporal,
        archivo_funciones =
          archivo_funciones_temporal,
        k_lsa =
          modelo$svd$k_final
      )
  })

  #----------------------------------------------------------
  # 5. Tiempo de varias consultas con objetos ya cargados
  #----------------------------------------------------------

  # Normalizar la tabla de consultas.
  consultas <- tibble::as_tibble(
    consultas
  )

  if (
    !"consulta" %in% names(consultas)
  ) {
    stop("La tabla de consultas debe contener la columna consulta.")
  }

  if (
    !"consulta_id" %in% names(consultas)
  ) {
    # Normalizar la tabla de consultas.
    consultas <- consultas |>
      dplyr::mutate(
        consulta_id = paste0(
          "Q",
          dplyr::row_number()
        ),
        .before = 1
      )
  }

  # Cronometrar una estrategia durante varias repeticiones.
  medir_consulta <- function(
      consulta_id,
      consulta,
      estrategia
  ) {
    # Seleccionar buscar_tfidf o buscar_lsa.
    funcion <- if (
      estrategia == "TF-IDF"
    ) {
      funciones$buscar_tfidf
    } else {
      funciones$buscar_lsa
    }

    # Guardar el mensaje cuando una consulta falla.
    error_consulta <- NA_character_
    # Controlar la validez de todas las repeticiones.
    resultados_validos <- TRUE

    gc()

    # Cronometrar el bloque de consultas.
    tiempo <- system.time({
      # Recorrer de forma reproducible los elementos definidos.
      for (
        repeticion in
          seq_len(repeticiones)
      ) {
        # Guardar el ranking de cada repetición.
        resultado <- tryCatch(
          funcion(
            consulta = consulta,
            n_resultados =
              n_resultados,
            modelo = modelo
          ),
          error = function(e) e
        )

        if (
          inherits(resultado, "error")
        ) {
          # Guardar el mensaje cuando una consulta falla.
          error_consulta <-
            conditionMessage(resultado)
          # Controlar la validez de todas las repeticiones.
          resultados_validos <- FALSE
          break
        }

        if (
          nrow(resultado) !=
            n_resultados
        ) {
          # Controlar la validez de todas las repeticiones.
          resultados_validos <- FALSE
          # Guardar el mensaje cuando una consulta falla.
          error_consulta <- paste(
            "La consulta no devolvió",
            n_resultados,
            "resultados."
          )
          break
        }
      }
    })

    # Registrar cuántas repeticiones terminaron.
    repeticiones_realizadas <- if (
      resultados_validos
    ) {
      repeticiones
    } else {
      max(
        repeticion,
        1L
      )
    }

    tibble::tibble(
      consulta_id = consulta_id,
      consulta = consulta,
      estrategia = estrategia,
      repeticiones =
        repeticiones_realizadas,
      tiempo_total_segundos =
        unname(tiempo["elapsed"]),
      tiempo_promedio_ms =
        unname(tiempo["elapsed"]) /
        repeticiones_realizadas *
        1000,
      resultados_validos =
        resultados_validos,
      error = error_consulta
    )
  }

  # Consolidar tiempos por consulta y estrategia.
  tiempos_consultas <- purrr::map_dfr(
    seq_len(nrow(consultas)),
    function(i) {
      dplyr::bind_rows(
        medir_consulta(
          consultas$consulta_id[i],
          consultas$consulta[i],
          "TF-IDF"
        ),
        medir_consulta(
          consultas$consulta_id[i],
          consultas$consulta[i],
          "LSA"
        )
      )
    }
  )

  # Resumir promedio, mediana y máximo.
  resumen_tiempos_estrategia <-
    tiempos_consultas |>
    dplyr::group_by(estrategia) |>
    dplyr::summarise(
      consultas_medidas = dplyr::n(),
      tiempo_promedio_ms = mean(
        tiempo_promedio_ms,
        na.rm = TRUE
      ),
      tiempo_mediano_ms = stats::median(
        tiempo_promedio_ms,
        na.rm = TRUE
      ),
      tiempo_maximo_ms = max(
        tiempo_promedio_ms,
        na.rm = TRUE
      ),
      todas_validas = all(
        resultados_validos
      ),
      .groups = "drop"
    )

  #----------------------------------------------------------
  # 6. Verificar que la consulta normal no reconstruye objetos
  #----------------------------------------------------------

  # Registrar huellas después de consultar.
  md5_despues <- tools::md5sum(
    c(
      modelo = archivo_modelo,
      funciones = archivo_funciones
    )
  )

  # Demostrar que una consulta no reconstruye.
  auditoria_busqueda_cargada <-
    tibble::tibble(
      indicador = c(
        "Modelo cargado una sola vez",
        "Funciones cargadas una sola vez",
        "Consultas TF-IDF válidas",
        "Consultas LSA válidas",
        "Archivos sin cambios durante las consultas",
        "Preparación ejecutada dentro de una búsqueda normal"
      ),
      valor = c(
        !is.null(modelo),
        is.environment(funciones),
        all(
          tiempos_consultas$
            resultados_validos[
              tiempos_consultas$
                estrategia ==
                "TF-IDF"
            ]
        ),
        all(
          tiempos_consultas$
            resultados_validos[
              tiempos_consultas$
                estrategia ==
                "LSA"
            ]
        ),
        identical(
          unname(md5_antes),
          unname(md5_despues)
        ),
        FALSE
      )
    )

  # Comparar carga inicial y preparación completa.
  tiempos_operaciones <- tibble::tibble(
    operacion = c(
      "Carga inicial de los RDS",
      "Preparación completa de TF-IDF y LSA"
    ),
    tiempo_segundos = c(
      unname(
        tiempo_carga["elapsed"]
      ),
      unname(
        tiempo_preparacion["elapsed"]
      )
    )
  )

  list(
    fecha_medicion = Sys.time(),
    parametros = list(
      repeticiones = repeticiones,
      n_resultados = n_resultados,
      limite_memoria_gb =
        limite_memoria_gb
    ),
    dimensiones_busqueda =
      dimensiones_busqueda,
    reduccion_dimension =
      reduccion_dimension,
    memoria_objetos =
      memoria_objetos,
    resumen_memoria =
      resumen_memoria,
    tiempos_operaciones =
      tiempos_operaciones,
    tiempos_consultas =
      tiempos_consultas,
    resumen_tiempos_estrategia =
      resumen_tiempos_estrategia,
    auditoria_busqueda_cargada =
      auditoria_busqueda_cargada
  )
}
