# TablaReactable helpers ----

#' Operador interno para fallback de valores NULL
#' @keywords internal
`%||%` <- function(x, y) if (!is.null(x)) x else y

#' Convierte un vector R en un arreglo JSON string para JS
#' @keywords internal
.to_json_arr <- function(v) {
  if (is.null(v) || length(v) == 0) return("[]")
  paste0('["', paste(as.character(v), collapse = '","'), '"]')
}

#' Genera JS para click por fila
#' @keywords internal
.js_fila <- function(input_id, filas_bloqueadas_json = "[]") {
  paste0(
    "function(rowInfo) {",
    "  var bloq = ", filas_bloqueadas_json, ";",
    "  if (bloq.length > 0) {",
    "    var vals = Object.values(rowInfo.values).map(function(v) {",
    "      return String(v).trim().toUpperCase();",
    "    });",
    "    var bup = bloq.map(function(b) { return String(b).trim().toUpperCase(); });",
    "    if (vals.some(function(v) { return bup.indexOf(v) >= 0; })) return;",
    "  }",
    "  Shiny.setInputValue('", input_id, "', {",
    "    row_index : rowInfo.index,",
    "    values    : rowInfo.values,",
    "    nonce     : Math.random()",
    "  }, {priority: 'event'});",
    "}"
  )
}

#' Genera JS para click por celda
#' @keywords internal
.js_celda <- function(input_id,
                      cols_activos_json = "[]",
                      filas_bloqueadas_json = "[]") {
  paste0(
    "function(rowInfo, colInfo) {",
    "  var cols = ", cols_activos_json, ";",
    "  if (cols.length > 0 && cols.indexOf(colInfo.id) < 0) return;",
    "  var bloq = ", filas_bloqueadas_json, ";",
    "  if (bloq.length > 0) {",
    "    var vals = Object.values(rowInfo.values).map(function(v) {",
    "      return String(v).trim().toUpperCase();",
    "    });",
    "    var bup = bloq.map(function(b) { return String(b).trim().toUpperCase(); });",
    "    if (vals.some(function(v) { return bup.indexOf(v) >= 0; })) return;",
    "  }",
    "  Shiny.setInputValue('", input_id, "', {",
    "    row_index : rowInfo.index,",
    "    values    : rowInfo.values,",
    "    col       : colInfo.id,",
    "    valor     : rowInfo.values[colInfo.id],",
    "    nonce     : Math.random()",
    "  }, {priority: 'event'});",
    "}"
  )
}

#' Genera JS para click por columna
#' @keywords internal
.js_columna <- function(input_id, cols_activos_json = "[]") {
  paste0(
    "function(rowInfo, colInfo) {",
    "  var cols = ", cols_activos_json, ";",
    "  if (cols.length > 0 && cols.indexOf(colInfo.id) < 0) return;",
    "  Shiny.setInputValue('", input_id, "', {",
    "    col   : colInfo.id,",
    "    nonce : Math.random()",
    "  }, {priority: 'event'});",
    "}"
  )
}

#' Genera nota de interaccion automatica
#' @keywords internal
.nota_interaccion <- function(modo_seleccion, sortable) {
  txt_modo <- switch(
    modo_seleccion,
    fila = "ℹ️  Clic en una fila para abrir su detalle.",
    celda = paste0(
      "ℹ️  Clic en una celda para ver su detalle;",
      " el encabezado identifica la columna seleccionada."
    ),
    columna = "ℹ️  Clic en cualquier celda de una columna para explorar sus valores.",
    ninguno = NULL
  )

  txt_orden <- if (sortable) {
    "⇅  Clic en el encabezado de una columna para ordenar la tabla."
  } else {
    NULL
  }

  if (is.null(txt_modo) && is.null(txt_orden)) return(NULL)

  lineas <- Filter(Negate(is.null), list(txt_modo, txt_orden))

  shiny::tags$div(
    class = "caja-modal-footer",
    lapply(lineas, function(l) shiny::tags$div(l))
  )
}

#' Construye bloque footer opcional
#' @keywords internal
.footer_bloque <- function(footer, footer_tipo) {
  if (is.null(footer)) return(NULL)

  css_class <- switch(
    footer_tipo,
    info = "caja-modal-footer",
    warning = "caja-modal-footer-warning",
    dark = "caja-modal-footer-dark",
    danger = "caja-modal-footer-danger",
    "caja-modal-footer"
  )

  if (inherits(footer, "shiny.tag") || inherits(footer, "shiny.tag.list")) {
    shiny::tags$div(class = css_class, footer)
  } else {
    shiny::tags$div(class = css_class, as.character(footer))
  }
}

#' @keywords internal
.FORMATOS_RACAFE <- c(
  "coma", "numero", "dinero", "dolares", "miles",
  "porcentaje", "cientifico", "millones", "entero",
  "tiempo", "kwh", "log"
)

#' Resuelve formateador de celda para reactable::colDef
#' @keywords internal
.resolver_fmt_celda <- function(formato) {
  formato <- tolower(trimws(formato))

  if (formato == "fecha") {
    return(function(v) {
      if (is.na(v)) return("—")
      tryCatch(format(as.Date(v), "%d/%m/%Y"), error = function(e) as.character(v))
    })
  }

  if (!formato %in% .FORMATOS_RACAFE) {
    stop(paste0(
      "TablaReactable col_specs: formato '", formato, "' no reconocido. ",
      "Use uno de: ", paste(.FORMATOS_RACAFE, collapse = ", "), " o 'fecha'."
    ))
  }

  formateador <- racafe::DefinirFormato(formato)

  function(v) {
    if (is.na(v)) return("—")
    formateador(v)
  }
}

#' Construye colDefs por defecto con overrides parciales/completos
#' @keywords internal
.coldefs_default <- function(
    data,
    columnas_override,
    col_specs,
    id_col,
    col_header_n,
    sortable_flag
) {
  nms <- names(data)
  col_header_idx <- seq_len(col_header_n)
  col_specs <- col_specs %||% list()

  defaults <- stats::setNames(
    lapply(seq_along(nms), function(i) {
      nm <- nms[[i]]
      specs <- col_specs[[nm]] %||% list()

      if (!is.null(id_col) && nm == id_col) {
        return(reactable::colDef(show = FALSE))
      }

      extra_class <- if (i %in% col_header_idx) "rt-col-header" else NULL
      label <- specs$label %||% nm
      formato <- specs$formato %||% "numero"
      min_width <- specs$min_width %||% NULL
      max_width <- specs$max_width %||% NULL
      color_fn <- specs$color_fn
      alinear <- specs$alinear %||% NULL

      if (is.numeric(data[[nm]])) {
        fmt_fn <- .resolver_fmt_celda(formato)
        reactable::colDef(
          name = label,
          class = extra_class,
          sortable = sortable_flag,
          minWidth = min_width,
          maxWidth = max_width,
          align = alinear,
          cell = fmt_fn,
          style = color_fn
        )
      } else if (inherits(data[[nm]], c("Date", "POSIXct", "POSIXlt"))) {
        fmt_fn <- .resolver_fmt_celda(specs$formato %||% "fecha")
        reactable::colDef(
          name = label,
          class = extra_class,
          sortable = sortable_flag,
          minWidth = min_width,
          maxWidth = max_width,
          cell = fmt_fn,
          style = color_fn
        )
      } else {
        reactable::colDef(
          name = label,
          class = extra_class,
          sortable = sortable_flag,
          minWidth = min_width,
          maxWidth = max_width,
          align = alinear,
          style = color_fn
        )
      }
    }),
    nms
  )

  if (!is.null(columnas_override)) {
    for (nm in names(columnas_override)) {
      defaults[[nm]] <- columnas_override[[nm]]
    }
  }

  defaults
}

#' @keywords internal
.js_row_class <- function() {
  reactable::JS("function(rowInfo) {
    var vals = Object.values(rowInfo.values).map(function(v) {
      return String(v).trim().toUpperCase();
    });
    if (vals.some(function(v) { return v === 'TOTAL'; })) {
      return 'rt-fila-total';
    }
    if (vals.some(function(v) { return v === 'OTROS'; })) {
      return 'rt-fila-otros';
    }
    return '';
  }")
}

#' @keywords internal
.normalizar_seleccion <- function(click, modo, df, id_col) {
  switch(
    modo,
    fila = {
      id_val <- if (!is.null(id_col)) click$values[[id_col]] else click$row_index
      fila_df <- if (!is.null(id_col)) {
        df[df[[id_col]] == id_val, , drop = FALSE]
      } else {
        df[click$row_index + 1L, , drop = FALSE]
      }
      list(modo = "fila", id = id_val, fila = fila_df)
    },
    celda = {
      id_val <- if (!is.null(id_col)) click$values[[id_col]] else click$row_index
      fila_df <- if (!is.null(id_col)) {
        df[df[[id_col]] == id_val, , drop = FALSE]
      } else {
        df[click$row_index + 1L, , drop = FALSE]
      }
      list(
        modo = "celda",
        id = id_val,
        col = click$col,
        valor = click$valor,
        fila = fila_df
      )
    },
    columna = {
      list(
        modo = "columna",
        col = click$col,
        data = df[, click$col, drop = FALSE]
      )
    }
  )
}

#' @keywords internal
.modal_titulo_tag <- function(sel, modal_titulo_fn, modal_icon) {
  titulo_txt <- modal_titulo_fn(sel)
  if (!is.null(modal_icon) && nzchar(modal_icon)) {
    shiny::tagList(shiny::icon(modal_icon), " ", titulo_txt)
  } else {
    titulo_txt
  }
}

#' @keywords internal
.modal_footer_bloque <- function(modal_footer, modal_footer_tipo) {
  botones <- shiny::tagList(shiny::modalButton("Cerrar"))

  if (is.null(modal_footer)) return(botones)

  css <- switch(
    modal_footer_tipo,
    info = "caja-modal-footer",
    warning = "caja-modal-footer-warning",
    dark = "caja-modal-footer-dark",
    danger = "caja-modal-footer-danger",
    "caja-modal-footer"
  )

  shiny::tagList(
    shiny::tags$div(
      class = css,
      if (inherits(modal_footer, "shiny.tag") ||
          inherits(modal_footer, "shiny.tag.list")) {
        modal_footer
      } else {
        as.character(modal_footer)
      }
    ),
    botones
  )
}

#' UI del modulo TablaReactable
#'
#' @param id ID del modulo Shiny.
#' @param titulo Titulo principal. `NULL` para omitir.
#' @param subtitulo Subtitulo secundario. `NULL` para omitir.
#' @param footer Contenido del footer: `NULL` | string | `tagList`.
#' @param footer_tipo Estilo del footer: `"info"` | `"warning"` | `"dark"` | `"danger"`.
#' @param sortable Activar estilos de encabezado ordenable. Debe coincidir con el server.
#'
#' @return `shiny.tag` contenedor con `reactableOutput`, nota y badge.
#' @export
TablaReactableUI <- function(
    id,
    titulo = NULL,
    subtitulo = NULL,
    footer = NULL,
    footer_tipo = c("info", "warning", "dark", "danger"),
    sortable = TRUE
) {
  ns <- shiny::NS(id)
  footer_tipo <- match.arg(footer_tipo)

  clases_contenedor <- trimws(paste(
    "rt-contenedor reactable-wrap",
    if (sortable) "rt-sortable" else ""
  ))

  header_bloque <- if (!is.null(titulo) && nzchar(titulo)) {
    shiny::tagList(
      shiny::tags$div(class = "rt-titulo", titulo),
      if (!is.null(subtitulo) && nzchar(subtitulo)) {
        shiny::tags$div(class = "rt-subtitulo", subtitulo)
      }
    )
  }

  shiny::tags$div(
    class = clases_contenedor,
    header_bloque,
    reactable::reactableOutput(ns("tabla")),
    shiny::uiOutput(ns("nota_interaccion")),
    .footer_bloque(footer, footer_tipo),
    shiny::uiOutput(ns("badge_seleccion"))
  )
}

#' Server del modulo TablaReactable
#'
#' Tabla reactable con seleccion configurable por fila, celda o columna.
#' Retorna una seleccion normalizada y opcionalmente abre un modal integrado.
#'
#' @param id ID del modulo Shiny.
#' @param data `reactive()` que retorna un `data.frame`.
#' @param columnas `list()` de `reactable::colDef` para override completo selectivo.
#' @param col_specs Named list con `label`, `formato`, `min_width`, `max_width`,
#'   `color_fn`, `alinear` por columna.
#' @param modo_seleccion `"fila"` | `"celda"` | `"columna"` | `"ninguno"`.
#' @param id_col Columna identificadora primaria. `NULL` usa `row_index`.
#' @param col_header_n Numero de columnas iniciales con clase `.rt-col-header`.
#' @param sortable Habilita ordenamiento por encabezado.
#' @param searchable Habilita buscador global.
#' @param page_size Filas por pagina.
#' @param compact Modo compacto.
#' @param mostrar_badge Muestra resumen del ultimo click.
#' @param modal_titulo_fn Funcion `function(sel)` para titulo del modal.
#' @param modal_contenido_fn Funcion `function(sel)` para cuerpo del modal.
#' @param modal_pre_fn Hook previo sincronico antes de `showModal()`.
#' @param modal_size Tamano del modal (`"s"`, `"m"`, `"l"`, `"xl"`).
#' @param modal_icon Icono FontAwesome del titulo modal.
#' @param modal_footer Footer adicional del modal.
#' @param modal_footer_tipo Estilo del footer del modal.
#' @param cols_activos Vector de columnas habilitadas para seleccion en JS.
#' @param filas_bloqueadas Vector de valores bloqueados por fila en JS.
#' @param filas_seleccionables Restriccion en R (`vector` de IDs o funcion).
#'
#' @return Lista con `seleccion` (reactive), `modo` (string) y `limpiar` (function).
#' @export
TablaReactable <- function(
    id,
    data,
    columnas = NULL,
    col_specs = NULL,
    modo_seleccion = c("fila", "celda", "columna", "ninguno"),
    id_col = NULL,
    col_header_n = 1L,
    sortable = TRUE,
    searchable = TRUE,
    page_size = 15,
    compact = TRUE,
    mostrar_badge = FALSE,
    modal_titulo_fn = NULL,
    modal_contenido_fn = NULL,
    modal_pre_fn = NULL,
    modal_size = "xl",
    modal_icon = NULL,
    modal_footer = NULL,
    modal_footer_tipo = c("info", "warning", "dark", "danger"),
    cols_activos = NULL,
    filas_bloqueadas = NULL,
    filas_seleccionables = NULL
) {
  modo_seleccion <- match.arg(modo_seleccion)
  modal_footer_tipo <- match.arg(modal_footer_tipo)
  col_header_n <- max(1L, as.integer(col_header_n))

  cols_activos_json <- .to_json_arr(cols_activos)
  filas_bloqueadas_json <- .to_json_arr(filas_bloqueadas)
  usar_modal <- !is.null(modal_titulo_fn) && !is.null(modal_contenido_fn)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    seleccion_r <- shiny::reactiveVal(NULL)

    on_click <- switch(
      modo_seleccion,
      fila = reactable::JS(.js_fila(ns("click"), filas_bloqueadas_json = filas_bloqueadas_json)),
      celda = reactable::JS(.js_celda(
        ns("click"),
        cols_activos_json = cols_activos_json,
        filas_bloqueadas_json = filas_bloqueadas_json
      )),
      columna = reactable::JS(.js_columna(ns("click"), cols_activos_json = cols_activos_json)),
      ninguno = NULL
    )

    output$tabla <- reactable::renderReactable({
      shiny::req(data())
      df <- data()
      coldefs <- .coldefs_default(
        data = df,
        columnas_override = columnas,
        col_specs = col_specs,
        id_col = id_col,
        col_header_n = col_header_n,
        sortable_flag = sortable
      )
      reactable::reactable(
        data = df,
        columns = coldefs,
        onClick = on_click,
        rowClass = .js_row_class(),
        sortable = sortable,
        highlight = modo_seleccion != "ninguno",
        searchable = searchable,
        defaultPageSize = page_size,
        compact = compact,
        bordered = TRUE,
        striped = FALSE,
        language = reactable::reactableLang(
          searchPlaceholder = "Buscar...",
          noData = "Sin resultados",
          pageInfo = "{rowStart}–{rowEnd} de {rows} registros",
          pagePrevious = "Anterior",
          pageNext = "Siguiente"
        ),
        theme = reactable::reactableTheme(
          headerStyle = list(
            fontWeight = "600",
            fontSize = "12px",
            cursor = if (sortable) "pointer" else "default"
          ),
          cellStyle = list(
            fontSize = "12px",
            padding = "3px 6px",
            cursor = if (modo_seleccion != "ninguno") "pointer" else "default"
          )
        )
      )
    })

    output$nota_interaccion <- shiny::renderUI({
      .nota_interaccion(modo_seleccion, sortable)
    })

    shiny::observeEvent(input$click, {
      shiny::req(!is.null(input$click))
      sel <- .normalizar_seleccion(input$click, modo_seleccion, data(), id_col)

      if (!is.null(filas_seleccionables) && sel$modo %in% c("fila", "celda")) {
        permitida <- if (is.function(filas_seleccionables)) {
          isTRUE(filas_seleccionables(sel$fila))
        } else {
          isTRUE(as.character(sel$id) %in% as.character(filas_seleccionables))
        }
        if (!permitida) return()
      }

      seleccion_r(sel)

      if (usar_modal) {
        if (!is.null(modal_pre_fn)) modal_pre_fn(sel)
        shiny::showModal(shiny::modalDialog(
          title = .modal_titulo_tag(sel, modal_titulo_fn, modal_icon),
          size = modal_size,
          easyClose = TRUE,
          footer = .modal_footer_bloque(modal_footer, modal_footer_tipo),
          modal_contenido_fn(sel)
        ))
      }
    }, ignoreNULL = TRUE)

    output$badge_seleccion <- shiny::renderUI({
      shiny::req(mostrar_badge, !is.null(seleccion_r()))
      sel <- seleccion_r()
      txt <- switch(
        sel$modo,
        fila = paste0("Fila: ", sel$id),
        celda = paste0("Celda [", sel$col, "] = ", sel$valor, "  —  ID: ", sel$id),
        columna = paste0("Columna: ", sel$col)
      )
      shiny::tags$p(
        shiny::icon("mouse-pointer"),
        shiny::tags$span(txt, style = "font-size:11px; color:#555; margin-left:4px;"),
        style = "margin-top:6px; margin-bottom:0;"
      )
    })

    list(
      seleccion = shiny::reactive(seleccion_r()),
      modo = modo_seleccion,
      limpiar = function() seleccion_r(NULL)
    )
  })
}
