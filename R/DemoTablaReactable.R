#' Demo autocontenida del modulo TablaReactable
#'
#' Lanza una aplicacion Shiny con bs4Dash que cubre todas las variaciones
#' del modulo: modos fila/celda/columna/ninguno, modal integrado (fila y celda),
#' showcase de `col_specs` con formatos racafe, modulos externos reactivos,
#' filas bloqueadas en JS, panel inline en el padre, restriccion de columnas
#' activas y restriccion de filas por predicado R.
#'
#' @details
#' Todos los datasets, helpers, modulos auxiliares, UI y server estan
#' encapsulados en el cuerpo de la funcion. Los modulos auxiliares
#' (`DetalleAsesor`, `ResumenSegmento`, `DetalleClienteModal`, `DetalleLeadModal`)
#' son versiones minimas orientadas a mostrar el patron de integracion,
#' no analisis de datos completos.
#'
#' @param ... Parametros reservados para extensiones futuras.
#'
#' @return Objeto `shiny.appobj` listo para ejecutar.
#' @references
#' * Shiny: <https://shiny.posit.co/>
#' * reactable: <https://glin.github.io/reactable/>
#' * bs4Dash: <https://rinterface.github.io/bs4Dash/>
#' @export
#'
#' @examples
#' \dontrun{
#'   DemoTablaReactable()
#' }
DemoTablaReactable <- function(...) {

  # DATOS ----
  set.seed(42)
  clientes_base <- tibble::tibble(
    cliente_id   = paste0("C", sprintf("%03d", 1:38)),
    razon_social = paste("Empresa", LETTERS[((1:38 - 1) %% 26) + 1],
                         sample(c("S.A.S", "Ltda", "S.A"), 38, replace = TRUE)),
    asesor    = sample(c("Ana Torres", "Luis Gomez", "Maria Rios",
                         "Carlos Vera", "Sofia Pena"), 38, replace = TRUE),
    segmento  = sample(c("A", "B", "C"), 38, replace = TRUE, prob = c(0.2, 0.5, 0.3)),
    estado    = sample(c("CLIENTE ACTIVO", "CLIENTE A RECUPERAR", "NUEVO DEL PERIODO"),
                       38, replace = TRUE, prob = c(0.55, 0.30, 0.15)),
    sacos     = round(runif(38, 500, 15000)),
    margen    = round(runif(38, 2e6, 80e6)),
    tasa_fact = round(runif(38, 20, 100), 1) / 100,
    fecha_ult = as.Date("2024-01-01") + sample(0:365, 38, replace = TRUE)
  )

  add_rows_otros_total <- function(df, key_col, otros_vals, total_vals) {
    dplyr::bind_rows(df, otros_vals, total_vals)
  }

  clientes <- add_rows_otros_total(
    clientes_base,
    "razon_social",
    tibble::tibble(cliente_id = "-", razon_social = "OTROS", asesor = "-",
                   segmento = "-", estado = "-", sacos = 4200L,
                   margen = 18500000L, tasa_fact = 0.613, fecha_ult = as.Date(NA)),
    tibble::tibble(cliente_id = "-", razon_social = "TOTAL", asesor = "-",
                   segmento = "-", estado = "-",
                   sacos     = sum(clientes_base$sacos) + 4200L,
                   margen    = sum(clientes_base$margen) + 18500000L,
                   tasa_fact = round(mean(c(clientes_base$tasa_fact, 0.613)), 3),
                   fecha_ult = as.Date(NA))
  )

  ventas_base <- tibble::tibble(
    mes = rep(format(seq.Date(as.Date("2024-01-01"), by = "month",
                              length.out = 11), "%b %Y"), each = 3),
    linea        = rep(c("Linea 1", "Linea 2", "Linea 3"), 11),
    sacos        = round(runif(33, 1000, 20000)),
    margen       = round(runif(33, 5e6, 100e6)),
    cumpl_sacos  = round(runif(33, 40, 120), 1) / 100,
    cumpl_margen = round(runif(33, 40, 120), 1) / 100
  )
  ventas_mes <- add_rows_otros_total(
    ventas_base,
    "mes",
    tibble::tibble(mes = "OTROS", linea = "-", sacos = 3100L, margen = 12000000L,
                   cumpl_sacos = 0.55, cumpl_margen = 0.48),
    tibble::tibble(mes = "TOTAL", linea = "-",
                   sacos        = sum(ventas_base$sacos) + 3100L,
                   margen       = sum(ventas_base$margen) + 12000000L,
                   cumpl_sacos  = round(mean(c(ventas_base$cumpl_sacos,  0.55)), 3),
                   cumpl_margen = round(mean(c(ventas_base$cumpl_margen, 0.48)), 3))
  )
  transiciones <- dplyr::bind_rows(
    tibble::tibble(
      estado_origen  = rep(c("ACTIVO", "A RECUPERAR", "NUEVO"), each = 3),
      estado_destino = rep(c("ACTIVO", "A RECUPERAR", "NUEVO"), 3),
      n_transiciones = c(120L, 18L, 0L, 25L, 45L, 0L, 0L, 0L, 22L),
      pct            = c(87.0, 13.0, 0.0, 35.7, 64.3, 0.0, 0.0, 0.0, 100.0) / 100),
    tibble::tibble(estado_origen = "OTROS", estado_destino = "-",
                   n_transiciones = 5L, pct = 0.021),
    tibble::tibble(estado_origen = "TOTAL", estado_destino = "-",
                   n_transiciones = 235L, pct = 1.0)
  )
  presupuesto_asesor <- tibble::tibble(
    asesor       = c("Ana Torres", "Luis Gomez", "Maria Rios",
                     "Carlos Vera", "Sofia Pena", "OTROS", "TOTAL"),
    clientes     = c(12L, 9L, 15L, 7L, 10L, 3L, 56L),
    sacos_real   = c(48200, 31500, 62000, 22000, 41000, 4200, 208900),
    sacos_ppto   = c(50000, 35000, 60000, 25000, 45000, 5000, 220000),
    margen_real  = c(980e6, 640e6, 1250e6, 445e6, 830e6, 85e6, 4230e6),
    margen_ppto  = c(1000e6, 700e6, 1200e6, 500e6, 900e6, 100e6, 4400e6),
    cumpl_sacos  = c(0.964, 0.900, 1.033, 0.880, 0.911, 0.840, 0.950),
    cumpl_margen = c(0.980, 0.914, 1.042, 0.890, 0.922, 0.850, 0.961),
    kwh_promedio = c(124.5, 98.2, 210.3, 77.8, 155.0, 45.0, 711.0),
    margen_mm    = c(980, 640, 1250, 445, 830, 85, 4230)
  )
  leads <- tibble::tibble(
    lead_id        = paste0("L", sprintf("%03d", 1:20)),
    razon_social   = paste("Prospecto", sample(LETTERS, 20)),
    asesor         = sample(c("Ana Torres", "Luis Gomez", "Maria Rios"),
                            20, replace = TRUE),
    estado_lead    = sample(c("NUEVO", "CONTACTADO", "PROPUESTA", "NEGOCIACION"),
                            20, replace = TRUE),
    sacos_op       = round(runif(20, 100, 5000)),
    margen_op      = round(runif(20, 500e3, 20e6)),
    fecha_creacion = as.Date("2024-06-01") + sample(0:180, 20, replace = TRUE),
    Editar         = "Editar",
    Contacto       = "Contactar"
  )

  # COLDEFS MANUALES ----
  coldefs_clientes <- list(
    estado = reactable::colDef(
      name = "Estado", minWidth = 160,
      style = function(v) {
        list(
          background = switch(v, "CLIENTE ACTIVO" = "#EFF6FF",
                              "CLIENTE A RECUPERAR" = "#FFF8EC",
                              "NUEVO DEL PERIODO" = "#EDFBF2", "white"),
          color      = switch(v, "CLIENTE ACTIVO" = "#1A5276",
                              "CLIENTE A RECUPERAR" = "#784212",
                              "NUEVO DEL PERIODO" = "#1E8449", "#333"),
          fontWeight = "600"
        )
      }
    ),
    tasa_fact = reactable::colDef(
      name  = "Tasa %",
      cell  = function(v) if (is.na(v)) "-" else paste0(round(v * 100, 1), "%"),
      style = function(v) {
        list(background = if (is.na(v)) "#F8F9FA" else if (v >= 0.75) "#D5F5E3"
             else if (v >= 0.50) "#FCF3CF" else "#FADBD8")
      }
    ),
    margen    = reactable::colDef(name = "Margen",
      cell = function(v) if (is.na(v)) "-" else paste0("$", format(round(v), big.mark = ","))),
    sacos     = reactable::colDef(name = "Sacos",
      cell = function(v) if (is.na(v)) "-" else format(round(v), big.mark = ",")),
    fecha_ult = reactable::colDef(name = "Ult. Factura",
      cell = function(v) if (is.na(v)) "-" else format(as.Date(v), "%d/%m/%Y"))
  )
  coldefs_ventas <- list(
    cumpl_sacos = reactable::colDef(
      name  = "% Sacos",
      cell  = function(v) if (is.na(v)) "-" else paste0(round(v * 100, 1), "%"),
      style = function(v) {
        list(background = if (is.na(v)) "#F8F9FA" else if (v >= 0.90) "#D5F5E3"
             else if (v >= 0.70) "#FCF3CF" else "#FADBD8")
      }
    ),
    cumpl_margen = reactable::colDef(
      name  = "% Margen",
      cell  = function(v) if (is.na(v)) "-" else paste0(round(v * 100, 1), "%"),
      style = function(v) {
        list(background = if (is.na(v)) "#F8F9FA" else if (v >= 0.90) "#D5F5E3"
             else if (v >= 0.70) "#FCF3CF" else "#FADBD8")
      }
    ),
    sacos  = reactable::colDef(
      cell = function(v) if (is.na(v)) "-" else format(round(v), big.mark = ",")),
    margen = reactable::colDef(
      cell = function(v) if (is.na(v)) "-" else paste0("$", format(round(v), big.mark = ",")))
  )

  # COL_SPECS REUTILIZABLES ----
  .sfn <- function(v, hi = 1.0, md = 0.85) {
    if (is.na(v)) return(NULL)
    list(background = if (v >= hi) "#D5F5E3" else if (v >= md) "#FCF3CF" else "#FADBD8",
         fontWeight = "600")
  }
  .tfn <- function(v) {
    if (is.na(v)) return(NULL)
    list(background = if (v >= 0.75) "#D5F5E3" else if (v >= 0.50) "#FCF3CF" else "#FADBD8")
  }
  specs_presupuesto <- list(
    asesor       = list(label = "Asesor",      min_width = 130),
    clientes     = list(label = "Clientes",    formato = "coma",       min_width = 80),
    sacos_real   = list(label = "Sacos Real",  formato = "coma",       min_width = 100),
    sacos_ppto   = list(label = "Sacos Ppto",  formato = "coma",       min_width = 100),
    margen_real  = list(label = "Margen Real", formato = "dinero",     min_width = 130),
    margen_ppto  = list(label = "Margen Ppto", formato = "dinero",     min_width = 130),
    cumpl_sacos  = list(label = "% Sacos",     formato = "porcentaje", min_width = 90,  color_fn = .sfn),
    cumpl_margen = list(label = "% Margen",    formato = "porcentaje", min_width = 90,  color_fn = .sfn),
    kwh_promedio = list(label = "kWh Prom",    formato = "kwh",        min_width = 100),
    margen_mm    = list(label = "Margen ($M)", formato = "millones",   min_width = 120)
  )
  specs_leads <- list(
    lead_id        = list(label = "ID",         min_width = 70),
    razon_social   = list(label = "Prospecto",  min_width = 160),
    asesor         = list(label = "Asesor",     min_width = 120),
    estado_lead    = list(label = "Estado",     min_width = 110),
    sacos_op       = list(label = "Sacos Op.",  formato = "coma",   min_width = 90),
    margen_op      = list(label = "Margen Op.", formato = "dinero", min_width = 120),
    fecha_creacion = list(label = "Creacion",   formato = "fecha",  min_width = 100),
    Editar         = list(label = "Editar",     min_width = 80, alinear = "center"),
    Contacto       = list(label = "Contacto",   min_width = 80, alinear = "center")
  )
  specs_clientes_ext <- list(
    cliente_id   = list(label = "ID",      min_width = 60),
    razon_social = list(label = "Cliente", min_width = 160),
    asesor       = list(label = "Asesor",  min_width = 120),
    segmento     = list(label = "Seg.",    min_width = 55,  alinear = "center"),
    estado       = list(label = "Estado",  min_width = 155),
    sacos        = list(label = "Sacos",   formato = "coma",       min_width = 90),
    margen       = list(label = "Margen",  formato = "dinero",     min_width = 120),
    tasa_fact    = list(label = "Tasa %",  formato = "porcentaje", min_width = 80, color_fn = .tfn)
  )

  # HELPERS UI ----
  .kpi_mini <- function(label, valor, color = "#2C7BB6") {
    shiny::tags$div(
      style = paste0("display:inline-flex; flex-direction:column; padding:8px 14px;",
                     "background:#F8F9FA; border-radius:6px; min-width:100px;"),
      shiny::tags$span(label, style = "font-size:10px; color:#888;"),
      shiny::tags$strong(valor, style = paste0("font-size:14px; color:", color, ";"))
    )
  }
  .panel_log <- function(output_id, titulo) {
    bs4Dash::bs4Card(
      title = shiny::tagList(shiny::icon("terminal"), " ", titulo),
      width = 12, status = "secondary", solidHeader = FALSE, collapsible = FALSE,
      style = "background:#F8F9FA; border:1px solid #DEE2E6;",
      shiny::uiOutput(output_id)
    )
  }
  .espera_seleccion <- function(msg = "Seleccione una fila para ver el detalle.") {
    shiny::tags$div(
      style = "padding:32px; text-align:center;",
      shiny::icon("hand-pointer",
                  style = "color:#BDC3C7; font-size:32px; display:block; margin-bottom:10px;"),
      shiny::tags$p(msg, style = "color:#999; font-size:13px; margin:0;")
    )
  }
  .render_log <- function(sel_r, output, out_id) {
    output[[out_id]] <- shiny::renderUI({
      sel <- sel_r()
      if (is.null(sel)) {
        return(shiny::p(shiny::icon("info-circle"), " Sin seleccion activa.",
                        style = "color:#999; font-size:12px; margin:8px 0 0;"))
      }
      bg  <- switch(sel$modo, fila = "#EFF6FF", celda = "#EDFBF2",
                    columna = "#FFF8EC", "#F8F9FA")
      col <- switch(sel$modo, fila = "#1A5276", celda = "#1E8449",
                    columna = "#784212", "#555")
      cuerpo <- switch(sel$modo,
        fila = shiny::tags$table(
          class = "table table-sm table-borderless",
          style = "font-size:12px; margin-bottom:0;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("Campo", style = "color:#555; width:140px;"),
            shiny::tags$th("Valor", style = "color:#555;"))),
          shiny::tags$tbody(lapply(names(sel$fila), function(nm) {
            shiny::tags$tr(shiny::tags$td(shiny::tags$strong(nm)),
                           shiny::tags$td(as.character(sel$fila[[nm]])))
          }))
        ),
        celda = shiny::tags$div(
          style = "display:flex; gap:16px; flex-wrap:wrap; font-size:12px;",
          shiny::tags$div(
            shiny::tags$span("Columna", style = "color:#888; display:block; font-size:10px;"),
            shiny::tags$strong(sel$col, style = "color:#2C7BB6;")),
          shiny::tags$div(
            shiny::tags$span("Valor", style = "color:#888; display:block; font-size:10px;"),
            shiny::tags$strong(as.character(sel$valor), style = "color:#1E8449;"))
        ),
        columna = shiny::tags$div(
          style = "font-size:12px;",
          shiny::tags$p(shiny::icon("columns"),
                        shiny::tags$strong(paste0(" Columna: ", sel$col)),
                        style = "margin-bottom:6px; color:#2C7BB6;"),
          shiny::tags$p(paste0("Filas: ", nrow(sel$data),
                               "  |  NAs: ", sum(is.na(sel$data[[sel$col]]))),
                        style = "color:#555; margin:0;")
        )
      )
      shiny::tagList(
        shiny::tags$p(
          shiny::tags$span(paste0("modo: ", toupper(sel$modo)),
                           style = paste0("font-size:10px; font-weight:600; padding:2px 8px;",
                                          "border-radius:3px; background:", bg,
                                          "; color:", col, "; margin-right:6px;")),
          style = "margin-bottom:8px;"),
        cuerpo
      )
    })
  }
  .plotly_std <- function(p, xtitle = "", ytitle = "", yformat = ",.0f",
                          xformat = NULL, hovermode = "closest") {
    xax <- list(title = xtitle)
    if (!is.null(xformat)) xax$tickformat <- xformat
    p |>
      plotly::layout(xaxis = xax,
                     yaxis = list(title = ytitle, tickformat = yformat),
                     plot_bgcolor = "white", paper_bgcolor = "white",
                     hovermode = hovermode,
                     legend = list(orientation = "h", y = -0.28)) |>
      plotly::config(displayModeBar = FALSE)
  }

  # MODALES ESTATICOS ----
  .modal_detalle_cliente <- function(sel) {
    f  <- sel$fila
    ec <- switch(as.character(f$estado[[1]]), "CLIENTE ACTIVO" = "#1A5276",
                 "CLIENTE A RECUPERAR" = "#784212", "NUEVO DEL PERIODO" = "#1E8449", "#333")
    eb <- switch(as.character(f$estado[[1]]), "CLIENTE ACTIVO" = "#EFF6FF",
                 "CLIENTE A RECUPERAR" = "#FFF8EC", "NUEVO DEL PERIODO" = "#EDFBF2", "#F8F9FA")
    shiny::tagList(
      shiny::tags$div(
        style = "padding:12px 0 14px; border-bottom:1px solid #E9ECEF; margin-bottom:16px;",
        shiny::tags$h5(as.character(f$razon_social[[1]]),
                       style = "margin:0 0 4px; font-weight:700;"),
        shiny::tags$span(paste0("ID: ", as.character(f$cliente_id[[1]])),
                         style = "font-size:11px; color:#888;")
      ),
      shiny::tags$div(
        style = "display:flex; flex-wrap:wrap; gap:10px; margin-bottom:16px;",
        .kpi_mini("Asesor",   as.character(f$asesor[[1]])),
        .kpi_mini("Segmento", as.character(f$segmento[[1]]), "#555"),
        .kpi_mini("Sacos",    format(as.integer(f$sacos[[1]]), big.mark = ",")),
        .kpi_mini("Margen",   paste0("$", format(round(as.numeric(f$margen[[1]])),
                                                 big.mark = ",")), "#1E8449"),
        .kpi_mini("Tasa %",   paste0(round(as.numeric(f$tasa_fact[[1]]) * 100, 1), "%"),
                  if (!is.na(f$tasa_fact[[1]]) && f$tasa_fact[[1]] >= 0.75)
                    "#1E8449" else "#943126")
      ),
      shiny::tags$div(
        style = paste0("display:inline-block; padding:4px 14px; border-radius:20px;",
                       "background:", eb, "; color:", ec, "; font-weight:600; font-size:12px;"),
        as.character(f$estado[[1]])
      )
    )
  }
  .modal_accion_lead <- function(sel) {
    f <- sel$fila
    header <- shiny::tags$div(
      style = "display:flex; flex-wrap:wrap; gap:10px; margin-bottom:14px;",
      .kpi_mini("Lead",      as.character(f$razon_social[[1]])),
      .kpi_mini("Estado",    as.character(f$estado_lead[[1]]), "#2C7BB6"),
      .kpi_mini("Sacos Op.", format(as.integer(f$sacos_op[[1]]), big.mark = ","))
    )
    cuerpo <- if (sel$col == "Editar") {
      shiny::tags$p(shiny::icon("edit"), " Formulario de edicion simulado.",
                    style = "font-size:12px; color:#888;")
    } else if (sel$col == "Contacto") {
      shiny::tags$p(shiny::icon("phone"), " Formulario de contacto simulado.",
                    style = "font-size:12px; color:#888;")
    } else {
      shiny::tags$p(shiny::icon("info-circle"),
                    paste0("Columna '", sel$col, "' sin accion asociada."),
                    style = "font-size:13px; color:#888;")
    }
    shiny::tagList(header, cuerpo)
  }

  # MODULO AUXILIAR: DetalleAsesor ----
  DetalleAsesorUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::uiOutput(ns("kpis")),
      shiny::fluidRow(
        shiny::column(6,
          bs4Dash::bs4Card(title = "Sacos real vs ppto", width = 12,
                           solidHeader = FALSE, status = "primary", collapsible = FALSE,
                           plotly::plotlyOutput(ns("plot_sacos"), height = "220px"))
        ),
        shiny::column(6,
          bs4Dash::bs4Card(title = "Clientes asignados", width = 12,
                           solidHeader = FALSE, status = "secondary", collapsible = FALSE,
                           TablaReactableUI(ns("sub_clientes"), sortable = TRUE))
        )
      )
    )
  }
  DetalleAsesor <- function(id, fila_r) {
    shiny::moduleServer(id, function(input, output, session) {
      clientes_r <- shiny::reactive({
        shiny::req(fila_r())
        nm <- as.character(fila_r()$asesor[[1]])
        if (nm %in% c("OTROS", "TOTAL", "-")) return(tibble::tibble())
        dplyr::filter(clientes_base, asesor == nm) |>
          dplyr::select(cliente_id, razon_social, segmento, estado, sacos, margen, tasa_fact)
      })
      output$kpis <- shiny::renderUI({
        shiny::req(fila_r())
        f <- fila_r()
        shiny::tags$div(
          style = "padding:8px 0 14px; display:flex; flex-wrap:wrap; gap:10px;",
          .kpi_mini("Asesor",     as.character(f$asesor[[1]])),
          .kpi_mini("Sacos Real", format(as.integer(f$sacos_real[[1]]), big.mark = ",")),
          .kpi_mini("% Sacos",    paste0(round(as.numeric(f$cumpl_sacos[[1]]) * 100, 1), "%"),
                    if (f$cumpl_sacos[[1]] >= 1) "#1E8449" else "#943126"),
          .kpi_mini("kWh Prom",   paste0(as.numeric(f$kwh_promedio[[1]]), " kWh"), "#555")
        )
      })
      output$plot_sacos <- plotly::renderPlotly({
        shiny::req(fila_r())
        f <- fila_r()
        p <- plotly::plot_ly(
          x = c("Real", "Presupuesto"),
          y = c(as.numeric(f$sacos_real[[1]]), as.numeric(f$sacos_ppto[[1]])),
          type = "bar", marker = list(color = c("#2C7BB6", "#AED6F1")),
          hovertemplate = "%{x}: <b>%{y:,.0f}</b><extra></extra>"
        )
        .plotly_std(p, ytitle = "Sacos") |> plotly::layout(bargap = 0.4)
      })
      TablaReactable(
        id = "sub_clientes", data = clientes_r,
        col_specs = list(
          cliente_id   = list(label = "ID",      min_width = 55),
          razon_social = list(label = "Cliente", min_width = 140),
          segmento     = list(label = "Seg.",    min_width = 50, alinear = "center"),
          estado       = list(label = "Estado",  min_width = 140),
          sacos        = list(label = "Sacos",   formato = "coma",       min_width = 80),
          margen       = list(label = "Margen",  formato = "dinero",     min_width = 110),
          tasa_fact    = list(label = "Tasa %",  formato = "porcentaje", min_width = 75,
                              color_fn = .tfn)
        ),
        modo_seleccion = "ninguno", col_header_n = 2L, sortable = TRUE, page_size = 5
      )
    })
  }

  # MODULO AUXILIAR: ResumenSegmento ----
  ResumenSegmentoUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::uiOutput(ns("kpis")),
      shiny::fluidRow(
        shiny::column(5,
          bs4Dash::bs4Card(title = "Clientes por estado", width = 12,
                           solidHeader = FALSE, status = "warning", collapsible = FALSE,
                           plotly::plotlyOutput(ns("plot_dona"), height = "250px"))
        ),
        shiny::column(7,
          bs4Dash::bs4Card(title = "Resumen por estado", width = 12,
                           solidHeader = FALSE, status = "secondary", collapsible = FALSE,
                           TablaReactableUI(ns("sub_seg"), sortable = TRUE))
        )
      )
    )
  }
  ResumenSegmento <- function(id, segmento_r) {
    shiny::moduleServer(id, function(input, output, session) {
      pal <- c("CLIENTE ACTIVO" = "#2C7BB6", "CLIENTE A RECUPERAR" = "#F4A820",
               "NUEVO DEL PERIODO" = "#27AE60")
      resumen_r <- shiny::reactive({
        shiny::req(segmento_r(), segmento_r() %in% c("A", "B", "C"))
        clientes_base |>
          dplyr::filter(segmento == segmento_r()) |>
          dplyr::group_by(estado) |>
          dplyr::summarise(n_clientes = dplyr::n(), sacos = sum(sacos),
                           margen = sum(margen), tasa_media = mean(tasa_fact),
                           .groups = "drop") |>
          dplyr::mutate(color = dplyr::coalesce(pal[estado], "#BDC3C7"))
      })
      output$kpis <- shiny::renderUI({
        shiny::req(nrow(resumen_r()) > 0)
        df <- resumen_r()
        shiny::tags$div(
          style = "padding:8px 0 14px; display:flex; flex-wrap:wrap; gap:10px;",
          shiny::tags$div(
            style = paste0("padding:4px 14px; border-radius:20px; background:#EFF6FF;",
                           "color:#1A5276; font-weight:700; font-size:13px;"),
            paste0("Segmento ", segmento_r())
          ),
          .kpi_mini("Clientes", format(sum(df$n_clientes), big.mark = ",")),
          .kpi_mini("Sacos",    format(sum(df$sacos), big.mark = ",")),
          .kpi_mini("Margen",   paste0("$", round(sum(df$margen) / 1e6, 1), "M"), "#1E8449")
        )
      })
      output$plot_dona <- plotly::renderPlotly({
        shiny::req(nrow(resumen_r()) > 0)
        df <- resumen_r()
        p <- plotly::plot_ly(df, labels = ~estado, values = ~n_clientes, type = "pie",
                             hole = 0.52, marker = list(colors = df$color),
                             textinfo = "percent+label",
                             hovertemplate = "<b>%{label}</b><br>%{value} clientes<extra></extra>")
        .plotly_std(p) |> plotly::layout(showlegend = FALSE)
      })
      TablaReactable(
        id = "sub_seg", data = resumen_r,
        col_specs = list(
          estado     = list(label = "Estado",   min_width = 155),
          n_clientes = list(label = "Clientes", formato = "coma",       min_width = 80),
          sacos      = list(label = "Sacos",    formato = "coma",       min_width = 90),
          margen     = list(label = "Margen",   formato = "dinero",     min_width = 120),
          tasa_media = list(label = "Tasa %",   formato = "porcentaje", min_width = 90,
                            color_fn = .tfn)
        ),
        modo_seleccion = "ninguno", col_header_n = 1L, sortable = TRUE, page_size = 5
      )
    })
  }

  # MODULO AUXILIAR: DetalleClienteModal ----
  DetalleClienteModalUI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
      shiny::uiOutput(ns("header")),
      shiny::fluidRow(
        shiny::column(7,
          bs4Dash::bs4Card(title = "Companeros de asesor", width = 12,
                           solidHeader = FALSE, status = "primary", collapsible = FALSE,
                           TablaReactableUI(ns("sub_companeros"), sortable = TRUE))
        ),
        shiny::column(5,
          bs4Dash::bs4Card(title = "Distribucion del segmento", width = 12,
                           solidHeader = FALSE, status = "warning", collapsible = FALSE,
                           plotly::plotlyOutput(ns("plot_seg"), height = "200px"))
        )
      )
    )
  }
  DetalleClienteModal <- function(id, sel_r) {
    shiny::moduleServer(id, function(input, output, session) {
      fila_r <- shiny::reactive({ shiny::req(sel_r()); sel_r()$fila })
      comp_r <- shiny::reactive({
        shiny::req(fila_r())
        clientes_base |>
          dplyr::filter(asesor == as.character(fila_r()$asesor[[1]]),
                        cliente_id != as.character(fila_r()$cliente_id[[1]])) |>
          dplyr::select(cliente_id, razon_social, segmento, estado, sacos, margen, tasa_fact)
      })
      seg_r <- shiny::reactive({
        shiny::req(fila_r())
        clientes_base |>
          dplyr::filter(segmento == as.character(fila_r()$segmento[[1]])) |>
          dplyr::count(estado, name = "n")
      })
      output$header <- shiny::renderUI({
        shiny::req(fila_r())
        f    <- fila_r()
        tasa <- as.numeric(f$tasa_fact[[1]])
        ec   <- switch(as.character(f$estado[[1]]), "CLIENTE ACTIVO" = "#1A5276",
                       "CLIENTE A RECUPERAR" = "#784212", "NUEVO DEL PERIODO" = "#1E8449", "#333")
        eb   <- switch(as.character(f$estado[[1]]), "CLIENTE ACTIVO" = "#EFF6FF",
                       "CLIENTE A RECUPERAR" = "#FFF8EC", "NUEVO DEL PERIODO" = "#EDFBF2", "#F8F9FA")
        shiny::tags$div(
          style = "padding:8px 0 14px;",
          shiny::tags$div(
            style = "display:flex; align-items:center; gap:12px; margin-bottom:12px;",
            shiny::tags$div(
              style = paste0("padding:3px 12px; border-radius:20px; background:", eb,
                             "; color:", ec, "; font-weight:600; font-size:12px;"),
              as.character(f$estado[[1]])),
            shiny::tags$span(paste0("ID: ", as.character(f$cliente_id[[1]])),
                             style = "font-size:11px; color:#888;")
          ),
          shiny::tags$div(
            style = "display:flex; flex-wrap:wrap; gap:10px;",
            .kpi_mini("Asesor",   as.character(f$asesor[[1]])),
            .kpi_mini("Segmento", as.character(f$segmento[[1]]), "#555"),
            .kpi_mini("Sacos",    format(as.integer(f$sacos[[1]]), big.mark = ",")),
            .kpi_mini("Margen",   paste0("$", format(round(as.numeric(f$margen[[1]])),
                                                     big.mark = ",")), "#1E8449"),
            .kpi_mini("Tasa %",   paste0(round(tasa * 100, 1), "%"),
                      if (!is.na(tasa) && tasa >= 0.75) "#1E8449" else "#943126")
          )
        )
      })
      TablaReactable(
        id = "sub_companeros", data = comp_r,
        col_specs = list(
          cliente_id   = list(label = "ID",      min_width = 55),
          razon_social = list(label = "Cliente", min_width = 140),
          segmento     = list(label = "Seg.",    min_width = 50, alinear = "center"),
          estado       = list(label = "Estado",  min_width = 140),
          sacos        = list(label = "Sacos",   formato = "coma",       min_width = 75),
          margen       = list(label = "Margen",  formato = "dinero",     min_width = 105),
          tasa_fact    = list(label = "Tasa %",  formato = "porcentaje", min_width = 70,
                              color_fn = .tfn)
        ),
        modo_seleccion = "ninguno", col_header_n = 2L, sortable = TRUE, page_size = 5
      )
      output$plot_seg <- plotly::renderPlotly({
        shiny::req(nrow(seg_r()) > 0)
        df  <- seg_r()
        pal <- c("CLIENTE ACTIVO" = "#2C7BB6", "CLIENTE A RECUPERAR" = "#F4A820",
                 "NUEVO DEL PERIODO" = "#27AE60")
        p <- plotly::plot_ly(df, labels = ~estado, values = ~n, type = "pie", hole = 0.50,
                             marker = list(colors = dplyr::coalesce(pal[df$estado], "#BDC3C7")),
                             textinfo = "percent",
                             hovertemplate = "<b>%{label}</b><br>%{value}<extra></extra>")
        p |>
          plotly::layout(showlegend = TRUE, plot_bgcolor = "white", paper_bgcolor = "white",
                         legend = list(font = list(size = 10)),
                         margin = list(t = 5, b = 5, l = 0, r = 0)) |>
          plotly::config(displayModeBar = FALSE)
      })
    })
  }

  # MODULO AUXILIAR: DetalleLeadModal ----
  DetalleLeadModalUI <- function(id) shiny::uiOutput(shiny::NS(id)("contenido"))
  DetalleLeadModal <- function(id, sel_r) {
    shiny::moduleServer(id, function(input, output, session) {
      output$contenido <- shiny::renderUI({
        shiny::req(sel_r())
        sel <- sel_r()
        f   <- sel$fila
        header <- shiny::tags$div(
          style = "display:flex; flex-wrap:wrap; gap:10px; margin-bottom:14px;",
          .kpi_mini("Lead",      as.character(f$razon_social[[1]])),
          .kpi_mini("Asesor",    as.character(f$asesor[[1]])),
          .kpi_mini("Estado",    as.character(f$estado_lead[[1]]), "#2C7BB6"),
          .kpi_mini("Sacos Op.", format(as.integer(f$sacos_op[[1]]), big.mark = ",")),
          .kpi_mini("Margen Op.",paste0("$", format(round(as.numeric(f$margen_op[[1]])),
                                                   big.mark = ",")), "#1E8449")
        )
        cuerpo <- if (sel$col == "Editar") {
          shiny::tagList(
            shiny::tags$h6(shiny::tagList(shiny::icon("edit"), " Editar lead"),
                           style = "margin:0 0 10px; font-weight:700;"),
            shiny::fluidRow(
              shiny::column(6, shiny::tags$div(class = "form-group",
                shiny::tags$label("Estado", style = "font-size:12px;"),
                shiny::tags$select(class = "form-control form-control-sm",
                  shiny::tags$option("NUEVO"), shiny::tags$option("CONTACTADO"),
                  shiny::tags$option("PROPUESTA"), shiny::tags$option("NEGOCIACION")))),
              shiny::column(6, shiny::tags$div(class = "form-group",
                shiny::tags$label("Asesor", style = "font-size:12px;"),
                shiny::tags$select(class = "form-control form-control-sm",
                  shiny::tags$option("Ana Torres"), shiny::tags$option("Luis Gomez"),
                  shiny::tags$option("Maria Rios"))))
            ),
            shiny::tags$p(shiny::icon("circle-info"), " Formulario simulado.",
                          style = "font-size:11px; color:#888; margin-top:6px;")
          )
        } else if (sel$col == "Contacto") {
          shiny::tagList(
            shiny::tags$h6(shiny::tagList(shiny::icon("phone"), " Registrar contacto"),
                           style = "margin:0 0 10px; font-weight:700;"),
            shiny::tags$div(class = "form-group",
              shiny::tags$label("Observaciones", style = "font-size:12px;"),
              shiny::tags$textarea(class = "form-control form-control-sm", rows = 3,
                                   placeholder = "Resultado del contacto...")),
            shiny::tags$p(shiny::icon("circle-info"), " Formulario simulado.",
                          style = "font-size:11px; color:#888; margin-top:6px;")
          )
        } else {
          shiny::tags$p(shiny::icon("info-circle"),
                        paste0("Columna '", sel$col, "' sin accion asociada."),
                        style = "font-size:13px; color:#888;")
        }
        shiny::tagList(header, cuerpo)
      })
    })
  }

  # UI ----
  ui <- bs4Dash::bs4DashPage(
    dark = FALSE, title = "Demo TablaReactable",
    header = bs4Dash::bs4DashNavbar(
      title = bs4Dash::bs4DashBrand(
        title = shiny::tagList(
          shiny::tags$span("TablaReactable", style = "font-weight:700; letter-spacing:.5px;"),
          shiny::tags$span(" Demo",          style = "font-weight:300; color:#aaa;")),
        color = "white")),
    sidebar = bs4Dash::bs4DashSidebar(bs4Dash::bs4SidebarMenu(
      bs4Dash::bs4SidebarMenuSubItem("Base",              "tab_grupo_base",    icon = shiny::icon("table")),
      bs4Dash::bs4SidebarMenuItem("Modo Fila",            "tab_fila",          icon = shiny::icon("table-rows")),
      bs4Dash::bs4SidebarMenuItem("Modo Celda",           "tab_celda",         icon = shiny::icon("table-cells")),
      bs4Dash::bs4SidebarMenuItem("Modo Columna",         "tab_columna",       icon = shiny::icon("table-columns")),
      bs4Dash::bs4SidebarMenuItem("Solo lectura",         "tab_ninguno",       icon = shiny::icon("book-open")),
      bs4Dash::bs4SidebarMenuSubItem("Avanzado",          "tab_grupo_avanzado",icon = shiny::icon("wand-magic-sparkles")),
      bs4Dash::bs4SidebarMenuItem("Modal — Fila",    "tab_modal_fila",    icon = shiny::icon("window-restore")),
      bs4Dash::bs4SidebarMenuItem("Modal — Celda",   "tab_modal_celda",   icon = shiny::icon("hand-pointer")),
      bs4Dash::bs4SidebarMenuItem("Showcase col_specs",   "tab_col_specs",     icon = shiny::icon("sliders")),
      bs4Dash::bs4SidebarMenuSubItem("Modulos Externos",  "tab_grupo_ext",     icon = shiny::icon("puzzle-piece")),
      bs4Dash::bs4SidebarMenuItem("Asesor + Grafico",     "tab_ext_asesor",    icon = shiny::icon("user-tie")),
      bs4Dash::bs4SidebarMenuItem("Segmento + Dona",      "tab_ext_segmento",  icon = shiny::icon("layer-group")),
      bs4Dash::bs4SidebarMenuSubItem("Restricciones",     "tab_grupo_panel",   icon = shiny::icon("sliders")),
      bs4Dash::bs4SidebarMenuItem("Filas Bloqueadas",     "tab_panel_inline",  icon = shiny::icon("ban")),
      bs4Dash::bs4SidebarMenuItem("Panel en Padre",       "tab_panel_externo", icon = shiny::icon("columns")),
      bs4Dash::bs4SidebarMenuItem("Restriccion Cols",     "tab_rest_cols",     icon = shiny::icon("filter")),
      bs4Dash::bs4SidebarMenuItem("Restriccion Filas",    "tab_rest_filas",    icon = shiny::icon("ban"))
    )),
    body = bs4Dash::bs4DashBody(
      shiny::tags$style(shiny::HTML(".caja-modal-footer {font-size:12px;color:#6C757D;}")),
      bs4Dash::bs4TabItems(
      bs4Dash::bs4TabItem(tabName = "tab_fila",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(title = "Cartera de clientes", width = 12,
                           solidHeader = TRUE, status = "primary",
            TablaReactableUI("demo_fila", titulo = "Cartera de clientes",
                             subtitulo = "Clic en una fila para ver el detalle",
                             footer = "Fuente: CRM.", footer_tipo = "info", sortable = TRUE))
        )),
        shiny::fluidRow(
          shiny::column(6, .panel_log("log_fila", "seleccion() reactive")),
          shiny::column(6, bs4Dash::bs4Card(title = "Detalle del cliente", width = 12,
                                            solidHeader = TRUE, status = "info",
                                            shiny::uiOutput("accion_fila")))
        )
      ),
      bs4Dash::bs4TabItem(tabName = "tab_celda",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(title = "Ventas por mes y linea", width = 12,
                           solidHeader = TRUE, status = "primary",
            TablaReactableUI("demo_celda", titulo = "Ventas mensuales por linea",
                             subtitulo = "Clic en % Sacos o % Margen",
                             footer = "% cumplimiento sobre presupuesto vigente.",
                             footer_tipo = "warning", sortable = TRUE))
        )),
        shiny::fluidRow(
          shiny::column(6, .panel_log("log_celda", "seleccion() reactive")),
          shiny::column(6, bs4Dash::bs4Card(title = "Analisis cumplimiento", width = 12,
                                            solidHeader = TRUE, status = "info",
                                            shiny::uiOutput("accion_celda")))
        )
      ),
      bs4Dash::bs4TabItem(tabName = "tab_columna",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(title = "Transiciones de estado", width = 12,
                           solidHeader = TRUE, status = "primary",
            TablaReactableUI("demo_columna", titulo = "Matriz de transiciones",
                             subtitulo = "Clic en una columna para estadisticas",
                             footer = "Transiciones entre periodos consecutivos.",
                             footer_tipo = "dark", sortable = TRUE))
        )),
        shiny::fluidRow(
          shiny::column(6, .panel_log("log_columna", "seleccion() reactive")),
          shiny::column(6, bs4Dash::bs4Card(title = "Estadisticas de la columna",
                                            width = 12, solidHeader = TRUE, status = "info",
                                            shiny::uiOutput("accion_columna")))
        )
      ),
      bs4Dash::bs4TabItem(tabName = "tab_ninguno",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(title = "Solo lectura", width = 12,
                           solidHeader = TRUE, status = "secondary",
            TablaReactableUI("demo_ninguno", titulo = "Referencia de cartera",
                             subtitulo = "Sin interaccion de seleccion",
                             footer = "No se registra seleccion.", footer_tipo = "dark",
                             sortable = FALSE))
        ))
      ),
      bs4Dash::bs4TabItem(tabName = "tab_modal_fila",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(
            title  = "Modal con modulo hijo — Fila",
            width  = 12, solidHeader = TRUE, status = "success",
            footer = shiny::tags$div(class = "caja-modal-footer",
              shiny::tags$div(shiny::icon("circle-info"),
                              " Patron eager: DetalleClienteModal registrado al arrancar."),
              shiny::tags$div(shiny::icon("bolt"),
                              " modal_pre_fn actualiza rv antes de showModal().")),
            TablaReactableUI("demo_modal_fila",
                             titulo = "Clientes — modulo reactivo en modal",
                             subtitulo = "Clic en una fila para abrir el modal",
                             sortable = TRUE)
          )
        )),
        shiny::fluidRow(shiny::column(12, .panel_log("log_modal_fila", "seleccion() en paralelo")))
      ),
      bs4Dash::bs4TabItem(tabName = "tab_modal_celda",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(
            title  = "Modal con modulo hijo — Celda",
            width  = 12, solidHeader = TRUE, status = "warning",
            footer = shiny::tags$div(class = "caja-modal-footer",
              shiny::tags$div(shiny::icon("filter"),
                              " cols_activos = c('Editar', 'Contacto'): filtro JS."),
              shiny::tags$div(shiny::icon("puzzle-piece"),
                              " DetalleLeadModal despacha por sel$col.")),
            TablaReactableUI("demo_modal_celda",
                             titulo = "Leads — modulo reactivo por columna",
                             subtitulo = "Clic en 'Editar' o 'Contactar'",
                             footer = "Patron eager: un modulo, cero boilerplate.",
                             footer_tipo = "warning", sortable = TRUE)
          )
        )),
        shiny::fluidRow(shiny::column(12, .panel_log("log_modal_celda", "seleccion() col y valor")))
      ),
      bs4Dash::bs4TabItem(tabName = "tab_col_specs",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(
            title  = "Showcase col_specs — formatos racafe",
            width  = 12, solidHeader = TRUE, status = "info",
            footer = shiny::tags$div(class = "caja-modal-footer",
              shiny::tags$div(shiny::icon("circle-info"),
                              " Formatos: coma, dinero, porcentaje, kwh, millones."),
              shiny::tags$div(shiny::icon("triangle-exclamation"),
                              " porcentaje escala x100: valores deben ser [0, 1].")),
            TablaReactableUI("demo_col_specs", titulo = "Presupuesto por asesor",
                             subtitulo = "Formatos: coma, dinero, porcentaje, kwh, millones",
                             sortable = TRUE)
          )
        )),
        shiny::fluidRow(
          shiny::column(6, .panel_log("log_col_specs", "seleccion() modo fila")),
          shiny::column(6, bs4Dash::bs4Card(title = "Resumen del asesor", width = 12,
                                            solidHeader = TRUE, status = "info",
                                            shiny::uiOutput("accion_col_specs")))
        )
      ),
      bs4Dash::bs4TabItem(tabName = "tab_ext_asesor",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(
            title  = "Presupuesto por asesor", width = 12, solidHeader = TRUE, status = "primary",
            footer = shiny::tags$div(class = "caja-modal-footer", shiny::icon("circle-info"),
                                     " Seleccionar fila activa DetalleAsesor: KPIs + barras + sub-tabla."),
            TablaReactableUI("demo_ext_asesor", titulo = "Presupuesto por asesor",
                             subtitulo = "Seleccione un asesor para ver el detalle",
                             sortable = TRUE))
        )),
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(title = "DetalleAsesor — modulo externo", width = 12,
                           solidHeader = TRUE, status = "info", collapsible = FALSE,
                           shiny::uiOutput("wrapper_asesor"))
        ))
      ),
      bs4Dash::bs4TabItem(tabName = "tab_ext_segmento",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(
            title  = "Clientes", width = 12, solidHeader = TRUE, status = "primary",
            footer = shiny::tags$div(class = "caja-modal-footer", shiny::icon("circle-info"),
                                     " Seleccionar cliente activa ResumenSegmento: KPIs + dona + sub-tabla."),
            TablaReactableUI("demo_ext_segmento",
                             titulo = "Clientes — seleccion de segmento",
                             subtitulo = "Seleccione un cliente para ver el resumen",
                             sortable = TRUE))
        )),
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(title = "ResumenSegmento — modulo externo", width = 12,
                           solidHeader = TRUE, status = "warning", collapsible = FALSE,
                           shiny::uiOutput("wrapper_segmento"))
        ))
      ),
      bs4Dash::bs4TabItem(tabName = "tab_panel_inline",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(
            title  = "Filas bloqueadas + modal integrado", width = 12,
            solidHeader = TRUE, status = "success",
            footer = shiny::tags$div(class = "caja-modal-footer",
              shiny::icon("ban"), " filas_bloqueadas = c('TOTAL','OTROS') suprimidas en JS.",
              shiny::tags$br(),
              shiny::icon("circle-info"),
              " modal_titulo_fn + modal_contenido_fn activan showModal(): cero observeEvent."),
            TablaReactableUI("demo_panel_inline",
                             titulo = "Cartera — modal con filas bloqueadas",
                             subtitulo = "TOTAL y OTROS bloqueadas. Clic en otra fila abre modal.",
                             sortable = TRUE))
        ))
      ),
      bs4Dash::bs4TabItem(tabName = "tab_panel_externo",
        shiny::fluidRow(
          shiny::column(8,
            bs4Dash::bs4Card(
              title  = "Tabla — seleccion", width = 12, solidHeader = TRUE, status = "primary",
              footer = shiny::tags$div(class = "caja-modal-footer",
                shiny::icon("circle-info"),
                " Patron inline: seleccion() emitida al padre via observeEvent + renderUI."),
              TablaReactableUI("demo_panel_externo", titulo = "Presupuesto por asesor",
                               subtitulo = "Seleccione un asesor para ver el panel lateral",
                               sortable = TRUE))
          ),
          shiny::column(4,
            bs4Dash::bs4Card(title = "Detalle — panel del padre", width = 12,
                             solidHeader = TRUE, status = "info", collapsible = FALSE,
                             shiny::uiOutput("panel_asesor_externo")))
        )
      ),
      bs4Dash::bs4TabItem(tabName = "tab_rest_cols",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(
            title  = "Restriccion de columnas activas", width = 12,
            solidHeader = TRUE, status = "warning",
            footer = shiny::tags$div(class = "caja-modal-footer",
              shiny::icon("circle-info"),
              " cols_activos = c('Editar','Contacto'): filtro JS puro, sin round-trip."),
            TablaReactableUI("demo_rest_cols",
                             titulo = "Leads — solo columnas de accion activas",
                             subtitulo = "Solo 'Editar' y 'Contactar' disparan el modal.",
                             sortable = TRUE))
        )),
        shiny::fluidRow(shiny::column(12, .panel_log("log_rest_cols", "seleccion() Editar / Contacto")))
      ),
      bs4Dash::bs4TabItem(tabName = "tab_rest_filas",
        shiny::fluidRow(shiny::column(12,
          bs4Dash::bs4Card(
            title  = "Restriccion de filas seleccionables", width = 12,
            solidHeader = TRUE, status = "danger",
            footer = shiny::tags$div(class = "caja-modal-footer",
              shiny::tags$div(shiny::icon("ban"),
                              " filas_bloqueadas: TOTAL/OTROS suprimidas en JS."),
              shiny::tags$div(shiny::icon("code"),
                              " filas_seleccionables: predicado R, solo segmento A.")),
            TablaReactableUI("demo_rest_filas",
                             titulo = "Clientes — solo segmento A seleccionable",
                             subtitulo = "TOTAL/OTROS bloqueadas. Solo seg. A activa panel.",
                             sortable = TRUE))
        )),
        shiny::fluidRow(
          shiny::column(6, .panel_log("log_rest_filas", "seleccion() solo seg A")),
          shiny::column(6, bs4Dash::bs4Card(title = "Detalle del cliente", width = 12,
                                            solidHeader = TRUE, status = "info",
                                            shiny::uiOutput("accion_rest_filas")))
        )
      )
    )),
    footer = bs4Dash::bs4DashFooter(
      left  = shiny::tagList(shiny::tags$span("racafeModulos", style = "font-weight:600;"),
                             " — TablaReactable Demo"),
      right = "2025"
    )
  )

  # SERVER ----
  server <- function(input, output, session) {
    sel_fila <- TablaReactable(
      id = "demo_fila", data = shiny::reactive(clientes), columnas = coldefs_clientes,
      modo_seleccion = "fila", id_col = "cliente_id", col_header_n = 2L,
      sortable = TRUE, page_size = 10, mostrar_badge = TRUE
    )
    .render_log(sel_fila$seleccion, output, "log_fila")
    output$accion_fila <- shiny::renderUI({
      sel <- sel_fila$seleccion()
      if (is.null(sel)) return(shiny::tags$p("Esperando seleccion...",
                                              style = "color:#aaa; font-size:12px; margin:0;"))
      f  <- sel$fila
      ec <- switch(as.character(f$estado[[1]]), "CLIENTE ACTIVO" = "#1A5276",
                   "CLIENTE A RECUPERAR" = "#784212", "NUEVO DEL PERIODO" = "#1E8449", "#333")
      shiny::tags$div(
        shiny::tags$p(shiny::icon("id-card"), shiny::tags$strong(f$razon_social),
                      shiny::tags$span(paste0(" — ", f$cliente_id),
                                       style = "color:#888; font-size:11px;"),
                      style = "margin-bottom:10px;"),
        shiny::tags$div(style = "display:flex; gap:10px; flex-wrap:wrap;",
          .kpi_mini("Asesor",   as.character(f$asesor[[1]])),
          .kpi_mini("Segmento", as.character(f$segmento[[1]]), "#555"),
          .kpi_mini("Estado",   as.character(f$estado[[1]]), ec),
          .kpi_mini("Tasa",     paste0(round(as.numeric(f$tasa_fact[[1]]) * 100, 1), "%"),
                    if (!is.na(f$tasa_fact[[1]]) && f$tasa_fact[[1]] >= 0.75)
                      "#1E8449" else "#943126"),
          .kpi_mini("Sacos",  format(as.integer(f$sacos[[1]]), big.mark = ",")),
          .kpi_mini("Margen", paste0("$", format(round(as.numeric(f$margen[[1]])),
                                                 big.mark = ",")), "#1E8449")
        )
      )
    })

    sel_celda <- TablaReactable(
      id = "demo_celda", data = shiny::reactive(ventas_mes), columnas = coldefs_ventas,
      modo_seleccion = "celda", id_col = NULL, col_header_n = 2L,
      sortable = TRUE, page_size = 12, mostrar_badge = TRUE
    )
    .render_log(sel_celda$seleccion, output, "log_celda")
    output$accion_celda <- shiny::renderUI({
      sel <- sel_celda$seleccion()
      if (is.null(sel)) return(shiny::tags$p("Esperando seleccion...",
                                              style = "color:#aaa; font-size:12px; margin:0;"))
      if (sel$col %in% c("cumpl_sacos", "cumpl_margen")) {
        v     <- as.numeric(sel$valor)
        color <- if (v >= 0.90) "#1E8449" else if (v >= 0.70) "#784212" else "#943126"
        shiny::tags$div(
          shiny::tags$p(shiny::icon("chart-bar"),
                        shiny::tags$strong(paste0(sel$col, " = ", round(v * 100, 1), "%")),
                        style = paste0("color:", color, "; margin-bottom:4px;")),
          shiny::tags$p(if (v >= 0.90) "✅ Optimo"
                        else if (v >= 0.70) "⚠️ Zona de alerta"
                        else "❌ Critico",
                        style = "font-size:12px; color:#555; margin:0;")
        )
      } else {
        shiny::tags$p(shiny::icon("info-circle"),
                      paste0("Celda [", sel$col, "]. Clic en % para analisis."),
                      style = "font-size:12px; color:#888; margin:0;")
      }
    })

    sel_columna <- TablaReactable(
      id = "demo_columna", data = shiny::reactive(transiciones),
      modo_seleccion = "columna", col_header_n = 2L,
      sortable = TRUE, page_size = 10, mostrar_badge = TRUE
    )
    .render_log(sel_columna$seleccion, output, "log_columna")
    output$accion_columna <- shiny::renderUI({
      sel <- sel_columna$seleccion()
      if (is.null(sel)) return(shiny::tags$p("Esperando seleccion...",
                                              style = "color:#aaa; font-size:12px; margin:0;"))
      col_data <- sel$data[[sel$col]]
      shiny::tags$div(
        shiny::tags$p(shiny::icon("columns"), shiny::tags$strong(paste0("Columna: ", sel$col)),
                      style = "color:#2C7BB6; margin-bottom:8px;"),
        if (is.numeric(col_data)) {
          shiny::tags$div(style = "display:flex; gap:12px; flex-wrap:wrap;",
            .kpi_mini("Min",   format(min(col_data,  na.rm = TRUE), big.mark = ",")),
            .kpi_mini("Max",   format(max(col_data,  na.rm = TRUE), big.mark = ",")),
            .kpi_mini("Media", format(round(mean(col_data, na.rm = TRUE), 1), big.mark = ",")),
            .kpi_mini("Suma",  format(sum(col_data,  na.rm = TRUE), big.mark = ","), "#1E8449"))
        } else {
          frec <- sort(table(col_data), decreasing = TRUE)
          shiny::tags$div(style = "font-size:12px;", lapply(seq_along(frec), function(i) {
            shiny::tags$span(paste0(names(frec)[i], " (", frec[i], ")"),
                             style = paste0("display:inline-block; margin:2px 4px; padding:2px 8px;",
                                            "background:#EFF6FF; border-radius:3px; color:#1A5276;"))
          }))
        }
      )
    })

    TablaReactable(
      id = "demo_ninguno", data = shiny::reactive(clientes), columnas = coldefs_clientes,
      modo_seleccion = "ninguno", id_col = "cliente_id",
      col_header_n = 1L, sortable = FALSE, page_size = 15
    )

    sel_modal_fila_rv <- shiny::reactiveVal(NULL)
    DetalleClienteModal("mod_det_cliente", shiny::reactive(sel_modal_fila_rv()))
    sel_modal_fila <- TablaReactable(
      id = "demo_modal_fila", data = shiny::reactive(clientes),
      columnas = coldefs_clientes, modo_seleccion = "fila", id_col = "cliente_id",
      col_header_n = 2L, sortable = TRUE, page_size = 10,
      modal_icon = "id-card", modal_size = "xl",
      modal_titulo_fn    = function(sel) paste0("Detalle: ", as.character(sel$fila$razon_social[[1]])),
      modal_pre_fn       = function(sel) sel_modal_fila_rv(sel),
      modal_contenido_fn = function(sel) DetalleClienteModalUI("mod_det_cliente")
    )
    .render_log(sel_modal_fila$seleccion, output, "log_modal_fila")

    sel_modal_celda_rv <- shiny::reactiveVal(NULL)
    DetalleLeadModal("mod_det_lead", shiny::reactive(sel_modal_celda_rv()))
    sel_modal_celda <- TablaReactable(
      id = "demo_modal_celda", data = shiny::reactive(leads),
      col_specs = specs_leads, modo_seleccion = "celda", id_col = "lead_id",
      col_header_n = 2L, sortable = TRUE, page_size = 10,
      cols_activos = c("Editar", "Contacto"),
      modal_icon = "bolt", modal_size = "l",
      modal_titulo_fn    = function(sel) {
        accion <- switch(sel$col, Editar = "Editar lead", Contacto = "Registrar contacto",
                         paste0("Celda: ", sel$col))
        paste0(accion, " — ", as.character(sel$fila$razon_social[[1]]))
      },
      modal_pre_fn       = function(sel) sel_modal_celda_rv(sel),
      modal_contenido_fn = function(sel) DetalleLeadModalUI("mod_det_lead"),
      modal_footer = "Patron eager: rv actualizado antes de showModal().",
      modal_footer_tipo = "info"
    )
    .render_log(sel_modal_celda$seleccion, output, "log_modal_celda")

    sel_col_specs <- TablaReactable(
      id = "demo_col_specs", data = shiny::reactive(presupuesto_asesor),
      col_specs = specs_presupuesto, modo_seleccion = "fila", id_col = NULL,
      col_header_n = 1L, sortable = TRUE, page_size = 10, mostrar_badge = TRUE
    )
    .render_log(sel_col_specs$seleccion, output, "log_col_specs")
    output$accion_col_specs <- shiny::renderUI({
      sel <- sel_col_specs$seleccion()
      if (is.null(sel)) return(shiny::tags$p("Esperando seleccion...",
                                              style = "color:#aaa; font-size:12px; margin:0;"))
      f       <- sel$fila
      cumpl_s <- as.numeric(f$cumpl_sacos[[1]])
      cumpl_m <- as.numeric(f$cumpl_margen[[1]])
      col_cs  <- if (cumpl_s >= 1.0) "#1E8449" else if (cumpl_s >= 0.85) "#784212" else "#943126"
      col_cm  <- if (cumpl_m >= 1.0) "#1E8449" else if (cumpl_m >= 0.85) "#784212" else "#943126"
      shiny::tags$div(
        shiny::tags$p(shiny::icon("user-tie"), shiny::tags$strong(as.character(f$asesor[[1]])),
                      style = "margin-bottom:12px; font-size:14px;"),
        shiny::tags$div(style = "display:flex; gap:10px; flex-wrap:wrap;",
          .kpi_mini("Clientes",    format(as.integer(f$clientes[[1]]), big.mark = ",")),
          .kpi_mini("Sacos Real",  format(as.integer(f$sacos_real[[1]]), big.mark = ",")),
          .kpi_mini("Margen Real", paste0("$", format(round(as.numeric(f$margen_real[[1]])),
                                                      big.mark = ",")), "#1E8449"),
          .kpi_mini("% Sacos",     paste0(round(cumpl_s * 100, 1), "%"), col_cs),
          .kpi_mini("% Margen",    paste0(round(cumpl_m * 100, 1), "%"), col_cm),
          .kpi_mini("kWh Prom",    paste0(as.numeric(f$kwh_promedio[[1]]), " kWh"), "#555"),
          .kpi_mini("Margen ($M)", paste0("$", as.numeric(f$margen_mm[[1]]), "M"), "#2C7BB6")
        )
      )
    })

    fila_asesor_rv <- shiny::reactiveVal(NULL)
    sel_ext_asesor <- TablaReactable(
      id = "demo_ext_asesor", data = shiny::reactive(presupuesto_asesor),
      col_specs = specs_presupuesto, modo_seleccion = "fila", id_col = NULL,
      col_header_n = 1L, sortable = TRUE, page_size = 10, mostrar_badge = TRUE
    )
    shiny::observeEvent(sel_ext_asesor$seleccion(), {
      sel <- sel_ext_asesor$seleccion()
      if (!is.null(sel)) fila_asesor_rv(sel$fila)
    })
    output$wrapper_asesor <- shiny::renderUI({
      if (is.null(fila_asesor_rv())) return(.espera_seleccion())
      DetalleAsesorUI("mod_asesor")
    })
    DetalleAsesor("mod_asesor", shiny::reactive(fila_asesor_rv()))

    segmento_rv <- shiny::reactiveVal(NULL)
    sel_ext_segmento <- TablaReactable(
      id = "demo_ext_segmento",
      data = shiny::reactive(dplyr::select(clientes, cliente_id, razon_social,
                                           asesor, segmento, estado, sacos, margen, tasa_fact)),
      col_specs = specs_clientes_ext, modo_seleccion = "fila", id_col = "cliente_id",
      col_header_n = 2L, sortable = TRUE, page_size = 10, mostrar_badge = TRUE
    )
    shiny::observeEvent(sel_ext_segmento$seleccion(), {
      sel <- sel_ext_segmento$seleccion()
      if (!is.null(sel)) {
        seg <- as.character(sel$fila$segmento[[1]])
        if (seg %in% c("A", "B", "C")) segmento_rv(seg)
      }
    })
    output$wrapper_segmento <- shiny::renderUI({
      if (is.null(segmento_rv())) return(.espera_seleccion(
        "Seleccione un cliente para ver el resumen de su segmento."))
      ResumenSegmentoUI("mod_segmento")
    })
    ResumenSegmento("mod_segmento", shiny::reactive(segmento_rv()))

    TablaReactable(
      id = "demo_panel_inline", data = shiny::reactive(clientes),
      columnas = coldefs_clientes, modo_seleccion = "fila", id_col = "cliente_id",
      col_header_n = 2L, sortable = TRUE, page_size = 10,
      filas_bloqueadas   = c("TOTAL", "OTROS"),
      modal_icon         = "id-card", modal_size = "l",
      modal_titulo_fn    = function(sel) paste0("Detalle: ", as.character(sel$fila$razon_social[[1]])),
      modal_contenido_fn = function(sel) .modal_detalle_cliente(sel)
    )

    fila_externo_rv <- shiny::reactiveVal(NULL)
    sel_panel_externo <- TablaReactable(
      id = "demo_panel_externo", data = shiny::reactive(presupuesto_asesor),
      col_specs = specs_presupuesto, modo_seleccion = "fila", id_col = NULL,
      col_header_n = 1L, sortable = TRUE, page_size = 10,
      filas_bloqueadas = c("TOTAL", "OTROS")
    )
    shiny::observeEvent(sel_panel_externo$seleccion(), {
      sel <- sel_panel_externo$seleccion()
      if (!is.null(sel)) fila_externo_rv(sel$fila)
    })
    output$panel_asesor_externo <- shiny::renderUI({
      f <- fila_externo_rv()
      if (is.null(f)) return(.espera_seleccion("Seleccione un asesor."))
      cumpl_s <- as.numeric(f$cumpl_sacos[[1]])
      cumpl_m <- as.numeric(f$cumpl_margen[[1]])
      shiny::tags$div(
        shiny::tags$p(shiny::icon("user-tie"), shiny::tags$strong(as.character(f$asesor[[1]])),
                      style = "font-size:14px; margin-bottom:12px;"),
        shiny::tags$div(style = "display:flex; flex-wrap:wrap; gap:10px;",
          .kpi_mini("Clientes",   format(as.integer(f$clientes[[1]]), big.mark = ",")),
          .kpi_mini("Sacos Real", format(as.integer(f$sacos_real[[1]]), big.mark = ",")),
          .kpi_mini("% Sacos",    paste0(round(cumpl_s * 100, 1), "%"),
                    if (cumpl_s >= 1) "#1E8449" else if (cumpl_s >= .85) "#784212" else "#943126"),
          .kpi_mini("% Margen",   paste0(round(cumpl_m * 100, 1), "%"),
                    if (cumpl_m >= 1) "#1E8449" else if (cumpl_m >= .85) "#784212" else "#943126"),
          .kpi_mini("kWh Prom",   paste0(as.numeric(f$kwh_promedio[[1]]), " kWh"), "#555")
        )
      )
    })

    sel_rest_cols <- TablaReactable(
      id = "demo_rest_cols", data = shiny::reactive(leads),
      col_specs = specs_leads, modo_seleccion = "celda", id_col = "lead_id",
      col_header_n = 2L, sortable = TRUE, page_size = 10,
      cols_activos = c("Editar", "Contacto"), mostrar_badge = TRUE,
      modal_icon = "bolt", modal_size = "m",
      modal_titulo_fn    = function(sel) {
        accion <- switch(sel$col, Editar = "Editar lead", Contacto = "Registrar contacto",
                         paste0("Columna: ", sel$col))
        paste0(accion, " — ", as.character(sel$fila$razon_social[[1]]))
      },
      modal_contenido_fn = function(sel) .modal_accion_lead(sel)
    )
    .render_log(sel_rest_cols$seleccion, output, "log_rest_cols")

    sel_rest_filas <- TablaReactable(
      id = "demo_rest_filas", data = shiny::reactive(clientes),
      columnas = coldefs_clientes, modo_seleccion = "fila", id_col = "cliente_id",
      col_header_n = 2L, sortable = TRUE, page_size = 10, mostrar_badge = TRUE,
      filas_bloqueadas     = c("TOTAL", "OTROS"),
      filas_seleccionables = function(f) isTRUE(as.character(f$segmento[[1]]) == "A")
    )
    .render_log(sel_rest_filas$seleccion, output, "log_rest_filas")
    output$accion_rest_filas <- shiny::renderUI({
      sel <- sel_rest_filas$seleccion()
      if (is.null(sel)) return(shiny::tags$p(shiny::icon("info-circle"),
                                              " Solo segmento A activa la seleccion.",
                                              style = "color:#888; font-size:12px; margin:0;"))
      f <- sel$fila
      shiny::tags$div(
        shiny::tags$div(
          style = paste0("display:inline-block; padding:3px 12px; border-radius:20px;",
                         "background:#EFF6FF; color:#1A5276; font-size:11px;",
                         "font-weight:600; margin-bottom:10px;"),
          "Segmento A — cliente seleccionado"),
        shiny::tags$div(style = "display:flex; gap:10px; flex-wrap:wrap;",
          .kpi_mini("Cliente", as.character(f$razon_social[[1]])),
          .kpi_mini("Asesor",  as.character(f$asesor[[1]])),
          .kpi_mini("Estado",  as.character(f$estado[[1]]),
                    switch(as.character(f$estado[[1]]), "CLIENTE ACTIVO" = "#1A5276",
                           "CLIENTE A RECUPERAR" = "#784212",
                           "NUEVO DEL PERIODO" = "#1E8449", "#333")),
          .kpi_mini("Sacos",  format(as.integer(f$sacos[[1]]), big.mark = ",")),
          .kpi_mini("Margen", paste0("$", format(round(as.numeric(f$margen[[1]])),
                                                 big.mark = ",")), "#1E8449")
        )
      )
    })
  }

  shiny::shinyApp(ui, server)
}
