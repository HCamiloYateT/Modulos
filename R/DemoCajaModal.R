#' DemoCajaModal: script de app de demostración
#'
#' Este script contiene la función exportada `DemoCajaModal()` y módulos
#' auxiliares internos que ilustran escenarios de uso de `CajaModal()`.
#'
#' @keywords internal

#' Lanza la app de demostracion de CajaModal
#'
#' Ejecuta una aplicacion Shiny interactiva con 12 instancias del modulo
#' [CajaModal()] que cubren todas las variaciones documentadas: valores
#' numericos, semaforos, HTML libre, fondo hex, string plano, y modulos
#' complejos en modal (reactable, plotly, gt, formulario de filtros).
#'
#' Cada caja incluye un panel colapsable con el codigo fuente de sus bloques
#' UI y Server para facilitar la consulta durante el desarrollo.
#'
#' @return Ejecuta `shiny::shinyApp()` (no retorna valor).
#' @export
#'
#' @examples
#' \dontrun{
#' DemoCajaModal()
#' }
DemoCajaModal <- function() {

  # Librerias ----
  library(shiny)
  library(bs4Dash)
  library(reactable)
  library(dplyr)
  library(plotly)
  library(gt)


  # Datos simulados ----

  set.seed(42)
  .clientes <- tibble::tibble(
    cliente_id   = paste0("UC-", 100:124),
    razon_social = paste("Cliente", LETTERS[1:25]),
    asesor       = sample(c("Ana Lopez", "Carlos Ruiz", "Maria Torres"), 25, replace = TRUE),
    segmento     = sample(c("A", "B", "C"), 25, replace = TRUE),
    estado       = sample(
      c("CLIENTE ACTIVO", "CLIENTE A RECUPERAR", "NUEVO DEL PERIODO"),
      25, replace = TRUE, prob = c(0.6, 0.25, 0.15)
    ),
    sacos  = round(runif(25, 500, 8000)),
    margen = round(runif(25, 2e6, 20e6))
  )
  .ind <- list(retencion = 73.4, perdida = 28.6, reactivacion = 85.1)


  # Helper tabla base ----

  .tabla_clientes <- function(data) {
    reactable::reactable(
      data            = data,
      bordered        = TRUE,
      compact         = TRUE,
      highlight       = TRUE,
      searchable      = TRUE,
      defaultPageSize = 10,
      columns = list(
        cliente_id   = reactable::colDef(name = "ID",           minWidth = 80),
        razon_social = reactable::colDef(name = "Razon Social", minWidth = 160),
        asesor       = reactable::colDef(name = "Asesor",       minWidth = 130),
        segmento     = reactable::colDef(name = "Segmento",     minWidth = 90),
        estado       = reactable::colDef(
          name     = "Estado",
          minWidth = 160,
          style    = function(value) {
            bg <- switch(value,
              "CLIENTE ACTIVO"      = "#EFF6FF",
              "CLIENTE A RECUPERAR" = "#FFF8EC",
              "NUEVO DEL PERIODO"   = "#EDFBF2",
              "white"
            )
            list(background = bg, fontWeight = "500")
          }
        ),
        sacos  = reactable::colDef(
          name = "Sacos", minWidth = 90,
          cell = function(v) format(v, big.mark = ",")
        ),
        margen = reactable::colDef(
          name = "Margen", minWidth = 110,
          cell = function(v) format(v, big.mark = ",")
        )
      ),
      theme = reactable::reactableTheme(
        headerStyle = list(fontWeight = "600", fontSize = "12px"),
        cellStyle   = list(fontSize = "12px")
      )
    )
  }


  # Modulo ModTablaFiltrable ----
  # Reactable con filtros de estado y asesor sincronizados desde el padre

  ModTablaFiltrableUI <- function(id) {
    ns <- NS(id)
    tagList(
      fluidRow(
        column(6, selectInput(ns("sel_estado"), "Estado", width = "100%",
          choices = c(
            "Todos" = "", "CLIENTE ACTIVO", "CLIENTE A RECUPERAR", "NUEVO DEL PERIODO"
          )
        )),
        column(6, selectInput(ns("sel_asesor"), "Asesor", width = "100%",
          choices = c("Todos" = "", "Ana Lopez", "Carlos Ruiz", "Maria Torres")
        ))
      ),
      reactable::reactableOutput(ns("tabla")),
      uiOutput(ns("resumen"))
    )
  }

  ModTablaFiltrable <- function(id, data, estado_r = reactive(""), asesor_r = reactive("")) {
    moduleServer(id, function(input, output, session) {
      observe({ updateSelectInput(session, "sel_estado", selected = estado_r()) })
      observe({ updateSelectInput(session, "sel_asesor", selected = asesor_r()) })

      # Filtrado reactivo por estado y asesor
      datos_filtrados <- reactive({
        df <- data()
        if (nzchar(input$sel_estado)) df <- df %>% filter(estado == input$sel_estado)
        if (nzchar(input$sel_asesor)) df <- df %>% filter(asesor == input$sel_asesor)
        df
      })

      output$tabla   <- reactable::renderReactable({ .tabla_clientes(datos_filtrados()) })
      output$resumen <- renderUI({
        p(icon("filter"),
          sprintf(" %d de %d UCs segun filtros activos", nrow(datos_filtrados()), nrow(data())),
          style = "font-size:11px; color:#888; margin-top:6px;")
      })
    })
  }


  # Modulo ModGraficoPlotly ----
  # Barras de sacos por asesor con opacidad diferencial por segmento

  ModGraficoPlotlyUI <- function(id) {
    ns <- NS(id)
    tagList(
      fluidRow(
        column(4, selectInput(ns("sel_seg"), "Segmento a resaltar",
          choices = c("Todos" = "", "A", "B", "C"), width = "100%"
        )),
        column(8, p(
          style = "font-size:11px; color:#888; margin-top:28px;",
          "Barras de sacos acumulados por asesor. Segmento resaltado en color."
        ))
      ),
      plotly::plotlyOutput(ns("grafico"), height = "320px")
    )
  }

  ModGraficoPlotly <- function(id, data, segmento_r = reactive("")) {
    moduleServer(id, function(input, output, session) {
      observe({ updateSelectInput(session, "sel_seg", selected = segmento_r()) })

      # Agrupacion y calculo de opacidad por segmento seleccionado
      datos_plot <- reactive({
        data() %>%
          group_by(asesor, segmento) %>%
          summarise(sacos = sum(sacos), .groups = "drop") %>%
          mutate(
            resaltado = if (nzchar(input$sel_seg)) segmento == input$sel_seg else TRUE,
            alpha_val = ifelse(resaltado, 1, 0.3)
          )
      })

      output$grafico <- plotly::renderPlotly({
        plotly::plot_ly(
          data      = datos_plot(),
          x         = ~asesor,
          y         = ~sacos,
          color     = ~segmento,
          colors    = c(A = "#2980B9", B = "#27AE60", C = "#E67E22"),
          type      = "bar",
          opacity   = ~alpha_val,
          text      = ~paste0(segmento, ": ", format(sacos, big.mark = ",")),
          hoverinfo = "text"
        ) %>%
          plotly::layout(
            barmode = "stack",
            xaxis   = list(title = "Asesor"),
            yaxis   = list(title = "Sacos acumulados"),
            legend  = list(orientation = "h", y = -0.2),
            margin  = list(t = 20, b = 60)
          )
      })
    })
  }


  # Modulo ModTablaGt ----
  # Tabla gt con top N clientes por margen y fondo condicional por segmento

  ModTablaGtUI <- function(id) {
    ns <- NS(id)
    tagList(
      fluidRow(
        column(4, numericInput(ns("top_n"), "Top N por margen",
          value = 10, min = 1, max = 25, width = "100%"
        )),
        column(8, p(
          style = "font-size:11px; color:#888; margin-top:28px;",
          "Clientes ordenados por margen descendente. Fondo condicional por segmento."
        ))
      ),
      gt::gt_output(ns("tabla_gt"))
    )
  }

  ModTablaGt <- function(id, data, top_n_r = reactive(10L)) {
    moduleServer(id, function(input, output, session) {
      observe({ updateNumericInput(session, "top_n", value = top_n_r()) })

      # Seleccion de top N por margen descendente
      datos_gt <- reactive({
        n <- max(1L, as.integer(input$top_n %||% top_n_r()))
        data() %>%
          arrange(desc(margen)) %>%
          slice_head(n = n) %>%
          select(cliente_id, razon_social, asesor, segmento, sacos, margen)
      })

      output$tabla_gt <- gt::render_gt({
        datos_gt() %>%
          gt::gt() %>%
          gt::cols_label(
            cliente_id   ~ "ID",
            razon_social ~ "Razon Social",
            asesor       ~ "Asesor",
            segmento     ~ "Segmento",
            sacos        ~ "Sacos",
            margen       ~ "Margen"
          ) %>%
          gt::fmt_number(columns = sacos,  decimals = 0, sep_mark = ",") %>%
          gt::fmt_number(columns = margen, decimals = 0, sep_mark = ",") %>%
          gt::tab_style(
            style     = gt::cell_fill(color = "#EBF5FB"),
            locations = gt::cells_body(rows = segmento == "A")
          ) %>%
          gt::tab_style(
            style     = gt::cell_fill(color = "#EAFAF1"),
            locations = gt::cells_body(rows = segmento == "B")
          ) %>%
          gt::tab_style(
            style     = gt::cell_fill(color = "#FEF9E7"),
            locations = gt::cells_body(rows = segmento == "C")
          ) %>%
          gt::tab_options(
            table.font.size              = 12,
            column_labels.font.weight    = "bold",
            table.width                  = gt::pct(100)
          )
      })
    })
  }


  # Modulo ModFormulario ----
  # Formulario de filtros con resumen de confirmacion reactivo

  ModFormularioUI <- function(id) {
    ns <- NS(id)
    tagList(
      fluidRow(
        column(6, selectInput(ns("f_segmento"), "Segmento",
          choices = c("Todos" = "", "A", "B", "C"), width = "100%"
        )),
        column(6, selectInput(ns("f_asesor"), "Asesor",
          choices = c("Todos" = ""), width = "100%"
        ))
      ),
      fluidRow(
        column(6, sliderInput(ns("f_sacos"), "Rango de sacos",
          min = 0, max = 10000, value = c(0, 10000), step = 100, width = "100%"
        )),
        column(6, sliderInput(ns("f_margen"), "Rango de margen (millones)",
          min = 0, max = 25, value = c(0, 25), step = 1, width = "100%"
        ))
      ),
      actionButton(ns("btn_confirmar"), "Confirmar seleccion", icon = icon("check"),
        style = "background-color:#1A5276; color:white; border:none; margin-top:8px;"
      ),
      hr(),
      uiOutput(ns("resumen_confirmado"))
    )
  }

  ModFormulario <- function(id, asesores_r = reactive(character(0)),
                            segmentos_r = reactive(character(0))) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      observe({ updateSelectInput(session, "f_asesor", choices = c("Todos" = "", asesores_r())) })

      # Resumen confirmado almacenado como reactiveVal
      resumen_rv <- reactiveVal(NULL)
      observeEvent(input$btn_confirmar, {
        resumen_rv(list(
          segmento = if (nzchar(input$f_segmento)) input$f_segmento else "Todos",
          asesor   = if (nzchar(input$f_asesor))   input$f_asesor   else "Todos",
          sacos    = input$f_sacos,
          margen   = input$f_margen
        ))
      })

      output$resumen_confirmado <- renderUI({
        r <- resumen_rv()
        if (is.null(r)) {
          return(p(
            icon("info-circle"),
            " Complete los campos y confirme para ver el resumen.",
            style = "font-size:12px; color:#888;"
          ))
        }
        tagList(
          h6(icon("check-circle", style = "color:#1A5276;"),
            " Resumen de seleccion confirmada",
            style = "color:#1A5276; margin-bottom:8px;"
          ),
          tags$ul(style = "font-size:13px; color:#333; line-height:2;",
            tags$li(tags$strong("Segmento: "), r$segmento),
            tags$li(tags$strong("Asesor: "),   r$asesor),
            tags$li(tags$strong("Sacos: "),
              paste0(format(r$sacos[1], big.mark = ","), " — ", format(r$sacos[2], big.mark = ","))
            ),
            tags$li(tags$strong("Margen: "), paste0("$", r$margen[1], "M — $", r$margen[2], "M"))
          )
        )
      })
    })
  }


  # Helper codigo fuente ----
  # Genera un panel colapsable con los bloques UI y Server de cada caja

  .bloque_codigo <- function(codigo_ui, codigo_server) {
    .pre <- function(txt) {
      tags$pre(
        style = paste(
          "background:#F4F6F7; border:1px solid #D5D8DC; border-radius:4px;",
          "font-size:11px; padding:10px; max-height:260px;",
          "overflow-y:auto; overflow-x:auto; white-space:pre;"
        ),
        txt
      )
    }
    bs4Dash::bs4Card(
      width       = 12,
      collapsed   = TRUE,
      collapsible = TRUE,
      status      = "secondary",
      solidHeader = FALSE,
      title       = tagList(icon("code"), " Ver codigo fuente"),
      fluidRow(
        column(6, tags$b("UI"),     .pre(codigo_ui)),
        column(6, tags$b("Server"), .pre(codigo_server))
      )
    )
  }


  # Codigo fuente por caja ----
  # Lista de strings literales con el bloque UI y Server de cada variante

  .codigo <- list(

    kpi_1 = list(
      ui = 'CajaModalUI("kpi_1")',
      server = 'CajaModal("kpi_1",
  valor = nrow(.clientes), formato = "numero",
  texto = "Total Unidades Comerciales", icono = "users",
  colores = c(fondo = "white"), mostrar_boton = TRUE,
  titulo_modal = "Detalle — Todas las Unidades Comerciales",
  icono_modal = "users",
  contenido_modal = function() tagList(
    .tabla_clientes(.clientes),
    p(icon("hand-pointer"), " Navegue por la tabla",
      style = "font-size:11px; color:#888;")
  )
)'
    ),

    kpi_2 = list(
      ui = 'CajaModalUI("kpi_2")',
      server = 'CajaModal("kpi_2",
  valor = n_activos, formato = "numero",
  texto = "Clientes Activos", icono = "check-circle",
  colores = c(fondo = "white"), mostrar_boton = FALSE,
  footer = paste0(n_activos, " de ", nrow(.clientes), " UCs en estado activo"),
  footer_class = "caja-modal-footer"
)'
    ),

    kpi_3 = list(
      ui = 'CajaModalUI("kpi_3")',
      server = 'CajaModal("kpi_3",
  valor  = html_valor(.ind$reactivacion, formato = "porcentaje",
             color = racafe::col_kpi(.ind$reactivacion, prop = TRUE, ref = 100)),
  formato = "porcentaje",
  texto  = html_texto("Reactivacion (A Recuperar -> Activo)",
             color = racafe::col_kpi(.ind$reactivacion, prop = TRUE, ref = 100)),
  icono = "sync-alt", colores = c(fondo = "white"), mostrar_boton = FALSE,
  footer = "A Recuperar t que pasan a Activos en t+1 / Total A Recuperar en t",
  footer_class = "caja-modal-footer"
)'
    ),

    kpi_4 = list(
      ui = 'CajaModalUI("kpi_4")',
      server = 'CajaModal("kpi_4",
  valor  = html_valor(.ind$retencion, formato = "porcentaje",
             color = racafe::col_kpi(.ind$retencion, prop = TRUE, ref = 100)),
  formato = "porcentaje",
  texto  = html_texto("Retencion (Activo -> Activo)",
             color = racafe::col_kpi(.ind$retencion, prop = TRUE, ref = 100)),
  icono = "check-double", colores = c(fondo = "white"), mostrar_boton = FALSE,
  footer = "Activos t que permanecen Activos en t+1 / Total Activos en t",
  footer_class = "caja-modal-footer-warning"
)'
    ),

    kpi_5 = list(
      ui = 'CajaModalUI("kpi_5")',
      server = 'CajaModal("kpi_5",
  valor  = html_valor(.ind$perdida, formato = "porcentaje",
             color = racafe::col_kpi(.ind$perdida, prop = FALSE)),
  formato = "porcentaje",
  texto  = html_texto("Perdida Dinamica (Activo -> A Recuperar)",
             color = racafe::col_kpi(.ind$perdida, prop = FALSE)),
  icono = "arrow-down", colores = c(fondo = "white"), mostrar_boton = TRUE,
  titulo_modal = "Detalle — Clientes en riesgo de perdida",
  icono_modal  = "arrow-down",
  contenido_modal = function() tagList(
    .tabla_clientes(.clientes %>% filter(estado == "CLIENTE A RECUPERAR")),
    p(icon("info-circle"), " UCs clasificadas como A Recuperar",
      style = "font-size:11px; color:#888;")
  ),
  footer = reactive(paste0("Corte al ", format(Sys.Date(), "%d/%m/%Y"))),
  footer_class = "caja-modal-footer-warning"
)'
    ),

    kpi_6 = list(
      ui = 'CajaModalUI("kpi_6")',
      server = 'CajaModal("kpi_6",
  valor = tags$span(
    tags$span(style = "font-size:2em; font-weight:700; color:#8E44AD;", n_nuevos),
    tags$span(style = "background:#8E44AD; color:white; border-radius:4px;
               font-size:0.7em; padding:1px 6px; margin-left:4px;", "NEW")
  ),
  formato = "numero",
  texto = htmltools::HTML(as.character(tagList(
    icon("star", style = "color:#8E44AD; margin-right:4px;"),
    tags$span(style = "color:#8E44AD; font-size:1.1em;", "Altas en Cohorte")
  ))),
  icono = "user-plus", colores = c(fondo = "white"), mostrar_boton = TRUE,
  titulo_modal = "Detalle — Altas en Cohorte", icono_modal = "user-plus",
  contenido_modal = function() tagList(
    .tabla_clientes(.clientes %>% filter(estado == "NUEVO DEL PERIODO")),
    p(icon("hand-pointer"), " Clientes que ingresan por primera vez",
      style = "font-size:11px; color:#888;")
  ),
  footer = "Clientes sin historial previo que facturan por primera vez",
  footer_class = "caja-modal-footer"
)'
    ),

    kpi_7 = list(
      ui = 'CajaModalUI("kpi_7")',
      server = 'CajaModal("kpi_7",
  valor = html_valor(n_activos, formato = "numero", color = "#1A5276"),
  formato = "numero",
  texto = html_texto("Clientes Activos", color = "#1A5276"),
  icono = "check-circle", colores = c(fondo = "white"),
  color_fondo_hex = "#D6EAF8",
  mostrar_boton = TRUE, titulo_modal = "Detalle — Clientes Activos",
  icono_modal = "check-circle",
  contenido_modal = function() tagList(
    .tabla_clientes(.clientes %>% filter(estado == "CLIENTE ACTIVO")),
    p(icon("hand-pointer"), " Solo UCs en estado Activo",
      style = "font-size:11px; color:#888;")
  )
)'
    ),

    kpi_8 = list(
      ui = 'CajaModalUI("kpi_8")',
      server = 'CajaModal("kpi_8",
  valor = paste0(n_recuperar, " / ", nrow(.clientes)),
  formato = "numero", texto = "A Recuperar / Total",
  icono = "exclamation-triangle", colores = c(fondo = "white"),
  mostrar_boton = FALSE,
  footer = paste0(round(n_recuperar / nrow(.clientes) * 100, 1),
                  "% de la poblacion requiere gestion activa"),
  footer_class = "caja-modal-footer-dark"
)'
    ),

    kpi_9 = list(
      ui = 'CajaModalUI("kpi_9")',
      server = '# Registro previo en server (obligatorio antes del click):
ModTablaFiltrable("mod_tabla", data = clientes_r,
  estado_r = reactive("CLIENTE ACTIVO"), asesor_r = reactive(""))

CajaModal("kpi_9",
  valor = n_activos, formato = "numero",
  texto = "Activos — tabla filtrable", icono = "table",
  colores = c(fondo = "white"), mostrar_boton = TRUE,
  titulo_modal = "Clientes — Tabla con filtros reactivos",
  icono_modal = "table", tamano_modal = "xl",
  contenido_modal = function() ModTablaFiltrableUI("mod_tabla"),
  footer = "Filtros de estado y asesor reactivos desde el padre",
  footer_class = "caja-modal-footer"
)'
    ),

    kpi_10 = list(
      ui = 'CajaModalUI("kpi_10")',
      server = '# Registro previo en server (obligatorio antes del click):
ModGraficoPlotly("mod_plotly", data = clientes_r, segmento_r = reactive("A"))

CajaModal("kpi_10",
  valor  = html_valor(sum(.clientes$sacos), formato = "numero", color = "#2980B9"),
  formato = "numero",
  texto  = html_texto("Sacos totales — grafico por asesor", color = "#2980B9"),
  icono = "chart-bar", colores = c(fondo = "white"), mostrar_boton = TRUE,
  titulo_modal = "Distribucion de sacos por asesor y segmento",
  icono_modal = "chart-bar", tamano_modal = "xl",
  contenido_modal = function() ModGraficoPlotlyUI("mod_plotly"),
  footer = "Segmento A preseleccionado — modificable en el modal",
  footer_class = "caja-modal-footer"
)'
    ),

    kpi_11 = list(
      ui = 'CajaModalUI("kpi_11")',
      server = '# Registro previo en server (obligatorio antes del click):
ModTablaGt("mod_gt", data = clientes_r, top_n_r = reactive(10L))

CajaModal("kpi_11",
  valor  = html_valor(max(.clientes$margen), formato = "numero", color = "#1A5276"),
  formato = "numero",
  texto  = html_texto("Margen maximo — top clientes gt", color = "#1A5276"),
  icono = "star", colores = c(fondo = "white"), color_fondo_hex = "#EBF5FB",
  mostrar_boton = TRUE, titulo_modal = "Top clientes por margen — tabla gt",
  icono_modal = "star", tamano_modal = "l",
  contenido_modal = function() ModTablaGtUI("mod_gt"),
  footer = "Top 10 por defecto — ajustable en el modal",
  footer_class = "caja-modal-footer"
)'
    ),

    kpi_12 = list(
      ui = 'CajaModalUI("kpi_12")',
      server = '# Registro previo en server (obligatorio antes del click):
ModFormulario("mod_form", asesores_r = asesores_r, segmentos_r = segmentos_r)

CajaModal("kpi_12",
  valor  = html_valor(length(unique(.clientes$segmento)), formato = "numero", color = "#6C3483"),
  formato = "numero",
  texto  = html_texto("Segmentos — formulario de filtros", color = "#6C3483"),
  icono = "sliders-h", colores = c(fondo = "white"), mostrar_boton = TRUE,
  titulo_modal = "Configuracion de filtros de segmento",
  icono_modal = "sliders-h", tamano_modal = "l",
  contenido_modal = function() ModFormularioUI("mod_form"),
  footer = "Confirmar para ver resumen de seleccion",
  footer_class = "caja-modal-footer"
)'
    )
  )


  # Helper etiqueta de caja ----

  .h6_label <- function(txt) h6(txt, style = "color:#555; margin-bottom:4px;")

  # Helper columna con caja y codigo ----
  # Agrupa CajaModalUI + panel de codigo en una sola columna para la UI

  .col_caja <- function(id, label) {
    column(3,
      .h6_label(label),
      CajaModalUI(id),
      .bloque_codigo(.codigo[[id]]$ui, .codigo[[id]]$server)
    )
  }


  # UI ----

  ui <- bs4Dash::bs4DashPage(
    title  = "Demo CajaModal",
    header  = bs4Dash::bs4DashNavbar(title = "Demo — CajaModal", skin = "dark"),
    sidebar = bs4Dash::bs4DashSidebar(disable = TRUE),
    body = bs4Dash::bs4DashBody(
      includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
      # Fila 1: numericos y semaforos
      bs4Dash::bs4Card(
        title       = "Fila 1 — Numericos y semaforos",
        width       = 12,
        solidHeader = TRUE,
        status      = "white",
        collapsible = TRUE,
        fluidRow(
          .col_caja("kpi_1", "Caja 1 — Numerico + modal"),
          .col_caja("kpi_2", "Caja 2 — Numerico sin modal"),
          .col_caja("kpi_3", "Caja 3 — Semaforo reactivacion"),
          .col_caja("kpi_4", "Caja 4 — Semaforo retencion")
        )
      ),

      # Fila 2: HTML avanzado y variaciones de fondo
      bs4Dash::bs4Card(
        title       = "Fila 2 — HTML avanzado y variaciones de fondo",
        width       = 12,
        solidHeader = TRUE,
        status      = "white",
        collapsible = TRUE,
        fluidRow(
          .col_caja("kpi_5", "Caja 5 — Semaforo perdida + modal"),
          .col_caja("kpi_6", "Caja 6 — HTML libre + modal"),
          .col_caja("kpi_7", "Caja 7 — Fondo hex + modal"),
          .col_caja("kpi_8", "Caja 8 — String plano sin modal")
        )
      ),

      # Fila 3: modulos complejos en modal
      bs4Dash::bs4Card(
        title       = "Fila 3 — Modulos complejos en modal",
        width       = 12,
        solidHeader = TRUE,
        status      = "white",
        collapsible = TRUE,
        fluidRow(
          .col_caja("kpi_9",  "Caja 9 — ModTablaFiltrable"),
          .col_caja("kpi_10", "Caja 10 — ModGraficoPlotly"),
          .col_caja("kpi_11", "Caja 11 — ModTablaGt"),
          .col_caja("kpi_12", "Caja 12 — ModFormulario")
        )
      )
    ),
    footer = bs4Dash::bs4DashFooter(left = "Demo CajaModal", right = "2025")
  )


  # Server ----

  server <- function(input, output, session) {

    # Reactivos compartidos con modulos de modal
    clientes_r  <- reactive(.clientes)
    asesores_r  <- reactive(sort(unique(.clientes$asesor)))
    segmentos_r <- reactive(sort(unique(.clientes$segmento)))

    n_activos   <- sum(.clientes$estado == "CLIENTE ACTIVO")
    n_recuperar <- sum(.clientes$estado == "CLIENTE A RECUPERAR")
    n_nuevos    <- sum(.clientes$estado == "NUEVO DEL PERIODO")

    # Registro de modulos de modal — debe ocurrir antes del primer click
    ModTablaFiltrable("mod_tabla",  data = clientes_r,
      estado_r = reactive("CLIENTE ACTIVO"), asesor_r = reactive(""))
    ModGraficoPlotly( "mod_plotly", data = clientes_r, segmento_r = reactive("A"))
    ModTablaGt(       "mod_gt",     data = clientes_r, top_n_r    = reactive(10L))
    ModFormulario(    "mod_form",   asesores_r = asesores_r, segmentos_r = segmentos_r)

    # Cajas 1-8: variaciones base ----

    CajaModal("kpi_1",
      valor = nrow(.clientes), formato = "numero",
      texto = "Total Unidades Comerciales", icono = "users",
      colores = c(fondo = "white"), mostrar_boton = TRUE,
      titulo_modal = "Detalle — Todas las Unidades Comerciales",
      icono_modal  = "users",
      contenido_modal = function() tagList(
        .tabla_clientes(.clientes),
        p(icon("hand-pointer"), " Navegue por la tabla para ver el detalle",
          style = "font-size:11px; color:#888; margin-top:6px;")
      )
    )

    CajaModal("kpi_2",
      valor = n_activos, formato = "numero",
      texto = "Clientes Activos", icono = "check-circle",
      colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer       = paste0(n_activos, " de ", nrow(.clientes), " UCs en estado activo"),
      footer_class = "caja-modal-footer"
    )

    CajaModal("kpi_3",
      valor  = html_valor(.ind$reactivacion, formato = "porcentaje",
                 color = racafe::col_kpi(.ind$reactivacion, prop = TRUE, ref = 100)),
      formato = "porcentaje",
      texto  = html_texto("Reactivacion (A Recuperar -> Activo)",
                 color = racafe::col_kpi(.ind$reactivacion, prop = TRUE, ref = 100)),
      icono = "sync-alt", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer       = "A Recuperar t que pasan a Activos en t+1 / Total A Recuperar en t",
      footer_class = "caja-modal-footer"
    )

    CajaModal("kpi_4",
      valor  = html_valor(.ind$retencion, formato = "porcentaje",
                 color = racafe::col_kpi(.ind$retencion, prop = TRUE, ref = 100)),
      formato = "porcentaje",
      texto  = html_texto("Retencion (Activo -> Activo)",
                 color = racafe::col_kpi(.ind$retencion, prop = TRUE, ref = 100)),
      icono = "check-double", colores = c(fondo = "white"), mostrar_boton = FALSE,
      footer       = "Activos t que permanecen Activos en t+1 / Total Activos en t",
      footer_class = "caja-modal-footer-warning"
    )

    CajaModal("kpi_5",
      valor  = html_valor(.ind$perdida, formato = "porcentaje",
                 color = racafe::col_kpi(.ind$perdida, prop = FALSE)),
      formato = "porcentaje",
      texto  = html_texto("Perdida Dinamica (Activo -> A Recuperar)",
                 color = racafe::col_kpi(.ind$perdida, prop = FALSE)),
      icono = "arrow-down", colores = c(fondo = "white"), mostrar_boton = TRUE,
      titulo_modal = "Detalle — Clientes en riesgo de perdida",
      icono_modal  = "arrow-down",
      contenido_modal = function() tagList(
        .tabla_clientes(.clientes %>% filter(estado == "CLIENTE A RECUPERAR")),
        p(icon("info-circle"), " UCs clasificadas como A Recuperar",
          style = "font-size:11px; color:#888; margin-top:6px;")
      ),
      footer       = reactive(paste0("Corte al ", format(Sys.Date(), "%d/%m/%Y"))),
      footer_class = "caja-modal-footer-warning"
    )

    CajaModal("kpi_6",
      valor = tags$span(
        tags$span(style = "font-size:2em; font-weight:700; color:#8E44AD;", n_nuevos),
        tags$span(
          style = paste("display:inline-block; background:#8E44AD; color:white;",
            "border-radius:4px; font-size:0.7em; padding:1px 6px; margin-left:4px;"),
          "NEW"
        )
      ),
      formato = "numero",
      texto = htmltools::HTML(as.character(tagList(
        icon("star", style = "color:#8E44AD; margin-right:4px;"),
        tags$span(style = "color:#8E44AD; font-size:1.1em;", "Altas en Cohorte")
      ))),
      icono = "user-plus", colores = c(fondo = "white"), mostrar_boton = TRUE,
      titulo_modal = "Detalle — Altas en Cohorte", icono_modal = "user-plus",
      contenido_modal = function() tagList(
        .tabla_clientes(.clientes %>% filter(estado == "NUEVO DEL PERIODO")),
        p(icon("hand-pointer"), " Clientes que ingresan por primera vez al periodo",
          style = "font-size:11px; color:#888; margin-top:6px;")
      ),
      footer       = "Clientes sin historial previo que facturan por primera vez",
      footer_class = "caja-modal-footer"
    )

    CajaModal("kpi_7",
      valor = html_valor(n_activos, formato = "numero", color = "#1A5276"),
      formato = "numero",
      texto = html_texto("Clientes Activos", color = "#1A5276"),
      icono = "check-circle", colores = c(fondo = "white"),
      color_fondo_hex = "#D6EAF8", mostrar_boton = TRUE,
      titulo_modal = "Detalle — Clientes Activos", icono_modal = "check-circle",
      contenido_modal = function() tagList(
        .tabla_clientes(.clientes %>% filter(estado == "CLIENTE ACTIVO")),
        p(icon("hand-pointer"), " Solo UCs en estado Activo",
          style = "font-size:11px; color:#888; margin-top:6px;")
      )
    )

    CajaModal("kpi_8",
      valor = paste0(n_recuperar, " / ", nrow(.clientes)),
      formato = "numero", texto = "A Recuperar / Total",
      icono = "exclamation-triangle", colores = c(fondo = "white"),
      mostrar_boton = FALSE,
      footer = paste0(
        round(n_recuperar / nrow(.clientes) * 100, 1),
        "% de la poblacion requiere gestion activa"
      ),
      footer_class = "caja-modal-footer-dark"
    )

    # Cajas 9-12: modulos complejos en modal ----

    CajaModal("kpi_9",
      valor = n_activos, formato = "numero",
      texto = "Activos — tabla filtrable", icono = "table",
      colores = c(fondo = "white"), mostrar_boton = TRUE,
      titulo_modal = "Clientes — Tabla con filtros reactivos",
      icono_modal  = "table", tamano_modal = "xl",
      contenido_modal = function() ModTablaFiltrableUI("mod_tabla"),
      footer       = "Filtros de estado y asesor reactivos desde el padre",
      footer_class = "caja-modal-footer"
    )

    CajaModal("kpi_10",
      valor  = html_valor(sum(.clientes$sacos), formato = "numero", color = "#2980B9"),
      formato = "numero",
      texto  = html_texto("Sacos totales — grafico por asesor", color = "#2980B9"),
      icono = "chart-bar", colores = c(fondo = "white"), mostrar_boton = TRUE,
      titulo_modal = "Distribucion de sacos por asesor y segmento",
      icono_modal  = "chart-bar", tamano_modal = "xl",
      contenido_modal = function() ModGraficoPlotlyUI("mod_plotly"),
      footer       = "Segmento A preseleccionado — modificable en el modal",
      footer_class = "caja-modal-footer"
    )

    CajaModal("kpi_11",
      valor  = html_valor(max(.clientes$margen), formato = "numero", color = "#1A5276"),
      formato = "numero",
      texto  = html_texto("Margen maximo — top clientes gt", color = "#1A5276"),
      icono = "star", colores = c(fondo = "white"), color_fondo_hex = "#EBF5FB",
      mostrar_boton = TRUE,
      titulo_modal  = "Top clientes por margen — tabla gt",
      icono_modal   = "star", tamano_modal = "l",
      contenido_modal = function() ModTablaGtUI("mod_gt"),
      footer       = "Top 10 por defecto — ajustable en el modal",
      footer_class = "caja-modal-footer"
    )

    CajaModal("kpi_12",
      valor  = html_valor(length(unique(.clientes$segmento)), formato = "numero", color = "#6C3483"),
      formato = "numero",
      texto  = html_texto("Segmentos — formulario de filtros", color = "#6C3483"),
      icono = "sliders-h", colores = c(fondo = "white"), mostrar_boton = TRUE,
      titulo_modal = "Configuracion de filtros de segmento",
      icono_modal  = "sliders-h", tamano_modal = "l",
      contenido_modal = function() ModFormularioUI("mod_form"),
      footer       = "Confirmar para ver resumen de seleccion",
      footer_class = "caja-modal-footer"
    )
  }


  # App ----

  shiny::shinyApp(ui, server)
}
