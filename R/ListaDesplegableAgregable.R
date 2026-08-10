#' UI del módulo ListaDesplegableAgregable
#'
#' Crea los contenedores para una lista desplegable y para el formulario que
#' permite agregar opciones al catálogo vigente de la sesión.
#'
#' @param id String. Identificador del módulo Shiny.
#'
#' @return Un `tagList` con las salidas UI del módulo.
#' @export
ListaDesplegableAgregableUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("picker_ui")),
    shiny::uiOutput(ns("nuevo_ui"))
  )
}


#' Lista desplegable con creación de opciones en la sesión
#'
#' Extiende [racafeShiny::ListaDesplegable()] con la opción `"AGREGAR..."`.
#' Al seleccionarla se muestra un campo de captura, se valida que el valor no
#' esté vacío ni duplicado y se solicita confirmación antes de incorporarlo al
#' catálogo en memoria. Las opciones creadas no se persisten fuera de la sesión.
#'
#' @param id String. Identificador del módulo Shiny.
#' @param label Etiqueta del control.
#' @param choices Vector de opciones iniciales.
#' @param selected Opción u opciones seleccionadas inicialmente.
#' @param multiple Logical. Permite seleccionar varias opciones.
#' @param fem Logical. Se transmite a `ListaDesplegable()` para usar las
#'   etiquetas femeninas correspondientes.
#'
#' @return Lista con los reactivos `seleccion` y `choices`.
#' @export
ListaDesplegableAgregable <- function(
    id,
    label = NULL,
    choices,
    selected = choices,
    multiple = TRUE,
    fem = FALSE
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    .lbl_agregar <- "AGREGAR..."
    .valor_input <- function(x) if (is.null(x)) character(0) else x

    choices_r <- shiny::reactiveVal(choices)
    mostrar_nuevo_r <- shiny::reactiveVal(FALSE)

    output$picker_ui <- shiny::renderUI({
      racafeShiny::ListaDesplegable(
        ns("sel"),
        label = label,
        choices = c(choices_r(), .lbl_agregar),
        selected = selected,
        multiple = multiple,
        fem = fem
      )
    })

    shiny::observeEvent(input$sel, {
      seleccion <- .valor_input(input$sel)
      if (.lbl_agregar %in% seleccion) {
        mostrar_nuevo_r(TRUE)
        shinyWidgets::updatePickerInput(
          session,
          "sel",
          selected = setdiff(seleccion, .lbl_agregar)
        )
      }
    }, ignoreNULL = FALSE)

    output$nuevo_ui <- shiny::renderUI({
      shiny::req(mostrar_nuevo_r())
      shiny::tags$div(
        style = "display:flex;align-items:flex-start;gap:6px;margin-top:6px;",
        shiny::tags$div(
          style = "flex:1;",
          shiny::textInput(
            ns("nuevo_valor"),
            label = NULL,
            placeholder = "Escriba la nueva opción...",
            width = "100%"
          ),
          shiny::uiOutput(ns("error_valor"))
        ),
        racafeShiny::Boton(
          ns("guardar_nuevo"),
          label = NULL,
          icono = "floppy-disk",
          size = "xs",
          title = "Guardar",
          color_fuente = "#C11007",
          color_fondo = "#FFF"
        )
      )
    })

    valor_valido <- shiny::reactive({
      valor <- trimws(if (is.null(input$nuevo_valor)) "" else input$nuevo_valor)
      if (!nzchar(valor)) return(FALSE)
      !toupper(valor) %in% toupper(trimws(choices_r()))
    })

    output$error_valor <- shiny::renderUI({
      shiny::req(mostrar_nuevo_r())
      valor <- trimws(if (is.null(input$nuevo_valor)) "" else input$nuevo_valor)
      if (nzchar(valor) && !valor_valido()) {
        return(shiny::tags$div(
          style = "color:#DA291C;font-size:11px;margin-top:2px;",
          "Esa opción ya existe"
        ))
      }
      NULL
    })

    shiny::observe({
      shiny::req(mostrar_nuevo_r())
      if (isTRUE(valor_valido())) {
        shinyjs::enable("guardar_nuevo")
      } else {
        shinyjs::disable("guardar_nuevo")
      }
    })

    shiny::observeEvent(input$guardar_nuevo, {
      shiny::req(valor_valido())
      racafeShiny::MostrarModalConfirmacion(
        ns = ns,
        titulo = "Confirmar nueva opción",
        texto = paste0(
          "¿Deseas agregar \"", trimws(input$nuevo_valor), "\" a la lista?"
        ),
        id_cancelar = "CancelarNuevo",
        id_confirmar = "ConfirmarNuevo",
        label_confirmar = "Guardar",
        icono_confirmar = "floppy-disk",
        color_confirmar = "#198754"
      )
    })

    shiny::observeEvent(input$CancelarNuevo, {
      shiny::removeModal()
    })

    shiny::observeEvent(input$ConfirmarNuevo, {
      shiny::req(valor_valido())
      nuevo <- trimws(input$nuevo_valor)
      actualizadas <- c(choices_r(), nuevo)
      previa <- setdiff(.valor_input(input$sel), .lbl_agregar)
      nueva_seleccion <- if (isTRUE(multiple)) c(previa, nuevo) else nuevo

      choices_r(actualizadas)
      shinyWidgets::updatePickerInput(
        session,
        "sel",
        choices = c(actualizadas, .lbl_agregar),
        selected = nueva_seleccion
      )
      shiny::updateTextInput(session, "nuevo_valor", value = "")
      mostrar_nuevo_r(FALSE)
      shiny::removeModal()
      shiny::showNotification(
        paste0("Opción \"", nuevo, "\" agregada"),
        duration = 3,
        type = "message"
      )
    })

    list(
      seleccion = shiny::reactive({
        setdiff(.valor_input(input$sel), .lbl_agregar)
      }),
      choices = shiny::reactive(choices_r())
    )
  })
}


#' Demo de ListaDesplegableAgregable
#'
#' Ejecuta una aplicación autocontenida con versiones múltiple y única del
#' módulo, además de una salida que permite inspeccionar selección y catálogo.
#'
#' @return Una aplicación `shiny.appobj`.
#' @export
#' @examples
#' \dontrun{DemoListaDesplegableAgregable()}
DemoListaDesplegableAgregable <- function() {
  .card_codigo <- function(titulo, codigo) {
    bs4Dash::bs4Card(
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      status = "white",
      title = shiny::tagList(shiny::icon("code"), paste(" Código:", titulo)),
      shiny::tags$pre(
        style = paste0(
          "background:#F8F9FA;border:1px solid #DEE2E6;border-radius:4px;",
          "padding:12px;font-size:11px;overflow-x:auto;margin:0;"
        ),
        shiny::tags$code(codigo)
      )
    )
  }

  codigo_multiple <- paste(
    'ListaDesplegableAgregableUI("seg_multiple")',
    "",
    'ListaDesplegableAgregable(',
    '  "seg_multiple", label = "Segmento",',
    '  choices = c("A LA MEDIDA", "CONVENCIONALES", "PREMIUM"),',
    '  selected = NULL, multiple = TRUE',
    ')',
    sep = "\n"
  )
  codigo_single <- paste(
    'ListaDesplegableAgregableUI("cat_single")',
    "",
    'ListaDesplegableAgregable(',
    '  "cat_single", label = "Categoría",',
    '  choices = c("CAFE", "AZUCAR", "PANELA"),',
    '  selected = "CAFE", multiple = FALSE, fem = TRUE',
    ')',
    sep = "\n"
  )

  ui <- bs4Dash::bs4DashPage(
    title = "Demo ListaDesplegableAgregable",
    header = bs4Dash::bs4DashNavbar(title = "Demo ListaDesplegableAgregable"),
    sidebar = bs4Dash::bs4DashSidebar(disable = TRUE),
    footer = bs4Dash::bs4DashFooter(),
    body = bs4Dash::bs4DashBody(
      shinyjs::useShinyjs(),
      shiny::includeCSS(paste0(
        "https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
        "refs/heads/main/Styles/style.css"
      )),
      shiny::fluidRow(
        bs4Dash::bs4Card(
          width = 6,
          collapsible = FALSE,
          title = "Segmento (múltiple)",
          ListaDesplegableAgregableUI("seg_multiple")
        ),
        bs4Dash::bs4Card(
          width = 6,
          collapsible = FALSE,
          title = "Categoría (única)",
          ListaDesplegableAgregableUI("cat_single")
        )
      ),
      shiny::fluidRow(
        bs4Dash::bs4Card(
          width = 12,
          collapsible = FALSE,
          title = "Selección y catálogo en vivo",
          shiny::verbatimTextOutput("resultado")
        )
      ),
      shiny::fluidRow(
        .card_codigo("Picker múltiple con opción de agregar", codigo_multiple),
        .card_codigo("Picker único con opción de agregar", codigo_single)
      )
    )
  )

  server <- function(input, output, session) {
    res_multiple <- ListaDesplegableAgregable(
      "seg_multiple",
      label = "Segmento",
      choices = c("A LA MEDIDA", "CONVENCIONALES", "PREMIUM"),
      selected = NULL,
      multiple = TRUE
    )
    res_single <- ListaDesplegableAgregable(
      "cat_single",
      label = "Categoría",
      choices = c("CAFE", "AZUCAR", "PANELA"),
      selected = "CAFE",
      multiple = FALSE,
      fem = TRUE
    )

    output$resultado <- shiny::renderPrint({
      list(
        seg_seleccion = res_multiple$seleccion(),
        seg_catalogo = res_multiple$choices(),
        cat_seleccion = res_single$seleccion(),
        cat_catalogo = res_single$choices()
      )
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
