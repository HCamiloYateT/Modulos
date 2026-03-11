# CajaModalUI ----

#' UI del modulo CajaModal
#'
#' Debe ubicarse en el cuerpo de la UI del modulo padre. Incluye tres salidas:
#' el estilo CSS opcional para fondo hex, el ValueBox renderizado y el footer
#' opcional debajo de la caja.
#'
#' @param id String. ID del modulo Shiny. Debe coincidir con el `id` usado
#'   en [CajaModal()].
#'
#' @return `tagList` con los tres `uiOutput` / `bs4ValueBoxOutput` del modulo.
#' @export
#'
#' @examples
#' # En la UI del padre:
#' CajaModalUI("mi_kpi")
CajaModalUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("fondo_hex_style")),
    bs4Dash::bs4ValueBoxOutput(ns("caja"), width = 12),
    shiny::uiOutput(ns("footer"))
  )
}


# CajaModal ----

#' Server del modulo CajaModal
#'
#' Encapsula un `bs4ValueBox` con la capacidad de abrir un modal al hacer
#' click. El contenido del modal es arbitrario y se define desde el modulo
#' padre como una funcion sin argumentos evaluada en el momento del click.
#'
#' @param id String. ID del modulo Shiny.
#' @param valor Reactive o escalar. Numerico, string, `shiny.tag` o
#'   `htmltools::HTML()` a mostrar como valor principal de la caja.
#' @param formato String. Formato del valor cuando es numerico. Uno de
#'   `"numero"` (default), `"porcentaje"`, `"moneda"`. Solo aplica si
#'   `valor` es numerico puro.
#' @param texto Reactive o escalar. String, `htmltools::HTML()` o `shiny.tag`
#'   para el subtitulo de la caja. Los `shiny.tag` se convierten
#'   automaticamente. Default `""`.
#' @param icono String. Nombre de icono Font Awesome. Default `"info-circle"`.
#' @param colores Reactive o vector nombrado. Tokens de color bs4Dash para
#'   el fondo del box (ej: `c(fondo = "white")`). Para color hex usar
#'   `color_fondo_hex`. Default `c(fondo = "white")`.
#' @param color_fondo_hex Reactive o string. Color hex CSS para el fondo del
#'   box. Se inyecta como `<style>` en un `uiOutput` independiente porque
#'   `renderbs4ValueBox` exige `shiny.tag` puro. `NULL` = ignorado (default).
#' @param mostrar_boton Logical. `TRUE` activa el boton "Ver detalle".
#'   Default `TRUE`.
#' @param contenido_modal Function o `NULL`. Funcion sin argumentos que
#'   retorna la UI del modal. Se evalua al momento del click, no al iniciar
#'   el modulo. Default `NULL`.
#' @param titulo_modal Reactive o string. Titulo del encabezado del modal.
#'   Default `""`.
#' @param icono_modal String. Icono Font Awesome del encabezado del modal.
#'   Default `"info-circle"`.
#' @param tamano_modal String. Tamano del modal. Uno de `"s"`, `"m"`, `"l"`,
#'   `"xl"`. Default `"xl"`.
#' @param footer Reactive, string o `NULL`. Texto debajo de la caja.
#'   `NULL` o cadena vacia omite el footer. Default `NULL`.
#' @param footer_class String. Clase CSS del parrafo footer. Debe estar
#'   definida en el `style.css` del proyecto. Default `"caja-modal-footer"`.
#'
#' @return Nada (side-effects de Shiny moduleServer).
#' @export
#'
#' @examples
#' # En la UI:
#' # CajaModalUI("mi_kpi")
#'
#' # En el server:
#' # CajaModal(
#' #   id    = "mi_kpi",
#' #   valor = reactive(nrow(datos())),
#' #   texto = "Total clientes",
#' #   icono = "users",
#' #   contenido_modal = function() reactable::reactableOutput("mi_tabla")
#' # )
CajaModal <- function(
    id,
    valor,
    formato          = "numero",
    texto            = "",
    icono            = "info-circle",
    colores          = c(fondo = "white"),
    color_fondo_hex  = NULL,
    mostrar_boton    = TRUE,
    contenido_modal  = NULL,
    titulo_modal     = "",
    icono_modal      = "info-circle",
    tamano_modal     = "xl",
    footer           = NULL,
    footer_class     = "caja-modal-footer"
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helpers internos ----
    # Resuelve reactivos y escalares de forma uniforme
    .val <- function(x) if (shiny::is.reactive(x)) x() else x

    # Convierte shiny.tag/tagList a HTML para satisfacer la validacion de
    # CajaValor que exige character de longitud 1
    .normalizar_texto <- function(x) {
      v <- .val(x)
      if (inherits(v, c("shiny.tag", "shiny.tag.list"))) {
        return(htmltools::HTML(as.character(v)))
      }
      v
    }

    # CSS fondo hex ----
    # renderbs4ValueBox exige shiny.tag puro — no acepta tagList con <style>.
    # El CSS se inyecta via uiOutput independiente con selector escopado al
    # id del output: #ns("caja") .small-box
    output$fondo_hex_style <- shiny::renderUI({
      hex <- .val(color_fondo_hex)
      if (is.null(hex) || !nzchar(hex)) return(NULL)
      shiny::tags$style(paste0(
        "#", ns("caja"), " .small-box { background-color: ", hex, " !important; }"
      ))
    })

    # Render caja ----
    # Delega en racafe::CajaValor. El boton solo se activa si mostrar_boton
    # es TRUE y existe contenido_modal definido
    output$caja <- bs4Dash::renderbs4ValueBox({
      id_boton <- if (isTRUE(mostrar_boton) && !is.null(contenido_modal)) {
        ns("btn_abrir")
      } else {
        NULL
      }
      racafe::CajaValor(
        valor         = .val(valor),
        formato       = formato,
        texto         = .normalizar_texto(texto),
        icono         = icono,
        inputId       = id_boton,
        mostrar_boton = isTRUE(mostrar_boton) && !is.null(contenido_modal),
        colores       = .val(colores)
      )
    })

    # Render footer ----
    # Retorna NULL si footer es NULL o cadena vacia para evitar espacio en blanco
    output$footer <- shiny::renderUI({
      txt <- .val(footer)
      if (is.null(txt) || !nzchar(as.character(txt))) return(NULL)
      shiny::p(txt, class = footer_class)
    })

    # Apertura modal ----
    # contenido_modal se evalua aqui para construir la UI solo cuando el
    # usuario abre el modal. El server del modulo hijo debe estar registrado
    # en el padre antes de que esto ocurra
    shiny::observeEvent(input$btn_abrir, {
      shiny::req(!is.null(contenido_modal))
      shiny::showModal(
        shiny::modalDialog(
          title     = shiny::tagList(shiny::icon(icono_modal), " ", .val(titulo_modal)),
          size      = tamano_modal,
          easyClose = TRUE,
          footer    = shiny::modalButton("Cerrar"),
          contenido_modal()
        )
      )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

  })
}
