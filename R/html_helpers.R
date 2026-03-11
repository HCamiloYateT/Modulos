#' CajaModal: módulo Shiny reutilizable para KPIs con modal
#'
#' Script principal del módulo `CajaModal`, incluyendo helpers de formato
#' (`html_valor()` y `html_texto()`), función UI y función server.
#'
#' @keywords internal

# Helpers HTML ----

#' Genera HTML de valor numerico coloreado para usar en CajaModal
#'
#' Construye un fragmento HTML con el valor formateado y coloreado,
#' listo para pasarse al argumento `valor` de [CajaModal()].
#'
#' @param v Valor numerico a formatear.
#' @param formato String. Uno de `"numero"`, `"porcentaje"`, `"moneda"`.
#' @param color String. Color CSS aplicado al span interior del numero.
#'   Default `"#212529"`.
#' @param tamano_pct Numeric. Multiplicador de tamano de fuente em. Default `2`.
#'
#' @return Objeto `shiny.tag` con el HTML del valor coloreado.
#' @export
#'
#' @examples
#' html_valor(73.4, formato = "porcentaje", color = "#27AE60")
html_valor <- function(v, formato = "numero", color = "#212529", tamano_pct = 2) {
  racafe::FormatearTexto(
    racafe::FormatearNumero(as.numeric(v), formato = formato, color = color),
    tamano_pct = tamano_pct, negrita = TRUE
  )
}


#' Genera HTML de subtitulo coloreado para usar en CajaModal
#'
#' Construye un fragmento HTML con texto coloreado y tamano configurable,
#' listo para pasarse al argumento `texto` de [CajaModal()].
#'
#' @param texto String. Texto a mostrar como subtitulo de la caja.
#' @param color String. Color CSS del texto. Default `"#212529"`.
#' @param tamano_pct Numeric. Multiplicador de tamano de fuente em. Default `1.2`.
#'
#' @return Objeto `shiny.tag` con el HTML del subtitulo coloreado.
#' @export
#'
#' @examples
#' html_texto("Clientes Activos", color = "#1A5276")
html_texto <- function(texto, color = "#212529", tamano_pct = 1.2) {
  racafe::FormatearTexto(texto, tamano_pct = tamano_pct, color = color)
}


