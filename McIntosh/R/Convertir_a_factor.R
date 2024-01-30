#' Convertir un conjunto de variables a factor.
#'
#' @param data Archivo de datos.
#' @param columnas_a_convertir Nombre de las columnas que desea pasar a factor.
#'
#' @return Archivo de datos, con las variables seleccionadas transformadas a factor.
#' @export
#'
#' @examples
#' convertir_a_factor("Nombre_conjunto_de_datos", c("columna1","columna2","columna_n"))
#'
convertir_a_factor <- function(data, columnas_a_convertir) {
  for (col in columnas_a_convertir) {
    if (col %in% names(data)) {
      data[[col]] <- as.factor(data[[col]])
    } else {
      warning(paste("La columna", col, "no existe en el data frame. Ignorada."))
    }
  }
  return(data)
}
