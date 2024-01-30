#' Analisis combinado - Efectos fijos
#'
#' @param datos Un data.frame con los datos.
#' @param varresp Nombre de la columna donde se encuentra la variable respuesta
#' @param bloques Nombre de la columna donde se encuentran los bloques
#' @param sitios Nombre de la columna donde se encuentra los diferentes entornos
#' @param tratamiento Nombre de la columna donde se encuentran los tratamientos
#'
#' @return Tabla ANOVA con los valores F calculados segun McIntosh
#'
#' @importFrom stats lm pf
#'
#' @export
#'
#' @examples
#'
efectosfijos <- function(datos,varresp,bloques,sitios,tratamiento){

  modelo.fijos<-lm(paste0(varresp,"~",sitios,"+",bloques,"%in%",sitios,"+",tratamiento,"+",sitios,":",tratamiento),data=datos)
  anovafijos<-anova(modelo.fijos)
  ##Modifico la tabla anova
  fv<-rownames(anovafijos)
  resultados<-data.frame(
    "FV"= c(
        fv[1],
        fv[2],
        fv[4],
        fv[5]
            ),
    "CM"=c(
      anovafijos[1,3],
      anovafijos[2,3],
      anovafijos[4,3],
      anovafijos[5,3]
    ),
    "GL"=c(
      anovafijos[1,1],
      anovafijos[2,1],
      anovafijos[4,1],
      anovafijos[5,1]
    ),
    ##Calculo los estadisticos F, para efectos fijos segun McIntosh
    "Estadistico.F"=c(
      round(anovafijos[1,3]/anovafijos[3,3],2),
      round(anovafijos[2,3]/anovafijos[5,3],2),
      round(anovafijos[4,3]/anovafijos[5,3],2),
      "-"),
    ##Calculo los p.valor de los F de arriba.
    ##Si uso round y el p-valor es muy chico no lo pone en notacion cientifica, pone 0.
    "P.valor"=c(
              pf(c(anovafijos[1,3]/anovafijos[3,3]), df1=anovafijos[1,1], df2=anovafijos[3,1], lower.tail=FALSE),
              pf(c(anovafijos[2,3]/anovafijos[5,3]), df1=anovafijos[2,1], df2=anovafijos[5,1], lower.tail=FALSE),
              pf(c(anovafijos[4,3]/anovafijos[5,3]), df1=anovafijos[4,1], df2=anovafijos[5,1], lower.tail=FALSE),
              "-")
                         )
  return(resultados)
}
