% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Funcion_EFijos.R
\name{efectosfijos}
\alias{efectosfijos}
\title{Analisis combinado - Efectos fijos}
\usage{
efectosfijos(datos, varresp, bloques, sitios, tratamiento)
}
\arguments{
\item{datos}{Un data.frame con los datos.}

\item{varresp}{Nombre de la columna donde se encuentra la variable respuesta}

\item{bloques}{Nombre de la columna donde se encuentran los bloques}

\item{sitios}{Nombre de la columna donde se encuentra los diferentes entornos}

\item{tratamiento}{Nombre de la columna donde se encuentran los tratamientos}
}
\value{
Tabla ANOVA con los valores F calculados segun McIntosh
}
\description{
Analisis combinado - Efectos fijos
}
