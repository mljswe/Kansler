#' Vektor med grundfarger
#'
#' 12 farger, varav de tre forsta ar uka:s huvudsakliga profilfarger. resten
#' ar hamtade fran arsrapporten 2020 samt pahittade
#'
#' @export
uka_farg_vect <- c(gul1 = "#C38B2C",
              lila1 = "#602467",
              bla1 = "#0064A9",
              gul2 = "#E1C289",
              lila2 = "#A786AA",
              bla2 = "#8CAAD8",
              gul3 = "#73521A",
              lila3 = "#321335",
              bla3 = "#0003a8",
              gul4 = "#fce0b1",
              lila4 = "#D0C1D6",
              bla4 = "#C8D3EC")



#' Funktion som extraherar profilfarger
#'
#' Funktion som ger tillgang till uka:s profilfarger, som de ar lagrade i
#' vektorn uka_farg_vect.
#'
#' @param ... char-vektor som hamtar specifika farger
#'
#' @export
uka_farg <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (uka_farg_vect)

  uka_farg_vect[cols]
}



#' Svensk procent
#'
#' @param x tal som ska anges som procent
#' @param ggr100 boolean; TRUE: x ar i decimalform (och multipliceras med 100). Standard.
#'                        FALSE: x ar inte i decimalform (multpliceras inte med 100).
#' @param n antal decimaler (nsmall)
#'
#'
Svensk_procent <- function(x, ggr100 = TRUE, n) {
 paste0(format(round(x*(ggr100*100)), nsmall = n, decimal.mark =","), " %")
}












