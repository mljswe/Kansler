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
#' @export
Svensk_procent <- function(x, ggr100 = TRUE, n) {
 paste0(format(round(x*(ggr100*100)), nsmall = n, decimal.mark =","), " %")
}








#' Tema UKA
#'
#' Tema som anvands for formatering av figurer. Ar tankt att designad for uttag i formatet 4:6 / landscape.
#'
#' @param size anvands for att ange storlek pa text. default = 10
#' @param angle ange vinkel pa x-axelns text. "a0" default. "a90" ger 90 grader.
#'
#'
#' @export
theme_uka1 <- function(size = 10, angle = "a0")  {

if (angle == "a0") {a = 0; h = 0.5} else if (angle == "a90") {a = 90; h <- 1}

    ggplot2::theme(legend.position = "bottom",
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 legend.title  = ggplot2::element_blank(),
                 axis.ticks.y.left = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 text = ggplot2::element_text(colour = "black", size = size),
                 axis.text.x = ggplot2::element_text(angle = a, hjust = h),
                 plot.background = ggplot2::element_rect(fill = "white"),
                 panel.background = ggplot2::element_rect(fill = "white"),
                 legend.background = ggplot2::element_rect(fill = "white"),
                 #legend.margin = ggplot2::element_rect(fill = "white"),
                 legend.key = ggplot2::element_rect(fill = "white"),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(color = "#9d9d9c", size = 0.1))
}



#' UKA linjediagram 1
#'
#' använder theme_uka1 och uka_farg. for testning
#'
#'
#' @param x x-variabel
#' @param y y-variabel (num)
#' @param g gruppvariabel
#' @param df dataframe
#'
#' @return linjediagram
#' @export
#'
#' @examples anvander make_df: uka_line(df1 = df, x = nam, y = num, g = gro)
#'
gguka_line <- function(df1, x, y, g) {

  ymax <- df1 %>% dplyr::select({{y}}) %>% max()

  ggplot2::ggplot(df1, ggplot2::aes(x = !!enquo(x), y = {{y}}, group = {{g}})) +
    geom_line(aes(color = {{g}}), size = 0.8) +
    theme_uka1(angle = "a0") +
    ggplot2::scale_color_manual(values = as.vector(uka_farg())) +
    ggplot2::scale_y_continuous(limits = c(0, ymax*1.2))

}




### Fixa y i scale ovan


#' testdata
#'
#' @return df som anvands for test
#' @export
#'
#' @examples df <- make_df()
make_df <- function() {
num <- c(1:20)
nam <- letters[1:20]
gro = c(rep("0",7), rep("1", 7), rep("2", 6))
out <- as.data.frame(cbind(num = as.numeric(num), nam, gro))
return(out)
}




