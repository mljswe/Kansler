

##### FARG OCH FORM #####

# 1. Farg - vektorer med profilerfarger samt olika paletter
# 2: Form - teman och standard-plots




###############################################################################
################################# 1. Farg #####################################
###############################################################################


# 1.1

#' Vektor med kanslerns farger
#'
#' 18 farger:
#' nr. 1-3 uka:s huvudsakliga profilfarger
#' nr. 4-6 sekundarfarger
#'
#' @export
uka_farg_vect <- c(gul1 = "#ffab2e",
                   gul2 = "#fec367",
                   gul3 = "#ffd490",
                   gul4 = "#c68529",
                   gul5 = "#d7a560",
                   gul6 = "#e1bc87",
                   bla1 = "#016cbc",
                   bla2 = "#4290ce",
                   bla3 = "#76acdb",
                   bla4 = "#084d98",
                   bla5 = "#4a78b3",
                   bla6 = "#7a99c5",
                   lila1 = "#632896",
                   lila2 = "#8c63b2",
                   lila3 = "#a98dc7",
                   lila4 = "#4d2470",
                   lila5 = "#775a92",
                   lila6 = "#9883ac")





# 1.2

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




# 1.3

#' Funktion som skapar paletter baserade pa fargerna i vektorn
#'
#' uka_prim:         UKA:s primara farger (9 st)
#' uka_sek:          UKA:s sekundara farg (9 st)
#' uka_gul/bla/lila: UKA:s gula/bla/lila farger (6 var)
#' uka_1:            Forslag pa standardtema (12 st)
#'
#' @export
uka_farg_paletter <- list("uka_prim" =  c(uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "1")],
                                          uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "2")],
                                          uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "3")]),
                          "uka_sek"  =  c(uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "4")],
                                          uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "5")],
                                          uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "6")]),
                          "uka_gul"  =    uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "gul")],
                          "uka_bla"  =    uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "bla")],
                          "uka_lila" =    uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "lila")],
                          "uka_1"    =  c(uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "4")],
                                          uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "1")],
                                          uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "3")],
                                          uka_farg_vect[stringr::str_detect(names(uka_farg_vect), "6")]))


#' Tema UKA
#'
#' Tema som anvands for formatering av figurer. Ar tankt att designad for uttag i formatet 4:6 / landscape.
#'
#' @param size anvands for att ange storlek pa text. default = 10
#' @param angle ange vinkel pa x-axelns text. "a0" default. "a90" ger 90 grader.
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
    ggplot2::scale_color_manual(values = as.vector(uka_farg_paletter[["uka_1"]])) +
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
num <- as.numeric(c(1:20))
nam <- letters[1:20]
gro = c(rep("0",3), rep("1", 3), rep("2", 3), rep("3", 3), rep("4", 3), rep("5", 3), rep("6", 2))
out <- as.data.frame(cbind(num = as.numeric(num), nam, gro))
return(out)
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




