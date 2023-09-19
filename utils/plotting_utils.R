# several of these functions have now been moved into stylehaven
library(stylehaven)
library(ggrepel)
library(sf)
# suppress values below nmin in tables
nmin <- 50
font_add_weights("Roboto Slab", regular = 400, semibold = 500, bold = 600, black = 800)
########## feel free to change all the theme stuff & colors
# fonts here match the fonts used in the draft rmd, but that can change as well
############################ COLORS ----
# seq_pal <- rev(colorspace::sequential_hcl(5, palette = "Blue-Yellow"))
# seq_pal <- rev(c("#00346f", "#006995", "#009ba4", "#7bcaaa", "#ddf5bc"))
seq_pal_gen <- function(cols, n) {
  m <- colorspace::coords(as(colorspace::hex2RGB(cols), "polarLUV"))
  # 2 x 3 matrix
  h <- m[,3]; c <- m[,2]; l <- m[,1]
  colorspace::sequential_hcl(n = n, h1 = h[1], h2 = h[2], c1 = c[1], c2 = c[2], l1 = l[1], l2 = l[2])
}

bupu <- function(n) RColorBrewer::brewer.pal(n, "BuPu")
bupu9 <- bupu(9)
# main_color <- colorspace::lighten(bupu9[8], amount = 0.1)
main_color <- "#3b3bdc"
# seq_pal <- bupu(6)[6:2]
pal_mtx <- palx(main_color, n_shades = 6, n_hues = 12)
race_pal <- c("gray50", unname(pal_mtx$shade03[c("pink", "blue", "lime", "orange")]))
gender_pal <- unname(c(pal_mtx$shade02[["indigo"]], pal_mtx$shade03[["lime"]]))
# true/false values e.g. NA suppressed values
tf_pal <- c("TRUE" = "gray30", "FALSE" = "white")
line_pal <- list(l = 0.6, m = 0.4, s = 0.2)
seq_pal <- seq_pal_gen(unname(c(pal_mtx$shade02[["indigo"]], pal_mtx$shade03[["lime"]])), n = 4)

############################ GGPLOT ----

def_margin <- margin(4, 4, 4, 4, "pt")

theme_src <- function(base_family = "Roboto Slab", base_size = 18, ...) {
  camiller::theme_din(base_family = base_family, base_size = base_size, ...) +
    theme(axis.text = element_text(color = "black"),
          strip.text = element_text(color = "black", size = rel(1.05), family = "Roboto Slab Semibold", vjust = 0),
          strip.text.x = element_text(hjust = 0.5, margin = margin(4, 0, 4, 0, "pt")),
          strip.text.y = element_text(vjust = 0, margin = margin(0, 4, 0, 4, "pt")),
          strip.background.x = element_blank(),
          panel.spacing.x = unit(0.2, "in"),
          plot.title = element_text(size = rel(1.2), face = "bold", family = "Roboto Slab"),
          plot.subtitle = element_text(size = rel(1.05), face = "plain", color = "black", family = "Roboto Slab Semibold"),
          legend.text = element_text(margin = margin(0, 1, 0, -0.25, "lines"), size = rel(0.8)),
          legend.title = element_text(size = rel(0.8), face = "plain", family = "Roboto Slab Semibold")
    )
}

theme_set(theme_src())
update_geom_defaults("text", list(family = "Roboto Slab", fontface = "bold", size = 4, color = "white"))
update_geom_defaults("text_repel", list(family = "Roboto Slab", fontface = "bold", size = 4, color = "white"))
update_geom_defaults("point", list(alpha = 0.9))
update_geom_defaults("sf", list(linewidth = line_pal$s))
update_geom_defaults("col", list(fill = main_color))


## TEXT ----
clean_ages <- function(x) {
  x <- stringr::str_replace(x, "(?<=\\d)_(?=\\d)", "-")
  x <- stringr::str_replace(x, "(?<=[A-Za-z])\\B(?=\\d)", " ")
  x <- stringr::str_replace(x, "plus", "+")
  x <- camiller::clean_titles(x)
  x
}
