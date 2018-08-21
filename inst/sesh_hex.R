#' -----
#' title: Hexsticker for sesh
#' author: nathancday@@gmail.com
#' -----

library(hexSticker)
library(emojifont)

s <- ggplot() +
    geom_emoji("call_me_hand", color = "#4b9d13", size = 30, angle = 90, fontface = "bold") +
    theme_void()

# external window to view due to RStudio rendering problems
quartz()
s


sticker(s, s_height = 10, s_width = 2, s_x = 1,
        package="sesh", p_size=10, p_x = 1, p_y = 1.5, p_color = "#fe9be3",
        h_fill = "#f5e593", h_color = "#6fe2dd", h_size = 2,
        filename="inst/sesh_hex.png")

