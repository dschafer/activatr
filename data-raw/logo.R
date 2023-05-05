#' This generates the logo in activatr.png, and uses it in the package.

library(dplyr)
library(ggplot2)
library(hexSticker)

x <- 1:13
shoe_df <- dplyr::bind_rows(
  data.frame(
    x = x,
    y = c(5, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    fill = "support"
  ),
  data.frame(
    x = x,
    y = c(0, 0, 0, 0, 15, 12.5, 10, 7.5, 5, 0, 0, 0, 0),
    fill = "tongue"
  ),
  data.frame(
    x = x,
    y = c(65, 60, 55, 55, 65, 60, 55, 50, 45, 40, 35, 25, 15),
    fill = "upper"
  ),
  data.frame(
    x = x,
    y = 8,
    fill = "zsole"
  ),
  data.frame(
    x = x,
    y = c(5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 10),
    fill = "zzfloor"
  )
)
shoe <- ggplot(shoe_df) +
  geom_col(aes(x = x, y = y, fill = fill), show.legend = FALSE) +
  theme_void() +
  scale_fill_manual(
    values = c("white", "#fdae61", "#66c2a5", "white", "#d53e4f")
  )
sticker <- sticker(
  shoe,
  package = "activatr",
  p_size = 26,
  p_y = .67,
  p_x = 1.0,
  h_fill = "#d53e4f",
  h_color = "#fdae61",
  s_x = 1.0,
  s_y = 1.25,
  s_width = 1.2,
  s_height = .5
)

usethis::use_logo("activatr.png")
