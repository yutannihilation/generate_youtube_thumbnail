library(ggplot2)

theta1 <- pi * 1 / 80
theta2 <- 0

pkg_name <- "rstudio::conf(2022)"
pkg_ver <- ""
start_time <- "2022/6/21 22:00~"

f <- systemfonts::system_fonts() |>
  dplyr::filter(family == "Iosevka", style == "Heavy") |>
  dplyr::pull(path)

d_pkg <- string2path::string2fill(pkg_name, f[1], tolerance = 0.3) |>
  dplyr::mutate(
    tibble::tibble(
      x = x * 0.485 - 0.19 + 0.13 * (y - mean(y)) - 0.013 * glyph_id,
      y = y * 0.93 - 0.13 + 0.08 * (1.3 * (x - 0.6)) * (0.3 + y)
    ),
    # 回転
    x = x * cos(theta1) - y * sin(theta1),
    y = x * sin(theta1) + y * cos(theta1),
    fill = as.integer(100 * x + 30 * y + sqrt(triangle_id) + 70 * rnorm(dplyr::n()))
  ) |>
  dplyr::group_by(triangle_id) |>
  dplyr::mutate(
    fill = max(fill)
  ) |>
  dplyr::ungroup()

d_ver <- string2path::string2fill(pkg_ver, f[1], tolerance = 0.01) |>
  dplyr::mutate(
    x = x * 0.8 + 1.4,
    y = y * 0.8 - 0.4,
    # 回転
    x = x * cos(theta2) - y * sin(theta2),
    y = x * sin(theta2) + y * cos(theta2),
    fill = as.integer(100 + 100 * x + 30 * y + sqrt(triangle_id) + 70 * rnorm(dplyr::n()))
  ) |>
  dplyr::group_by(triangle_id) |>
  dplyr::mutate(
    fill = max(fill)
  ) |>
  dplyr::ungroup()

set.seed(15)
n <- 400
d <- data.frame(
  x = rnorm(n, 1.7, 1.0),
  y = rnorm(n, 0.1, 1.0),
  text = sample(
    c("<-", "plot()", "runif()", "if (", "} else {",
      "df", "stop()",
      "<-", "=", "^", "@", "$", "[[", "]]",
      "<-", "=", "^", "@", "$", "[[", "]]",
      "<-", "=", "^", "@", "$", "[[", "]]",
      "data.frame()", "+", "*", "%*%", "\\(x)", "q()",
      "*", "&&", "||", "[", "]", "|>",
      "*", "&&", "||", "[", "]", "|>",
      "*", "&&", "||", "[", "]", "|>",
      "%in%", "mean()", "sd()", "?", "1:10", "tidyverse"),
    size = n,
    replace = TRUE
  ),
  angle = runif(n, -20, 60)
)

p <- ggplot(mapping = aes(x, y)) +
  geom_text(data = d, aes(label = text, angle = angle), size = 6 * 4, colour = alpha("white", 0.4), family = "Iosevka") +
  geom_polygon(data = d_pkg, aes(group = triangle_id, fill = fill), colour = alpha("white", 0.3), size = 0.45) +
  geom_polygon(data = d_ver, aes(group = triangle_id, fill = fill), colour = alpha("white", 0.3), size = 0.45) +
  scale_fill_viridis_c(option = "A", guide = "none", begin = 0.0, end = 0.7, direction = -1) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#C5C5C5"))

p1 <- p +
  annotate("text", x = 0.05, y = -0.55, label = start_time, hjust = 0,
           family = "Iosevka SS08", fontface = "bold", size = 6.7 * 4, colour = alpha("black", 0.67)) +
  coord_equal(
    xlim = c(0,  3),
    ylim = c(-1.3, 1.7) * 9 / 16
  )

ggsave("icon.png", p1, width = 1280, height = 720, units = "px", scale = 4)

browseURL("icon.png")

p2 <- p +
  annotate("text", x = 0.05, y = -0.55, label = "もうすぐはじまるよ～", hjust = 0,
           family = "Noto Sans JP", fontface = "bold", size = 6.7 * 4, colour = alpha("black", 0.67)) +
  coord_equal(
    xlim = c(0,  3),
    ylim = c(-1.3, 1.7) * 9 / 16
  )

ggsave("waiting.png", p2, width = 1280, height = 720, units = "px", scale = 4)

browseURL("waiting.png")
