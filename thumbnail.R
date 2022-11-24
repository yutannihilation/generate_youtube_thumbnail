library(ggplot2)

scale <- colorspace::scale_fill_continuous_sequential(
  h1 = 247, h2 = NA, c1 = 43, c2 = 0, l1 = 99, l2 = 1, p1 = 1.0, p2 = NA,
  guide = "none"
)

shadow_colour <- colorspace::darken(alpha("#dd4211", 0.2), 0.4)

theta1 <- pi * 13.1 / 360
theta2 <- pi * 15.0 / 360

set.seed(15)

pkg_name <- "codegrip"
pkg_ver <- ""
start_time <- "2022/12/13 22:00~"

d_pkg <- string2path::string2fill(pkg_name, "Noto Sans JP", font_weight = "black", tolerance = 0.01) |>
  dplyr::mutate(
    tibble::tibble(
      x = x * 0.95 + 0.22,
      y = y * 0.95 - 0.17
    ),
    # 回転
    x = x * cos(theta1) - y * sin(theta1),
    y = x * sin(theta1) + y * cos(theta1),
    fill = as.integer(100 * x + 30 * y + sqrt(triangle_id) + 50 * rnorm(dplyr::n()))
  ) |>
  dplyr::group_by(triangle_id) |>
  dplyr::mutate(
    fill = max(fill)
  ) |>
  dplyr::ungroup()

d_ver <- string2path::string2fill(pkg_ver, "Noto Sans JP", font_weight = "black", tolerance = 0.01) |>
  dplyr::mutate(
    x = x * 0.6 + 1.11,
    y = y * 0.6 - 0.41,
    # 回転
    x = x * cos(theta2) - y * sin(theta2),
    y = x * sin(theta2) + y * cos(theta2),
    fill = as.integer(70 + 100 * x + 30 * y + sqrt(triangle_id) + 50 * rnorm(dplyr::n()))
  ) |>
  dplyr::group_by(triangle_id) |>
  dplyr::mutate(
    fill = max(fill)
  ) |>
  dplyr::ungroup()

n <- 400
d <- data.frame(
  x = rnorm(n, 1.7, 1.0),
  y = rnorm(n, 0.1, 1.0),
  text = sample(
    c("<-", "plot()", "runif()", "if (", "} else {",
      "df", "stop()", "is.na()", "is.null()",
      "<-", "=", "^", "@", "$", "[[", "]]", "==", "!=",
      "<-", "=", "^", "@", "$", "[[", "]]", "==", "!=",
      "<-", "=", "^", "@", "$", "[[", "]]", "==", "!=",
      "data.frame()", "+", "*", "%*%", "\\(x)", "q()",
      "*", "&&", "||", "[", "]", "|>",
      "*", "&&", "||", "[", "]", "|>",
      "*", "&&", "||", "[", "]", "|>",
      "%||%", "TRUE", "FALSE", "na.rm",
      "%in%", "mean()", "sd()", "?", "1:10", "tidyverse"),
    size = n,
    replace = TRUE
  ),
  angle = runif(n, -20, 60)
)

p <- ggplot(mapping = aes(x, y, group = triangle_id)) +
  geom_text(data = d, aes(label = text, angle = angle, group = NULL),
            size = 6 * 4, colour = alpha("white", 0.4), family = "Iosevka") +
  geom_polygon(data = d_pkg, aes(x + 0.05,  y - 0.03), fill = shadow_colour, colour = "transparent") +
  geom_polygon(data = d_ver, aes(x + 0.045, y - 0.02), fill = shadow_colour, colour = "transparent") +
  geom_polygon(data = d_pkg, aes(fill = fill), colour = "transparent") +
  geom_polygon(data = d_ver, aes(fill = fill), colour = "transparent") +
  geom_path(data = d_pkg, colour = alpha("white", 0.3), linewidth = 0.75) +
  geom_path(data = d_ver, colour = alpha("white", 0.3), linewidth = 0.75) +
  geom_point(data = d_pkg, colour = alpha("white", 0.3), size = 2.4) +
  geom_point(data = d_ver, colour = alpha("white", 0.3), size = 2.4) +
  scale +
  theme_void() +
  theme(plot.background = element_rect(fill = "#DDDDDD"))

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

p3 <- p +
  annotate("text", x = 0.05, y = -0.55, label = "ありがとうございました！", hjust = 0,
           family = "Noto Sans JP", fontface = "bold", size = 6.7 * 4, colour = alpha("black", 0.67)) +
  coord_equal(
    xlim = c(0,  3),
    ylim = c(-1.3, 1.7) * 9 / 16
  )

ggsave("goodbye.png", p3, width = 1280, height = 720, units = "px", scale = 4)
