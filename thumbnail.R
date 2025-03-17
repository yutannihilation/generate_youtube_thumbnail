library(ggplot2)

# scale <- colorspace::scale_fill_continuous_sequential(
#   h1 = 270, h2 = 186, c1 = 55, c2 = 20, cmax = 60, l1 = 31, l2 = 92, p1 = 2.2, p2 = 0.7,
#   guide = "none", rev = TRUE
# )

scale <- scale_fill_viridis_c(option = "D", guide = "none", direction = -1)

shadow_colour <- colorspace::darken(alpha("#4211dd", 0.2), 0.4)

theta1 <- pi * 15.1 / 360
theta2 <- pi * 9.0 / 360

set.seed(15)

pkg_name <- "R"
pkg_ver <- "v4.5.0"
start_time <- "2025/03/18 22:00~"

font_family <- "Noto Sans JP"

d_pkg <- string2path::string2fill(
  pkg_name,
  font_family,
  font_weight = "black",
  tolerance = 0.01
) |>
  dplyr::mutate(
    x = x * 1.60 + 0.48,
    y = y * 1.60 + 0.00,
    data.frame(
      # 回転
      x = x * cos(theta1) - y * sin(theta1),
      y = x * sin(theta1) + y * cos(theta1)
    ),
    fill = abs(
      (60 * (x + 1))^1.2 + 30 * y + sqrt(triangle_id) + 50 * rnorm(dplyr::n())
    )^1.5
  ) |>
  dplyr::group_by(triangle_id) |>
  dplyr::mutate(
    fill = max(fill)
  ) |>
  dplyr::ungroup()

d_ver <- string2path::string2fill(
  pkg_ver,
  font_family,
  font_weight = "black",
  tolerance = 0.01
) |>
  dplyr::mutate(
    x = x * 0.71 + 1.00,
    y = y * 0.71 - 0.42,
    data.frame(
      # 回転
      x = x * cos(theta2) - y * sin(theta2),
      y = x * sin(theta2) + y * cos(theta2)
    ),
    fill = abs(
      70 +
        (60 * (x + 1))^1.2 +
        30 * y +
        sqrt(triangle_id) +
        50 * rnorm(dplyr::n())
    )^1.5
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
    c(
      "<-",
      "plot()",
      "runif()",
      "if (",
      "} else {",
      "df",
      "stop()",
      "is.na()",
      "is.null()",
      "<-",
      "=",
      "^",
      "@",
      "$",
      "[[",
      "]]",
      "==",
      "!=",
      "<-",
      "=",
      "^",
      "@",
      "$",
      "[[",
      "]]",
      "==",
      "!=",
      "<-",
      "=",
      "^",
      "@",
      "$",
      "[[",
      "]]",
      "==",
      "!=",
      "data.frame()",
      "+",
      "*",
      "%*%",
      "\\(x)",
      "q()",
      "*",
      "&&",
      "||",
      "[",
      "]",
      "|>",
      "*",
      "&&",
      "||",
      "[",
      "]",
      "|>",
      "*",
      "&&",
      "||",
      "[",
      "]",
      "|>",
      "%||%",
      "TRUE",
      "FALSE",
      "na.rm",
      "%in%",
      "mean()",
      "sd()",
      "?",
      "1:10",
      "tidyverse"
    ),
    size = n,
    replace = TRUE
  ),
  angle = runif(n, -20, 60)
)

p <- ggplot(mapping = aes(x, y, group = triangle_id)) +
  geom_text(
    data = d,
    aes(label = text, angle = angle, group = NULL),
    size = 6 * 4,
    colour = alpha("white", 0.4),
    family = "Iosevka"
  ) +
  geom_polygon(
    data = d_pkg,
    aes(x + 0.05, y - 0.03),
    fill = shadow_colour,
    colour = "transparent"
  ) +
  geom_polygon(
    data = d_ver,
    aes(x + 0.045, y - 0.02),
    fill = shadow_colour,
    colour = "transparent"
  ) +
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
  annotate(
    "text",
    x = 0.05,
    y = -0.55,
    label = start_time,
    hjust = 0,
    family = "Iosevka",
    fontface = "bold",
    size = 6.7 * 4,
    colour = alpha("black", 0.67)
  ) +
  coord_equal(
    xlim = c(0, 3),
    ylim = c(-1.3, 1.7) * 9 / 16
  )

ggsave("icon.png", p1, width = 1280, height = 720, units = "px", scale = 4)

p2 <- p +
  annotate(
    "text",
    x = 0.05,
    y = -0.55,
    label = "もうすぐはじまるよ～",
    hjust = 0,
    family = font_family,
    fontface = "bold",
    size = 6.7 * 4,
    colour = alpha("black", 0.67)
  ) +
  coord_equal(
    xlim = c(0, 3),
    ylim = c(-1.3, 1.7) * 9 / 16
  )

ggsave("waiting.png", p2, width = 1280, height = 720, units = "px", scale = 4)

p3 <- p +
  annotate(
    "text",
    x = 0.05,
    y = -0.55,
    label = "ありがとうございました！",
    hjust = 0,
    family = font_family,
    fontface = "bold",
    size = 6.7 * 4,
    colour = alpha("black", 0.67)
  ) +
  coord_equal(
    xlim = c(0, 3),
    ylim = c(-1.3, 1.7) * 9 / 16
  )

ggsave("goodbye.png", p3, width = 1280, height = 720, units = "px", scale = 4)
