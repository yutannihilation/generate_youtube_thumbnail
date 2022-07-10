library(ggplot2)

theta1 <- pi * -1 / 70
theta2 <- pi * -1 / 26

set.seed(15)

pkg_name <- "plumber"
pkg_ver <- "1.2.0"
start_time <- "2022/7/12 22:00~"

f <- systemfonts::system_fonts() |>
  dplyr::filter(family == "Iosevka", style == "Heavy") |>
  dplyr::pull(path)

d_pkg <- string2path::string2fill(pkg_name, f[1], tolerance = 0.3) |>
  dplyr::mutate(
    tibble::tibble(
      x = x * 1.00 + 0.13 + 0.35 * (y - mean(y)),
      y = y * 0.95 + 0.33
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

d_ver <- string2path::string2fill(pkg_ver, f[1], tolerance = 0.01) |>
  dplyr::mutate(
    x = x * 0.6 + 0.6 + 0.05 * (y - mean(y)),
    y = y * 0.6 - 0.13,
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

p <- ggplot(mapping = aes(x, y)) +
  geom_text(data = d, aes(label = text, angle = angle), size = 6 * 4, colour = alpha("white", 0.4), family = "Iosevka") +
  geom_polygon(data = d_pkg, aes(x + 0.01, y - 0.03, group = triangle_id), fill = alpha("#0079dd", 0.3), colour = "transparent") +
  geom_polygon(data = d_ver, aes(x + 0.01, y - 0.03, group = triangle_id), fill = alpha("#0079dd", 0.3), colour = "transparent") +
  geom_polygon(data = d_pkg, aes(group = triangle_id, fill = fill), colour = alpha("white", 0.3), linewidth = 0.45) +
  geom_polygon(data = d_ver, aes(group = triangle_id, fill = fill), colour = alpha("white", 0.3), linewidth = 0.45) +
  scale_fill_viridis_c(option = "F", guide = "none", end = 0.85) +
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
