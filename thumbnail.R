library(ggplot2)


f <- systemfonts::system_fonts() |>
  dplyr::filter(family == "Iosevka", style == "Heavy") |>
  dplyr::pull(path)

d_pkg <- string2path::string2fill("webshot2", f[1], tolerance = 0.01) |>
  dplyr::mutate(
    x = x * 1.0 - 0.1,
    y = y * 1.0 + 0.2,
    fill = as.integer(10 * x + 3 * y  + 1 * triangle_id + 5 * runif(dplyr::n()))
  ) |>
  dplyr::group_by(triangle_id) |>
  dplyr::mutate(
    fill = max(fill)
  ) |>
  dplyr::ungroup()

d_ver <- string2path::string2fill("0.1.0", f[1], tolerance = 0.01) |>
  dplyr::mutate(
    x = x * 0.6 + 1.5,
    y = y * 0.6 - 0.2,
    fill = as.integer(triangle_id + 60 * runif(dplyr::n()))
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
  geom_polygon(data = d_pkg, aes(group = triangle_id, fill = fill)) +
  geom_polygon(data = d_ver, aes(group = triangle_id, fill = fill)) +
  annotate("text", x = 0.05, y = -0.65, label = "2022/5/24 22:00~", hjust = 0,
           family = "Iosevka SS08", fontface = "bold", size = 6.7 * 4, colour = alpha("black", 0.87)) +
  scale_fill_viridis_c(option = "A", guide = "none") +
  coord_equal(
    xlim = c(0,  3),
    ylim = c(-1.3, 1.7) * 9 / 16
  ) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey"))

ggsave("icon.png", p, width = 1280, height = 720, units = "px", scale = 4)

browseURL("icon.png")
