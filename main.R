# Qatar Cars Data Analysis


# 1. Load packages and data

library(tidyverse)
library(here)

qatar_cars <- read_csv(here("data", "qatarcars.csv"))


# 2. Dataset summary

dataset_summary <- qatar_cars |>
  summarise(
    Observations = n(),
    Countries = n_distinct(origin),
    Brands = n_distinct(make),
    `Engine Types` = paste0(
      n_distinct(enginetype),
      " (Petrol, Hybrid, Electric)"
    ),
    `Price Range (QAR)` = paste0(
      scales::comma(min(price, na.rm = TRUE)),
      " - ",
      scales::comma(max(price, na.rm = TRUE))
    )
  )


# 3. Car prices by country

price_by_origin <- qatar_cars |>
  mutate(
    origin = fct_reorder(
      origin,
      price,
      median,
      na.rm = TRUE
    )
  )

price_by_origin_plot <- price_by_origin |>
  ggplot(
    aes(
      x = origin,
      y = price,
      fill = origin
    )
  ) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2) +
  scale_y_log10(
    labels = scales::label_comma()
  ) +
  labs(
    x = "Country of Origin",
    y = "Price (QAR)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )


# 4. Price vs horsepower by engine type

horsepower_data <- qatar_cars |>
  filter(
    !is.na(horsepower),
    !is.na(price),
    !is.na(enginetype)
  )

horsepower_price_plot <- horsepower_data |>
  ggplot(
    aes(
      x = horsepower,
      y = price,
      color = enginetype
    )
  ) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10(
    labels = scales::label_comma()
  ) +
  labs(
    x = "Horsepower (hp)",
    y = "Price (QAR, log scale)",
    color = "Engine Type"
  ) +
  theme_minimal()


# 5. PCA analysis

pca_data <- qatar_cars |>
  select(
    price,
    length,
    width,
    height,
    trunk,
    mass,
    origin,
    enginetype
  ) |>
  filter(
    complete.cases(
      pick(
        price,
        length,
        width,
        height,
        trunk,
        mass
      )
    )
  )

pca_result <- pca_data |>
  select(
    price,
    length,
    width,
    height,
    trunk,
    mass
  ) |>
  scale() |>
  prcomp()

pca_scores <- as_tibble(pca_result$x) |>
  bind_cols(
    pca_data |>
      select(origin, enginetype)
  )

pca_loadings <- as_tibble(
  pca_result$rotation,
  rownames = "variable"
) |>
  mutate(
    PC1_scaled = PC1 * 4,
    PC2_scaled = PC2 * 4
  )

pca_plot <- ggplot() +
  geom_point(
    data = pca_scores,
    aes(
      x = PC1,
      y = PC2,
      color = origin,
      shape = enginetype
    )
  ) +
  geom_segment(
    data = pca_loadings,
    aes(
      x = 0,
      y = 0,
      xend = PC1_scaled,
      yend = PC2_scaled
    ),
    arrow = arrow(
      length = unit(0.25, "cm")
    )
  ) +
  geom_text(
    data = pca_loadings,
    aes(
      x = PC1_scaled * 1.12,
      y = PC2_scaled * 1.12,
      label = variable
    )
  ) +
  labs(
    title = "PCA Biplot: Size, Price, Origin, and Engine Type",
    x = "PC1",
    y = "PC2",
    color = "Origin",
    shape = "Engine Type"
  ) +
  theme_minimal()