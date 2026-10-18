# Article: https://doi.org/10.1038/533452a
# Data: https://doi.org/10.6084/m9.figshare.3394951
file = "data/Reproducibilitysurveyrawdata20160523.txt"
# download.file("http://www.nature.com/polopoly_fs/7.36742!/file/Reproducibility%20Survey%20Raw%20Data.xlsx", file)

r2 <- readr::read_tsv(
  file,
  locale = readr::locale(encoding = "windows-1252"),
  skip = 1,
  n_max = 1,
  name_repair = "none"
)
r1 <- readr::read_tsv(
  file,
  locale = readr::locale(encoding = "windows-1252"),
  n_max = 1,
  name_repair = "none"
)
composite_colname = paste(colnames(r1), colnames(r2))

df <- readr::read_tsv(
  file,
  locale = readr::locale(encoding = "windows-1252"),
  skip = 1
)
colnames(df) <- composite_colname |> vctrs::vec_as_names(repair = "unique")

## PLOT (English)

library(dplyr)
library(ggplot2)

question <- colnames(df)[20] |> trimws()

plot_df <- df |>
  rename(answer = 20) |>
  filter(!is.na(answer)) |>
  count(answer) |>
  mutate(
    pct = n / sum(n),
    answer = stringr::str_wrap(answer, 40),
    answer = forcats::fct_reorder(answer, n) # sort bars by frequency
  )

ggplot(plot_df, aes(x = n, y = answer)) +
  geom_col(fill = "#2C7FB8", width = 0.7) +
  geom_text(
    aes(label = paste0(n, " (", scales::percent(pct, accuracy = 1), ")")),
    hjust = -0.1,
    size = 4
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = stringr::str_wrap(question, 70),
    subtitle = paste0("n = ", sum(plot_df$n), " respondents"),
    x = "Number of respondents",
    y = NULL,
    caption = "Data: Nature reproducibility survey (Baker, 2016)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold")
  )


## PLOT (French)

library(dplyr)
library(ggplot2)

# Titre de la question, traduit à la main (la colonne d'origine est en anglais)
titre_fr <- "Y-a-t-il une « crise de la reproductibilité » ?"

# Traduction des réponses (à ajuster si les libellés de vos données diffèrent) :
# toute réponse non listée ici garde son libellé anglais d'origine
traduction <- c(
  "There is a significant crisis of reproducibility" = "Crise importante",
  "There is a slight crisis of reproducibility" = "Légère crise",
  "There is no crisis of reproducibility" = "Pas de crise",
  "I don't know" = "Ne sais pas"
)

plot_df <- df |>
  rename(answer = 20) |>
  filter(!is.na(answer)) |>
  mutate(answer = trimws(answer), answer = recode(answer, !!!traduction)) |>
  count(answer) |>
  mutate(
    pct = n / sum(n),
    answer = stringr::str_wrap(answer, 40),
    answer = forcats::fct_reorder(answer, n)
  )

ggplot(plot_df, aes(x = n, y = answer)) +
  geom_col(fill = "#2C7FB8", width = 0.7) +
  geom_text(
    aes(
      label = paste0(
        scales::number(n, big.mark = " "),
        " (",
        scales::percent(pct, accuracy = 1, decimal.mark = ",", suffix = " %"),
        ")"
      )
    ),
    hjust = -0.1,
    size = 4
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = stringr::str_wrap(titre_fr, 70),
    subtitle = paste0(
      "n = ",
      scales::number(sum(plot_df$n), big.mark = " "),
      " répondants"
    ),
    x = "Nombre de répondants",
    y = NULL,
    caption = "Données : enquête de Nature sur la reproductibilité (Baker, 2016)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold")
  )
# fs::dir_create("fig")
ggsave("fig/repr_crisis.png", width = 8, height = 6)
