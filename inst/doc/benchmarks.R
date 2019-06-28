## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)

pretty_sec <- function(x) {
  x[!is.na(x)] <- prettyunits::pretty_sec(x[!is.na(x)])
  x
}

read_benchmark <- function(file, desc) {
  vroom::vroom(file, col_types = c("cccdddd")) %>%
    filter(type == "real", op != "setup") %>%
    mutate(
      method = case_when(
        package == "data.table" ~ "data.table",
        package == "readr" ~ "dplyr",
        package == "read.delim" ~ "base",
        package == "write.delim" ~ "base",
        grepl("^vroom", package) ~ sub(".*_", "", package)
        ),
      altrep = case_when(
        grepl("^vroom [(]full altrep[)]", package) ~ "full",
        grepl("^vroom [(]no altrep[)]", package) ~ "none",
        grepl("^vroom_", package) ~ "normal",
        TRUE ~ NA_character_
        ),
      package = case_when(
        grepl("^vroom", package) ~ "vroom",
        TRUE ~ package
      ),
    label = fct_reorder(
      glue::glue("{package}{altrep}\n{method}",
        altrep = ifelse(is.na(altrep), "", glue::glue("(altrep: {altrep})"))
      ),
      time,
      sum),
    op = factor(op, desc)
  )
}

generate_subtitle <- function(data) {
  rows <- scales::comma(data$rows[[1]])
  cols <- scales::comma(data$cols[[1]])
  size <- fs::fs_bytes(data$size[[1]])
  glue::glue("{rows} x {cols} - {size}B")
}

plot_benchmark <- function(data, title) {

  subtitle <- generate_subtitle(data)

  data %>%
    filter(package != "read.delim") %>%
    ggplot() +
    geom_bar(aes(x = label, y = time, fill = op, group = label), stat = "identity") +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    scale_y_continuous(labels = scales::number_format(suffix = "s")) +
    theme(legend.position = "bottom") +
    coord_flip() +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, fill = NULL)
}

make_table <- function(data) {
  data %>%
    select(-label, -type, -size, -rows, -cols) %>%
    spread(op, time) %>%
    mutate(
      total = read + print + head + tail + sample + filter + aggregate,
    ) %>%
    rename(manip = method) %>%
    arrange(desc(total)) %>%
    mutate_if(is.numeric, pretty_sec) %>%
    knitr::kable(digits = 2, align = "r", format = "html")
}

desc <- c("setup", "read", "print", "head", "tail", "sample", "filter", "aggregate")

## ---- fig.height = 8, fig.width=10, warning = FALSE, echo = FALSE, message = FALSE----
taxi <- read_benchmark(system.file("bench", "taxi-times.tsv", package = "vroom"), desc)

plot_benchmark(taxi, "Time to analyze taxi trip data")

make_table(taxi)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
all_num <- read_benchmark(system.file("bench", "all_numeric-times.tsv", package = "vroom"), desc)

plot_benchmark(all_num, "Time to analyze all numeric data")

make_table(all_num)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
all_chr <- read_benchmark(system.file("bench", "all_character-times.tsv", package = "vroom"), desc)

plot_benchmark(all_chr, "Time to analyze all character data")

make_table(all_chr)

## ---- echo = FALSE, message = FALSE--------------------------------------
mult <- read_benchmark(system.file("bench", "taxi_multiple-times.tsv", package = "vroom"), desc)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
plot_benchmark(mult, "Time to analyze multiple file data")

make_table(mult)

## ---- echo = FALSE, message = FALSE--------------------------------------
fwf <- read_benchmark(system.file("bench", "fwf-times.tsv", package = "vroom"), desc)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
plot_benchmark(fwf, "Time to analyze fixed width data")

make_table(fwf)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
desc_w <- rev(c("xz", "gzip", "multithreaded gzip", "zstandard", "uncompressed"))
taxi_writing <- read_benchmark(system.file("bench", "taxi_writing-times.tsv", package = "vroom"), desc_w) %>%
  mutate(package = factor(package, c("write.delim", "readr", "data.table", "vroom")))

subtitle <- generate_subtitle(taxi_writing)

taxi_writing %>%
  ggplot(aes(x = op, y = time, fill = package)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE, padding = .05)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_y_continuous(labels = scales::number_format(suffix = "s")) +
  theme(legend.position = "bottom") +
  coord_flip() +
  labs(title = "Writing taxi trip data", subtitle = subtitle, x = NULL, y = NULL, fill = NULL)

taxi_writing %>%
  select(-size, -rows, -cols, -type, -method, -altrep, -label) %>%
  mutate_if(is.numeric, pretty_sec) %>%
  spread(package, time) %>%
  arrange(desc(op)) %>%
  rename(method = op) %>%
  knitr::kable(digits = 2, align = "r", format = "html")

## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------
si <- vroom::vroom(system.file("bench", "sessioninfo.tsv", package = "vroom"))
class(si) <- c("packages_info", "data.frame")
select(as.data.frame(si), package, version = ondiskversion, date, source) %>%
  knitr::kable()

