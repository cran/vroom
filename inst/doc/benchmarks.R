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
      package = fct_reorder(sub("_", "\n", package), time, sum),
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
    geom_bar(aes(x = package, y = time, fill = op, group = package), stat = "identity") +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    scale_y_continuous(labels = function(x) format(bench::as_bench_time(x))) +
    theme(legend.position = "bottom") +
    coord_flip() +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, fill = NULL)
}

make_table <- function(data) {
  times <- data %>%
    group_by(package, op) %>%
    filter(type == "real") %>%
    tally(wt = time) %>%
    spread(op, n) %>%
    mutate(total = sum(read, print, head, tail, sample, filter, aggregate))

  times %>%
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

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
desc_w <- c("uncompressed", "gzip", "multithreaded gzip")
taxi_writing <- read_benchmark(system.file("bench", "taxi_writing-times.tsv", package = "vroom"), desc_w) %>%
  spread(op, time) %>%
  arrange(!is.na(`multithreaded gzip`), desc(`multithreaded gzip`)) %>%
  mutate(package = fct_rev(fct_inorder(package)))

subtitle <- generate_subtitle(taxi_writing)

taxi_writing %>%
  select(-size, -rows, -cols, -type) %>%
  gather(op, time, -package) %>%
  mutate(op = factor(op, desc_w)) %>%
  ggplot() +
  geom_bar(aes(x = package, y = time, fill = op), stat = "identity", position = position_dodge2(preserve = "single", reverse = TRUE)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_y_continuous(labels = function(x) format(bench::as_bench_time(x))) +
  theme(legend.position = "bottom") +
  coord_flip() +
  labs(title = "Writing taxi trip data", subtitle = subtitle, x = NULL, y = NULL, fill = NULL)

taxi_writing %>%
  select(-size, -rows, -cols, -type) %>%
  mutate_if(is.numeric, pretty_sec) %>%
  knitr::kable(digits = 2, align = "r", format = "html")

## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------
si <- vroom::vroom(system.file("bench", "sessioninfo.tsv", package = "vroom"))
class(si) <- c("packages_info", "data.frame")
select(as.data.frame(si), package, version = ondiskversion, date, source) %>%
  knitr::kable()

