# Dependencies ------------------------------------------------------------

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(Vennerable))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(rmarkdown))

# Globals -----------------------------------------------------------------

get_groups_by_factor <- function(data_frame, factor_var, identifier = "Projekt") {
  lvls <- levels(data_frame[[factor_var]])
  groups <- list()
  length(groups) <- length(lvls)
  names(groups) <- lvls

  tmp <- dplyr::select_(data_frame, identifier, factor_var)

  for (lvl in lvls) {
    groups[[lvl]] <- dplyr::filter(tmp, tmp[[factor_var]] == lvl) %>%
      getElement(identifier)
  }
  return(groups)
}

get_groups_by_cols <- function(data_frame, col_names, identifier = "Projekt") {
  groups <- list()
  length(groups) <- length(col_names)
  names(groups) <- col_names

  tmp <- dplyr::select(data_frame, dplyr::one_of(c(identifier, col_names)))

  for (lvl in col_names) {
    groups[[lvl]] <- dplyr::filter(tmp, tmp[[lvl]]) %>%
      getElement(identifier)
  }
  return(groups)
}

# Data --------------------------------------------------------------------

platforms <- readr::read_csv("plattformen.csv")

type_cols <- c("Händische Liste",
               "Link-/Projektsammlung",
               "Sachspendenplattform",
               "Zeitspendenplattform",
               "Vernetzungsplattform")
platforms[, type_cols] <- platforms[, type_cols] == "1"

target_cols <- names(platforms)[dplyr::starts_with("ZG", vars = names(platforms))]
platforms[, target_cols] <- platforms[, target_cols] == "ja"

factor_cols <- c("Typ", "Stadt", "Localization")
for (col in factor_cols) {
  platforms[[col]] <- factor(platforms[[col]])
}

# Sanity Checks -----------------------------------------------------------

diff <- length(platforms$Projekt) - length(unique(platforms$Projekt))
if (diff > 0) {
  abort(sprintf("%d non-unique project names", diff))
}
diff <- length(platforms$Webseite) - length(unique(platforms$Webseite))
if (diff > 0) {
  abort(sprintf("%d non-unique project urls", diff))
}

# Projects by Type --------------------------------------------------------

types <- get_groups_by_cols(platforms, type_cols)

# Projects by Target Group ------------------------------------------------

target_groups <- get_groups_by_cols(platforms, target_cols)
names(target_groups) <- sapply(names(target_groups), substring, 4)

# Projects by Origin ------------------------------------------------------

cities <- get_groups_by_factor(platforms, "Stadt")

# Projects by Localization ------------------------------------------------

localizations <- get_groups_by_factor(platforms, "Localization")

# Pairwise Overlap --------------------------------------------------------

pairs <- utils::combn(names(types), 2, simplify = FALSE)
inter <- numeric(length(pairs))
for (i in 1:length(pairs)) {
  a <- types[[pairs[[i]][1]]]
  b <- types[[pairs[[i]][2]]]
  inter[i] <- length(intersect(a, b))
}
tibble(Paar = sapply(pairs, paste, sep = "", collapse = ", "), Schnittmenge = inter)

# Report ------------------------------------------------------------------

rmarkdown::render(
  "platform_report_de.Rmd",
  output_format = "all",
  params = list(
    types = types,
    target_groups = target_groups,
    origins = cities,
    reach = localizations
  )
)
