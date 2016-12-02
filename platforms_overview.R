# Dependencies ------------------------------------------------------------

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))

# Globals -----------------------------------------------------------------

get_groups <- function(data_frame, variable, identifier = "Projekt") {
  lvls <- levels(data_frame[[variable]])
  groups <- list()
  length(groups) <- length(lvls)
  names(groups) <- lvls

  tmp <- dplyr::select_(data_frame, identifier, variable)

  for (lvl in lvls) {
    groups[[lvl]] <- dplyr::filter(tmp, tmp[[variable]] == lvl) %>%
      getElement(identifier)
  }
  return(groups)
}

# Data --------------------------------------------------------------------

platforms <- readr::read_csv("social collective Vernetzungsplattformen - csv_Vorlage.csv")

type_cols <- c("Händische Liste",
               "Link-/Projektsammlung",
               "Sachspendenplattform",
               "Zeitspendenplattform",
               "Vernetzungsplattform")
platforms[, type_cols] <- platforms[, type_cols] == "1"

target_cols <- dplyr::starts_with("ZG", vars = names(platforms))
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

types = list()
length(types) = length(type_cols)
names(types) = type_cols

tmp <- dplyr::select(platforms, Projekt, one_of(type_cols))

for (lvl in type_cols) {
  types[[lvl]] <- dplyr::filter(tmp, tmp[[lvl]]) %>%
    getElement("Projekt")
}

# Projects by Target Group ------------------------------------------------

target_lvls = names(platforms)[target_cols]
target_groups = list()
length(target_groups) = length(target_lvls)
names(target_groups) = target_lvls

tmp <- dplyr::select(platforms, Projekt, target_cols)

for (lvl in target_lvls) {
  target_groups[[lvl]] <- dplyr::filter(tmp, tmp[[lvl]]) %>%
    getElement("Projekt")
}

# Projects by Origin ------------------------------------------------------

cities <- get_groups(platforms, "Stadt")

# Projects by Localization ------------------------------------------------

localizations <- get_groups(platforms, "Localization")

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
