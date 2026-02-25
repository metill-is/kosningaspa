source(here("R", "data_prep.R"))

# Define the color palette
party_colors <- c(
    "D"      = "#00aef0",
    "B"      = "#01422a",
    "C"      = "#ff7d17",
    "F"      = "#ffc93d",
    "J"      = "#ED1C24",
    "M"      = "#13216a",
    "P"      = "#060606",
    "S"      = "#ee1400",
    "V"      = "#508B2B",
    "others" = "#808080"
)

# gallup poll results (raw count)
gallup_party_support |>
    group_by(party, date) |>
    summarize(count = prod(support)) |>
    ggplot() +
    geom_line(aes(date, count, color = party), linewidth = 1) +
    scale_color_manual(values = party_colors)

# gallup poll results (prop)
party_support |>
    group_by(party, date) |>
    filter(type == "prop") |>
    ggplot() +
    geom_line(aes(date, support, color = party), linewidth = 1) +
    scale_color_manual(values = party_colors)
