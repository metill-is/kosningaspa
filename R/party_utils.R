#' @export
party_tibble <- function() {
  box::use(
    dplyr[tribble]
  )
  tribble(
    ~flokkur, ~bokstafur, ~litur,
    "Sjálfstæðisflokkurinn", "D", "#377eb8",
    "Framsóknarflokkurinn", "B", "#41ab5d",
    "Samfylkingin", "S", "#e41a1c",
    "Vinstri Græn", "V", "#00441b",
    "Viðreisn", "C", "#ff7d14",
    "Píratar", "P", "#984ea3",
    "Miðflokkurinn", "M", "#08306b",
    "Flokkur Fólksins", "F", "#FBB829",
    "Sósíalistaflokkurinn", "J", "#67000d",
    "Annað", "Other", "grey50"
  )
}
