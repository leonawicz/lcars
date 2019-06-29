lcars_2357 <- data.frame(
  series = 2357L,
  name = c("pale-canary", "neon-carrot", "golden-tanoi", "lilac", "eggplant", "anakiwa", "mariner", "bahama-blue"),
  value = c("#ffff99", "#ff9933", "#ffcc66", "#cc99cc", "#664466", "#99ccff", "#3366cc", "#006699"),
  stringsAsFactors = FALSE
)

lcars_2369 <- data.frame(
  series = 2369L,
  name = c("blue-bell", "melrose", "lilac", "hopbush", "chestnut-rose", "orange-peel", "atomic-tangerine", "pale-canary"),
  value = c("#9999cc", "#9999ff", "#cc99cc", "#cc6699", "#cc6666", "#ff9966", "#ff9900", "#ffff99"),
  stringsAsFactors = FALSE
)

lcars_2375 <- data.frame(
  series = 2375L,
  name = c("danub", "indigo", "lavender-purple", "cosmic", "red-damask", "medium-carmine", "bourbon", "sandy-brown"),
  value = c("#6688cc", "#4455bb","#9977aa", "#774466", "#dd6644", "#aa5533", "#bb6622", "#ee9955"),
  stringsAsFactors = FALSE
)

lcars_2379 <- data.frame(
  series = 2379L,
  name = c("periwinkle", "dodger-pale", "dodger-soft", "near-blue", "navy-blue", "husk", "rust", "tamarillo"),
  value = c("#ccddff", "#5599ff", "#3366ff", "#0011ee", "#000088", "#bbaa55", "#bb4411", "#882211"),
  stringsAsFactors = FALSE
)

lcarsColors <- do.call(rbind, list(lcars_2357, lcars_2369, lcars_2375, lcars_2379))

usethis::use_data(lcarsColors)
