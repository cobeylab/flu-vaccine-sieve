ten_colors = c(
  'blue' = "#1f77b4",
  'orange' = "#ff7f0e",
  'green' = "#2ca02c",
  'red' = "#d62728",
  'purple' = "#9467bd",
  'brown' = "#8c564b",
  'pink' = "#e377c2",
  'grey' = "#7f7f7f",
  'sick' = "#bcbd22",
  'turquoise' = "#17becf"
)

tencol_palette <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ten_colors
  colorRampPalette(pal, ...)
}

scale_color_tencol = function(palette = 'main', ...) {
  pal <- tencol_palette(palette = palette)
  discrete_scale("colour", paste0("tencol_", palette), palette = pal, ...)
}

trvb = c(
  'purple'  = '#6818bf',
  'orange1' = '#e8993b',
  'lime' = '#92cb6f',
  'skyblue' = '#528ecf',
  'red' = '#ef0021', 
  'corn' = '#ddba47', 
  'seagreen' = '#6dc390',
  'blue' = '#595bd7',
  'orange' = '#f45f2f',
  'sick' = '#bac957',
  'strange' = '#56b0b4'
)

trvb_palette <- function(palette = "main", reverse = FALSE, ...) {
  pal <- trvb
  colorRampPalette(pal, ...)
}

scale_color_trvb = function(palette = 'main', ...) {
  pal <- trvb_palette(palette = palette)
  discrete_scale("colour", paste0("trvb_", palette), palette = pal, ...)
}

  