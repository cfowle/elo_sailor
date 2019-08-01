library(ggplot2)

limesight_mellow_colors = c("#065143", "#57AC6E", "#95D7AE", "#38AECC", "#0090C1")
limesight_bright_colors = c("#57AC6E", "#0090C1", "#3066BE", "#963484", "#E83F6F")
limesight_grey = c("#8C96AA", "#71788C", "#525E72", "#3D4554", "#333847")
limesight_sailing = c("#A5C8D3", "#c32a30", "#002f55")

limesight_palettes = list(
  "main"   = limesight_mellow_colors,
  "bright" = limesight_bright_colors,
  "grey"   = limesight_grey,
  "sailing"= limesight_sailing
)

limesight_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- limesight_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_color_limesight <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- limesight_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("limesight_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_limesight <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- limesight_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("limesight_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

theme_limesight = function()
{theme_minimal(base_size = 12, base_family=c("Avenir"))%+replace%
    theme(
      plot.title = element_text(size = 18, 
                                face = "bold",
                                margin = margin(0, 0, 15, 0),
                                vjust = 0,
                                hjust=0.5),
      axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
      axis.title.y = element_text(margin = margin(0, 10, 0, 0), angle = 90)
    )}
