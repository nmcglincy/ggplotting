fixed_panel_size <- function(p = NULL, 
  g = ggplotGrob(p), 
  file = NULL,
  w = 5,
  h = 5){
  # based on function take from:
  # http://stackoverflow.com/questions/30571198/how-achieve-identical-facet-sizes-and-scales-in-several-multi-facet-ggplot2-grah/30571289#30571289
  require(ggplot2)
  require(grid)

  margin = unit(1,"mm"),
  width=unit(w, "in"), 
  height=unit(h, "in")

  panels <- grep("panel", g$layout$name)
  panel_index_w<- unique(g$layout$l[panels])
  panel_index_h<- unique(g$layout$t[panels])
  nw <- length(panel_index_w)
  nh <- length(panel_index_h)
  
  # the following conversion is necessary
  # because there is no `[<-`.unit method
  # so promoting to unit.list allows standard list indexing
  g$widths <- grid:::unit.list(g$widths)
  g$heights <- grid:::unit.list(g$heights)
  
  g$widths[panel_index_w] <-  rep(list(width), nw)
  g$heights[panel_index_h] <-  rep(list(height), nh)
  class(g) <- c("fixed", class(g), "ggplot")
  if(!is.null(file))
    ggsave(file, g, 
           width = convertWidth(sum(g$widths) + margin, 
            unitTo = "in", 
            valueOnly = TRUE),
           height = convertHeight(sum(g$heights) + margin, 
            unitTo = "in", 
            valueOnly = TRUE))
  g
}

# print.fixed <- function(x) grid.draw(x)