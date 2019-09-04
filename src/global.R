pkgs = c('dplyr','stringr','reshape2', 'tibble', 'ggplot2','ggsci', 'grid', 
         'cowplot', "Metabase", "ggmetaplots","RColorBrewer","gridExtra", 
         "zheatmap", "MatCorR")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
load("../../analysis/hdl/apps/app/data/data.rda")
## -------- my_theme -----------------------------------------------------------
text.size = 3.5
title.size = 11
my_theme = function(){
    theme_bw() +
        theme(
            axis.title = element_text(size = title.size),
            axis.text = element_text(size = title.size - 2),
            legend.title = element_text(size = title.size - 1),
            legend.text = element_text(size = title.size - 2),
            legend.key.size = unit(1, "line"),
            plot.title = element_text(hjust=0.5, size = title.size),
            strip.text = element_text(size = title.size)
        )
}
theme_scatter = function(){
    my_theme() +
        theme(legend.position = "non",
              axis.title.y = element_text(vjust = -1.5))
}
theme_boxplot = function(){
    my_theme() +
        theme(
            axis.title.x = element_blank(),
            axis.text.x = element_text(colour = "black")
        )
}