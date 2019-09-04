setwd(dirname(parent.frame(2)$ofile))
source("global.R")
pkgs = c("zheatmap","ggrepel", "gtable")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
load("../../analysis/hdl/apps/app/data/data.rda")
## -------- my_theme -----------------------------------------------------------
text.size = 2.5
title.size = 9
my_theme = function(){
    theme_bw() +
        theme(
            text = element_text(family = "Helvetica"),
            axis.title = element_text(size = title.size),
            axis.text = element_text(size = title.size - 2, color = "black"),
            legend.title = element_text(size = title.size - 1),
            legend.text = element_text(size = title.size - 2),
            legend.key.size = unit(1, "line"),
            plot.title = element_text(hjust=0.5, size = title.size),
            strip.text = element_text(size = title.size),
            axis.line = element_blank(),
            panel.border = element_rect(color = "black", size = 0.75),
            axis.ticks = element_line(color = "black"),
            strip.background = element_rect(size = 0.75)
        )
}
theme_scatter = function(){
    my_theme()
}
theme_boxplot = function(){
    my_theme() +
        theme(
            axis.title.x = element_blank(),
            axis.text.x = element_text(colour = "black")
        )
}
## -------- p1: heatmap --------------------------------------------------------
limma = data$limma$lpd$feature$Proportion
ftrs = limma$padj <= 0.05
lset = subset_features(data$data$lpd$feature$Proportion, ftrs)
edata = as.data.frame(t(lset$conc_table)) %>%
    mutate(Timepoint = lset$sample_table$Timepoint,
           Treatment = lset$sample_table$Treatment,
           Subject = lset$sample_table$Subject) %>%
    melt(id.var = c("Subject", "Treatment", "Timepoint")) %>%
    dcast(Subject + Treatment + variable ~ Timepoint) %>%
    mutate(change = Post - Pre) %>%
    dcast(Subject + Treatment ~ variable, value.var = "change")
design = edata[,1:2]
edata = edata[,-(1:2)]
edata = apply(edata, 2, function(col) scale(col))
rownames(edata) = paste0(design$Subject, design$Treatment)
p1 = zheatmap(data=as.data.frame(t(edata)), scale = "none", 
         colSideBar = design$Treatment, 
         colors = colorRampPalette(colors = rev(brewer.pal(11, "RdBu")))(256),
         heights = c(4, 45), print = FALSE, text.size = title.size - 2) %>%
    gtable_add_padding(padding = unit(c(0.1,0,0.1,0.2), "in"))

## -------- cluster scatter ----------------------------------------------------
lset = data$data$lpd$feature$Proportion
hc = hclust(dist(t(apply(lset$conc_table, 1, scale))))
clusters = cutree(hc, h = 8)
mset = data$data$lpd$feature$Proportion
lset$feature_data$hclusters = clusters
lset = summarize_features(lset, "hclusters")
design = model.matrix(data = as(lset$sample_table, "data.frame"),
                      ~ Treatment*Timepoint + Subject + 1)
top = mSet_limma(lset, design, coef = 13, p.value = 13)
df = lset$conc_table %>%
    t %>% as.data.frame %>%
    mutate(
        Treatment = lset$sample_table$Treatment,
        Timepoint = lset$sample_table$Timepoint,
        Subject   = lset$sample_table$Subject
    ) %>%
    melt(id.var = c("Treatment", "Timepoint", "Subject"),
         variable.name = "cluster") %>%
    dcast(cluster + Subject + Treatment ~ Timepoint) %>%
    mutate(change = Post - Pre) %>%
    group_by(cluster, Treatment) %>%
    summarize(change = mean(change)) %>%
    dcast(cluster~Treatment) %>%
    mutate(pvalue = top$pvalue,
           padj = top$padj) %>%
    arrange(desc(pvalue))
rownames(df) = df$cluster

annot.clust = as.character(df$cluster[!(df$FF < 0.003 & df$FF > -0.003 & df$Med < 0.003 & df$Med > -0.003)])
circle = function(center = c(0,0), r = 1, npoints = 100){
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}
p.scatter = ggplot(df) +
    geom_vline(xintercept = 0, size = 0.25) + 
    geom_hline(yintercept = 0, size = 0.25) +
    geom_point(aes(x = FF, y = Med, fill = padj <= 0.05),
               size = 3, alpha = 0.75, shape = 21) +
    geom_text_repel(data = df[annot.clust, ], 
                    aes(x = FF, y = Med, label = cluster),
                    size = text.size, seed = 255) +    
    geom_text(data = df["1",], aes(FF, Med, label = cluster), 
              nudge_x = -0.00125, nudge_y = 0.0075, size = text.size) +
    annotate("segment", x = df["1", "FF"], y = df["1", "Med"],
             xend = df["1","FF"]-0.00125 * 0.75,
             yend = df["1","Med"]+ 0.0075 * 0.75) +
    geom_text(data = df["28",], aes(FF, Med, label = cluster),
              nudge_x = 0.005, nudge_y = -0.005, size = text.size) +
    annotate("segment", x=df["28", "FF"], y = df["28", "Med"],
             xend = df["28", "FF"] + 0.005 * 0.75,
             yend = df["28", "Med"] - 0.005 * 0.75) +
    # geom_path(data=circle(c(0,0), r=0.002), aes(x,y),
    #           color = "blue", linetype = "dashed") +
    scale_fill_manual(
        values = c("white", pal_npg()(1)),
        guide = guide_legend(title = "adj P < 0.05")
    ) +
    labs(x = "FF (change from baseline)",
         y = "Med (change from baseline)") +
    theme_scatter() +
    theme(legend.position = "bottom",
          panel.border = element_rect(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.text =  element_text(color = "black", size = title.size-3))

sig.clusters = lapply(df$cluster[df$padj <= 0.05], function(i){
    names(clusters[clusters==i])
})
names(sig.clusters) = df$cluster[df$padj <= 0.05]

glist = lapply(names(sig.clusters), function(x){
    grobTree(rectGrob(gp = gpar(col = "white")), 
             rectGrob(height = unit(1-2/(5*length(sig.clusters[[x]])), "npc"),
                      width =unit(0.95, "npc"),
                      gp = gpar(col = "grey")),
             textGrob(paste(sort(sig.clusters[[x]]), collapse = "\n"),
                      x = unit(0.1, "npc"),
                      gp = gpar(fontsize = title.size - 2),
                      hjust = 0),
             pointsGrob(x = unit(0.9, "npc"),
                        y =unit(1-1/(2*length(sig.clusters[[x]])), "npc"),
                        pch = 19, size = unit(1, "char"),
                        gp = gpar(col = pal_npg()(1), alpha = 0.75)),
             textGrob(x, x = unit(0.9, "npc"), 
                      y =unit(1-1/(2*length(sig.clusters[[x]])), "npc"),
                      gp = gpar(fontsize = title.size - 2,
                                col = "white"))
             )
})
names(glist) = names(sig.clusters)

panel.A = arrangeGrob(
    grob(rectGrob()), glist[["1"]], glist[["6"]], 
    nrow = 1, widths = c(0.325, 1, 1)
) 
panel.C = arrangeGrob(
    grob(rectGrob()), glist[["11"]], glist[["20"]], glist[["28"]],
    nrow = 2, layout_matrix = rbind(c(1,2,3), c(1,2,4)),
    heights = c(1,1), widths = c(0.325, 1, 1)
)
panel.D = arrangeGrob(
    grobs = glist[as.character(c(8, 12))],
    ncol = 1, heights=c(10, 15)
)
p2 = arrangeGrob(
    panel.A, p.scatter, panel.C, panel.D,
    ncol = 2, nrow = 3, heights = c(5,15,6), widths = c(3,1),
    layout_matrix = rbind(c(1,4), c(2,4), c(3,4))
)
## -------- boxplot ------------------------------------------------------------
design = model.matrix(data = as(lset$sample_table, "data.frame"),
                      ~ Treatment*Timepoint + Subject + 1)
limma_hc = mSet_limma(lset, design, coef = 13, p.value = 13)
rownames(limma_hc) = featureNames(lset)
clusters_to_plot = as.character(rownames(limma_hc[limma_hc$padj < 0.05,]))
lset = transform_by_sample(lset, function(x) x*100)
boxplots = lapply(clusters_to_plot, function(xx){
    p = limma_hc[xx,"padj"]
    if( p < 0.001)
        title = str_c("cluster ", xx, " (adj P < ", 0.001, ")")
    else
        title = str_c("cluster ", xx, " (adj P = ", round(p, digits = 3),")")
    plot_boxplot(lset, x = "Timepoint", feature = xx,
                 cols = "Treatment", line = "Subject", point.size = 2,
                 color = "Subject", color.pal = pal_npg()(10)) +
        labs(title = title,
             x = "", y = "mg %") +
        theme_boxplot() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = title.size - 3),
              legend.key.size =  unit(1, "line")) +
        guides(color = guide_legend(title.position = "top", title.hjust = 0.5,
                                    label.position = "bottom"))
})
lgd = get_legend(boxplots[[1]])
boxplots = lapply(boxplots, function(p) p+theme(legend.position = "none"))
boxplots = align_plots(plotlist = boxplots, align = "hv", axis = "tblr")
## -------- save ---------------------------------------------------------------

panel.middle = plot_grid(
    lgd, boxplots[[5]], boxplots[[6]], boxplots[[7]], nrow = 4,
    rel_heights = c(1.5, 2, 2 ,2),
    labels = c("", LETTERS[6:8]), label_size = title.size - 1,
    label_x = 0.1, label_y = 0.9
)
panel.bottom = plot_grid(p1, panel.middle, grob(rectGrob()), p2, 
                         nrow=1, rel_widths = c(1.4,1, 0.05,1.5)) +
    draw_label("A", x = 0.03, y = 0.975, size = title.size-1, fontface = "bold") +
    draw_label("I", x = 0.60, y = 0.8,  size = title.size-1, fontface = "bold")
panel.top = plot_grid(plotlist = boxplots[1:4], nrow = 1,
                      labels = LETTERS[2:5], label_size = title.size - 1,
                      label_x = 0.1, label_y = 0.9)
p = plot_grid(panel.top, panel.bottom, nrow = 2, rel_heights = c(2,7.5))

plotlist = boxplots
plotlist$lgd = lgd

p = plot_grid(
    plot_grid(
        p1, p2, nrow = 1,
        labels = c("A", "B"), label_x = 0.03, label_y =0.975, 
        label_size = title.size - 1
    ),
    plot_grid(
        plotlist = plotlist, nrow = 2,
        labels = LETTERS[3:9],label_size = title.size - 1,
        label_x = 0.1, label_y = 0.9
    ),
    nrow = 2, rel_heights = c(3,2)
)

# ggsave("img/fig3_lpd_species.png", p,
#        width = 8, height = 8, units = "in", dpi = 300)

filename = "../img/pdf/fig2_lpd_species.pdf"
ggsave(filename, p, scale = 1.4,
       width = 140, height = 140, units = "mm", dpi = 1000)
extrafont::embed_fonts(filename, outfile = filename)