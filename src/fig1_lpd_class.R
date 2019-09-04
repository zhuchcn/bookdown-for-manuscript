setwd(dirname(parent.frame(2)$ofile))
source("global.R")
pkgs = c("ggtree", "treeio","gridExtra", "glue")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
load("data.rda")
load("data/similar_matrix.rda")
## -------- p1: qc mean vs sd --------------------------------------------------
p1 = plot_qc(data$data$lpd$feature$Proportion,
             mean = "qc_mean", sd = "qc_sd") +
    theme_scatter()

## -------- p2: qc mean vs cv --------------------------------------------------
p2 = plot_qc(data$data$lpd$feature$Proportion,
             mean = "qc_mean", cv = "qc_cv") +
    theme_scatter()

## -------- p3: pie chart ------------------------------------------------------
df = data$data$lpd$class$Proportion$conc_table %>%
    t %>% as.data.frame %>%
    rownames_to_column("id") %>%
    melt(id.var = "id") %>%
    group_by(variable) %>%
    summarize(mean = mean(value)) %>%
    arrange(desc(mean)) %>%
    mutate(variable = factor(variable, levels = variable),
           offset = rev(cumsum(rev(mean))) - mean / 2)
set.seed(25)
color_pal = colorRampPalette(pal_npg()(9))(9)[sample(1:9)]
p3 = ggplot(df, aes(x = 1, y = mean, fill = variable)) +
    geom_bar(stat = "identity", color = "white") +
    geom_text(data=df[1:6,], 
              aes(x = c(rep(1.1, 5), 1.2), y = offset, 
                  label = paste(format(mean * 100, digits = 2) , "%")),
              color = "white", size = text.size-1) +
    coord_polar("y") +
    #facet_grid(rows = "Treatment") +
    scale_fill_manual(values = color_pal) +
    guides(fill = guide_legend(ncol = 2)) +
    my_theme() +
    theme(
        # legend
        legend.title = element_blank(),
        # plot
        panel.border = element_blank(),
        panel.grid = element_blank(),
        # scales
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
    )

## -------- my boxplot function ------------------------------------------------
my_boxplot = function(df) {
    return(
        ggplot(df, aes(x = Timepoint, y = value)) +
            geom_boxplot() +
            stat_boxplot(geom="errorbar") +
            geom_point(aes(color = Subject)) +
            geom_line(aes(group = Subject, color = Subject)) +
            scale_color_npg() +
            facet_grid(~Treatment) +
            theme_boxplot()
    )
}

## -------- p4: boxplot PE -----------------------------------------------------
p = data$limma$lpd$class$Proportion["PE","pvalue"]

p4 = data.frame(
    value = data$data$lpd$class$Proportion$conc_table["PE",] * 100,
    Treatment = data$data$lpd$class$Proportion$sample_table$Treatment,
    Timepoint = data$data$lpd$class$Proportion$sample_table$Timepoint,
    Subject = data$data$lpd$class$Proportion$sample_table$Subject
) %>%
    my_boxplot +
    labs(x = "", y = "mg %", title = glue("PE (p < 0.001)"))
    
legend = get_legend(p4)
p4 = p4 + theme(legend.position = "none")
## -------- p5: boxplot SM -----------------------------------------------------
p = data$limma$lpd$class$Proportion["SM", "pvalue"]
p = round(p, 3)
p5 = data.frame(
    value = data$data$lpd$class$Proportion$conc_table["SM",] * 100,
    Treatment = data$data$lpd$class$Proportion$sample_table$Treatment,
    Timepoint = data$data$lpd$class$Proportion$sample_table$Timepoint,
    Subject = data$data$lpd$class$Proportion$sample_table$Subject
) %>%
    my_boxplot +
    labs(x = "", y = "mg %", title = glue("SM (p={p})")) +
    theme(legend.position = "none")
## -------- histograme ---------------------------------------------------------
p6 = ggplot(data$limma$lpd$feature$Proportion, 
           aes(pvalue)) +
    geom_histogram(colour = "white", fill=pal_lancet()(9)[1], bins=30) +
    geom_vline(xintercept = 0.05, color=pal_lancet()(9)[2], 
               linetype = "dashed", size=0.5) +
    annotate("text", x=0.2, y=30, label = "P=0.05", size=2.5) +
    labs(x="P Value (unadjusted)", y = "Number of lipid species") +
    my_theme() +
    theme(
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_line(size = 0.5),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        plot.margin = margin(l=10,r=10)
    )
## -------- p7 cladogram -------------------------------------------------------
lset = data$data$lpd$feature$Proportion
# Construct the tree using hclust
hc = hclust(dist(tanimoto.mat), method="complete")
tree = as.phylo(hc)

# annotation data 1, with lipid class
class.dat = data.frame(
    class = lset$feature_data$class[lset$feature_data$Annotation == tree$tip.label], 
    stringsAsFactors = F
)
rownames(class.dat) = tree$tip.label

# annotation data 2, with change after treatment
change.dat = t(lset$conc_table) %>% as.data.frame %>% 
    mutate(Treatment = lset$sample_table$Treatment,
           Timepoint = lset$sample_table$Timepoint,
           Subject   = lset$sample_table$Subject) %>%
    melt(id.var = c("Treatment", "Timepoint", "Subject")) %>%
    dcast(Subject + Treatment + variable ~ Timepoint) %>%
    mutate(change = (Post - Pre)/(Post + Pre) * 2) %>%
    # mutate(change = log2(Post/Pre +1)) %>% # seems the log2 FC doesn't work so well
    dcast(Subject + variable ~ Treatment, value.var = "change") %>% 
    group_by(variable) %>%
    summarize(Med = mean(Med), FF = mean(FF)) %>%
    arrange(variable == tree$tip.label) %>%
    as.data.frame %>%
    mutate( Med = scale(Med), FF = scale(FF) ) %>%
    column_to_rownames("variable")

anno = data.frame(
    class = class.dat$class,
    FF = change.dat$FF,
    Med = change.dat$Med,
    label = tree$tip.label
) 
min = min(min(anno$FF), min(anno$Med))
max = max(max(anno$FF), max(anno$Med))

anno = anno %>%
    mutate(FF = cut(FF, breaks = seq(min, max, length.out = 100)),
           Med = cut(Med, breaks = seq(min, max, length.out = 100))) %>%
    column_to_rownames("label")

colors = c(
    pal_npg()(9), 
    colorRampPalette(rev(brewer.pal(11, "RdBu")))(100)
)
names(colors) = c(levels(anno$class), levels(anno$FF))

anno.t = rownames_to_column(anno, "label") %>%
    melt(id.vars = "label")

p7 = ggtree(tree) %>% 
    ggtree::rotate(172) %>% ggtree::rotate(179) %>% ggtree::rotate(192)
p7 = gheatmap(p7, anno, offset = 0, width = 0.3, font.size = text.size) +
    scale_fill_manual(values = colors[anno.t$value], guide = F)+
    #theme(legend.position =  "none") +
    coord_polar(theta = "y")

lgd1 = change.dat %>%
    rownames_to_column("id") %>%
    melt(id.vars = "id") %>%
    ggplot() +
    geom_tile(aes(x = variable, y = id, fill = value)) +
    scale_fill_gradientn(
        colours = colorRampPalette(rev(brewer.pal(11, "RdBu")))(100) ) +
    guides(fill = guide_colorbar(title = "Fold Change")) +
    my_theme()
lgd1 = get_legend(lgd1)
lgd2 = class.dat %>%
    rownames_to_column("id") %>%
    ggplot() +
    geom_tile(aes(x=1, y = id, fill = class)) +
    scale_fill_npg() +
    my_theme()
lgd2 = get_legend(lgd2)

p7 = arrangeGrob(p7, lgd1, lgd2, layout_matrix = rbind(c(1,2),c(1,3)),
                 widths = c(5,1))
## -------- pca ----------------------------------------------------------------
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
prComp = prcomp(edata)
sdev = prComp$sdev / sum(prComp$sdev)
df = data.frame(
    PC1 = prComp$x[, "PC1"],
    PC2 = prComp$x[, "PC2"],
    PC3 = prComp$x[, "PC3"],
    Treatment = design$Treatment,
    Subject = design$Subject
)
p8 = ggplot(df, aes(x=PC1, y = PC2, color = Treatment)) +
    geom_point() +
    stat_ellipse() +
    labs(x = str_c("PC1 [ ", round(sdev[1] * 100, 1), "% ]" ),
         y = str_c("PC1 [ ", round(sdev[2] * 100, 1), "% ]" )) +
    scale_color_lancet() +
    my_theme()
legend2 = get_legend(p8)
p8 = p8 + theme(legend.position = "none")
df = data.frame(
    PC1 = prComp$rotation[, "PC1"],
    PC2 = prComp$rotation[, "PC2"]
) %>% 
    rownames_to_column("Feature")

p9 = ggplot(df, aes(x = PC1, y = PC2)) +
    geom_text(aes(label = Feature), check_overlap = TRUE, size = 2) +
    # geom_text_repel(aes(label = Feature), segment.size = 0, size = 1.5) +
    scale_x_continuous(limits = c(-0.23,0.22)) +
    my_theme()
## -------- align --------------------------------------------------------------
p12 = align_plots(p1, p2, align = "hv", axis = "tblr")
# p45 = align_plots(p4, p5, align = "hv", axis = "tblr")
p89 = align_plots(p8, p9, align = "h", axis = "tblr")

p = plot_grid(
    plot_grid(
        plot_grid(
            plot_grid(
                p12[[1]], p12[[2]], rel_widths = c(1,1,1.25), nrow = 1,
                labels = c("A", 'B'), label_size = title.size, hjust = -2, vjust = 2
            ),
            p3,
            nrow = 2, labels = c("", "C"), label_size = title.size, hjust = -2, vjust = 2
        ), 
        p7, nrow = 1, labels = c("", "E"), rel_widths = c(1,1.2),
        label_size = title.size, hjust = -2, vjust = 2
    ),
    plot_grid(
        p4, p6, p89[[1]], legend2, p89[[2]], nrow = 1, 
        labels = c("D", "F", "G", "", "H"), label_size = title.size,
        hjust = -2, vjust = 2, rel_widths = c(1,1,1,0.3,1)
    ),
    nrow = 2, rel_heights = c(2,1)
)

# ggsave("../img/fig2_lpd_class.png", p,
#        height = 7, width = 12, units = "in", dpi = 300)

filename = "../img/pdf/fig1_lpd_class.pdf"
ggsave(filename, p, scale = 1.75,
       height = 110, width = 190, units = "mm",
       dpi = 1000)
#extrafont::embed_fonts(filename, outfile = filename)