setwd(dirname(parent.frame(2)$ofile))
source("global.R")
pkgs = c("latex2exp", "Cairo")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
load("data/data.rda")
## -------- eod ----------------------------------------------------------------
p.eod = plot_boxplot(
    data$data$lpd$summarize$EOD18,
    x = "Timepoint", feature = "PC", cols = "Treatment",
    color = "Subject", line = "Subject", color.pal = pal_npg()(10)
) +
    guides(color=guide_legend(title.position = "left", label.position = "right",
                              nrow=1)) +
    labs(title = TeX("PC EOD_{18} ( P < 0.001 )"),
         y = "Equiv of double bonds\n(18 carbon)") +
    theme_boxplot() +
    theme(legend.direction = "horizontal")
lgd = get_legend(p.eod)
p.eod = p.eod + theme(legend.position = "none")
## -------- acl ----------------------------------------------------------------
p.acl = plot_boxplot(
    data$data$lpd$summarize$ACL,
    x = "Timepoint", feature = "PC", cols = "Treatment",
    color = "Subject", line = "Subject", color.pal = pal_npg()(10)
) +
    labs(title = "PC ACL ( P = 0.001)",
         y = "Average Chain Length") +
    theme_boxplot() +
    theme(legend.position = "none")
## -------- odd ----------------------------------------------------------------
p.odd = plot_boxplot(
    data$data$lpd$summarize$`Odd Chain`,
    x = "Timepoint", feature = "PC", cols = "Treatment",
    color = "Subject", line = "Subject", color.pal = pal_npg()(10)
) +
    labs(title = 'OCFA-PC ( P < 0.001)',
         y = "Abundance (umol/ml)") +
    theme_boxplot() +
    theme(legend.position = "none")

## -------- odd ----------------------------------------------------------------
oddset = data$data$lpd$summarize$`Odd Chain`

df = data.frame(
    lipid = featureNames(oddset),
    amt = rowMeans(oddset$conc_table),
    stringsAsFactors = F
) %>%
    mutate(
        lipid = factor(lipid, levels = c("PC", "SM", "TG", "Cer", "Overall")))
df$label = sapply(df$amt, function(amt){
    b = floor(log10(amt))
    a = round(amt / 10 ^ b, 2)
    str_c("$", a, " \\times 10^{", b, "}$")
})

design.ff = model.matrix(data = subset(oddset$sample_table, Treatment == "FF"),
                         ~ Timepoint + Subject + 1)
design.med = model.matrix(data = subset(oddset$sample_table, Treatment == "Med"),
                          ~ Timepoint + Subject + 1)

limma.odd.ff = mSet_limma(
    subset_samples(oddset, oddset$sample_table$Treatment == "FF"),
    design.ff, p.value = 2, coef = 2
)
limma.odd.med = mSet_limma(
    subset_samples(oddset, oddset$sample_table$Treatment == "Med"),
    design.med, p.value = 2, coef = 2
)

df2 = data.frame(
    row.names = rownames(limma.odd.ff),
    logFC.ff = limma.odd.ff$logFC,
    logFC.med = limma.odd.med$logFC,
    p.ff = limma.odd.ff$pvalue,
    p.med = limma.odd.med$pvalue
) %>%
    rownames_to_column("lipid") %>%
    filter(lipid != "Overall") %>%
    melt(id.var = "lipid") %>%
    mutate(
        coef = str_split_fixed(variable, "\\.",2)[,1],
        tx = str_split_fixed(variable, "\\.",2)[,2]
    ) %>%
    dcast(lipid + tx ~ coef, value.var = "value") %>%
    mutate(
        label = ifelse(p > 0.05, "-", ""),
        lipid = factor(lipid, levels = c("PC", "SM", "TG", "Cer", "Overall"))
    )

pa = ggplot(df[df$lipid != "Overall",]) +
    geom_col(aes(x = lipid, y = amt), width = 0.5, fill = "grey17") +
    geom_text(aes(x = lipid, y = amt + 0.0005, label = TeX(label, output = "character")),
              size = text.size - 1, parse = TRUE) +
    labs(title = "OCFA-associated Lipids",
         y = "umol/ml") +
    my_theme() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
pb = ggplot(df2) +
    geom_text(aes(x = lipid, y = tx, label = label), fontface = "bold") +
    geom_segment(aes( x = 1, y = 0.7, xend = 1, yend = 1.3), 
                 arrow = arrow(length = unit("1.5", "mm"), type = "closed"), size = 0.25) +
    geom_segment(aes( x = 1, y = 2.3, xend = 1, yend = 1.7), 
                 arrow = arrow(length = unit("1.5", "mm"), type = "closed"), size = 0.25) +
    scale_y_discrete(labels = c("FF", "Med")) +
    my_theme() +
    theme(
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black")
    )
podd2 = align_plots(pa, pb, align = "v", axis = "lr")
podd2 = plot_grid(plotlist = podd2, nrow = 2, rel_heights = c(2.5,1))

## -------- align and save -----------------------------------------------------
p = plot_grid(
    plot_grid(p.eod, p.acl, nrow = 1, labels = c("A","B"),
              label_x=0.01, label_y = 0.98, label_size = title.size),
    plot_grid(p.odd, podd2, nrow = 1, labels = c("C","D"),
              label_x=0.01, label_y = 0.98, label_size = title.size),
    lgd,
    nrow = 3,
    
    rel_heights = c(3,3,1)
)

# ggsave("../img/fig3_lpd_eod.png", p,
#        height = 5.8, width = 6.5, units = "in", dpi = 300)

filename = "../img/pdf/fig3_lpd_eod.pdf"
ggsave(filename, p, scale = 1.75, device = cairo_pdf,
       height = 80, width = 90, units = "mm", dpi = 1000)
#extrafont::embed_fonts(filename, outfile = filename)