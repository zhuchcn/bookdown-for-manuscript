setwd(dirname(parent.frame(2)$ofile))
source("global.R")
## -------- scatterplot: lipid ratios vs Apo A1 --------------------------------
df = data.frame(
    apoa1 = data$data$fct$conc_table["HDL ApoA1",],
    pc.lpc = data$data$lpd$summarize$`Lipid Ratios`$conc_table["PC/LPC",],
    ce.fc = data$data$lpd$summarize$`Lipid Ratios`$conc_table["CE/Cholesterol",],
    surface.core = data$data$lpd$summarize$`Lipid Ratios`$conc_table["surface/core",],
    Subject = data$data$fct$sample_table$Subject
)
## pc/lpc vs ce/fc
cor = corTest(df$pc.lpc, df$ce.fc, "spearman")
p = round(cor[3], digits = 3) 
r = round(cor[2], digits = 3)
p.pclpc.cefc = ggscatterplot(df, x = "pc.lpc", y = "ce.fc", color = "Subject",
                             color.pal = pal_npg()(10)) + 
    annotate(geom = "text", x = 100, y = 3.5, size = text.size,
             label = str_c("p = ", p, "\nr = ", r), hjust = 0) +
    labs(x = "PC/LPC",
         y = "CE/Cholesterol") + 
    theme_scatter() +
    theme(legend.position = "none")
## surface/core vs ce/fc
cor = corTest(df$surface.core, df$ce.fc, "spearman")
r = round(cor[2], digits = 3)
p.surfacecore.cefc = ggscatterplot(
    df, x = "surface.core", y = "ce.fc", color = "Subject",
    color.pal = pal_npg()(10)
) + 
    annotate(geom = "text", x = 1.05, y = 3.75, size = text.size,
             label = str_c("p < 2.2e-16\nr = ", r), hjust = 0) +
    labs(x = "Surface/Core Lipids",
         y = "CE/Cholesterol") + 
    theme_scatter() +
    theme(legend.position = "none")
## -------- algin and save -----------------------------------------------------
p = plot_grid(
    p.pclpc.cefc, p.surfacecore.cefc,
    align = "hv", axis = "tblr", labels = c("A", "B"),
    label_size = title.size - 1, label_x = 0, label_y = 0.99
)

ggsave("supplement_figures/figS1_lpd_fct.png", p,
       height = 2.25, width = 6, units = "in", dpi = 300)
