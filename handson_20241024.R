# R codes for hands-on part (Ryosuke Fujii)
# 2024/10/25 Stat Genet 2024
# Please visit https://github.com/fujichaaan/statgenet2024

# Loading packages --------------------------------------------------------

# install.packages(c("tidyverse", "readxl", "UpSetR", "ggvenn", "umap", "ggsci", "ggrepel"))
library(tidyverse); library(readxl); library(UpSetR); library(ggvenn); library(umap); library(ggsci); library(ggrepel)


# P.19 --------------------------------------------------------------------

# Reading table and Edit variable
ishigaki_R4 <- read_excel("data/ishigaki.xlsx") |>
  filter(year == "R4") |>
  mutate(causes = factor(causes,
                         levels = c("Other", setdiff(causes[order(num)], "Other")),
                         labels = c("その他", "肝硬変", "結核", "糖尿病", "高血圧性疾患", "不慮の事故", "肺炎", "自殺", "脳血管疾患", "老衰", "心疾患", "悪性新生物")))

# Draw bar chart
ggplot(ishigaki_R4, aes(x = num, y = causes)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(x = num + 10, y = causes),
            label = ishigaki_R4$num, size = 8) +
  labs(x = "人数", y = "死因") +
  theme_bw(base_family = "HiraKakuPro-W3") +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20),
        axis.text.y = element_text(size = 26, hjust = 0, color = "black"))



# P.20 --------------------------------------------------------------------

# Reading table and Edit variable
ishigaki_R4 <- read_excel("data/ishigaki.xlsx") |>
  filter(year == "R4") |>
  mutate(causes = factor(causes,
                         labels = c("悪性新生物", "脳血管疾患", "糖尿病", "心疾患", "高血圧性疾患", "肝硬変", "その他", "肺炎", "老衰", "自殺", "結核", "不慮の事故")))

# Draw bar chart
ggplot(ishigaki_R4, aes(x = num, y = causes)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(x = num + 10, y = causes), 
            label = ishigaki_R4$num, size = 8) +
  labs(x = "人数", y = "死因") +
  theme_bw(base_family = "HiraKakuPro-W3") +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20),
        axis.text.y = element_text(size = 26, hjust = 0, color = "black"))



# P.21 ---------------------------------------------------------------------

# Reading table and Edit this
ishigaki_R4 <- read_excel("data/ishigaki.xlsx") |>
  filter(year == "R4") |>
  mutate(causes = factor(causes,
                         levels = c("Other", setdiff(causes[order(num)], "Other")),
                         labels = c("その他", "肝硬変", "結核", "糖尿病", "高血圧性疾患", "不慮の事故", "肺炎", "自殺", "脳血管疾患", "老衰", "心疾患", "悪性新生物")),
         cancer = if_else(causes == "悪性新生物", "yes", "no"))

# Draw bar chart
ggplot(ishigaki_R4, aes(x = num, y = year, fill = cancer)) +
  geom_bar(stat = "identity", position =  "fill", width = 0.6) +
  geom_text(aes(x = 0.13, y = 1),
            label = "121\n(25.0%)", size = 12, color = "#dcdcdc") +
  geom_text(aes(x = 0.92, y = 1),
            label = "483", size = 8, color = "black") +
  scale_fill_manual(values = c("#dcdcdc", "black")) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),
        legend.position = "none")



# P.22 ---------------------------------------------------------------------

# Reading table and Edit this
ishigaki_R4 <- read_excel("data/ishigaki.xlsx") |>
  filter(year == "R4") |>
  mutate(causes = factor(causes,
                         levels = c("Other", setdiff(causes[order(num)], "Other")),
                         labels = c("その他", "肝硬変", "結核", "糖尿病", "高血圧性疾患", "不慮の事故", "肺炎", "自殺", "脳血管疾患", "老衰", "心疾患", "悪性新生物")),
         cancer = if_else(causes == "悪性新生物", "yes", "no"))

# Draw bar chart
ggplot(ishigaki_R4, aes(x = num, y = causes, fill = cancer)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = num + 10, y = causes, color = cancer),
            label = ishigaki_R4$num, size = 8) +
  labs(x = "人数", y = "死因") +
  scale_fill_manual(values = c("#dcdcdc", "black")) +
  scale_color_manual(values = c("#dcdcdc", "black")) +
  theme_bw(base_family = "HiraKakuPro-W3") +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20),
        axis.text.y = element_text(size = 26, hjust = 0, color = "black"),
        legend.position = "none")



# P.27 --------------------------------------------------------------------

# Reading table and Edit this
ishigaki_R4 <- read_excel("data/ishigaki.xlsx") |>
  filter(year == "R4") |>
  mutate(causes = factor(causes,
                         levels = c("Other", setdiff(causes[order(num)], "Other")),
                         labels = c("その他", "肝硬変", "結核", "糖尿病", "高血圧性疾患", "不慮の事故", "肺炎", "自殺", "脳血管疾患", "老衰", "心疾患", "悪性新生物")))

# Draw pie chart
ggplot(ishigaki_R4, aes(x = "", y = num, fill = causes)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = "死因") +
  scale_fill_hue(direction = -1) +
  theme_void(base_family = "HiraKaKuPro-W3") +
  theme(legend.title = element_text(size = 24),
        legend.text = element_text(size = 26))

# Bar chart
ggplot(ishigaki_R4, aes(x = num, y = causes, fill = causes)) +
  geom_bar(stat = "identity") +
  labs(x = "人数", y = "死因") +
  scale_fill_hue(direction = -1) +
  theme_bw(base_family = "HiraKaKuPro-W3") +
  theme(axis.title = element_text(size = 24, color = "black"),
        axis.text = element_text(size = 20, color = "black"),
        axis.text.y = element_text(size = 24, hjust = 0),
        legend.position = "none")




# P.28 --------------------------------------------------------------------

# Reading table and Edit this
ishigaki_cancer <- read_excel("data/ishigaki.xlsx") |>
  mutate(causes_cancer = factor(if_else(causes == "Cancer", "Cancer", "Others"),
                                levels = c("Others", "Cancer"),
                                labels = c("その他", "悪性新生物")))

# Draw pie chart year by year
ishigaki_cancer |>
  filter(year == "R4") |>
  group_by(causes_cancer) |>
  summarise(total = sum(num)) |>
  ggplot(aes(x = "", y = total, fill = causes_cancer)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void(base_family = "HiraKaKuPro-W3") +
  theme(legend.position = "none")




# P.29 --------------------------------------------------------------------

# Reading table and Edit this
ishigaki <- read_excel("data/ishigaki.xlsx") |>
  mutate(causes = factor(causes,
                         levels = c("Other", setdiff(causes[order(num)], "Other")),
                         labels = c("その他", "肝硬変", "結核", "糖尿病", "高血圧性疾患", "不慮の事故", "肺炎", "自殺", "脳血管疾患", "老衰", "心疾患", "悪性新生物")))

# Draw stacked bar chart year by year
ggplot(ishigaki, aes(x = year, y = num, fill = causes)) +
  geom_bar(stat = "identity") +
  labs(x = "年（和暦）", y = "人数", fill = "死因") +
  scale_fill_hue(direction = -1) +
  theme_bw(base_family = "HiraKaKuPro-W3") +
  theme(axis.title = element_text(size = 24, color = "black"),
        axis.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 24, color = "black"),
        legend.text = element_text(size = 26, color = "black"))



# P.30 --------------------------------------------------------------------

ishigaki <- read_excel("data/ishigaki.xlsx") |>
  mutate(causes = factor(causes,
                         levels = c(setdiff(causes[order(-num)], "Other"), "Other"),
                         labels = c("悪性新生物", "心疾患", "老衰", "脳血管疾患", "自殺", "肺炎", "不慮の事故", "高血圧性疾患", "糖尿病","結核", "肝硬変", "その他")))


ggplot(ishigaki, aes(x = year, y = num, fill = causes)) +
  facet_wrap(. ~ causes, nrow = 3) +
  geom_bar(stat = "identity") +
  labs(x = "年（和暦）", y = "人数", fill = "死因") +
  scale_fill_hue() +
  theme_bw(base_family = "HiraKakuPro-W3") +
  theme(axis.title = element_text(size = 30),
        axis.text = element_text(size = 18),
        legend.position = "none",
        strip.text.x = element_text(size = 28))



# P.32 --------------------------------------------------------------------

# Download each GWAS summary and select only CHR = 12
# read_tsv("data/phenocode-KoGES_SBP.tsv.gz") |>
#   filter(chrom == 12) |>
#   rename(P_sbp = pval) |>
#   select(rsids, P_sbp) |>
#   write_tsv("data/sbp.tsv.gz")
# 
# read_tsv("data/phenocode-KoGES_LDL.tsv.gz") |>
#   filter(chrom == 12) |>
#   rename(P_ldl = pval) |>
#   select(rsids, P_ldl) |>
#   write_tsv("data/ldl.tsv.gz")
# 
# read_tsv("data/phenocode-KoGES_TG.tsv.gz") |>
#   filter(chrom == 12) |>
#   rename(P_tg = pval) |>
#   select(rsids, P_tg) |>
#   write_tsv("data/tg.tsv.gz")
# 
# read_tsv("data/phenocode-KoGES_Glucose.tsv.gz") |>
#   filter(chrom == 12) |>
#   rename(P_glu = pval) |>
#   select(rsids, P_glu) |>
#   write_tsv("data/glu.tsv.gz")

# Count the variants with P < 5.0 * 10-8
gwas_four <- read_table("data/gwas_four.txt") |>
  mutate(sbp_gws = if_else(P_sbp < 5.0 * 10^-8, 1, 0),
         ldl_gws = if_else(P_ldl < 5.0 * 10^-8, 1, 0),
         tg_gws  = if_else(P_tg  < 5.0 * 10^-8, 1, 0),
         glu_gws = if_else(P_glu < 5.0 * 10^-8, 1, 0))

# Draw bar chart
gwas_four |>
  select(sbp_gws, ldl_gws, tg_gws, glu_gws) |>
  pivot_longer(cols = everything(), 
               names_to = "Column",
               values_to = "Count") |>
  ggplot(aes(x = Column, y = Count)) +
  geom_bar(stat = "identity") +
  labs(x = "Pheno", y = "Count") +
  theme_minimal() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20))


# P.32 --------------------------------------------------------------------

# Draw Upset plot
upset(data.frame(gwas_four),
      sets = c("sbp_gws", "ldl_gws", "tg_gws", "glu_gws"),
      order.by = "freq",
      point.size = 3, line.size = 1.5,
      text.scale = c(2, 2, 3, 3, 4, 4))



# P.34 --------------------------------------------------------------------

# Data formating
gwas_four_venn <- gwas_four |>
  mutate(sbp_gws = if_else(P_sbp < 5.0 * 10^-8, TRUE, FALSE),
         ldl_gws = if_else(P_ldl < 5.0 * 10^-8, TRUE, FALSE),
         tg_gws  = if_else(P_tg  < 5.0 * 10^-8, TRUE, FALSE),
         glu_gws = if_else(P_glu < 5.0 * 10^-8, TRUE, FALSE)) |>
  select(sbp_gws, ldl_gws, tg_gws, glu_gws)

# Draw Venn diagram
ggvenn(gwas_four_venn,
       fill_color = c("#4269D0FF", "#EFB118FF", "#FF725CFF", "#6CC5B0FF"),
       fill_alpha = 0.8,
       stroke_size = 0.5, 
       text_size = 6,
       set_name_size = 8,
       show_percentage = TRUE,
       show_outside = "always")


# P.48L -------------------------------------------------------------------

# Data preparation
# data.norm <- read_excel("data/dimension.xlsx") |>
#   select(2:101)
# info.norm <- read_excel("data/dimension.xlsx") |>
#   select(1)
# 
# Run PCA
# pca.norm <- prcomp(data.norm, scale. = T)
# info.pca <- info.norm |> 
#   cbind(pca.norm$x[,1:2])
# 
# Export dataset
# write_csv(info.pca, "data/PCA.csv")

# Draw scatter plot with PCA
read_csv("data/PCA.csv") |>
  ggplot(aes(x = PC1, y = PC2, color = factor(label))) + 
  geom_point(alpha = 0.8) +
  guides(color = guide_legend(override.aes = aes(size = 4))) + 
  scale_color_observable() +
  labs(color = "Label") +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16))



# P.48R -------------------------------------------------------------------

# Data preparation
# data.norm <- read_excel("data/dimension.xlsx") |>
#   select(2:101)
# info.norm <- read_excel("data/dimension.xlsx") |>
#   select(1)
# 
# Run UMAP
# set.seed(1234)
# umap.norm <- umap(scale(data.norm))
# info.umap <- info.norm |> 
#   mutate(umap1 = umap.norm$layout[,1], umap2 = umap.norm$layout[,2])
#
# Export dataset
# write_csv(info.umap, "data/UMAP.csv")

# Load UMAP.csv & Draw scatter plot with UMAP
read_csv("data/UMAP.csv") |>
  ggplot(aes(x = umap1, y = umap2, color = factor(label))) + 
  geom_point(alpha = 0.8) +
  guides(color = guide_legend(override.aes = aes(size = 4))) + 
  scale_color_observable() +
  labs(color = "Label") +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16))



# P.49 --------------------------------------------------------------------

# Data handling
# volcano <- read_excel("data/volcano.xlsx") |>
#   mutate(log2fc = log2(fc),
#          log10fdr = -log10(fdr),
#          class = case_when(
#            log2fc >= 1 & log10fdr >= 1.3 ~ "Increase",
#            log2fc <= -1 & log10fdr >= 1.3 ~ "Decrease",
#            TRUE ~ "None"))
# 
# volcano_label <- volcano |>
#   top_n(-10, p_val)
# 
# Export data
# write.csv(volcano, "data/volcano.csv", row.names = F)
# write.csv(volcano_label, "data/volcano_label.csv", row.names = F)

# Visualization
volcano_label <- read_csv("data/volcano_label.csv")

read_csv("data/volcano.csv") |>
  ggplot(aes(x = log2fc, y = log10fdr, color = class)) +
  geom_vline(xintercept = c(-1, 1), linetype = 2, color = "Grey") +
  geom_hline(yintercept = 1.3, linetype = 2, color = "Grey") +
  geom_point(size = 3, alpha = .6) +
  geom_text_repel(data = volcano_label, 
                  aes(x = log2fc, y = log10fdr, label = gene_id)) +
  guides(color = guide_legend(override.aes = aes(size = 4))) + 
  scale_color_manual(values = c("#287686", "#BA6C76", "#AFAEAD")) +
  labs(x = expression(paste({log[2]}, "(fold change)", seq = "")),
       y = expression(paste({-log[10]}, "(q value)", seq = "")),
       color = "Label") +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))