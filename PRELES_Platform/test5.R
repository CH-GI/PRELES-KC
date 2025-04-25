library(pheatmap)

set.seed(123)
data <- matrix(rnorm(100), nrow = 10)
colnames(data) <- paste("Col", 1:10, sep = "")
rownames(data) <- paste("Row", 1:10, sep = "")

# 创建注释数据框
annotation_col <- data.frame(
  Group = factor(rep(c("A", "B"), each = 5))
)
rownames(annotation_col) <- colnames(data)

annotation_row <- data.frame(
  Category = factor(rep(c("X", "Y"), 5))
)
rownames(annotation_row) <- rownames(data)

# 绘制带有注释和自定义颜色的热力图
my_colors <- colorRampPalette(c("blue", "white", "red"))(50)
pheatmap(data, 
         annotation_col = annotation_col, 
         annotation_row = annotation_row, 
         color = my_colors, 
         main = "My Heatmap", 
         fontsize = 12, 
         cellwidth = 15, 
         cellheight = 12)
