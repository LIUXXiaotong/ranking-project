

p_all <- plot_grid(p1,p2,ncol=2,nrow = 1, labels = c("A", "B"))



legend_a <- get_legend(
  
     p1 + guides(color = guide_legend(nrow = 2),
                 linetype = guide_legend(nrow = 2)) + theme(legend.position = "bottom")
   )
 



plot_grid(p_all, legend_a, ncol=1,nrow = 2, rel_heights = c(2, 0.3))
  
  
ggsave("b_all.jpg")

