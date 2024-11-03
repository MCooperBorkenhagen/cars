dwell_at_closing = 30


exclude = c('sedan_1', 'test_1', 'test_2', 'test_3')

max_PC1 = max(pcs$PC1) + 1
max_PC2 = max(pcs$PC2) + 1
min_PC1 = min(pcs$PC1) - 1
min_PC2 = min(pcs$PC2) - 1


### All the words
for (e in sort(unique(pcs$epoch))){
  if (e > 0){
    
    this_epoch = pcs %>% 
      filter(car %nin% exclude) %>% 
      filter(epoch == e)
    
    performance_this_epoch = performance %>% 
      filter(epoch <= e) %>% 
      filter(epoch != 0)
    
    plot = this_epoch %>% 
      ggplot(aes(PC1, PC2, label = car)) +
      geom_label(size = 2) +
      ylim(c(min_PC2, max_PC2)) +
      xlim(c(min_PC1, max_PC1)) +
      labs(x = "Dimension 1",
           y = "Dimension 2") +
      theme_apa() +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) 
    
    plot_2 = performance_this_epoch %>% 
      ggplot(aes(epoch, mse)) +
      geom_line() +
      geom_point() +
      labs(x = "Learning time", y = "Error") +
      theme_apa() +
      ylim(c(0, max(performance_this_epoch$mse))) +
      xlim(c(1, max(performance$epoch)))
    
    filename = str_c("outputs/accuracy/plot_", e, ".png")
    ggsave(filename, plot = plot_2, width = 6, height = 4, dpi = 300)
    
    plots = plot_grid(plot, plot_2)
    
    filename = str_c("outputs/plot_", e, ".png")
    ggsave(filename, plot = plots, width = 12, height = 4, dpi = 300)
    print(paste("Epoch", e, "done"))
    
    if (e == max((pcs$epoch))){
      
      for (i in seq(dwell_at_closing)){

        filename = str_c("outputs/accuracy/plot_", e + i, ".png")
        ggsave(filename, plot = plot_2, width = 6, height = 4, dpi = 300)
        
        filename = str_c("outputs/plot_", e + i, ".png")
        ggsave(filename, plot = plots, width = 12, height = 4, dpi = 300)
        print(paste("Extra plot", i, "done"))
      }
}}}
  


# now each of the test items:
# test_1
plot = pcs %>% 
  filter(car %nin% c('sedan_1', 'test_2', "test_3")) %>% 
  filter(epoch == e) %>% 
  ggplot(aes(PC1, PC2, label = car)) +
  geom_label(size = 2) +
  ylim(c(min_PC2, max_PC2)) +
  xlim(c(min_PC1, max_PC1)) +
  labs(x = "Dimension 1",
       y = "Dimension 2") +
  theme_apa() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave('outputs/test_items/plot_50_test_1.png', plot = plot, width = 6, height = 4, dpi = 300)

# test_2
plot = pcs %>% 
  filter(car %nin% c('sedan_1', 'test_1', "test_3")) %>% 
  filter(epoch == e) %>% 
  ggplot(aes(PC1, PC2, label = car)) +
  geom_label(size = 2) +
  ylim(c(min_PC2, max_PC2)) +
  xlim(c(min_PC1, max_PC1)) +
  labs(x = "Dimension 1",
       y = "Dimension 2") +
  theme_apa() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave('outputs/test_items/plot_50_test_2.png', plot = plot, width = 6, height = 4, dpi = 300)

# test_3
plot = pcs %>% 
  filter(car %nin% c('sedan_1', 'test_1', "test_2")) %>% 
  filter(epoch == e) %>% 
  ggplot(aes(PC1, PC2, label = car)) +
  geom_label(size = 2) +
  ylim(c(min_PC2, max_PC2)) +
  xlim(c(min_PC1, max_PC1)) +
  labs(x = "Dimension 1",
       y = "Dimension 2") +
  theme_apa() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave('outputs/test_items/plot_50_test_3.png', plot = plot, width = 6, height = 4, dpi = 300)
