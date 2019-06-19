bar_chart <- function( var1 = var1){
  var1 <- enquo(var1)
  label <- quo_name(var1)
  x %>% 
    count(!! var1, sort = TRUE) %>%
    slice(1:20) %>% 
    ggplot(aes(x = reorder(!! var1, -n), y = n, fill = !! var1)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = glue("Top twitter {label} in #bioinformatics"),
         x = glue("{label}")) +
    theme(plot.title = element_text(size = rel(2))) +
    theme_minimal() +
    theme(legend.position="none") +
    scale_fill_manual(
      values = colorRampPalette(
        RColorBrewer::brewer.pal(n = 25, name = "Spectral"))(25),
      guide = guide_legend(reverse = TRUE))
  
}