ci_fun = function(df, val, val_up, val_down, color, axis) {
  ggplot(df, aes(x = week, y = {{val}}, fill = status)) + 
    geom_ribbon(aes(ymin = {{val_down}}, ymax = {{val_up}}), alpha = 0.3) + 
  # geom_line(aes(y = {{val_up}}, color = {{color}}), linetype = "dashed") +  # Dashed upper bound
    #geom_errorbar(aes(ymin = {{val_down}}, ymax = {{val_up}}, width = 0.2)) +
    geom_point(aes(color = {{color}}),size = 2) + 
    geom_line(aes(color = {{color}})) + 
    # geom_point(aes(color = status), size = 2) +
    #geom_hline(yintercept = 0) +
    theme_classic() +
    ylab(axis) +
    scale_fill_manual(values = status_pal) +  # Use scale_fill_manual for ribbon color
    scale_color_manual(values = status_pal) + # Keep scale_color_manual for line/point color
    theme(plot.title = element_text(hjust = 0, vjust = 1),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12))
}
