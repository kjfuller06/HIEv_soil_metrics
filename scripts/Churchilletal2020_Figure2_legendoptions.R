tiff(file = "FIELD_env_plots_legend.tiff", width =900, height = 900, units = "px", res = 200)
plot(1, type="n", xaxt = 'n', yaxt = 'n', bty = 'n', xlim=c(0, 0.01), ylim=c(0, 0.01), ann = FALSE)
legend(title = "Colours","center", y = NULL, 
       legend=c("aT-C","aT-D","eT-C","eT-D"),
       lty=c(1, 1, 1, 1, 1, 3), col = c('blue', 'lightskyblue', 'red', 'red4'), lwd=c(10, 10, 10, 10),bty='n')
legend(title = "Line types","bottom", y = NULL, 
       legend=c("Control", "Drought"),
       lty=c(1, 3), col = c('black', 'black'), lwd=c(3, 3),bty='n')
dev.off()
