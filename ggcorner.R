ggcorner <- function(s){
ci  <- function(data, mapping, ..., low = "#74dbef", high = "#0074e4") {
    # get the x and y data to use the other code
       ggplot(data = data, mapping = mapping) +
       stat_ci(level = 0.95,geom = "polygon",fill="#feb24c",color="black",linetype="dashed",alpha=0.5,...) +
       stat_ci(level = 0.68,geom = "polygon",fill="#f03b20",color="black",linetype="dashed",alpha=0.5,...) +
      
      scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
      scale_alpha(range=c(0.2,1)) +
      theme_bw() +
      theme( text = element_text(size=60),axis.text = element_text(size=9.5),
             axis.text.x = element_text(size=8, angle=90),
             axis.text.y = element_text(size=8),
             strip.text.x = element_text(colour = 'red',size = 20),
             strip.text.y = element_text(colour = 'red',size = 20),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())
  }
  hist2 <- function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping) +
      
      stat_density_ridges(aes(y=0,fill=factor(..quantile..)),geom = "density_ridges_gradient", calc_ecdf = TRUE, 
                          #                        quantiles = c(0.025, 0.16,  0.84, 0.975)
                          quantiles = c(0.025, 0.16,  0.84, 0.975)) + 

      scale_fill_manual(name = "Probability", values = c("gray90","#feb24c","#f03b20",
                                                         "#feb24c","gray90")) +
      #    scale_fill_viridis(discrete=TRUE)+
      #     geom_histogram(bins = 10,fill="#bf812d",colour="#1F3552",...) +
      #    geom_density(adjust = 1.5,fill="#bf812d",colour="#1F3552",...) +
      
      theme_void() + theme( panel.grid.minor=element_blank(),
                            panel.grid.major = element_blank())
  }
  
  ggs_pairs(s, 
            labeller = "label_parsed",
            diag=list(continuous = hist2),
            upper = "blank",
            lower = list(continuous = ci),
            switch="both",
            showStrips=TRUE
  ) 
  
}
