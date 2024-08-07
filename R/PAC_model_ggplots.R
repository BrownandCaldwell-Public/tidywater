#create dataframe for plotting

#define water
water <- suppressWarnings(define_water(doc=2.5, uv254=.05,toc=1.5))

#define ranges for dose and time
doses <- seq(5,30, by=5)
times <- seq(10,1440, by=286) # will give 5 values

# initialize empty data frame to store results
results <- data.frame(dose=numeric(0), time=numeric(0), predicted_doc=numeric(0))

#define type of PAC 
type="bituminous"

#loop through data frame to calculate predicted_doc 
for (d in doses){
  for (t in times) {
    predicted_doc <- pac_toc(water, d=d, t=t, type=type)@doc
    
    results <-rbind(results,data.frame(dose=d,time=t, predicted_doc=predicted_doc,type=type))
  }
}


print(results)

PAC_plot <- ggplot(results, aes(x=dose, y=predicted_doc, color=as.factor(time)))+
  geom_line() +
  labs(
    title=paste("Predicted DOC Values for Different Doses & Times for", type, "PAC"),
    x="Dose (mg/L)",
    y="Predicted DOC (mg/L)",
    color="Contact Time (Minutes)"
  ) +
  theme_minimal()+
  theme(plot.background=element_rect(fill="white"))

ggsave(paste0("PAC_plot_",type,".png"), plot=PAC_plot ,width=10,height=6)
write.csv(results, file=paste0("results_", type, ".csv"), row.names=FALSE)