ggplot(base_env$influenza_england_1978_school) + theme_classic() +
  geom_point(aes(x=date, y=in_bed)) +
  xlab("Date") +
  ylab("In bed (prevalence)") +
  scale_x_date(date_labels="%b %d", date_breaks="2 days")