rm(list = ls())

rm(list = ls())

url <- 'https://data.iowa.gov/api/views/murf-9x69/rows.csv?accessType=DOWNLOAD'

df <- read.csv(file = url, stringsAsFactors = TRUE, na.strings = c(NA,""))

#remove rows that don't make sense
df <- df[(df$Age - df$Years.Served) >= 0,]
df$Serving.Life.with.Possibility.of.Parole <- NULL
df <- df[complete.cases(df), ]

#new column for binned ages
df$Ages_binned <- cut(df$Age, 10*(0:10), labels = c("1-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))

library(ggplot2)

p <- qplot(x = Age, y = Years.Served, data = df, geom = "point", color = Sex)
p <- p + xlab("Age")
p <- p + ylab("Number of Years Served")
p <- p + ggtitle("Age Vs. Number of Years Served")

ggsave(plot = p, filename = "AgeVsYearsServedScatterPlot.png", width = 8, height = 4, dpi = 600)




p <- qplot(x = Offense.Type.Most.Serious.Crime, data = df, geom = "bar", facets = .~Ages_binned, fill = Offense.Type.Most.Serious.Crime)
p <- p + xlab("Offense Type")
p <- p + ylab("Count")
p <- p + ggtitle("Offense Types Per Age Group")
p <- p + theme(axis.text.x = element_blank())

ggsave(plot = p, filename = "AgeVsOffenseType.png", width = 8, height = 4, dpi = 600)



# How does gender impact the type of crime one commits.  In addition, determine if there is a difference between male and female criminal activity.  We will analyze this with a histogram of offense type.(Anselmo)

ggsave(plot = p, filename = "AgeVsOffenseType.png", width = 8, height = 4, dpi = 600)

gg <- ggplot(df, aes(x=Sex)) + 
      geom_bar(aes(fill = Offense.Class.Most.Serious.Crime)) +
      labs(y="CRIME",
           x="GENDER",
           title="Gender Vs Type of Crime",
           caption = "Source: data.iowa.gov")

ggsave(plot = gg, filename = "GenderVsOffenseType.png", width = 8, height = 4, dpi = 600)

print(gg)




