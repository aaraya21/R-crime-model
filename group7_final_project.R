print("cleaning workspace")
rm(list = ls())

print("import libraries")
library(ggplot2)
library("dplyr")

print("define global funcitons")
save_graph <- function(name, plot){
    ggsave(plot = plot, filename = paste(name, ".png", sep = ''), width = 8, height = 4, dpi = 600)
}

print("define global vars")
graphList <- list()



print("fecth dataset")
url <- 'https://data.iowa.gov/api/views/murf-9x69/rows.csv?accessType=DOWNLOAD'
df <- read.csv(file = url, stringsAsFactors = TRUE, na.strings = c(NA, ""))


print("################   Scrub Data   ################")

print("Removing bad data")
df <- df[(df$Age - df$Years.Served) >= 0,]
df$Serving.Life.with.Possibility.of.Parole <- NULL
df <- df[complete.cases(df),]
print("Adding Groupings (Bins)")
df$Ages_binned <- cut(df$Age, 10 * (0 : 10), labels = c("1-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))
# Subsets
maleCrimes <- df[which(df$gender == 'Male')]

print("################   Analyzing Data   ################")
p <- qplot(x = Age, y = Years.Served, data = df, geom = "point", color = Sex)
p <- p + xlab("Age")
p <- p + ylab("Number of Years Served")
p <- p + ggtitle("Age Vs. Number of Years Served")

graphList[['AgeVsYearsServedScatterPlot']] <- p



p <- qplot(x = Offense.Type.Most.Serious.Crime, data = df, geom = "bar", facets = . ~ Ages_binned, fill = Offense.Type.Most.Serious.Crime)
p <- p + xlab("Offense Type")
p <- p + ylab("Count")
p <- p + ggtitle("Offense Types Per Age Group")
p <- p + theme(axis.text.x = element_blank())

graphList[['AgeVsOffenseType.png']] <- p

p <- ggplot(df, aes(x = Sex)) +
    geom_bar(aes(fill = Offense.Class.Most.Serious.Crime)) +
    labs(y = "CRIME",
    x = "GENDER",
    title = "Gender Vs Type of Crime",
    caption = "Source: data.iowa.gov")

graphList[['GenderVsOffenseClass']] <- p


p <- ggplot(df, aes(x = Sex)) +
    geom_bar(aes(fill = Offense.Type.Most.Serious.Crime)) +
    labs(y = "CRIME",
    x = "GENDER",
    title = "Gender Vs Type of Crime",
    caption = "Source: data.iowa.gov")

graphList[['GenderVsOffenseType']] <- p



print("writing graphs to files")
graphNames <- names(graphList)
for (i in graphNames) {save_graph(toString(i), graphList[[i]])}



# summary stats
totalCrimes <- length(df[, "Sex"])
countofCrimesbyGender <- df %>% group_by(Sex) %>% summarize(count = n())
myColumns <- names(df) %in% c("Sex", "Offense.Type.Most.Serious.Crime")

print("processing complete")

