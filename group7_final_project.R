print("cleaning workspace")
rm(list = ls())

print("import libraries")
library(ggplot2)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(stringr)

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
df$Jurisdiction <- tolower(df$Jurisdiction)
df$Jurisdiction <- na.omit(df$Jurisdiction, na)

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


# summary stats
totalCrimes <- length(df[, "Sex"])
countofCrimesbyGender <- df %>% group_by(Sex) %>% summarize(count = n())
myColumns <- names(df) %in% c("Sex", "Offense.Type.Most.Serious.Crime")


### group by jurisdiction
dfj <- group_by(df, Jurisdiction)
j_summ <- summarize(dfj, value = n())
head(j_summ)

### subset region by county name
data(county.regions)
cr <- county.regions
cr <- subset(cr, state.abb == "IA")
cr <- cr[, c("region", "county.name")]
head(cr)

###reorder summ & cr
j_summ_ordered <- j_summ[order(tolower(df$Jurisdiction)), ]

cr <- cr[order(cr$county.name), ]

###check if counties and jurisdiction are the same
identical(tolower(j_summ_ordered), cr$county.name)

fixed <- inner_join(cr, j_summ_ordered, by = c("county.name" = "Jurisdiction"))

identical(tolower(fixed$county.name), cr$county.name)

###copy region info to j_summ
fixed$region <- cr$region

###get map
p <- county_choropleth(fixed, title = "Crimes by County", num_colors = 9, state_zoom = "iowa") +
      scale_fill_brewer(name="Frequency", palette="PuRd")
graphList[['FrequencyofCrimesbyCounty']] <- p

##########################################################
# Load population data by county
data(df_pop_county)

# Store the data in a shorter name
dpc <- df_pop_county

# Make population map

q <- county_choropleth(dpc, title = "Population by County", state_zoom = "iowa") +
      scale_fill_brewer(name = NULL, palette = "Blues")

graphList[['PopulationbyCounty']] <- q

###########################################################

print("writing graphs to files")
graphNames <- names(graphList)
for (i in graphNames) {save_graph(toString(i), graphList[[i]])}

print("processing complete")

