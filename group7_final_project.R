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
df <- df[(df$Age - df$Years.Served) >= 15,]
df$Serving.Life.with.Possibility.of.Parole <- NULL
df <- df[complete.cases(df),]
print("Adding Groupings (Bins)")
df$Ages_binned <- cut(df$Age, 10*(0:10), labels = c("1-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100"))
df$AgeAtIncarceration <- df$Age - df$Years.Served
df$Jurisdiction <- tolower(df$Jurisdiction)
df$Jurisdiction <- na.omit(df$Jurisdiction, na)

# Subsets
maleCrimes <- df[which(df$gender == 'Male')]

print("################   Analyzing Data   ################")
p <- qplot(x = AgeAtIncarceration, y = Years.Served, data = df, geom = "point", color=Offense.Type.Most.Serious.Crime, alpha=.5)
p <- p + xlab("Age at Incarceration")
p <- p + ylab("Number of Years Served")
p <- p + ggtitle("Age at Incarceration Vs. Number of Years Served")
p <- p + labs(color="Offense Type", alpha="Alpha", size="Years Served")

graphList[['AgeAtIncarcerationVsYearsServedScatterPlot']] <- p



p <- qplot(x = Offense.Type.Most.Serious.Crime, data = df, geom = "bar", facets = .~Ages_binned, fill = Offense.Type.Most.Serious.Crime)
p <- p + xlab("Age Group")
p <- p + ylab("Count")
p <- p + ggtitle("Offense Types Per Age Group")
p <- p + theme(axis.text.x = element_blank())
p <- p + scale_fill_discrete(name = "Offense Type")

graphList[['AgeVsOffenseType']] <- p

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
ggsave(plot = p, filename = "FrequencyofCrimesbyCounty.png", width = 8, height = 4, dpi = 600)
p

##########################################################
# Load population data by county
data(df_pop_county)

# Store the data in a shorter name
dpc <- df_pop_county

# Make population map

q <- county_choropleth(dpc, title = "Population by County", state_zoom = "iowa") +
scale_fill_brewer(name = NULL, palette = "Blues")
q
ggsave(plot = q, filename = "PopulationbyCounty.png", width = 8, height = 4, dpi = 600)

###########################################################


print("processing complete")

