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
df$Ages_binned <- cut(df$Age, 10 * (0 : 10), labels = c("1-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100"))
df$AgeAtIncarceration <- df$Age - df$Years.Served
df$Jurisdiction <- tolower(df$Jurisdiction)
df$Jurisdiction <- na.omit(df$Jurisdiction, na)

# Subsets
maleCrimes <- df[which(df$gender == 'Male')]

print("################   Analyzing Data   ################")
p <- qplot(x = AgeAtIncarceration, y = Years.Served, data = df, geom = "point", color = Offense.Type.Most.Serious.Crime, alpha = .5)
p <- p + xlab("Age at Incarceration")
p <- p + ylab("Number of Years Served")
p <- p + ggtitle("Age at Incarceration Vs. Number of Years Served")
p <- p + labs(color = "Offense Type", alpha = "Alpha", size = "Years Served")

graphList[['AgeAtIncarcerationVsYearsServedScatterPlot']] <- p



p <- qplot(x = Offense.Type.Most.Serious.Crime, data = df, geom = "bar", facets = . ~ Ages_binned, fill = Offense.Type.Most.Serious.Crime)
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
j_summ_ordered <- j_summ[order(tolower(df$Jurisdiction)),]

cr <- cr[order(cr$county.name),]

###check if counties and jurisdiction are the same
identical(tolower(j_summ_ordered), cr$county.name)

fixed <- inner_join(cr, j_summ_ordered, by = c("county.name" = "Jurisdiction"))

identical(tolower(fixed$county.name), cr$county.name)

###copy region info to j_summ
fixed$region <- cr$region

###get map
p <- county_choropleth(fixed, title = "Crimes by County", num_colors = 9, state_zoom = "iowa") +
scale_fill_brewer(name = "Frequency", palette = "PuRd")
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
dpc <- dpc[dpc$region %in% fixed$region, ]
dpc <- dpc[order(dpc$region), ]
identical(dpc$region, fixed$region)
dpc2 <- dpc
dpc2$value <- fixed$value/dpc$value

# Get map
s <- county_choropleth(dpc2, title = "Per Capita Crimes by County", state_zoom = "iowa") +
    scale_fill_brewer(name = "Frequency", palette = 15)

graphList[['PerCapitaCrimes']] <- s

###########################################################

print("writing graphs to files")
graphNames <- names(graphList)
for (i in graphNames) {save_graph(toString(i), graphList[[i]])}



#################

# Analysis of Race/Ethnic origin impact on crimes

library(scales)

#Add Hispanic to the Race factors
levels(df$Race) <- c(levels(df$Race), "Hispanic")
df$Race[which(df$Ethnic.Origin == "Hispanic")] <- "Hispanic"

# Make the Race a ordered factor for easier analysis
df$Race <- ordered(df$Race, levels = c("American Indian or Alaska Native ","Asian or Pacific Islander ",  "Black ", "Hispanic", "White "))
summary(df$Race)

# Create a bar plot for convicted Crimes by Race
p <- ggplot(df, aes(x = Race, fill = I("steelblue")))
p <- p + geom_bar(aes(y = (..count..)/sum(..count..)))
p <- p + geom_text(aes(y = ((..count..)/sum(..count..)),
label = scales::percent((..count..)/sum(..count..))),
stat = "count", hjust = -0.1)
p <- p + coord_flip()
p <- p + xlab("Race/Ethnic Origin")
p <- p + ylab("Percentage")
p <- p + ggtitle("Crimes (imprisoned) by Race/Ethnic Origin")
p <- p + scale_y_continuous(labels = scales::percent, limits = c(0, 1))


ggsave(plot = p, filename = "CrimesWithRaceDistribution.png", width = 8, height = 4, dpi = 600)


# Create a data frame from quick stats page of https://www.census.gov/quickfacts/ia
# Adjust it to include Ethnic Origin as a Race.
df_ia <- data.frame(Race=c("American Indian or Alaska Native ","Asian or Pacific Islander ","Black ","White ","Hispanic"),
Percentage=c(0.5,2.7,3.8,85.5,7.5))

# Create a bar plot (geom_col()) to create a graph for side-by-side comparison
p <- ggplot(df_ia, aes(Race, Percentage, fill = I("steelblue")))
p <- p + geom_col()
p <- p + coord_flip()
p <- p + geom_text(aes(label = paste(Percentage,"%",''), y = Percentage + 2,  hjust = 0.1))
p <- p + ggtitle("Population Distribution by Race/Ethnic Origin")
p <- p + scale_y_continuous(limits = c(0, 100))

save_graph("GeneralPopulationWithRaceDistribution",p)



p <- qplot(x = AgeAtIncarceration, data = df, geom = "histogram", binwidth = 10, facets = .~Offense.Type.Most.Serious.Crime, fill = Race)
p <- p + scale_fill_hue(name = "Default")
p <- p + xlab("Race/Ethnic Origin ")
p <- p + ylab("Count")
p <- p + ggtitle("Age at Incarceration vs Race/Ethnic Origin per Offense Type")
p <- p + theme(legend.position = "bottom" )

ggsave(plot = p, filename = "RaceVsAgeOfCrime-Facets-OffenseType.png", width = 10, height = 4, dpi = 600)



# Create a box plot to represent Race vs Age at incarceration
p <- qplot(Race, AgeAtIncarceration, data = df, geom = "boxplot")
p <- p + ylab("Age at Incarceration")
p <- p + xlab("Race")
p <- p + ggtitle("Age at Incarceration vs Race/Ethnic Origin")
p <- p + theme(legend.position = "bottom" )

ggsave(plot = p, filename = "RaceVsAgeOfCrimeBox.png", width = 10, height = 4, dpi = 600)


################
print("processing complete")

