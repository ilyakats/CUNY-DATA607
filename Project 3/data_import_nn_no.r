# Nkasi Nedd
# Nnaemezue Obi-Eyisi

library(RMySQL)
library(dplyr)

connection <- dbConnect(MySQL(), user='root', password='data607pw', host='35.185.104.222', dbname='datascienceskills')

categories <- c('program_languages', 'analysis_software', 'bigdata_tool', 'databases')

program_languages <- c('bash', 'r', 'python', 'java', 'c++', 'ruby', 'perl', 'matlab', 'javascript', 'scala', 'php')
analysis_software <- c('excel', 'tableau', 'd3.js', 'sas', 'spss', 'd3', 'saas', 'pandas', 'numpy', 'scipy', 'sps', 'spotfire', 'scikits.learn', 'splunk', 'powerpoint', 'h2o')
bigdata_tool <- c('hadoop', 'mapreduce', 'spark', 'pig', 'hive', 'shark', 'oozie', 'zookeeper', 'flume', 'mahout')
databases <- c('sql', 'nosql', 'hbase', 'cassandra', 'mongodb', 'mysql', 'mssql', 'postgresql', 'oracle db', 'rdbms')

# Convert Categories to Data Frame and write to database
categories_df <- as.data.frame(categories)
names(categories_df) <- "Description"
dbWriteTable(conn=connection, name='Categories', value=categories_df, overwrite=FALSE, append=TRUE, row.names=0)

# Convert Program Languages to Data Frame and write to database
progam_languages_df <- data.frame(SkillDescription = program_languages, 
	SkillCategory = 1)
dbWriteTable(conn=connection, name='Skills', value=progam_languages_df, overwrite=FALSE, append=TRUE, row.names=0)


# Convert Analysis Software to Data Frame and write to database
analysis_software_df <- data.frame(SkillDescription = analysis_software, 
	SkillCategory = 2)
dbWriteTable(conn=connection, name='Skills', value=analysis_software_df, overwrite=FALSE, append=TRUE, row.names=0)


# Convert Big Data tools to Data Frame and write to database
bigdata_tool_df <- data.frame(SkillDescription = bigdata_tool,
	SkillCategory = 3)
dbWriteTable(conn=connection, name='Skills', value= bigdata_tool_df, overwrite=FALSE, append=TRUE, row.names=0)


# Convert Databases to Data Frame and write to database
databases_df <- data.frame(SkillDescription = databases, SkillCategory = 4)
dbWriteTable(conn=connection, name='Skills', value=databases_df, 
	overwrite=FALSE, append=TRUE, row.names=0)

# Read in All skills Data from sql database
skillsData <- dbGetQuery(connection, "SELECT * FROM Skills;")


# Setup Austin for data
Location_df <- data.frame(Description = "Austin, Texas", Country = "US")
dbWriteTable(conn=connection, name='Location', value=Location_df, 
	overwrite=FALSE, append=TRUE, row.names=0)

URL_austin2017 <- "https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/Austin_TX_0416.csv"

Austin2017_data <- read.csv(URL_austin2017)

M1 <- merge(Austin2017_data, skillsData, 
	by.x = "Skill", by.y = "SkillDescription")

Austin2017_df <- select(m1, SkillID, Count, Ranking)
Austin2017_df['Place'] <- 1

Austin2017_df <- select(Austin2017_df, SkillID, Place, Count, Ranking)
Austin2017_df <- rename(Austin2017_df, Skill = SkillID, 
	Amount = Count, Rating = Ranking)
dbWriteTable(conn=connection, name='skillsdata', value=Austin2017_df, overwrite=FALSE, append=TRUE, row.names=0)

# ----------------------------------------------------------------------------
# Setup Chicago for data

Location_df <- data.frame(Description = "Chicago, Illinois", Country = "US")
dbWriteTable(conn=connection, name='Location', value=Location_df, 
	overwrite=FALSE, append=TRUE, row.names=0)

URL_chicago2017 <- "https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/Chicago_IL_0418.csv"

Chicago2017_data <- read.csv(URL_chicago2017)

m1<- merge(Chicago2017_data, skillsData, 
	by.x = "Skill", by.y = "SkillDescription")

Chicago2017_df <- select(m1, SkillID, Count, Ranking)
Chicago2017_df['Place'] <- 2

Chicago2017_df <- select(Chicago2017_df, SkillID, Place, Count, Ranking)
Chicago2017_df <- rename(Chicago2017_df, Skill = SkillID, 
	Amount = Count, Rating = Ranking)
dbWriteTable(conn=connection, name='skillsdata', value=Chicago2017_df, overwrite=FALSE, append=TRUE, row.names=0)

# ----------------------------------------------------------------------------
# Setup Detroit for data

Location_df <- data.frame(Description = "Detroit, Michigan", Country = "US")
dbWriteTable(conn=connection, name='Location', value=Location_df, 
	overwrite=FALSE, append=TRUE, row.names=0)


URL_Detroit2017 <- "https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/DT_MI_2017.csv"

Detroit2017_data <- read.csv(URL_Detroit2017)

m1<- merge(Detroit2017_data, skillsData, 
	by.x = "Skill", by.y = "SkillDescription")

Detroit2017_df <- select(m1, SkillID, Count, Ranking)
Detroit2017_df['Place'] <- 3

Detroit2017_df <- select(Detroit2017_df, SkillID, Place, Count, Ranking)
Detroit2017_df <- rename(Detroit2017_df, Skill = SkillID, 
	Amount = Count, Rating = Ranking)
dbWriteTable(conn=connection, name='skillsdata', value=Detroit2017_df, overwrite=FALSE, append=TRUE, row.names=0)

# ----------------------------------------------------------------------------
# Setup remaining data

Locations <- c("N/A", "N/A", "N/A", "N/A", "New York, New York", "Paris", "Seattle, Washington", "San Francisco, California", "Sydney", "Toronto, Ontario")

Countries <- c("England", "Germany", "India", "South Africa", "US", "France", "US", "US", "Australia", "Canada")

Location_df <- data.frame(Description = Locations, Country = Countries)

dbWriteTable(conn=connection, name='Location', value=Location_df, 
	overwrite=FALSE, append=TRUE, row.names=0)

URLs <- c("https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/England_2017.csv",     "https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/Germany_0418.csv",
"https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/India_0418.csv",          "https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/SouthAfrica_0418.csv",
"https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/NY_NY_2017.csv",          "https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/Paris_2017.csv",
"https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/SE_WA_2017.csv",          "https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/SF_CA_2017.csv",
"https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/Sydney_AU_2017.csv",          "https://raw.githubusercontent.com/NNedd/DATA-607-Project-3/master/2017_results_150%20cases_per_city/Toronto_ON_2017.csv")


i <- 1
for (i in 1:10)
{
	location_data <- read.csv(URLs[i])
	m1<- merge(location_data, skillsData, 
		by.x = "Skill", by.y = "SkillDescription")
	locationskillsdf <- select(m1, SkillID, Count, Ranking)
	locationskillsdf['Place'] <- i+3
	locationskillsdf <- select(locationskillsdf, SkillID, 
		Place, Count, Ranking)
	locationskillsdf <- rename(locationskillsdf, Skill = SkillID, 
		Amount = Count, Rating = Ranking)
	dbWriteTable(conn=connection, name='skillsdata', value=locationskillsdf, 
		overwrite=FALSE, append=TRUE, row.names=0)
}

all_data <- dbGetQuery(connection, 
	"SELECT Location.Description, Location.Country, Skills.SkillDescription, 
		Categories.Description, skillsdata.Amount, skillsdata.Rating
	FROM skillsdata
	LEFT JOIN (Skills 
	LEFT JOIN Categories 
		ON Skills.SkillCategory = Categories.CategoryID, Location)
		ON (Skills.SkillID = skillsdata.Skill AND 
			Location.LocationID = skillsdata.Place);")
dbDisconnect(connection)
