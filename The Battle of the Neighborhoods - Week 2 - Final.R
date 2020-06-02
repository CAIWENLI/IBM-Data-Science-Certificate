library(geosphere)
library(RCurl)
library(RJSONIO)

bookstores <- read.csv("C:/Users/lisal/desktop/bookstores.csv")
schools <- read.csv("C:/Users/lisal/desktop/schools.csv")
tbl_vars(schools)

b <- 2 # miles in radius

bookstore <- data.frame()

target <- schools[,c("Name","Lat","Lng")]

target <- target[complete.cases(target),]


for (i in 1: length(target$m_store_num))
{
  a <- as.character(target[i,1])
  
  ab <- target[ i , c("Lng","Lat")]
  
  ab2 <- bookstores[, c("Lng","Lat")]
  
  test <- as.data.frame(distm(ab, ab2))
  
  test1 <- as.data.frame(t(test))
  
  test2 <- cbind(data.frame(bookstores,test1))
  
  colnames(test2) <- c("Num","Name","Address","Lng","Lat", "meter")
  
  test2 <- mutate(test2, kilometer = meter/1000, miles = kilometer * 0.621371)
  
  store1 <- filter(test2, miles <= b)
  
  if (nrow(store1)>0) {
    
    store1 <- mutate(store1, school_name = a)
    
    bookstore <- rbind(store1, bookstore)
    
  }
  
}


library(censusapi)
library(zipcode)

data("zipcode")

mycensuskey <- "83727719320153260e02437cf2b38a90e2289050"
myvintage <- 2018

# B19013_001E Median household income
# B01002_001E Median age
# B01003_001E Total Population
# B01001_002E Male (Sex by Age)
# B01001_026E Female (Sex by Age)
# B01001A_001E SEX BY AGE (WHITE ALONE)
# B01001B_001E SEX BY AGE (BLACK OR AFRICAN AMERICAN ALONE)
# B01001C_001E SEX BY AGE (AMERICAN INDIAN AND ALASKA NATIVE ALONE)
# B01001D_001E SEX BY AGE (ASIAN ALONE)
# B01001E_001E SEX BY AGE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)
# B01001F_001E SEX BY AGE (SOME OTHER RACE ALONE)
# B01001I_001E SEX BY AGE (HISPANIC OR LATINO)

data <- getCensus(name = "acs/acs5", 
                  vintage=myvintage, 
                  key=mycensuskey, 
                  vars = c("NAME","B19013_001E","B01002_001E", 
                           "B01003_001E","B01001_002E","B01001_026E",
                           "B01001A_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001E_001E","B01001F_001E","B01001I_001E"), 
                  region = "zip code tabulation area:*")

colnames(data) <- c("zip", "zip_full", "median_income", "median_age", 
                    "total_population", "male_population", "female_population", 
                    "white_alone", "black_alone", "american_indian_alaska_native_alone", "asian_alone", "pacific_islander_alone", "other_race", "hispanic_latino")

write.csv(store1,"C:/Users/lisal/desktop/bookstores.csv")

data <- data %>% 
  filter(zip %in% '60651')

write.csv(data,"C:/Users/lisal/desktop/demoinfo.csv")
