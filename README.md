Using Descriptive Statistics to Analyze Data in R: Coursera Guided
Project
================
Leopoldine Mirtil

### DATA SOURCE

The data is from the “Using Descriptive Statistics to Analyze Data
in R” guided project course offered through Coursera.com and taught by 
instructor Nikunj Maheshwari. 

### OBJECTIVE

Guided project to calculate descriptive statistical metrics on a data
set and create a data quality report file.

### Task 1 - Load and View Data

#### Set Directory

``` r
setwd("C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Guided Projects/R Guided Project/DescriptiveStatisticsInR")
```

#### Load Library

``` r
library(tidyverse)
library(rmarkdown)
```

#### Import and View Data

``` r
data <- read.csv("data.csv")

head(data)
```

    ##   Make      Model Year            Engine.Fuel.Type Engine.HP Engine.Cylinders
    ## 1  BMW 1 Series M 2011 premium unleaded (required)       335                6
    ## 2  BMW   1 Series 2011 premium unleaded (required)       300                6
    ## 3  BMW   1 Series 2011 premium unleaded (required)       300                6
    ## 4  BMW   1 Series 2011 premium unleaded (required)       230                6
    ## 5  BMW   1 Series 2011 premium unleaded (required)       230                6
    ## 6  BMW   1 Series 2012 premium unleaded (required)       230                6
    ##   Transmission.Type    Driven_Wheels Number.of.Doors
    ## 1            MANUAL rear wheel drive               2
    ## 2            MANUAL rear wheel drive               2
    ## 3            MANUAL rear wheel drive               2
    ## 4            MANUAL rear wheel drive               2
    ## 5            MANUAL rear wheel drive               2
    ## 6            MANUAL rear wheel drive               2
    ##                         Market.Category Vehicle.Size Vehicle.Style highway.MPG
    ## 1 Factory Tuner,Luxury,High-Performance      Compact         Coupe          26
    ## 2                    Luxury,Performance      Compact   Convertible          28
    ## 3               Luxury,High-Performance      Compact         Coupe          28
    ## 4                    Luxury,Performance      Compact         Coupe          28
    ## 5                                Luxury      Compact   Convertible          28
    ## 6                    Luxury,Performance      Compact         Coupe          28
    ##   city.mpg Popularity  MSRP
    ## 1       19       3916 46135
    ## 2       19       3916 40650
    ## 3       20       3916 36350
    ## 4       18       3916 29450
    ## 5       18       3916 34500
    ## 6       18       3916 31200

### Task 2 - Calculate the Measure of Frequency Metrics

#### Number of Observations in Column

``` r
length(data$Transmission.Type)
```

    ## [1] 11914

#### Number of Unique Categories

``` r
length(unique(data$Transmission.Type))   
```

    ## [1] 5

#### Calculate Frequency Distribution of each Unique Category in a Column

``` r
table(data$Transmission.Type)
```

    ## 
    ## AUTOMATED_MANUAL        AUTOMATIC     DIRECT_DRIVE           MANUAL 
    ##              626             8266               68             2935 
    ##          UNKNOWN 
    ##               19

#### Store Output into Variable

``` r
freq <- sort(table(data$Transmission.Type), decreasing = TRUE) 

freq
```

    ## 
    ##        AUTOMATIC           MANUAL AUTOMATED_MANUAL     DIRECT_DRIVE 
    ##             8266             2935              626               68 
    ##          UNKNOWN 
    ##               19

### Task 3 - Calculate the Measure of Central Tendency Metrics

#### Mean

``` r
mean(data$Engine.HP)   
```

    ## [1] NA

``` r
mean(data$Engine.HP, na.rm=TRUE)  # 'na.rm=TRUE' removes missing values 
```

    ## [1] 249.3861

#### Median

``` r
median(data$Engine.HP, na.rm=TRUE) 
```

    ## [1] 227

#### Mode

``` r
uniqueValues <- unique(data$Engine.HP) #finds all unique values in column and store it in variable

uniqueValues [which.max(tabulate(match(data$Engine.HP, uniqueValues)))]  
```

    ## [1] 200

### Task 4 - Calculate the Measure of Dispersion Metrics

``` r
min(data$Engine.HP, na.rm= TRUE)  
```

    ## [1] 55

``` r
max(data$Engine.HP, na.rm= TRUE)  
```

    ## [1] 1001

``` r
range(data$Engine.HP, na.rm= TRUE) # gives mean and max values 
```

    ## [1]   55 1001

``` r
var(data$Engine.HP, na.rm= TRUE)  # variance measures how much each value is varying/deviating from mean
```

    ## [1] 11922.86

``` r
                            # doesn't use same units as the column and is not interpretable as a result

sd(data$Engine.HP, na.rm= TRUE) # standard deviation measures average deviation off each value of mean
```

    ## [1] 109.1919

``` r
                            # sq root of variance and has same units as the column
```

### Task 5 - Use R’s in-built Functions for Additional Data Quality Metrics

#### Find Data Type of Column

``` r
class(data$Engine.HP) 
```

    ## [1] "integer"

#### Calculate Number of Missing Values

``` r
sum(is.na(data$Transmission.Type)) 
```

    ## [1] 0

``` r
sum(is.na(data$Number.of.Doors))
```

    ## [1] 6

### Task 6 - Create Custom R Function to Calculate Descriptive Statistics on any given Dataset

#### Count Total Values in each Column

``` r
apply(data, MARGIN=2, length)
```

    ##              Make             Model              Year  Engine.Fuel.Type 
    ##             11914             11914             11914             11914 
    ##         Engine.HP  Engine.Cylinders Transmission.Type     Driven_Wheels 
    ##             11914             11914             11914             11914 
    ##   Number.of.Doors   Market.Category      Vehicle.Size     Vehicle.Style 
    ##             11914             11914             11914             11914 
    ##       highway.MPG          city.mpg        Popularity              MSRP 
    ##             11914             11914             11914             11914

#### Find Minimum Value of each Column

``` r
sapply(data, function(x) min(x, na.rm=TRUE)) 
```

    ##               Make              Model               Year   Engine.Fuel.Type 
    ##            "Acura"         "1 Series"             "1990"                 "" 
    ##          Engine.HP   Engine.Cylinders  Transmission.Type      Driven_Wheels 
    ##               "55"                "0" "AUTOMATED_MANUAL"  "all wheel drive" 
    ##    Number.of.Doors    Market.Category       Vehicle.Size      Vehicle.Style 
    ##                "2"        "Crossover"          "Compact"    "2dr Hatchback" 
    ##        highway.MPG           city.mpg         Popularity               MSRP 
    ##               "12"                "7"                "2"             "2000"

#### Revise Import Formula

``` r
data <- read.csv("data.csv", stringsAsFactors = FALSE) 
```

#### Create Dataframe showing all Descriptive

``` r
quality_data <- function(df=NULL){
  if(is.null(df)) print("Please pass a non-empty data frame")
  summary_table <- do.call(data.frame, 
                           list(
                             Min = sapply(df, function(x) min(x, na.rm=TRUE)),
                             Max = sapply(df, function(x) max(x, na.rm=TRUE)),
                             Mean = sapply(df, function(x) mean(x, na.rm=TRUE)),
                             SD = sapply(df, function(x) sd(x, na.rm=TRUE)),
                             Total = apply(df, 2, length),
                             NULLS = sapply(df, function(x) sum(is.na(x))),
                             Unique = sapply(df, function(x) length(unique(x))),
                             dataType = sapply(df, class)
                           ))
  nums <- vapply(summary_table, is.numeric, FUN.VALUE = logical(1))
  summary_table[,nums] <- round(summary_table[, nums], digits = 3)
  return(summary_table)
}
```

### Task 7 - Export Results of Descriptive Statistics to a Data Quality Report File

#### Use New Function to View Summary Statistics

``` r
df_quality <- quality_data(data)

df_quality 
```

    ##                                Min                Max      Mean        SD Total
    ## Make                         Acura              Volvo        NA        NA 11914
    ## Model                     1 Series             Zephyr        NA  1486.006 11914
    ## Year                          1990               2017  2010.384     7.580 11914
    ## Engine.Fuel.Type                     regular unleaded        NA        NA 11914
    ## Engine.HP                       55               1001   249.386   109.192 11914
    ## Engine.Cylinders                 0                 16     5.629     1.781 11914
    ## Transmission.Type AUTOMATED_MANUAL            UNKNOWN        NA        NA 11914
    ## Driven_Wheels      all wheel drive   rear wheel drive        NA        NA 11914
    ## Number.of.Doors                  2                  4     3.436     0.881 11914
    ## Market.Category          Crossover Performance,Hybrid        NA        NA 11914
    ## Vehicle.Size               Compact            Midsize        NA        NA 11914
    ## Vehicle.Style        2dr Hatchback              Wagon        NA        NA 11914
    ## highway.MPG                     12                354    26.637     8.863 11914
    ## city.mpg                         7                137    19.733     8.988 11914
    ## Popularity                       2               5657  1554.911  1441.855 11914
    ## MSRP                          2000            2065902 40594.737 60109.104 11914
    ##                   NULLS Unique  dataType
    ## Make                  0     48 character
    ## Model                 0    915 character
    ## Year                  0     28   integer
    ## Engine.Fuel.Type      0     11 character
    ## Engine.HP            69    357   integer
    ## Engine.Cylinders     30     10   integer
    ## Transmission.Type     0      5 character
    ## Driven_Wheels         0      4 character
    ## Number.of.Doors       6      4   integer
    ## Market.Category       0     72 character
    ## Vehicle.Size          0      3 character
    ## Vehicle.Style         0     16 character
    ## highway.MPG           0     59   integer
    ## city.mpg              0     69   integer
    ## Popularity            0     48   integer
    ## MSRP                  0   6049   integer

#### Convert Row Names into Separate Columns

``` r
df_quality <- cbind(Columns=rownames(df_quality), 
                    data.frame(df_quality, row.names = NULL))  


df_quality 
```

    ##              Columns              Min                Max      Mean        SD
    ## 1               Make            Acura              Volvo        NA        NA
    ## 2              Model         1 Series             Zephyr        NA  1486.006
    ## 3               Year             1990               2017  2010.384     7.580
    ## 4   Engine.Fuel.Type                    regular unleaded        NA        NA
    ## 5          Engine.HP               55               1001   249.386   109.192
    ## 6   Engine.Cylinders                0                 16     5.629     1.781
    ## 7  Transmission.Type AUTOMATED_MANUAL            UNKNOWN        NA        NA
    ## 8      Driven_Wheels  all wheel drive   rear wheel drive        NA        NA
    ## 9    Number.of.Doors                2                  4     3.436     0.881
    ## 10   Market.Category        Crossover Performance,Hybrid        NA        NA
    ## 11      Vehicle.Size          Compact            Midsize        NA        NA
    ## 12     Vehicle.Style    2dr Hatchback              Wagon        NA        NA
    ## 13       highway.MPG               12                354    26.637     8.863
    ## 14          city.mpg                7                137    19.733     8.988
    ## 15        Popularity                2               5657  1554.911  1441.855
    ## 16              MSRP             2000            2065902 40594.737 60109.104
    ##    Total NULLS Unique  dataType
    ## 1  11914     0     48 character
    ## 2  11914     0    915 character
    ## 3  11914     0     28   integer
    ## 4  11914     0     11 character
    ## 5  11914    69    357   integer
    ## 6  11914    30     10   integer
    ## 7  11914     0      5 character
    ## 8  11914     0      4 character
    ## 9  11914     6      4   integer
    ## 10 11914     0     72 character
    ## 11 11914     0      3 character
    ## 12 11914     0     16 character
    ## 13 11914     0     59   integer
    ## 14 11914     0     69   integer
    ## 15 11914     0     48   integer
    ## 16 11914     0   6049   integer

#### Export File

``` r
write.csv(df_quality, "Data Quality Report.csv", row.names = FALSE)
```

#### Export File without Overwriting

``` r
write.csv(df_quality, paste("Data Quality Report",
          format(Sys.time(), "%d-%m-%Y-%H%M%S"),
          ".csv"), 
          row.names = FALSE)
```
