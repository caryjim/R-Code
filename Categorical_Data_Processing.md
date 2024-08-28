Categorical Data Processing - Frequency and Contingency Table
================
Cary K. Jim
August 28, 2024

\#Import Libraries and Packages

``` r
library(flextable) #for making tables
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ purrr::compose() masks flextable::compose()
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ dplyr::lag()     masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl) #to read excel files
library(conflicted) #to detect conflicted packages and alert 
library(formattable) 
library(psych)
library(janitor)
library(officer) # to manipulate table when export to word
```

\#Setup Data Path & Import Files Note. There are 2 different read csv
functions used in this step

``` r
setwd("~/R")

#Step 1: Import the data file of coded headers
survey <- read.csv("Survey_dummy_data.csv", fileEncoding = "Windows-1252", header = TRUE)

#Check the column headers and you would see the labels and headers are both present
head(survey)
```

    ##   Workplace          Units                Q1                Q2
    ## 1      Site Workplace_Unit            Timely         Courteous
    ## 2   A_Place             SA    Strongly agree    Strongly agree
    ## 3   A_Place             JS Strongly disagree Strongly disagree
    ## 4   A_Place             NK    Strongly agree    Strongly agree
    ## 5   A_Place             GH Strongly disagree Strongly disagree
    ## 6   A_Place             SW             Agree             Agree
    ##                  Q3                Q4                        Q5
    ## 1           Helpful          Resolved              Environment 
    ## 2    Strongly agree    Strongly agree Not applicable/Don't know
    ## 3 Strongly disagree Strongly disagree         Strongly disagree
    ## 4    Strongly agree    Strongly agree            Strongly agree
    ## 5 Strongly disagree Strongly disagree         Strongly disagree
    ## 6             Agree             Agree                     Agree

``` r
#You want to save the labels for later use. You only want the survey items. Use col() to check index if needed
item_labels <- readr::read_csv("Survey_dummy_data.csv", col_select = c(3:7), n_max = 1, show_col_types = FALSE)

#Reload survey data and drop the row with labels
survey <- read.csv("Survey_dummy_data.csv", fileEncoding = "Windows-1252", header = TRUE)[-1,]
```

\#Review data & initial exploration This dummy data is a workplace
survey where different units gave ratings to the 5 criteria. This
initial step is to verify the number of workplaces and units in the
dataset.  
\# Unique Count of Groups

``` r
print(paste("Number of Unique Workplace:",
            length(unique(survey$Workplace))))
```

    ## [1] "Number of Unique Workplace: 5"

``` r
print(paste("Number of Unique Units:",
            length(unique(survey$Units))))
```

    ## [1] "Number of Unique Units: 14"

# Unique Values in each Group

We also want to see the name of each Workplace and Units as a list and
sorted by alphabetical order

``` r
print(paste("Name of the Workplace: ",
            sort(unique(survey$Workplace))))
```

    ## [1] "Name of the Workplace:  A_Place" "Name of the Workplace:  B_Place"
    ## [3] "Name of the Workplace:  C_Place" "Name of the Workplace:  D_Place"
    ## [5] "Name of the Workplace:  E_Place"

``` r
print(paste("Name of the Units:",
            (sort(unique(survey$Units))))
      )
```

    ##  [1] "Name of the Units: DM" "Name of the Units: DW" "Name of the Units: GH"
    ##  [4] "Name of the Units: JP" "Name of the Units: JS" "Name of the Units: LP"
    ##  [7] "Name of the Units: MR" "Name of the Units: MV" "Name of the Units: NK"
    ## [10] "Name of the Units: PR" "Name of the Units: RC" "Name of the Units: RM"
    ## [13] "Name of the Units: SA" "Name of the Units: SW"

\#Frequency Count of Values in each Group Frequency Count of Workplace

``` r
table(survey$Workplace)
```

    ## 
    ## A_Place B_Place C_Place D_Place E_Place 
    ##      14      20      26      27      12

Frequency Count of Units

``` r
table(survey$Units)
```

    ## 
    ## DM DW GH JP JS LP MR MV NK PR RC RM SA SW 
    ##  7  7  7  8  7  7  7  7  7  7  7  7  7  7

# Output Tables

For this survey, we have to produce 2 sets of table for each workplace
and units. A frequency table for counts and a contingency table of the
survey results. \## Set 1. Create Frequency Table \### Example 1.
Frequency Table fo Workplace

``` r
#Assign the column needed to the groupby function  
Freq_Table <-
  survey %>%
  group_by(Workplace)%>%
  summarise("Freq" = n()) %>% 
  mutate(Percent = (Freq/sum(Freq))) %>%
  adorn_totals("row") %>%
  ungroup()
  
#Format the Percent column
Freq_Table$Percent <- 
  formattable::percent(Freq_Table$Percent, 1)

#Make it a flextable object and set headers
Freq_Table <- 
  flextable(Freq_Table)%>%
  set_header_labels(Freq_Table,
                    Var1 = "Clinical Site",
                    Freq = "Frequency",
                    Percent = "Percent")

#Adjust the table size and export as word document 
save_as_docx(autofit(Freq_Table, 
                     add_w = 1, add_h = 1),
             path = "~/R/Tables_Charts/Workplace_Freq.docx")
#Preview (This automatically open a word doc)
#print(Freq_Table, preview = "docx")
```

### Example 2. Frequency Table of Units

``` r
#Assign the column needed to the groupby function  
Freq_Table <-
  survey %>%
  group_by(Units)%>%
  summarise("Freq" = n()) %>% 
  mutate(Percent = (Freq/sum(Freq))) %>%
  adorn_totals("row") %>%
  ungroup()
  
#Format the Percent column
Freq_Table$Percent <- 
  formattable::percent(Freq_Table$Percent, 1)

#Make it a flextable object and set headers
Freq_Table <- 
  flextable(Freq_Table)%>%
  set_header_labels(Freq_Table,
                    Var1 = "Clinical Site",
                    Freq = "Frequency",
                    Percent = "Percent")

#Adjust the table size and export as word document 
save_as_docx(autofit(Freq_Table, 
                     add_w = 1, add_h = 1),
             path = "~/R/Tables_Charts/Workplace_Freq.docx")
#Preview (This automatically open a word doc)
#print(Freq_Table, preview = "docx")
```

## Set 2. Contingency Table (All groups)

For this output, the traditional way of making a contingency table will
be combined with a mean calculation in the output. The following steps
will pre-made the table of mean scores to be used at a later stage in a
joined table.

### Recode data and make a numeric dataframe

``` r
#Setup the necessary factor codes for conversion
#Ensure the category starts with the lowest value in order. For example, Strongly disagree with be 1
survey_num <- survey
category_labels <- c("Strongly disagree", "Disagree", "Agree", "Strongly agree", "Not applicable/Don't know")
questions_to_recode <- c("Q1", "Q2", "Q3", "Q4", "Q5")

#Convert the values to factor levels and then make it a numerical value 
survey_num[questions_to_recode] <- lapply(survey_num[questions_to_recode], function(x) {
           numeric_values <- as.numeric(factor(x, levels = category_labels))
           replace(numeric_values, numeric_values == 5, NA)
    })
#Check data type to confirm
str(survey_num)
```

    ## 'data.frame':    99 obs. of  7 variables:
    ##  $ Workplace: chr  "A_Place" "A_Place" "A_Place" "A_Place" ...
    ##  $ Units    : chr  "SA" "JS" "NK" "GH" ...
    ##  $ Q1       : num  4 1 4 1 3 3 2 4 4 3 ...
    ##  $ Q2       : num  4 1 4 1 3 3 3 4 4 3 ...
    ##  $ Q3       : num  4 1 4 1 3 3 4 4 4 3 ...
    ##  $ Q4       : num  4 1 4 1 3 3 4 4 4 3 ...
    ##  $ Q5       : num  NA 1 4 1 3 3 3 4 4 NA ...

## Mean Calculation

### Items Means for All Workplace

``` r
#Transpose the dataframe from wide to long format
workplace_mean <- 
  survey_num %>%
  gather(ItemQ, LevelQ , -c(Workplace, Units)) %>%
  arrange(Workplace) %>%
  group_by(ItemQ) %>%
  summarise("Mean" = round(mean(LevelQ, na.rm = TRUE), 2))
```

### Items Means by Individual Workplace

This table has 3 columns: Workplace, ItemQ, and Mean

``` r
workplace_indv_mean <- 
  survey_num %>%
  gather(ItemQ, LevelQ , -c(Workplace, Units)) %>%
  arrange(Workplace) %>%
  group_by(Workplace, ItemQ) %>%
  summarise("Mean" = round(mean(LevelQ, na.rm = TRUE), 2))
```

    ## `summarise()` has grouped output by 'Workplace'. You can override using the
    ## `.groups` argument.

## Contingency Tables

### Create a long shape table for all workplace

Workplace_long_table has 3 columns, Workplace, ItemQ, and Response_Scale

``` r
#Setup factor levels for the categorical dataframe
#Previously, we have already setup the "category_labels" variable 
#category_labels <- c("Strongly disagree", "Disagree", "Agree", "Strongly agree", "Not applicable/Don't know")

workplace_long_table <- 
  survey[, c(1, 3:7)] %>%  #select the needed columns
  gather(ItemQ, Response_Scale, -c(Workplace)) %>%
  mutate(Response_Scale = factor(Response_Scale,
                                 levels = category_labels))
str(workplace_long_table)
```

    ## 'data.frame':    495 obs. of  3 variables:
    ##  $ Workplace     : chr  "A_Place" "A_Place" "A_Place" "A_Place" ...
    ##  $ ItemQ         : chr  "Q1" "Q1" "Q1" "Q1" ...
    ##  $ Response_Scale: Factor w/ 5 levels "Strongly disagree",..: 4 1 4 1 3 3 2 4 4 3 ...

``` r
#To check if the result table is correct or not, we can use table()
table(survey$Workplace) 
```

    ## 
    ## A_Place B_Place C_Place D_Place E_Place 
    ##      14      20      26      27      12

``` r
#It means that we should see 14 rows of Q1 for A_Place, 20 rows for B_Place and so on. 
```

Although we only have 5 workplace in this example. We want to develop a
way to process this type of data in a scalable way. The next step
demonstrate how to create a subset dataframe and each indexed subset
will be feed into a loop to generate these cross tabs.

``` r
# Create the table headers variable
table_headers <- c("Survey Items", "1-Strongly Disagree", "2-Disagree",
  "3-Agree", "4-Strongly Agree", "Not Applicable/Don't Know",
  "Total Response", "Mean score on 4-point scale")

# Create the list of question label for mapping to the final output_items
item_statements <- as.list(item_labels)
```

### Cross-tabs for All Workplace

The WP_crosstab has 7 columns, ItemQ, Response Scale from Strongly
Disagree to Not Applicale, and Total. The WP_xtab_final is first
combining WP_crosstab with workplace_mean df. Then, the item statements
are added to the df and final headers are renamed.

``` r
WP_crosstab <-
  sjPlot::tab_xtab(
  var.row = workplace_long_table$ItemQ,
  var.col = workplace_long_table$Response_Scale,
  show.summary = FALSE,
  show.row.prc = TRUE,
  use.viewer = FALSE) %>%
  sjtable2df::xtab2df(., output = "data.frame") %>%
  dplyr::filter(ItemQ != "Total") #remove the total row from xtab

## Add mean to the last column
WP_xtab_final <- 
  left_join(WP_crosstab, workplace_mean, by = join_by("ItemQ"))

WP_xtab_final <- 
  WP_xtab_final %>%
  mutate(statements = recode(ItemQ, !!!item_statements)) %>%
  relocate(statements, .before = ItemQ) %>%
  select(-2) %>% #Drop ItemQ column as it is no longer needed 
  purrr::set_names(table_headers) #Redo headers

#Export docx
save_as_docx(flextable(WP_xtab_final),
             path = "~/R/Tables_Charts/Wp_xtab_final.docx")
#Print to check as well
#print(WP_final)
```

### Subset Workplace Dataframe & Mean Workplace Dataframe

``` r
#Split the All Workplace long table
WP_long_split <- 
  split(workplace_long_table, workplace_long_table$Workplace)

#Split the Workplace mean for individual workplace
WP_indv_mean_split <- 
  split(workplace_indv_mean, workplace_indv_mean$Workplace)

#Check the split df, if needed 
#str(WP_split)
```

### Generate Individual Workplace Contingency Table

This script generates the individual table and export each in a word
document with a header above the table.

``` r
#Assign a variable to get the unique label of Workplace
WP_name <- unique(workplace_long_table$Workplace)

for (wp in WP_name) {
  
  # Select the subset df at each iteration of the loop
  current_df <- WP_long_split[[wp]]  
  
  # Create the cross_tab table
  ind_WP_xtab <- 
    sjPlot::tab_xtab(
    var.row = current_df$ItemQ,
    var.col = current_df$Response_Scale,
    show.summary = FALSE,
    show.row.prc = TRUE,
    use.viewer = FALSE) %>% 
    sjtable2df::xtab2df(., output = "data.frame") %>%
    dplyr::filter(ItemQ != "Total") # remove the total row from xtab
  
  #Join additional data
  ind_WP_xtab <- left_join(ind_WP_xtab, WP_indv_mean_split[[wp]],
                      by = join_by("ItemQ"))
  
  #Recode, relocate, and rename columns
  ind_WP_xtab <- 
    ind_WP_xtab %>%
    mutate(statements = recode(ItemQ, !!!item_statements)) %>%
    relocate(statements, .before = ItemQ) %>%
    select(-c(2,9)) %>%
    purrr::set_names(table_headers)
  
  #Create flextable object 
  WP_flextable <- flextable(ind_WP_xtab)
  
#Add label to each cross tab 
  doc <- read_docx() %>%
    body_add_par(paste("Workplace:", wp), style = "heading 2") %>%
    # Add label
    body_add_flextable(WP_flextable) %>%
    body_add_par(" ", style = "Normal") 
  # Add a blank line after the table

#Export docx
output_folder <- "~/R/Tables_Charts/"

print(doc, target = paste0(output_folder, "Individual_Workpkace_", wp, ".docx"))

#Execution confirmation
message("Exported table for Workplace:", wp)

}
```

    ## Exported table for Workplace:A_Place

    ## Exported table for Workplace:B_Place

    ## Exported table for Workplace:C_Place

    ## Exported table for Workplace:D_Place

    ## Exported table for Workplace:E_Place

### Generate One Word Doc of all Tables

``` r
library(officer)
library(flextable)

# Assign a variable to get the unique label of Workplace
WP_name <- unique(workplace_long_table$Workplace)

# Create a new Word document
doc <- read_docx()

for (wp in WP_name) {
  
  # Select the subset df at each iteration of the loop
  current_df <- WP_long_split[[wp]]  
  
  # Create the cross_tab table
  ind_WP_xtab <- 
    sjPlot::tab_xtab(
      var.row = current_df$ItemQ,
      var.col = current_df$Response_Scale,
      show.summary = FALSE,
      show.row.prc = TRUE,
      use.viewer = FALSE) %>% 
    sjtable2df::xtab2df(., output = "data.frame") %>%
    dplyr::filter(ItemQ != "Total") 
  
  # Join additional data
  ind_WP_xtab <- left_join(ind_WP_xtab,
                           WP_indv_mean_split[[wp]],
                           by = join_by("ItemQ"))
  
  # Recode, relocate, and rename columns
  ind_WP_xtab <- 
    ind_WP_xtab %>%
    mutate(statements = recode(ItemQ, !!!item_statements)) %>%
    relocate(statements, .before = ItemQ) %>%
    select(-c(2,9)) %>%
    purrr::set_names(table_headers) # Redo headers
  
  # Create a flextable object
  WP_flextable <- flextable(ind_WP_xtab)
  
  # Add a label for each table and add the table to the document
  doc <- doc %>%
    body_add_par(paste("Table for Workplace:", wp), style = "heading 2") %>% # Add label
    body_add_flextable(WP_flextable) %>%
    body_add_par(" ", style = "Normal") # Add a blank line after the table
}

# Save the combined document with all tables
print(doc, target = "~/R/Tables_Charts/Combined_Workplace_Tables.docx")

# Execution confirmation
message("Exported combined document with tables for all Workplaces")
```

    ## Exported combined document with tables for all Workplaces
