library(plyr)
library(dplyr)
library (ggplot2)
library(tidyverse)
library(janitor)

setwd("C:/Users/PJNat/Dropbox/Projects/IITA_Consult/Data")
profile_data<-read.csv("C:/Users/PJNat/Dropbox/Projects/IITA_Consult/Data/Profile_data_v2.csv")
DQ_data<-read.csv("C:/Users/PJNat/Dropbox/Projects/IITA_Consult/Data/DQQ_Dataset_12_12_2024_v3.csv")
mrg_data <- inner_join(profile_data, DQ_data, by = "Respondent_ID")

qc_data<-subset(mrg_data, mrg_data$QC1 =="0" & mrg_data$QC2 =="0" & mrg_data$QC3 =="0"& mrg_data$QC4 =="1")

qc_data$Date <- as.Date(qc_data$today, format = "%m/%d/%Y")

qc_data$dayname<-weekdays(as.Date(qc_data$Date))

qc_monday<-subset(qc_data, qc_data$dayname=="Monday")
qc_thursday<-subset(qc_data, qc_data$dayname=="Thursday")
qc_mon_thur<-subset(qc_data, qc_data$dayname=="Thursday" | qc_data$dayname=="Monday")



qc_data_wk<- qc_data %>%
  mutate(week = week(ymd(Date)))



###Profile data analyze


profile_data$Gender_class <- as.factor(profile_data$Gender_class)
ggplot(profile_data, aes(x = Age)) + geom_histogram() + facet_wrap(~Gender_class)

basic_plot <-  ggplot(
  profile_data, 
  aes(
    x = Age, 
    fill = Gender_class, 
    y = ifelse(
      test = Gender_class == "male", 
      yes = -Population, 
      no = Population
    )
  )
) + 
  geom_bar(stat = "identity") 

basic_plot

# Creating a sample dataset
data <- data.frame(
  Age = c(0:9, 0:9),
  Gender = c(rep("Male", 10), rep("Female", 10)),
  Population = c(200, 250, 300, 350, 400, 450, 500, 550, 600, 650,
                 190, 240, 290, 330, 380, 430, 480, 530, 580, 630)
)

head(data)



##DQ data analyze

# Main function
# Main function
dqqWebCalculator <- function(data) {
  
  foodGroup <- function(data) {
    data <- data %>% 
      rename(
        CaseID = "Respondent_ID", 
        Gender = "Gender", 
        District = "LGA_Code", 
        Age = "Age",
        Economic = "Economic.Category",
        DQQ1 = "X01_Foods_made_from_grains", 
        DQQ2 = "X02_Whole_grains", 
        DQQ3 = "X03_White_roots_or_tubers", 
        DQQ4 = "X04_Pulses", 
        DQQ5 = "X05_Vitamin_A.rich_orange_vegetables", 
        DQQ6 = "X06_Dark_green_leafy_vegetables", 
        DQQ7 = "X07_Other_vegetables",  
        DQQ8 = "X08_Vitamin_A.rich_fruits", 
        DQQ9 = "X09_Citrus", 
        DQQ10 = "X10_Other_fruits", 
        DQQ11 = "X11_Baked_or_grain.based_sweets", 
        DQQ12 = "X12_Other_sweets", 
        DQQ13 = "X13_Eggs", 
        DQQ14 = "X14_Cheese", 
        DQQ15 = "X15_Yogurt", 
        DQQ16 = "X16_Processed_meats", 
        DQQ17 = "X17_Unprocessed_red_meat_ruminant", 
        DQQ18 = "X18_Unprocessed_red_meat_non_ruminant", 
        DQQ19 = "X19_Poultry", 
        DQQ20 = "X20_Fish_or_seafood", 
        DQQ21 = "X21_Nuts_or_seeds", 
        DQQ22 = "X22_Packaged_ultra.processed_salty_snacks", 
        DQQ23 = "X23_Instant_noodles", 
        DQQ24 = "X24_Deep_fried_foods", 
        DQQ25 = "X25_Milk", 
        DQQ26 = "X26_Sweet.tea_coffee_or_cocoa", 
        DQQ27 = "X27_Fruit_juice_or_fruit_drinks", 
        DQQ28 = "X28_Soft_drinks", 
        DQQ29 = "X29_Fast_food" 
      ) %>%
      mutate(
        ncdp = ((rowSums(pick("DQQ2") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ8") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ9") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ10") == 1, na.rm=TRUE) > 0)),
        
        ncdr = ((rowSums(pick("DQQ28") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ11") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ12") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ17","DQQ18") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ24") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ23", "DQQ29") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ22") == 1, na.rm=TRUE) > 0)),
        
        gdr = (ncdp - ncdr + 9), 
        
        all5a = ifelse((rowSums(pick("DQQ5", "DQQ6", "DQQ7") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5b = ifelse((rowSums(pick("DQQ8","DQQ9", "DQQ10") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5c = ifelse((rowSums(pick("DQQ4","DQQ21") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5d = ifelse((rowSums(pick("DQQ13", "DQQ14", "DQQ15", "DQQ16", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5e = ifelse((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5 = ifelse((all5a + all5b + all5c + all5d + all5e) == 5, 1, 0),
        
        fgds = ((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ14","DQQ15", "DQQ25") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ13") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5","DQQ8") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ9","DQQ10") == 1, na.rm=TRUE) > 0)),
        
        mddw = ifelse((fgds >= 5 & Gender == 1 &  Age >= 15 & Age <= 49), 1, 
                      ifelse((fgds < 5 & Gender == 1 &  Age >= 15 & Age <= 49), 0, NA)),
        zvegfr = ifelse((rowSums(pick("DQQ5","DQQ6", "DQQ7", "DQQ8", "DQQ9", "DQQ10") == 1, 
                                 na.rm=TRUE) > 0) == TRUE, 0, 1),
        vegfr = ifelse((rowSums(pick("DQQ5","DQQ6", "DQQ7", "DQQ8", "DQQ9", "DQQ10") == 1, 
                                na.rm=TRUE) > 0) == TRUE, 1, 0),
        safd = ifelse((rowSums(pick("DQQ22","DQQ23","DQQ24") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        swtfd = ifelse((rowSums(pick("DQQ11", "DQQ12") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        # extra 
        swtbev = ifelse((rowSums(pick("DQQ26","DQQ27","DQQ28") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        snf = ifelse((rowSums(pick("DQQ22","DQQ23","DQQ29") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        dairy = ifelse((rowSums(pick("DQQ14","DQQ15","DQQ25") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        dveg = ifelse((rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        anml = ifelse((rowSums(pick("DQQ16", "DQQ17","DQQ18", "DQQ19", "DQQ20") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        ofr = ifelse((rowSums(pick("DQQ10") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        oveg = ifelse((rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        umeat = ifelse((rowSums(pick("DQQ17","DQQ18") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5e = ifelse((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
      )
  }
  dataComplete <- foodGroup(data)
  
  Gender <- fct_collapse(as.factor(dataComplete$Gender), "Female" = "1", "Male" = "0")
  dataComplete$Gender <- Gender
  
  District <- fct_collapse(as.factor(dataComplete$District))
  dataComplete$District <- District
  
  Age <- fct_collapse(as.factor(dataComplete$Age))
  dataComplete$Age <- Age
  
  Economic <- fct_collapse(as.factor(dataComplete$Economic))
  dataComplete$Economic <- Economic
  
  dat <- dataComplete %>% 
    #pivot_longer(cols = DQQ1:swtfd, names_to = "Indicator", values_to = "Value") %>%
    pivot_longer(cols = c("Gender", "District","Age", "Economic"), names_to = "Sub", values_to = "Subgroup") %>%
    select(!Sub) %>%
    group_by(Subgroup) %>%
    reframe(
      "GDR score" = round(mean(gdr, na.rm = TRUE), digits = 2),
      "NCD-Protect" = round(mean(ncdp, na.rm = TRUE), digits = 2),
      "NCD-Risk" = round(mean(ncdr, na.rm = TRUE), digits = 2),
      "All-5" = round(mean(all5 == 1, na.rm = TRUE)*100, digits = 2),
      "At least one vegetable" = round(mean(all5a == 1, na.rm = TRUE)*100, digits = 2),
      "At least one fruit" = round(mean(all5b == 1, na.rm = TRUE)*100, digits = 2),
      "At least one pulse, nut, or seed" = round(mean(all5c == 1, na.rm = TRUE)*100, digits = 2),
      "At least one animal-source food" = round(mean(all5d == 1, na.rm = TRUE)*100, digits = 2),
      "At least one starchy staple food" = round(mean(all5e == 1, na.rm = TRUE)*100, digits = 2),
      "MDD-W" = round(mean(mddw == 1, na.rm = TRUE)*100, digits = 2),
      "Dietary diversity score" = round(mean(fgds, na.rm = TRUE), digits = 2),
      "Zero vegetable or fruit consumption" = round(mean(zvegfr == 1, na.rm = TRUE)*100, digits = 2),
      "At least one vegetable or fruit" = round(mean(vegfr == 1, na.rm = TRUE)*100, digits = 2),
      "Pulse consumption" = round(mean(DQQ4 == 1, na.rm = TRUE)*100, digits = 2),
      "Nuts or seeds consumption" = round(mean(DQQ21 == 1, na.rm = TRUE)*100, digits = 2),
      "Whole grain consumption" = round(mean(DQQ2 == 1, na.rm = TRUE)*100, digits = 2),
      "Processed meat consumption" = round(mean(DQQ16 == 1, na.rm = TRUE)*100, digits = 2),
      "Salty snacks, instant noodles, or fast food" = round(mean(snf == 1, na.rm = TRUE)*100, digits = 2),
      "Salty or fried snack consumption" = round(mean(safd == 1, na.rm = TRUE)*100, digits = 2),
      "Deep fried foods" = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2),
      "Sweet foods consumption" = round(mean(swtfd == 1, na.rm = TRUE)*100, digits = 2),
      "Sweet beverages" = round(mean(swtbev == 1, na.rm = TRUE)*100, digits = 2),
      "Soft drink consumption" = round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2),
      "01 Foods made from grains" = round(mean(DQQ1 == 1, na.rm = TRUE)*100, digits = 2),
      "02 Whole grains" = round(mean(DQQ2 == 1, na.rm = TRUE)*100, digits = 2),
      "03 White roots or tubers" = round(mean(DQQ3 == 1, na.rm = TRUE)*100, digits = 2),
      "04 Pulses" = round(mean(DQQ4 == 1, na.rm = TRUE)*100, digits = 2),
      "05 Vitamin A-rich orange vegetables" = round(mean(DQQ5 == 1, na.rm = TRUE)*100, digits = 2),
      "06 Dark green leafy vegetables" = round(mean(dveg == 1, na.rm = TRUE)*100, digits = 2),
      "07 Other vegetables" = round(mean(oveg == 1, na.rm = TRUE)*100, digits = 2),
      "08 Vitamin A-rich fruits" = round(mean(DQQ8 == 1, na.rm = TRUE)*100, digits = 2),
      "09 Citrus" = round(mean(DQQ9 == 1, na.rm = TRUE)*100, digits = 2),
      "10 Other fruits" = round(mean(ofr == 1, na.rm = TRUE)*100, digits = 2),
      "11 Baked or grain-based sweets" = round(mean(DQQ11 == 1, na.rm = TRUE)*100, digits = 2),
      "12 Other sweets"= round(mean(DQQ12 == 1, na.rm = TRUE)*100, digits = 2),
      "13 Eggs" = round(mean(DQQ13 == 1, na.rm = TRUE)*100, digits = 2),
      "14 Cheese" = round(mean(DQQ14 == 1, na.rm = TRUE)*100, digits = 2),
      "15 Yogurt" = round(mean(DQQ15 == 1, na.rm = TRUE)*100, digits = 2),
      "16 Processed meats" = round(mean(DQQ16 == 1, na.rm = TRUE)*100, digits = 2),
      "17 Unprocessed red meat (ruminant)" = round(mean(DQQ17 == 1, na.rm = TRUE)*100, digits = 2),
      "18 Unprocessed red meat (non-ruminant)" = round(mean(DQQ18 == 1, na.rm = TRUE)*100, digits = 2),
      "19 Poultry" = round(mean(DQQ19 == 1, na.rm = TRUE)*100, digits = 2),
      "20 Fish or seafood" = round(mean(DQQ20 == 1, na.rm = TRUE)*100, digits = 2),
      "21 Nuts or seeds" = round(mean(DQQ21 == 1, na.rm = TRUE)*100, digits = 2),
      "22 Packaged ultra-processed salty snacks" = round(mean(DQQ22 == 1, na.rm = TRUE)*100, digits = 2),
      "23 Instant noodles" = round(mean(DQQ23 == 1, na.rm = TRUE)*100, digits = 2),
      "24 Deep fried foods" = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2),
      "25 Fluid milk" = round(mean(DQQ25 == 1, na.rm = TRUE)*100, digits = 2),
      "26 Sweet tea, coffee, or cocoa" = round(mean(DQQ26 == 1, na.rm = TRUE)*100, digits = 2),
      "27 Fruit juice and fruit drinks" = round(mean(DQQ27 == 1, na.rm = TRUE)*100, digits = 2),
      "28 Soft drinks" = round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2),
      "29 Fast food" = round(mean(DQQ29 == 1, na.rm = TRUE)*100, digits = 2),
      "Meat, poultry, or fish" = round(mean(anml == 1, na.rm = TRUE)*100, digits = 2),
      "Unprocessed red meat" = round(mean(umeat == 1, na.rm = TRUE)*100, digits = 2),
      "Dairy" = round(mean(dairy == 1, na.rm = TRUE)*100, digits = 2),
    )
  
  datAll <- dataComplete %>% 
    group_by(Country) %>%
    reframe(
      "GDR score" = round(mean(gdr, na.rm = TRUE), digits = 2),
      "NCD-Protect" = round(mean(ncdp, na.rm = TRUE), digits = 2),
      "NCD-Risk" = round(mean(ncdr, na.rm = TRUE), digits = 2),
      "All-5" = round(mean(all5 == 1, na.rm = TRUE)*100, digits = 2),
      "At least one vegetable" = round(mean(all5a == 1, na.rm = TRUE)*100, digits = 2),
      "At least one fruit" = round(mean(all5b == 1, na.rm = TRUE)*100, digits = 2),
      "At least one pulse, nut, or seed" = round(mean(all5c == 1, na.rm = TRUE)*100, digits = 2),
      "At least one animal-source food" = round(mean(all5d == 1, na.rm = TRUE)*100, digits = 2),
      "At least one starchy staple food" = round(mean(all5e == 1, na.rm = TRUE)*100, digits = 2),
      "MDD-W" = round(mean(mddw == 1, na.rm = TRUE)*100, digits = 2),
      "Dietary diversity score" = round(mean(fgds, na.rm = TRUE), digits = 2),
      "Zero vegetable or fruit consumption" = round(mean(zvegfr == 1, na.rm = TRUE)*100, digits = 2),
      "At least one vegetable or fruit" = round(mean(vegfr == 1, na.rm = TRUE)*100, digits = 2),
      "Pulse consumption" = round(mean(DQQ4 == 1, na.rm = TRUE)*100, digits = 2),
      "Nuts or seeds consumption" = round(mean(DQQ21 == 1, na.rm = TRUE)*100, digits = 2),
      "Whole grain consumption" = round(mean(DQQ2 == 1, na.rm = TRUE)*100, digits = 2),
      "Processed meat consumption" = round(mean(DQQ16 == 1, na.rm = TRUE)*100, digits = 2),
      "Salty snacks, instant noodles, or fast food" = round(mean(snf == 1, na.rm = TRUE)*100, digits = 2),
      "Salty or fried snack consumption" = round(mean(safd == 1, na.rm = TRUE)*100, digits = 2),
      "Deep fried foods" = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2),
      "Sweet foods consumption" = round(mean(swtfd == 1, na.rm = TRUE)*100, digits = 2),
      "Sweet beverages" = round(mean(swtbev == 1, na.rm = TRUE)*100, digits = 2),
      "Soft drink consumption" = round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2),
      "01 Foods made from grains" = round(mean(DQQ1 == 1, na.rm = TRUE)*100, digits = 2),
      "02 Whole grains" = round(mean(DQQ2 == 1, na.rm = TRUE)*100, digits = 2),
      "03 White roots or tubers" = round(mean(DQQ3 == 1, na.rm = TRUE)*100, digits = 2),
      "04 Pulses" = round(mean(DQQ4 == 1, na.rm = TRUE)*100, digits = 2),
      "05 Vitamin A-rich orange vegetables" = round(mean(DQQ5 == 1, na.rm = TRUE)*100, digits = 2),
      "06 Dark green leafy vegetables" = round(mean(dveg == 1, na.rm = TRUE)*100, digits = 2),
      "07 Other vegetables" = round(mean(oveg == 1, na.rm = TRUE)*100, digits = 2),
      "08 Vitamin A-rich fruits" = round(mean(DQQ8 == 1, na.rm = TRUE)*100, digits = 2),
      "09 Citrus" = round(mean(DQQ9 == 1, na.rm = TRUE)*100, digits = 2),
      "10 Other fruits" = round(mean(ofr == 1, na.rm = TRUE)*100, digits = 2),
      "11 Baked or grain-based sweets" = round(mean(DQQ11 == 1, na.rm = TRUE)*100, digits = 2),
      "12 Other sweets"= round(mean(DQQ12 == 1, na.rm = TRUE)*100, digits = 2),
      "13 Eggs" = round(mean(DQQ13 == 1, na.rm = TRUE)*100, digits = 2),
      "14 Cheese" = round(mean(DQQ14 == 1, na.rm = TRUE)*100, digits = 2),
      "15 Yogurt" = round(mean(DQQ15 == 1, na.rm = TRUE)*100, digits = 2),
      "16 Processed meats" = round(mean(DQQ16 == 1, na.rm = TRUE)*100, digits = 2),
      "17 Unprocessed red meat (ruminant)" = round(mean(DQQ17 == 1, na.rm = TRUE)*100, digits = 2),
      "18 Unprocessed red meat (non-ruminant)" = round(mean(DQQ18 == 1, na.rm = TRUE)*100, digits = 2),
      "19 Poultry" = round(mean(DQQ19 == 1, na.rm = TRUE)*100, digits = 2),
      "20 Fish or seafood" = round(mean(DQQ20 == 1, na.rm = TRUE)*100, digits = 2),
      "21 Nuts or seeds" = round(mean(DQQ21 == 1, na.rm = TRUE)*100, digits = 2),
      "22 Packaged ultra-processed salty snacks" = round(mean(DQQ22 == 1, na.rm = TRUE)*100, digits = 2),
      "23 Instant noodles" = round(mean(DQQ23 == 1, na.rm = TRUE)*100, digits = 2),
      "24 Deep fried foods" = round(mean(DQQ24 == 1, na.rm = TRUE)*100, digits = 2),
      "25 Fluid milk" = round(mean(DQQ25 == 1, na.rm = TRUE)*100, digits = 2),
      "26 Sweet tea, coffee, or cocoa" = round(mean(DQQ26 == 1, na.rm = TRUE)*100, digits = 2),
      "27 Fruit juice and fruit drinks" = round(mean(DQQ27 == 1, na.rm = TRUE)*100, digits = 2),
      "28 Soft drinks" = round(mean(DQQ28 == 1, na.rm = TRUE)*100, digits = 2),
      "29 Fast food" = round(mean(DQQ29 == 1, na.rm = TRUE)*100, digits = 2),
      "Meat, poultry, or fish" = round(mean(anml == 1, na.rm = TRUE)*100, digits = 2),
      "Unprocessed red meat" = round(mean(umeat == 1, na.rm = TRUE)*100, digits = 2),
      "Dairy" = round(mean(dairy == 1, na.rm = TRUE)*100, digits = 2)) %>%
    rename(Subgroup = Country) %>%
    mutate(Subgroup = "All") 
  
  dqqResults <- bind_rows(datAll, dat) %>%
    t  %>% 
    as.data.frame %>% 
    row_to_names(row_number = 1) %>%
    rownames_to_column(var = "Indicator")
  
  
  write_csv(dqqResults, "C:/Users/PJNat/Dropbox/Projects/IITA_Consult/Nasarawa_results/dqqnasarawa_results.csv")
}

dqqWebCalculator(qc_data)


##Focusing only on summary metrics
dqqWebCalculator2 <- function(data) {
  
  foodGroup2 <- function(data) {
    data <- data %>% 
      rename(
        CaseID = "Respondent_ID", 
        Gender = "Gender", 
        District = "LGA_Code", 
        Age = "Age",
        Economic = "Economic.Category",
        DQQ1 = "X01_Foods_made_from_grains", 
        DQQ2 = "X02_Whole_grains", 
        DQQ3 = "X03_White_roots_or_tubers", 
        DQQ4 = "X04_Pulses", 
        DQQ5 = "X05_Vitamin_A.rich_orange_vegetables", 
        DQQ6 = "X06_Dark_green_leafy_vegetables", 
        DQQ7 = "X07_Other_vegetables",  
        DQQ8 = "X08_Vitamin_A.rich_fruits", 
        DQQ9 = "X09_Citrus", 
        DQQ10 = "X10_Other_fruits", 
        DQQ11 = "X11_Baked_or_grain.based_sweets", 
        DQQ12 = "X12_Other_sweets", 
        DQQ13 = "X13_Eggs", 
        DQQ14 = "X14_Cheese", 
        DQQ15 = "X15_Yogurt", 
        DQQ16 = "X16_Processed_meats", 
        DQQ17 = "X17_Unprocessed_red_meat_ruminant", 
        DQQ18 = "X18_Unprocessed_red_meat_non_ruminant", 
        DQQ19 = "X19_Poultry", 
        DQQ20 = "X20_Fish_or_seafood", 
        DQQ21 = "X21_Nuts_or_seeds", 
        DQQ22 = "X22_Packaged_ultra.processed_salty_snacks", 
        DQQ23 = "X23_Instant_noodles", 
        DQQ24 = "X24_Deep_fried_foods", 
        DQQ25 = "X25_Milk", 
        DQQ26 = "X26_Sweet.tea_coffee_or_cocoa", 
        DQQ27 = "X27_Fruit_juice_or_fruit_drinks", 
        DQQ28 = "X28_Soft_drinks", 
        DQQ29 = "X29_Fast_food" 
      ) %>%
      mutate(
        ncdp = ((rowSums(pick("DQQ2") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ8") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ9") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ10") == 1, na.rm=TRUE) > 0)),
        
        ncdr = ((rowSums(pick("DQQ28") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ11") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ12") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ17","DQQ18") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ24") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ23", "DQQ29") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ22") == 1, na.rm=TRUE) > 0)),
        
        gdr = (ncdp - ncdr + 9), 
        
        all5a = ifelse((rowSums(pick("DQQ5", "DQQ6", "DQQ7") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5b = ifelse((rowSums(pick("DQQ8","DQQ9", "DQQ10") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5c = ifelse((rowSums(pick("DQQ4","DQQ21") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5d = ifelse((rowSums(pick("DQQ13", "DQQ14", "DQQ15", "DQQ16", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5e = ifelse((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5 = ifelse((all5a + all5b + all5c + all5d + all5e) == 5, 1, 0),
        
        fgds = ((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ14","DQQ15", "DQQ25") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ13") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5","DQQ8") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ9","DQQ10") == 1, na.rm=TRUE) > 0)),
        
        mddw = ifelse((fgds >= 5 & Gender == 1 &  Age >= 15 & Age <= 49), 1, 
                      ifelse((fgds < 5 & Gender == 1 &  Age >= 15 & Age <= 49), 0, NA)),
        zvegfr = ifelse((rowSums(pick("DQQ5","DQQ6", "DQQ7", "DQQ8", "DQQ9", "DQQ10") == 1, 
                                 na.rm=TRUE) > 0) == TRUE, 0, 1),
        vegfr = ifelse((rowSums(pick("DQQ5","DQQ6", "DQQ7", "DQQ8", "DQQ9", "DQQ10") == 1, 
                                na.rm=TRUE) > 0) == TRUE, 1, 0),
        safd = ifelse((rowSums(pick("DQQ22","DQQ23","DQQ24") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        swtfd = ifelse((rowSums(pick("DQQ11", "DQQ12") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        # extra 
        swtbev = ifelse((rowSums(pick("DQQ26","DQQ27","DQQ28") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        snf = ifelse((rowSums(pick("DQQ22","DQQ23","DQQ29") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        dairy = ifelse((rowSums(pick("DQQ14","DQQ15","DQQ25") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        dveg = ifelse((rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        anml = ifelse((rowSums(pick("DQQ16", "DQQ17","DQQ18", "DQQ19", "DQQ20") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        ofr = ifelse((rowSums(pick("DQQ10") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        oveg = ifelse((rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        umeat = ifelse((rowSums(pick("DQQ17","DQQ18") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5e = ifelse((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
      )
  }
  dataComplete <- foodGroup2(data)
  
  Gender <- fct_collapse(as.factor(dataComplete$Gender), "Female" = "1", "Male" = "0")
  dataComplete$Gender <- Gender
  
  District <- fct_collapse(as.factor(dataComplete$District))
  dataComplete$District <- District
  
  Age <- fct_collapse(as.factor(dataComplete$Age))
  dataComplete$Age <- Age
  
  Economic <- fct_collapse(as.factor(dataComplete$Economic))
  dataComplete$Economic <- Economic
  
  dat <- dataComplete %>% 
    #pivot_longer(cols = DQQ1:swtfd, names_to = "Indicator", values_to = "Value") %>%
    pivot_longer(cols = c("Gender", "District","Age", "Economic"), names_to = "Sub", values_to = "Subgroup") %>%
    select(!Sub) %>%
    group_by(Subgroup) %>%
    reframe(
      "GDR score" = round(mean(gdr, na.rm = TRUE), digits = 2),
      "NCD-Protect" = round(mean(ncdp, na.rm = TRUE), digits = 2),
      "NCD-Risk" = round(mean(ncdr, na.rm = TRUE), digits = 2),
      "All-5" = round(mean(all5 == 1, na.rm = TRUE)*100, digits = 2),
      "At least one vegetable" = round(mean(all5a == 1, na.rm = TRUE)*100, digits = 2),
      "At least one fruit" = round(mean(all5b == 1, na.rm = TRUE)*100, digits = 2),
      "At least one pulse, nut, or seed" = round(mean(all5c == 1, na.rm = TRUE)*100, digits = 2),
      "At least one animal-source food" = round(mean(all5d == 1, na.rm = TRUE)*100, digits = 2),
      "At least one starchy staple food" = round(mean(all5e == 1, na.rm = TRUE)*100, digits = 2),
      "MDD-W" = round(mean(mddw == 1, na.rm = TRUE)*100, digits = 2),
      "Dietary diversity score" = round(mean(fgds, na.rm = TRUE), digits = 2),
    )
  
  datAll <- dataComplete %>% 
    group_by(Country) %>%
    reframe(
      "GDR score" = round(mean(gdr, na.rm = TRUE), digits = 2),
      "NCD-Protect" = round(mean(ncdp, na.rm = TRUE), digits = 2),
      "NCD-Risk" = round(mean(ncdr, na.rm = TRUE), digits = 2),
      "All-5" = round(mean(all5 == 1, na.rm = TRUE)*100, digits = 2),
      "At least one vegetable" = round(mean(all5a == 1, na.rm = TRUE)*100, digits = 2),
      "At least one fruit" = round(mean(all5b == 1, na.rm = TRUE)*100, digits = 2),
      "At least one pulse, nut, or seed" = round(mean(all5c == 1, na.rm = TRUE)*100, digits = 2),
      "At least one animal-source food" = round(mean(all5d == 1, na.rm = TRUE)*100, digits = 2),
      "At least one starchy staple food" = round(mean(all5e == 1, na.rm = TRUE)*100, digits = 2),
      "MDD-W" = round(mean(mddw == 1, na.rm = TRUE)*100, digits = 2),
      "Dietary diversity score" = round(mean(fgds, na.rm = TRUE), digits = 2)) %>%
    rename(Subgroup = Country) %>%
    mutate(Subgroup = "All") 
  
  dqqResults <- bind_rows(datAll, dat) %>%
    t  %>% 
    as.data.frame %>% 
    row_to_names(row_number = 1) %>%
    rownames_to_column(var = "Indicator")
  
  
  write_csv(dqqResults, "C:/Users/PJNat/Dropbox/Projects/IITA_Consult/Nasarawa_results/dqqnasarawa_summaryresults.csv")
}


dqqWebCalculator2(qc_data)







##Retry for Week .....this works for weekly trend
#qc_data_wk<- qc_data %>%
  #mutate(week = week(ymd(Date)))


library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)

dqqWebCalculatorWeekly <- function(data) {
  
  # Helper function for calculating food group values
  foodGroup2 <- function(data) {
    data <- data %>% 
      rename(
        CaseID = "Respondent_ID", 
        Gender = "Gender", 
        District = "LGA_Code", 
        Age = "Age",
        Economic = "Economic.Category",
        DQQ1 = "X01_Foods_made_from_grains", 
        DQQ2 = "X02_Whole_grains", 
        DQQ3 = "X03_White_roots_or_tubers", 
        DQQ4 = "X04_Pulses", 
        DQQ5 = "X05_Vitamin_A.rich_orange_vegetables", 
        DQQ6 = "X06_Dark_green_leafy_vegetables", 
        DQQ7 = "X07_Other_vegetables",  
        DQQ8 = "X08_Vitamin_A.rich_fruits", 
        DQQ9 = "X09_Citrus", 
        DQQ10 = "X10_Other_fruits", 
        DQQ11 = "X11_Baked_or_grain.based_sweets", 
        DQQ12 = "X12_Other_sweets", 
        DQQ13 = "X13_Eggs", 
        DQQ14 = "X14_Cheese", 
        DQQ15 = "X15_Yogurt", 
        DQQ16 = "X16_Processed_meats", 
        DQQ17 = "X17_Unprocessed_red_meat_ruminant", 
        DQQ18 = "X18_Unprocessed_red_meat_non_ruminant", 
        DQQ19 = "X19_Poultry", 
        DQQ20 = "X20_Fish_or_seafood", 
        DQQ21 = "X21_Nuts_or_seeds", 
        DQQ22 = "X22_Packaged_ultra.processed_salty_snacks", 
        DQQ23 = "X23_Instant_noodles", 
        DQQ24 = "X24_Deep_fried_foods", 
        DQQ25 = "X25_Milk", 
        DQQ26 = "X26_Sweet.tea_coffee_or_cocoa", 
        DQQ27 = "X27_Fruit_juice_or_fruit_drinks", 
        DQQ28 = "X28_Soft_drinks", 
        DQQ29 = "X29_Fast_food" 
      ) %>%
      mutate( Week = floor_date(as.Date(Date), unit = "week"),
        ncdp = ((rowSums(pick("DQQ2") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ8") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ9") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ10") == 1, na.rm=TRUE) > 0)),
        
        ncdr = ((rowSums(pick("DQQ28") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ11") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ12") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ17","DQQ18") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ24") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ23", "DQQ29") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ22") == 1, na.rm=TRUE) > 0)),
        
        gdr = (ncdp - ncdr + 9), 
        
        all5a = ifelse((rowSums(pick("DQQ5", "DQQ6", "DQQ7") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5b = ifelse((rowSums(pick("DQQ8","DQQ9", "DQQ10") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5c = ifelse((rowSums(pick("DQQ4","DQQ21") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5d = ifelse((rowSums(pick("DQQ13", "DQQ14", "DQQ15", "DQQ16", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5e = ifelse((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5 = ifelse((all5a + all5b + all5c + all5d + all5e) == 5, 1, 0),
        
        fgds = ((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ14","DQQ15", "DQQ25") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ13") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5","DQQ8") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ9","DQQ10") == 1, na.rm=TRUE) > 0)),
        
        mddw = ifelse((fgds >= 5 & Gender == 1 &  Age >= 15 & Age <= 49), 1, 
                      ifelse((fgds < 5 & Gender == 1 &  Age >= 15 & Age <= 49), 0, NA)),
        zvegfr = ifelse((rowSums(pick("DQQ5","DQQ6", "DQQ7", "DQQ8", "DQQ9", "DQQ10") == 1, 
                                 na.rm=TRUE) > 0) == TRUE, 0, 1),
        vegfr = ifelse((rowSums(pick("DQQ5","DQQ6", "DQQ7", "DQQ8", "DQQ9", "DQQ10") == 1, 
                                na.rm=TRUE) > 0) == TRUE, 1, 0),
        safd = ifelse((rowSums(pick("DQQ22","DQQ23","DQQ24") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        swtfd = ifelse((rowSums(pick("DQQ11", "DQQ12") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0)
        
    )
    return(data)
  }
    
  
  # Prepare complete data with food group calculations
  dataComplete <- foodGroup2(data)
  
  # Subgroup values for "All"
  weeklyResults <- dataComplete %>%
    group_by(Week) %>%
    summarize(
      `GDR score` = round(mean(gdr, na.rm = TRUE), 2),
      `NCD-Protect` = round(mean(ncdp, na.rm = TRUE), 2),
      `NCD-Risk` = round(mean(ncdr, na.rm = TRUE), 2),
      `All-5` = round(mean(all5 == 1, na.rm = TRUE) * 100, 2),
      `At least one vegetable` = round(mean(all5a == 1, na.rm = TRUE) * 100, 2),
      `At least one fruit` = round(mean(all5b == 1, na.rm = TRUE) * 100, 2),
      `At least one pulse, nut, or seed` = round(mean(all5c == 1, na.rm = TRUE) * 100, 2),
      `At least one animal-source food` = round(mean(all5d == 1, na.rm = TRUE) * 100, 2),
      `At least one starchy staple food` = round(mean(all5e == 1, na.rm = TRUE) * 100, 2),
      `MDD-W` = round(mean(mddw == 1, na.rm = TRUE) * 100, 2),
      `Dietary diversity score` = round(mean(fgds, na.rm = TRUE), 2)
    ) %>%
    pivot_longer(cols = -Week, names_to = "Indicator", values_to = "Value")
  
  # Save weekly results
  write_csv(weeklyResults, "weekly_results.csv")
  
  # Plot indicators over time
  ggplot(weeklyResults, aes(x = Week, y = Value, color = Indicator)) +
    geom_line(size = 1) +
    facet_wrap(~Indicator, scales = "free_y") +
    labs(title = "Indicator Trends Over Time", x = "Week", y = "Value") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Call the function with your dataset
dqqWebCalculatorWeekly(qc_data)



##New weekly by day of the week - i.e. Monday vs Thursday
dqqWebCalculatorWeekly <- function(data) {
  
  # Helper function for calculating food group values
  foodGroup2 <- function(data) {
    data <- data %>% 
      rename(
        CaseID = "Respondent_ID", 
        Gender = "Gender", 
        District = "LGA_Code", 
        Age = "Age",
        Economic = "Economic.Category",
        DQQ1 = "X01_Foods_made_from_grains", 
        DQQ2 = "X02_Whole_grains", 
        DQQ3 = "X03_White_roots_or_tubers", 
        DQQ4 = "X04_Pulses", 
        DQQ5 = "X05_Vitamin_A.rich_orange_vegetables", 
        DQQ6 = "X06_Dark_green_leafy_vegetables", 
        DQQ7 = "X07_Other_vegetables",  
        DQQ8 = "X08_Vitamin_A.rich_fruits", 
        DQQ9 = "X09_Citrus", 
        DQQ10 = "X10_Other_fruits", 
        DQQ11 = "X11_Baked_or_grain.based_sweets", 
        DQQ12 = "X12_Other_sweets", 
        DQQ13 = "X13_Eggs", 
        DQQ14 = "X14_Cheese", 
        DQQ15 = "X15_Yogurt", 
        DQQ16 = "X16_Processed_meats", 
        DQQ17 = "X17_Unprocessed_red_meat_ruminant", 
        DQQ18 = "X18_Unprocessed_red_meat_non_ruminant", 
        DQQ19 = "X19_Poultry", 
        DQQ20 = "X20_Fish_or_seafood", 
        DQQ21 = "X21_Nuts_or_seeds", 
        DQQ22 = "X22_Packaged_ultra.processed_salty_snacks", 
        DQQ23 = "X23_Instant_noodles", 
        DQQ24 = "X24_Deep_fried_foods", 
        DQQ25 = "X25_Milk", 
        DQQ26 = "X26_Sweet.tea_coffee_or_cocoa", 
        DQQ27 = "X27_Fruit_juice_or_fruit_drinks", 
        DQQ28 = "X28_Soft_drinks", 
        DQQ29 = "X29_Fast_food" 
      ) %>%
      mutate(
        Week = floor_date(as.Date(Date), unit = "week"),
        Gender = as.character(Gender), # Ensure Gender is consistently a character
        ncdp = ((rowSums(pick("DQQ2") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ8") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ9") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ10") == 1, na.rm=TRUE) > 0)),
        
        ncdr = ((rowSums(pick("DQQ28") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ11") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ12") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ17","DQQ18") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ24") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ23", "DQQ29") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ22") == 1, na.rm=TRUE) > 0)),
        
        gdr = (ncdp - ncdr + 9), 
        
        all5a = ifelse((rowSums(pick("DQQ5", "DQQ6", "DQQ7") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5b = ifelse((rowSums(pick("DQQ8","DQQ9", "DQQ10") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5c = ifelse((rowSums(pick("DQQ4","DQQ21") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5d = ifelse((rowSums(pick("DQQ13", "DQQ14", "DQQ15", "DQQ16", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5e = ifelse((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5 = ifelse((all5a + all5b + all5c + all5d + all5e) == 5, 1, 0),
        
        fgds = ((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ14","DQQ15", "DQQ25") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ13") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5","DQQ8") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ9","DQQ10") == 1, na.rm=TRUE) > 0)),
        mddw = ifelse((fgds >= 5 & Gender == 1 &  Age >= 15 & Age <= 49), 1, 
                      ifelse((fgds < 5 & Gender == 1 &  Age >= 15 & Age <= 49), 0, NA))
      )
    return(data)
  }
  
  # Prepare complete data with food group calculations
  dataComplete <- foodGroup2(data)
  
  write_csv(dataComplete, "DQQ_individual_data.csv")
  
  # Subgroup values by Day of the week
  weeklyByweekday <- dataComplete %>%
    group_by(Week, dayname) %>%
    summarize(
      `GDR score` = round(mean(gdr, na.rm = TRUE), 2),
      `NCD-Protect` = round(mean(ncdp, na.rm = TRUE), 2),
      `NCD-Risk` = round(mean(ncdr, na.rm = TRUE), 2),
      `All-5` = round(mean(all5 == 1, na.rm = TRUE) * 100, 2),
      `MDD-W` = round(mean(mddw == 1, na.rm = TRUE) * 100, 2),
      `Dietary diversity score` = round(mean(fgds, na.rm = TRUE), 2)
    ) %>%
    pivot_longer(cols = -c(Week, dayname), names_to = "Indicator", values_to = "Value")
  
  
  # Combine gender-specific and overall data
  combinedResults <- weeklyByweekday
  
  # Save combined results
  write_csv(combinedResults, "weekly_results_with_dayofweek.csv")
  
  # Plot indicators over time
  
  ggplot(combinedResults, aes(x = Week, y = Value)) +
    geom_line(aes(color = dayname, linetype = dayname), size = 1) +
    facet_wrap(~Indicator, scales = "free_y") +
    scale_linetype_manual(
      values = c("Monday" = "dashed", "Thursday" = "dashed"),
      labels = c("Monday" = "Monday", "Monday" = "Monday")
    ) +
    scale_color_manual(
      values = c("Monday" = "blue", "Thursday" = "brown"),
      labels = c("Monday" = "Monday", "Thursday" = "Thursday")
    ) +
    labs(
      title = "Indicator Trends Over Time by Day of the Week and Overall",
      subtitle = "Comparison of dietary indicators for day before Monday, Thursday, and Overall trends",
      x = "Week",
      y = "Value",
      color = "Day Reported",
      linetype = "Day Reported"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(size = 0.5, color = "gray80"),
      panel.grid.minor = element_blank()
    )
  
  
}



# Call the function with your dataset
dqqWebCalculatorWeekly(qc_mon_thur)







##New weekly by Gender code - This works for Gender differentiated trend
dqqWebCalculatorWeekly <- function(data) {
  
  # Helper function for calculating food group values
  foodGroup2 <- function(data) {
    data <- data %>% 
      rename(
        CaseID = "Respondent_ID", 
        Gender = "Gender", 
        District = "LGA_Code", 
        Age = "Age",
        Economic = "Economic.Category",
        DQQ1 = "X01_Foods_made_from_grains", 
        DQQ2 = "X02_Whole_grains", 
        DQQ3 = "X03_White_roots_or_tubers", 
        DQQ4 = "X04_Pulses", 
        DQQ5 = "X05_Vitamin_A.rich_orange_vegetables", 
        DQQ6 = "X06_Dark_green_leafy_vegetables", 
        DQQ7 = "X07_Other_vegetables",  
        DQQ8 = "X08_Vitamin_A.rich_fruits", 
        DQQ9 = "X09_Citrus", 
        DQQ10 = "X10_Other_fruits", 
        DQQ11 = "X11_Baked_or_grain.based_sweets", 
        DQQ12 = "X12_Other_sweets", 
        DQQ13 = "X13_Eggs", 
        DQQ14 = "X14_Cheese", 
        DQQ15 = "X15_Yogurt", 
        DQQ16 = "X16_Processed_meats", 
        DQQ17 = "X17_Unprocessed_red_meat_ruminant", 
        DQQ18 = "X18_Unprocessed_red_meat_non_ruminant", 
        DQQ19 = "X19_Poultry", 
        DQQ20 = "X20_Fish_or_seafood", 
        DQQ21 = "X21_Nuts_or_seeds", 
        DQQ22 = "X22_Packaged_ultra.processed_salty_snacks", 
        DQQ23 = "X23_Instant_noodles", 
        DQQ24 = "X24_Deep_fried_foods", 
        DQQ25 = "X25_Milk", 
        DQQ26 = "X26_Sweet.tea_coffee_or_cocoa", 
        DQQ27 = "X27_Fruit_juice_or_fruit_drinks", 
        DQQ28 = "X28_Soft_drinks", 
        DQQ29 = "X29_Fast_food" 
      ) %>%
      mutate(
        Week = floor_date(as.Date(Date), unit = "week"),
        Gender = as.character(Gender), # Ensure Gender is consistently a character
        ncdp = ((rowSums(pick("DQQ2") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ8") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ9") == 1, na.rm=TRUE) > 0)+
                  (rowSums(pick("DQQ10") == 1, na.rm=TRUE) > 0)),
        
        ncdr = ((rowSums(pick("DQQ28") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ11") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ12") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ17","DQQ18") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ24") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ23", "DQQ29") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ22") == 1, na.rm=TRUE) > 0)),
        
        gdr = (ncdp - ncdr + 9), 
        
        all5a = ifelse((rowSums(pick("DQQ5", "DQQ6", "DQQ7") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5b = ifelse((rowSums(pick("DQQ8","DQQ9", "DQQ10") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5c = ifelse((rowSums(pick("DQQ4","DQQ21") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5d = ifelse((rowSums(pick("DQQ13", "DQQ14", "DQQ15", "DQQ16", "DQQ17", "DQQ18", "DQQ19", "DQQ20", "DQQ25") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5e = ifelse((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) == TRUE, 1, 0),
        all5 = ifelse((all5a + all5b + all5c + all5d + all5e) == 5, 1, 0),
        
        fgds = ((rowSums(pick("DQQ1","DQQ2", "DQQ3") == 1, na.rm=TRUE) > 0) + 
                  (rowSums(pick("DQQ4") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ21") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ14","DQQ15", "DQQ25") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ16","DQQ17", "DQQ18", "DQQ19","DQQ20") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ13") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ6") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ5","DQQ8") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ7") == 1, na.rm=TRUE) > 0) +
                  (rowSums(pick("DQQ9","DQQ10") == 1, na.rm=TRUE) > 0)),
        mddw = ifelse((fgds >= 5 & Gender == 1 &  Age >= 15 & Age <= 49), 1, 
                      ifelse((fgds < 5 & Gender == 1 &  Age >= 15 & Age <= 49), 0, NA))
              )
    return(data)
  }
  
  # Prepare complete data with food group calculations
  dataComplete <- foodGroup2(data)
  
  write_csv(dataComplete, "DQQ_individual_data.csv")
  
  # Subgroup values by Gender
  weeklyByGender <- dataComplete %>%
    group_by(Week, Gender) %>%
    summarize(
      `GDR score` = round(mean(gdr, na.rm = TRUE), 2),
      `NCD-Protect` = round(mean(ncdp, na.rm = TRUE), 2),
      `NCD-Risk` = round(mean(ncdr, na.rm = TRUE), 2),
      `All-5` = round(mean(all5 == 1, na.rm = TRUE) * 100, 2),
      `MDD-W` = round(mean(mddw == 1, na.rm = TRUE) * 100, 2),
      `Dietary diversity score` = round(mean(fgds, na.rm = TRUE), 2)
    ) %>%
    pivot_longer(cols = -c(Week, Gender), names_to = "Indicator", values_to = "Value")
  
  # Calculate overall weekly averages
  overallWeekly <- dataComplete %>%
    group_by(Week) %>%
    summarize(
      `GDR score` = round(mean(gdr, na.rm = TRUE), 2),
      `NCD-Protect` = round(mean(ncdp, na.rm = TRUE), 2),
      `NCD-Risk` = round(mean(ncdr, na.rm = TRUE), 2),
      `All-5` = round(mean(all5 == 1, na.rm = TRUE) * 100, 2),
      `MDD-W` = round(mean(mddw == 1, na.rm = TRUE) * 100, 2),
      `Dietary diversity score` = round(mean(fgds, na.rm = TRUE), 2)
    ) %>%
    pivot_longer(cols = -Week, names_to = "Indicator", values_to = "Value") %>%
    mutate(Gender = "Overall") # Add Gender column for consistency
  
  # Combine gender-specific and overall data
  combinedResults <- bind_rows(weeklyByGender, overallWeekly)
  
  # Save combined results
  write_csv(combinedResults, "weekly_results_with_gender.csv")
  
  # Plot indicators over time
  
  ggplot(combinedResults, aes(x = Week, y = Value)) +
    geom_line(aes(color = Gender, linetype = Gender), size = 1) +
    facet_wrap(~Indicator, scales = "free_y") +
    scale_linetype_manual(
      values = c("0" = "dashed", "1" = "dotted", "Overall" = "solid"),
      labels = c("0" = "Male", "1" = "Female", "Overall" = "Overall")
    ) +
    scale_color_manual(
      values = c("0" = "#1f78b4", "1" = "#e31a1c", "Overall" = "#33a02c"),
      labels = c("0" = "Male", "1" = "Female", "Overall" = "Overall")
    ) +
    labs(
      title = "Indicator Trends Over Time by Gender and Overall",
      subtitle = "Comparison of dietary indicators for Males, Females, and Overall trends",
      x = "Week",
      y = "Value",
      color = "Category",
      linetype = "Category"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      panel.grid.major = element_line(size = 0.5, color = "gray80"),
      panel.grid.minor = element_blank()
    )
  
 
}



# Call the function with your dataset
dqqWebCalculatorWeekly(qc_data)
  
dqqWebCalculatorWeekly(qc_monday)
dqqWebCalculatorWeekly(qc_thursday)
  
  






#Scratch
ggplot(combinedResults, aes(x = Week, y = Value, color = Gender, linetype = Gender)) +
  geom_line(size = 1) +
  facet_wrap(~Indicator, scales = "free_y") +
  labs(title = "Indicator Trends Over Time by Gender and Overall", x = "Week", y = "Value") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold")
  )


###Renaming directly from raw dataset....will revert to complete this
Rename "introduction_group/Consent" as "Consent"

Filter for consent= "yes"

Drop columns  

rename(
  CaseID = "Respondent_ID", 
  Gender = "Gender_Female_1_Male_0", 
  District = "Locality_Urban_1_Rural_0", 
  Age = "Age_years",
  Economic = "Economic Category",
  DQQ1 = "01_Foods_made_from_grains", 
  DQQ2 = "02_Whole_grains", 
  DQQ3 = "03_White_roots_or_tubers", 
  DQQ4 = "04_Pulses", 
  DQQ5 = "05_Vitamin_A-rich_orange_vegetables", 
  DQQ6a = "question_group/leafy2", 
  DQQ6b = "question_group/leafy3", 
  DQQ7 = "07_Other_vegetables",  
  DQQ8 = "08_Vitamin_A-rich_fruits", 
  DQQ9 = "09_Citrus", 
  DQQ10a = "question_group/fruit3",
  DQQ10b = "question_group/fruit4",
  DQQ11 = "11_Baked_or_grain-based_sweets", 
  DQQ12 = "12_Other_sweets", 
  DQQ13 = "13_Eggs", 
  DQQ14 = "14_Cheese", 
  DQQ15 = "15_Yogurt", 
  DQQ16 = "16_Processed_meats", 
  DQQ17 = "17_Unprocessed_red_meat_ruminant", 
  DQQ18 = "18_Unprocessed_red_meat_non-ruminant", 
  DQQ19 = "19_Poultry", 
  DQQ20 = "20_Fish_or_seafood", 
  DQQ21 = "21_Nuts_or_seeds", 
  DQQ22 = "22_Packaged_ultra-processed_salty_snacks", 
  DQQ23 = "23_Instant_noodles", 
  DQQ24 = "24_Deep_fried_foods", 
  DQQ25 = "25_Milk", 
  DQQ26 = "26_Sweet tea_coffee_or_cocoa", 
  DQQ27 = "27_Fruit_juice_or_fruit_drinks", 
  DQQ28 = "28_Soft_drinks", 
  DQQ29 = "29_Fast_food" 
) 
