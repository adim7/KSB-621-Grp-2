library(tidyverse)
library(lubridate)
#Getting the data files loaded into R
dudes_data_paths <- fs::dir_info("../KSB_621/Data/")

paths_chr <- dudes_data_paths %>%
  pull(path)

dudes_data <- paths_chr %>%
  map(read_csv) %>%
  set_names(paths_chr)


#Assign appropriate names to tibbles
All_join <- dudes_data[1] %>%
  map_df(~ .)
ChargeTypesandClassifications <- dudes_data[2] %>%
  map_df(~ .)
EventWorkOrders <- dudes_data[3] %>%
  map_df(~ .)
Events_all <- dudes_data[4] %>%
  map_df(~ .)
InvoiceHeader <- dudes_data[5] %>%
  map_df(~ .)
InvoiceLineItems_all <- dudes_data[6] %>%
  map_df(~ .)
InvoicePayments <- dudes_data[7] %>%
  map_df(~ .)
RoomCapacityData <- dudes_data[8] %>%
  map_df(~ .)

#Cleaning and preprocessing Events data
Events_all[Events_all == "-"] <- NA # converting all non-standard characters ("-") to NAs

clean_events_df <- Events_all %>%
  mutate(AcctNum = as.character(AcctNum),
         EventStartDate = mdy(EventStartDate), #convert to standard date format from chr
         EventStartMonth = factor(EventStartMonth, levels = c("Jan", "Feb", "Mar", "Apr", 
                                                              "May", "Jun", "Jul", "Aug", 
                                                              "Sep", "Oct", "Nov", "Dec")), #convert to factor variable and set order of months
         EventStartDayofWeek = factor(EventStartDayofWeek, 
                                      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), #convert to factor variable and set order of week days
         EventStartTimeSetup = parse_date_time(EventStartTimeSetup, '%I:%M %p'), #convert to standard time format from chr
         EventStopDate = mdy(EventStopDate), #convert to standard date format from chr
         EventEndDayofWeek =  factor(EventEndDayofWeek, 
                                     levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), #convert to factor variable
         EventStopTimeBreakdown = parse_date_time(EventStopTimeBreakdown, '%I:%M %p')) %>% #convert to standard time format from chr
  separate(EventStartTimeSetup, into = c("date", "EventStartTimeSetup"), sep = " ") %>% #separating time from default date from function
  separate(EventStopTimeBreakdown, into = c("date", "EventStopTimeBreakdown"), sep = " ") #separating time from default date from function

# Drop variables with over 60% missing values
events_all <- clean_events_df %>%
  select(-AlternateFSScheduleID)

################################################################################################

#Cleaning and preprocessing Event Work Order data

clean_workorders_df <- EventWorkOrders %>%
  mutate(AcctNum = as.character(AcctNum),
         FSScheduleID = as.character(FSScheduleID),
         EventWOID = as.character(EventWOID),
         EventWOHours = ifelse(EventWOHours == "-", NA, EventWOHours),
         EventWOHours = as.numeric(EventWOHours),
         DateCompleted = ymd(DateCompleted),
         WOStatus = ifelse(WOStatus == "-", NA, WOStatus),
         WOStatus = factor(WOStatus),
         WOCraft = ifelse(WOCraft == "-", NA, WOCraft),
         WOCraft = factor(WOCraft),
         Priority = factor(Priority),
         DateEstStart = ymd(DateEstStart),
         DateEstComp = ymd(DateEstComp),
         EstimatedHours = as.numeric(EstimatedHours),
         EstimatedCosts = as.numeric(EstimatedCosts))

# Drop variables with over 60% missing values
eventworkorders <- clean_workorders_df %>%
  select(-DateEstStart)


################################################################################################

#Cleaning and preprocessing Invoice Header data

InvoiceHeader[InvoiceHeader == "-"] <- NA

clean_invoiceheader_df <- InvoiceHeader %>%
  mutate(AcctNum = as.character(AcctNum),
         InvoiceDate = mdy(InvoiceDate),
         InvoiceDueDate = mdy(InvoiceDueDate),
         InvoiceCreateDate = mdy(InvoiceCreateDate),
         InvoiceAmount = as.numeric(InvoiceAmount),
         InvoiceLineItems = as.integer(InvoiceLineItems),
         InvoiceStatus = factor(InvoiceStatus))

# Drop variables with over 60% missing values
invoiceheader <- clean_invoiceheader_df %>%
  select(-InvoiceNOTdue, -InvoiceOverdue)


################################################################################################

#Cleaning and preprocessing Invoice Line Items data

InvoiceLineItems_all[InvoiceLineItems_all == "-"] <- NA

clean_invoicelineitems_df <- InvoiceLineItems_all %>%
  mutate(LineItemDate = mdy_hm(LineItemDate))

# Drop variables with over 60% missing values
invoicelineitems_all <- clean_invoicelineitems_df %>%
  select(-LastUpdateByName)


################################################################################################

#Cleaning and preprocessing Invoice Payments data

InvoicePayments[InvoicePayments == "-"] <- NA

clean_invoicepayments_df <- InvoicePayments %>%
  mutate(AcctNum = as.character(AcctNum),
         PaymentType = factor(PaymentType),
         TrnsTypeID = factor(TrnsTypeID),
         PaymentDate = mdy(PaymentDate),
         PaymentDeposit = factor(PaymentDeposit))

# Drop variables with over 60% missing values
invoicepayments <- clean_invoicepayments_df %>%
  select(-PaymentDesc)


################################################################################################

#Cleaning and preprocessing Charge Type & Classification data

clean_chargetypesandclassifications_df <- ChargeTypesandClassifications %>%
  mutate(ChargeType = factor(ChargeType),
         ChargeClass = factor(ChargeClass))

chargetypesandclassifications <- clean_chargetypesandclassifications_df


################################################################################################

#load room capacity data

roomcapacity <- RoomCapacityData

################################################################################################


# Joining all the preprocessed datasets

All_join <- invoicelineitems_all %>% group_by(AcctInvID) %>% mutate(ID_2 = row_number()) %>%
  left_join(invoiceheader %>% group_by(AcctInvID) %>% mutate(ID_2 = row_number()),
            by = c("AcctInvID", "ID_2")) %>% group_by(AcctInvID) %>% mutate(ID_3 = row_number()) %>%
  left_join(invoicepayments %>% group_by(AcctInvID) %>% mutate(ID_3 = row_number()),
            by = c("AcctInvID", "ID_3")) %>% group_by(AcctEventID) %>% mutate(ID_4 = row_number()) %>%
  left_join(events_all %>% group_by(AcctEventID) %>% mutate(ID_4 = row_number()),
            by = c("AcctEventID", "ID_4")) %>% group_by(FSScheduleID) %>% mutate(ID_5 = row_number()) %>%
  left_join(eventworkorders %>% unite(FSScheduleID, c("AcctNum", "FSScheduleID"), sep = "|") %>%
              group_by(FSScheduleID) %>% mutate(ID_5 = row_number()),
            by = c("FSScheduleID", "ID_5")) %>% group_by(AcctChargeID) %>%
  left_join(chargetypesandclassifications  %>% mutate(ID_6 = row_number()),
            by = c("AcctChargeID")) %>%
  left_join(roomcapacity,
            by = c("RoomID")) %>%
  ungroup() %>%
  separate(AcctEventID, into = c("AcctNum", "EventID")) %>%
  select(-ID_2, -AcctNum.x, -ID_3, -AcctNum.y, -ID_4, -ID_5, -ID_6, -date) %>%
  mutate(InvoiceAmount = ifelse(is.na(InvoiceAmount), 0.00, InvoiceAmount),
         InvoiceLineItems = ifelse(is.na(InvoiceLineItems), 0, InvoiceLineItems),
         InvoiceStatus = ifelse(is.na(InvoiceStatus), "", InvoiceStatus),
         PaymentAmount = ifelse(is.na(PaymentAmount), 0.00, PaymentAmount),
         EstimatedHours = ifelse(is.na(EstimatedHours), 0.00, EstimatedHours),
         EstimatedCosts = ifelse(is.na(EstimatedCosts), 0, EstimatedCosts),
         EventHours = ifelse(is.na(EventHours), 0, EventHours),
         EventWOHours = ifelse(is.na(EventWOHours), 0, EventWOHours)) 

#All_join %>%
  #write_csv("~/Documents/R Projects 2020/Dude Solutions/Dude Solutions Project/KSB_621/Data/All_join.csv")

# Pre processing data for model building
# Response to Business Question 2 - How much should a client charge to rent out a specific room?

# Considerations

# Determine key variables for building predictive model - ID variables included just for audit

rent_spec_df <- All_join %>%
  select(AcctNum, EventID, EventDesc, LineItemDesc,  LineItemPriceEach, InvoiceDate, PaymentDate, EventStartDate, 
         EventStartMonth, EventStartDayofWeek, EventTime, EventStopDate, EventEndDayofWeek,
         EventHours, StopHour, HoursWeekend, AreaDesc, EventWOCostsPriority, ChargeType, ChargeClass, RoomCapacity)

# Take a look at the data filtering for all observations with Room Capacity Data
# Only a little over 5% of the All_join data has RoomCapacity data

rent_spec_df %>%
  filter(!is.na(RoomCapacity))

# Review filtering options to help address missing values in Room Description
# If we filter based on the specified columns, we can then review the missing values in room description 
# in relation to the preceding and succeeding rows, as well as accounts and/or event id and infer the room type. 
# A number of observations appear to be for "storage room". WE may need to filter out storage room using line Item Desc

# Select specific variables  for modeling, including AcctNum and EventID for ID purposes
rent_spec_df <- All_join %>%
  select(AcctNum, EventID, LineItemPriceEach, InvoiceAmount, PaymentAmount,
         EventTitle, EventStartDate, EventStartMonth, EventStartDayofWeek, EventTime,
         EventHours, StopHour, HoursWeekend, RoomDescription, EventWOCosts, EventWOHours, RoomCapacity, ChargeClass) %>%
  filter(year(EventStartDate) == 2019) # focus on 2019 data for now


# check proportion of missing values
missing_values <- sort(colMeans(is.na(rent_spec_df)), decreasing = T)

x <- names(missing_values)
y <- tibble(missing_values)

z <- bind_cols(x, y) %>%
  rename(Variable = ...1, `Missing %` = missing_values) %>%
  kable() %>%


# preprocess model data model data

model_df19 <- read_csv("Data/model_df19.csv")


prep_model_df <- model_df19 %>%
  select(-AcctNum, -EventID, -EventTitle, -ChargeClass, -RoomDescription) %>%
  mutate(across(where(is.character) & !c(HoursWeekend), as.factor), # convert chr vars to factors
         across(where(is.Date) & !c(EventStartDate), month), # extract the month value in dates except event start date
         StartDay = day(EventStartDate), # Extract day number from event start date
         #StartYr = year(EventStartDate), # Extract year from event start date
         HoursWeekend = str_replace(HoursWeekend, ":", "."),
         HoursWeekend = as.double(HoursWeekend),
         EventStartDate <- NULL)


# Attempted imputation for RoomCapacity

library(mice)    
set.seed(123)
pmd_imp <- prep_model_df  %>%
  mice(method = "cart", m = 5, seed = 123) %>%
  complete(); summary(pmd_imp)  

pmd_imp %>%
  write_csv("/Data/pmd_imp.csv")

# Update to imputed data to prepare for cluster Analysis because you can only use numeric data

pmd_imp_updated <- pmd_imp %>%
  mutate(EventCategory = recode_factor(Event_Category, 
                                       "Religious" = 1,
                                       "Sports/Fitness" = 2,
                                       "Social/Meetup" = 3,
                                       "Miscellaneous" = 4,
                                       "Performaning/Arts" = 5,
                                       "Academic/Professional" = 6,
                                       "AfterSchool" = 7),
         EventDayofWk = recode_factor(EventStartDayofWeek,
                                      "Mon" = 1,
                                      "Tue" = 2,
                                      "Wed" = 3,
                                      "Thu" = 4,
                                      "Fri" = 5,
                                      "Sat" = 6,
                                      "Sun" = 7, .ordered = TRUE),
         EventMonth = month(EventStartDate)) %>%
  select(-Event_Category, -EventTime, -EventStartDayofWeek, -EventStartMonth, -EventStartDate) %>%
  mutate(across(everything(), as.numeric))
