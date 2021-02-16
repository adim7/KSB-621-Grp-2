
#Getting the data files loaded
dudes_data_paths <- fs::dir_info("../KSB_621/Data/")

paths_chr <- dudes_data_paths %>%
  pull(path)

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
         EventWOHours = as.numeric(EventWOHours),
         WOStatus = factor(WOStatus),
         WOCraft = factor(WOCraft),
         Priority = factor(Priority))

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
  ungroup() %>%
  separate(AcctEventID, into = c("AcctNum", "EventID")) %>%
  select(-ID_2, -AcctNum.x, -ID_3, -AcctNum.y, -ID_4, -ID_5, -ID_6, -date) %>%
  mutate(InvoiceAmount = ifelse(is.na(InvoiceAmount), 0.00, InvoiceAmount),
         InvoiceLineItems = ifelse(is.na(InvoiceLineItems), 0, InvoiceLineItems),
         InvoiceStatus = ifelse(is.na(InvoiceStatus), "", InvoiceStatus),
         PaymentAmount = ifelse(is.na(PaymentAmount), 0.00, PaymentAmount),
         EstimatedHours = ifelse(is.na(EstimatedHours), 0.00, EstimatedHours),
         EstimatedCosts = ifelse(is.na(EstimatedCosts), 0, EstimatedCosts),
         EventHours = ifelse(is.na(EventHours), 0, EventHours)); All_join %>% head(5)
