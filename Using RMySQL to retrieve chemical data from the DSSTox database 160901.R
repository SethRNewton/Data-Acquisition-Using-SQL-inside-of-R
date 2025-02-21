##RMySQL connecting to dsstox db 9/1/2016

## In this R script, I will connect to a remote database from EPA called dsstox 
## to retrieve data on a list of chemicals I am studying.

## Note that this study has been published in the journal Environmental Pollution here:
##  https://www.sciencedirect.com/science/article/abs/pii/S026974911732691X

## I will do this inside of R to avoid the need to install desktop software to interact with the sql database
## I use two packages to accomplish this - RMySQL and DBConnect


# Install packages (if necessary) and load them
install.packages("RMySQL")
library(RMySQL)

install.packages("dbConnect") #Update - dbConnect does not work for R version 4
library(dbConnect)

## Establish a connection to the database
## Note - my password is not provided here for obvious reasons and was instead stored in a variable called my_password
mydb = dbConnect(MySQL(), user=username, password= my_password, dbname='stg_chemprop', host='au.epa.gov')


## Lets learn about the database structure
dbGetInfo(mydb)         ##General info on db connection
dbListTables(mydb)      ##Tables contained in the Database
dbListFields(mydb, "chemical_lists")    ##Lists the columns in the table "chemical_lists"
dbListFields(mydb, "compound_relationships")
dbListFields(mydb, "compounds")
dbListFields(mydb, "generic_substance_compounds")


## Notes on RMySQL commands:
##dbReadTable() can be used to get the whole table.  Probably shouldn't use it unless you know how big these tables are

##dbGetQuery() can be used to extract specific rows from a table
##e.g. dbGetQuery(mydb, "SELECT * FROM chemical_lists LIMIT 5;")


##dbSendQuery() submits the query but will not extract any data
##dbFetch() will fetch data from the query that was sent
##The previous two work together to submit a query and subsequently retrieve data but you can check the submitted query before fetching it using the following:


##dbGetInfo() function returns information about query that has been submitted for execution using dbSendQuery().  You would assign a variable name to dbSendQuery and then call that variable in dbGetInfo.  e.g. physiology.data <- dbSendQuery(mydb, "SELECT * FROM parameter_values;")  dbGetInfo(physiology.data)
##dbGetStatement() function returns query that has been submitted for execution using dbSendQuery()
##dbGetRowCount() function returns the number of rows fetched from the database by the dbFetch()
##dbGetRowsAffected() function returns the number of rows affected
##dbColumnInfo() function returns information about the columns of the table for which query has been submitted
##dbClearResult() function frees all the resources associated with the result set of the dbSendQuery() function


## I have a molecular formula and I want to retrieve the names of all chemicals that correspond to that formula

TestComp_GSC <- dbGetQuery(mydb, "
SELECT c.dsstox_substance_id, c.preferred_name, c.casrn, e.mol_formula, count(distinct f.id) 
FROM stg_dsstox.compounds e
LEFT JOIN generic_substance_compounds d ON e.id = d.fk_compound_id
LEFT JOIN generic_substances c ON d.fk_generic_substance_id = c.id
LEFT JOIN source_generic_substance_mappings b ON c.id = b.fk_generic_substance_id
LEFT JOIN source_substances a on b.fk_source_substance_id = a.id
LEFT JOIN chemical_lists f ON a.fk_chemical_list_id = f.id
        WHERE e.mol_formula = 'C10H12ClNO3' and e.fragment_count>=1 and e.radical_count=0
GROUP BY c.dsstox_substance_id, c.preferred_name, c.casrn, e.mol_formula;
                        ")

## Now I have a list of all chemicals that correspond to the molecular formulas I had

## What I really need to do is get predictions of a chemical property called "log P"
## There are several models that have been used to make prediction of log P for all chemicals in the database
## However, some these prediction are stored in a different database called stg_chemprop 
##  and some are stored in the original stg_dsstox so I will have to make a new connection for each property


## I will use the CASRN # (or CAS for short - a chemical identifyer unique to each chemical)
##  to get other identifiers used internally in these databases


setwd("\\\\Aa.ad.epa.gov/ord/RTP/Users/R-Z/snewto03/Net MyDocuments/Sethn/NTS Drinking Water (Brita Project)/Dashboard Query/PChem props")

##Get compound_ids from Casnums

Casnums <- read.csv("CASnums_for_logP_160922.csv", stringsAsFactors = F, header= F)

## The database was giving me trouble inputing a list to search on so I will make a loop that generates a new query with one CAS at a time

mydb = dbConnect(MySQL(), user=username, password=my_password, dbname='stg_dsstox', host='au.epa.gov')

FirstString <- "SELECT e.id FROM generic_substances c LEFT JOIN generic_substance_compounds d ON c.id = d.fk_generic_substance_id LEFT JOIN Compounds e ON d.fk_compound_id = e.id WHERE c.casrn = '"

LastString <- "';"

fk_compound_id <- numeric()

for (i in 1:nrow(Casnums)){
  Cas <- Casnums[i,1]
  Query <- paste(FirstString, Cas, LastString, sep="")
  Compound_ID <- dbGetQuery(mydb, Query)
  fk_compound_id[i] <- Compound_ID[1,1]
  
}


LogP_training_set_160922 <- cbind(Casnums, fk_compound_id)         



##Get NCCT log P

mydb = dbConnect(MySQL(), user=username, password=my_password, dbname='stg_chemprop', host='au.epa.gov')        


FirstString <- "SELECT result_value FROM qsar_predicted_properties WHERE efk_qsar_model_id = 22 AND efk_dsstox_compound_id = "
LastString <- ";"        


NCCT_LogP <- character()

for (i in 1:nrow(Casnums)){
  if (!is.na(fk_compound_id[i])){
    Query <- paste(FirstString, fk_compound_id[i], LastString, sep="")
    Value <- dbGetQuery(mydb, Query)
    NCCT_LogP[i] <- Value[1,1]
  } else NCCT_LogP[i] <- "NA"
}

LogP_training_set_160922 <- cbind(LogP_training_set_160922, NCCT_LogP)




##Get Episuite log P

FirstString <- "SELECT result_value FROM qsar_predicted_properties WHERE efk_qsar_model_id = 35 AND efk_dsstox_compound_id = "
LastString <- ";"        


Episuite_LogP <- character()

for (i in 1:nrow(Casnums)){
  if (!is.na(fk_compound_id[i])){
    Query <- paste(FirstString, fk_compound_id[i], LastString, sep="")
    Value <- dbGetQuery(mydb, Query)
    Episuite_LogP[i] <- Value[1,1]
  } else Episuite_LogP[i] <- "NA"
}

LogP_training_set_160922 <- cbind(LogP_training_set_160922, Episuite_LogP)

## I now have pulled predicted log P values for all our chemicals
## I will save them to csv to use and share

write.csv(LogP_training_set_160922, file = "LogP_training_set_160922.csv")

dbDisconnect(mydb)





## Getting Log P values for a second project
## I have another project that requires predicted log P values
## Furthermore, I am told there are more log P models so I will explore the databases to find them

library(RMySQL)

library(dbConnect)




setwd("\\\\Aa.ad.epa.gov/ord/RTP/Users/R-Z/snewto03/Net MyDocuments/Sethn/NTS Drinking Water (Brita Project)/Dashboard Query/PChem props")

Chemicals <- read.csv("Clean RTs for Kamel Repulled logPs 161108.csv", stringsAsFactors = F)


##Get compound_ids from Casnums
mydb = dbConnect(MySQL(), user=username, password=my_password, dbname='stg_dsstox', host='au.epa.gov')

dbListTables(mydb)      ##Tables contained in the Database
dbListFields(mydb, "compounds")


FirstString <- "SELECT c.casrn, e.id, e.acd_iupac_name FROM generic_substances c LEFT JOIN generic_substance_compounds d ON c.id = d.fk_generic_substance_id LEFT JOIN Compounds e ON d.fk_compound_id = e.id WHERE c.casrn = '"

LastString <- "';"


for (i in 2:nrow(Chemicals)){
  Cas <- Chemicals[i,2]
  Query <- paste(FirstString, Cas, LastString, sep="")
  NewID <- dbGetQuery(mydb, Query)
  IDs <- rbind(IDs, NewID)
  
}



##Get NCCT log P

mydb = dbConnect(MySQL(), user=username, password=my_password, dbname='stg_chemprop', host='au.epa.gov')        


FirstString <- "SELECT result_value FROM qsar_predicted_properties WHERE efk_qsar_model_id = 22 AND efk_dsstox_compound_id = "
LastString <- ";"        


NCCT_LogP <- character()
fk_compound_id <- IDs$id

for (i in 1:nrow(IDs)){
  if (!is.na(fk_compound_id[i])){
    Query <- paste(FirstString, fk_compound_id[i], LastString, sep="")
    Value <- dbGetQuery(mydb, Query)
    NCCT_LogP[i] <- Value[1,1]
  } else NCCT_LogP[i] <- "NA"
}

IDs <- cbind(IDs, NCCT_LogP)


##Get Episuite log P

FirstString <- "SELECT result_value FROM qsar_predicted_properties WHERE efk_qsar_model_id = 35 AND efk_dsstox_compound_id = "
LastString <- ";"        


Episuite_LogP <- character()

for (i in 1:nrow(IDs)){
  if (!is.na(fk_compound_id[i])){
    Query <- paste(FirstString, fk_compound_id[i], LastString, sep="")
    Value <- dbGetQuery(mydb, Query)
    Episuite_LogP[i] <- Value[1,1]
  } else Episuite_LogP[i] <- "NA"
}

IDs <- cbind(IDs, Episuite_LogP)



##Check to see if there are more log P models in the qsar database

mydb = dbConnect(MySQL(), user=username, password=my_password, dbname='stg_qsar', host='au.epa.gov')

dbListTables(mydb)      ##Tables contained in the Database
dbListFields(mydb, "models")

models <- dbGetQuery(mydb, "SELECT * FROM models")


## There are two more log P models - NICEATM_LogP which is #114 and ACD_LogP #125



##Get NICEATM_LogP log P

mydb = dbConnect(MySQL(), user=username, password=my_password, dbname='stg_chemprop', host='au.epa.gov') 

FirstString <- "SELECT result_value FROM qsar_predicted_properties WHERE efk_qsar_model_id = 114 AND efk_dsstox_compound_id = "
LastString <- ";"        


NICEATM_LogP <- character()

for (i in 1:nrow(IDs)){
  if (!is.na(fk_compound_id[i])){
    Query <- paste(FirstString, fk_compound_id[i], LastString, sep="")
    Value <- dbGetQuery(mydb, Query)
    NICEATM_LogP[i] <- Value[1,1]
  } else NICEATM_LogP[i] <- "NA"
}

IDs <- cbind(IDs, NICEATM_LogP)


##Get ACD_LogP log P


FirstString <- "SELECT result_value FROM qsar_predicted_properties WHERE efk_qsar_model_id = 125 AND efk_dsstox_compound_id = "
LastString <- ";"        


ACD_LogP <- character()

for (i in 1:nrow(IDs)){
  if (!is.na(fk_compound_id[i])){
    Query <- paste(FirstString, fk_compound_id[i], LastString, sep="")
    Value <- dbGetQuery(mydb, Query)
    ACD_LogP[i] <- Value[1,1]
  } else ACD_LogP[i] <- "NA"
}

IDs <- cbind(IDs, ACD_LogP)

## I now have five different predicted log P values for my chemicals
## I will now save them in and RData file if I want to use them in R later and as a csv for use outside of R 

save(IDs, file = "Repulled Log Ps 4 models 161108.RData")
write.csv(IDs, file = "Repulled Log Ps 4 models 161108.csv")




