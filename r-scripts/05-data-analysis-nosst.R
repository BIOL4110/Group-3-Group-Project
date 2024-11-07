## 05-data-analysis-nosst
# Nov 6. 2024
# IN, 

df <- read_csv("data-processed/full_harmonized_btgt.csv")
colnames(df) 

## will make some quick graphs using this dataset - no model fitting yet ------
df %>%
  ggplot(aes(x = year, y = abundance))+
  geom_point(color = "pink") +
  facet_wrap(~ th_genus_species)
## only full data for clupea harengus it looks like - temperate only?
## this df only have temperate not both...

## then we could maybe join them together even if they dont add up 

## NEXT TUESDAY HAVE CODE READY 

head(harmonized_biotime_processed)
## use stuarts code to measure beta diversity - changes in Beta diversity compared to SST
# comapre species richness/abundnace to year/ sst / compare between diff families 
# glm to  understand relationship between species abundance and year 


head(harmonized_globtherm_processed) ## double check cdfiltyer out phylum == chordata
#

head(df)

unique(harmonized_globtherm_processed$th_genus_species)
#