rm(list = setdiff(ls(), c("yields_complete","sprays_complete","operations_complete","fertiliser_list_complete")))
library(jsonlite)
library(readr)
## first get all datasets with the same headings for merging
fertiliser_list_complete<- fertiliser_list_complete %>% 
  rename(name= Product.Name) %>% 
  rename(units=Units)
operations_complete<- operations_complete %>% 
  select(Product.Name,Quantity, Units,ecid,section) %>% 
  rename(name=Product.Name) %>% 
  rename(units=Units)
sprays_complete<- sprays_complete %>% 
  rename(name=Name)
yields_complete<- yields_complete %>% 
  rename(name= Crop.Type) %>% 
  mutate(units="t") %>% 
  rename(Quantity= total.yield) %>% 
  rename(Area= Working.Area.ha) %>% 
  mutate(Area.units="ha")
yields_complete<-yields_complete %>% 
  select(name,Quantity,units,Area,Area.units,ecid,section)


## merge datasets
emissions_data<- bind_rows(
  fertiliser_list_complete,
  operations_complete,
  sprays_complete,
  yields_complete
)

## slim dataset so 1 row per unique ecid
condensed_data<- emissions_data %>% 
  group_by(ecid) %>% 
  summarise(
    Quantity= sum(Quantity, na.rm = TRUE),
    name=first(name),
    units= first(units),
    Area= sum(Area, na.rm = TRUE),
    Area.units=first(Area.units),
    section=first(section),
    .groups = "drop"
  )


## build template for insersion 
## turn the dataset into emissions sections
row_to_json <- function(ecid, Quantity, units) {
  
  # choose section depending on ecid prefix
  section_val <- if (startsWith(ecid, "inpu_")) {
    "inputs"
  } else if (startsWith(ecid, "crop_")) {
    "crops"
  } else if (startsWith(ecid,"fuel_")) {
    "fuels"
  } else {
    ""
  }
  
  list(
    uuid = "",  
    section = section_val,
    emissions_category = list(
      name = "",
      id = ecid
    ),
    user_input = list(
      quantity = list(
        value = Quantity,
        description = "",
        units = units
      )
    ),
    calculator_output = list(
      positive = list(value = NA, units = "tCO2e"),
      negative = list(value = 0, units = "tCO2e"),
      scope_1 = list(value = 0, units = "tCO2e"),
      scope_1_CO2 = list(value = 0, units = "tCO2e"),
      scope_1_biogenic_CO2 = list(value = 0, units = "tCO2e"),
      scope_1_mechanical_CO2 = list(value = 0, units = "tCO2e"),
      scope_1_CH4 = list(value = 0, units = "tCO2e"),
      scope_1_mechanical_CH4 = list(value = 0, units = "tCO2e"),
      scope_1_otherbiogenic_CH4 = list(value = 0, units = "tCO2e"),
      scope_1_enteric_CH4 = list(value = 0, units = "tCO2e"),
      scope_1_manure_CH4 = list(value = 0, units = "tCO2e"),
      scope_1_N2O = list(value = 0, units = "tCO2e"),
      scope_1_direct_N2O = list(value = 0, units = "tCO2e"),
      scope_1_indirect_volatilisation_N2O = list(value = 0, units = "tCO2e"),
      scope_1_indirect_leaching_N2O = list(value = 0, units = "tCO2e"),
      scope_1_mechanical_N2O = list(value = 0, units = "tCO2e"),
      scope_1_otherGHG = list(value = 0, units = "tCO2e"),
      scope_2 = list(value = 0, units = "tCO2e"),
      scope_2_CO2 = list(value = 0, units = "tCO2e"),
      scope_2_CH4 = list(value = 0, units = "tCO2e"),
      scope_2_N2O = list(value = 0, units = "tCO2e"),
      scope_2_otherGHG = list(value = 0, units = "tCO2e"),
      scope_3 = list(value = 0, units = "tCO2e"),
      scope_3_CO2 = list(value = 0, units = "tCO2e"),
      scope_3_CH4 = list(value = 0, units = "tCO2e"),
      scope_3_N2O = list(value = 0, units = "tCO2e"),
      scope_3_otherGHG = list(value = 0, units = "tCO2e"),
      scope_4 = list(value = 0, units = "tCO2e"),
      kg_manure = list(value = 0, units = "tCO2e"),
      kg_manure_N = list(value = 0, units = "tCO2e")
    )
  )
}

## insert for fuels and inputs (will have to change the function to deal with crops as it has two input fields)

fuelinputs_emissions_list<- condensed_data %>% 
  filter(section %in% c("inputs","fuels")) %>% 
  select(ecid,Quantity,units) %>% 
  pmap(row_to_json)

## now generating crops template
row_to_json_crops <- function(ecid, Quantity, units,Area,Area.units) {
  
  # choose section depending on ecid prefix
  section_val <- if (startsWith(ecid, "inpu_")) {
    "inputs"
  } else if (startsWith(ecid, "crop_")) {
    "crops"
  } else if (startsWith(ecid,"fuel_")) {
    "fuels"
  } else {
    ""
  }
  
  list(
    uuid = "",  
    section = section_val,
    emissions_category = list(
      name = "",
      id = ecid
    ),
    user_input = list(
      quantity = list(
        value = Quantity,
        description = "Tonnes harvested",
        units = units
      ),
      area = list(
        value= Area,
        description = "Area under Cultivation (ha)",
        units = Area.units
      )
    ),
    calculator_output = list(
      positive = list(value = NA, units = "tCO2e"),
      negative = list(value = 0, units = "tCO2e"),
      scope_1 = list(value = 0, units = "tCO2e"),
      scope_1_CO2 = list(value = 0, units = "tCO2e"),
      scope_1_biogenic_CO2 = list(value = 0, units = "tCO2e"),
      scope_1_mechanical_CO2 = list(value = 0, units = "tCO2e"),
      scope_1_CH4 = list(value = 0, units = "tCO2e"),
      scope_1_mechanical_CH4 = list(value = 0, units = "tCO2e"),
      scope_1_otherbiogenic_CH4 = list(value = 0, units = "tCO2e"),
      scope_1_enteric_CH4 = list(value = 0, units = "tCO2e"),
      scope_1_manure_CH4 = list(value = 0, units = "tCO2e"),
      scope_1_N2O = list(value = 0, units = "tCO2e"),
      scope_1_direct_N2O = list(value = 0, units = "tCO2e"),
      scope_1_indirect_volatilisation_N2O = list(value = 0, units = "tCO2e"),
      scope_1_indirect_leaching_N2O = list(value = 0, units = "tCO2e"),
      scope_1_mechanical_N2O = list(value = 0, units = "tCO2e"),
      scope_1_otherGHG = list(value = 0, units = "tCO2e"),
      scope_2 = list(value = 0, units = "tCO2e"),
      scope_2_CO2 = list(value = 0, units = "tCO2e"),
      scope_2_CH4 = list(value = 0, units = "tCO2e"),
      scope_2_N2O = list(value = 0, units = "tCO2e"),
      scope_2_otherGHG = list(value = 0, units = "tCO2e"),
      scope_3 = list(value = 0, units = "tCO2e"),
      scope_3_CO2 = list(value = 0, units = "tCO2e"),
      scope_3_CH4 = list(value = 0, units = "tCO2e"),
      scope_3_N2O = list(value = 0, units = "tCO2e"),
      scope_3_otherGHG = list(value = 0, units = "tCO2e"),
      scope_4 = list(value = 0, units = "tCO2e"),
      kg_manure = list(value = 0, units = "tCO2e"),
      kg_manure_N = list(value = 0, units = "tCO2e")
    )
  )
}

crops_emissions_list<- condensed_data %>% 
  filter(section=="crops") %>% 
  select(ecid,Quantity,units,Area,Area.units) %>% 
  pmap(row_to_json_crops)


## now merge both of these together
all_emissions_list <- c(crops_emissions_list,fuelinputs_emissions_list)


## load template
template<- fromJSON("template.json", simplifyVector = FALSE)

## load emissions onto template
template[[1]]$emissions<-all_emissions_list

## change report info
name<- "Dyson Farms Stubton"
template[[1]]$title<- name
end_date <- "2024-12-31"
template[[1]]$reporting_period$end<-end_date
start_date <- "2024-01-01"
template[[1]]$reporting_period$start<- start_date

write_json(template, path = "Dyson Farms Stubton report.json",pretty=TRUE, auto_unbox=TRUE)
