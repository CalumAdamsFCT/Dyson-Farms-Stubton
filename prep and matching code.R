library(dplyr)
library(purrr)
library(readr)
library(stringr)

## prep for matching process
fertiliser_list<- read.csv("fertiliser_list.csv")
field_analysis<- read.csv("field_analysis.csv")
field_list<- read.csv("field_list.csv")
yields<- read.csv("yields.csv")
ecids<- read.csv("inputs matching list.csv")

## first - fertilisers
#units
unit_count<- fertiliser_list %>% 
  count(Units, sort = TRUE)

item_count<- fertiliser_list %>% 
  count(Product.Name, sort = TRUE)

## change kg to t
fertiliser_list<- fertiliser_list %>% 
  mutate(
    Quantity = if_else(
      Units=="kg", Quantity/1000, Quantity
    ),
    Units=if_else(
      Units=="kg","t", Units
    )
  )

## change m3 to l
fertiliser_list<-fertiliser_list %>% 
  mutate(
    Quantity= if_else(
      Units=="m3", Quantity*1000, Quantity
    ),
    Units= if_else(
      Units=="m3", "l",Units
    )
  )

## change all units to lower case
fertiliser_list$Units<- tolower(fertiliser_list$Units)


## match to ecid
fertiliser_list<- fertiliser_list %>% 
  mutate(
    ecid= case_when(
      str_detect(Product.Name, regex("Pig Manure", ignore_case= TRUE))~ "crop_00077", # assumed autumn application
      str_detect(Product.Name, regex("Nuram N35s", ignore_case= TRUE))~ "inpu_00606",
      str_detect(Product.Name, regex("Kalfos", ignore_case= TRUE))~ "inpu_00023",
      str_detect(Product.Name, regex("DAP 18.46.0", ignore_case= TRUE))~ "inpu_00233", # DAP - assumed European origin
      str_detect(Product.Name, regex("Folex B", ignore_case= TRUE))~ "inpu_06857",
      str_detect(Product.Name, regex("Yara N25:14.3so3", ignore_case= TRUE))~ "inpu_00665",
      str_detect(Product.Name, regex("YaraVita Gramitrel", ignore_case= TRUE))~ "inpu_06859", # Omex Folex Mg 13 - the closest nutrient composition 
      str_detect(Product.Name, regex("Nufol", ignore_case= TRUE))~ "inpu_06929"
  )
  )

## need to delete multiples of each fertiliser as there is a row for each nutrient within each fertiliser being used
fertiliser_list <- fertiliser_list %>%
  filter(Fertiliser.Nutrient.Name %in% c("N", "CaO", "B", "Cu"))

## keep only neccessary columns
fertiliser_list_complete<- fertiliser_list %>% 
  select(Product.Name, Quantity, Units, ecid)

fertiliser_list_complete<- fertiliser_list_complete %>% 
  mutate(section="inputs")


## crops ##
field_count<- yields %>% 
  count(Field.Name, sort = TRUE)

field_list<- field_list %>% 
  rename(Field.Name = Field.Reference)

field_list<- field_list %>% 
  rename(Crop.Type = Crop.Group)

## adding field area to yields
yields<- yields %>% 
  left_join(
    field_list %>% select(Field.Name, Working.Area.ha),
    by= "Field.Name"
  )

yields <- yields %>%
  left_join(
    field_list %>% select(Field.Name, Crop.Type, Working.Area.ha),
    by = c("Field.Name", "Crop.Type")
  )

yields<- yields %>% 
  mutate(
    Working.Area.ha= case_when(
      Field.Name == "Airfield"~ 22.44,
      Field.Name == "Balloon Field"~ 18.84,
      Field.Name == "Barrier Field"~ 9.36,
      Field.Name == "Bottom/Top Simons"~ 18.84,
      Field.Name == "Brandon Field"~ 13.7,
      Field.Name == "Bridge Field Stub"~ 29.9,
      Field.Name == "Concrete Field"~ 4.81,
      Field.Name == "Conifer Field"~ 21.42,
      Field.Name == "England Field"~ 30.92,
      Field.Name == "Gokart Field"~ 4.08,
      Field.Name == "Gorse Field"~ 16.36,
      Field.Name == "Grahams Field"~ 2.53,
      Field.Name == "Green Walk"~ 4.22,
      Field.Name == "Lane Field"~ 12.06,
      Field.Name == "Lillies Meadow"~ 6.72,
      Field.Name == "Little Gates"~ 41.1,
      Field.Name == "Long Field (Stubton)"~ 30.88,
      Field.Name == "Middle Field"~ 22.02,
      Field.Name == "Modder"~ 13.42,
      Field.Name == "North 20ac"~ 7.26,
      Field.Name == "North Field"~ 30.92,
      Field.Name == "Popular Field"~ 6.45,
      Field.Name == "Rectory"~ 17.33,
      Field.Name == "Rookery"~ 13.34,
      Field.Name == "South Yard Field"~ 14.78,
      Field.Name == "Stack Field"~ 3.74,
      Field.Name == "The Park"~ 16.45,
      Field.Name == "Village Field"~ 9.44,
      Field.Name == "Warren"~ 13.27,
      Field.Name == "Wedding Field"~ 5.9,
      Field.Name == "Widnes"~ 27.5,
      Field.Name == "Wood Field"~ 10.75,
     TRUE ~ Working.Area.ha
    )
  )

## need to calculate total.yield
yields<- yields %>% 
  mutate(
    total.yield= Actual.t.ha * Working.Area.ha
  )

crop_count<- yields %>% 
  count(Crop.Type, sort = TRUE)

## residue info added
yields<- yields %>% 
  mutate(
    residues= case_when(
      Crop.Type == "Sugar Beet"~ "Incorporated/None",
      Crop.Type == "Winter Rape"~ "Incorporated/None",
      Crop.Type == "Maize Forage"~ "Removed",
      Crop.Type == "Winter Forage Rye"~ "Removed",
      Crop.Type == "Spring Wheat" & Field.Name =="Brandon Field|Bridge Field Stub"~ "Incorporated/None",
      Crop.Type == "Spring Wheat" & Field.Name =="Warren" ~ "Removed",
      Crop.Type == "Group 2" ~ "Incorporated/None",
      Crop.Type == "Forage Oats and Vetch" ~ "Removed",
      Crop.Type == "Group 1" ~ "Incorporated/None"
    )
  )

## manual match ecids
yields<- yields %>% 
  mutate(
    ecid= case_when(
      str_detect(Crop.Type, regex("Forage Oats and Vetch", ignore_case= TRUE))~ "crop_00110",
      str_detect(Crop.Type, regex("Group 1|Group 2", ignore_case= TRUE))~ "crop_00151",
      str_detect(Crop.Type, regex("Maize Forage", ignore_case= TRUE))~ "crop_00140",
      str_detect(Field.Name, regex("Warren", ignore_case= TRUE))~ "crop_00112",
      str_detect(Field.Name, regex("Brandon Field|Bridge Field Stub", ignore_case= TRUE))~ "crop_00151",
      str_detect(Crop.Type, regex("Sugar Beet", ignore_case= TRUE))~ "crop_00013",
      str_detect(Crop.Type, regex("Winter Forage Rye", ignore_case= TRUE))~ "crop_00113",
      str_detect(Crop.Type, regex("Winter Rape", ignore_case= TRUE))~ "crop_00004"
    )
  )

## add section
yields<- yields %>% 
  mutate(section="crops")
yields_complete <- yields
yields_complete<- yields_complete %>% 
  select(Crop.Type, total.yield, Working.Area.ha, ecid,section)
## sprays ##
pesticides<- field_analysis %>% 
  filter(Heading.Type=="Pesticides")
not_pesticides<- field_analysis %>% 
  filter(Heading.Type!="Pesticides")
heading_count<- not_pesticides %>% 
  count(Heading.Type, sort = TRUE)

sprays <- field_analysis %>% 
  filter(Heading.Type %in% c("Pesticides","Nutrition"))

fert_count<- sprays %>% 
  filter(Heading=="Fertiliser") %>% 
  count(Name, sort = TRUE)

sprays<- sprays %>% 
  filter(Heading!="Fertiliser")

## match sprays
sprays<- sprays %>% 
  rename(Name = Product.Name)
sprays<- sprays %>% 
  rename(units = Units)
sprays<- sprays %>% 
  mutate(units=tolower(units))
## remove MAPP number from name column
sprays<- sprays %>% 
  mutate(
    Name= str_trim(str_remove(Name,"\\s*\\([^\\)]*\\)"))
  )

## ROUND 1- add ecid column to each dataset based upon exact matches of name as well as having the same units

sprays<- sprays %>% 
  left_join(
    ecids %>% select(Name,units,ecid),
    by = c("Name", "units")
  )

unmatchedsprays<- sprays %>% 
  filter(is.na(ecid))

spray_count<- unmatchedsprays %>% 
  count(Name, sort = TRUE)

## ROUND 2 - assign generic sprays

sprays<- sprays %>% 
  mutate(
    ecid = case_when(
      is.na(ecid) & Heading == "Adjuvants" & units == "l" ~ "inpu_00638", # adjuvant not listed
      is.na(ecid) & Heading == "Fungicides" & units == "l" ~ "inpu_00625", # liquid fungicides not listed
      is.na(ecid) & Heading == "Fungicides" & units == "kg" ~ "inpu_00626", # solid fungicides not listed
      is.na(ecid) & Heading == "Herbicides" & units == "l" ~ "inpu_00629", # liquid herbicide not listed
      is.na(ecid) & Heading == "Herbicides" & units == "kg" ~ "inpu_00628", # solid herbicide not listed
      is.na(ecid) & Heading == "Insecticides" & units == "l" ~ "inpu_00630", # liquid insecticide not listed
      is.na(ecid) & Heading == "Insecticides" & units == "kg" ~ "inpu_00631", # solid insecticide not listed
      is.na(ecid) & Heading == "Molluscicides" & units == "kg" ~ "inpu_00632", # solid molluscicide not listed
      TRUE ~ ecid
    )
  )


## ROUND 3 - manual matching

sprays<- sprays %>% 
  mutate(
    ecid= case_when(
      str_detect(Name, regex("Omex Folex Magnesium", ignore_case= TRUE))~ "inpu_06861",
      str_detect(Name, regex("Mn Super|Crop Rooter Plus|Yara Crop Boost|YaraVita Brassitrel Pro|Nutrel K-MAN Plus|Yara Maize Boost", ignore_case= TRUE))~ "inpu_00664", # liquid fert no N
      str_detect(Name, regex("Jubilee SX", ignore_case= TRUE))~ "inpu_00628",
      str_detect(Name, regex("Stomp Aqua", ignore_case= TRUE))~ "inpu_03479",
      str_detect(Name, regex("Vimoy + Cello", ignore_case= TRUE))~ "inpu_05678",
      str_detect(Name, regex("Shiro", ignore_case= TRUE))~ "inpu_00423",
      str_detect(Name, regex("Cameo SX", ignore_case= TRUE))~ "inpu_01223",
      TRUE ~ ecid
    )
  )

#Stomp aqua g to kg and Vimoy and Cello from ha dose to L Shiro g to kg cameo sx g to kg
sprays<- sprays %>% 
  mutate(
    Quantity= if_else(
      units=="g", Quantity/1000, Quantity
    ),
    units=if_else(
      units=="g","kg",units
    ),
    units=if_else(
      units=="ha dose","l",units
    )
  )
unit_count<- sprays %>% 
  count(units, sort = TRUE)


## create sprays_complete
sprays_complete<- sprays %>% 
  filter(!is.na(ecid)& ecid!= "")

sprays_complete<- sprays_complete %>% 
  mutate(section="inputs")

sprays_complete<- sprays_complete %>% 
  select(Name, Quantity, units, ecid, section)

## operations ## 
headings<- operations %>% 
  count(Heading.Group, sort = TRUE)

operations<- field_analysis %>% 
  filter(Heading.Group!= "Trace Elements") %>% 
  filter(Heading.Group!="Seed / Plants") %>% 
  filter(Heading.Group!= "Other Variable Costs") %>% 
  filter(Heading.Group!= "Herbicides") %>% 
  filter(Heading.Group!= "Fungicides") %>% 
  filter(Heading.Group!= "Adjuvants") %>% 
  filter(Heading.Group!= "Growth Regulators") %>% 
  filter(Heading.Group!= "Insecticides") %>% 
  filter(Heading.Group!= "Molluscicides") %>% 
  filter(Heading.Group!= "Organic Manure") %>% 
  filter(Heading.Group!= "Fertiliser")
  
operations_list<- operations %>% 
  count(Product.Name, sort = TRUE)

unit_count<- operations %>% 
  count(Units,sort = TRUE)

## manually add ecids
operations<- operations %>% 
  mutate(
    ecid= case_when(
      str_detect(Product.Name, regex("Spray|Weed wiping", ignore_case= TRUE))~ "fuel_000139",
      str_detect(Product.Name, regex("Pellets Applied", ignore_case= TRUE))~ "fuel_000138", # assumes solid fert distribution
      str_detect(Product.Name, regex("Claas Lexion 780", ignore_case= TRUE))~ "fuel_000144", # combining
      str_detect(Product.Name, regex("Roll|Rolling", ignore_case= TRUE))~ "fuel_000124",
      str_detect(Product.Name, regex("Trailed Muck Spreader", ignore_case= TRUE))~ "fuel_000140",
      str_detect(Product.Name, regex("Drill Horsch Avatar 12m|Drill Vaderstad Tempo 9m|Drill Vaderstad Tempo 9m + Grange toolbar", ignore_case= TRUE))~ "fuel_000131", # assumes direct drill
      str_detect(Product.Name, regex("Krone Forager/carting/clamping|Vervaet Beet Harvester & carting", ignore_case= TRUE))~ "fuel_000145", # assumes forage harvester
      str_detect(Product.Name, regex("Vaderstad Top Down|Drill Combi|Sumo Quattro", ignore_case= TRUE))~ "fuel_000130", # assumes cultivator drill
      str_detect(Product.Name, regex("Dal-Bo Seedbed harrow", ignore_case= TRUE))~ "fuel_000126", # assumes stuble cultivations light
      str_detect(Product.Name, regex("Fert Spread|Fert Spreading", ignore_case= TRUE))~ "fuel_000138",
      str_detect(Product.Name, regex("Inter-row Hoe|Vaderstad NZ", ignore_case= TRUE))~ "fuel_000125", # assumes spring tine
    )
  )

na_count<- operations %>% 
  count(is.na(ecid))

## prep dataset for loading onto JSON
operations<- operations %>% 
  filter(Units=="ha")

operations_complete<- operations %>% 
  mutate(section= "fuels")

## save csvs 
write.csv(fertiliser_list_complete, "fertilisers.csv")
write.csv(sprays_complete, "sprays.csv")
write.csv(yields_complete,"yields.csv")
write.csv(operations_complete,"operations.csv")
