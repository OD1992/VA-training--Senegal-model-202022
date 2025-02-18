
####################Define year to be added in the suffix of saved objects################################
year<-c(2020:2022)
##########################################################################################################
kk=2

# Step 1: Read in new case data ======
coordinates <- read_csv("data-raw/Org Unit - Corrected Coordinates2.csv") %>% 
  dplyr::select(adm1 = `Org Unit Name Level 2`, adm2 = `Org Unit Name Level 3`,
                hf = `Org Unit Short Name`, lat = Latitude, long = Longitude) %>% 
  mutate(adm1 = snakecase::to_snake_case(adm1),
         adm2 = snakecase::to_snake_case(adm2),
         hf = snakecase::to_snake_case(hf),
         unique_id = paste("sen",row_number(), sep="_"))

data_2020 <- readxl::read_xlsx(paste0(dirpath, "data-raw/Extraction Data Paludisme DS FS DHIS2 2020_2021_2022.xlsx"), sheet = 1)
data_2021 <- readxl::read_xlsx(paste0(dirpath, "data-raw/Extraction Data Paludisme DS FS DHIS2 2020_2021_2022.xlsx"), sheet = 2)
data_2022 <- readxl::read_xlsx(paste0(dirpath, "data-raw/Extraction Data Paludisme DS FS DHIS2 2020_2021_2022.xlsx"), sheet = 3)


data_2020_2022 <- bind_rows(data_2020, data_2021, data_2022) %>% 
  janitor::clean_names() %>% 
  separate(mois, into = c('monthname', 'year'), sep = " ") %>% 
  mutate(month = factor(monthname, levels = c("Janvier","Février","Mars","Avril","Mai","Juin",
                                              "Juillet","Août","Septembre","Octobre","Novembre","Décembre")),
         month = as.numeric(month),
         year = as.numeric(year)) %>% 
  mutate(adm1 = snakecase::to_snake_case(regions),
         adm2 = snakecase::to_snake_case(districts),
         hf = snakecase::to_snake_case(formation_sanitaire),
         conf_u5 = cas_confirmes_feminin_0_4_ans + cas_confirmes_masculin_0_4_ans,
         conf_ov5 = cas_confirmes_feminin_5_ans_excluant_les_femmes_enceintes + cas_confirmes_masculin_5_ans_excluant_les_femmes_enceintes,
         pres_all = NA,
         pres_u5 = NA,
         pres_ov5 = NA,
         pres_preg = NA) %>% 
  dplyr::select(adm1 = adm1, adm2 = adm2, hf, month, year , 
                allout_all = total_consultants_toutes_affections_confondues,
                allout_ov5 = consultants_toutes_affections_confondues_5_ans_excluant_les_femmes_enceintes,
                allout_preg = consultants_toutes_affections_confondues_femme_enceinte,
                allout_u5 = consultants_toutes_affections_confondues_0_4_ans,
                conf_all = total_cas_confirmes, 
                conf_ov5,
                conf_preg = cas_confirmes_femme_enceinte,
                conf_u5,
                pres_all, pres_ov5, pres_preg, pres_u5,
                test_all = total_tests_tdr_realises,
                test_ov5 = tests_tdr_realises_5_ans_excluant_les_femmes_enceintes,
                test_preg = tests_tdr_realises_femme_enceinte,
                test_u5 = tests_tdr_realises_0_4_ans,
                conf_f_all = total_cas_confirmes_feminin,
                conf_f_ov5 = cas_confirmes_feminin_5_ans_excluant_les_femmes_enceintes,
                conf_f_u5 = cas_confirmes_feminin_0_4_ans,
                conf_m_all = total_cas_confirmes_masculin,
                conf_m_ov5 = cas_confirmes_masculin_5_ans_excluant_les_femmes_enceintes,
                conf_m_u5 = cas_confirmes_masculin_0_4_ans,
                susp_all = total_cas_suspect,
                susp_ov5 = cas_suspect_5_ans_excluant_les_femmes_enceintes,
                susp_preg = cas_suspect_femme_enceinte,
                susp_u5 = cas_suspect_0_4_ans) %>% 
  left_join(coordinates)

unique_points <- data_2020_2022 %>% 
  distinct(adm1, adm2, unique_id, hf, lat, long) %>% 
  filter(!is.na(lat)&!is.na(unique_id)) %>% 
  #mutate(id = paste("sen",row_number(), sep="_")) %>% 
  dplyr::select(unique_id = unique_id, hf, x = long, y = lat) 

write.csv(data_2020_2022, "data-cleaned/data_cleaned.csv")
