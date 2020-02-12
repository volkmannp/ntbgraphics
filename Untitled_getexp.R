getexpdata <- function(directory, 
                       analysis = c("4arm", "2arm_tg", "2arm_ko", "2arm_sd"),
                       ordercolumns = c("dontcare", "ntb", "rdoc")) {
  
  # import data
  meta.data <-  readxl::read_excel(paste0(directory,"/Meta Behavior.xlsx"))
  animal.list <-  readxl::read_excel(paste0(directory, "/Animal List.xlsx"))
  
  data.animal.joined <-  animal.list %>%
    # exclude NAs in Genotype
    filter(Genotype!= 'NA') %>%
    # merge conditions in case of 4-arm
    `if`(analysis == "4arm", unite(., col="GT_Env", Genotype, Environmental, sep= "_", remove = FALSE), .) %>%
    # join animals and behavior data
    left_join(meta.data, by = c("RFID" = "Animal")) %>%
    # change order of columns
>>>>>>    ??#`if`(analysis == "4arm", select(., RFID, GT_Env, Meanspeed, Timeimmobile, SerialLearn),.) %>% 
    # change values from chr to num
    mutate_at(vars(Meanspeed:SerialLearn),list(as.numeric)) %>%
    # select relevant columns
    `if`(analysis == "4arm", select(., RFID, GT_Env, Meanspeed:SerialLearn),.) %>%
    `if`(analysis == "2arm_tg", select(., RFID, Genotype, Meanspeed:SerialLearn),.) %>%
    `if`(analysis == "2arm_ko", select(., RFID, Genotype, Meanspeed:SerialLearn),.) %>% 
    `if`(analysis == "2arm_sd", select(., RFID, Environmental, Meanspeed:SerialLearn),.)
  
  #return amended dataframe
  return(data.animal.joined)
}

getexpdata(directory = paste0(system.file("extdata/", package = "ntbgraphics", mustWork = T),"/"), 
           analysis = "4arm")
