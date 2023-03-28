# Functions to make tidy data

read_cassavabase <- function(phenotypeFile) {
  
  possibly_read_csv <- purrr::possibly(read.csv, NA)
  # works if from Download page
  indata <- possibly_read_csv(phenotypeFile,
                            na.strings = c("#VALUE!",NA,".",""," ","-","\""),
                            stringsAsFactors = F)
ifelse(is.na(indata),
       indata <- possibly_read_csv(phenotypeFile,
                                   na.strings = c("#VALUE!",NA,".",""," ","-","\""),
                                   stringsAsFactors = F, skip = skip_col, check.names = F),
       indata)


  cat("\nTrials interested are:\n", unique(indata[, c(6)]))

  return(indata) 
}


# Load the theme used
theme_xiaofei <- function(){ 
  theme_gray() %+replace%
    theme(axis.text.x = element_text(face = "bold", colour = "black", size = 8, angle = 45, hjust = 1),
          axis.text.y = element_text(face = "bold", colour="black", size = 8),
          axis.title.y = element_text(size = 12,face = "bold", angle = 90, vjust = 3) ,
          axis.title.x = element_text(size = 12,face = "bold", vjust = -0.5) ,
          plot.title = element_text(size = 16, face = "bold.italic", hjust = 0.5),
          plot.margin = unit(c(1,1,1,2), "cm") # top, right, bottom, left
          #legend.position = "none"
)}





# change column names into standar names

change_colname = function (sel_data = sel_data ,
                           new_names = NA){
  
  # I have the col name of database and the standardized col names
  new_col_names = 
    read.csv("https://raw.githubusercontent.com/lfdelgadom/standar_col_names_CB/main/standar_col_names.csv") 
  
  # if there is new column, it will tell you what is the new names, then you need give the "new_names"
  # and the re-run the function
  if (sum(!names(sel_data) %in% new_col_names$database_col_name) != 0){
    unique_col = setdiff(names(sel_data), new_col_names$database_col_name)
    print("In the sel_data, there is/are unique column(s):")
    print(unique_col)
    print("Please add the unique column(s) to the standard col names")
    print("File location,
       https://raw.githubusercontent.com/lfdelgadom/standar_col_names_CB/main/standar_col_names.csv")
    if(is.na(new_names)) {print("ERROR: The unique column(s) not changed yet")}
    if(!is.na(new_names)){
      unique_names = data.frame(database_col_name = unique_col,
                                analysis_col_name = new_names)
      new_col_names = rbind(new_col_names, unique_names)
      
      base_sel_colname = data.frame(names(sel_data))
      names(base_sel_colname) = "database_col_name"
      sel_new_colname = base_sel_colname %>%
        left_join(new_col_names, by = c( "database_col_name"))
      
      names(sel_data) = sel_new_colname$analysis_col_name
      print("Good, new columns were added. The column names are standardized names now!")
      
      ####  remove columns without data
      sel_data_kp = sel_data %>%
        select(   where(  ~!all(is.na(.x)) )    )
      
      return(sel_data_kp)
    }
    
    
  }
  # if there is no new col names, it is easy
  if (sum(!names(sel_data) %in% new_col_names$database_col_name) == 0){
    base_sel_colname = data.frame(names(sel_data))
    names(base_sel_colname) = "database_col_name"
    sel_new_colname = base_sel_colname %>%
      left_join(new_col_names, by = c( "database_col_name"))
    
    names(sel_data) = sel_new_colname$analysis_col_name
    print("Good, the column names are standardized names now!")
    
    ####  remove columns without data
    sel_data_kp = sel_data %>%
      select(   where(  ~!all(is.na(.x)) )   )
    
    return(sel_data_kp)
    
  }
}



# are there duplicated rows and columns?
# if so, change or remove

# ---- function
row_col_dup = function (sel_data_kp= sel_data_kp) {
  
  row_col_ct = sel_data_kp %>%
    count(use_trial_name, use_col_number, use_row_number, sort=TRUE) %>%
    filter(n>1) %>%
    arrange(use_row_number, use_col_number)
  
  if (nrow(row_col_ct) >0) {
    print("ERROR: The duplicated row and column combination:")
    print(row_col_ct)
    
    row_col_ct_bind = row_col_ct %>%
      mutate(trial_row_col = paste(use_trial_name, use_col_number, use_row_number, sep = "_"))
    
    duplicated_plot = sel_data_kp %>%
      mutate(trial_row_col = paste(use_trial_name, use_col_number, use_row_number, sep = "_")) %>%
      filter(trial_row_col %in% row_col_ct_bind$trial_row_col) %>%
      select(use_plot_name, use_col_number, use_row_number, use_trial_name, use_plot_number) %>%
      arrange(use_trial_name, use_plot_number, use_row_number, use_col_number )
    
    print("Here are the plot names:")
    print(duplicated_plot)
    print("Please fix the ERROR!")
    return(duplicated_plot)
    
  }
  if (nrow(row_col_ct) == 0) {
    print("Good, there is no duplicated combination of row and column.")
  }
  
}

# check duplicate in plot_number ---------------------------------------


plot_number_dup = function (sel_data_kp = sel_data_kp) {
  
  plot_number_ct = sel_data_kp %>%
    count(plot_number, sort=TRUE) %>%
    filter(n>1) %>%
    arrange(plot_number)
  
  if (nrow(plot_number_ct) > 0) {
    print("ERROR: There are duplicate plot number: ")
    #print(row_col_ct)
    
    duplicated_plot = sel_data_kp %>%
      filter(plot_number %in% plot_number_ct$plot_number) %>%
      select(plot_name, col_number, row_number, plot_number) %>%
      arrange(plot_number, row_number, col_number )
    
    print("Please fix the ERROR!, Here are the plot names:")
    print(duplicated_plot, n = Inf)
    
  }
  if (nrow(plot_number_ct) == 0) {
    print("Good, there is no duplicated plot number.")
  }
  
}

#### function visualize the layout -

trial_layout <- function(trial = sel_data_kp) {
  trial_list <- unique(trial$use_trial_name)
  for (i in 1:length(trial_list)) {
    trial_i <- trial %>%
      filter(use_trial_name %in% trial_list[i])
    myplot <- ggplot(trial_i, aes(x = factor(use_col_number), y = factor(use_row_number), fill = factor(use_rep_number))) +
      geom_tile(color = "black", size = 0.5) + # Black border on tiles
      geom_tile(
        data = trial_i %>% filter(use_check_test == "check"),
        aes(fill = use_check_test), col = "black"
      ) +
      geom_text(data = trial_i %>% filter(use_check_test == "check"),
        aes(label = use_accession_name), size = 2) +
      geom_text(data = trial_i %>% filter(use_check_test == "test"),
                aes(label = use_plot_number), size = 3) +
      labs(x = "col_number", y = "row_number", fill = "rep", title = trial_list[i]) +
      coord_fixed() + # Square tiles 
      theme_xiaofei() 
    
      print(myplot)
  }
}

# function visualize the layout - families

trial_layout_family <- function(trial = sel_data_kp) {
  trial_list <- unique(trial$use_trial_name)
  for (i in 1:length(trial_list)) {
    trial_i <- trial %>%
      filter(use_trial_name %in% trial_list[i])
    myplot <- ggplot(trial_i, aes(x = factor(use_col_number), y = factor(use_row_number))) +
      geom_tile(color = "black", size = 0.5) + # Black border on tiles
      labs(x = "col_number", y = "row_number", title = trial_list[i]) +
      coord_fixed() + # Square tiles
      theme_minimal() + # Minimal theme, no grey background
      geom_tile(data = trial_i %>%
        filter(use_check_test == "check"), fill = "black", show.legend = F) +
      geom_tile(
        data = trial_i,
        aes(fill = use_family_name), col = "black"
      )
    theme(
      panel.grid = element_blank(), # No underlying grid lines
      axis.text.x = element_text( # Vertical text on x axis
        angle = 0, vjust = 0.5, hjust = 0
      )
    )
    print(myplot)
    # layout <<- myplot Save layout in output folder
  }
}

### 2.7 convert accession_name to standard names and add the check_test column

# ---- function
check_clone_name = function(clone_list,
                            new_names = NA,
                            add_check = NULL) {
  
  ## 1). list of released varieties
  released = 
    read.csv("https://raw.githubusercontent.com/lfdelgadom/standar_col_names_CB/main/Cassava%20Varieties%20with%20multiple%20names_2021Jan.csv", 
             na.strings = "", check.names = F)
  cat("Released varieties:")
  print(sort(released$accession_name)) # the correct name format
  
  ## 2). accessions in genebank
  #eGWAS = "D:\\OneDrive - CGIAR\\Data Analysis\\Documento presentation 13 (Cassava dataset)\\(9) Genetic Gain Waxy\\data\\"
  genebank = read.csv("https://raw.githubusercontent.com/lfdelgadom/standar_col_names_CB/main/CIAT_genebank_cassava.csv", 
                      na.strings = "", check.names = F)
  genebank_clone = unique(genebank$`Accession number`)
  
  ## 3. other know clones
  # we can add more clones   -------- flexibility
  known_clone = c("C19", "C243", "C33", "C39", "C413", "TME3", "HB60" , "KU50", "C4", "BB1", "BB2", "BB3", "KM505", "TMEB419")
  cat("The other known clones:")
  print(sort(known_clone))
  
  # all the clones in the file
  accession_out = data.frame("oldName" = unique(clone_list), "newName" = unique(clone_list))
  accession_out$newName = gsub(" ", "", accession_out$newName) # remove space in names
  accession_all = unique(accession_out$newName)  # list of clones after remove space
  
  variety_wrong = accession_all [!str_detect(accession_all, "-") &   # without "-" in name
                                   !accession_all %in% genebank_clone &    # not in genebank
                                   !accession_all %in% known_clone &       # not in known clones
                                   !accession_all %in% released$accession_name] # not in released list
  
  
  
  if(sum(!is.na(new_names)) ==0) {
    
    if(length(variety_wrong)>0){
      print("The clones below did not have the correct name.")
      print("Need give the new name to -- new_names, then re-run the function")
      print(variety_wrong)
    }
    
    if(length(variety_wrong)==0){
      print("Good, the released names were correctly used")
      
      # trial accessions in released collections
      trial_clone_released = released %>%
        filter(accession_name %in% accession_out$newName) %>%
        data.frame()
      
      # change the trial accession into standard names
      if(nrow(trial_clone_released) >=1) {
        for(j in 1:nrow(trial_clone_released)){
          
          accession_out = accession_out %>%
            mutate(newName =
                     ifelse(newName == trial_clone_released[j,1] ,
                            trial_clone_released[j,2], newName) )
        }
      }
      
      if(nrow(trial_clone_released) ==0) {
        accession_out$newName = accession_out$oldName
      }
      
      accession_all_format = unique(accession_out$newName)
      variety_format = accession_all_format [str_detect(accession_all_format, "_is_")]
      print("Now the standard names are used. Here are the released varieties")
      print(variety_format)
      
      # add check_released column
      
      accession_out$use_check_released = NA
      check_list = c(variety_format, add_check)
      # change the check column
      if(length(check_list) >0){
        accession_out = accession_out %>%
          mutate(use_check_released =
                   ifelse(newName %in% check_list ,
                          "check_released", "test") )
      }
      
      if(length(check_list) == 0){
        accession_out$use_check_released = "test"
      }
      
      check_after = accession_out %>%
        filter(use_check_released == "check_released") %>%
        select(newName) %>%
        unique()
      print("The check or released clones are:")
      print(check_after)
      
      names(accession_out) = c("accession_name_ori", "use_accession_name",
                               "use_check_released")
      
      
      return(accession_out)
      
    }
    
    
  }
  
  
  
  if(sum(!is.na(new_names)) >0) {
    
    old_new_name = data.frame(old_name = variety_wrong,   # with both wrong and correct names
                              new_name = new_names)
    
    for(i in 1:nrow(old_new_name)){
      accession_out = accession_out %>%
        mutate(newName =
                 ifelse(newName == old_new_name[i,1] ,
                        old_new_name[i,2], newName) )   # replace with standard format
    }
    
    accession_all_modi = accession_out$newName
    variety_2wrong =  accession_all_modi [!str_detect( accession_all_modi, "-") &   # without "-" in name
                                            ! accession_all_modi %in% genebank_clone &    # not in genebank
                                            ! accession_all_modi %in% known_clone &       # not in known clones
                                            ! accession_all_modi %in% released$accession_name] # not in released list
    
    if(length(variety_2wrong)>0){
      print("The clones below might not have the correct name.")
      print("Please double-check them!")
      print(variety_2wrong)
    }
    
    if(length(variety_2wrong)==0){
      print("Good, the released names were correctly used")
    }
    
    
    # trial accessions in released collections
    trial_clone_released = released %>%
      filter(accession_name %in% accession_out$newName) %>%
      data.frame()
    
    # change the trial accession into standard names
    if(nrow(trial_clone_released) >0) {
      for(j in 1:nrow(trial_clone_released)){
        
        accession_out = accession_out %>%
          mutate(newName =
                   ifelse(newName == trial_clone_released[j,1] ,
                          trial_clone_released[j,2], newName) )
        
      }
    }
    
    if(nrow(trial_clone_released) ==0) {
      accession_out$newName = accession_out$oldName
    }
    
    accession_all_format = unique(accession_out$newName)
    variety_format = accession_all_format [str_detect(accession_all_format, "_is_")]
    print("Now the standard names are used. Here are the released varieties")
    print(variety_format)
    
    # add check_released column
    
    accession_out$use_check_released = NA
    check_list = c(variety_format, add_check)
    # change the check column
    if(length(check_list) >0){
      accession_out = accession_out %>%
        mutate(use_check_released =
                 ifelse(newName %in% check_list ,
                        "check_released", "test") )
    }
    
    if(length(check_list) == 0){
      accession_out$use_check_released = "test"
    }
    
    check_after = accession_out %>%
      filter(use_check_released == "check_released") %>%
      select(newName) %>%
      unique()
    print("The check or released clones are:")
    print(check_after)
    
    names(accession_out) = c("accession_name_ori", "use_accession_name",
                             "use_check_released")
    return(accession_out)
    
    
  }
  
  
  
}


#### function 6.1. check numeric of obs_ traits   ------------------------------ 6.1 6.1 6.1 6.1 6.1


is_numeric = function (trial_data) {
  
  all_trait = names(trial_data)[str_detect(names(trial_data), "obs_")]
  numeric_trait = names(select_if(trial_data[, all_trait], is.numeric))
  
  if(sum(all_trait%in% numeric_trait) == length(all_trait) ) {
    print("Good, all traits are numeric!")
  }
  if (sum(all_trait%in% numeric_trait) != length(all_trait) ) {
    print("The traits are not numeric. Need fix it!")
    print (all_trait [!all_trait%in% numeric_trait])
    print("After fixing the error, please re-run the function, is_numeric")
  }
  
}

### 2.9 add GIS information 

add_GIS <-
  function(trial_data = trial_standard) {
    GIS_info <-
      read.csv("https://raw.githubusercontent.com/lfdelgadom/standar_col_names_CB/main/standardization%20trait%20trial%20location.csv", na.strings = "", check.names = F)

    GIS_info <- GIS_info %>%
      select(
        "location_CassavaBase", "use_latitude", "use_longitude",
        "use_altitude", "use_department", "use_country", "use_ag_zone", "use_location_short"
      ) %>% 
      mutate(use_longitude = as.numeric(use_longitude), 
             use_latitude = as.numeric(use_latitude))

    trial_loc <- unique(trial_data$use_location)
    print("The locations are:")
    print(trial_loc)
    in_database <- trial_loc %in% GIS_info$location_CassavaBase

    if (sum(in_database) != length(in_database)) {
      trial_loc_database <- data.frame(
        trial_loc_list = trial_loc,
        loc_in_CassavaBase = in_database
      )
     print(trial_loc_database)
    }

    if (sum(in_database) == length(in_database)) {
      print("All locations are in the database.")
      trial_data <- left_join(trial_data, GIS_info, by = c("use_location" = "location_CassavaBase"))
    }
    return(trial_data)
  }


### 2.13  visualize the difference among trials in all traits **********************


# ---- function
BOXPLOT_VAR = function (my_dat){
  
  # remove columns with all NA
  my_dat_noNA = my_dat[, colSums(is.na(my_dat)) < nrow(my_dat)]
  trait_wanted = names(my_dat_noNA)[names(my_dat_noNA) %in% trait_list]
  
    # save as PDF, can adjust the figure size
  pdf(paste(folder, "01_", year_interest, trial_interest, "_boxplot_",
            Sys.Date(),".pdf", sep=""), width = 4, height = 6)
  
  for(i in 1: length(trait_wanted)){
    y_DATA = my_dat_noNA[[trait_wanted[i]]]   # data frame or vector?
    x_DATA = my_dat_noNA$trial_name
    my_DATA = my_dat_noNA
    y_LABEL = trait_wanted[i]
    x_LABEL = NULL
    TITLE = NULL
    y_MAX = max(y_DATA, na.rm = TRUE) * 1.2
    y_MIN = 0
    
    plot_box = ggplot(my_DATA, aes(x = x_DATA, y = y_DATA))+
      geom_violin(trim = FALSE, fill="gray")+
      geom_boxplot(width = 0.2) +
      coord_cartesian(ylim = c(y_MIN,y_MAX))+
      theme_xiaofei  +
      labs(y = y_LABEL , x = x_LABEL,
           title = TITLE)
    plot(plot_box)
  }
  dev.off()
}


### 2.15   get the count info of clones  **********************
# the number of trials, locations, years for each clone


## ****************** FUNCTION START ************************************* ##
CLONE_CT = function(my_dat ){
  
  clones = unique(my_dat$accession_name)
  ### 1.3.1 total plots per clone
  clone_plotNUM = my_dat %>%
    count(accession_name) %>%
    arrange(accession_name) %>%
    rename(plot_ct = n )
  
  
  ### 1.3.2 total trials per clone
  clone_envNUM = my_dat %>%
    distinct(accession_name, trial_name) %>%
    count(accession_name) %>%
    arrange(accession_name) %>%
    rename(trial_ct = n ) %>%
    select(accession_name, trial_ct)
  
  
  ### 1.3.3 number trials per year per clone
  years = unique(my_dat$year)
  clone_yrNUM = data.frame(matrix(nrow = length(clones),
                                  ncol = 1+length(years)))
  colnames(clone_yrNUM) = c("accession_name", paste(years, "_ct", sep=""))
  env_yr_clone = subset(my_dat, select = c("accession_name", "trial_name", "year"))
  
  for(i in 1:length(clones)){
    env_clone_i = subset(env_yr_clone, accession_name == clones[i])
    clone_yrNUM[i,1] = clones[i]
    for(j in 1:length(years)){
      year_j = subset(env_clone_i, year ==years[j])
      clone_yrNUM[i,c(j+1)] =  length(unique(year_j$trial_name))
    }
  }
  
  ### 1.3.4 number trials per location per clone
  locations = unique(my_dat$location)
  clone_locNUM = data.frame(matrix(nrow = length(clones),
                                   ncol = 1+length(locations)))
  colnames(clone_locNUM) = c("accession_name", paste(locations, "_ct", sep=""))
  env_loc_clone = subset(my_dat, select = c("accession_name", "trial_name", "location"))
  
  for(i in 1:length(clones)){
    env_clone_i = subset(env_loc_clone, accession_name == clones[i])
    clone_locNUM[i,1] = clones[i]
    for(j in 1:length(locations)){
      location_j = subset(env_clone_i, location ==locations[j])
      clone_locNUM[i,c(j+1)] =  length(unique(location_j$trial_name))
    }
  }
  
  ### 1.3.5 merge 1.3.1, 1.3.2, 1.3.3, 1.3.4
  ct_1 = merge(clone_plotNUM, clone_envNUM, by="accession_name")
  ct_2 = merge(ct_1, clone_yrNUM, by="accession_name")
  clone_ct_info = merge(ct_2, clone_locNUM, by = "accession_name") %>%
    arrange( desc(plot_ct) )
  
  write.csv(clone_ct_info, paste(folder, "01_", year_interest, trial_interest,
                                 "_clone_ct_", Sys.Date(),".csv", sep=""), row.names=FALSE)
}

### 2.16   select traits for analysis *******************

# mean, SD  by trials

# ---- function
remove_no_var = function(my_dat) {
  
  mean_trial = my_dat[, c("trial_name", "rep_number", all_of(analysis_trait))] %>%
    group_by(trial_name, rep_number) %>%
    summarise_all(mean, na.rm=TRUE)
  
  sd_trial = my_dat[, c("trial_name", "rep_number", all_of(analysis_trait))] %>%
    group_by(trial_name, rep_number) %>%
    summarise_all(sd, na.rm=TRUE)
  
  sd_mean = colMeans(sd_trial[, c( analysis_trait)] , na.rm = TRUE)
  sd_mean = data.frame (sd_mean) %>%
    rownames_to_column(var = "trait") %>%
    rename(mean_of_sd = sd_mean)
  
  print("The mean of SD of each trait:")
  print(sd_mean[order(sd_mean$mean_of_sd),])
  
  sd_mean_0 = sd_mean %>%
    filter(mean_of_sd == 0 )
  
  if (nrow(sd_mean_0) ==0) {
    print("Good, no traits without variance.")
    return(my_dat)
  }
  
  if (nrow(sd_mean_0) >0) {
    print("The traits without variation:")
    print(sd_mean_0)
    print("Remove the traits from the trial data")
    
    
    analysis_trait = analysis_trait[!analysis_trait %in% sd_mean_0$trait]
    
    my_dat = my_dat %>%
      select(all_of(meta_info), all_of(analysis_trait))
    return(my_dat)
  }
  
}

# Remove traits with non variation
remove_no_var_tidy <- function(my_dat, analysis_trait, meta_info) {
  
  # remove columns with all NA
  not_all_na = function(x) any(!is.na(x))
  my_dat_noNA = my_dat %>% select_if(not_all_na)
  
  mean_trial = my_dat[, c("trial_name",  all_of(analysis_trait))] %>%
    group_by(trial_name) %>%
    summarise_all(mean, na.rm=TRUE)
  
  sd_trial = my_dat[, c("trial_name",  all_of(analysis_trait))] %>%
    group_by(trial_name) %>%
    summarise_all(sd, na.rm=TRUE)
  
  sd_mean = colMeans(sd_trial[, c(analysis_trait)] , na.rm = TRUE)
  sd_mean = data.frame (sd_mean) %>%
    rownames_to_column(var = "trait") %>%
    rename(mean_of_sd = sd_mean) %>% 
    arrange(mean_of_sd)
  
  print("The mean of SD of each trait:")
  print(sd_mean)
  sd_mean <<- sd_mean
  
  sd_mean_0 = sd_mean %>%
    filter(mean_of_sd == 0 )
  
  if (nrow(sd_mean_0) ==0) {
    print("Good, no traits without variance.")
    trial_rm_sd = my_dat
  }else{ 
    print("The traits without variation are:")
    print(sd_mean_0)
    
    analysis_trait = analysis_trait[!analysis_trait %in% sd_mean_0$trait]
    
    trial_rm_sd = my_dat %>%
      select(all_of(meta_info), all_of(analysis_trait))
    print("We have removed the trait with SD=0")
    return(trial_rm_sd)
    
  }
}



#### __7. count of clones in each trial and environment__


## ****************** FUNCTION START ***********


CLONE_CT_tidy = function(my_dat ){
clones = unique(my_dat$accession_name)
### 1.3.1 total plots per clone
clone_plotNUM = my_dat %>%
  count(accession_name) %>%
  arrange(accession_name) %>%
  rename(plot_ct = n )


### 1.3.2 total trials per clone
clone_envNUM = my_dat %>%
  distinct(accession_name, trial_name) %>%
  count(accession_name) %>%
  arrange(accession_name) %>%
  rename(trial_ct = n ) %>%
  select(accession_name, trial_ct)


### 1.3.3 number trials per year per clone
years = unique(my_dat$year)
clone_yrNUM = data.frame(matrix(nrow = length(clones),
                                ncol = 1+length(years)))
colnames(clone_yrNUM) = c("accession_name", paste(years, "_ct", sep=""))
env_yr_clone = subset(my_dat, select = c("accession_name", "trial_name", "year"))

for(i in 1:length(clones)){
  env_clone_i = subset(env_yr_clone, accession_name == clones[i])
  clone_yrNUM[i,1] = clones[i]
  for(j in 1:length(years)){
    year_j = subset(env_clone_i, year ==years[j])
    clone_yrNUM[i,c(j+1)] =  length(unique(year_j$trial_name))
  }
}

### 1.3.4 number trials per location per clone
locations = unique(my_dat$location)
clone_locNUM = data.frame(matrix(nrow = length(clones),
                                 ncol = 1+length(locations)))
colnames(clone_locNUM) = c("accession_name", paste(locations, "_ct", sep=""))
env_loc_clone = subset(my_dat, select = c("accession_name", "trial_name", "location"))

for(i in 1:length(clones)){
  env_clone_i = subset(env_loc_clone, accession_name == clones[i])
  clone_locNUM[i,1] = clones[i]
  for(j in 1:length(locations)){
    location_j = subset(env_clone_i, location ==locations[j])
    clone_locNUM[i,c(j+1)] =  length(unique(location_j$trial_name))
  }
}

### 1.3.5 merge 1.3.1, 1.3.2, 1.3.3, 1.3.4
ct_1 = merge(clone_plotNUM, clone_envNUM, by="accession_name")
ct_2 = merge(ct_1, clone_yrNUM, by="accession_name")
clone_ct_info = merge(ct_2, clone_locNUM, by = "accession_name") %>%
  arrange( desc(plot_ct) )

clone_ct_info <<- clone_ct_info

}




#### __8. visualize the variation within and among trials using boxplot__
# ---- function

BOXPLOT_VAR_tidy = function (my_dat,
                        trait_wanted){
# remove columns with all NA
not_all_na = function(x) any(!is.na(x))
my_dat_noNA = my_dat %>% select_if(not_all_na)

# save as PDF, can adjust the figure size
pdf(paste(folder, "01_", experiment, "_boxplot_",
          Sys.Date(),".pdf", sep=""), width = 6, height = 6)

for(i in 1: length(trait_wanted)){
  y_DATA = my_dat_noNA[[trait_wanted[i]]]   # data frame or vector?
  x_DATA = my_dat_noNA$trial_name
  my_DATA = my_dat_noNA
  y_LABEL = trait_wanted[i]
  x_LABEL = NULL
  TITLE = trait_wanted[i]
  y_MAX = max(y_DATA, na.rm = TRUE) * 1.2
  y_MIN = 0
  
  plot_box = ggplot(my_DATA, aes(x = x_DATA, y = y_DATA))+
    geom_violin(trim = FALSE, fill="gray")+
    geom_boxplot(width = 0.2, trim = FALSE) +
    coord_cartesian(ylim = c(y_MIN,y_MAX))+
    theme_xiaofei  +
    labs(y = y_LABEL , x = x_LABEL,
         title = TITLE)
  plot(plot_box)
}
dev.off()
}


#### __ 12.1 BLUE boxplot 

BOXPLOT_VAR_blues = function (my_dat,
                             trait_wanted){
# remove columns with all NA
not_all_na = function(x) any(!is.na(x))
my_dat_noNA = my_dat %>% select_if(not_all_na)

# save as PDF, can adjust the figure size
pdf(paste(folder, "01_", experiment, "_BLUE_boxplot_",
          Sys.Date(),".pdf", sep=""), width = 4, height = 6)


for(i in 1: length(trait_wanted_blue)){
  y_DATA = my_dat_noNA[[trait_wanted_blue[i]]]   # data frame or vector?
  x_DATA = my_dat_noNA$trial
  my_DATA = my_dat_noNA
  y_LABEL = trait_wanted_blue[i]
  x_LABEL = NULL
  TITLE = NULL
  y_MAX = max(y_DATA, na.rm = TRUE) * 1.2
  y_MIN = 0
  
  plot_box = ggplot(my_DATA, aes(x = x_DATA, y = y_DATA))+
    geom_violin(trim=FALSE, fill="gray")+
    geom_boxplot(width=0.2) +
    coord_cartesian(ylim = c(y_MIN,y_MAX))+
    theme(axis.text.x = element_text(face="bold", colour="black", size=12, angle = 45, hjust = 1),
          axis.text.y = element_text(face="bold", colour="black", size=12),
          axis.title.y=element_text(size=14,face="bold", angle = 90, vjust = 3) ,
          axis.title.x=element_text(size=14,face="bold", vjust = -0.5) ,
          plot.title = element_text(color="red", size=16, face="bold.italic", hjust = 0.5),
          plot.margin = unit(c(1,1,1,2), "cm"), # top, right, bottom, left
          legend.position = "none"
    )  +
    labs(y = y_LABEL , x = x_LABEL,
         title = TITLE)
  plot(plot_box)
}
dev.off()
}

