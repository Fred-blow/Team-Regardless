---
title: "2021 Demographic Analysis of Illegal Mining: 
        Insights from Education and Age Group"
lastchanged: "05-11-2023"
---
  
# Setup (Including GSS Theme)
{
    # Load packages
    library(ggplot2)
    library(tidyverse)
    library(ggtext)
    library(gghighlight)
    library(sysfonts)
    library(showtextdb)
    library(showtext)
    library(glue)
    library(scales)
    library(kableExtra)
    library(patchwork)
    library(forcats)
    library(tidyverse)
    library(ggbump)
    library(reshape2)
    library(plotly)
    library(readxl)
    library(lubridate)
    library(haven)
    
    # No scientific notation
    options(scipen = 999)
    
    # Set language to English
    Sys.setenv(LANG = "en")
    
    # Load the font
    font_add("Century Gothic", "input/Century Gothic.ttf")
    
    # Make sure ggplot recognizes the font 
    # and set the font to high-res
    showtext_auto()
    showtext::showtext_opts(dpi = 300)
    
    # Set default colour for plots with multiple categories
    options(ggplot2.discrete.colour = c("#210D69", "#DB2E76", "#586889", "#227C42"))
    options(ggplot2.discrete.fill   = c("#210D69", "#DB2E76", "#586889", "#227C42"))
    
    # Set default colour for plots with a single category
    update_geom_defaults("bar",   list(fill = "#27A0CC"))
    update_geom_defaults("col",   list(fill = "#27A0CC"))
    
    # Update the font to show in geom_text()
    update_geom_defaults("text",   list(family =  "Century Gothic", size = 4.5 ))
    GSS_font <-  "Century Gothic"
    
    # Create a GGS theme based on the theme_gray()
    gssthemes<-function(){
      theme_gray() %+replace%
        theme(
          text=element_text(family =  "Century Gothic",
                            colour="black",
                            size=10),
          plot.margin = margin(0.5,0.3, 0.3, 0.3, "cm"),
          # plot.title =element_textbox_simple(family= "Century Gothic", size=16,
          #                                    lineheight=1,
          #                                    margin=margin(b=10)),
          # plot.title.position="plot",
          plot.caption=element_markdown(hjust=0, color="gray",
                                        lineheight=1.5,
                                        margin =margin(t=10)),
          plot.caption.position="plot",
          axis.title.y=element_text(color="black", angle=90, size = 10),
          axis.title.x=element_text(color="black",size = 10),
          axis.text.x=element_text(color="black", size = 10, vjust = 0, 
                                   margin = margin(t = 5, r = 5, b = 0, l = 0, unit = "pt")),
          axis.text.y=element_text(color="black", size = 10, hjust = 1, 
                                   margin = margin(t = 5, r = 5, b = 0, l = 0, unit = "pt")),
          legend.text=element_text(color="black",  size = 10),
          panel.grid.major.y=element_line(color="gray", size=0.25),
          panel.grid.major.x=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_rect(fill="white", color=NA),
          plot.background=element_rect(fill="white", color=NA),
          legend.background=element_rect(fill="white", color=NA),
          strip.background =element_rect(fill="white",  color=NA),
          legend.key = element_rect(fill = "white", color = NA), 
          strip.text = element_text(size = 20,  
                                    margin = margin(t = 5, r = 0, b = 10, l = 0, unit = "pt"))
        )
    }
    
    # This will make the labels of the bar chart a bit nicer, by ending above the highest data point
    nicelimits <- function(x) {
      range(scales::extended_breaks(only.loose = TRUE)(x))
    }
    
    # Define color palette
    statscolours_color_scheme  <- c("#382873", "#0168C8", "#00B050")
    
    # Locality
    national_color <- "#27A0CC"
    urban_color <- "#871A5B"
    rural_color <- "#22D0B6"
    urbanrural_color_scheme  <- c("#27A0CC", "#871A5B", "#22D0B6")
    
    # Sex
    male_color <- "#206095"
    female_color <- "#F66068"
    malefemale_color_scheme <- c("#27A0CC", "#206095", "#F66068")
    
    # Positive/negative
    negative_color <- "#cc3333"
    positive_color <- "#33cccc"
    
    # Pallete
    neutral_color_scheme  <- c("#002060", "#0070C0", "#00B0F0", "#8EA9DB", "#9BC2E6", "#FFFFCC")
    posneg_color_scheme  <- c("#38761D","#6AA84F","#93C47D","#F4CCCC","#E06666","#990000")
    posneutralneg_color_scheme  <- c("#38761D","#6AA84F","#FFFFCC","#E06666","#990000")
    population_color_scheme  <- c("#FFFFCC","#C7E9B4","#7FCDBB","#41B6C4","#2C7FB8")
    incidence_color_scheme  <- c("#FECCCC","#FF9999","#FF6666","#FF3333","#CC0000","#990000")
    
    
    # Economic sector colour
    industry_color<-"#14607A"
    agric_color<-"#07BB9E"
    services_color<-"#F98B00"
    economic_sectors<- c("#14607A","#07BB9E","#F98B00")
    
    # Food
    food_colour<- "#3ECDB9"
    nonfood_colour<-"#04BCFC"
    
    # Region
    Ahafo_color_code <- "#4B644B"
    Ashanti_color_code <- "#7D96AF"
    Bono_color_code <- "#EBA07E"
    Bono_East_color_code <- "#09979B"
    Central_color_code <- "#EA879C"
    Eastern_color_code <- "#E5B9AD"
    Greater_Accra_color_code <- "#CC9B7A"
    Northern_color_code <- "#FDD835"
    North_East_code <- "#0070C0"
    Oti_color_code <- "#AE2B47"
    Savannah_color_code <- "#F94240"
    Upper_East_color_code <- "#903000"
    Upper_West_color_code <- "#0F3B6C"
    Volta_color_code <- "#59A77F"
    Western_color_code <- "#FDAE6B"
    Western_North_color_code <- "#B173A0"
    
    # Trade colors
    col_export <-"#006D2C"
    col_import <- "#08519C"
    
  }

# Read the 10% sample data of the 2021 PHC from GSS
{
  totpop <- read.csv("input/defactopopn_10%_20221011d.csv")
}

# Create a vector for the illegal mining districts
{
  illegal_mining_dist <- c("Tarkwa-Nsuaem Municipal", 
                         "Prestea/Huni Valley Municipal",
                         "Wassa Amenfi East Municipal", "Upper Denkyira West",
                         "Birim North", "Upper Denkyira East Municipal", 
                         "Amansie Central")
}

# Filter data by mining industry type + select relevant variables 
{
  illegal_mine_data <-  totpop %>%
    filter(distcode %in% illegal_mining_dist, 
           p02 >14, p15b1 == "Mining and quarrying") %>%
  select(region, distcode, subdist, urbrur, p02, a11d, 
         p11a, p12a, p12b, p15b1) 
}

# Rename the selected variables
{
  illegal_mine_data_renamed <- illegal_mine_data %>%
    rename(sex = a11d, age = p02, read_write = p11a, education = p12a,
           highest_level = p12b, industry_type = p15b1)
}

# Create an age group variable
{
  illegal_mine_age_grp <- illegal_mine_data_renamed %>%
  mutate(age_group = case_when(
    age >= 15 & age <= 24 ~ "15-24",
    age >= 25 & age <= 34 ~ "25-34",
    age >= 35 & age <= 44 ~ "35-44",
    age >= 45 & age <= 54 ~ "45-54",
    age >= 55 & age <= 64 ~ "55-64",
    age >= 65  ~ "65+"))
}

# Filter data by all industry type + select relevant variables 
{
  pop_all_industry <-  totpop %>%
    filter(distcode %in% illegal_mining_dist, 
           p02 >14) %>%
    select(region, distcode, subdist, urbrur, p02, a11d, 
           p11a, p12a, p12b, p15b1) 
}

# Rename the selected variables
{
  pop_all_industry_renamed <- pop_all_industry %>%
    rename(sex = a11d, age = p02, read_write = p11a, education = p12a,
           highest_level = p12b, industry_type = p15b1)
}



## Figures ## 


# Create a histogram of all ages
{
  # Mutate count for the age
  {
  age_count <- illegal_mine_age_grp %>% 
    count(age_group)
}
  
  # Plot
  {
    ggplot(age_count, aes(x = age_group, y = n)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "black", 
               width = 1.0) +
      gssthemes() +
      labs(x = "Age group", y = "Count") +
      theme_minimal() +
      geom_vline(xintercept = which(age_count$age_group == "35-44") + 0.5,
                 linetype = "dashed", color = "red", size = 1.5) + 
      theme_minimal() +
      theme(text = element_text(family = "Century Gothic"))
    
    ggsave("output/Histogram of Ages and Their Count.png", height = 6, width = 8)
    
  }
}

# Group data by age group (15-44 years) and education status + plot chat
{
  # Group data   
{
  illegal_mine_grouped <- illegal_mine_age_grp %>% 
  filter(age_group != "45+") %>% 
  group_by(age_group, education) %>% 
  summarise(count = n()) %>%
  mutate(age_group = factor(age_group, levels = c("15-24", "25-34", "35-44")),
         # Calculate percentage 
         percent = count / sum(count) * 100)  
}

  # Plot
  {
  ggplot(illegal_mine_grouped, aes(x = age_group, y = percent, fill = education)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(percent), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, color = "black", size = 5) +
  gssthemes() +
  scale_fill_manual(values = c("Attended in the past" = "#14607A", 
                               "Attending now" = "#07BB9E", "Never attended" = "#27A0CC"),
                    name = "Education Status") +
  labs(x = "Age group", y = "Percent", fill = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 100, by = 20)) +
  theme(legend.position = "right", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
  
  ggsave("output/Age group by education status.png", height = 6, width = 12)
}
  
}

# Mining industry vs other industry type
{
  # Group data by mining and other industry
   mining_other <- pop_all_industry_renamed %>% 
    mutate(Industry = case_when(
      industry_type == "Mining and quarrying" ~ "Mining",
      industry_type != "Mining and quarrying" ~ "Other")) %>% 
    count(Industry)
  
  
  # Plot
  ggplot(mining_other, aes(x = "",
                               y = n, 
                               fill = Industry)) +
    geom_col() +
    coord_polar(theta = 'y') +
    scale_fill_manual(values = c("Mining" = "red", "Other"= "darkblue")) +
    geom_text(aes(label = percent(n/sum(n, na.rm = T))),
              color = "white", size = 4, position = position_stack(vjust = 0.5)) +
    gssthemes() +
    theme(axis.text = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y= element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank())  +
    labs(x = NULL, y = NULL)
  
  ggsave("output/percentage of persons in mining industry.png")
  
}

# Group by education levels
{
  education_level <- illegal_mine_age_grp %>% 
    filter(highest_level != "") %>% 
    mutate(levels = case_when(
      highest_level %in% c("JSS/JHS", "Kindergarten",
                          "Middle", "Nursery", "Primary") ~ "JHS and below",
      highest_level %in% c("Post middle/secondary Certificate", "SSS/SHS",
                          "Secondary", "Post middle/secondary Diploma",
                          "Voc/technical/commercial") ~ "Secondary",
     highest_level %in% c("Tertiary - Bachelor?s Degree", "Tertiary - Master?s Degree",
                        "Tertiary - PhD", "Tertiary - Post graduate Certificate/Diploma",
                          "Tertiary ? HND") ~ "Tertiary")) %>% 
    count(levels) 
  
  
  # Plot
  {
    education_level %>% 
      ggplot(mapping = aes(x = str_wrap(levels, 20), y = n/sum(n))) +
      geom_col(width = 0.8) +
      geom_text(mapping = aes(label = scales::percent(n/sum(n))), hjust = -0.2, size = 3) +
      gssthemes() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = NULL, y = "Count")+
      coord_flip(clip = "off") +
      theme(panel.grid.major.x=element_line(color = "gray", size=0.25),
            panel.grid.major.y=element_blank())
    
    ggsave("output/education levels.png", height = 6, width = 8)
    
    }
  
}