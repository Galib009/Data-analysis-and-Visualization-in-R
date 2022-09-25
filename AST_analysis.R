# loading the required libraries - 
library(tidyverse)
library(readxl)


setwd("E:/GitHub")

ast_df <- read_excel("ESBL_SIDA_all_results_latest_7_4_2022.xlsx", sheet = 1)

#View(ast_df)

# data Management steps (filtering and selecting variables)- 
f_ast_df <- ast_df %>%
  filter(Type != "P/Ps")  %>%
  filter(Type != "NA") %>%
  filter(Isolate != "NG")%>%
  filter(Isolate != "KEC_EC_Repeat") %>%
  filter(`Trimethoprim-silphamethoxazole` != "NA") %>%
  select(ID, Type,Isolate, Location, LB_DB, contains("AST") )

#View(f_ast_df)

# Making it long from wide dataframe:
l_f_ast_df <- f_ast_df %>% 
  pivot_longer(
    cols = 6:16, 
    names_to = "Antibiotic_name",
    values_to = "Sensitivity"
  )
#View(l_f_ast_df)

# removings rows having sensitivity = NA -
l_f_ast_df <- l_f_ast_df %>%
  filter(Sensitivity != "NA")

# adding column based on the name of another column (making I to R):
ml_f_ast_df <- l_f_ast_df %>%
  mutate(Sensitivity_main = case_when(
    endsWith(Sensitivity, "R") ~ "R",
    endsWith(Sensitivity, "I") ~ "R",
    endsWith(Sensitivity, "S") ~ "S"
  )
  )

#View(ml_f_ast_df)

# Making all character columns to factor:
ml_f_ast_df <- mutate_if(ml_f_ast_df, is.character, as.factor)

#calculating number of each isolate of only "R" by isolate and Location:
f_ast_count <- ml_f_ast_df %>%
  group_by(Isolate, Location, LB_DB, Antibiotic_name,Sensitivity_main , .drop = FALSE) %>%
  count()
View(f_ast_count)

# replacing characters as intended ->
f_ast_count$Antibiotic_name<-gsub("_AST","",as.character(f_ast_count$Antibiotic_name))
f_ast_count$LB_DB<-gsub("LB","Live Bird",as.character(f_ast_count$LB_DB))
f_ast_count$LB_DB<-gsub("DB","Dead Bird",as.character(f_ast_count$LB_DB))
f_ast_count$Location<-gsub("Dhaka","Urban (Dhaka)",as.character(f_ast_count$Location))
f_ast_count$Location<-gsub("Mirzapur","Rural (Mirzapur)",as.character(f_ast_count$Location))



bad_s_percentage_df  <-  f_ast_count %>%
  filter(Sensitivity_main != "S")
#View(bad_s_percentage_df)

### facet_grid :-
main_plot <- ggplot(bad_s_percentage_df, aes(fill= LB_DB, x = Antibiotic_name, y = n)) +
  geom_bar(position="dodge",stat = "identity") +
  facet_grid(Location ~ Isolate) + 
  ggtitle("") + ylab("Number of Resistant Isolates") + xlab("Antibiotic Name") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8))) + # turn off minor grid
  scale_y_continuous(breaks=seq(0, 50, 5)) + 
  scale_fill_discrete(name = "Bird Status")

main_plot

ggsave("AST.tiff", units="in", width=12, height=6, dpi=300, compression = 'lzw')
