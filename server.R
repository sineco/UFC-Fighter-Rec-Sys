library(shiny)
library(dplyr)

## Read the data
fights <- read.csv("./data/allFigths.csv", header = TRUE, na.strings = c(""))
fighters <- read.csv("./data/allFighters.csv", header = TRUE, 
                     na.strings = c(""), colClasses=c("name"="character")) 

## Pre-process data
# remove duplicated rows
fighters <- fighters[!duplicated(fighters$fid),]
#fighters$name <- as.character(fighters$name)

# Unify country names
fighters$country[fighters$country == "USA"] <- "United States"
fighters$country[fighters$country == "Finnland"] <- "Finland"
fighters$country <- factor(fighters$country)

# Standardize the win methods
fights$method[grepl("^No", fights$method)] <- "NC"
fights$method <- factor(fights$method)

indices <- fights$method == "Technical" & grepl("^Submission", fights$method_d)
indices2 <- fights$method == "Technical" & grepl("^Decision", fights$method_d)
fights$method[indices] <- "Submission"
fights$method[indices2] <- "Decision"
fights$method <- factor(fights$method)

# Standardize the techniques
fights$method_d[fights$method_d == "Arm Triangle Choke"] <- "Arm-Triangle Choke"



# A non-reactive function that will be available to each user session
similarityFunction <- function(type_win, method_win) {
    cat("type_win::")
    print(type_win)
    merged <- merge(x = fights, y = fighters, by.x = 'f1fid', by.y = 'fid', 
                    all.x=TRUE)
    
    wins_by_type <- filter(merged, f1result == 'win', method == type_win, 
                           method_d == method_win)
   
    wins_by_type <- select(wins_by_type, event_name, f1name, f1result, method, 
                           method_d)
    cat("Wins by type::")
    print(wins_by_type)
    
    #Rank by method_d
    by_name <- group_by(wins_by_type, f1name, method_d )
    rank <- summarise(by_name, count = n())
    df <- arrange(rank, desc(count))
    
    return(df[1:3,])
}

findMajorityWins <- function(selected_fighter){
    merged <- merge(x = fights, y = fighters, by.x = 'f1fid', by.y = 'fid', 
                    all.x=TRUE)
    
    wins <- filter(merged, f1result == 'win', name == selected_fighter)
    wins <- select(wins, event_name, f1name, f1result, method, method_d)
    wins_by_method <- group_by(wins, method)
    rank_methods <-summarise(wins_by_method, count = n())
    # Select the majority win type (e.g. submission, KO, TKO, decision, etc)
    majority_wins <- rank_methods[[which.max(rank_methods$count),1]]
    
}

findMajorityMethod <- function(selected_fighter, method_win){
    merged <- merge(x = fights, y = fighters, by.x = 'f1fid', by.y = 'fid', 
                    all.x=TRUE)
    wins <- filter(merged, f1result == 'win', name == selected_fighter, 
                   method == method_win)
    wins_by_method <- group_by(wins, method_d)
    rank_methods <-summarise(wins_by_method, count = n())
    # Select the majority method of win (e.g. punches, armbar, etc)  
    print(rank_methods)
    majority_method <- rank_methods[[which.max(rank_methods$count),1]]
 
}

shinyServer(
    function(input, output) {
        # Drop-down selection box for choosing fighter
        output$choose_fighter <- renderUI({
            fighter_names <- unique(fighters$name)
            selectInput("my_fighter", "Choose a fighter you like", 
                        choices = fighter_names, selectize = TRUE, 
                        multiple = FALSE, selected = 'Conor McGregor')
        })
        
        output$fighter_out <- renderPrint({input$my_fighter})
      
        output$view <- renderTable({
            sub_fighter <- select(fighters, name, nick, birth_date, height, weight,
                                  association, class, locality, country)
        
            sub_fighter[sub_fighter$name == input$my_fighter,]
            
        })
        
        output$fighter_record <- renderTable({
            merged <- merge(x = fights, y = fighters, by.x = 'f1fid', by.y = 'fid', 
                            all.x=TRUE)
            
            
            if (is.null(input$my_fighter)){
                # For some reason it always start with null even though I 
                # used selected = 'Conor McGregor' in selectInput
                selected_fighter <- "Conor McGregor"
            }else{
                selected_fighter <- input$my_fighter
            }
                    
            
            my_fights <- filter(merged, name == selected_fighter)
           
            summarise(my_fights,
                      wins = sum(f1result=="win"),
                      number_of_fights = nrow(my_fights),
                      number_of_KOs = sum(method=="KO"),
                      number_of_TKOs = sum(method=="TKO"),
                      number_of_decisions = sum(method=="Decision"),
                      number_of_submission = sum(method=="Submission"),
                      number_of_NC = sum(f1result=="NC"),
                      number_of_draws = sum(f1result=="draw")
            )
            
        })
        
        output$view_recommended <- renderTable({
            if (input$goButton == 0)
                return()
            
            isolate({
                majority_win <- findMajorityWins(input$my_fighter)
                majority_method <- findMajorityMethod(input$my_fighter, 
                                                      majority_win)
                similarityFunction(majority_win, majority_method)
                
            })
        
            
        })
        
       
    }
)