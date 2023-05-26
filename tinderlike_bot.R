library(telegram.bot)
library(httr)
library(jsonlite)

source("secret.R")
update_path = "updates/tbot/"

tbot <- Bot(token = bot_token)
print(tbot$getMe())

# Команды

commands <- list(
     list(command = "/like", description = "Like a user"),
     list(command = "/dislike", description = "Dislike a user"),
     list(command = "/match", description = "View your matches"),
     list(command = "/profile", description = "Create a user profile"),
     list(command = "/next", description = "See the next user profile to evaluate")
)

# Обработка команд
# Define a function to handle updates from Telegram
handle_updates <- function(tbot, updates) {
     for (update in updates) {
          message <- update$message
          chat_id <- message$chat_id
          text <- message$text
          
          if (startsWith(text, "/like")) {
               # Add the user to the list of likes
               tbot$sendMessage(chat_id = chat_id, text = "like")
          } else if (startsWith(text, "/dislike")) {
               # Add the user to the list of dislikes
               tbot$sendMessage(chat_id = chat_id, text = "dislike")
          } else if (startsWith(text, "/match")) {
               # Display a list of matches
               tbot$sendMessage(chat_id = chat_id, text = "match")
          } else if (startsWith(text, "/profile")) {
               create_profile(bot, update)
          } else if (startsWith(text, "/next")) {
               next_profile(bot, update)
          }
     }
}

# Define a list to store user profiles
user_profiles <- list()

# Define a function to create a user profile
create_profile <- function(tbot, update) {
     message <- update$message
     chat_id <- message$chat_id
     
     # Check if the user already has a profile
     if (chat_id %in% names(user_profiles)) {
          send_message(tbot, chat_id, "You already have a profile.")
          return()
     }
     
     # Ask the user for their name
     send_message(tbot, chat_id, "What's your name?")
     name <- wait_for_text(tbot, chat_id)
     
     # Ask the user for their age
     send_message(tbot, chat_id, "How old are you?")
     age <- as.integer(wait_for_text(tbot, chat_id))
     
     # Ask the user for their gender
     send_message(tbot, chat_id, "What's your gender? (male/female/non-binary)")
     gender <- wait_for_text(tbot, chat_id)
     
     # Create the user profile
     user_profiles[[chat_id]] <- list(
          name = name,
          age = age,
          gender = gender,
          likes = list(),
          dislikes = list()
     )
     
     send_message(tbot, chat_id, "Profile created!")
}

# Define a function to see the next user profile to evaluate
next_profile <- function(tbot, update) {
     message <- update$message
     chat_id <- message$chat_id
     
     # Check if the user has a profile
     if (!(chat_id %in% names(user_profiles))) {
          send_message(tbot, chat_id, "You need to create a profile first.")
          return()
     }
     
     # Get the user's likes and dislikes
     likes <- user_profiles[[chat_id]]$likes
     dislikes <- user_profiles[[chat_id]]$dislikes
     
     # Get a list of all user profiles except the user's own profile
     other_profiles <- user_profiles[names(user_profiles) != chat_id]
     
     # Filter out profiles that the user has already liked or disliked
     other_profiles <- Filter(function(profile) {
          !(profile$chat_id %in% c(likes, dislikes))
     }, other_profiles)
     
     # If there are no more profiles to evaluate, send a message to the user
     if (length(other_profiles) == 0) {
          send_message(tbot, chat_id, "There are no more profiles to evaluate.")
          return()
     }
     
     # Get the next profile to evaluate
     next_profile <- other_profiles[[1]]
     
     # Send a message to the user with the next profile's information
     send_message(tbot, chat_id, paste0(
          "Name: ", next_profile$name, "n",
          "Age: ", next_profile$age, "n",
          "Gender: ", next_profile$gender
     ))
}

# Get the last update ID
last_update_id <- 0
# Define a function to get updates from Telegram
get_updates <- function(tbot) {

     if (exists("last_update_id.rds")) {
          last_update_id <- readRDS(paste0(update_path,"last_update_id.rds"))
     }
     
     # Get updates from Telegram
     updates <- tbot$getUpdates()
     
     # Save the last update ID
     if (length(updates) > 0) {
          last_update_id <- max(sapply(updates, function(update) update$update_id))
          saveRDS(last_update_id, paste0(update_path,"last_update_id.rds"))
     }
     
     return(updates)
}


# Start bot and continuously get updates and handle them
while (TRUE) {
     updates <- get_updates(tbot)
     handle_updates(tbot, updates)
     Sys.sleep(1)
}
     