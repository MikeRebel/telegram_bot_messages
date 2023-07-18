library(telegram.bot)
library(httr)
library(jsonlite)
library(data.table)
library(maditr)

source("secret.R")
source("helpers.R")
update_path = "updates/tbot/"

tbot <- Bot(token = tbot_token)
print(tbot$getMe())

updates <- tbot$getUpdates()

# Команды
# start - Strat bot
# like  - Like a user
# dislike  - Dislike a user
# match  - View your matches
# profile  - Create a user profile
# next  - See the next user profile to evaluate


# commands <- list(
#      list(command = "/like", description = "Like a user"),
#      list(command = "/dislike", description = "Dislike a user"),
#      list(command = "/match", description = "View your matches"),
#      list(command = "/profile", description = "Create a user profile"),
#      list(command = "/next", description = "See the next user profile to evaluate")
# )

# Обработка команд
# Define a function to handle updates from Telegram
# handle_updates <- function(tbot, updates) {
#      for (update in updates) {
#           message <- update$message
#           chat_id <- message$chat_id
#           text <- message$text
#           
#           if (startsWith(text, "/like")) {
#                # Add the user to the list of likes
#                tbot$sendMessage(chat_id = chat_id, text = "like")
#           } else if (startsWith(text, "/dislike")) {
#                # Add the user to the list of dislikes
#                tbot$sendMessage(chat_id = chat_id, text = "dislike")
#           } else if (startsWith(text, "/match")) {
#                # Display a list of matches
#                tbot$sendMessage(chat_id = chat_id, text = "match")
#           } else if (startsWith(text, "/profile")) {
#                create_profile(tbot, update)
#           } else if (startsWith(text, "/next")) {
#                next_profile(tbot, update)
#           }
#      }
# }

# Define a list to store user profiles
user_profiles <- list()

# Define a function to create a user profile
create_profile <- function(tbot, update) {
     user_message <- as.character(update$message$text)
     chat_id <- update$message$chat_id
     user_id <- update$message$from_user
     user_name <- update$message$from$username
     
     # Check if the user already has a profile
     if (user_id %in% names(user_profiles)) {
          tbot$send_message(chat_id, "You already have a profile.")
          return()
     }
     
     # Ask the user for their name
     tbot$send_message(chat_id, "What's your name?")
     name <- tbot$wait_for_text(chat_id)
     
     # Ask the user for their age
     tbot$send_message(chat_id, "How old are you?")
     age <- as.integer(wait_for_text(tbot, chat_id))
     
     # Ask the user for their gender
     tbot$send_message(chat_id, "What's your gender? (male/female)")
     gender <- wait_for_text(tbot, chat_id)
     
     # Create the user profile
     user_profiles[[chat_id]] <- list(
          name = name,
          age = age,
          gender = gender,
          likes = list(),
          dislikes = list()
     )
     
     tbot$send_message(chat_id, "Profile created!")
}

# Define a function to see the next user profile to evaluate
next_profile <- function(tbot, update) {
     message <- update$message
     chat_id <- message$chat_id
     
     # Check if the user has a profile
     if (!(chat_id %in% names(user_profiles))) {
          tbot$send_message(chat_id, "You need to create a profile first.")
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
          tbot$send_message(chat_id, "There are no more profiles to evaluate.")
          return()
     }
     
     # Get the next profile to evaluate
     next_profile <- other_profiles[[1]]
     
     # Send a message to the user with the next profile's information
     tbot$send_message(chat_id, paste0(
          "Name: ", next_profile$name, "n",
          "Age: ", next_profile$age, "n",
          "Gender: ", next_profile$gender
     ))
}

# Get the last update ID
last_update_id <- 0


rm(updater)
updater <- Updater(token = tbot_token)

start_handler <- CommandHandler("start", start)
create_profile_handler <- CommandHandler("profile", create_profile)
updater <- updater + start_handler + create_profile_handler


updater$start_polling()
# Start bot and continuously get updates and handle them
#while (TRUE) {
#     updates <- get_updates(tbot)
#     handle_updates(tbot, updates)
#     Sys.sleep(5)
#}
     