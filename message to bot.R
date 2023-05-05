library(telegram)
library(telegram.bot)
library(httr)
library(jsonlite)

bot_token <- ""

library(telegram.bot)

bot <- Bot(token = bot_token)
print(bot$getMe())

updates <- bot$getUpdates()
chat_id <- updates[[1]][["message"]][["chat"]][["id"]]

bot$sendMessage(chat_id = chat_id, text = "TestReply")


output <- capture.output({
     # Your R code goes here
     x <- 1:10
     mean(x)
})

bot$sendMessage(chat_id = chat_id, text = paste(output, collapse = "\n"))

