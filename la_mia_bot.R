source("secret.R")
source("helpers.R")

bot <- Bot(token = mia_token)
print(bot$getMe())
updates <- bot$getUpdates()
# chat_id <- updates[[1]][["message"]][["chat"]][["id"]]
# bot$sendMessage(chat_id = chat_id, text = "TestReply")



start_handler <- CommandHandler("start", start)
save_image_to_local_handler <- MessageHandler(save_image_to_local, MessageFilters$photo)
preview_images_handler <- CommandHandler("preview_all", preview_pictures)
image_name_save <- CommandHandler("image_name_save", save_picture_info)
show_my_pictures <- CommandHandler("show_my_pictures", show_pictures)
rm(updater)
updater <- Updater(token = mia_token)
updater <- updater + start_handler
updater <- updater + save_image_to_local_handler
updater <- updater + preview_images_handler
updater <- updater + image_name_save
updater <- updater + show_my_pictures
updater$start_polling()


