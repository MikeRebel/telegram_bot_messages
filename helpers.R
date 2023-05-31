dushno_congratulations = data.table(
     
   phrase = c(
 "настоящий чемпион в душении всех вокруг	",
 "непревзойденный мастер душного искусства	",
 "душнение - это твое второе имя	",
 "твои душные способности покорили всех	",
 "ты заслуживаешь золотой медали за свою душноту	",
 "настоящий герой на поле битвы душневых	",
 "ты настоящий мастер в душных делах	",
 "в своем стремлении душнить 	",
 "король душных баталий	",
 "заслуживает звания Мистер Душнитель	",
 "настоящий лидер в душных играх	",
 "своей жажде душнить всех вокруг	",
 "настоящий герой на поле душных битв	",
 "твои умения в душнении просто поразительны	",
 "заслуживаешь звания Король Душнитель	",
 "настоящий чемпион в душных играх	",
 "в своих попытках душить всех вокруг	",
 "главный душитель в нашей команде	",
 "заслуживает звания Мистер Душительность	",
 "настоящий гуру в душных делах	",
 "в своих стремлениях душнить всех вокруг	",
 "настоящий герой на поле душных битв	",
 "твои умения в душении просто поразительны	",
 "заслуживает звания Душнитель № 1	",
 "настоящий мастер в душных играх	",
 "в своих попытках душинить всех вокруг	",
 "главный душнитель в нашей команде	",
 "заслуживает звания Мистер Душнительный	",
 "настоящий чемпион в душных играх	",
 "в своих стремлениях душнить всех вокруг	",
 "король душных баталий	",
 "заслуживает звание Душнитель Года	",
 "настоящий мастер в душных играх	",
 "в своих желаниях душнить всех вокруг	",
 "настоящий герой на поле душных битв	",
 "заслуживает звания Мистер Душительность	",
 "настоящий гуру в душных делах	",
 "в своих стремлениях душнить всех вокруг	",
 "настоящий герой на поле душных баталий	",
 "твои умения в душении поразительны	",
 "заслуживает звания Король Душительности	",
 "настоящий чемпион в душных играх	",
 "в своих попытках душнить всех вокруг	",
 "главный душнитель в нашей команде	",
 "заслуживает звания Мистер Душнительный	",
 "настоящий мастер в душных играх	",
 "в своих желаниях душнить всех вокруг	",
 "настоящий герой душнитель на поле	"
          
          
          
     )
)


get_random_number <- function() {
     # Get current date
     today <- as.Date(Sys.time())
     
     # Check if today's date is already stored in the environment
     if (exists("last_date") && last_date == today) {
          # If today's date is already stored, check if 1 has been returned 3 times
          if (exists("count_ones") && count_ones >= 5) {
               # If 1 has been returned 3 times, return 0
               return(0)
          } else {
               # If 1 has not been returned 3 times, randomly return 0 or 1
               random_number <- sample(1:random_number_multiplicatior, 1)
               if (random_number == 1) {
                    # If 1 is returned, increment the count of 1s returned today
                    count_ones <<- count_ones + 1
                    random_number_multiplicatior <<- random_number_multiplicatior * (1 + count_ones)
                    return(random_number)
               }
               return(0)
          }
     } else {
          # If today's date is not stored, reset the count of 1s returned and store today's date
          count_ones <<- 0
          last_date <<- today
          random_number_multiplicatior <<- 25
          # Randomly return 0 or 1
          random_number <- sample(0:random_number_multiplicatior, 1)
          if (random_number == 1) {
               # If 1 is returned, increment the count of 1s returned today
               count_ones <<- count_ones + 1
               return(random_number)
          }
          return(0)
     }
}


