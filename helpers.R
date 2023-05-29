get_random_number <- function() {
     # Get current date
     today <- as.Date(Sys.time())
     
     # Check if today's date is already stored in the environment
     if (exists("last_date") && last_date == today) {
          # If today's date is already stored, check if 1 has been returned 3 times
          if (exists("count_ones") && count_ones >= 3) {
               # If 1 has been returned 3 times, return 0
               return(0)
          } else {
               # If 1 has not been returned 3 times, randomly return 0 or 1
               random_number <- sample(1:random_number_multiplicatior, 1)
               if (random_number == 1) {
                    # If 1 is returned, increment the count of 1s returned today
                    count_ones <<- count_ones + 1
                    random_number_multiplicatior <<- random_number_multiplicatior * count_ones
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
