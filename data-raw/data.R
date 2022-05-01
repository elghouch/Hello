require(devtools)

## code to prepare `language` dataset goes here

language <- data.frame(code = c("en","fr","it","es","de"), hello = c("Hello","Bonjour", "Bonjourno", "Hola", "Hallo") )

use_data(language, overwrite = TRUE)
