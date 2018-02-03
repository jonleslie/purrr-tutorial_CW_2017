library(tidyverse)
library(skimr)
library(data.tree)
load("data/swapi.rda")

str(people, 2)
length(people) #87 lists
people[[1]][1] #Luke Skywalker
names(people[[1]])
people[1] %>% str()
people[[1]] %>% str()
people[[1]]$name

skimr::skim(people)
data.tree::FromListSimple(people)
warnings()

# map() -------------------------------------------------------------------

luke <- people[[1]]
luke %>% select(starships) %>% 
  count()
str(Luke)
luke$starships
length(luke$starships)

# make it a recipe, use .x as a placeholder
map(people, ~length( .x$starships))

# Find homeworld of each character
load("data/planet_lookup.rda")
planet_lookup
str(planet_lookup)
names(planet_lookup)

luke$homeworld
planet_lookup[names(planet_lookup) == luke$homeworld]
planet_lookup[[luke$homeworld]]
planet_lookup[luke$homeworld]
map(people, ~planet_lookup[.x$homeworld]) %>% str()
map(people, ~planet_lookup[[.x$homeworld]]) %>% str()

# Friends of map() --------------------------------------------------------

# names
people <- people %>% set_names(map_chr(people, "name")) # map_chr(people, ~.x[["name"]])
people

# How many starships has each character been in?
map(people, ~ length(.x[["starships"]])) #returns a list
map_int(people, ~ length(.x[["starships"]]))  #returns a vector

# What color is each character's hair?
map(people, ~ .x[["hair_color"]])
map_chr(people, ~ .x[["hair_color"]])

# is the character male?
map(people, ~ .x[["gender"]] == "male")
map_lgl(people, ~ .x[["gender"]] == "male")

# How heavy is each character
map(people, ~ .x[["mass"]])
map_dbl(people, ~ .x[["mass"]]) #Doesn't work, roll back to map() to investigate
map_chr(people, ~ .x[["mass"]])
# could use something like this:
map_chr(people, ~ .x[["mass"]]) %>% 
  readr::parse_number(na = "unknown")


# .f can be a formula -----------------------------------------------------

# .f can be a string or integer -------------------------------------------

# map(.x, .f = "some_name") is equivalent to map(.x, ~ .x[[some_name]])
# map(.x, .f = some_number) is equivalent to map(.x, ~ .x[[some_number]])

# so:
map_chr(people, ~ .x[["hair_color"]])
# becomes:
map_chr(people, "hair_color")
map_chr(people, 4)


# .f can be a function ----------------------------------------------------

# map(.x, .f = some_function, ...) is equivalent to map(.x, ~ some_function(.x, ...))

# Pull out starships for eaach element in people
char_starships <- map(people, "starships")
char_starships
# Now get the length of each:
map(char_starships, length)
map_int(char_starships, length)
# In one go:
map(people, "starships") %>% map_int(length)
# is equivalent to:
map_int(people, ~ length(.x[["starships"]]))

# # Some challenge questions:
# Which film (see films) has the most characters?
# Create the planet_lookup vector from earlier.
# Which species has the most possible eye colors?

str(films)
# skim(films)
data.tree::FromListSimple(films)
films[1]
map(films, ~ .x[["characters"]]) %>% map(length)
map(films, "characters") %>% 
  map_int(length) %>% 
  set_names(map_chr(films, "title"))

head(planet_lookup)
map_chr(planets, ~.x[["name"]]) %>% set_names(map_chr(planets, "url"))

map_chr(species, "eye_colors") %>% str_split(",") %>% map_int(length)

# purrr and list columns --------------------------------------------------


# in people, cases are the characters, and variables are the items in the list
# can see code/star_wars-tbl.R
people_tbl <- tibble(
  name = people %>% map_chr("name"),
  films = people %>% map("films"), # returns a list!
  height = people %>% map_chr("height") %>% 
    readr::parse_number(na = "unknown"),
  species = people %>% map_chr("species", .null = NA_character_)
)
str(people_tbl$films)
people_tbl$films[1] %>% str()
people_tbl$films[[1]] %>% str()

people_tbl$films
people_tbl <- people_tbl %>% 
  mutate(film_numbers = map(films, ~ film_number_lookup[.x]),
         n_films = map_int(films, length)
  )
people_tbl
people_tbl$films
people_tbl$film_numbers

# Create a new character column that collapses the film numbers into a single string
people_tbl$film_numbers[1] %>% str()
test <- people_tbl$film_numbers[1]
str(test)
people_tbl$film_numbers[[1]]  # A vector
test[1] %>% str()  # A list
test[[1]] %>% str() # A vector
paste(test[[1]], collapse = ", ")
paste(people_tbl$film_numbers[[1]], collapse = ", ") #NEED TO DO ALL OF THIS WITH A MAP INSIDE MUTATE()

people_tbl %>% 
  mutate(film_squashed = map_chr(film_numbers, ~ paste(.x, collapse = ", ")))


# More iteration functions ------------------------------------------------

?download.file
# url, destfile
?rnorm
# n, mean?
?lm
# formula, data
?predict.lm
# object, newdata
?write.csv
# x, file


# NEISS -------------------------------------------------------------------

load("data/neiss_by_day.rda")
per_day
per_day[[1]]
plots <- map(per_day, ~ ggplot(.x, aes(trmt_date, count)) + geom_line())

?invoke_map
