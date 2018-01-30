library(tidyverse)
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
people <- people %>% set_names(map_chr(people, "name"))
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
