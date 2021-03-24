dat <- clipr::read_clip_tbl()
dat %>% tibble -> dat
dat

dat_2 <-
dat %>%
  dplyr::group_by(user_id) %>%
  filter(add = TRUE, is.na(dv)|
           !str_detect(dv, "(G|g)luten"))

dat %>%
  pivot_wider(names_from = question, values_from = dv) %>% # Reshape to wide
  filter(str_detect(`Allergies?`, "(G|g)luten", negate = TRUE) | str_detect(`food choice`, "(G|g)luten", negate = TRUE)) %>% # Filter
  pivot_longer(-user_id, names_to = "question", values_to = "dv") # Reshape back to original shape



dat2 <- dat %>% group_by(user_id) %>% summarise(question = list(question), dv = list(dv))# %>% pull(dv) %>% map(no_gluten)
  filter(map_lgl(no_gluten)

dat %>% filter(!str_detect(dv,"(G|g)luten") )
.x <- dat

no_gluten <- function(x){filter(x,!str_detect(x$dv,"(G|g)luten"))}

no_gluten(dat)

mutate(dv = dv %>% map(ID2, ~ filter(.x, ID2 != .y)))

dat2 %>%
mutate(gluten = map(dv, str_detect, pattern = "(G|g)luten"),
       gluten = !map_lgl(gluten, any)) %>%
  filter(gluten) %>% select(-gluten) %>%
  unnest(question,dv)

dat %>% group_by(user_id) %>% summarise(question = list(question), dv = list(dv)) %>%
  mutate(gluten = map(dv, str_detect, pattern = "(G|g)luten"),gluten = !map_lgl(gluten, any)) %>%
  filter(gluten) %>% select(-gluten) %>%
  unnest(c(question,dv))

dat

dat %>%
  mutate(across(where(is.character), ~
                  na_if(., "na") %>% na_if("Soy")))


sample_info = tribble(
  ~id, ~could_be_here, ~or_here,    ~or_even_in_this_one,
  1,   NA,             "not_me",    "find_me_other_stuff",
  2,   "Extra_Find_Me", NA,         "diff_stuff",
  3,   NA,              "Find_me",  NA,
  4,   NA,              "not_here", "not_here_either"
)

find_me <- function(x){
  x %>% str_detect(across(everything(), pattern = "find_me", ignore.case = TRUE))
}

sample_info %>% mutate(found = map_lgl())

list_sample <- as.list(sample = sample_info)

sample_info  %>% rowwise() %>%
  mutate(found = ~str_detect(across(everything()), pattern = "find_me", ignore_case = TRUE))) %>% pull(found)

  sample_info %>%
    rowwise %>%
    mutate(found = any(str_detect(across(everything), regex("find_me", ignore.case = TRUE))))

  library(tictoc)
  tic()
  sample_info %>%
    rowwise() %>%
    mutate(find_me = any(str_detect(across(-id), regex("find_me", ignore_case = TRUE)), na.rm = TRUE))
  toc()


  sample_info %>%
  mutate(found = map_lgl(.,~str_detect(.x, regex("find_me", ignore_case = TRUE))))

  sample_info <- unite(sample_info,new)
  sample_info$check <- grepl("find_me",sample_info$new,ignore.case=TRUE)

  tic()
  sample_info %>% unite(new, remove = FALSE) %>%
    mutate(found = str_detect(.$new, regex("find_me", ignore_case = TRUE))) %>%
    select(-new)
  toc()

  tic()
  sample_info %>%
    mutate(find_me = pmap_lgl(across(-id), ~ any(str_detect(c(...), regex("find_me", ignore_case = TRUE)), na.rm = TRUE)))
toc()

lazt %>%
  mutate(find_me = pmap_lgl(across(-id), ~ any(str_detect(c(...), regex("find_me", ignore_case = TRUE)), na.rm = TRUE)))
toc()


tibble(list_col = list(c(1, 5, 7),
                       5,
                       c(10, 10, 11))) %>%
  mutate(list_sum = sum(list_col))


tibble(list_col = list(c(1, 5, 7),
                       5,
                       c(10, 10, 11))) %>%
  mutate(list_sum = map_dbl(list_col, sum))



# Purrr on gapminder ------------------------------------------------------

# to download the data directly:
gapminder_orig <- read.csv("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder-FiveYearData.csv")
# define a copy of the original dataset that we will clean and play with
gapminder <- gapminder_orig


# The goal of this exercise is to fit a separate linear model for each continent without splitting up the data.
# Create the following data frame that has the continent, each term in the model for the continent, its
# linear model coefficient estimate, and standard error.



qwe <- gapminder %>% group_by(continent) %>% nest
qwe %>% mutate(lm = map(data,~lm(lifeExp ~ pop + gdpPercap + year, data = .x))) %>%
  mutate(tidy = map(lm,~
                      broom::tidy(.x))) %>%
  transmute(continent = continent,
            tidy = tidy) %>%
  unnest(tidy)


gapminder_list <- gapminder %>% split(gapminder$continent) %>%
  map(~sample_n(., 5))
gapminder_list
