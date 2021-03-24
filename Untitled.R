metadata <-  structure(list(Run = c("ERR2804817", "ERR2804818", "ERR2804819",
                            "ERR2804820", "ERR2804821", "ERR2834367", "ERR2834371", "ERR2834373",
                            "ERR2834374", "ERR2834375", "ERR2834376", "ERR2834377", "ERR2834379",
                            "ERR2828323", "ERR2828326", "ERR2828327", "ERR2828328", "ERR2828330"
), LibraryLayout = c("PAIRED", "PAIRED", "PAIRED", "PAIRED",
                     "PAIRED", "PAIRED", "PAIRED", "PAIRED", "PAIRED", "PAIRED", "PAIRED",
                     "PAIRED", "PAIRED", "SINGLE", "SINGLE", "SINGLE", "SINGLE", "SINGLE"
), Library.Name = c("Bangladeshi_2yr", "Bangladeshi_2yr", "Bangladeshi_2yr",
                    "Bangladeshi_2yr", "Bangladeshi_2yr", "table S7A,B; WGS", "table S7A,B; WGS",
                    "table S7A,B; WGS", "table S7A,B; WGS", "table S7A,B; WGS", "table S7A,B; WGS",
                    "table S7A,B; WGS", "table S7A,B; WGS", "table S12", "table S12",
                    "table S12", "table S12", "table S12"), LibrarySource = c("METAGENOMIC",
                                                                              "METAGENOMIC", "METAGENOMIC", "METAGENOMIC", "METAGENOMIC", "GENOMIC",
                                                                              "GENOMIC", "GENOMIC", "GENOMIC", "GENOMIC", "GENOMIC", "GENOMIC",
                                                                              "GENOMIC", "METATRANSCRIPTOMIC", "METATRANSCRIPTOMIC", "METATRANSCRIPTOMIC",
                                                                              "METATRANSCRIPTOMIC", "METATRANSCRIPTOMIC"), Instrument = c("Illumina MiSeq",
                                                                                                                                          "Illumina MiSeq", "Illumina MiSeq", "Illumina MiSeq", "Illumina MiSeq",
                                                                                                                                          "Illumina MiSeq", "Illumina MiSeq", "Illumina MiSeq", "Illumina MiSeq",
                                                                                                                                          "Illumina MiSeq", "Illumina MiSeq", "Illumina MiSeq", "Illumina MiSeq",
                                                                                                                                          "NextSeq 500", "NextSeq 500", "NextSeq 500", "NextSeq 500", "NextSeq 500"
                                                                              )), row.names = c(1L, 2L, 3L, 4L, 5L, 73L, 74L, 75L, 76L, 77L,
                                                                                                78L, 79L, 80L, 806L, 807L, 808L, 809L, 810L), class = "data.frame")

 #Here is what I have for now
  `%notin%` = Negate(`%in%`)
  tmp = metadata %>% filter_all(any_vars(everything(), str_detect(., "shotgun|WGS|whole genome|all_genome|WXS|WholeGenomeShotgun|Whole genome shotgun|Metatranscriptomic|WXS")))
  meta = meta[meta$Run%notin%tmp$Run,]

 regex_match <-  "shotgun|WGS|whole genome|all_genome|WXS|WholeGenomeShotgun|Whole genome shotgun|Metatranscriptomic|WXS"


 metadata %>%
   rowwise() %>%
   mutate(regex_match = any(str_detect(c_across(-Run), regex(regex_match, negate = TRUE)), na.rm = FALSE))

  metadata %>%
    mutate(match = map_lgl(across(everything()),~{any(str_detect(.x$,regex_match))}))
      match = replace_na(match,TRUE))

      %>% filter(cost) %>% select(-cost)

data(msleep)
regex_match <- "omni"
metadata %>%
  rowwise() %>%
  mutate(regex_match = any(str_detect(c_across(is.character), regex(regex_match)), na.rm = TRUE)) %>%
  filter(!regex_match)

regex_match <-  "shotgun|WGS|whole genome|all_genome|WXS|WholeGenomeShotgun|Whole genome shotgun|Metatranscriptomic|WXS"
metadata %>%
  rowwise() %>%
  mutate(regex_match = any(str_detect(c_across(is.character), regex(regex_match)), na.rm = FALSE),
         regex_match = replace_na(regex_match, TRUE)) %>%
  filter(!regex_match)

regex_match <- "omni"
msleep %>%
  rowwise() %>%
  mutate(regex_match = any(str_detect(c_across(is.character), regex(regex_match)), na.rm = FALSE),
         regex_match = replace_na(regex_match, TRUE)) %>%
  filter(!regex_match)

msleep %>%
  rowwise() %>%
  mutate(regex_match = any(str_detect(c_across(is.character), regex(regex_match)), na.rm = TRUE)) %>%
  filter(!regex_match)

msleep %>%
  rowwise() %>%
  mutate(regex_match = any(str_detect(c_across(is.character), regex(regex_match)), na.rm = FALSE))
  filter(!regex_match)

  metadata %>%
    unite(new, remove = FALSE) %>%
    mutate(not_found = str_detect(.$new, regex(regex_match), negate = TRUE)) %>%
    filter(not_found) %>%
    select(-c(new,not_found))


# Combine {stringr} and mutate() to manipulate multiple columns at --------



  df <- tibble(name = c("Person_1","Person_2","Person_3"),
               `AxxBxx1:0` = c("1:04","2:02","0:1"),
               `AxxCxx5:0` = c("5:04","3:02","0:0"),
               `BxxCxx2:1` = c("2:14","1:03","0:1"))

  df_2 <- tibble(name = c("A","B","C"),
                 AxxBxx_real = "1:0",
                 AxxCxx_real = "5:0",
                 BxxCxx_real = "2:1",
                 AxxBxx_bet = c("1:0","2:0","0:1"),
                 AxxCxx_bet = c("5:0","3:0","0:0"),
                 BxxCxx_bet = c("2:1","1:0","0:1"),
                 AxxBxx_result = c("4","2",""),
                 AxxCxx_result = c("4","2",""),
                 BxxCxx_result = c("4","3",""))

qwe <- names(df[-1]) %>% str_extract("(\\d:\\d$)")
names(df[-1]) <- names(df[-1]) %>%  str_replace(pattern = qwe,"real"); names(df)

df %>% separate(across(-c(name)), col = c("bet","result"), sep = ".$",remove = FALSE)
df %>% mutate(new = str_replace(`AxxBxx1:0` ,"(\\d:\\d)(.?)", "\\1_\\2"))

qwe <- df %>% modify( ~str_replace(.x ,"(\\d:\\d)(.?)", "\\1_\\2"))# %>%
  separate(across(everything()), into = c("bet","result"), sep = "_")

sep <- function(col){
  separate(col = expr(!!col), into = c("bet","result"), sep = "_")}
}

qwe[,2] %>% pull-> .x
do.call(rbind, str_split(.x, '_'))

qwe[-1] %>% modify(~do.call(rbind, str_split(.x, '_')))

df %>% mutate(name = c("A","B","C")) %>%
  modify( ~str_replace(.x ,"(\\d:\\d)(.?)", "\\1_\\2")) %>%
  mutate(across(everything(), ~str_replace(., "_$","_0")))
  modify(~{separate(.x, c("foo", "bar"), "_")})

asd[asd == ""] <- "0"


separate(.x, c("foo", "bar"), "_")

qwe %>% mutate(across(everything(), ~str_replace(., "_$","_0")))

asd %>% separate(col = 2,  data = .,  into = c(paste0(colnames(.col),"_bet"),paste0(colnames(.col),"_result")), sep = "_")

colnames(asd[-1])-> zxc
zxc %>% length
asd %>%
  separate(col = !!sym(zxc), data = .,into = c(glue::glue("{zxc}_bet"),glue::glue("{zxc}_result")), sep = "_")


sep <- function(df,col){
  df %>%
    select(col) %>%
    separate(col = !!sym(coln), data = .,
             into = c(glue::glue("{coln}_bet"),
                      glue::glue("{coln}_result")), sep = "_",
             remove = TRUE)
}

map(.x = .x = asd, .y =zxc, .f = sep) %>% reduce(cbind)

map(asd,sep,zxc)

sep <- function(df,col){
  df %>% select(col) %>%
    separate(col = col, data = .,
             into = c(glue::glue("{col}_bet"),
                      glue::glue("{col}_result")), sep = "_",
             remove = TRUE)
}

asd %>% mutate(data = map(.,sep))

asd %>% sep(zxc[1])

sep(asd,zxc[1])

asd[-1] %>% map(zxc,1:3,.f = sep)

modify(asd, ~ sep(.x))

new <- list()
for(i in seq_along(zxc)){
  temp <- asd %>% sep(zxc[i])
  new[[i]] <- temp
  }
reduce(new,cbind)
df %>% mutate(name = c("A","B","C"))

names(df[-1]) %>% str_extract("(\\d:\\d$)") %>%
  tibble(AB = rep(.[1],3),
         AC = rep(.[2],3),
         BC = rep(.[3],3))

names(df[-1]) %>% str_extract("(\\d:\\d$)") %>% tibble(name = .) %>% t


df %>% pivot_longer(cols = -name,
                    names_to = c('col1', 'real'),
                    names_pattern = '([A-Za-z]+)(\\d+:\\d+)')  %>%
  extract(value, c('bet', 'result'), '(\\d+:.)(.)?') %>%
  pivot_wider(names_from = col1, values_from = c(real, bet, result),
              names_glue = '{col1}_{.value}')
df
