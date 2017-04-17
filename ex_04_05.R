library(purrr)
library(dplyr)
library(stringr)

hamlet = readLines("/data/Shakespeare/hamlet.txt") %>% .[.!=""]

# map step

map_count_words = function(line)
{
  line %>%
    tolower() %>%
    str_replace_all("[^a-z ]", "") %>%
    str_split(" ") %>%
    .[[1]] %>%
    table() %>%
    as.list()
}

map_res = lapply(hamlet, map_count_words)

# shuffle step

map_res_flat = flatten(map_res)

keys = names(map_res_flat) %>% unique()
shuf_res = lapply(
  keys, 
  function(key)  
    map_res_flat[names(map_res_flat) == key] %>% unlist() %>% setNames(NULL)
) %>% 
  setNames(keys)


## Reduce step

reduce_res = lapply(shuf_res, sum)


## Make data frame

d = data.frame(keys = names(reduce_res), values = unlist(reduce_res))
View(d)


# Why 16 the's?

n_thes = map(map_res, "the") %>% map_dbl( ~if(length(.) > 0) {.} else { 0 } ) 
which.max(n_thes)

n_thes[1936]
hamlet[1936]




## K Means

set.seed(123)
obs = c(rnorm(30, -1, 0.3), rnorm(30, 0, 0.3), rnorm(30, 1.5, 0.3))
lbl = sample(1:3, length(obs), replace=TRUE)
ctr = list(-0.3, 0, 0.3) %>% setNames(1:3)

res = list()
for(i in 1:10) {
  map_func = function(obs, ctr)
  {
    label = which.min(abs(obs-unlist(ctr)))
    
    list(obs) %>% setNames(label)
  }
  
  map_res = lapply(obs, map_func, ctr=ctr)
  map_res_flat = flatten(map_res)
  
  keys = as.character(1:3)
  shuf_res = lapply(
    keys, 
    function(key)  
      map_res_flat[names(map_res_flat) == key] %>% unlist() %>% setNames(NULL)
  ) %>% 
    setNames(keys)
  
  reduce_res = lapply(shuf_res, mean)
  
  ctr = reduce_res
  res[[i]] = ctr
}

bind_rows(res)

