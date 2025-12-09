# Har du problem med ÆØÅ i R 4.2
Sys.setlocale("LC_ALL","no_NB.utf8")

pacman::p_load(httr, # Api calls
               tidyverse
)

# API CALL
df <- GET("https://data.ssb.no/api/klass/v1/correspondencetables/426?language=nb",
          # Henter korrespondansetabell 426. 
          add_headers(`accept` = 'application/json'),
          content_type('application/json'),
          encoding = "UTF-8") 


df <- RJSONIO::fromJSON(content(df, "text"),
                        nullValue = NA)
# Klargjør for omgjøring til data.frame

df <- do.call(rbind, df$correspondenceMaps) %>% 
  as.data.frame()
# Gjør om fra .json til data.frame

df <- df %>% 
  rename(styrk98 = sourceCode,
         styrk08 = targetCode) %>% 
  distinct(styrk98, styrk08)

write.csv2(df, 
           "styrk_98_08.csv")


