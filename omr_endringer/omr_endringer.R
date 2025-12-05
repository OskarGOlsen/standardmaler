# Har du problem med ÆØÅ i R 4.2
Sys.setlocale("LC_ALL","no_NB.utf8")

pacman::p_load(httr, # Api calls
               tidyverse
               )


df <- GET("https://data.ssb.no/api/klass/v1/classifications/1/codes?from=2000-01-01&to=2025-01-01",
         add_headers(`accept` = 'application/json'),
         content_type('application/json'),
         encoding = "UTF-8") 


df <- RJSONIO::fromJSON(content(df, "text"),
                              nullValue = NA)

df <- do.call(rbind, sbtabell[[1]]) %>% 
  as.data.frame()

df <- df %>% 
  rename(ny_grk = parentCode,
         gammel_grk = code,
         fra_aar = validFromInRequestedRange,
         til_aar = validToInRequestedRange) %>% 
  distinct(ny_grk, gammel_grk, fra_aar, til_aar)

write.csv(df, "alle_grk_endringer.csv")

# Hvis en grunnkrets er registrert uten ny (altså den ikke har blitt endret
# .. til noe nytt) dropper vi den. Vi kan bruke oppføringen der den ble
# .. endret til det den er

df <- df %>% 
  filter(!is.na(ny_grk))

set.seed(123)

df <- df %>% 
  group_by(gammel_grk) %>% 
  slice_sample() %>%
  ungroup()
# Det hender grunnkretser deler seg, da blir det to oppføringer på 
# .. gammel grunnkrets. Jeg beholder da en tilfeldig. 


## FINNE SISTE VERSJON ##

sjekk <- setNames(df$ny_grk, df$gammel_grk)
# Knytter sammen unike ny og gammel grunnkrets

siste <- function(f) {
  while (!is.na(sjekk[f])) {
    f <- sjekk[f]
  }
  f
}


# Hovedpoenget som følger er dette:
# .. Hvis en kode som er oppført som 'ny', også på et tidspunkt er 
# .. oppført som 'gammel', så er det ikke siste kode. Da går man videre til
# .. neste, og ser om dette er siste versjon av koden. Når man kommer
# .. til en ny kode som ikke er oppført som gammel på et senere tidspunkt,
# .. så blir dette siste kode. Dette gjøres da også for alle tidligere koder
# .. slik at man ikke hopper over ledd, f.eks. dersom en kode er endret mer
# .. enn to ganger

endr <- df %>% 
  rowwise() %>% 
  mutate(siste = siste(ny_grk)) %>% 
  distinct(gammel_grk, siste)

write_csv2(endr,
           "grk_endringer_nyeste.csv")

