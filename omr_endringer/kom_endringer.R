# Har du problem med ÆØÅ i R 4.2
Sys.setlocale("LC_ALL","no_NB.utf8")

pacman::p_load(httr, # Api calls
               tidyverse
)

# API CALL
df <- GET("https://data.ssb.no/api/klass/v1/classifications/131/changes?from=2000-01-01&to=2025-01-01",
          # Her kan man endre hvilke år man ønsker fra/til. 
          # .. under from=xxxx-01-01 og to=xxxx-01-01
          add_headers(`accept` = 'application/json'),
          content_type('application/json'),
          encoding = "UTF-8") 


df <- RJSONIO::fromJSON(content(df, "text"),
                        nullValue = NA)
# Klargjør for omgjøring til data.frame

df <- do.call(rbind, df[[1]]) %>% 
  as.data.frame()
# Gjør om fra .json til data.frame

df <- df %>% 
  rename(ny_kom = newCode,
         gammel_kom = oldCode,
         endringsdato = changeOccurred) %>% 
  # Gir nye variabelnavn
  mutate(grk_endringsaar = substr(endringsdato, 1, 4)) %>% 
  # Lager endringsvariabel (år, dato er likt)
  distinct(ny_kom, gammel_kom, grk_endringsaar) 
# Beholder unike obs. på disse tre variablene

# Hvis en grunnkrets er registrert uten ny (altså den ikke har blitt endret
# .. til noe nytt) dropper vi den. Vi kan bruke oppføringen der den ble
# .. endret til det den er

df <- df %>% 
  filter(gammel_kom != ny_kom)
# Dropper hvis grunnkretsen ikke endres. Grunnkretser som 'peker' til seg
# .. til seg selv vil skape trøbbel senere

set.seed(123)

df <- df %>% 
  group_by(gammel_kom) %>% 
  slice_sample() %>%
  ungroup()
# Det hender grunnkretser deler seg, da blir det to oppføringer på 
# .. gammel grunnkrets. Jeg beholder da en tilfeldig. 

## FINNE SISTE VERSJON ##

sjekk <- setNames(df$ny_kom, df$gammel_kom)
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


kom_endr <- df %>% 
  rowwise() %>% 
  mutate(siste = siste(ny_kom)) %>% 
  distinct(gammel_kom, siste)

write_csv2(kom_endr,
           "kom_endringer_nyeste.csv")

