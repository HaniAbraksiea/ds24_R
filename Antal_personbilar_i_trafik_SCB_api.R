# Ladda nödvändiga paket
if (!require(httr)) install.packages("httr")
if (!require(jsonlite)) install.packages("jsonlite")

library(httr)
library(jsonlite)

# Skapa query enligt SCB:s JSON-struktur
query <- list(
  query = list(
    list(
      code = "Fordonsslag",
      selection = list(
        filter = "item",
        values = list("PERS")
      )
    ),
    list(
      code = "Bestand",
      selection = list(
        filter = "item",
        values = list("ITRAF")
      )
    ),
    list(
      code = "Tid",
      selection = list(
        filter = "item",
        values = list(
          "2024M01", "2024M02", "2024M03", "2024M04", "2024M05", "2024M06",
          "2024M07", "2024M08", "2024M09", "2024M10", "2024M11", "2024M12",
          "2025M01", "2025M02", "2025M03"
        )
      )
    )
  ),
  response = list(format = "json")
)

# Konvertera till JSON
json_body <- toJSON(query, auto_unbox = TRUE)

# Skicka till SCB:s API
res <- POST(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/Fordon",
  body = json_body,
  encode = "json",
  content_type_json()
)

# Kontrollera svar
stop_for_status(res)

# Läs ut innehåll
data_raw <- content(res, as = "text")
data_parsed <- fromJSON(data_raw)

# Omvandla till data.frame
df <- as.data.frame(data_parsed$data)
print(df)

library(ggplot2)
library(lubridate)

# Steg 1: Extrahera månadskod och antal
df_split <- df
df_split$MånadKod <- sub(".*,.*,\\s*", "", df_split$key)
df_split$Antal <- as.numeric(df_split$values)

# Steg 2: Konvertera till datum (för sortering) och formatera
df_split$Datum <- ym(df_split$MånadKod)  # Från lubridate: ger t.ex. 2024-01-01
df_split$Månad <- format(df_split$Datum, "%b %Y")  # T.ex. "Jan 2024"

# Sortera efter datum så att plotten blir korrekt
df_split <- df_split[order(df_split$Datum), ]

# Sätt faktornivåer i korrekt ordning
df_split$Månad <- factor(df_split$Månad, levels = unique(df_split$Månad))

# Steg 3: Plotta
ggplot(df_split, aes(x = Månad, y = Antal, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Antal personbilar i trafik (SCB)",
    x = "Månad",
    y = "Antal bilar"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

