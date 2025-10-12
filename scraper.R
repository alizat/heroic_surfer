suppressMessages({
    library(tidyverse)
    library(lubridate)
    library(plotly)
    library(tictoc)
    library(glue)
    library(rvest)

    options(dplyr.summarise.inform = FALSE)
})


# TODO: download all html pages first!
# TODO: scrape author names when the type is "Philosopher's Notes"
# TODO: scrape book subtitles (if available) when the type is "Philosopher's Notes"


# extract items from all html pages!
all_dfs <- list()
htm_files <- list.files('htmls/', pattern = '.htm$', full.names = TRUE)
for (f in htm_files) {
    page <- read_html(f)
    items_raw <- html_elements(page, "body > div > main > article > div > div > div > div")  # items
    items_df <- tibble(title = "", type  = "", desc  = "") %>% head(0)
    for (ir in items_raw) {
        items_df <- items_df %>% add_row(
            title = (ir %>% html_elements('a > h2') %>% html_text())[[1]],
            type  = (ir %>% html_elements('div > div') %>% html_text())[[1]],
            desc  = (ir %>% html_elements('.line-clamp-2') %>% html_text() %>% str_squish())[[1]]
        )
    }
    all_dfs[[f]] <- items_df
}

# adjust names of the lists
names(all_dfs) <- names(all_dfs) %>% str_remove('^htmls/heroic_') %>% str_remove('.htm$') %>% str_replace_all('_', ' ') %>% str_to_title()

