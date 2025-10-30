suppressMessages({
    library(tidyverse)
    library(lubridate)
    library(plotly)
    library(tictoc)
    library(glue)
    library(rvest)

    options(dplyr.summarise.inform = FALSE)
})


# extract items from all html pages!
all_dfs <- list()
missions_page <- read_html('http://legacy.heroic.us/missions')
missions <- missions_page %>% html_elements('main > section > div > a') %>% html_attr('href') %>% str_remove('.*/')
for (mission in missions) {
    html_link <- paste0('http://legacy.heroic.us/missions/', mission)
    print(glue('Processing link: {html_link}'))
    page <- read_html(html_link)
    items_raw <- html_elements(page, "body > div > main > article > div > div > div > div")  # items
    items_df <- tibble(title = "", subtitle = "", author = "", type  = "", desc  = "") %>% head(0)
    for (ir in items_raw) {
        title_i <- (ir %>% html_elements('a > h2')        %>% html_text())[[1]]
        type_i  <- (ir %>% html_elements('div > div')     %>% html_text() %>% setdiff('Locked'))[[1]]
        desc_i  <- (ir %>% html_elements('.line-clamp-2') %>% html_text() %>% str_squish())[[1]]
        if (type_i == "Philosopher's Notes") {
            author_i   <- ir %>% html_elements('p > span > a') %>% html_text() %>% paste(collapse = ', ')
            subtitle_i <- ir %>% html_elements('h4')           %>% html_text()
            if (length(author_i)   == 0) { author_i   <- NA }
            if (length(subtitle_i) == 0) { subtitle_i <- NA }
        } else {
            author_i   <- ""
            subtitle_i <- ""
        }
        
        items_df <- items_df %>% add_row(
            title    = title_i,
            subtitle = subtitle_i,
            author   = author_i,
            type     = type_i,
            desc     = desc_i
        )
    }
    all_dfs[[mission]] <- items_df %>% distinct()
    writeLines(as.character(page), glue("htmls/{mission}.html"))
}

# adjust names of the lists
names(all_dfs) <- names(all_dfs) %>% str_remove('.*/') %>% str_remove('.htm$') %>% str_replace_all('_', ' ') %>% str_to_title()

# add mission to each dataframe
for (n in names(all_dfs)) {
    all_dfs[[n]] <- all_dfs[[n]] %>% mutate(mission = n)
}

# combine all dataframes
all_data <- bind_rows(all_dfs) %>% select(mission, everything())

# save to csv
write_csv(all_data, 'heroic_missions_catalog.csv')
print('Scraping complete! Data saved to heroic_missions_catalog.csv')
