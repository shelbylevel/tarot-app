library(shiny)
library(shinyMobile)
library(markdown)
library(stringr)
library(dplyr)
library(purrr)
library(googlesheets4)

# Authenticate non-interactively using the Connect Cloud secret variable
gs4_auth(path = Sys.getenv("GSHEET_JSON"))
sheet_id <- Sys.getenv("GSHEET_ID")

# ==============================================================================
# 1. PARSE ALL DECK GUIDEBOOKS IN data/
# ==============================================================================
parse_guidebook <- function(file_path) {
  raw_text <- paste(
    readLines(file_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  # Split by ### (Level 3 headers represent the 78 individual cards)
  chunks <- str_split(raw_text, "(?m)^###\\s+")[[1]]
  card_chunks <- chunks[-1]

  deck_list <- map(card_chunks, function(chunk) {
    # Clean off any trailing ## Suit headers attached to the last card of a suit
    chunk <- str_split(chunk, "(?m)^##\\s+")[[1]][1]

    lines <- str_split(chunk, "\n", n = 2)[[1]]
    title <- str_trim(lines[1])
    body <- ifelse(length(lines) > 1, str_trim(lines[2]), "")

    # Split Upright vs Reversed using #### Reversed
    parts <- str_split(body, "(?m)^####\\s+")[[1]]
    upright_md <- str_trim(parts[1])
    reversed_md <- ifelse(
      length(parts) > 1,
      paste0("#### ", str_trim(parts[2])),
      ""
    )

    list(
      title = title,
      upright = upright_md,
      reversed = reversed_md
    )
  })

  names(deck_list) <- map_chr(deck_list, "title")
  deck_list
}

# "robin-wood-guidebook.md" -> "Robin Wood"
deck_name_from_file <- function(file_path) {
  basename(file_path) |>
    str_remove("\\.md$") |>
    str_remove("[-_]guidebook$") |>
    str_replace_all("[-_]", " ") |>
    str_to_title()
}

# "Robin Wood" -> "robin_wood"; "The Star" -> "the_star"
to_slug <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

md_files <- list.files("data", pattern = "\\.md$", full.names = TRUE)

decks <- map(md_files, function(f) {
  name <- deck_name_from_file(f)
  list(
    name = name,
    slug = to_slug(name),
    cards = parse_guidebook(f)
  )
})
names(decks) <- map_chr(decks, "name")
deck_choices <- names(decks)

# Serve card images: inst/img/<deck_slug>/<card_slug>.jpg
addResourcePath("card-images", "inst/img")

card_image_ui <- function(deck_slug, card_title, reversed = FALSE) {
  img_path <- file.path(
    "inst/img",
    deck_slug,
    paste0(to_slug(card_title), ".jpg")
  )
  if (file.exists(img_path)) {
    tags$img(
      src = file.path(
        "card-images",
        deck_slug,
        paste0(to_slug(card_title), ".jpg")
      ),
      alt = card_title,
      class = if (reversed) "tarot-card-img is-reversed" else "tarot-card-img"
    )
  } else {
    NULL
  }
}

# Uppercase the keyword line (first italic-only line) in a card entry
caps_keywords <- function(md_text) {
  str_replace(md_text, "(?m)^\\*([^*]+)\\*$", function(m) {
    paste0("*", str_to_upper(str_remove_all(m, "\\*")), "*")
  })
}

# Schema for the two Google Sheets tabs
empty_readings <- data.frame(
  reading_id = character(),
  date = character(),
  time = character(),
  deck = character(),
  spread_type = character(),
  question = character(),
  notes = character(),
  stringsAsFactors = FALSE
)

empty_cards <- data.frame(
  reading_id = character(),
  date = character(),
  position_num = integer(),
  position_label = character(),
  card = character(),
  orientation = character(),
  card_notes = character(),
  stringsAsFactors = FALSE
)

# Load both tabs; fall back to empty frames if a tab is missing
load_readings <- function() {
  df <- tryCatch(
    read_sheet(sheet_id, sheet = "readings", col_types = "ccccccc"),
    error = function(e) empty_readings
  )
  if (nrow(df) == 0) empty_readings else as.data.frame(df)
}

load_cards <- function() {
  df <- tryCatch(
    read_sheet(sheet_id, sheet = "cards", col_types = "ccicccc"),
    error = function(e) empty_cards
  )
  if (nrow(df) == 0) empty_cards else as.data.frame(df)
}

# ==============================================================================
# 2. iOS-NATIVE UI (shinyMobile / Framework7)
# ==============================================================================
ui <- f7Page(
  title = "Tarot Journal",
  options = list(
    theme = "ios",
    dark = TRUE,
    color = "#D4AF37",
    filled = FALSE
  ),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Cinzel+Decorative:wght@400;600&display=swap"
    ),
    tags$link(rel = "stylesheet", href = "tarot.css")
  ),
  f7TabLayout(
    navbar = f7Navbar(
      title = span(class = "tarot-serif", "Tarot Journal"),
      hairline = TRUE,
      transparent = FALSE
    ),
    f7Tabs(
      id = "tabs",
      animated = FALSE,
      swipeable = FALSE,

      # --- TAB 1: LOG A MANUAL READING ---
      f7Tab(
        tabName = "NewReading",
        icon = f7Icon("square_stack_3d_up"),

        f7Block(
          strong = TRUE,
          inset = TRUE,
          div(class = "section-title", "1. Spread Setup"),
          f7Select(
            "deck_name",
            "Deck Used:",
            choices = deck_choices
          ),
          f7Text(
            "spread_type",
            "Spread Type (Optional):",
            placeholder = "e.g., Three Card, Celtic Cross"
          ),
          f7TextArea(
            "reading_question",
            "Question or Intention:",
            placeholder = "What do I need to see right now?"
          )
        ),

        f7Block(
          strong = TRUE,
          inset = TRUE,
          div(class = "section-title", "2. Add Cards Drawn"),
          selectInput(
            "pick_card",
            "Card Drawn:",
            choices = NULL,
            width = "100%"
          ),
          f7Text(
            "pick_pos",
            "Position (Optional):",
            placeholder = "e.g., Past / Situation"
          ),
          radioButtons(
            "pick_orient",
            "Orientation:",
            choices = c("Upright", "Reversed"),
            inline = TRUE
          ),
          f7TextArea(
            "pick_notes",
            "Notes for This Card (Optional):",
            placeholder = "First impressions, imagery that stood out..."
          ),
          br(),
          f7Button(
            "add_card_btn",
            "Add Card to Spread",
            fill = TRUE,
            rounded = TRUE
          )
        ),

        # Step 3: Live Spread Meanings
        uiOutput("active_spread_ui"),

        # Step 4: Reflection & Save
        uiOutput("save_panel_ui"),
        br()
      ),

      # --- TAB 2: PAST READINGS LOG ---
      f7Tab(
        tabName = "History",
        icon = f7Icon("clock"),
        uiOutput("history_ui"),
        br()
      ),

      # --- TAB 3: QUICK GUIDEBOOK LOOKUP ---
      f7Tab(
        tabName = "Guidebook",
        icon = f7Icon("book"),
        f7Block(
          strong = TRUE,
          inset = TRUE,
          f7Select(
            "lookup_deck",
            "Deck:",
            choices = deck_choices
          ),
          selectInput(
            "lookup_card",
            "Browse Any Card:",
            choices = NULL,
            width = "100%"
          )
        ),
        f7Block(
          strong = TRUE,
          inset = TRUE,
          uiOutput("lookup_display")
        ),
        br()
      )
    )
  )
)

# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  # Reactive list storing cards added to the current spread
  current_spread <- reactiveVal(list())

  # Reactive triggers for refreshing history
  saved_readings <- reactiveVal(load_readings())
  saved_cards <- reactiveVal(load_cards())

  # Currently selected deck for the New Reading tab
  reading_deck <- reactive({
    req(input$deck_name)
    decks[[input$deck_name]]
  })

  # Currently selected deck for the Guidebook tab
  lookup_deck <- reactive({
    req(input$lookup_deck)
    decks[[input$lookup_deck]]
  })

  # Keep card dropdowns in sync with the selected deck
  observeEvent(input$deck_name, {
    updateSelectInput(
      session,
      "pick_card",
      choices = names(reading_deck()$cards)
    )
  })

  observeEvent(input$lookup_deck, {
    updateSelectInput(
      session,
      "lookup_card",
      choices = names(lookup_deck()$cards)
    )
  })

  # Add card to current spread
  observeEvent(input$add_card_btn, {
    req(input$pick_card)
    spread <- current_spread()

    pos_label <- ifelse(
      str_trim(input$pick_pos) == "",
      paste("Card", length(spread) + 1),
      str_trim(input$pick_pos)
    )

    new_entry <- list(
      position = pos_label,
      card = input$pick_card,
      orientation = input$pick_orient,
      notes = str_trim(input$pick_notes),
      deck = input$deck_name
    )

    current_spread(append(spread, list(new_entry)))
    updateF7Text("pick_pos", value = "", session = session) # Reset position box for next card
    updateF7TextArea("pick_notes", value = "", session = session)
  })

  # Remove a single card from the spread
  observeEvent(input$remove_card_idx, {
    spread <- current_spread()
    idx <- input$remove_card_idx
    if (idx >= 1 && idx <= length(spread)) {
      current_spread(spread[-idx])
    }
  })

  # Clear spread
  observeEvent(input$clear_spread_btn, {
    current_spread(list())
    updateF7TextArea("reading_question", value = "", session = session)
    updateF7TextArea("reading_notes", value = "", session = session)
  })

  # Render the active spread with full guidebook narratives
  output$active_spread_ui <- renderUI({
    spread <- current_spread()
    if (length(spread) == 0) {
      return(NULL)
    }

    cards_ui <- imap(spread, function(item, idx) {
      deck <- decks[[item$deck]]
      card_data <- deck$cards[[item$card]]
      req(card_data)

      # Show only the meaning matching the drawn orientation
      md_text <- if (
        item$orientation == "Reversed" && card_data$reversed != ""
      ) {
        # Drop the "#### Reversed" header so only keywords + meaning show
        str_remove(card_data$reversed, "^####\\s+Reversed\\s*\n+")
      } else {
        card_data$upright
      }

      md_text <- caps_keywords(md_text)

      badge_class <- ifelse(
        item$orientation == "Upright",
        "badge-upright",
        "badge-reversed"
      )

      f7Card(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          tags$span(tags$strong(item$position), " — ", item$card),
          div(
            style = "display: flex; gap: 8px; align-items: center;",
            tags$span(class = badge_class, item$orientation),
            tags$a(
              class = "remove-card-btn",
              style = "cursor: pointer; color: var(--f7-color-red, #ff3b30);",
              onclick = sprintf(
                "Shiny.setInputValue('remove_card_idx', %d, {priority: 'event'})",
                idx
              ),
              f7Icon("xmark_circle_fill")
            )
          )
        ),
        hr(),
        div(
          style = "display: flex; gap: 12px; align-items: flex-start;",
          div(
            style = "flex: 0 0 35%;",
            card_image_ui(
              deck$slug,
              item$card,
              reversed = item$orientation == "Reversed"
            )
          ),
          div(
            style = "flex: 1; font-size: 14px;",
            HTML(markdownToHTML(text = md_text, fragment.only = TRUE))
          )
        ),
        if (!is.null(item$notes) && item$notes != "") {
          div(
            class = "notes-box",
            tags$strong("Your note: "),
            item$notes
          )
        }
      )
    })

    tagList(
      div(
        class = "section-title",
        style = "padding: 0 16px;",
        "Spread Meanings"
      ),
      cards_ui
    )
  })

  # Reflection & save panel, shown only when the spread has cards
  output$save_panel_ui <- renderUI({
    if (length(current_spread()) == 0) {
      return(NULL)
    }
    f7Block(
      strong = TRUE,
      inset = TRUE,
      div(class = "section-title", "3. Final Reflections & Save"),
      f7TextArea(
        "reading_notes",
        "Your Gut Takeaway:",
        placeholder = "What does your gut say when looking at them together?"
      ),
      br(),
      f7Segment(
        rounded = TRUE,
        f7Button(
          "clear_spread_btn",
          "Clear Spread",
          outline = TRUE,
          fill = FALSE
        ),
        f7Button(
          "save_reading_btn",
          "Save to Log",
          color = "green",
          fill = TRUE
        )
      )
    )
  })

  # Save reading to CSV
  observeEvent(input$save_reading_btn, {
    spread <- current_spread()
    req(length(spread) > 0)

    now <- Sys.time()
    reading_id <- format(now, "%Y%m%d-%H%M%S")

    new_reading <- data.frame(
      reading_id = reading_id,
      date = format(now, "%Y-%m-%d"),
      time = format(now, "%I:%M %p"),
      deck = input$deck_name,
      spread_type = str_trim(input$spread_type),
      question = ifelse(
        str_trim(input$reading_question) == "",
        "General Pull",
        input$reading_question
      ),
      notes = input$reading_notes,
      stringsAsFactors = FALSE
    )

    new_cards <- bind_rows(imap(spread, function(x, i) {
      data.frame(
        reading_id = reading_id,
        date = format(now, "%Y-%m-%d"),
        position_num = i,
        position_label = x$position,
        card = x$card,
        orientation = x$orientation,
        card_notes = x$notes,
        stringsAsFactors = FALSE
      )
    }))

    sheet_append(sheet_id, new_reading, sheet = "readings")
    sheet_append(sheet_id, new_cards, sheet = "cards")

    # Prepend locally so newest readings display first
    saved_readings(bind_rows(new_reading, saved_readings()))
    saved_cards(bind_rows(saved_cards(), new_cards))

    # Reset spread & notify user
    current_spread(list())
    updateF7TextArea("reading_question", value = "", session = session)
    updateF7TextArea("reading_notes", value = "", session = session)
    f7Toast(
      text = "Reading saved to your log!",
      position = "bottom",
      session = session
    )
  })

  # Render History Tab
  output$history_ui <- renderUI({
    readings <- saved_readings()
    cards <- saved_cards()
    if (nrow(readings) == 0) {
      return(f7Block(
        strong = TRUE,
        inset = TRUE,
        p("No readings logged yet. Pull some cards and save your first spread!")
      ))
    }

    map(seq_len(nrow(readings)), function(i) {
      row <- readings[i, ]
      reading_cards <- cards |>
        filter(reading_id == row$reading_id) |>
        arrange(position_num)

      card_items <- pmap_chr(
        reading_cards,
        function(
          position_label,
          card,
          orientation,
          card_notes,
          ...
        ) {
          item <- paste0(position_label, ": ", card, " (", orientation, ")")
          if (!is.na(card_notes) && str_trim(card_notes) != "") {
            item <- paste0(item, " — ", card_notes)
          }
          item
        }
      )

      f7Card(
        div(
          style = "display: flex; justify-content: space-between; align-items: baseline;",
          tags$strong(row$question),
          tags$span(class = "muted", paste(row$date, row$time))
        ),
        tags$p(
          class = "muted",
          f7Icon("shippingbox"),
          " Deck: ",
          row$deck,
          if (!is.na(row$spread_type) && str_trim(row$spread_type) != "") {
            paste0(" · ", row$spread_type)
          }
        ),
        tags$ul(
          style = "margin: 4px 0 8px; padding-left: 20px; font-size: 14px;",
          map(card_items, tags$li)
        ),
        if (!is.na(row$notes) && str_trim(row$notes) != "") {
          tags$div(
            class = "notes-box",
            tags$strong("Notes: "),
            row$notes
          )
        }
      )
    })
  })

  # Render Standalone Guidebook Lookup
  output$lookup_display <- renderUI({
    req(input$lookup_card)
    deck <- lookup_deck()
    card_data <- deck$cards[[input$lookup_card]]
    req(card_data)

    md_text <- paste0("### ", card_data$title, "\n\n", card_data$upright)
    if (card_data$reversed != "") {
      md_text <- paste0(md_text, "\n\n---\n\n", card_data$reversed)
    }
    md_text <- caps_keywords(md_text)

    tagList(
      div(
        style = "text-align: center; margin-bottom: 12px;",
        card_image_ui(deck$slug, input$lookup_card)
      ),
      HTML(markdownToHTML(text = md_text, fragment.only = TRUE))
    )
  })
}

shinyApp(ui, server)
