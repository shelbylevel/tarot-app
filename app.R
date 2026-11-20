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

    # Split on #### headers and match sections by name, dropping header lines
    parts <- str_split(body, "(?m)^####\\s+")[[1]]
    parts <- str_trim(parts[nzchar(str_trim(parts))])
    upright_md <- str_trim(str_remove(
      parts[str_detect(parts, regex("^upright", ignore_case = TRUE))][1],
      regex("^upright\\s*\n*", ignore_case = TRUE)
    ))
    reversed_parts <- parts[
      str_detect(parts, regex("^reversed", ignore_case = TRUE))
    ]
    reversed_md <- ifelse(
      length(reversed_parts) > 0,
      str_trim(str_remove(
        reversed_parts[1],
        regex("^reversed\\s*\n*", ignore_case = TRUE)
      )),
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

# Group a deck's card titles into Major Arcana and the four suits
suit_groups <- function(card_titles) {
  suits <- c("Wands", "Cups", "Swords", "Pentacles")
  groups <- map(suits, function(s) {
    card_titles[str_detect(card_titles, paste0(s, "$"))]
  })
  names(groups) <- suits
  majors <- card_titles[!card_titles %in% unlist(groups)]
  c(list("Major Arcana" = majors), groups)
}

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
# 1b. SPREAD DEFINITIONS (data/spreads.csv)
# ==============================================================================
# Columns: name, description, |-separated positions, and a |-separated layout
# of "col;row" grid cells (trailing "r" = rotated 90°, e.g. a crossing card)
spreads <- read.csv("data/spreads.csv", stringsAsFactors = FALSE)

parse_layout <- function(layout_str) {
  cells <- str_split(layout_str, "\\|")[[1]]
  bind_rows(lapply(cells, function(cell) {
    rotated <- str_detect(cell, "r$")
    xy <- as.integer(str_split(str_remove(cell, "r$"), ";")[[1]])
    data.frame(col = xy[1], row = xy[2], rotated = rotated)
  }))
}

# Mini card-position diagram for a spread, using .mini-card-art styling.
# `current` highlights that position index with a gold fill; indices below
# `current` get a softer gold outline to show they're already filled.
# `labels` (optional, one per position) adds a hover/tap tooltip with the
# drawn card's name.
spread_layout_ui <- function(
  spread,
  symbol = "✦",
  current = NULL,
  labels = NULL
) {
  cells <- parse_layout(spread$layout)
  div(
    class = "spread-layout",
    style = sprintf(
      "grid-template-columns: repeat(%d, 26px); grid-template-rows: repeat(%d, 40px);",
      max(cells$col),
      max(cells$row)
    ),
    lapply(seq_len(nrow(cells)), function(i) {
      has_label <- !is.null(labels) && i <= length(labels) && !is.na(labels[i])
      div(
        class = paste(
          "mini-card-art",
          if (cells$rotated[i]) "is-crossed" else "",
          if (!is.null(current) && i == current) "is-current" else "",
          if (!is.null(current) && i < current) "is-filled" else "",
          if (has_label) "has-tip" else ""
        ),
        `data-card` = if (has_label) labels[i],
        style = sprintf(
          "grid-column: %d; grid-row: %d;",
          cells$col[i],
          cells$row[i]
        ),
        symbol
      )
    })
  )
}

# ==============================================================================
# 2. iOS-NATIVE UI (shinyMobile / Framework7)
# ==============================================================================
ui <- f7Page(
  allowPWA = TRUE,
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
    tags$link(rel = "stylesheet", href = "tarot.css"),
    # iOS home-screen support
    tags$link(rel = "apple-touch-icon", href = "icons/apple-touch-icon.png"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(
      name = "apple-mobile-web-app-status-bar-style",
      content = "black-translucent"
    ),
    tags$meta(name = "apple-mobile-web-app-title", content = "Tarot"),
    tags$script(HTML(
      "$(document).on('click', '.suit-chip', function() {
        var chip = $(this);
        chip.siblings('.suit-chip').removeClass('active');
        chip.addClass('active');
        Shiny.setInputValue('suit_filter', {
          suit: chip.data('suit'),
          sheet: chip.parent().data('sheet'),
          nonce: Math.random()
        }, {priority: 'event'});
      });
      $(document).on('click', '.orient-chip', function() {
        var chip = $(this);
        chip.siblings('.orient-chip').removeClass('active');
        chip.addClass('active');
        Shiny.setInputValue('pick_orient', chip.data('orient'), {priority: 'event'});
      });"
    ))
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
        title = "New Reading",
        tabName = "NewReading",
        icon = f7Icon("wand_stars_inverse"),

        f7Block(
          strong = TRUE,
          inset = TRUE,
          div(class = "section-title", "1. Spread Setup"),
          f7Select(
            "deck_name",
            "Select a Deck",
            choices = deck_choices
          ),
          f7Text(
            "spread_type",
            "Spread Type",
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
          uiOutput("spread_template_ui"),
          f7Text(
            "pick_pos",
            "Position",
            placeholder = "e.g., Past / Situation"
          ),
          div(
            class = "chip-row",
            `data-sheet` = "pick_card_sheet",
            span(
              class = "tarot-chip suit-chip",
              `data-suit` = "Major Arcana",
              f7Icon("sparkles"),
              "Major Arcana"
            ),
            span(
              class = "tarot-chip suit-chip",
              `data-suit` = "Wands",
              f7Icon("flame"),
              "Wands"
            ),
            span(
              class = "tarot-chip suit-chip",
              `data-suit` = "Cups",
              f7Icon("drop"),
              "Cups"
            ),
            span(
              class = "tarot-chip suit-chip",
              `data-suit` = "Swords",
              f7Icon("wind"),
              "Swords"
            ),
            span(
              class = "tarot-chip suit-chip",
              `data-suit` = "Pentacles",
              f7Icon("tree"),
              "Pentacles"
            )
          ),
          uiOutput("pick_card_selected"),
          div(
            class = "chip-row orient-chips",
            span(
              class = "tarot-chip orient-chip active",
              `data-orient` = "Upright",
              f7Icon("arrow_up"),
              "Upright"
            ),
            span(
              class = "tarot-chip orient-chip",
              `data-orient` = "Reversed",
              f7Icon("arrow_down"),
              "Reversed"
            )
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

      # --- TAB 2: SPREAD GALLERY ---
      f7Tab(
        title = "Spread Gallery",
        tabName = "Spreads",
        icon = f7Icon("rectangle_on_rectangle_angled"),
        f7Block(
          strong = TRUE,
          inset = TRUE,
          div(class = "section-title", "Spread Library"),
          p(
            class = "muted",
            "Tap a spread to see its positions. Edit data/spreads.csv to add your own."
          )
        ),
        uiOutput("spread_gallery"),
        br()
      ),

      # --- TAB 3: PAST READINGS LOG ---
      f7Tab(
        title = "Log",
        tabName = "Log",
        icon = f7Icon("bookmark"),
        uiOutput("log_ui"),
        br()
      ),

      # --- TAB 4: QUICK GUIDEBOOK LOOKUP ---
      f7Tab(
        title = "Guidebook",
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
          div(
            class = "chip-row",
            `data-sheet` = "lookup_card_sheet",
            span(
              class = "tarot-chip suit-chip",
              `data-suit` = "Major Arcana",
              f7Icon("sparkles"),
              "Major Arcana"
            ),
            span(
              class = "tarot-chip suit-chip",
              `data-suit` = "Wands",
              f7Icon("flame"),
              "Wands"
            ),
            span(
              class = "tarot-chip suit-chip",
              `data-suit` = "Cups",
              f7Icon("drop"),
              "Cups"
            ),
            span(
              class = "tarot-chip suit-chip",
              `data-suit` = "Swords",
              f7Icon("wind"),
              "Swords"
            ),
            span(
              class = "tarot-chip suit-chip",
              `data-suit` = "Pentacles",
              f7Icon("tree"),
              "Pentacles"
            )
          ),
        ),
        f7Block(
          strong = TRUE,
          inset = TRUE,
          uiOutput("lookup_display")
        ),
        br()
      )
    )
  ),

  f7Sheet(
    id = "spread_sheet",
    swipeToClose = TRUE,
    backdrop = TRUE,
    uiOutput("spread_detail_ui")
  ),

  # Bottom sheet card pickers, one per tab that uses suit chips
  f7Sheet(
    id = "pick_card_sheet",
    swipeToClose = TRUE,
    backdrop = TRUE,
    uiOutput("pick_card_sheet_ui")
  ),
  f7Sheet(
    id = "lookup_card_sheet",
    swipeToClose = TRUE,
    backdrop = TRUE,
    uiOutput("lookup_card_sheet_ui")
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

  # Active suit filter, driven by the suit chips (none selected initially).
  # Clicking a chip also opens the bottom sheet named by the chip row's
  # data-sheet attribute.
  active_suit <- reactiveVal(NULL)
  observeEvent(input$suit_filter, {
    active_suit(input$suit_filter$suit)
    updateF7Sheet(id = input$suit_filter$sheet, session = session)
  })

  # Scrollable card list inside a picker sheet, styled like the old dropdown
  card_sheet_ui <- function(suit, cards, inputId, selected) {
    items <- if (length(cards) == 0) {
      div(class = "sheet-card-item disabled", "No cards")
    } else {
      lapply(cards, function(card) {
        is_curr <- identical(selected, card)
        div(
          class = paste("sheet-card-item", if (is_curr) "is-selected" else ""),
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
            inputId,
            gsub("'", "\\\\'", card)
          ),
          span(card),
          if (is_curr) {
            span(class = "check-mark", style = "color: var(--gold);", "✦")
          }
        )
      })
    }

    div(
      style = "padding: 12px 16px 20px;",
      div(class = "sheet-handle"),
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
        h3(
          class = "tarot-serif",
          style = "margin: 0; font-size: 20px;",
          suit
        ),
        span(style = "font-size: 12px; opacity: 0.6;", "Tap card to select")
      ),
      div(class = "sheet-card-list", items)
    )
  }

  output$pick_card_sheet_ui <- renderUI({
    req(active_suit())
    cards <- suit_groups(names(reading_deck()$cards))[[active_suit()]]
    card_sheet_ui(active_suit(), cards, "pick_card", input$pick_card)
  })

  output$lookup_card_sheet_ui <- renderUI({
    req(active_suit())
    cards <- suit_groups(names(lookup_deck()$cards))[[active_suit()]]
    card_sheet_ui(active_suit(), cards, "lookup_card", input$lookup_card)
  })

  # Close the picker sheets once a card is chosen
  observeEvent(input$pick_card, {
    updateF7Sheet(id = "pick_card_sheet", session = session)
  })
  observeEvent(input$lookup_card, {
    updateF7Sheet(id = "lookup_card_sheet", session = session)
  })

  # Show the currently picked card since there is no select box label anymore
  output$pick_card_selected <- renderUI({
    if (is.null(input$pick_card)) {
      return(NULL)
    }
    div(
      class = "muted",
      style = "margin: 4px 0 8px;",
      paste("Card Drawn:", input$pick_card)
    )
  })

  # Card orientation, driven by the orientation chips (Upright by default)
  pick_orientation <- reactiveVal("Upright")
  observeEvent(input$pick_orient, {
    pick_orientation(input$pick_orient)
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
      orientation = pick_orientation(),
      notes = str_trim(input$pick_notes),
      deck = input$deck_name
    )

    current_spread(append(spread, list(new_entry)))

    # Advance to the next position label when a spread template is active
    # (current_spread() already includes the card just added)
    positions <- active_positions()
    next_pos <- ""
    if (!is.null(positions)) {
      next_idx <- length(current_spread()) + 1
      if (next_idx <= length(positions)) {
        next_pos <- positions[next_idx]
      } else {
        active_positions(NULL) # template exhausted
        active_spread_name(NULL)
      }
    }
    updateF7Text("pick_pos", value = next_pos, session = session)
    updateF7TextArea("pick_notes", value = "", session = session)
  })

  # Remove a single card from the spread
  observeEvent(input$remove_card_idx, {
    spread <- current_spread()
    idx <- input$remove_card_idx
    if (idx >= 1 && idx <= length(spread)) {
      current_spread(spread[-idx])
    }

    # Rewind the prefilled position label to the new next position
    positions <- active_positions()
    if (!is.null(positions)) {
      next_idx <- length(current_spread()) + 1
      updateF7Text(
        "pick_pos",
        value = if (next_idx <= length(positions)) positions[next_idx] else "",
        session = session
      )
    }
  })

  # Clear spread
  observeEvent(input$clear_spread_btn, {
    current_spread(list())
    active_positions(NULL)
    active_spread_name(NULL)
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
        card_data$reversed
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
            tags$span(
              class = badge_class,
              f7Icon(
                if (item$orientation == "Upright") "arrow_up" else "arrow_down"
              ),
              item$orientation
            ),
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
  output$log_ui <- renderUI({
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

      # Show a diagram with filled positions for every reading: the template
      # layout when the spread type is known, otherwise a generic row of N
      n_drawn <- nrow(reading_cards)
      sp_match <- spreads[spreads$name == str_trim(row$spread_type), ]
      sp_diagram <- if (nrow(sp_match) == 1) {
        sp_match
      } else if (n_drawn > 0) {
        data.frame(
          name = row$spread_type,
          description = "",
          positions = paste(reading_cards$position_label, collapse = "|"),
          layout = paste(sprintf("%d;1", seq_len(n_drawn)), collapse = "|"),
          stringsAsFactors = FALSE
        )
      }
      diagram <- if (!is.null(sp_diagram)) {
        spread_layout_ui(
          sp_diagram,
          current = n_drawn + 1,
          labels = reading_cards$card
        )
      }

      f7Card(
        div(
          style = "display: flex; justify-content: space-between; align-items: baseline;",
          tags$strong(row$question),
          tags$span(class = "muted", paste(row$date, row$time))
        ),
        diagram,
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

  # Spread gallery: one card per spread type, tap to open detail sheet
  output$spread_gallery <- renderUI({
    lapply(seq_len(nrow(spreads)), function(i) {
      sp <- spreads[i, ]
      f7Card(
        class = "spread-gallery-card",
        onclick = sprintf(
          "Shiny.setInputValue('select_spread', %d, {priority: 'event'})",
          i
        ),
        div(
          class = "spread-card-inner",
          div(
            h4(class = "tarot-serif", sp$name),
            p(
              class = "muted",
              style = "margin:2px 0 0 0;",
              sp$description
            )
          ),
          spread_layout_ui(sp)
        )
      )
    })
  })

  selected_spread <- reactiveVal(NULL)
  observeEvent(input$select_spread, {
    selected_spread(spreads[input$select_spread, ])
    updateF7Sheet(id = "spread_sheet", session = session)
  })

  output$spread_detail_ui <- renderUI({
    sp <- selected_spread()
    req(sp)
    positions <- str_split(sp$positions, "\\|")[[1]]

    div(
      style = "padding: 16px; text-align: center;",
      h2(
        class = "tarot-serif",
        style = "margin:8px 0 0 0; font-size:28px;",
        sp$name
      ),
      p(
        class = "muted",
        paste(length(positions), "cards ·", sp$description)
      ),
      spread_layout_ui(sp),
      tags$ul(
        class = "position-list",
        lapply(seq_along(positions), function(i) {
          tags$li(
            span(class = "pos-num", i),
            span(positions[i])
          )
        })
      ),
      br(),
      f7Button(
        "use_spread_btn",
        "Use This Spread",
        fill = TRUE,
        rounded = TRUE
      )
    )
  })

  # Position labels of the spread being logged (NULL = free-form positions)
  active_positions <- reactiveVal(NULL)
  active_spread_name <- reactiveVal(NULL)

  # Mini diagram + progress indicator while a spread template is active
  output$spread_template_ui <- renderUI({
    positions <- active_positions()
    req(positions)
    sp <- spreads[spreads$name == active_spread_name(), ]
    next_idx <- min(length(current_spread()) + 1, length(positions))

    div(
      style = "display: flex; align-items: center; gap: 14px; margin-bottom: 10px;",
      if (nrow(sp) == 1) {
        spread_layout_ui(
          sp,
          current = if (next_idx <= length(positions)) next_idx
        )
      },
      div(
        div(class = "tarot-serif", style = "font-size: 15px;", sp$name),
        div(
          class = "muted",
          sprintf("Position %d of %d", next_idx, length(positions))
        )
      )
    )
  })

  # "Use This Spread": prefill New Reading tab and jump to it
  observeEvent(input$use_spread_btn, {
    sp <- selected_spread()
    req(sp)
    positions <- str_split(sp$positions, "\\|")[[1]]

    active_positions(positions)
    active_spread_name(sp$name)
    updateF7Text("spread_type", value = sp$name, session = session)
    updateF7Text("pick_pos", value = positions[1], session = session)
    updateF7Sheet(id = "spread_sheet", session = session)
    updateF7Tabs(id = "tabs", selected = "NewReading", session = session)
    f7Toast(
      text = paste("Spread loaded:", sp$name),
      position = "bottom",
      session = session
    )
  })

  # Render Standalone Guidebook Lookup
  output$lookup_display <- renderUI({
    req(input$lookup_card)
    deck <- lookup_deck()
    card_data <- deck$cards[[input$lookup_card]]
    req(card_data)

    # Always show both meanings, upright first, with orientation labels
    md_text <- paste0(
      "### ",
      card_data$title,
      "\n\n#### Upright\n\n",
      card_data$upright
    )
    if (card_data$reversed != "") {
      md_text <- paste0(
        md_text,
        "\n\n---\n\n#### Reversed\n\n",
        card_data$reversed
      )
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
