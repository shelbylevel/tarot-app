library(shiny)
library(bslib)
library(markdown)
library(stringr)
library(dplyr)
library(purrr)

# ==============================================================================
# 1. PARSE THE ROBIN WOOD MARKDOWN FILE
# ==============================================================================
parse_robin_wood <- function(file_path) {
  raw_text <- paste(
    readLines(file_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  # Split by ### (Level 3 headers represent the 78 individual cards)
  chunks <- str_split(raw_text, "(?m)^###\\s+")[[1]]

  # The first chunk contains intro & Major Arcana header; skip it for card parsing
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

  # Name the list elements by Card Title
  names(deck_list) <- map_chr(deck_list, "title")
  return(deck_list)
}

# Load the guidebook
guidebook <- parse_robin_wood("data/robin-wood-guidebook.md")
card_choices <- names(guidebook)
log_file <- "reading_log.csv"

# Helper to load existing reading logs
load_logs <- function() {
  if (file.exists(log_file)) {
    read.csv(log_file, stringsAsFactors = FALSE)
  } else {
    data.frame(
      id = character(),
      date = character(),
      deck = character(),
      question = character(),
      cards_summary = character(),
      notes = character(),
      stringsAsFactors = FALSE
    )
  }
}

# ==============================================================================
# 2. MOBILE-FIRST UI
# ==============================================================================
ui <- page_navbar(
  # title = "Tarot Journal",
  title = div(
    style = "font-family: 'Cinzel Decorative', serif; color: #D4AF37; font-weight: 600;",
    icon("moon", style = "margin-right: 8px;"),
    "Tarot Journal"
  ),
  # theme = bs_theme(
  #   version = 5,
  #   bootswatch = "sandstone", # Warm, earthy tone suited for Robin Wood
  #   primary = "#4A6B53", # Forest green accent
  #   base_font = font_google("Lora"),
  #   heading_font = font_google("Cinzel")
  # ),
  theme = bs_theme(
    version = 5,
    preset = "darkly",
    bg = "#12131A",
    fg = "#E8E8E8",
    primary = "#D4AF37",
    secondary = "#2A2B35",
    success = "#6B8E23",
    info = "#4682B4",
    warning = "#DAA520",
    danger = "#8B0000",
    base_font = font_google("Lora"),
    heading_font = font_google("Cinzel Decorative"),
    code_font = font_google("Fira Mono")
  ),
  fillable = FALSE,
  header = tags$style(HTML(
    "
    .navbar {
      background: linear-gradient(135deg, #12131A 0%, #1A1B28 100%) !important;
      border-bottom: 2px solid #D4AF37;
      box-shadow: 0 2px 8px rgba(212, 175, 55, 0.2);
    }
    .card {
      background-color: #1A1B28 !important;
      border: 1px solid #2A2B35 !important;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.5);
      transition: transform 0.2s, box-shadow 0.2s;
    }
    .card:hover {
      transform: translateY(-2px);
      box-shadow: 0 6px 16px rgba(212, 175, 55, 0.3);
    }
    .card-header {
      background: linear-gradient(135deg, #2A2B35 0%, #1F2029 100%) !important;
      border-bottom: 1px solid #D4AF37 !important;
      color: #D4AF37 !important;
      font-weight: 600;
    }
    .btn-primary {
      background: linear-gradient(135deg, #D4AF37 0%, #B8941F 100%) !important;
      border: none !important;
      color: #12131A !important;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      box-shadow: 0 2px 8px rgba(212, 175, 55, 0.4);
    }
    .btn-primary:hover {
      background: linear-gradient(135deg, #E5C047 0%, #C9A527 100%) !important;
      box-shadow: 0 4px 12px rgba(212, 175, 55, 0.6);
    }
    .btn-success {
      background-color: #6B8E23 !important;
      border: none !important;
      font-weight: 600;
    }
    .btn-outline-danger {
      border-color: #8B0000 !important;
      color: #8B0000 !important;
      font-weight: 600;
    }
    .btn-outline-danger:hover {
      background-color: #8B0000 !important;
      color: #E8E8E8 !important;
    }
    .form-control, .form-select {
      background-color: #1F2029 !important;
      border: 1px solid #3A3B45 !important;
      color: #E8E8E8 !important;
    }
    .form-control:focus, .form-select:focus {
      border-color: #D4AF37 !important;
      box-shadow: 0 0 0 0.2rem rgba(212, 175, 55, 0.25) !important;
    }
    .badge {
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    .border-primary {
      border-color: #D4AF37 !important;
    }
    hr {
      border-color: #3A3B45 !important;
      opacity: 0.5;
    }
    .bg-light {
      background-color: #2A2B35 !important;
    }
    .text-muted {
      color: #9A9BA5 !important;
    }
    h5 {
      color: #D4AF37;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 1px;
      margin-top: 1.5rem;
    }
  "
  )),

  # --- TAB 1: LOG A MANUAL READING ---
  nav_panel(
    title = "New Reading",
    icon = icon("layer-group"),

    # Step 1: Reading Context
    card(
      card_header("1. Spread Setup"),
      textInput(
        "deck_name",
        "Deck Used:",
        value = "Robin Wood Tarot",
        width = "100%"
      ),
      textAreaInput(
        "reading_question",
        "Question or Intention:",
        placeholder = "What do I need to see right now?",
        rows = 2,
        width = "100%"
      )
    ),

    # Step 2: Input Pulled Cards
    card(
      card_header("2. Add Cards Drawn"),
      layout_columns(
        col_widths = c(12, 6, 6),
        selectizeInput(
          "pick_card",
          "Card Drawn:",
          choices = card_choices,
          width = "100%"
        ),
        textInput(
          "pick_pos",
          "Position (Optional):",
          placeholder = "e.g., Past / Situation",
          width = "100%"
        ),
        radioButtons(
          "pick_orient",
          "Orientation:",
          choices = c("Upright", "Reversed"),
          inline = TRUE
        )
      ),
      actionButton(
        "add_card_btn",
        "Add Card to Spread",
        icon = icon("plus"),
        class = "btn-primary w-100"
      )
    ),

    # Step 3: Live Spread Meanings
    uiOutput("active_spread_ui"),

    # Step 4: Reflection & Save
    conditionalPanel(
      condition = "output.has_cards == true",
      card(
        card_header("3. Final Reflections & Save"),
        textAreaInput(
          "reading_notes",
          "Your Gut Takeaway:",
          placeholder = "Robin Wood's rule: what does your gut say when looking at them together?",
          rows = 3,
          width = "100%"
        ),
        layout_columns(
          col_widths = c(6, 6),
          actionButton(
            "clear_spread_btn",
            "Clear Spread",
            class = "btn-outline-danger w-100"
          ),
          actionButton(
            "save_reading_btn",
            "Save to Log",
            icon = icon("bookmark"),
            class = "btn-success w-100"
          )
        )
      )
    )
  ),

  # --- TAB 2: PAST READINGS LOG ---
  nav_panel(
    title = "History",
    icon = icon("clock-rotate-left"),
    uiOutput("history_ui")
  ),

  # --- TAB 3: QUICK GUIDEBOOK LOOKUP ---
  nav_panel(
    title = "Guidebook",
    icon = icon("book-open"),
    card(
      selectizeInput(
        "lookup_card",
        "Browse Any Card:",
        choices = card_choices,
        width = "100%"
      ),
      checkboxInput(
        "show_rev_lookup",
        "Show optional reversed entry",
        value = FALSE
      ),
      hr(),
      uiOutput("lookup_display")
    )
  )
)

# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  # Reactive list storing cards added to the current spread
  current_spread <- reactiveVal(list())

  # Reactive trigger for refreshing history
  saved_logs <- reactiveVal(load_logs())

  # Flag for conditional UI panel
  output$has_cards <- reactive({
    length(current_spread()) > 0
  })
  outputOptions(output, "has_cards", suspendWhenHidden = FALSE)

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
      orientation = input$pick_orient
    )

    current_spread(append(spread, list(new_entry)))
    updateTextInput(session, "pick_pos", value = "") # Reset position box for next card
  })

  # Clear spread
  observeEvent(input$clear_spread_btn, {
    current_spread(list())
    updateTextAreaInput(session, "reading_question", value = "")
    updateTextAreaInput(session, "reading_notes", value = "")
  })

  # Render the active spread with full Robin Wood narratives
  output$active_spread_ui <- renderUI({
    spread <- current_spread()
    if (length(spread) == 0) {
      return(NULL)
    }

    cards_ui <- imap(spread, function(item, idx) {
      card_data <- guidebook[[item$card]]

      # Combine upright + reversed if reversed was selected
      md_text <- card_data$upright
      if (item$orientation == "Reversed" && card_data$reversed != "") {
        md_text <- paste(md_text, "\n\n---\n\n", card_data$reversed)
      }

      badge_class <- ifelse(
        item$orientation == "Upright",
        "bg-success",
        "bg-warning text-dark"
      )

      card(
        class = "mb-3 border-start border-4 border-primary",
        card_header(
          class = "d-flex justify-content-between align-items-center",
          tags$span(tags$strong(item$position), " — ", item$card),
          tags$span(class = paste("badge", badge_class), item$orientation)
        ),
        HTML(markdownToHTML(text = md_text, fragment.only = TRUE))
      )
    })

    tagList(
      h5(class = "mt-3 mb-2", "Spread Meanings"),
      cards_ui
    )
  })

  # Save reading to CSV
  observeEvent(input$save_reading_btn, {
    spread <- current_spread()
    req(length(spread) > 0)

    summary_str <- map_chr(spread, function(x) {
      paste0(x$position, ": ", x$card, " (", x$orientation, ")")
    }) %>%
      paste(collapse = " | ")

    new_row <- data.frame(
      id = as.character(as.integer(Sys.time())),
      date = format(Sys.time(), "%Y-%m-%d %I:%M %p"),
      deck = ifelse(
        str_trim(input$deck_name) == "",
        "Robin Wood Tarot",
        input$deck_name
      ),
      question = ifelse(
        str_trim(input$reading_question) == "",
        "General Pull",
        input$reading_question
      ),
      cards_summary = summary_str,
      notes = input$reading_notes,
      stringsAsFactors = FALSE
    )

    updated_logs <- bind_rows(new_row, saved_logs())
    write.csv(updated_logs, log_file, row.names = FALSE)
    saved_logs(updated_logs)

    # Reset spread & notify user
    current_spread(list())
    updateTextAreaInput(session, "reading_question", value = "")
    updateTextAreaInput(session, "reading_notes", value = "")
    showNotification("Reading saved to your log!", type = "message")
  })

  # Render History Tab
  output$history_ui <- renderUI({
    logs <- saved_logs()
    if (nrow(logs) == 0) {
      return(card(p(
        "No readings logged yet. Pull some cards and save your first spread!"
      )))
    }

    map(seq_len(nrow(logs)), function(i) {
      row <- logs[i, ]
      card_items <- str_split(row$cards_summary, " \\| ")[[1]]

      card(
        class = "mb-3",
        card_header(
          class = "d-flex justify-content-between",
          tags$strong(row$question),
          tags$small(class = "text-muted", row$date)
        ),
        tags$p(
          class = "text-muted small mb-2",
          icon("box-open"),
          " Deck: ",
          row$deck
        ),
        tags$ul(
          class = "mb-2",
          map(card_items, tags$li)
        ),
        if (!is.na(row$notes) && str_trim(row$notes) != "") {
          tags$div(
            class = "p-2 bg-light rounded small",
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
    card_data <- guidebook[[input$lookup_card]]

    md_text <- paste0("### ", card_data$title, "\n\n", card_data$upright)
    if (input$show_rev_lookup && card_data$reversed != "") {
      md_text <- paste0(md_text, "\n\n---\n\n", card_data$reversed)
    }

    HTML(markdownToHTML(text = md_text, fragment.only = TRUE))
  })
}

shinyApp(ui, server)
