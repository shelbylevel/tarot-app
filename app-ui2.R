library(shiny)
library(shinyMobile)

# 1. Sample Deck Data by Suit
deck_cards <- list(
  "Major Arcana" = list(
    "The Fool" = list(
      num = "0",
      keywords = "BEGINNINGS · INNOCENCE · LEAP OF FAITH",
      meaning = "Step forward into the unknown with an open heart and trust your path."
    ),
    "The Magician" = list(
      num = "I",
      keywords = "WILLPOWER · MANIFESTATION · ACTION",
      meaning = "You already possess the tools and focus needed to turn vision into reality."
    ),
    "The High Priestess" = list(
      num = "II",
      keywords = "INTUITION · STILLNESS · INNER KNOWING",
      meaning = "Trust your inner knowing and practice patience. Deeper answers arrive through stillness and dreams."
    ),
    "The Empress" = list(
      num = "III",
      keywords = "ABUNDANCE · NURTURING · CREATIVITY",
      meaning = "Connect with sensory beauty, creative growth, and grounded generosity."
    )
  ),
  "Wands" = list(
    "Ace of Wands" = list(
      num = "ACE",
      keywords = "INSPIRATION · SPARK · NEW VENTURE",
      meaning = "A fresh burst of creative energy or passion is ready to be ignited."
    ),
    "Two of Wands" = list(
      num = "II",
      keywords = "PLANNING · VISION · DISCOVERY",
      meaning = "Look outward from where you stand and map your next bold move."
    )
  ),
  "Cups" = list(
    "Ace of Cups" = list(
      num = "ACE",
      keywords = "EMOTIONAL RENEWAL · COMPASSION · OPENNESS",
      meaning = "Allow new emotional clarity, tenderness, and intuition to overflow."
    ),
    "Two of Cups" = list(
      num = "II",
      keywords = "CONNECTION · HARMONY · MUTUAL RESPECT",
      meaning = "A balanced partnership or meaningful emotional resonance takes center stage."
    )
  ),
  "Swords" = list(
    "Ace of Swords" = list(
      num = "ACE",
      keywords = "CLARITY · TRUTH · BREAKTHROUGH",
      meaning = "Mental fog clears, revealing a sharp, undeniable truth."
    ),
    "Three of Swords" = list(
      num = "III",
      keywords = "HEARTBREAK · RELEASE · HONEST GRIEF",
      meaning = "Acknowledge emotional pain openly so genuine healing can begin."
    )
  )
)

# 2. Obsidian & Gold CSS (Matches your app + custom f7Sheet styling)
tarot_css <- "
@import url('https://googleapis.com');

:root {
  --bg: #131416;
  --surface: #1a1b1e;
  --gold: #d8b282;
  --gold-soft: #e6c396;
  --border: rgba(216, 178, 130, 0.3);
  --f7-font-family: -apple-system, BlinkMacSystemFont, sans-serif;
  --f7-bars-bg-color: var(--bg);
  --f7-bars-text-color: var(--gold-soft);
  --f7-theme-color: var(--gold);
}

.page-content {
  background: var(--bg) !important;
  color: #ece8e1;
}

.tarot-serif, .navbar .title {
  font-family: 'Cinzel Decorative', Georgia, serif !important;
  font-weight: 400 !important;
  color: var(--gold-soft) !important;
  letter-spacing: 0.03em;
}

.chip-row {
  display: flex;
  gap: 8px;
  padding: 6px 0 2px;
  overflow-x: auto;
  scrollbar-width: none;
}
.chip-row::-webkit-scrollbar { display: none; }

.tarot-chip {
  padding: 6px 14px;
  border-radius: 999px;
  font-size: 12px;
  white-space: nowrap;
  background: var(--surface);
  border: 1px solid var(--border);
  color: var(--gold);
  cursor: pointer;
  flex-shrink: 0;
  display: inline-flex;
  align-items: center;
  gap: 5px;
  transition: all 0.2s ease;
}

.tarot-chip.active {
  background: var(--gold);
  border-color: var(--gold);
  color: var(--bg);
  font-weight: 600;
}

.card, .sheet-modal {
  background: var(--surface) !important;
  border: 1px solid var(--border) !important;
  border-radius: 18px !important;
  box-shadow: none !important;
  color: #ece8e1 !important;
}

.sheet-modal {
  border-radius: 24px 24px 0 0 !important;
  border-bottom: none !important;
}

/* Scrollable Sheet List Items */
.sheet-handle {
  width: 38px;
  height: 4px;
  border-radius: 99px;
  background: rgba(216, 178, 130, 0.35);
  margin: 0 auto 14px;
}

.sheet-card-list {
  max-height: 48vh;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  gap: 6px;
  padding-bottom: 12px;
}

.sheet-card-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 12px 14px;
  border-radius: 12px;
  background: rgba(255, 255, 255, 0.02);
  border: 1px solid transparent;
  color: #ece8e1;
  text-decoration: none;
  font-size: 14px;
  transition: all 0.15s ease;
}

.sheet-card-item.is-selected {
  background: rgba(216, 178, 130, 0.15);
  border-color: var(--border);
  color: var(--gold-soft);
  font-weight: 600;
}

.card-preview-box {
  width: 150px;
  height: 230px;
  margin: 8px auto 18px;
  border-radius: 14px;
  background: #141517;
  border: 1px solid rgba(216, 178, 130, 0.55);
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  color: var(--gold-soft);
}
"

# 3. UI
ui <- f7Page(
  title = "Tarot Journal",
  options = list(theme = "ios", dark = TRUE),
  tags$head(tags$style(HTML(tarot_css))),

  f7SingleLayout(
    navbar = f7Navbar(title = span(class = "tarot-serif", "TAROT JOURNAL")),

    # Top Filter Card with Suit Chips
    f7Card(
      div(
        style = "font-size: 11px; letter-spacing: 0.08em; opacity: 0.6; margin-bottom: 6px;",
        "SUIT & ARCANA"
      ),
      uiOutput("suit_chips_ui")
    ),

    # Selected Card Guidebook Display
    f7Card(
      uiOutput("card_detail_ui")
    )
  ),

  # Single Reusable Bottom Sheet for Card Selection
  f7Sheet(
    id = "card_picker_sheet",
    swipeToClose = TRUE,
    backdrop = TRUE,
    div(
      style = "padding: 12px 16px 20px;",
      div(class = "sheet-handle"),
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
        h3(
          class = "tarot-serif",
          style = "margin: 0; font-size: 20px;",
          textOutput("sheet_suit_title", inline = TRUE)
        ),
        span(style = "font-size: 12px; opacity: 0.6;", "Tap card to view")
      ),
      uiOutput("sheet_card_options")
    )
  )
)

# 4. Server
server <- function(input, output, session) {
  selected_suit <- reactiveVal("Major Arcana")
  selected_card <- reactiveVal("The High Priestess")

  # Render horizontal suit chips
  output$suit_chips_ui <- renderUI({
    suits <- names(deck_cards)
    div(
      class = "chip-row",
      lapply(suits, function(suit) {
        is_active <- (suit == selected_suit())
        actionLink(
          inputId = paste0("suit_btn_", gsub(" ", "_", suit)),
          style = "text-decoration: none;",
          span(
            class = paste("tarot-chip", if (is_active) "active" else ""),
            if (is_active) "✦" else "✧",
            suit,
            span(style = "font-size: 10px; opacity: 0.8;", "▾")
          )
        )
      })
    )
  })

  # Observe any suit chip click -> set suit & open f7Sheet
  lapply(names(deck_cards), function(suit) {
    btn_id <- paste0("suit_btn_", gsub(" ", "_", suit))
    observeEvent(input[[btn_id]], {
      selected_suit(suit)
      updateF7Sheet(id = "card_picker_sheet")
    })
  })

  output$sheet_suit_title <- renderText({
    selected_suit()
  })

  # Render cards inside the f7Sheet for the active suit
  output$sheet_card_options <- renderUI({
    cards <- names(deck_cards[[selected_suit()]])
    div(
      class = "sheet-card-list",
      lapply(seq_along(cards), function(i) {
        card_name <- cards[i]
        card_info <- deck_cards[[selected_suit()]][[card_name]]
        is_curr <- (card_name == selected_card())

        actionLink(
          inputId = paste0("pick_card_", i),
          class = paste("sheet-card-item", if (is_curr) "is-selected" else ""),
          div(
            span(
              style = "opacity: 0.55; margin-right: 10px; font-size: 12px;",
              card_info$num
            ),
            span(card_name)
          ),
          if (is_curr) span(style = "color: var(--gold);", "✦") else NULL
        )
      })
    )
  })

  # Observe card selection inside the sheet -> update active card & close sheet
  observe({
    cards <- names(deck_cards[[selected_suit()]])
    lapply(seq_along(cards), function(i) {
      btn_id <- paste0("pick_card_", i)
      observeEvent(
        input[[btn_id]],
        {
          selected_card(cards[i])
          updateF7Sheet(id = "card_picker_sheet")
        },
        ignoreInit = TRUE,
        once = TRUE
      )
    })
  })

  # Render selected card guidebook detail
  output$card_detail_ui <- renderUI({
    suit <- selected_suit()
    card <- selected_card()
    # Fallback to first card in suit if current card isn't in newly picked suit
    if (!card %in% names(deck_cards[[suit]])) {
      card <- names(deck_cards[[suit]])[1]
      selected_card(card)
    }
    info <- deck_cards[[suit]][[card]]

    div(
      div(
        class = "card-preview-box",
        span(
          style = "font-size: 13px; opacity: 0.7; margin-bottom: 8px;",
          info$num
        ),
        span(
          class = "tarot-serif",
          style = "font-size: 15px; text-align: center; padding: 0 10px;",
          card
        )
      ),
      h3(style = "margin: 0 0 4px; font-size: 18px; color: #ece8e1;", card),
      div(
        style = "font-size: 12px; font-weight: 600; color: var(--gold); margin-bottom: 12px;",
        "Upright"
      ),
      div(
        style = "font-size: 11px; letter-spacing: 0.06em; opacity: 0.7; margin-bottom: 10px;",
        info$keywords
      ),
      p(
        style = "font-size: 14px; line-height: 1.55; margin: 0; opacity: 0.88;",
        info$meaning
      )
    )
  })
}

shinyApp(ui, server)
