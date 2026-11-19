library(shiny)
library(shinyMobile)

tarot_css <- "
@font-face {
  font-family: 'Cinzel Decorative';
  font-style: normal;
  font-weight: 400;
  src: url(https://gstatic.com) format('truetype');
}

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

.tarot-serif,
.navbar .title {
  font-family: 'Cinzel Decorative', Georgia, serif !important;
  font-weight: 400 !important;
  color: var(--gold-soft) !important;
  letter-spacing: 0.03em;
}

.tarot-greeting { padding: 8px 16px 4px; }
.tarot-greeting h2 { margin: 0; font-size: 24px; }
.tarot-greeting p { margin: 2px 0 0; font-size: 13px; opacity: 0.7; }

.chip-row {
  display: flex;
  gap: 8px;
  padding: 10px 16px;
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
}
.tarot-chip.active {
  background: var(--gold);
  border-color: var(--gold);
  color: var(--bg);
}

.card,
.sheet-modal {
  background: var(--surface) !important;
  border: 1px solid var(--border) !important;
  border-radius: 18px !important;
  box-shadow: none !important;
  color: #ece8e1 !important;
}
.sheet-modal { border-radius: 24px 24px 0 0 !important; }

.spread-card-inner {
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 12px;
}

.mini-card-art,
.tarot-card-frame {
  background: #141517;
  border: 1px solid rgba(216, 178, 130, 0.55);
  color: var(--gold-soft);
  display: flex;
  align-items: center;
  justify-content: center;
}

.mini-card-art {
  width: 46px;
  height: 70px;
  border-radius: 8px;
  font-size: 18px;
  flex-shrink: 0;
}

.tarot-card-frame {
  width: 170px;
  height: 265px;
  border-radius: 14px;
  margin: 14px 0 18px;
  flex-direction: column;
  transition: transform 0.35s ease;
}
.tarot-card-frame.is-reversed { transform: rotate(180deg); }

.cta-pill .button {
  background: var(--gold) !important;
  color: var(--bg) !important;
  border-radius: 10px !important;
  font-weight: 600;
}

.card-detail-hero {
  display: flex;
  flex-direction: column;
  align-items: center;
  text-align: center;
  padding: 8px 16px 24px;
}

.tarot-quote {
  font-family: Georgia, serif;
  font-style: italic;
  font-size: 18px;
  line-height: 1.4;
  color: var(--gold-soft);
  margin: 16px 0 12px;
}

.tarot-body-text {
  font-size: 14px;
  line-height: 1.6;
  text-align: left;
  opacity: 0.88;
}
"

sample_cards <- list(
  "The Magician" = list(
    arcana = "Major Arcana · I",
    symbol = "✦",
    keywords = c("Confidence", "Manifestation", "Action"),
    quote = "“You already have everything you need. Believe in your power and take action.”",
    meaning = "The Magician represents willpower, skill, and the ability to turn ideas into tangible reality. When pulled in a physical reading, notice what tools and resources are already within reach."
  ),
  "The Moon" = list(
    arcana = "Major Arcana · XVIII",
    symbol = "☾",
    keywords = c("Intuition", "Illusion", "Subconscious"),
    quote = "“Not everything is illuminated yet; trust your inner compass through the mist.”",
    meaning = "The Moon invites you to slow down and pay attention to dreams, gut feelings, and unspoken undercurrents before making a major decision."
  ),
  "Wheel of Fortune" = list(
    arcana = "Major Arcana · X",
    symbol = "❂",
    keywords = c("Cycles", "Turning Point", "Destiny"),
    quote = "“Go forward and follow where your heart leads as the wheel turns.”",
    meaning = "A reminder that seasons shift continuously. Stay grounded at the center of change and welcome unexpected openings."
  )
)

ui <- f7Page(
  title = "Arcana Journal",
  options = list(theme = "ios", dark = TRUE),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Cinzel+Decorative:wght@400;600&display=swap"
    ),
    tags$style(HTML(tarot_css))
  ),

  f7TabLayout(
    navbar = f7Navbar(
      title = span(class = "tarot-serif", "Arcana Journal"),
      rightPanel = FALSE
    ),

    f7Tabs(
      id = "main_tabs",
      animated = TRUE,

      f7Tab(
        title = "Spreads",
        tabName = "spreads",
        icon = f7Icon("sparkles"),
        active = TRUE,

        div(
          class = "tarot-greeting",
          h2(class = "tarot-serif", "Daily Reflection"),
          p(format(Sys.Date(), "%A, %b %d"))
        ),

        div(
          class = "chip-row",
          span(class = "tarot-chip active", "All Spreads"),
          span(class = "tarot-chip", "Daily Pull"),
          span(class = "tarot-chip", "Three-Card"),
          span(class = "tarot-chip", "Clarity")
        ),

        f7Card(
          div(
            class = "spread-card-inner",
            div(
              h3(
                class = "tarot-serif",
                style = "margin:0 0 4px 0; font-size:22px;",
                "Log Physical Draw"
              ),
              p(
                style = "margin:0 0 12px 0; font-size:13px; opacity:0.8;",
                "Record the cards you pulled from your deck."
              ),
              div(
                class = "cta-pill",
                style = "display: inline-block;",
                f7Button(
                  inputId = "open_log_sheet",
                  label = "Record Pull",
                  size = "small"
                )
              )
            ),
            div(class = "mini-card-art", "☀")
          )
        ),

        f7Card(
          div(
            class = "spread-card-inner",
            div(
              h4(
                class = "tarot-serif",
                "Single Card"
              ),
              p(
                style = "margin:2px 0 0 0; font-size:12px; opacity:0.75;",
                "Focused daily guidance"
              )
            ),
            div(class = "mini-card-art", "☾")
          )
        ),

        f7Card(
          div(
            class = "spread-card-inner",
            div(
              h4(
                class = "tarot-serif",
                "Past · Present · Future"
              ),
              p(
                style = "margin:2px 0 0 0; font-size:12px; opacity:0.75;",
                "Three-card time flow spread"
              )
            ),
            div(class = "mini-card-art", "✦")
          )
        ),

        f7Card(
          div(
            class = "spread-card-inner",
            div(
              h4(
                class = "tarot-serif",
                "Preview: The Magician"
              ),
              p(
                style = "margin:2px 0 0 0; font-size:12px; opacity:0.75;",
                "Tap to inspect card detail sheet"
              )
            ),
            f7Button(
              inputId = "inspect_magician",
              label = "View",
              size = "small",
              outline = TRUE,
              fill = FALSE
            )
          )
        )
      ),

      f7Tab(
        title = "History",
        tabName = "history",
        icon = f7Icon("book"),

        div(
          class = "tarot-greeting",
          h2(class = "tarot-serif", "Past Readings"),
          p("Your logged deck sessions")
        ),
        uiOutput("history_list")
      )
    )
  ),

  f7Sheet(
    id = "log_sheet",
    swipeToClose = TRUE,
    backdrop = TRUE,
    div(
      style = "padding: 16px;",
      h3(
        class = "tarot-serif",
        style = "margin-top:0; font-size:24px;",
        "Log Your Physical Pull"
      ),
      f7Select(
        inputId = "pull_spread",
        label = "Spread Type",
        choices = c("Single Card", "3-Card (Past / Present / Future)")
      ),
      f7Select(
        inputId = "pull_card",
        label = "Card Drawn",
        choices = names(sample_cards)
      ),
      f7Toggle(
        inputId = "pull_reversed",
        label = "Reversed (180°)",
        checked = FALSE
      ),
      f7TextArea(
        inputId = "pull_notes",
        label = "Reflection Notes",
        placeholder = "What stood out in your physical spread?"
      ),
      br(),
      div(
        class = "cta-pill",
        f7Button(
          inputId = "save_pull",
          label = "Save to Journal"
        )
      )
    )
  ),

  f7Sheet(
    id = "detail_sheet",
    swipeToClose = TRUE,
    backdrop = TRUE,
    uiOutput("card_detail_ui")
  )
)

server <- function(input, output, session) {
  readings <- reactiveVal(list(
    list(
      date = format(Sys.Date(), "%b %d"),
      spread = "Single Card",
      card = "The Magician",
      reversed = FALSE,
      notes = "Pulled this morning before starting my UI prototype."
    )
  ))

  selected_reading <- reactiveVal(
    list(card = "The Magician", reversed = FALSE, notes = "")
  )

  observeEvent(input$open_log_sheet, {
    updateF7Sheet(id = "log_sheet", session = session)
  })

  observeEvent(input$save_pull, {
    new_entry <- list(
      date = format(Sys.Date(), "%b %d"),
      spread = input$pull_spread,
      card = input$pull_card,
      reversed = isTRUE(input$pull_reversed),
      notes = input$pull_notes
    )
    readings(c(list(new_entry), readings()))
    selected_reading(new_entry)

    updateF7Sheet(id = "log_sheet", session = session)
    f7Toast(text = "Reading saved to journal")
    updateF7Sheet(id = "detail_sheet", session = session)
  })

  observeEvent(input$inspect_magician, {
    selected_reading(list(card = "The Magician", reversed = FALSE, notes = ""))
    updateF7Sheet(id = "detail_sheet", session = session)
  })

  output$card_detail_ui <- renderUI({
    item <- selected_reading()
    info <- sample_cards[[item$card]]
    frame_class <- if (isTRUE(item$reversed)) {
      "tarot-card-frame is-reversed"
    } else {
      "tarot-card-frame"
    }

    div(
      class = "card-detail-hero",
      p(style = "margin:0; font-size:12px; opacity:0.7;", info$arcana),
      h2(
        class = "tarot-serif",
        style = "margin:2px 0 0 0; font-size:32px;",
        paste0(item$card, if (isTRUE(item$reversed)) " (Reversed)" else "")
      ),

      div(
        class = frame_class,
        div(style = "font-size:48px; margin-bottom:8px;", info$symbol),
        div(class = "tarot-serif", style = "font-size:16px;", item$card)
      ),

      div(
        style = "display:flex; gap:8px; flex-wrap:wrap; justify-content:center;",
        lapply(info$keywords, function(kw) span(class = "tarot-chip", kw))
      ),

      div(class = "tarot-quote", info$quote),
      div(class = "tarot-body-text", info$meaning),

      if (nzchar(item$notes)) {
        div(
          style = "width:100%; margin-top:14px; padding-top:12px; border-top:1px solid rgba(255,255,255,0.12); text-align:left;",
          strong(style = "font-size:12px; opacity:0.7;", "YOUR NOTE"),
          p(style = "margin:4px 0 0 0; font-size:13px;", item$notes)
        )
      }
    )
  })

  output$history_list <- renderUI({
    lapply(seq_along(readings()), function(i) {
      r <- readings()[[i]]
      f7Card(
        div(
          class = "spread-card-inner",
          div(
            span(
              style = "font-size:11px; opacity:0.65;",
              paste(r$date, "·", r$spread)
            ),
            h4(
              class = "tarot-serif",
              style = "margin:2px 0; font-size:20px;",
              paste0(r$card, if (isTRUE(r$reversed)) " (Reversed)" else "")
            ),
            if (nzchar(r$notes)) {
              p(
                style = "margin:4px 0 0 0; font-size:13px; opacity:0.8;",
                r$notes
              )
            }
          ),
          div(
            class = "mini-card-art",
            style = if (isTRUE(r$reversed)) {
              "transform: rotate(180deg);"
            } else {
              ""
            },
            sample_cards[[r$card]]$symbol
          )
        )
      )
    })
  })
}

shinyApp(ui, server)
