library(shiny)
library(shinyMobile)

# 1. Helper for Custom-Styled Dropdown Chip
tarot_chip_dropdown <- function(inputId, label_output_id, choices) {
  menu_id <- paste0(inputId, "_menu")

  div(
    class = "chip-dropdown-wrap",
    # Trigger Chip
    tags$span(
      class = "tarot-chip active",
      onclick = sprintf(
        "event.stopPropagation(); document.getElementById('%s').classList.toggle('open');",
        menu_id
      ),
      textOutput(label_output_id, inline = TRUE),
      span(style = "margin-left: 5px; font-size: 10px;", "▾")
    ),
    # Floating Obsidian & Gold Menu
    div(
      id = menu_id,
      class = "tarot-dropdown-menu",
      lapply(seq_along(choices), function(i) {
        choice <- choices[i]
        div(
          class = paste("tarot-dropdown-item", if (i == 1) "selected" else ""),
          onclick = sprintf(
            "selectTarotOption(this, '%s', '%s', '%s')",
            inputId,
            menu_id,
            choice
          ),
          span(choice),
          span(class = "check-mark", "✦")
        )
      })
    )
  )
}

# 2. Helper for Toggleable Filter Chips
tarot_chip_toggle <- function(inputId, label) {
  tags$span(
    id = inputId,
    class = "tarot-chip",
    onclick = sprintf(
      "this.classList.toggle('active'); Shiny.setInputValue('%s', this.classList.contains('active'));",
      inputId
    ),
    label
  )
}

# 3. Sample Data
readings_data <- data.frame(
  card = c("The Magician", "The Moon", "Three of Cups", "Wheel of Fortune"),
  spread = c("Daily Pull", "Three-Card", "Daily Pull", "Clarity"),
  arcana = c("Major", "Major", "Minor", "Major"),
  reversed = c(FALSE, TRUE, FALSE, TRUE),
  symbol = c("✦", "☾", "🍷", "❂"),
  stringsAsFactors = FALSE
)

# 4. Obsidian & Gold CSS + Custom Dropdown Styling
tarot_css <- "
:root {
  --bg: #131416;
  --surface: #1a1b1e;
  --gold: #d8b282;
  --gold-soft: #e6c396;
  --border: rgba(216, 178, 130, 0.3);
}

.page-content {
  background: var(--bg) !important;
  color: #ece8e1;
}

.tarot-serif, .navbar .title {
  font-family: Georgia, serif !important;
  color: var(--gold-soft) !important;
}

/* Allow dropdown to overflow downward while keeping horizontal scroll */
.chip-row {
  display: flex;
  gap: 8px;
  padding: 12px 16px;
  overflow: visible;
  position: relative;
  z-index: 50;
}

.chip-dropdown-wrap {
  position: relative;
  display: inline-flex;
  flex-shrink: 0;
}

.tarot-chip {
  padding: 6px 14px;
  border-radius: 999px;
  font-size: 12px;
  white-space: nowrap;
  background: var(--surface);
  border: 1px solid var(--border);
  color: var(--gold);
  cursor: pointer;
  user-select: none;
  transition: all 0.2s ease;
  display: inline-flex;
  align-items: center;
}

.tarot-chip.active {
  background: var(--gold);
  border-color: var(--gold);
  color: var(--bg);
  font-weight: 600;
}

/* Custom Floating Dropdown Menu */
.tarot-dropdown-menu {
  position: absolute;
  top: calc(100% + 8px);
  left: 0;
  min-width: 175px;
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 16px;
  padding: 6px;
  box-shadow: 0 12px 28px rgba(0, 0, 0, 0.65);
  z-index: 999;
  opacity: 0;
  transform: translateY(-6px) scale(0.97);
  pointer-events: none;
  transition: opacity 0.18s ease, transform 0.18s ease;
}

.tarot-dropdown-menu.open {
  opacity: 1;
  transform: translateY(0) scale(1);
  pointer-events: auto;
}

.tarot-dropdown-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 9px 12px;
  border-radius: 10px;
  font-size: 13px;
  color: #ece8e1;
  cursor: pointer;
  transition: background 0.15s ease, color 0.15s ease;
}

.tarot-dropdown-item:hover {
  background: rgba(216, 178, 130, 0.12);
}

.tarot-dropdown-item.selected {
  background: rgba(216, 178, 130, 0.18);
  color: var(--gold-soft);
  font-weight: 600;
}

.tarot-dropdown-item .check-mark {
  opacity: 0;
  color: var(--gold);
  font-size: 11px;
}

.tarot-dropdown-item.selected .check-mark {
  opacity: 1;
}

.card {
  background: var(--surface) !important;
  border: 1px solid var(--border) !important;
  border-radius: 16px !important;
  color: #ece8e1 !important;
}

.spread-card-inner {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.mini-card-art {
  width: 42px;
  height: 62px;
  border-radius: 8px;
  border: 1px solid rgba(216, 178, 130, 0.55);
  color: var(--gold-soft);
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 18px;
}
"

# 5. Small JS helper to handle selection & closing when tapping outside
tarot_js <- "
function selectTarotOption(el, inputId, menuId, val) {
  const menu = document.getElementById(menuId);
  menu.querySelectorAll('.tarot-dropdown-item').forEach(item => {
    item.classList.remove('selected');
  });
  el.classList.add('selected');
  menu.classList.remove('open');
  Shiny.setInputValue(inputId, val);
}

document.addEventListener('click', function(e) {
  document.querySelectorAll('.tarot-dropdown-menu.open').forEach(menu => {
    if (!menu.parentElement.contains(e.target)) {
      menu.classList.remove('open');
    }
  });
});
"

# 6. App UI
ui <- f7Page(
  title = "Styled Chip Dropdown",
  options = list(theme = "ios", dark = TRUE),
  tags$head(
    tags$style(HTML(tarot_css)),
    tags$script(HTML(tarot_js))
  ),

  f7SingleLayout(
    navbar = f7Navbar(title = span(class = "tarot-serif", "Arcana Filter")),

    div(
      class = "chip-row",
      tarot_chip_dropdown(
        inputId = "spread_filter",
        label_output_id = "selected_spread_label",
        choices = c("All Spreads", "Daily Pull", "Three-Card", "Clarity")
      ),
      tarot_chip_toggle("major_only", "Major Arcana"),
      tarot_chip_toggle("reversed_only", "Reversed Only")
    ),

    uiOutput("filtered_cards")
  )
)

# 7. App Server
server <- function(input, output, session) {
  output$selected_spread_label <- renderText({
    if (is.null(input$spread_filter)) "All Spreads" else input$spread_filter
  })

  filtered_data <- reactive({
    df <- readings_data
    spread_val <- if (is.null(input$spread_filter)) {
      "All Spreads"
    } else {
      input$spread_filter
    }

    if (spread_val != "All Spreads") {
      df <- df[df$spread == spread_val, ]
    }
    if (isTRUE(input$major_only)) {
      df <- df[df$arcana == "Major", ]
    }
    if (isTRUE(input$reversed_only)) {
      df <- df[df$reversed == TRUE, ]
    }
    df
  })

  output$filtered_cards <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(div(
        style = "text-align: center; padding: 32px; opacity: 0.6;",
        "No readings match these filters."
      ))
    }
    lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      f7Card(
        div(
          class = "spread-card-inner",
          div(
            h4(class = "tarot-serif", style = "margin: 0;", row$card),
            p(
              style = "margin: 4px 0 0; font-size: 12px; opacity: 0.75;",
              paste0(
                row$spread,
                " · ",
                row$arcana,
                " Arcana",
                if (row$reversed) " (Reversed)" else ""
              )
            )
          ),
          div(class = "mini-card-art", row$symbol)
        )
      )
    })
  })
}

shinyApp(ui, server)
