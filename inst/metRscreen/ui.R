# UI##########
ui <- function() {
  bslib::page_fluid(
    shinyjs::useShinyjs(),
    shiny::tags$script(shiny::HTML("
  $(document).on('keydown', function(e) {
    var el = document.activeElement;
    var tag = el.tagName.toLowerCase();
    // only ignore shortcuts while typing (ticking a reason or choosing a screener still allows y/m/n)
    if (tag === 'textarea' || (tag === 'input' && el.type !== 'radio' && el.type !== 'checkbox')) return;
    // leave browser/system shortcuts alone (e.g. Cmd+Left is 'back', Cmd+N a new window)
    if (e.ctrlKey || e.metaKey || e.altKey) return;
    // greyed-out buttons (e.g. view-only browsing) can't be pressed from the keyboard either
    var press = function(id) {
      if (!$(id).prop('disabled')) $(id).click();
      e.preventDefault();
    };
    var key = e.key.length === 1 ? e.key.toLowerCase() : e.key;
    // a held key repeats: one press is one decision, so papers aren't decided without being read
    // (originalEvent because jQuery's event doesn't carry 'repeat')
    if (e.originalEvent && e.originalEvent.repeat && (key === 'y' || key === 'm' || key === 'n')) return;
    if (key === 'y') press('#Accept');
    if (key === 'm') press('#NoDecision');
    if (key === 'n') press('#Reject');
    // arrows move between papers, except on radio buttons, where they change the selection
    // (e.g. 'Who is screening?')
    if (el.type === 'radio') return;
    if (key === 'ArrowLeft') press('#Previous');
    if (key === 'ArrowRight') press('#Next');
  });
  // once a screener is chosen, move focus off 'Who is screening?' so the arrow keys move between papers
  // instead of quietly switching to the next screener
  $(document).on('change', 'input[name=\"choose.collab\"]', function() { this.blur(); });
")),
    shiny::tags$script(shiny::HTML("
  $(document).on('shiny:inputchanged', function(e) {
    if (e.name === 'font_size') {
      $('#title, #author, #year, #journal, #abstract, #keyword').css('font-size', e.value + 'px');
    }
  });
")),
    # colours for decisions and keywords: darker in light mode and lighter in dark
    # mode, so text keeps a contrast of at least 4.5:1 with the background
    shiny::tags$style(shiny::HTML("
  :root {
    --mrs-accept: #1e8449; --mrs-reject: #c0392b; --mrs-nodecision: #1f6fa8;
    --mrs-progress: #d62828; --mrs-viewonly: #a84f00;
    --mrs-kw1: #2b7d0c; --mrs-kw2: #DC143C; --mrs-kw3: #8a1fd1; --mrs-kw4: #9a5b00; --mrs-kw5: #334DFF;
  }
  [data-bs-theme='dark'] {
    --mrs-accept: #2ecc71; --mrs-reject: #ff6b5b; --mrs-nodecision: #5dade2;
    --mrs-progress: #ff5c5c; --mrs-viewonly: #e67e22;
    --mrs-kw1: #52D017; --mrs-kw2: #ff4d6d; --mrs-kw3: #c77dff; --mrs-kw4: #FFB133; --mrs-kw5: #7b8cff;
  }
  /* each keyword box also has its own underline, so matches can be told apart
     without relying on colour */
  .mrs-kw { font-weight: bold; text-decoration-line: underline;
            text-decoration-thickness: 2px; text-underline-offset: 3px; }
  .mrs-kw1 { color: var(--mrs-kw1); text-decoration-style: solid; }
  .mrs-kw2 { color: var(--mrs-kw2); text-decoration-style: double; }
  .mrs-kw3 { color: var(--mrs-kw3); text-decoration-style: dotted; }
  .mrs-kw4 { color: var(--mrs-kw4); text-decoration-style: dashed; }
  .mrs-kw5 { color: var(--mrs-kw5); text-decoration-style: wavy; }
  .mrs-colour-only .mrs-kw { text-decoration-line: none; }
  /* tip boxes: solid, high-contrast, and the opposite shade to the page */
  .tippy-tooltip.dark-theme { background-color: #212529; color: #ffffff;
    font-size: 13px; line-height: 1.5; padding: 8px 10px; opacity: 1; }
  .tippy-popper[x-placement^='top'] .tippy-tooltip.dark-theme .tippy-arrow { border-top-color: #212529; }
  .tippy-popper[x-placement^='bottom'] .tippy-tooltip.dark-theme .tippy-arrow { border-bottom-color: #212529; }
  [data-bs-theme='dark'] .tippy-tooltip.dark-theme { background-color: #f8f9fa; color: #212529; }
  [data-bs-theme='dark'] .tippy-popper[x-placement^='top'] .tippy-tooltip.dark-theme .tippy-arrow { border-top-color: #f8f9fa; }
  [data-bs-theme='dark'] .tippy-popper[x-placement^='bottom'] .tippy-tooltip.dark-theme .tippy-arrow { border-bottom-color: #f8f9fa; }
  /* in dark mode the Previous/Next buttons aren't bright white; hovering still fills them blue */
  [data-bs-theme='dark'] .bttn-fill.bttn-default { background: var(--bs-secondary-bg); color: #6ea8fe; }
  [data-bs-theme='dark'] .bttn-fill.bttn-default:hover,
  [data-bs-theme='dark'] .bttn-fill.bttn-default:focus { color: #ffffff; }
  .tippy-tooltip.dark-theme code { color: inherit; background: rgba(127,127,127,0.25);
    padding: 0 4px; border-radius: 3px; font-weight: bold; }
")),
    # switching keyword underlines off leaves colour only
    shiny::tags$script(shiny::HTML("
  $(document).on('shiny:inputchanged', function(e) {
    if (e.name === 'kw_underline') $('body').toggleClass('mrs-colour-only', !e.value);
  });
")),
    title = "metRscreen",
    shiny::actionButton(
      inputId = "citeme",
      style = "background-color: transparent; border-color: transparent; box-shadow: none;",
      label = shiny::tags$img(src = "logo/metRscreen.png", height = "78px", width = "70px")
    ),
    tippy::tippy_this(
      "citeme",
      "Click me!"
    ),
    shinyWidgets::actionBttn(
      inputId = "help",
      label = "Help",
      style = "fill",
      color = "success"
    ),
    # light/dark switch; starts in the same mode as your computer
    bslib::input_dark_mode(id = "dark_mode"),
    bslib::layout_sidebar(
      sidebar =
        bslib::sidebar(
          width = 400,
          position = "left",
          open = "open",
          bslib::card(
            shinyjs::hidden(
              shiny::tagList(
                shiny::div(
                  id = "previous.decisions",
                  shiny::tags$h5(shiny::HTML("<u><b>Previous screen decisions</b></u>")),
                  shiny::htmlOutput("hist.screen"),
                  shiny::htmlOutput("hist.reason"),
                  shiny::htmlOutput("screen.comment"),
                  shiny::htmlOutput("name.screener")
                )
              )
            ),
            shinyjs::hidden(
              shiny::htmlOutput("progress")
            ),
            # collaborative mode: other screeners' decisions, hidden by default
            # so that screening stays independent unless you choose to look
            shinyjs::hidden(
              shiny::div(
                id = "collab.panel",
                shinyWidgets::materialSwitch(
                  inputId = "show.others",
                  label = "Show other screeners' decisions",
                  value = FALSE,
                  status = "primary"
                ),
                shiny::uiOutput("others.decisions")
              )
            )
          ),
          bslib::card(
            shinyjs::hidden(
              shiny::radioButtons(
                inputId = "choose.collab",
                label = shiny::tags$strong("Who is screening?"),
                choices = c(""),
                inline = TRUE
              )
            ),
            # double screening: browse every paper without being able to screen it
            shinyjs::hidden(
              shiny::div(
                id = "browse.panel",
                shinyWidgets::materialSwitch(
                  inputId = "browse.all",
                  label = "Browse all papers (view only)",
                  value = FALSE,
                  status = "warning"
                ),
                shiny::uiOutput("assigned.to")
              )
            ),
            shinyWidgets::checkboxGroupButtons(
              inputId = "show.fields",
              label = character(0),
              choices = c("Title", "Author", "Year", "Journal"),
              status = "primary",
              selected = character(0),
              justified = "TRUE",
              checkIcon = list(
                yes = shiny::icon("square-check"),
                no = shiny::icon("square")
              ),
              size = "sm",
              direction = "horizontal"
            ),
            shiny::sliderInput(
              inputId = "font_size",
              label = "Font size",
              min = 10,
              max = 28,
              value = 14,
              step = 1,
              post = "px",
              ticks = FALSE
            ),
            # keywords can be shown by colour alone, or by colour plus a different
            # underline for each box (easier to tell apart for colour-blind screeners)
            shinyWidgets::materialSwitch(
              inputId = "kw_underline",
              label = "Underline keywords (as well as colour)",
              value = TRUE,
              status = "primary"
            ),
            # each label shows a sample of how that box's matches look (colour and underline)
            lapply(1:5, function(j) {
              colour <- c("Green", "Red", "Purple", "Orange", "Blue")[j]
              shinyWidgets::textInputIcon(
                paste0("search", j),
                shiny::tagList(
                  shiny::tags$span(class = paste0("mrs-kw mrs-kw", j), `aria-hidden` = "true", "Abc"),
                  paste0(" ", colour, " Keyword:")
                ),
                value = or_blank(keywords[[tolower(colour)]])
              )
            }),
            lapply(paste0("search", 1:5), function(id) {
              tippy::tippy_this(
                id,
                paste(
                  "<div style='text-align:left'>",
                  "Separate words with commas: <code>mother, father</code><br>",
                  "<code>*</code> = zero or more letters or numbers: <code>parent*</code> finds parent, parents, parental<br>",
                  "<code>?</code> = exactly one letter or number: <code>wom?n</code> finds woman, women",
                  "</div>"
                ),
                placement = "top",
                arrow = TRUE
              )
            }),
          ),
          bslib::card(
            shiny::splitLayout(
              shinyWidgets::actionBttn(
                inputId = "Previous",
                label = "Previous Study",
                style = "fill",
                color = "default",
                block = T,
                size = "sm"
              ),
              shinyWidgets::actionBttn(
                inputId = "Next",
                label = "Next Study",
                style = "fill",
                color = "default",
                block = T,
                size = "sm"
              )
            )
          )
        ),
      shiny::tags$style(shiny::HTML("
  #decision_buttons .btn-group { width: 100%; }
  #decision_buttons .btn-group .btn { flex: 1; }
")),
      # when there isn't room for both side by side, the list of screened papers
      # moves below the abstract instead of being squeezed into a thin column
      shiny::div(
        style = "display: flex; flex-wrap: wrap; gap: 0 24px;",
        shiny::div(
          style = "flex: 3 1 420px; min-width: 0;",
          shiny::div(
            style = "display: flex; flex-direction: column;",
            # keyboard shortcuts, so people know they exist
            shiny::div(
              id = "shortcut_bar",
              style = "display: flex; flex-wrap: wrap; gap: 4px 14px; align-items: center;
                 font-size: 13px; color: var(--bs-secondary-color); margin-bottom: 6px;",
              shiny::tags$span(shiny::icon("keyboard"), "Shortcuts:"),
              shiny::tags$span(shiny::tags$kbd("←"), " Previous"),
              shiny::tags$span(shiny::tags$kbd("→"), " Next"),
              shiny::tags$span(shiny::tags$kbd("Y"), " Accept"),
              shiny::tags$span(shiny::tags$kbd("N"), " Reject"),
              shiny::tags$span(shiny::tags$kbd("M"), " No Decision")
            ),
            shiny::div(
              id = "abstract_card_wrapper",
              style = "height: 500px; min-height: 150px; overflow-y: auto;",
              bslib::card(
                full_screen = FALSE,
                style = "height: 100%; overflow-y: auto;",
                shinyjs::hidden(shiny::htmlOutput("title")),
                shinyjs::hidden(shiny::htmlOutput("author")),
                shinyjs::hidden(shiny::htmlOutput("year")),
                shinyjs::hidden(shiny::htmlOutput("journal")),
                shiny::htmlOutput("abstract"),
                shiny::htmlOutput("keyword")
              )
            ),
            # drag handle
            shiny::div(
              id = "drag_handle",
              style = "height: 8px; background: var(--bs-border-color); cursor: ns-resize;
                 display: flex; align-items: center; justify-content: center;
                 border-radius: 4px; margin: 4px 0;",
              shiny::tags$small("⋯", style = "color: var(--bs-secondary-color); line-height: 0;")
            ),
            shiny::tags$script(shiny::HTML("
        (function() {
          var handle  = document.getElementById('drag_handle');
          var wrapper = document.getElementById('abstract_card_wrapper');
          var isDragging = false;
          var startY, startH;

          handle.addEventListener('mousedown', function(e) {
            isDragging = true;
            startY = e.clientY;
            startH = wrapper.offsetHeight;
            document.body.style.cursor = 'ns-resize';
            e.preventDefault();
          });

          document.addEventListener('mousemove', function(e) {
            if (!isDragging) return;
            var newH = Math.max(150, startH + (e.clientY - startY));
            wrapper.style.height = newH + 'px';
          });

          document.addEventListener('mouseup', function() {
            isDragging = false;
            document.body.style.cursor = '';
          });
        })();
      ")),
            shiny::uiOutput("view.only"),
            shiny::div(
              id = "decision_buttons",
              shinyWidgets::actionGroupButtons(
                inputIds = c("Reject", "NoDecision", "Accept"),
                labels = list("Reject", "No Decision", "Accept"),
                status = c("danger", "primary", "success"),
                size = "normal"
              )
            ),
            shiny::div(
              style = "margin-top: 10px;",
              shinyjs::hidden(
                shinyWidgets::prettyCheckboxGroup(
                  inputId = "reject.reason",
                  label = NULL,
                  choices = ""
                )
              ),
              shinyjs::hidden(
                shinyWidgets::textInputIcon(
                  "comments",
                  placeholder = "Screening comments",
                  label = NULL
                )
              )
            )
          )
        ),
        shiny::div(
          style = "flex: 1 1 220px; min-width: 0;",
          bslib::card(
            fill = FALSE,
            bslib::card_body(
              max_height = 500,
              style = "overflow-y: auto;",
              shiny::uiOutput("ref_list")
            )
          )
        )
      )
    )
  )
}
