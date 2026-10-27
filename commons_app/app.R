# Agentic QC assistant -- {commons} chat app
# Run from the project root:  shiny::runApp("commons_app")
source("agent.R", local = TRUE)
options(shinychat.history_options.store_auto.quiet = TRUE)

greeting <- paste(
  "Ask me about the double-programming QC of the CDISC pilot 3 study.",
  "Answers that run approved company QC code show a **green shield**.",
  "",
  "* <span class=\"suggestion\">Check the primary ADSL against the define.xml. What failed?</span>",
  "* <span class=\"suggestion\">Compare the primary and QC demographics ARDs. What needs my attention?</span>",
  "* <span class=\"suggestion\">Compare the primary and QC Kaplan-Meier figures.</span>",
  "* <span class=\"suggestion\">What is the mean age of subjects over 80 by sex in the primary ADSL?</span>",
  sep = "\n"
)

ui <- shinychat::page_chat(
  "Agentic QC -- CDISC pilot 3 (ADSL, ADTTE, Table 14.1.2, Figure 14.2.1)",
  id = "chat",
  greeting = greeting,
  theme = commons::commons_theme()
)

server <- function(input, output, session) {
  agent <- build_agent()
  commons::commons_server("chat", agent)
}

shiny::shinyApp(ui, server)
