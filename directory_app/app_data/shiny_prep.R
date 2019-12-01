#Create a search panel
checkboxInput("smooth", "Smooth"),
conditionalPanel(
  condition = "input.smooth == true",
  selectInput("smoothMethod", "Method",
              list("lm", "glm", "gam", "loess", "rlm"))
)