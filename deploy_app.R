rsconnect::setAccountInfo(
  name = Sys.getenv("USERNAME"),
  token = Sys.getenv("TOKEN"),
  secret = Sys.getenv("SECRET")
)

rsconnect::deployApp(
  account = Sys.getenv("USERNAME"),
  appName = "riskassess",
  appTitle = "WHO Seasonal Risk Assessment Tool",
  appFiles = c(
    "app.R",
    "R",
    "www"
  )
)
