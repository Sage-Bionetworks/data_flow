# SET UP OAUTH
oauth_client <- yaml::yaml.load_file("oauth_config.yml")

client_id <- toString(oauth_client$client_id)
client_secret <- toString(oauth_client$client_secret)
app_url <- toString(oauth_client$app_url)

if (is.null(client_id) || nchar(client_id) == 0) stop("missing DCA_CLIENT_ID environmental variable")
if (is.null(client_secret) || nchar(client_secret) == 0) stop("missing DCA_CLIENT_SECRET environmental variable")
if (is.null(app_url) || nchar(app_url) == 0) stop("missing DCA_APP_URL environmental variable")

# update port if running app locally
if (interactive()) {
  port <- httr::parse_url(app_url)$port
  if (is.null(port)) stop("running locally requires a TCP port that the application should listen on")
  options(shiny.port = as.numeric(port))
}

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth code is present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

app <- httr::oauth_app("shinysynapse",
                       key = client_id,
                       secret = client_secret,
                       redirect_uri = app_url
)

# These are the user info details ('claims') requested from Synapse:
claims <- list(
  family_name = NULL,
  given_name = NULL,
  email = NULL,
  email_verified = NULL,
  userid = NULL,
  orcid = NULL,
  is_certified = NULL,
  is_validated = NULL,
  validated_given_name = NULL,
  validated_family_name = NULL,
  validated_location = NULL,
  validated_email = NULL,
  validated_company = NULL,
  validated_at = NULL,
  validated_orcid = NULL,
  company = NULL
)

claimsParam <- jsonlite::toJSON(list(id_token = claims, userinfo = claims))
api <- httr::oauth_endpoint(
  authorize = paste0("https://signin.synapse.org?claims=", claimsParam),
  access = "https://repo-prod.prod.sagebase.org/auth/v1/oauth2/token"
)

# The 'openid' scope is required by the protocol for retrieving user information.
scope <- "openid view download modify"
