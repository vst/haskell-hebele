###################################
## ATLAS DEVELOPMENT ENVIRONMENT ##
###################################

env "dev" {
  ## Development database:
  url = "postgresql://postgres:postgres@localhost:5432/haskell-hebele?sslmode=disable"

  ## Atlas development database (See https://atlasgo.io/concepts/dev-database):
  dev = "docker://postgres/15.3/haskell-hebele-test?search_path=public"

  ## Migrations configuration:
  migration {
    ## Migrations directory:
    dir = "file://migrations"

    ## Migrations format:
    format = atlas
  }
}
