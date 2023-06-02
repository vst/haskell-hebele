schema "public" {
  comment = "Public schema"
}

###################
## TABLE: "user" ##
###################

table "user" {

  #############
  ## SCHEMA ##
  #############

  schema = schema.public

  #############
  ## COLUMNS ##
  #############

  column "id" {
    type    = uuid
    null    = false
    default = sql("gen_random_uuid()")
  }

  column "created_at" {
    type    = timestamptz
    null    = false
    default = sql("now()")
  }

  column "updated_at" {
    type    = timestamptz
    null    = false
    ## TODO: Add trigger to update `updated_at` column when needed.
    default = sql("now()")
  }

  column "email" {
    type = text
    null = false
    unique = true
  }

  column "fullname" {
    type = text
    null = true
  }

  #################
  ## PRIMARY KEY ##
  #################

  primary_key {
    columns = [column.id]
  }

  #################
  ## CONSTRAINTS ##
  #################

  check "non-empty email address" {
    expr = "email <> ''"
  }

  #############
  ## INDICES ##
  #############

  index "idx_user__email" {
    on {
      column = column.email
    }
  }
}

############################
## TABLE: "user_password" ##
############################

table "user_credentials" {

  #############
  ## SCHEMA ##
  #############

  schema = schema.public

  #############
  ## COLUMNS ##
  #############

  column "user_id" {
    type = uuid
    null = false
  }

  column "created_at" {
    type    = timestamptz
    null    = false
    default = sql("now()")
  }

  column "updated_at" {
    type    = timestamptz
    null    = false
    default = sql("now()")
  }

  column "password" {
    type = text
    null = false
  }

  #################
  ## PRIMARY KEY ##
  #################

  primary_key {
    columns = [column.user_id]
  }

  foreign_key "fk_user_id" {
    columns     = [column.user_id]
    ref_columns = [table.user.column.id]
    on_update   = RESTRICT
    on_delete   = RESTRICT
  }

  #################
  ## CONSTRAINTS ##
  #################

  check "non-empty password" {
    expr = "password <> ''"
  }

  #############
  ## INDICES ##
  #############
}
