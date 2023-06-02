-- Create "user" table
CREATE TABLE "user" ("id" uuid NOT NULL DEFAULT gen_random_uuid(), "created_at" timestamptz NOT NULL DEFAULT now(), "updated_at" timestamptz NOT NULL DEFAULT now(), "email" text NOT NULL, "fullname" text NULL, PRIMARY KEY ("id"), CONSTRAINT "non-empty email address" CHECK (email <> ''::text));
-- Create index "idx_user__email" to table: "user"
CREATE INDEX "idx_user__email" ON "user" ("email");
-- Create "user_credentials" table
CREATE TABLE "user_credentials" ("user_id" uuid NOT NULL, "created_at" timestamptz NOT NULL DEFAULT now(), "updated_at" timestamptz NOT NULL DEFAULT now(), "password" text NOT NULL, PRIMARY KEY ("user_id"), CONSTRAINT "fk_user_id" FOREIGN KEY ("user_id") REFERENCES "user" ("id") ON UPDATE RESTRICT ON DELETE RESTRICT, CONSTRAINT "non-empty password" CHECK (password <> ''::text));
