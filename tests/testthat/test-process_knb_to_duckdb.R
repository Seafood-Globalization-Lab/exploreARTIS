# This test checks whether the DuckDB database is created correctly from dummy CSV files

test_that("DuckDB is created with expected tables", {
  skip_on_cran()  # Skip this test on CRAN servers (they may block filesystem operations)
  skip_if_not_installed("duckdb")  # Skip if DuckDB is not installed

  # ---- STEP 1: Set up ----
  # Create a temporary folder to store test files (auto-cleaned after test runs)
  temp_dir <- tempdir()

  # Define a path for the temporary DuckDB file
  db_path <- file.path(temp_dir, "test.duckdb")

  # ---- STEP 2: Create dummy input files ----
  # Create a small dummy dataframe that mimics the structure of trade and consumption data
  dummy_df <- data.frame(
    hs_version = c("HS_2012", "HS_2012", "HS_2017"),  # Include filterable and non-filterable versions
    year = c(2018, 2019, 2020),                      # Include filterable and non-filterable years
    value = c(10, 20, 30)
  )

  # Write trade.csv and consumption.csv to temp_dir
  write.csv(dummy_df, file.path(temp_dir, "trade.csv"), row.names = FALSE)
  write.csv(dummy_df, file.path(temp_dir, "consumption.csv"), row.names = FALSE)

  # Create the 5 additional small CSV files required by your function
  lapply(c("baci", "code_max_resolved", "countries", "products", "sciname"), function(name) {
    write.csv(data.frame(col1 = 1:3), file.path(temp_dir, paste0(name, ".csv")), row.names = FALSE)
  })

  # ---- STEP 3: Run the function being tested ----
  # Use helper function directly (recommended), with filtering enabled
  create_duckdb_from_files(
    folder = temp_dir,
    db_path = db_path,
    artis_custom_timeseries = TRUE,
    hs_version_filter = "HS_2012",
    year_filter = c(2018, 2019)
  )

  # ---- STEP 4: Assertions ----

  # Check that the database file was created
  expect_true(file.exists(db_path))

  # Connect to the DuckDB database
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

  # Get a list of all tables in the database
  tables <- DBI::dbListTables(con)

  # Make sure all expected tables exist
  expected_tables <- c("trade", "consumption", "baci", "code_max_resolved", "countries", "products", "sciname", "metadata")
  expect_true(all(expected_tables %in% tables))

  # Check that the 'trade' table only includes filtered years and hs_version
  trade <- DBI::dbReadTable(con, "trade")
  expect_true(all(trade$year %in% c(2018, 2019)))        # Only filtered years
  expect_true(all(trade$hs_version == "HS_2012"))        # Only filtered HS version

  # Check that metadata table has a description column
  meta <- DBI::dbReadTable(con, "metadata")
  expect_true("table_description" %in% colnames(meta))

  # Close connection to DB
  DBI::dbDisconnect(con)
})
