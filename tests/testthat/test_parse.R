library(testthat)
library(dbtprotokoll)

#test_check("dbtprotokoll")

test_speakers <- tibble::tibble("id" = c("11111111", "22222222", "33333333"),
                                "titel" = c(NA_character_, "Dr.", NA_character_),
                                "vorname" = c("Monika", "Martin", "Mauzi"),
                                "nachname" = c("Mustermensch", "Mustermensch", "Mustermensch"),
                                "fraktion" = c(NA_character_, NA_character_, "fraktionslos"),
                                "rolle" = c(TRUE, TRUE, TRUE))
test_paragraphs <- tibble::tibble("id" = 2:7,
                                  "speech" = "ID10000000",
                                  "speech_type" = "speech",
                                  "speaker_id" = "11111111",
                                  "moderator" = NA_character_,
                                  "class" = c("J_1", "J", "O", "T", "T_NaS", "T_Drs"),
                                  "content" = c("Dies ist ein J_1 Element.", "Dies ist ein J Element.", "Dies ist ein O Element.", "Dies ist ein T Element.", "Dies ist ein T_NaS Element.", "Dies ist ein T_Drs Element."),
                                  "j_1_gate" = TRUE)
test_comments <- tibble::tibble("id" = 1,
                                "paragraph_id" = "ID10000000",
                                "content" = "(Dies ist ein Kommentar.)")
test_roles <- tibble::tibble("id" = c("11111111", "22222222", "33333333"),
                             "rolle.rolle_lang" = "Rolle",
                             "rolle.rolle_kurz" = "R")

context("parse_protocols")
test_that("parse minimal.xml", {
  minimal <- parse_protocol("./minimal.xml", check_schema = FALSE)
  expect_type(minimal, "list")
  expect_identical(minimal[[1]], test_speakers)
  expect_identical(minimal[[2]], test_paragraphs)
  expect_identical(minimal[[3]], test_comments)
  expect_identical(minimal[[4]], test_roles)
})

test_that("parse single protocol", {
  expect_error(parse_protocol(NULL, check_schema = FALSE))
  example_prot <- parse_protocol("./19001-data.xml", check_schema = FALSE)
  expect_s3_class(example_prot[[1]], "tbl_df")

  expect_named(example_prot[[1]], c("id", "titel", "vorname", "nachname", "ortszusatz", "fraktion", "rolle"))
  expect_named(example_prot[[2]], c("id", "speech", "speech_type", "speaker_id", "moderator", "class", "content", "j_1_gate"))
  expect_named(example_prot[[3]], c("id", "paragraph_id", "content"))
  expect_named(example_prot[[4]], c("id", "rolle.rolle_lang", "rolle.rolle_kurz"))

  for(i in 1:6){
    expect_type(example_prot[[1]][[i]], "character")
  }
  expect_type(example_prot[[1]]$rolle, "logical")
  expect_type(example_prot[[2]]$id, "integer")
  expect_type(example_prot[[2]]$j_1_gate, "logical")
  for(i in 2:7){
    expect_type(example_prot[[2]][[i]], "character")
  }
  expect_type(example_prot[[3]]$id, "double")
  expect_type(example_prot[[3]]$paragraph_id, "character")
  expect_type(example_prot[[3]]$content, "character")
  for(i in 1:3){
    expect_type(example_prot[[4]][[i]], "character")
  }
})

test_that("parse multiple protocols", {
  expect_identical(parse_protocols(path = "testdataset_multiple_protocols", check_schema = FALSE), parse_protocol("./minimal.xml", check_schema = FALSE))
  expect_error(parse_protocols(start = "minimal.xml", check_schema = FALSE))
})
