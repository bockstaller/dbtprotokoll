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

context("minimal.xml")
test_that("parse minimal.xml", {
  setwd("../")
  expect_error(parse_protocol(NULL))
  minimal <- parse_protocol("tests/minimal.xml")
  expect_type(minimal, "list")
  expect_identical(minimal[[1]], test_speakers)
  expect_identical(minimal[[2]], test_paragraphs)
  expect_identical(minimal[[3]], test_comments)
  expect_identical(minimal[[4]], test_roles)
})
