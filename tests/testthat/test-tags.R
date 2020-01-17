context("Shiny tags")

test_that("functions return shiny tags", {
  x <- list(lcarsBox(), lcarsSweep(), lcarsHeader(), lcarsBracket(),
            lcarsButton("btn1", "Button"), lcarsCheckbox("id1", "Checkbox"),
            lcarsh1(), lcarsh2(), lcarsh3(), lcarsh4(), lcarsh5(), lcarsh6(),
            lcarsPill(), lcarsLeftPill(), lcarsRightPill(), lcarsRect(),
            lcarsWell())
  expect_true(all(sapply(x, class) == "shiny.tag"))

  x <- list(lcarsRadio("id1", "Radio", c("a", "b")),
            lcarsRadioToggle("id1", "Radio Toggle", c("a", "b")),
            lcarsToggle("id1", "Toggle"))
  expect_true(all(sapply(x, function(x) class(x)[1]) == "shiny.tag.list"))
})
