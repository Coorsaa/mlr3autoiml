test_that("guide_workflow returns actionable structure", {
  auto = get_auto_iris(quick_start = FALSE)
  result = auto$result

  g1 = mlr3autoiml::guide_workflow(result, max_actions = 5L)
  g2 = auto$guide(max_actions = 5L)
  g3 = result$guide(max_actions = 5L)

  expect_true(is.list(g1))
  expect_true(all(c(
    "summary", "actions", "recommended_plots",
    "trust_summary", "model_story", "reader_questions"
  ) %in% names(g1)))
  expect_true(data.table::is.data.table(g1$summary))
  expect_true(data.table::is.data.table(g1$actions))
  expect_true(data.table::is.data.table(g1$trust_summary))
  expect_true(data.table::is.data.table(g1$model_story))
  expect_true(data.table::is.data.table(g1$reader_questions))
  expect_true(is.character(g1$recommended_plots))

  expect_true(data.table::is.data.table(g2$actions))
  expect_true(data.table::is.data.table(g3$actions))
  expect_lte(nrow(g1$actions), 5L)
  expect_true(all(c("performance_trust", "simple_vs_complex", "next_steps") %in% g1$reader_questions$question_id))
})
