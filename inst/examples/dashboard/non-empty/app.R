library(blockr.core)
library(blockr.ui)

run_demo_app(
  blocks = c(
    a = new_dataset_block("BOD"),
    b = new_dataset_block("ChickWeight"),
    c = new_merge_block("Time")
  ),
  links = c(
    ac = new_link("a", "c", "x"),
    bc = new_link("b", "c", "y")
  ),
  stacks = list(ac = c("a", "c"))
)
