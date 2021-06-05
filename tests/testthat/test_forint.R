test_that("forint() formats numbers correctly", {
          expect_equal(forint(42), "42 HUF")
          expect_equal(forint(420000), "420,000 HUF")
          expect_equal(forint(420.420), "420.42 HUF")
          })

test_that("forint() returns on error if x is non-numeric", {
          expect_error(forint("abc"), "Assertion on 'x' failed: Must be of type 'numeric', not 'character'.")
  })
