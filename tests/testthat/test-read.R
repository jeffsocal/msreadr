test_that("read mzml and mgf files", {

  suppressMessages(
    expect_no_error(
      system.file("extdata", "example.ms2.mzML", package = "msreadr") |>
        read_spectra()
    )
  )

  suppressMessages(
    expect_no_error(
      system.file("extdata", "example.ms2.mgf", package = "msreadr") |>
        read_spectra()
    )
  )

})
