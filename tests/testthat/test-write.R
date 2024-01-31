test_that("write an mgf file", {

  suppressMessages(
    expect_no_error(
      data <- system.file("extdata", "example.ms2.mzML", package = "msreadr") |>
        read_spectra()
    )
  )

  temp_file <- 'temp.mgf'
  suppressMessages(
    expect_no_error(
      data |> write_mgf(temp_file)
    )
  )

  if(file.exists(temp_file)){
    file.remove(temp_file)
  }

})
