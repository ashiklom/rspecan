library(rspecan)
library(testthat)

l <- list(
  a = list(
    a1 = iris,
    a2 = 1:10
  ),
  b = "hello",
  c = 1:5
)

hf <- tempfile()
list2hdf(hf, list(mygroup = l))
file.remove(hf)

# Test that two R scripts running at the same time can I/O to the same file
if (length(commandArgs(TRUE)) > 0) {
  hf <- ".test.hdf5"
  for (i in 1:50) {
    print(i)
    val <- rnorm(10)
    nms <- paste(sample(letters, 20), collapse = "")
    l <- list(val)
    names(l) <- nms
    list2hdf(hf, l, overwrite = TRUE)
    Sys.sleep(0.5)
  }
}
