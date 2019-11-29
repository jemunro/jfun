
#' @importFrom magrittr "%>%"
NULL

format_int <- function(x, min_width = 1L) {

  stopifnot(rlang::is_integerish(x))

  width <- max(min_width, 1 + floor(log10(max(x))) )
  stringr::str_pad(x, pad = "0",side = 'left', width = width)
}

spread_matrix <- function(data, row, col, val) {

  stopifnot(is.data.frame(data))

  row <- dplyr::enquo(row)
  col <- dplyr::enquo(col)
  val <- dplyr::enquo(val)

  dplyr::select(data, !!row, !!col, !!val) %>%
    tidyr::spread(!!col, !!val)  %>%
    { magrittr::set_rownames(as.matrix(dplyr::select(., -!!row)),
                             dplyr::pull(., !!row))
    }
}

# convert matrix row col index to vector index
inv_arr_ind <- function(row_idx, col_idx, nrow) {
  (col_idx - 1L) * nrow + row_idx
}

# all pairwise interger combinations
comb2_int <- function(x, names = c('a', 'b')) {
  if (length(x) == 1) {
    n <- x
  } else {
    n <- length(x)
  }
  n <- as.integer(n)
  stopifnot(n > 1)
  dplyr::tibble( a = rep(seq_len(n-1), seq.int(n-1, 1)) ) %>%
    dplyr::mutate(
      b = {
        i <- seq_along(a) + 1
        o <- c(0, cumsum((n-2):1))
        (i - o[a]) %>% as.integer() }) %>%
    {
      if (length(x) == 1) {
        .
      } else {
        dplyr::mutate(., a = x[a], b = x[b])
      }
    } %>%
    magrittr::set_colnames(names)
}


hue_tonal <- function(n, l1=70, l2=40, h=c(0, 360), phase=1) {

  h1 <- scales::hue_pal(h=h, l=l1)(n)
  h2 <- scales::hue_pal(h=h, l=l2)(n)
  r <- character(n)

  if(phase == 1){
    r[seq.int(1, n, 2)] <- h1[seq.int(1, n, 2)]
    if (n > 1){
      r[seq.int(2, n, 2)] <- h2[seq.int(2, n, 2)]
    }
  } else {
    r[seq.int(1, n, 2)] <- h1[seq.int(2, n, 2)]
    if (n > 1) {
      r[seq.int(2, n, 2)] <- h2[seq.int(1, n, 2)]
    }
  }
  r
}
