

#' Onset detection
#'
#' @param file_name
#' @param transform_file
#' @param normalise
#' @param hidePrint
#' @param type
#' @param if_bad_result_return_single_na
#' @param tidy_fun
#'
#' @return
#' @export
#'
#' @examples
onset_detection <- function(file_name,
                            transform_file = system.file('transform_files/onset_transform.n3', package = 'vampr'),
                            normalise = 1L,
                            hidePrint = TRUE,
                            type = NULL,
                            if_bad_result_return_single_na = NULL,
                            tidy_fun = tidy_onset_detection) {

  op_sys <- get_os()


  if(op_sys %in% c("osx", "linux", "windows")) {

    set_vamp_variable(op_sys)

    args <- sonic_annotator_construct_args(transform_file, vamp_cmd = "vamp:qm-vamp-plugins:qm-onsetdetector:onsets", file_name, normalise)

    sa_out <- sonic_annotator_construct_command(args, hidePrint, op_sys)

    if(length(sa_out) == 0) {
      # res <- pyin_handle_null(type, file_name)
      res <- NA
    } else {

      res <- read.csv(text = sa_out, header = FALSE) %>%
        tibble::as_tibble()

      res <- tidy_fun(res)

      file_name <- res$V1[[1]]

      res <- res %>% dplyr::select(-V1)

      res <- tibble::tibble(file_name, res)
    }

  } else {
    warning('OS not supported.')
  }


  # cond <- is.null(res$freq) | all(is.na(res$freq))
  # if(if_bad_result_return_single_na & cond) {
  #   res <- NA
  # }
  return(res)
}

tidy_onset_detection <- function(res) {
  res %>%
    dplyr::rename(onset = V2)
}

test_onset_detection <- function() {
  onset_detection(file_name = system.file('test_audio/test_drum.mp3', package = 'vampr') )
}


# t <- test_onset_detection()
