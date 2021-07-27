#' @title Roll dice
#'
#' @description Takes a Dungeons & Dragons style dice string, parses it, and
#' rolls it
#'
#' @param dice_string A Dungeons & Dragons style dice string, e.g. "2d4" to roll
#' two four-sided dice and add the result
#' @param verbose Show the details of the roll in the console?
#' @param seed (optional) Random number seed to use
#'
#' @return A number representing the result of the die roll

#' @examples
#' roll("d20")
#' roll("2d8")
#' roll("2d4+1")
#' roll("d6*3", seed = 123)
#' roll("3d6", verbose = TRUE)

roll <- function(dice_string, verbose = FALSE, seed = NULL) {
  p <- parse_dice_string(dice_string)
  p$verbose <- verbose
  if(!is.null(seed)) set.seed(seed)
  do.call(eval_dice, p)
}

parse_dice_string <- function(dice_string) {
  p <- stringr::str_replace(
                  dice_string,
                  "([1-9]\\d*)?d([1-9]\\d*)([/x\\*][1-9]\\d*)?([+-]\\d+)?",
                  c("\\1", "\\2", "\\3", "\\4"))
  list(
    number = suppressWarnings(as.integer(p[1])),
    die = suppressWarnings(as.integer(p[2])),
    times = ifelse(p[3] == "", NA, p[3]),
    plus = ifelse(p[4] == "", NA, p[4])
  )
}

parse_modifier <- function(modifier_string) {
  p <- stringr::str_replace(
                  modifier_string,
                  "([+-/x\\*])([1-9]\\d*)",
                  c("\\1", "\\2"))
  list(
    fun = switch(p[1],
                 "+" = `+`,
                 "-" = `-`,
                 "x" = `*`,
                 "*" = `*`,
                 "/" = `/`),
    value = suppressWarnings(as.integer(p[2]))
  )
}

eval_dice <- function(die, number, times, plus, verbose) {
  if(is.na(number)) number <- 1
  roll_vector <- sample(1:die, number, replace = TRUE)
  console_output <- paste(roll_vector, collapse = "+")
  roll <- sum(roll_vector)
  if(!is.na(times)) {
    m <- parse_modifier(times)
    roll <- m$fun(roll, m$value)
    console_output <- paste0("(", console_output, ")", times)
  }
  if(!is.na(plus)) {
    m <- parse_modifier(plus)
    roll <- m$fun(roll, m$value)
    console_output <- paste0(console_output, plus)
  }
  if(verbose) message(console_output)
  roll
}
