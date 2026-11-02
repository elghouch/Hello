#' Hello World
#'
#' `hello` says _"hello"_ in the user-specified language. The user is asked to give her/his name so that the hello message gets personalized.
#'
#' @param
#' who a `character` vector of length 1 that specifies the name of the person to whom the message is addressed.
#'
#' @param
#' lang a `character` vector of length 1 that specifies the preferred language. Default to "EN" for English. Other possible values include "FR" for French, "IT"  for Italian, "ES" for Spanish, or "DE" for German. Case is ignored.
#'
#' @param
#' LangData an optional data.frame with two columns each of mode `character`. The first column gives the language codes and the second column gives the corresponding "hello" word. Default to `language`.
#'
#' see `?language`
#'
#' @return
#' a `character` vector with a personalized _"hello"_ message.
#'
#' @examples
#' hello("Amelia")
#' hello("Emmanuel", "fR")
#' hello("Sami", "GE")
#' hello("Sami", "GE", LangData = data.frame("ge", "Hallo"))
#' hello(25) |> try()   # Error
#'
#' @export
#'
hello <- function(who, lang = "EN", LangData = language) {
  if (!is.character(who) || !(length(who) == 1)) {
    stop("Please enter a valid 'who'; see ?hello")
  }
  if (!is.character(lang) || !(length(lang) == 1)) {
    stop("Please enter a valid 'lang'; see ?hello")
  }
  if (!is.data.frame(LangData) || !(ncol(LangData) == 2)) {
    stop("Please enter a valid 'LangData'; see ?hello")
  }
  colnames(LangData) <- c("code", "hello")
  if (!is.character(LangData$code) || !is.character(LangData$hello)) {
    stop("Please enter a valid ''LangData'; see ?hello")
  }
  llang <- tolower(lang)
  LangData$code <- tolower(LangData$code)
  hello <- subset(LangData, LangData$code == llang)[1, 2]
  ifelse(
    !is.na(hello),
    stringr::str_c(hello, ", ", who, "!", sep = ""),
    stringr::str_c("Sorry, ", who, ", ", "your language ", "('", lang, "') ", "is not available!", sep = "")
  ) |> cat()
}
