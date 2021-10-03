#' Get all acceptable aliases of a unit name.
#'
#'

#' @name get_unit_aliases
#' @param name  [character]  Must have an exact length of 1.
#' @return  [Character] vector.

#' @examples

#'  get_unit_aliases("lb")
#' @export
get_unit_aliases<-function (name)
{
    assert_character(name, len = 1)
    xml <- get_unit_xml(name)
    .get_unit_names <- function(x) {
        one <- xml_find_all(x, xpath = glue(".//aliases//*[text()]")) %>%
            xml_text()
        two <- xml_find_all(x, xpath = glue(".//name//*[text()]")) %>%
            xml_text()
        three <- xml_find_all(x, xpath = glue(".//symbol[text()]")) %>%
            xml_text()
        c(one, two, three) %>% unique()
    }
    .get_unit_names(xml[[1]])
}
