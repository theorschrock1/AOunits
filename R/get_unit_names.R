#' Get a vector of all unit names in the database.
#'
#'

#' @name get_unit_names
#' @param type  [subset]  Possible values: c("base", "derived", "accepted", "common").  Defaults to c("base", "derived", "accepted", "common")
#' @return  [Character] vector.

#' @examples

#'  get_unit_names()
#' @export
get_unit_names<-function (type = c("base", "derived", "accepted", "common","prefixes"))
{
    assert_subset(type, choices = c("base", "derived", "accepted",
        "common","prefixes"))
    all_units = load_units_xml(type)
    .get_unit_names <- function(x) {
        one <- xml_find_all(x, xpath = glue("//aliases//*[text()]")) %>%
            xml_text()
        two <- xml_find_all(x, xpath = glue("//name//*[text()]")) %>%
            xml_text()

        three <- xml_find_all(x, xpath = glue("//symbol//*[text()]")) %>%
            xml_text()
        four<- xml_find_all(x, xpath = glue("//name[text()]")) %>%
            xml_text()
        five<- xml_find_all(x, xpath = glue("//symbol[text()]")) %>%
            xml_text()
        c(one, two, three,four,five) %>% unique()
    }
    lapply(all_units, .get_unit_names) %>% unlist() %>% unique()
}
