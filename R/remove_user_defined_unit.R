#' Remove a user defined unit.

#' @name remove_user_defined_unit
#' @param unit_name [character]  Must have an exact length of 1.
#' @param remove_dependencies  [logical]  Defaults to FALSE
#' @return  [NULL]

#' @examples

#'  new_base_unit(name = "USD", aliases = c("dollar", "us_dollar"),
#'  symbol = "$", definition = "Official currency of the United States")
#'  new_common_unit(name = "EUR", formula = "1.18*dollar", aliases = "euro",
#'  symbol = "\200", definition = "Official currency of the European Union")
#'  remove_user_defined_unit("USD", remove_dependencies = TRUE)
#' @export
remove_user_defined_unit<-function (unit_name, remove_dependencies = FALSE)
{
    if (!is_unit_user_defined(unit_name))
        stop(glue("\"{unit_name}\" is not user defined"))
    depends <- get_user_defined_unit_dependencies()
    has_depends = sapply(depends, function(x, y) any(y %in% x),
        x = unit_name)
    dependencies = names(has_depends)[has_depends == TRUE]
    dependencies = lapply(dependencies, function(x) str_split(x,
        "\\.")[[1]][2]) %>% unlist()
    if (any(has_depends) & remove_dependencies == F)
        stop(glue("'{unit_name}' has dependencies '{glue_collapse(   dependencies,sep=',')}'. Please set 'remove_dependencies' to TRUE to remove this unit and all dependencies"))
    if (any(has_depends) & remove_dependencies == TRUE)
        lapply(dependencies, remove_user_defined_unit, remove_dependencies = TRUE)
    db <- names(get_unit_xml(unit_name))
    dbxml <- load_units_xml(db)
    tmp = get_unit_xml(unit_name, tree = dbxml[[1]])
    xml_remove(tmp)
    write_units_xml(dbxml)
    suppressWarnings(detach("package:units", unload = TRUE, force = TRUE))
    suppressMessages(suppressWarnings(library(units)))
    print(glue("'{unit_name}' removed from {names(dbxml)} units DB"))
}
