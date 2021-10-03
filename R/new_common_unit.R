#' Create a new common unit.
#'
#' Common units have formulas derived from base units.

#' @name new_common_unit
#' @param name  [character]  Must have an exact length of 1.
#' @param aliases  [character]  NULL is ok.
#' @param symbol  [character]  NULL is ok.  Defaults to NULL
#' @param formula  [character]  Must have an exact length of 1.
#' @param definition  [character]  Must have an exact length of 1.  NULL is ok.  Defaults to NULL
#' @return  [Numeric] class unit

#' @examples

#'  new_base_unit(name = "USD", aliases = c("dollar", "us_dollar"),
#'  symbol = "$", definition = "Official currency of the United States")
#'  new_common_unit(name = "EUR", formula = "1.18*dollar", aliases = "euro",
#'  symbol = "\200", definition = "Official currency of the European Union")
#'  remove_user_defined_unit("USD", remove_dependencies = TRUE)

#' @export
new_common_unit<-function (name, formula, aliases, symbol = NULL, definition = NULL)
{
    assert_character(name, len = 1)
    assert_character(aliases, null.ok = TRUE)
    assert_character(symbol, null.ok = TRUE)
    assert_character(formula, len = 1)
    assert_character(definition, len = 1, null.ok = TRUE)
    sym = ""
    ntest = c(name, aliases, symbol) %in% get_unit_names()
    all_names = c(name, aliases)
    plural_test <- unique(str_remove(all_names, "s$"))
    if (l(all_names) != l(plural_test))
        stop("Do not include plual aliases or names")
    if (any(ntest)) {
        stop(glue("name or symbol \"{glue_collapse(c(name,aliases,symbol)[ntest],\",\")}\" already exists in DB"))
    }
    if (!is.null(symbol))
        sym <- glue("<symbol>{symbol}</symbol>")
    formula = parse_expr(formula)
    names_in_formula <- expr_find_names(formula)
    for (i in names_in_formula) {
        expr(set_units(1, !!i)) %>% eval()
    }
    def = ""
    alias = ""
    name_main = glue("<name><singular>{name}</singular></name>")
    if (!is.null(definition))
        def <- glue("<definition>{definition}</definition>")
    if (!is.null(aliases)) {
        a_names = glue_collapse(glue("<name><singular>{aliases}</singular></name>"))
        alias <- glue("<aliases>\n           {a_names}\n         </aliases>")
    }
    uxml <- load_units_xml(type = "common")$common
    formula_in = str_remove_all(deparse(formula), "\\s+")
    new_node <- read_xml(glue("<unit>\n  <def>{formula_in}</def>\n   {sym}{name_main}{alias}{def}\n  <userdefined/>\n  </unit>"))
    xml_add_child(uxml, new_node)
    write_units_xml(list(common = uxml))
    suppressWarnings(detach("package:units", unload = TRUE, force = TRUE))
    suppressMessages(suppressWarnings(library(units)))
    expr(set_units(1, !!name[1])) %>% eval()
}
