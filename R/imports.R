#' @import data.table
#' @import stringr
#' @import units
#' @import glue
#' @import rlang
#' @import R6
#' @import checkmate
#' @import sUtils
#' @import exprTools
#' @import xml2

# This script is intended for imports and .onLoad/.onAttach functions only.  Do not add other functions to this file or remove existing.

.onLoad <- function(libname, pkgname) {
#
#   eni=environment(units::make_units)
#   eni$udunits_exit()
#   eni$udunits_init(file.path(eni$.get_ud_xml_dir(), "udunits2.xml"))
#   if (eni$ud_is_parseable("B"))
#     .default_options$define_bel <- FALSE
#   do.call(eni$units_options, eni$.default_options)
#   if (l10n_info()[["UTF-8"]]) {
#     native <- "utf8"
#   } else if (l10n_info()[["Latin-1"]]) {
#     native <- "latin1"
#   } else {
#     native <-  "ascii"
#   }
#   eni$ud_set_encoding(native)
#   eni$register_all_s3_methods()


}
