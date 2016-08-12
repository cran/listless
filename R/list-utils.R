# This is low-level support stuff that eventually belongs somewhere else.
# Hopefully in https://github.com/renkun-ken/rlist, so we don't have to
# maintain it ourselves.

#' Get the depth of a list
#'
#' Gets the depth of a list (at its deepest point).
#'
#' @param l A variable, probably a list.
#' @param prune_empty_elts A logical value.  Should empty elements be pruned
#' without counting them?
#' @return A non-negative integer of the deepest depth of the list.
#' @seealso This is loosely based upon \code{list.depth} (internal to older
#' versions of the \code{Zelig} package), but gives different answers.
#' @examples
#' list_depth(list(1))
#' list_depth(list(1, list(2:3, 4:6)))
#'
#' # Atomic variables have depth 0
#' list_depth(1)
#'
#' # Empty elements can be pruned before counting
#' list_depth(list())
#' list_depth(list(), prune_empty_elts = TRUE)
#' @export
list_depth <- function (l, prune_empty_elts = FALSE)
{
  if(prune_empty_elts && length(l) == 0L)
  {
    return(0L)
  }
  if(!is.list(l) || is.atomic(l))
  {
    return(0L)
  }
  n <- vapply(l, list_depth, integer(1L), prune_empty_elts = prune_empty_elts)
  1L + max(n, 0L)
}

#' Get the names of a list
#'
#' Recursively gets the names of a list.
#' @param l A variable, probably a list.
#' @param sep A string to separate parts of the name.
#' @return A character vector, of the same number of elements as \code{l}.
#' @seealso Similar to \code{\link[base]{names}(\link[base]{unlist}(l))}, but
#' elements aren't numbered, and you can chose the separator.
#' @examples
#' \donttest{
#' (l <- list(
#'   a = 1,
#'   2:3,                             # missing names are blank
#'   c = list(ca = 4:6, 7:10, list(cca = 11:15)),
#'   d = list()                       # empty elt's silently ignored
#' ))
#' list_names(l)
#'
#' # For comparison
#' names(unlist(l))
#' }
#' @noRd
list_names <- function(l, sep = "|")
{
  if(!is.list(l))
  {
    warning("Coercing 'l' to a list.")
    l <- as.list(l)
  }
  nms <- list_names0(l, sep)
  substring(nms, 1, nchar(nms) - 1)
}

list_names0 <- function(l, sep = "|")
{
  if(!is.list(l) || is.atomic(l) || length(l) == 0L)
  {
    nms <- if(is.null(names(l)))
    {
      character(length(l))
    } else
    {
      names(l)
    }
    return(nms)
  }
  n <- vapply(
    l,
    function(x)
    {
      length(unlist(x, use.names = FALSE))
    },
    integer(1)
  )
  paste(
    rep(names(l), n),
    do.call(c, lapply(l, list_names0)),
    sep = sep
  )
}

#' Convert a list to a data frame
#'
#' Converts a list to a data frame, with names in multiple columns.
#' @param l A variable, probably a list.
#' @param name_variables A string. What should the columns formed from
#' the names of \code{l} be called?
#' @param values_variable A string. What should the columns formed from
#' the values of \code{l} be called?
#' @param stringsAsFactors Should character columns be converted to factors?
#' @return \code{\link[base]{data.frame}}.
#' @examples
#' (l <- list(
#'   a = 1,
#'   2:3,                             # missing names are blank
#'   c = list(ca = 4:6, 7:10, list(cca = 11:15)),
#'   d = list()                       # empty elt's silently ignored
#' ))
#' list_to_data.frame(l)
#'
#' # Custom column names
#' list_to_data.frame(l, c("group", "subgroup", "element"), "amount")
#' @importFrom tidyr separate
#' @export
list_to_data.frame <- function(l, name_variables = "names",
  values_variable = "values", stringsAsFactors = getOption("stringsAsFactors"))
{
  ul <- unlist(l, use.names = FALSE)
  if(is.null(ul))
  {
    ul <- logical()
  }
  d <- data.frame(
    names            = list_names(l),
    values           = ul,
    stringsAsFactors = stringsAsFactors,
    check.names      = FALSE, # not necessary
    check.rows       = FALSE  # not necessary
  )
  colnames(d)[2] <- values_variable

  depth <- list_depth(l)
  name_variables <- make.unique(rep_len(name_variables, depth))

  # separate_ fails with zero row data frames
  # https://github.com/hadley/tidyr/issues/100
  if(nrow(d) == 0)
  {
    colnames(d)[1] <- name_variables
    return(d)
  }
  tidyr::separate_(
    d,
    "names",
    name_variables,
    sep = "\\|",
    extra = "drop",
    fill  = "right"
  )
}

#' Summarise the structure of a list
#'
#' Summarise the structure of a list, in a tidy data frame.
#' @param l A variable, probably a list.
#' @param name_variables A string. What should the columns formed from
#' the names of \code{l} be called?
#' @param stringsAsFactors Should character columns be converted to factors?
#' @return \code{\link[base]{data.frame}}.
#' @seealso \code{\link[utils]{str}}, \code{\link{list_to_data.frame}}
#' @examples
#' l <- list(
#'   a = 1,
#'   b = matrix(1:6, 2),
#'   c = list(
#'     ca = y ~ x,
#'     list(cba = median, cbb = quote(1 + 1), cbc = expression(1 + 1)),
#'     cc = list(cca = as.name("xyz"))
#'   ),
#'   d = array(1:24, 2:4)
#' )
#' list_str(l)
#' @importFrom magrittr %>%
#' @importFrom tidyr separate
#' @export
list_str <- function(l, name_variables = "names",
                     stringsAsFactors = getOption("stringsAsFactors"))
{
  classes <- rapply(l, class)
  modes <- rapply(l, mode)
  lengths <- rapply(l, length)
  dims <- rapply(l, function(x) toString(dim(x))) %>%
    strsplit(", ", fixed = TRUE) %>%
    lapply(as.integer)

  d <- data.frame(
    names            = names(classes),
    class            = unname(classes),
    mode             = unname(modes),
    length           = unname(lengths),
    stringsAsFactors = stringsAsFactors,
    check.names      = FALSE, # not necessary
    check.rows       = FALSE  # not necessary
  )
  # Have to add list column after data.frame creation
  d$dims <- unname(dims)

  depth <- list_depth(l)
  name_variables <- make.unique(rep_len(name_variables, depth))

  # separate_ fails with zero row data frames
  # https://github.com/hadley/tidyr/issues/100
  if(nrow(d) == 0)
  {
    colnames(d)[1] <- name_variables
    return(d)
  }
  tidyr::separate_(
    d,
    "names",
    name_variables,
    sep = "\\.",
    extra = "drop",
    fill  = "right"
  )
}
