
check_promis <- function(x) {
  tryCatch(
    if (x < 20 | x > 80) {
      stop()
    } else {
      on.exit()
    }, error = function(e) {
      warning("PROMIS score outside of bounds, returning NA",
              call. = F)
      return(NA_character_)
    }
  )
}

possible_promis_names <-
  list(
    physfx = c("physfx", "Physical function"),
    paininter = c("paininter"),
    paininten = c("paininten"),
    gphys = c("gphys"),
    gment = c("gment"),
    dep = c("dep")
  )

standardize_promis <- function(domain) {
  . <- lapply(possible_promis_names, function(x) {
    stringr::str_which(string = domain,
                       pattern = x)
  })
  . <- lapply(., length)
  . <- names(which(. == 1))
  return(.)
}

#' Create pretty PROMIS labels
#'
#' @param domain the PROMIS domain, a string.
#'
#' @return the prettified domain lable, a string.
#' @export
#'
#' @examples
#' pretty_promis_labels("physfx")
pretty_promis_labels <- function(domain) {
  sdomain <- standardize_promis(domain)

  if (sdomain == "physfx") "Physical function"
  else if (sdomain == "paininter") "Pain interference"
  else if (sdomain == "paininten") "Pain intensity"
  else if (sdomain == "gphys") "Global physical health"
  else if (sdomain == "gment") "Global mental health"
  else if (sdomain == "dep") "Depression"
}

#' Function factory for PROMIS score clinical labels
#'
#' @param domain the PROMIS domain, a string.
#'
#' @return  a function that takes as an input a PROMIS score and outputs the clinical interpretation for the specific domain.
#' @export
#'
#' @examples
#' promis_domains("physfx")(42)
promis_domains <- function(domain) {

  sdomain <- standardize_promis(domain)

  if (sdomain == "physfx") {
    function(x) {
      . <- check_promis(x)
      if (!is.null(.)) return(.)

      if (x >= 45) "Normal limits"
      else if (x >= 40) "Mild"
      else if (x >= 30) "Moderate"
      else if (x < 30) "Severe"
    }
  }

  else if (sdomain == "paininter") {
    function(x) {
      . <- check_promis(x)
      if (!is.null(.)) return(.)

      if (x <= 55) "Normal limits"
      else if (x <= 60) "Mild"
      else if (x <= 70) "Moderate"
      else if (x > 70) "Severe"
    }
  }

  else if (sdomain == "paininten") {
    function(x) {
      . <- check_promis(x)
      if (!is.null(.)) return(.)

      if (x <= 55) "Normal limits"
      else if (x <= 60) "Mild"
      else if (x <= 70) "Moderate"
      else if (x > 70) "Severe"
    }
  }

  else if (sdomain == "gphys") {
    function(x) {
      . <- check_promis(x)
      if (!is.null(.)) return(.)

      if (x >= 58) "Excellent"
      else if (x >= 50) "Very good"
      else if (x >= 42) "Good"
      else if (x >= 35) "Fair"
      else if (x < 35) "Poor"
    }
  }

  else if (sdomain == "gment") {
    function(x) {
      . <- check_promis(x)
      if (!is.null(.)) return(.)

      if (x >= 56) "Excellent"
      else if (x >= 48) "Very good"
      else if (x >= 40) "Good"
      else if (x >= 29) "Fair"
      else if (x < 29) "Poor"
    }
  }

  else if (sdomain == "dep") {
    function(x) {
      . <- check_promis(x)
      if (!is.null(.)) return(.)

      if (x <= 55) "Normal limits"
      else if (x <= 60) "Mild"
      else if (x <= 70) "Moderate"
      else if (x > 70) "Severe"
    }
  }
}

#' Apply clinical labels to PROMIS scores
#'
#' @param domain the PROMIS domain, a string.
#' @param score the appropriate PROMIS domain score, a scalar.
#'
#' @return
#' @export
#'
#' @examples
#' label_promis("physfx", 42)
label_promis <- function(domain, score) {
  sdomain <- standardize_promis(domain)
  switch(sdomain,
         physfx = promis_domains("physfx")(score),
         paininter = promis_domains("paininter")(score),
         paininten = promis_domains("paininten")(score),
         gphys = promis_domains("gphys")(score),
         gment = promis_domains("gment")(score),
         dep = promis_domains("dep")(score))
}



