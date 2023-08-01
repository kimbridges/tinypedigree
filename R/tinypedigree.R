
#' Plot a Pedigree Diagram
#'
#' @param data The main data file with ID,dad,mom,gender and other cols.
#' @param links The file with id1 and id2 linking married without kids.
#' @param fold Break names at blanks.
#' @param symbolsize Adjust the size of all symbols.
#' @param textsize Adjust the size of all text.
#'
#' @return A pedigree plot.
#' @export
#'
#' @examples
#' tiny_pedigree(data=data,links=links,symbolsize=0.8,textsize=0.7,fold=FALSE)
tiny_pedigree <- function(data, links=NULL,
                          fold=TRUE,
                          symbolsize=1.8,
                          textsize=0.8){

  ## Convert all the data fields to lowercase
  names(data) <- tolower(names(data))

  ## Rename from old or alternative notation
  if("status" %in% colnames(data)) {data$stroke <- data$status}
  if("dead" %in% colnames(data))   {data$stroke <- data$dead}
  if("fill"   %in% colnames(data)) {data$hilite <- data$fill}
  if("sex"    %in% colnames(data)) {data$gender <- data$sex}
  if("father" %in% colnames(data)) {data$dad    <- data$father}
  if("mother" %in% colnames(data)) {data$mom    <- data$mother}
  if("highlight" %in% colnames(data)) {data$hilite <- data$highlight}

  ## Check the data fields; add the default values if not present
  "%!in%" <- Negate("%in%") ## create the operator
  if("color"  %!in% colnames(data)) {data$color  <- "black"}
  if("stroke" %!in% colnames(data)) {data$stroke <- 0}
  if("hilite" %!in% colnames(data)) {data$hilite <- 0}

  ## Convert yes/no to binary values
  data$affected <- NULL
  data$hilite <- tolower(as.character(data$hilite))
  data$affected <- dplyr::case_when(
    data$hilite == "yes"   ~ 1,
    data$hilite == "no"    ~ 0,
    data$hilite == "true"  ~ 1,
    data$hilite == "false" ~ 0,
    data$hilite == "1"     ~ 1,
    data$hilite == "0"     ~ 0)

  data$status   <- NULL
  data$stroke   <- tolower(as.character(data$stroke))
  data$status   <- dplyr::case_when(
    data$stroke == "yes"   ~ 1,
    data$stroke == "no"    ~ 0,
    data$stroke == "true"  ~ 1,
    data$stroke == "false" ~ 0,
    data$stroke == "1"     ~ 1,
    data$stroke == "0"     ~ 0)

  ## Put in a line break to stack the names
  if(isTRUE(fold)){
    data$id   <- gsub(" ","\n",data$id)
    data$dad  <- gsub(" ","\n",data$dad)
    data$mom  <- gsub(" ","\n",data$mom)}
  if(isTRUE(fold)&&!is.null(links)){
    ## Make sure the id1 and id2 are lower case
    names(links) <- tolower(names(links))
    links$id1 <- gsub(" ","\n",links$id1)
    links$id2 <- gsub(" ","\n",links$id2)
  }

  ## Generate the pedigree data structure (without no-children couples)
  if(is.null(links)){ped <- kinship2::pedigree(id = data$id,
                            dadid    = data$dad,
                            momid    = data$mom,
                            sex      = data$gender,
                            status   = data$status)} else {

    ## Generate the pedigree data structure (with no-children couples)
                            ## Create the code (4 for spouse)
                            links$code <- 4
                            ## Make sure the id1 and id2 are lower case
                            names(links) <- tolower(names(links))
                            ## Process the pedigree
                            ped <- kinship2::pedigree(id       = data$id,
                                            dadid    = data$dad,
                                            momid    = data$mom,
                                            sex      = data$gender,
                                            status   = data$status,
                                            relation = links) }

  ## Number of rows
  n <- nrow(data)

  ## Plot the pedigree chart
  plot(ped,
       cex        = textsize,
       symbolsize = symbolsize,
       col        = data$color,
       density    = c(-1, -1, -1, -1), ## all sections solid!
       affected   = data$affected)

} ## End function tiny_pedigree
