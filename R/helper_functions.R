create_new_directory <- function(base_directory, new_directory)
{
    dir.create(file.path(base_directory, new_directory), showWarnings=FALSE)
    return(file.path(base_directory, new_directory))
}

