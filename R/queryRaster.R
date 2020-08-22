#' Query the FAO database for a specific raster product
#'
#' This function is used to query a specific raster product in the FAO Gismanager.
#' Because data sets within the database vary greatly bettween the different
#' collection this function uses an easily abstractable pattern for the query.
#' This scheme follows the simple pattern of **what** you want to download,
#' **when** is the timeperiod you are interested in, and **where** is the region
#' you want to download.
#'
#' @param collection A length one charachter vector indication the collection where
#'   the queried product is found.
#' @param outdir A length one character vector with an existing directory. Any output
#'   files will be written here.
#' @param sleep_time A numeric value indicating the length of time R sleeps
#'   when checking for the availability of the result in seconds. Depending
#'   on the size of the queried raster (e.g. global extent at high spatial resolution)
#'   it might be a good idea to increase the sleep time. Defaults to 10 seconds.
#' @param product A length one character vector indicating the desired product to download.
#' @param dimensions A list object specifying the dimensions of the product expect
#'   of the time and measurment dimensions. Any other dimensions needs to be specified
#'   in the form of \code{dimensions = list(KEY = "value")}. In cases of products
#'   with only the temporal and measurement dimension an empty list should be handed
#'   to the function. This is also the Default.
#' @param aoi An sf object which bounding box is used to clip the raster extent.
#'   This is only relevant for regional or global datasets when clipping is desired.
#'   Simply omit this option when you want to download a complete dataset (e.g.
#'   a global dataset or a very local dataset such as WAPOR L3 data). The function
#'   will issue a warning in these cases that the extent was set to global/regional.
#' @param begin A length one character in the form of \code{"YYYY-MM-DD"} or an
#'   Date object in the same format. This time parameter is inclusive meaning that
#'   all timestemps equal or higher than \code{begin} will be included.
#' @param end A length one character in the form of \code{"YYYY-MM-DD"} or an
#'   Date object in the same format. This parameter is exclusive meaning that
#'   all timestemps lower thatn \code{end} will be included.
#' @param APIkey A length one charachter containing your API key. Can be obtained
#'   in the profile section at \href{https://wapor.apps.fao.org}{https://wapor.apps.fao.org}.
#' @param cutline A logical indicating whether the resulting raster should be cut
#'   to the bounding box of the \code{aoi} object. Only relevant when aoi is specified.
#' @param tiled Logical whether the resulting raster file should be tiled.
#' @param compressed Logical to indicate if the resulting raster should be compressed
#'   using LZW.
#' @param overviews Logical to indicate if overviews for the resulting raster should
#'   be created.
#'
#' @return nothing
#' @export wapor_queryRaster
#'
#' @importFrom sf st_bbox
#' @importFrom httr POST GET add_headers content write_disk
#' @importFrom jsonlite toJSON
#' @importFrom stringr str_replace_all str_sub
wapor_queryRaster <- function(collection = NULL,
                              product = NULL,
                              dimensions = list(),
                              aoi = NULL,
                              begin = NULL,
                              end = NULL,
                              APIkey = NULL,
                              outdir = ".",
                              cutline = FALSE,
                              tiled = FALSE,
                              compressed = FALSE,
                              overviews =  FALSE,
                              sleep_time = 10){


  ## prepare inputs
  # raster parameters
  cutline = ifelse(cutline, "true", "false")
  tiled = ifelse(tiled, "true", "false")
  compressed = ifelse(compressed, "true", "false")
  overviews = ifelse(overviews, "true", "false")

  if(is.null(APIkey)) stop("Please specify an API Key. Visit your profile at https://wapor.apps.fao.org/ to obtain one.")
  if(any(is.null(collection), is.null(product))) stop("Either no collection or product was specified")
  if(!class(dimensions) == "list") stop("Option 'dimensions' needs to be a list.")
  if(!is.null(aoi)){
    if(!any(class(aoi) %in% "sf")){
      stop("Option 'aoi' needs to be an sf object.")
    } else {
      extent = st_bbox(aoi)
    }
  } else {
    warning("No aoi object was specifed. Setting the query extent to global/local.")
    extent = c(xmin = -90, ymin = -180, xmax = 90, ymax = 180)
  }
  # initiate vector of temporal variables
  tmp_vars = c("YEAR", "MONTH", "DEKAD")

  # get product meta data
  metadata = wapor_productMETA(collection, product)
  dim_names = metadata$dimensions$code
  if(!any(dim_names %in% tmp_vars)){
    if(!any(is.null(begin), is.null(end))){
      warning("The selected product has no temporal dimension. However either start or end date was specified.
              Will continue to download the requested product but dropping the temporal inputs.")
      begin = NULL
      end = NULL
    }
  } else {
    if(any(is.null(begin), is.null(end))){
      stop("The requested product has a time dimension. However, either start or end date was not specified.")
    } else {
      if(!any(class(begin) %in% c("Date","character"), class(end) %in% c("Date","character"))){
        stop("Wrong class specifed for either begin or end. Must be of class 'Date' or 'character'")
      }
    }
  }

  dim_names = dim_names[!dim_names %in% tmp_vars]
  if(length(dim_names) > 0){
    if(!any(names(dimensions) %in% dim_names)){
      stop("There are non matching dimensions specified in the 'dimensions' option.
         Check the dimension output of wapor_productMETA() and change your input.")
    }
  }
  measure = metadata$info$code

  # get requested timesteps in case there is a time variable
  if(!is.null(begin)){
    temp_dim = tmp_vars[tmp_vars %in% metadata$dimensions$code]
    url = paste(dataurl, collection, "cubes", product, "dimensions", temp_dim, "members", sep = "/")
    parsed = get_and_parse(url)
    timesteps = unlist(lapply(parsed, function(x) x$code))
    timesteps_low = as.Date(str_sub(timesteps,2,11))
    timesteps_high = as.Date(str_sub(timesteps,13, 22))
    begin = as.Date(begin)
    end = as.Date(end)
    index_low = which(timesteps_low >= begin)
    index_high = which(timesteps_high <= end)
    index = index_low[which(index_low %in% index_high)]
    timesteps = timesteps[index]
    rm(timesteps_high, timesteps_low, index_high, index_low, index)
  } else {
    # ste timesteps to 1 for no time dimension (e.g. makes loop over timestep possible)
    timesteps = 1
  }

  # get access token for querying
  token = wapor_signin(APIkey)
  token_x = paste("Bearer ", token)

  for(step in timesteps){

    if(class(timesteps) == "character"){
      date_name  = as.Date(str_sub(step,2,11))
      if(temp_dim == "YEAR"){
        date_name = str_sub(date_name, 1, 4)
      } else if(temp_dim == "MONTH") {
        date_name = str_sub(date_name, 1, 7)
      }
      date_name = paste(date_name, temp_dim, sep = "_")
      if(length(dim_names)>0){
        tmp_filename = paste(product, paste(unlist(dimensions), date_name, sep = "_"), "clipped.tif", sep = "_")
        outname = paste0(file.path(outdir, paste(product, paste(unlist(dimensions), collapse = "_"), date_name, sep = "_")), ".tif")
      } else {
        tmp_filename = paste(product, date_name, "clipped.tif", sep = "_")
        outname = paste0(file.path(outdir, paste(product, date_name, sep = "_")), ".tif")
      }
    } else {
      if(length(dim_names)>0){
        tmp_filename = paste(product, paste(unlist(dimensions), collapse = "_"), "clipped.tif", sep = "_")
        outname = paste0(file.path(outdir, paste(product, paste(unlist(dimensions), collapse = "_"), sep = "_")), ".tif")
      } else {
        tmp_filename = paste(product, "clipped.tif", sep = "_")
        outname = paste0(file.path(outdir, paste(product, sep = "_")), ".tif")
      }

    }

    # skip downlaod when file exists
    if(file.exists(outname)) next

    # prepare dimension block of query json
    params = list()
    if(length(dim_names>0)){
      for(x in 1:length(dimensions)){
        value = dimensions[[x]]
        code = names(dimensions[x])
        para =  list(code = code, values = list(value))
        params = append(params, list(para))
      }
    }

    if(timesteps[1] !=  1){
      t_dim = list(
        list(code = temp_dim,
             values = list(step)))
      params = append(params, t_dim)
    }

    payload =  list(type = 'CropRaster',
                    params = list(
                      properties = list(
                        outputFileName = tmp_filename,
                        cutline = cutline,
                        tiled = tiled,
                        compressed = compressed,
                        overviews = overviews
                      ),
                      cube = list(
                        code = product,
                        workspaceCode = collection,
                        language = "en"
                      ),
                      dimensions =  params,
                      measures = list(measure)
                    )
    )

    polygon = list(
      shape = list(
        type = "Polygon",
        coordinates = list(
          list(list(extent[1], extent[2]),
               list(extent[1], extent[4]),
               list(extent[3], extent[4]),
               list(extent[3], extent[2]),
               list(extent[1], extent[2]))
        )))
    payload$params = append(payload$params, polygon)

    payload = toJSON(payload, pretty = T, auto_unbox = T)

    # clean payload
    payload = str_replace_all(payload, '"true"', 'true')
    payload = str_replace_all(payload, '"false"', 'false')
    # cat(payload)

    response =  POST(url = queryurl,
                     add_headers(Accept = "application/json",
                                 Authorization = token_x,
                                 "Content-type" = "application/json;charset=UTF-8"),
                     body = payload,
                     encode = "json",
                     ua)
    res = content(response)
    if(res$status == 200){
      job_url = res$response$links[[1]]$href
      job_response = GET(job_url, add_headers(Accept = "application/json",
                                              Authorization = token_x,
                                              "Content-type" = "application/json;charset=UTF-8"),
                         ua)
      job_status = content(job_response)$response$status

      while (job_status %in% c("RUNNING","WAITING")){
        Sys.sleep(sleep_time)
        job_response = GET(job_url,
                           add_headers(Accept = "application/json",
                                       Authorization = token_x,
                                       "Content-type" = "application/json;charset=UTF-8"),
                           ua)
        job_status = content(job_response)$response$status
      }

      if(job_status == "COMPLETED WITH ERRORS"){
        check_status(job_response)
      } else {
        job_response = GET(job_url,
                           add_headers(Accept = "application/json",
                                       Authorization = token_x,
                                       "Content-type" = "application/json;charset=UTF-8"),
                           ua)
        job_result = content(job_response)$response$output$downloadUrl
        GET(url = job_result, add_headers(Accept = "application/json",
                                          Authorization = token_x,
                                          "Content-type" = "application/json;charset=UTF-8"),
            ua,
            write_disk(outname, overwrite = T))
      }
    } else { # if status different from 200
      check_status(response)
    }
  }

}




