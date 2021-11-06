# from https://www.garrickadenbuie.com/blog/sharing-xaringan-slides/

#' Screenshot Your Title Slide for Share Image
#'
#' Takes a screenshot of your title slide for sharing on Twitter
#' (and other social media sites).
#'
#' @param slides_rmd Your slides file
#' @param path Path to new share image
screenshot_share_image <- function(
  slides_rmd,
  path_image = "share-card.png",
  goal = 'twitter'
) {
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop(
      "`webshot2` is required: ", 
      'remotes::install_github("rstudio/webshot2")'
    )
  }
  vheight <- if (goal == 'twitter') 600 else 720
  vwidth <- if (goal == 'twitter') 1146 else 1280
  ratio <- if (goal == 'twitter') '191:100' else '16:9'
  
  webshot2::rmdshot(
    doc = slides_rmd,
    file = path_image,
    vheight = vheight,
    vwidth = vwidth,
    rmd_args = list(
      output_options = list(
        nature = list(ratio = ratio),
        self_contained = TRUE
      )
    )
  )
  
  path_image
}

screenshot_share_image("cloudspotting/index.Rmd",
                       "cloudspotting/video-thumbnail.png",
                       goal = 'youtube')
screenshot_share_image("cloudspotting/index.Rmd", "cloudspotting/social-card.png")
screenshot_share_image("investigandopolisemia.Rmd", "investigandopolisemia.png")
screenshot_share_image("index.Rmd", "social-card.png")
