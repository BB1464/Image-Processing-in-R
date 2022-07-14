library(magick)
library(purrr)


dir.create("profiles")

photo_ids <- c("DItYlc26zVI", "bpxgyD4YYt4", "6anudmpILw4", "3dqSZidOkvs")

for (id in photo_ids) {
  download.file(
    sprintf("https://source.unsplash.com/%s", id),
    sprintf("profiles/%s.jpg", id)
  )
}


#I’ve put the photos in a profiles/ directory so that I can list the them all at once:

fs::dir_ls("profiles")

## profiles/3dqSZidOkvs.jpg profiles/6anudmpILw4.jpg profiles/DItYlc26zVI.jpg
## profiles/bpxgyD4YYt4.jpg


# Magick with R#
# The first step is to use the magick package to read in our profile pictures.


profiles <-
  fs::dir_ls("profiles") |>
  map(image_read)

profiles   # This line is not working

# Finding Faces

library(image.libfacedetection)

faces <- profiles |> image_detect_faces()

image_detect_faces(x = profiles)

plot(faces, all_profiles, only_box = TRUE)


faces





# Find face center

find_face_center <- function(image) {
  detections <- image.libfacedetection::image_detect_faces(image)$detections
  best_face <- which(detections$confidence == max(detections$confidence))
  dims <- as.list(detections[best_face[[1]], ])
  list(
    x = dims$x + dims$width / 2,
    y = dims$y + dims$height / 2
  )
}


face_center <- find_face_center(profiles[[3]])
str(face_center)

# Resize the Image
resize_fit <- function(image, size = 600) {
  info <- image_info(image)
  size <- min(size, info$width, info$height)
  image_resize(
    image,
    geometry_size_pixels(
      height = if (info$width >= info$height) size,
      width = if (info$height > info$width) size
    )
  )
}


resized_profile <-
  profiles[[3]] |>
  resize_fit()

resized_profile |> image_info()

# Find Resize Faces
resized_profile |>
  find_face_center()

crop_offset <- function(point, range, width) {
  # 4. Catch the edge case first
  if (width >= range) return(0)

  if ((point - width / 2) < 0) {
    # 1. must start at left edge
    return(0)
  }
  if ((point + width / 2) > range) {
    # 2. must start at right edge
    return(range - width)
  }
  # 3. enough space on both sides to center width in range
  point - width / 2
}


offset <- crop_offset(
  point = 579,
  range = 900,
  width = 600
)
offset


# Put it all Together
resize_crop_to_face <- function(image, size = 600) {
  image <- resize_fit(image, size)
  info <- image_info(image)

  # size may have changed after refit
  size <- min(info$height, info$width)

  is_image_square <- info$width == info$height
  if (is_image_square) {
    return(image)
  }

  face <- find_face_center(image)

  image_crop(
    image,
    geometry = geometry_area(
      width = size,
      height = size,
      x_off = crop_offset(face$x, info$width, size),
      y_off = crop_offset(face$y, info$height, size)
    )
  )
}


profiles <-
  fs::dir_ls("profiles") |>
  map(image_read) |>
  map(resize_crop_to_face)


# Write them back to Profile
fs::dir_create("profiles_cropped")

profiles |>
  iwalk(function(image, path) {
    new_path <- fs::path("profiles_cropped", fs::path_file(path))
    image_write(image, new_path)
  })







# package documentation for imagr_libfacedetection

# library(magick)
# path <- system.file(package="image.libfacedetection", "images", "handshake.jpg")
# x <- image_read(path)
# x
# faces <- image_detect_faces(x)
# faces
# plot(faces, x, border = "red", lwd = 7, col = "white", landmarks = TRUE)
#
#
# ##
# ## You can also directly pass on the RGB array in BGR format
# ## without the need of having magick
# ##
# tensor <- image_data(x, channels = "rgb")
# tensor <- as.integer(tensor)
# faces  <- image_detect_faces(tensor)
# str(faces)
# plot(faces, x)
