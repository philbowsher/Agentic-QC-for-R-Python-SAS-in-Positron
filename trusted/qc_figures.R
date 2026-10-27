# =============================================================================
# TRUSTED CODE BANK :: Figure production and bitmap-to-bitmap figure QC
# -----------------------------------------------------------------------------
# Status   : APPROVED (v1.0.0, 2026-10-23)  -- green-shield code
# Owner    : Statistical Programming Standards
# Lineage  : KM rendering via {ggsurvfit} (pharmaverse); pixel compare via
#            {magick} (ImageMagick compare). Pixel-level QC is only meaningful
#            when primary and QC render through the SAME device settings, which
#            is why rendering is centralised here.
# =============================================================================

qc_figure_device <- list(width = 9, height = 6, dpi = 150, bg = "white")

#' Standard Kaplan-Meier figure (company TLF style)
#' @noRd
tlf_km_plot <- function(adtte, group = "TRT01A",
                        title = "Kaplan-Meier Plot of Time to First Dermatologic Event",
                        subtitle = "Safety Population",
                        xlab = "Days since first dose",
                        risk_times = seq(0, 200, by = 25),
                        palette = NULL) {
  adtte$.group <- adtte[[group]]
  fit <- ggsurvfit::survfit2(survival::Surv(AVAL, 1 - CNSR) ~ .group, data = adtte)
  p <- ggsurvfit::ggsurvfit(fit, linewidth = 0.9) +
    ggsurvfit::add_censor_mark(size = 2) +
    ggsurvfit::add_risktable(times = risk_times, risktable_stats = "n.risk",
                             risktable_height = 0.22) +
    ggsurvfit::scale_ggsurvfit(x_scales = list(breaks = risk_times, limits = range(risk_times))) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab,
                  y = "Event-free probability") +
    ggplot2::theme(legend.position = "bottom")
  if (!is.null(palette)) {
    p <- p + ggplot2::scale_color_manual(values = palette)
  }
  p
}

#' Save a TLF figure with the locked company device settings
#' @noRd
tlf_save_png <- function(plot, path, device = qc_figure_device) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # ggplot2 caches font descent per device *name*. Positron's Plots pane also
  # renders through ragg ("agg_png") at a different resolution, so after any
  # plot is shown in the pane the cached metrics are wrong for this PNG and the
  # layout shifts ~2 px (breaking pixel-level QC). Clear the cache first.
  # (Internal ggplot2 object; tested with ggplot2 4.0.3.)
  cache <- tryCatch(utils::getFromNamespace("descent_cache", "ggplot2"), error = function(e) NULL)
  if (is.environment(cache)) rm(list = ls(cache, all.names = TRUE), envir = cache)
  ragg::agg_png(path, width = device$width, height = device$height, units = "in",
                res = device$dpi, background = device$bg)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  invisible(path)
}

#' Bitmap-to-bitmap figure comparison
#'
#' Reads both figures, standardises size, runs ImageMagick compare and writes a
#' three-panel composite (primary | QC | differences in red).
#' Returns a list with the pixel count that differs and the composite path.
#' @noRd
qc_compare_figures <- function(primary_png, qc_png, out_png,
                               fuzz = 5, highlight = "red", panel_width = 900) {
  a <- magick::image_read(primary_png)
  b <- magick::image_read(qc_png)

  # Standardise geometry: QC is resized to the primary's exact pixel size
  info_a <- magick::image_info(a)
  geom <- sprintf("%dx%d!", info_a$width, info_a$height)
  b <- magick::image_resize(b, geom)

  # ImageMagick compare: faded reference with differing pixels painted red
  diff_img <- magick::image_compare(a, b, metric = "AE", fuzz = fuzz)
  n_px <- as.numeric(attr(diff_img, "distortion"))
  overlay <- diff_img

  label <- function(img, txt, col = "black") {
    img |>
      magick::image_scale(as.character(panel_width)) |>
      magick::image_border("white", "0x60") |>
      magick::image_annotate(txt, size = 28, gravity = "north", location = "+0+12",
                             weight = 700, color = col)
  }
  composite <- magick::image_append(c(
    label(a, "PRIMARY"),
    label(b, "QC"),
    label(overlay, sprintf("DIFFERENCES: %s pixels (fuzz %d%%)", format(n_px, big.mark = ","), fuzz),
          col = if (n_px > 0) highlight else "darkgreen")
  )) |>
    magick::image_border("grey80", "2x2")

  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)
  magick::image_write(composite, out_png)
  invisible(list(
    pixels_different = n_px,
    pct_different = 100 * n_px / (info_a$width * info_a$height),
    status = if (n_px == 0) "MATCH" else "DIFFERENT",
    composite = out_png
  ))
}

#' KM figure: primary vs QC pixel comparison
#'
#' Bitmap-to-bitmap comparison of the primary and independent QC renderings
#' of Figure 14.2.1 (Kaplan-Meier, time to first dermatologic event). Both
#' figures are produced by the company figure standard, so any differing
#' pixel reflects a difference in the underlying data, not styling.
#'
#' @param fuzz `integer` Colour tolerance in percent before a pixel counts as different (default 5).
#' @return The number of differing pixels and a composite image
#'   (primary | QC | differences highlighted in red).
#' @provenance https://docs.ropensci.org/magick/reference/analysis.html (retrieved 2026-10-23)
#' @measure
km_figure_compare <- function(fuzz = 5,
                              root = getOption("agentic_qc.root", ".")) {
  res <- qc_compare_figures(
    primary_png = file.path(root, "output/figures/f_km_ttde_primary.png"),
    qc_png = file.path(root, "output/figures/f_km_ttde_qc.png"),
    out_png = file.path(root, "output/figures/f_km_ttde_diff_app.png"),
    fuzz = fuzz
  )
  img <- base64enc::dataURI(file = res$composite, mime = "image/png")
  summary <- sprintf(
    "Status: %s. %s pixels differ (%.2f%% of the image) at fuzz %d%%.",
    res$status, format(res$pixels_different, big.mark = ","), res$pct_different, fuzz
  )
  ellmer::ContentToolResult(
    value = summary,
    extra = list(display = shinychat::tool_result_display(
      html = sprintf("<p>%s</p><img src='%s' style='width:100%%;'/>", summary, img)
    ))
  )
}
