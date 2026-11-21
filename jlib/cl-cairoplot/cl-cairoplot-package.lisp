;;;; cl-cairoplot-package.lisp

(defpackage :cl-cairoplot
  (:use :cl)
  (:nicknames :cairoplot)
  (:export
   ;; colors.lisp
   #:color #:make-color #:color-p
   #:color-red #:color-green #:color-blue #:color-alpha
   #:*named-colors* #:define-named-color #:parse-color

   ;; colormaps.lisp
   #:colormap #:make-colormap #:colormap-name #:colormap-function #:colormap-rgb
   #:*named-colormaps* #:define-colormap #:colormap-names #:parse-colormap
   #:reverse-colormap #:table-colormap #:gradient-colormap #:diverging-colormap
   #:colormap-lut #:argb-word

   ;; cairo-graphics.lisp — canvas
   #:canvas #:canvas-output-type #:canvas-destination
   #:canvas-width #:canvas-height #:canvas-open-p #:canvas-resolution
   #:open-canvas #:close-canvas #:with-canvas #:flush-canvas #:next-page #:erase-canvas
   #:with-opacity-group
   #:set-document-metadata

   ;; cairo-graphics.lisp — thread safety
   #:canvas-lock #:with-canvas-lock #:with-batched-graphics

   ;; cairo-graphics.lisp — pixel buffers
   #:pixel-buffer #:octet-pixel-buffer #:foreign-pixel-buffer
   #:make-octet-pixel-buffer #:make-foreign-pixel-buffer #:free-pixel-buffer
   #:pixel-buffer-width #:pixel-buffer-height #:pixel-buffer-format
   #:pixel-buffer-stride #:pixel-buffer-size
   #:pixel-buffer-data #:pixel-buffer-pointer #:pixel-ref

   ;; cairo-graphics.lisp — state
   #:set-color #:set-line-width #:set-line-cap #:set-line-join
   #:set-dash #:fill-rule #:set-fill-rule
   #:as-canvas #:with-saved-state #:with-style
   #:with-color #:with-line-width #:with-dash
   #:translate #:rotate #:scale #:transform #:reset-transform
   #:with-translation #:with-rotation #:with-scaling

   ;; cairo-graphics.lisp — paths and drawing
   #:new-path #:new-sub-path #:move-to #:line-to #:rel-move-to #:rel-line-to
   #:curve-to #:arc #:arc-negative #:rectangle #:close-path
   #:current-point
   #:stroke #:stroke-preserve #:fill-path #:fill-preserve #:paint
   #:draw-line #:draw-polyline #:draw-polygon
   #:draw-rectangle #:draw-circle

   ;; cairo-graphics.lisp — clipping
   #:clip #:clip-preserve #:reset-clip #:clip-extents #:in-clip-p
   #:with-clip

   ;; cairo-graphics.lisp — text
   #:set-system-font #:set-hershey-font #:set-font-size
   #:draw-argb-image #:paint-mesh
   #:line-width #:text-path
   #:show-text #:draw-text #:text-extents #:font-extents #:char-glyph-index

   ;; fonts.lisp
   #:font #:system-font #:hershey-font
   #:make-system-font #:make-hershey-font #:copy-font
   #:font-size #:system-font-family #:system-font-slant #:system-font-weight
   #:hershey-font-name #:hershey-font-pen-width #:hershey-font-space-width
   #:*hershey-space-width*
   #:*named-fonts* #:define-named-font #:*default-font* #:parse-font
   #:apply-font #:set-font #:with-font
   #:canvas-font #:current-font #:*fallback-fonts* #:layout-text
   #:halo #:make-halo #:copy-halo #:halo-p #:halo-color #:halo-width
   #:halo-opacity #:halo-layers #:*named-halos* #:define-named-halo #:parse-halo
   #:*halo* #:with-halo #:halo-layer-list #:do-halo-layers #:stroke-halo
   #:text-size #:put-text

   ;; plot-device.lisp
   #:plot-device #:device-aspect #:device-viewport #:device-entered-viewport
   #:*default-viewport*
   #:open-device #:close-device #:with-device #:device-changes #:erase-device
   #:read-cursor #:device-viewports
   #:*output-type-systems* #:output-type-buffer #:device-opened
   #:device-to-canvas #:canvas-to-device #:world-to-device #:device-to-world
   #:to-canvas #:from-canvas #:convert-coords #:coords-transform

   ;; viewport.lisp
   #:viewport #:viewport-device #:viewport-clipping-region #:viewport-angle
   #:make-viewport #:set-window #:set-window-equal-scales #:set-window-sky
   #:set-transform #:viewport-transform #:viewport-axis-aligned-p
   #:viewport-window #:viewport-rectangle #:viewport-region
   #:plot-to-canvas #:canvas-to-plot #:with-viewport #:erase-viewport
   #:locate-point #:viewport-contains-p
   #:target-device #:target-viewport #:with-target #:defplot

   ;; clipping.lisp
   #:clipping-region #:rectangle-region #:polygon-region
   #:ellipse-region #:circle-region
   #:region-union #:region-intersection #:region-outside
   #:apply-clip #:with-clipping

   ;; drawing.lisp
   #:plot-line #:plot-polyline #:plot-polygon #:plot-rectangle
   #:plot-ellipse #:plot-circle #:plot-text #:with-plot-rotation

   ;; axes.lisp
   #:default-text-size #:axis-ticks #:draw-frame #:draw-box
   #:sexagesimal-ticks #:sexagesimal-text #:draw-secondary-axis

   ;; markers.lisp
   #:plot-points #:plot-point #:define-marker #:marker-names
   #:*pgplot-markers* #:*astronomical-symbols*

   ;; image.lisp
   #:plot-image #:image-limits #:scale-function
   #:colorbar-viewport #:draw-colorbar

   ;; contours.lisp
   #:contour-lines #:contour-levels #:plot-contours #:plot-filled-contours
   #:filled-contour-levels

   ;; plot3d.lisp
   #:view3d #:make-view3d #:view3d-viewport #:view3d-x-range #:view3d-y-range
   #:view3d-z-range #:view3d-azimuth #:view3d-elevation #:project3d
   #:plot3d-polyline #:plot3d-points #:plot3d-text #:draw-box3d
   #:plot-surface #:plot-floor-contours #:draw-axes3d

   ;; charts.lisp
   #:plot-arrow #:annotate #:plot-error-bars #:plot-fill-between
   #:histogram #:plot-histogram #:values-to-colors #:values-to-sizes
   #:histogram2d #:plot-histogram2d

   ;; legend.lisp
   #:draw-legend

   ;; layout.lisp
   #:make-panel-grid #:panel-number-sides #:panel-omitted-ends #:draw-panel-label
   #:add-inset #:draw-zoom-indicator

   ;; time-axes.lisp
   #:time-ticks #:calendar-to-mjd #:mjd-to-calendar #:iso-to-mjd #:time-to-unix
   #:unix-to-time
   ;; categorical.lisp
   #:*category-colors* #:category-colors #:plot-bars #:bar-chart #:box-stats
   #:plot-box-plot #:kde #:plot-violins #:category-ticks
   ;; polar.lisp
   #:set-window-polar #:draw-polar-grid #:polar-to-plot #:plot-to-polar #:plot-polar-bars
   #:viewport-polar
   ;; areas.lisp
   #:step-points #:plot-step #:stack-order #:stack-series #:plot-stacked-area
   ;; vectors.lisp
   #:plot-vectors #:plot-vector-field #:plot-streamlines
   ;; pie.lisp
   #:pie-chart #:color-luminance
   ;; corner.lisp
   #:corner-plot #:quantile
   ;; heatmap.lisp
   #:heatmap
   ;; sky.lisp
   #:sky-projection #:zenithal-projection #:allsky-projection #:tan-projection
   #:sin-projection #:arc-projection #:zea-projection #:stg-projection
   #:ait-projection #:mol-projection #:car-projection #:make-projection
   #:projection-boundary #:set-window-projection #:draw-projection-boundary
   #:projection-frame #:equatorial-to-frame #:frame-to-equatorial
   #:make-tan-projection #:projection-ra0
   #:projection-dec0 #:sky-to-plane #:plane-to-sky #:sky-to-plot #:plot-to-sky
   #:set-window-tan #:viewport-projection
   #:wcs #:make-wcs #:wcs-from-header #:wcs-projection #:wcs-crpix #:wcs-cd
   #:wcs-pixel-to-plane #:wcs-pixel-to-sky #:wcs-sky-to-pixel #:wcs-image-transform
   #:wcs-plane-transform #:wcs-pixel-scale #:reproject-image
   #:draw-sky-grid))
