;;; order.el --- Animation ordering and cycling configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Defines custom ordering and cycling for animations.
;;
;; If vibe-animation-cycle is defined, only those animations will be cycled
;; in the specified order. If not defined, all discovered animations will
;; be cycled in alphabetical order.

;;; Code:

;; Define which animations to cycle through and in what order
;; Comment out or remove this to cycle through all animations alphabetically
(defvar vibe-animation-cycle '("starfield" "matrix-rain" "aquarium" "snake" "scroll-logo")
  "List of animation modes to cycle through in order.
If nil or not defined, all animations will be cycled alphabetically.")

;; Alternative: cycle through all but specify order for some
;; (defvar vibe-animation-cycle '("starfield" "matrix"))  ; Only these two
;; (defvar vibe-animation-cycle nil)  ; All animations, alphabetical

(provide 'order)
;;; order.el ends here