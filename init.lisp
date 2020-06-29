
;; -*- lisp -*-

(in-package :stumpwm)


;; Basics
(set-prefix-key (kbd "s-SPC"))
(setf *startup-message* NIL
      *suppress-abort-messages* t
      *shell-program* (getenv "SHELL"))


;; Looks

;; (set-font (make-instance 'xft:font
;;                          :family "Inconsolata"
;;                          :subfamily "Regular"
;;                          :size 12))
;; (set-font "-windows-dina-medium-r-normal--13-100-96-96-c-80-iso8859-1")

(setf *colors* (list "#282828"      ; 0 black
                     "#cc241d"      ; 1 red
                     "#98971a"      ; 2 green
                     "#d79921"      ; 3 yellow
                     "#458588"      ; 4 blue
                     "#b16286"      ; 5 magenta
                     "#689d6a"      ; 6 cyan
                     "#a89984"))    ; 7 white


(setf *message-window-gravity* :center
      *input-window-gravity* :center
      *window-border-style* :thin
      *message-window-padding* 10
      *maxsize-border-width* 5
      *normal-border-width* 5
      *transient-border-width* 2
      stumpwm::*float-window-border* 2
      stumpwm::*float-window-title-height* 5
      *mouse-focus-policy* :sloppy) ;; :click, :ignore, :sloppy

(setf *mode-line-position* :bottom)
(run-commands "move-focus left" "mode-line" "fother" "mode-line")


(set-normal-gravity :center)
(set-maxsize-gravity :center)
(set-transient-gravity :center)


;; Commands

(defun window-cls-present-p (win-cls &optional all-groups)
  "Tell if a window (by class) is present"
  (let ((windows (group-windows (if all-groups (current-screen) (current-group)))))
    (member win-cls (mapcar #'window-class windows) :test #'string-equal)))

(defun run-or-raise-prefer-group (cmd win-cls)
  "If there are windows in the same class, cycle in those. Otherwise call
run-or-raise with group search t."
  (if (window-cls-present-p win-cls)
      (run-or-raise cmd `(:class ,win-cls) nil T)
      (run-or-raise cmd `(:class ,win-cls) T T)))


(defcommand emacs () ()
  (run-shell-command "emacsclient -c"))

(defcommand terminal () ()
  (run-shell-command "urxvtc"))


;; Startup Groups

(when *initializing*
  ;; Load Presets
  (run-commands "restore ~/.stumpwm.d/desktop.lisp" "restore ~/.stumpwm.d/layout.lisp")

  ;; Run startup applications
  ;; Communication
  (run-shell-command "slack")
  (run-shell-command "chromium --new-window https://outlook.office.com/mail/inbox")
  (run-shell-command "hexchat")
  ;; Procrastination
  (run-shell-command "chromium --new-window youtube.com")
  (run-shell-command "Discord")
  ;; Development
  (run-shell-command "emacsclient -c")
  (run-shell-command "next")
  ;; System
  (run-shell-command "urxvtc -e ncmpcpp")
  (run-shell-command "urxvtc -e clock.sh")
  (run-shell-command "urxvtc -e gotop")
  (run-shell-command "urxvtc -e cava")
  (run-shell-command "urxvtc -e stumpish")
  (run-shell-command "urxvtc")

  )


;; (when *initializing*
;;   (run-commands "gnew System"
;;                 "move-focus left"
;;                 "hsplit-equally 3"
;;                 "move-focus right"
;;                 "exec urxvtc -e stumpish"
;;                 "move-focus right"
;;                 "move-focus right"
;;                 "exec urxvtc -e ncmpcpp")

;; (run-commands "gnew Procrastination"
;;               "move-focus left"
;;               "exec Discord"
;;               "move-focus right"
;;               "exec chromium")

;; )

;; Top-level Keybinds

(define-key *top-map* (kbd "s-RET") "terminal")
(define-key *top-map* (kbd "s-q") "delete-window")
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-l") "move-focus right")
(define-key *top-map* (kbd "s-y") "next-in-frame")
(define-key *top-map* (kbd "s-n") "prev-in-frame")
(define-key *top-map* (kbd "s-d") "exec rofi -show run")
(define-key *top-map* (kbd "s-s") "exec scrot -s")


(define-key *top-map* (kbd "s-0") "select-window-by-number 0")
(define-key *top-map* (kbd "s-1") "select-window-by-number 1")
(define-key *top-map* (kbd "s-2") "select-window-by-number 2")
(define-key *top-map* (kbd "s-3") "select-window-by-number 3")
(define-key *top-map* (kbd "s-4") "select-window-by-number 4")
(define-key *top-map* (kbd "s-5") "select-window-by-number 5")
(define-key *top-map* (kbd "s-6") "select-window-by-number 6")
(define-key *top-map* (kbd "s-7") "select-window-by-number 7")
(define-key *top-map* (kbd "s-8") "select-window-by-number 8")
(define-key *top-map* (kbd "s-9") "select-window-by-number 9")



;; Main-level Keybinds

(define-key *root-map* (kbd "r") "restart-soft")
(define-key *root-map* (kbd "R") "restart-soft")
(define-key *root-map* (kbd "p") "repack-window-numbers")
(define-key *root-map* (kbd "h") "move-window left")
(define-key *root-map* (kbd "j") "move-window up")
(define-key *root-map* (kbd "k") "move-window down")
(define-key *root-map* (kbd "l") "move-window right")
