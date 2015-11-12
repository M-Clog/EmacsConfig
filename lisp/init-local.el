;;; package --- custom init file
;;; Commentary:
;;Custom init file on top of Purcell's stuff.

;;; Code:
;; Various visual stuff
(global-visual-line-mode 1) ; 1 for on, 0 for off.
(setq dired-listing-switches "-Al --si --time-style long-iso")
;; set a default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

;; Various keybindings
;; easy keys to split window. Key based on ErgoEmacs keybinding
(global-set-key (kbd "M-3") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "M-4") 'split-window-vertically) ; split pane top/bottom
(global-set-key (kbd "M-5") 'split-window-horizontally) ; split pane top/bottom
(global-set-key (kbd "M-2") 'delete-window) ; close current pane
(global-set-key (kbd "M-1") 'other-window) ; cursor to other pane

;; Various functions
(defun word-count-analysis (start end)
      "Count how many times each word is used in the region.
    Punctuation is ignored."
      (interactive "r")
      (let (words)
        (save-excursion
          (goto-char start)
          (while (re-search-forward "\\w+" end t)
            (let* ((word (intern (match-string 0)))
                   (cell (assq word words)))
              (if cell
                  (setcdr cell (1+ (cdr cell)))
                (setq words (cons (cons word 1) words))))))
        (when (interactive-p)
          (message "%S" words))
        words))

(defun word-histogram-region (posBegin posEnd)
  "Display word histogram showing frequency of word occurrence."
  (interactive "r")
  (message "Counting...")
  (let* ((ht (make-hash-table :test 'equal))
     (totals '()))
    (save-excursion
      (goto-char posBegin)
      (while (and (< (point) posEnd)
          (re-search-forward "\\w+\\W*" posEnd t))
    (puthash (match-string 0) (1+ (gethash (match-string 0) ht 0)) ht)))
  (maphash (lambda (key value)
         (setq totals (cons (list key value) totals)))
       ht)
  (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
  (with-output-to-temp-buffer "Word histogram"
    (princ (format "%d different words\n\n"
           (length totals)))
    (dolist (item totals)
      (let
      ((key (car item))
       (count (cadr item))
       (maxcount (cadr (car totals))))
    (princ (format "%2d %20s %s\n" count key
                   (make-string (/ (* count (min 36 maxcount)) maxcount) ?+))))))))


(defun buffer-mode-histogram ()
  "Display a histogram of Emacs buffer modes."
  (interactive)
  (let* ((ht (make-hash-table :test 'equal))
         (number-of-buffers (loop for buffer being the buffers
                                  for mode-name = (symbol-name (buffer-local-value 'major-mode buffer))
                                  do (incf (gethash mode-name ht 0))
                                  count 1))
         (totals (sort (loop for key being the hash-keys of ht
                             using (hash-values value)
                             collect (list key value))
                       (lambda (x y) (if (eql (second x) (second y))
                                         (string-lessp (first x) (first y))
                                       (> (second x) (second y)))))))
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
                      number-of-buffers (length totals)))
      (loop for (key count) in totals
            do (princ (format "%2d %20s %s\n"
                              count
                              (if (equal (substring key -5) "-mode")
                                  (substring key 0 -5) key)
                              (make-string count ?+)))))))



;; Emms, just the right thing to boast
(require 'emms-setup)
(emms-devel)
(emms-default-players)

(when (and window-system (executable-find "kdialog"))
  (setq emms-player-next-function
        (lambda ()
          (emms-next-noerror)
          (call-process "kdialog" nil nil nil "--title" "Emacs - EMMS"
                        "--passivepopup" (emms-show) "5"))))

(defun de-toggle-playing ()
  "Function to start/stop emmms."
  (interactive)
  (if emms-player-playing-p
      (emms-pause)
    (emms-start)))

(global-set-key (kbd "C-c e <up>") 'de-toggle-playing)
(global-set-key (kbd "C-c e <down>") 'emms-stop)
(global-set-key (kbd "C-c e <left>") 'emms-previous)
(global-set-key (kbd "C-c e <right>") 'emms-next)
(setq emms-mode-line-titlebar-function 'emms-mode-line-playlist-current)

(setq emms-last-played-format-alist
      '(((emms-last-played-seconds-today) . "%a %H:%M")
        (604800                           . "%a %H:%M") ; this week
        ((emms-last-played-seconds-month) . "%d")
        ((emms-last-played-seconds-year)  . "%m/%d")
        (t                                . "%Y/%m/%d")))

(eval-after-load "emms"
  '(progn

     (setq xwl-emms-playlist-last-track nil)
     (setq xwl-emms-playlist-last-indent "\\")

     (defun xwl-emms-track-description-function (track)
       "Return a description of the current track."
       (let* ((name (emms-track-name track))
              (type (emms-track-type track))
              (short-name (file-name-nondirectory name))
              (play-count (or (emms-track-get track 'play-count) 0))
              (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
              (empty "..."))
         (prog1
             (case (emms-track-type track)
               ((file url)
                (let* ((artist (or (emms-track-get track 'info-artist) empty))
                       (year (emms-track-get track 'info-year))
                       (playing-time (or (emms-track-get track 'info-playing-time) 0))
                       (min (/ playing-time 60))
                       (sec (% playing-time 60))
                       (album (or (emms-track-get track 'info-album) empty))
                       (tracknumber (emms-track-get track 'info-tracknumber))
                       (short-name (file-name-sans-extension
                                    (file-name-nondirectory name)))
                       (title (or (emms-track-get track 'info-title) short-name))

                       ;; last track
                       (ltrack xwl-emms-playlist-last-track)
                       (lartist (or (and ltrack (emms-track-get ltrack 'info-artist))
                                    empty))
                       (lalbum (or (and ltrack (emms-track-get ltrack 'info-album))
                                   empty))

                       (same-album-p (and (not (string= lalbum empty))
                                          (string= album lalbum))))
                  (format "%10s  %3d   %-20s%-60s%-35s%-15s%s"
                          (emms-last-played-format-date last-played)
                          play-count
                          artist

                          ;; Combine indention, tracknumber, title.
                          (concat
                           (if same-album-p ; indention by album
                               (setq xwl-emms-playlist-last-indent
                                     (concat " " xwl-emms-playlist-last-indent))
                             (setq xwl-emms-playlist-last-indent "\\")
                             "")
                           (if (and tracknumber ; tracknumber
                                    (not (zerop (string-to-number tracknumber))))
                               (format "%02d." (string-to-number tracknumber))
                             "")
                           title        ; title
                           )

                          ;; album
                          (cond ((string= album empty) empty)
                                ;; (same-album-p "  ")
                                (t (concat "《" album "》")))

                          (or year empty)
                          (if (or (> min 0)  (> sec 0))
                              (format "%02d:%02d" min sec)
                            empty))))
               ((url)
                (concat (symbol-name type) ":" name))
               (t
                (format "%-3d%s"
                        play-count
                        (concat (symbol-name type) ":" name))))
           (setq xwl-emms-playlist-last-track track))))

     (setq emms-track-description-function
           'xwl-emms-track-description-function)
     ))

;; Big Brother DataBase SetUp
(setq bbdb-file "~/.emacs.d/bbdb")           ;; keep ~/ clean; set before loading
(require 'bbdb)
(bbdb-initialize)
(setq
    bbdb-offer-save 1                        ;; 1 means save-without-asking


    bbdb-use-pop-up t                        ;; allow popups for addresses
    bbdb-electric-p t                        ;; be disposable with SPC
    bbdb-popup-target-lines  1               ;; very small

    bbdb-dwim-net-address-allow-redundancy t ;; always use full name
    bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

    bbdb-always-add-address t                ;; add new addresses to existing...
                                             ;; ...contacts automatically
    bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

    bbdb-completion-type nil                 ;; complete on anything

    bbdb-complete-name-allow-cycling t       ;; cycle through matches
                                             ;; this only works partially

    bbbd-message-caching-enabled t           ;; be fast
    bbdb-use-alternate-names t               ;; use AKA


    bbdb-elided-display t                    ;; single-line addresses

    ;; auto-create addresses from mail
    bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
    bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
    ;; NOTE: there can be only one entry per header (such as To, From)
    ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

    '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter\\|facebook\\|noreply\\|comment\\|wordpress\\|charter\\|reply")))


;; multi-occur in all opened buffers
(defun my-multi-occur-in-matching-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers (ALLBUFS)."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))
(global-set-key (kbd "C-S-s") 'my-multi-occur-in-matching-buffers)

;;; AUCTEX INIT
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Color theme
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-jsc-dark)))


(provide 'init-local)
;;; init-local.el ends here
