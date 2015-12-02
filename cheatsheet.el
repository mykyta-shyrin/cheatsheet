;;; cheatsheet --- helps you to create your own customized cheatsheet

;;; Commentary:
;;; Will add

;;; Code:

(require 'cl-lib)

(defconst cheatsheet--group-face
  '(:foreground "red")
  "Group name font face.")

(defconst cheatsheet--key-face
  '(:foreground "orange")
  "Cheat key font face.")


(defvar cheatsheet--cheat-list '()
  "List of cheats.")


(defun cheatsheet--if-symbol-to-string (string-like)
  "Convert STRING-LIKE to string."
  (if (symbolp string-like) (symbol-name string-like) string-like))

(defun cheatsheet--group-name (group)
  "Get GROUP name."
  (cheatsheet--if-symbol-to-string (plist-get group :name)))

(defun cheatsheet--group-cheats (group)
  "Get GROUP cheats."
  (cheatsheet--if-symbol-to-string (plist-get group :cheats)))

(defun cheatsheet--cheat-key (cheat)
  "Get CHEAT key."
  (cheatsheet--if-symbol-to-string (plist-get cheat :key)))

(defun cheatsheet--cheat-group (cheat)
  "Get CHEAT group."
  (cheatsheet--if-symbol-to-string (plist-get cheat :group)))

(defun cheatsheet--cheat-description (cheat)
  "Get CHEAT description."
  (cheatsheet--if-symbol-to-string (plist-get cheat :description)))

(defun cheatsheet-add (&rest cheat)
  "Add CHEAT to cheatsheet."
  (add-to-list 'cheatsheet--cheat-list cheat))

(defun cheatsheet--cheat-groups ()
  "Get all groups, submitted to cheatsheet."
  (reverse (delete-dups
            (mapcar 'cheatsheet--cheat-group
                    cheatsheet--cheat-list))))

(defun cheatsheet--get-group (group)
  "Get group struct with all cheats, belonging to GROUP."
  (cl-flet ((is-current-group (cheat)
                              (if (string= (cheatsheet--cheat-group cheat)
                                           group)
                                  cheat
                                nil)))
    (delq nil (mapcar #'is-current-group cheatsheet--cheat-list))))

(defun cheatsheet-get ()
  "Get cheatsheet as list of group structs, keeping defining order."
  (cl-flet ((make-group (group)
                        (list :name group
                              :cheats (cheatsheet--get-group group))))
    (mapcar #'make-group (cheatsheet--cheat-groups))))

(defun cheatsheet--format ()
  "Print the whole cheatsheet."
  (let* ((cheatsheet (cheatsheet-get))
         (formatted-groups (mapcar 'cheatsheet--format-group cheatsheet))
         (formatted-cheatsheet (apply 'concat formatted-groups)))
    formatted-cheatsheet))

(defun cheatsheet-show ()
  "Create buffer and show cheatsheet."
  (interactive)
  (cl-flet ((get-local-keymap
             ()
             (let ((keymap (make-keymap)))
               (define-key keymap (kbd "C-q") 'kill-buffer-and-window)
               keymap)))
    (switch-to-buffer-other-window "*cheatsheet*")
    (use-local-map (get-local-keymap))
    (erase-buffer)
    (insert (cheatsheet--format))
    (setq buffer-read-only t)))

(defun cheatsheet--format-cheat (cheat key-cell-length)
  "Format CHEAT row with KEY-CELL-LENGTH key cell length."
  (let* ((format-string (format "%%%ds - %%s\n" key-cell-length))
         (key (cheatsheet--cheat-key cheat))
         (description (cheatsheet--cheat-description cheat))
         (faced-key (propertize key 'face cheatsheet--key-face)))
    (format format-string faced-key description)))

(defun cheatsheet--format-group (group)
  "Format GROUP to table."
  (cl-flet ((key-length (cheat) (length (cheatsheet--cheat-key cheat)))
            (format-cheat (key-cell-length cheat)
                          (cheatsheet--format-cheat cheat key-cell-length)))

    (let* ((name (cheatsheet--group-name group))
           (cheats (cheatsheet--group-cheats group))
           (key-max-length (apply 'max (mapcar #'key-length cheats)))
           (key-cell-length (+ 2 key-max-length))
           (format-cheat (apply-partially #'format-cheat key-cell-length))
           (formatted-cheats (apply 'concat (mapcar format-cheat cheats)))
           (faced-group-name (propertize name 'face cheatsheet--group-face)))
      (concat faced-group-name "\n" formatted-cheats "\n"))))


(cheatsheet-add :group 'Common :key ":q" :description "leave Emacs.")
(cheatsheet-add :group 'Common :key "M-x buffer-menu" :description "select buffer to show in current frame.")
(cheatsheet-add :group 'Common :key "M-:" :description "eval expression.")
(cheatsheet-add :group 'Project :key "C-x C-f" :description "open file.")
(cheatsheet-add :group 'Project :key "C-c p f" :description "find file.")
(cheatsheet-add :group 'Project :key "C-c p s g" :description "search file.")

;;; cheatsheet.el ends here
