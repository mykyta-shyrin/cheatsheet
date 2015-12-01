;;; cheatsheet --- helps you to create your own customized cheatsheet

;;; Commentary:
;;; Will add

;;; Code:
(require 'cl-lib)

(defvar cheatsheet-cheat-list '()
  "List of cheats.")

(defun cheatsheet-if-symbol-to-string (string-like)
  "Convert STRING-LIKE to string."
  (if (symbolp string-like) (symbol-name string-like) string-like))

(defun cheatsheet-group-name (group)
  "Get GROUP name."
  (cheatsheet-if-symbol-to-string (plist-get group :name)))

(defun cheatsheet-group-cheats (group)
  "Get GROUP cheats."
  (cheatsheet-if-symbol-to-string (plist-get group :cheats)))

(defun cheatsheet-cheat-key (cheat)
  "Get CHEAT key."
  (cheatsheet-if-symbol-to-string (plist-get cheat :key)))

(defun cheatsheet-cheat-group (cheat)
  "Get CHEAT group."
  (cheatsheet-if-symbol-to-string (plist-get cheat :group)))

(defun cheatsheet-cheat-description (cheat)
  "Get CHEAT description."
  (cheatsheet-if-symbol-to-string (plist-get cheat :description)))

(defun cheatsheet-add (&rest cheat)
  "Add CHEAT to cheatsheet."
    (add-to-list 'cheatsheet-cheat-list cheat))

(defun cheatsheet-cheat-groups ()
  "Get all groups, submitted to cheatsheet."
  (reverse (delete-dups
            (mapcar 'cheatsheet-cheat-group
                    cheatsheet-cheat-list))))

(defun cheatsheet-get-group (group)
  "Get group struct with all cheats, belonging to GROUP."
  (let* ((is-group (lambda (group cheat)
                     (if (string= (cheatsheet-cheat-group cheat) group)
                         cheat
                       nil)))
         (filter-group (apply-partially is-group group)))
    (delq nil (mapcar filter-group cheatsheet-cheat-list))))

(defun cheatsheet-get ()
  "Get cheatsheet as list of group structs, keeping defining order."
  (let ((make-group (lambda (group)
                      (list :name group
                            :cheats (cheatsheet-get-group group)))))
    (mapcar make-group (cheatsheet-cheat-groups))))

(defun cheatsheet-cheat-format (cheat)
  "Get formatted CHEAT."
  (let ((key (plist-get cheat :key))
        (description (plist-get cheat :description)))
    (format " : %s\t\t\t- %s\n"
            key description)))

(defun cheatsheet-group-print (group)
  "Print GROUP of cheats."
  (princ (cheatsheet-format-group group)))

(defun cheatsheet-print ()
  "Print the whole cheatsheet."
  (mapc 'cheatsheet-group-print (cheatsheet-get)))

(defun cheatsheet-show ()
  "Create buffer and show cheatsheet."
  (interactive)
  (with-output-to-temp-buffer "*cheatsheet*"
    (cheatsheet-print)))

(defun cheatsheet-format-group (group)
  "Format GROUP to table."
  (cl-flet ((key-length (cheat) (length (cheatsheet-cheat-key cheat)))
            (format-cheat (format-string cheat)
                          (format format-string
                                  (cheatsheet-cheat-key cheat)
                                  (cheatsheet-cheat-description cheat))))
    (let* ((name (cheatsheet-group-name group))
           (cheats (cheatsheet-group-cheats group))

           (key-max-length (apply 'max (mapcar #'key-length cheats)))
           (key-cell-length (+ 2 key-max-length))

           (format-string (format " : %%%ds - %%s\n" key-cell-length))
           (format-cheat (apply-partially #'format-cheat format-string))
           (formatted-cheats (apply 'concat (mapcar format-cheat cheats))))
      (concat name "\n" formatted-cheats))))


(cheatsheet-add :group 'Common :key ":q" :description "leave Emacs.")
(cheatsheet-add :group 'Common :key "M-x buffer-menu" :description "select buffer to show in current frame.")
(cheatsheet-add :group 'Common :key "M-:" :description "eval expression.")
(cheatsheet-add :group 'Project :key "C-x C-f" :description "open file.")
(cheatsheet-add :group 'Project :key "C-c p f" :description "find file.")
(cheatsheet-add :group 'Project :key "C-c p s g" :description "search file.")

;;; cheatsheet.el ends here
