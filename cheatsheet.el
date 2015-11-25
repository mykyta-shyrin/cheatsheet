;;; cheatsheet --- helps you to create your own customized cheatsheet

;;; Commentary:
;;; Will add

;;; Code:

(require 'cl-lib)

(cl-defstruct cheat group key description)
(cl-defstruct group name cheats)

(defvar cheat-list '()
  "List of cheats.")

(defun cheat-add (&rest cheat)
  "Add CHEAT to cheatsheet."
  (let ((cheat (apply 'make-cheat cheat)))
    (add-to-list 'cheat-list cheat)))

(defun cheat-groups ()
  "Get all groups, submitted to cheatsheet."
  (reverse (delete-dups (mapcar 'cheat-group cheat-list))))

(defun cheatsheet-get-group (group)
  "Get group struct with all cheats, belonging to GROUP."
  (let* ((is-group (lambda (group cheat)
                     (if (string= (cheat-group cheat) group)
                         cheat
                       nil)))
         (filter-group (apply-partially is-group group)))
    (delq nil (mapcar filter-group cheat-list))))

(defun cheatsheet ()
  "Get cheatsheet as list of group structs, keeping defining order."
  (let ((make-group (lambda (group)
                      (make-group :name group
                                  :cheats (cheatsheet-get-group group)))))
    (mapcar make-group (cheat-groups))))

(defun cheat-print (cheat)
  "Print CHEAT."
  (let ((key (cheat-key cheat))
        (description (cheat-description cheat)))
    (princ
        (format " :%15s  -  %s\n"
                key description))))

(defun cheat-group-print (group)
  "Print GROUP of cheats."
  (let ((name (group-name group))
        (cheats (group-cheats group)))
    (princ name)
    (princ "\n")
    (mapc 'cheat-print cheats)
    (princ "\n")))

(defun cheatsheet-print ()
  "Print the whole cheatsheet."
  (mapc 'cheat-group-print (cheatsheet)))

(defun cheatsheet-show ()
  "Creat buffer and show cheatsheet."
  (interactive)
  (with-output-to-temp-buffer "*cheatsheet*" (cheatsheet-print)))


; (cheat-add :group 'Common :key ":q" :description "leave Emacs.")
; (cheat-add :group 'Project :key "C-x C-f" :description "open file.")
; (cheat-add :group 'Project :key "C-c p f" :description "find file.")
; (cheat-add :group 'Project :key "C-c p s g" :description "search file.")

;;; cheatsheet.el ends here
