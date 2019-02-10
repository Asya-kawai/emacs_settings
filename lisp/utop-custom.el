;;-*- coding:utf-8 -*- 
;;; utop-custom.el --- 

;; Keywords: utop 

;;; Code:

(require 'utop)
(require 'cl)

;;; copy from using-utop-in-emacs.html
(defconst init-file-name "toplevel.init")

(defconst build-dir-name "_build")

(defun upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
  manage to find it, return the containing directory. Else if we
  get to the toplevel directory and still can't find it, return
  nil. Start at startdir or . if startdir not given"

  (let ((dirname (expand-file-name
                  (if startdir startdir ".")))
        (found nil) ; found is set as a flag to leave loop if we find it
        (top nil))  ; top is set when we get
    ;; to / so that we only check it once

    ;; While we've neither been at the top last time nor have we found
    ;; the file.
    (while (not (or found top))
      ;; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
          (setq top t))

      ;; Check for the file
      (if (file-exists-p (expand-file-name filename dirname))
          (setq found t)
        ;; If not, move up a directory
        (setq dirname (expand-file-name ".." dirname))))
    ;; return statement
    (if found dirname nil)))

(defun should-include-p (file)
  "A predicate for wether a given file-path is relevant for
   setting up the `include` path of utop."
  (cond ((string= (file-name-base file) ".") nil)
        ((string= (file-name-base file) "..") nil)
        ((string-match ".*\.dSYM" file) nil)
        ((file-directory-p file) t)))


(defun ls (dir)
  "Returns directory contents. Only includes folders that
   are relevant for utop"
  (if (should-include-p dir)
      (remove-if-not 'should-include-p (directory-files dir t))
    nil))

(defun ls-r (dir)
  "Returns directory contents, decending into subfolders
   recursively. Only returns folders that are relevant for utop "
  (defun tail-rec (directories result)
    (if (> (length directories) 0)
        (let* ((folders (remove-if-not 'should-include-p directories))
               (next (mapcar 'ls folders))
               (flattened (apply #'append next)))
          (tail-rec flattened (append result folders)))
      result))
  (tail-rec (list dir) nil))

(defun utop-invocation (&optional startdir)
  "Generates an appropriately initialized utop buffer."
  (interactive)
  (let* ((dir (if startdir startdir default-directory))
         (project-root (upward-find-file init-file-name dir))
         (init-file (concat project-root "/" init-file-name))
         (build-dir (concat project-root "/" build-dir-name))
         (includes (ls-r build-dir))
         (includes-str (mapconcat (lambda (i) (concat "-I " i)) includes " "))
         (utop-command (concat "utop -emacs " "-init " init-file " " includes-str)))
    ;; The part below is mostly copied from utop.el; Look at the source for comments.
    (let ((buf (get-buffer utop-buffer-name)))
      (cond
       (buf
        (pop-to-buffer buf)
        (when (eq utop-state 'done) (utop-restart)))
       (t
        ;; This is the change. We set the command string explicitly.
        (setq utop-command utop-command)
        (setq buf (get-buffer-create utop-buffer-name))
        (pop-to-buffer buf)
        (with-current-buffer buf (utop-mode))))
      buf)))


(provide 'utop-custom)
;;; utop-custom.el ends here
