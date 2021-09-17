(defun wcy-c-open-other-file ()
  "if current file is a header file, then open the
   corresponding source file or vice versa.
  "
    (interactive)
    (let ((f (buffer-file-name))
          (headers '("h" "hpp" "hxx"))
          (sources '("c" "cxx" "cpp" "cc")))
      (if f
          (let* ((b (file-name-sans-extension f))
                 (x (file-name-extension f))
                 (s (cond
                     ((member x headers) sources)
                     ((member x sources) headers)
                     (t nil)))
                 (return-value nil))
            (while s
              (let ((try-file (concat b "." (car s))))
                (cond
                 ((find-buffer-visiting try-file)
                  (switch-to-buffer (find-buffer-visiting
                                     try-file))
                  (setq s nil
                        return-value t))
                 ((file-readable-p try-file)
                  (find-file try-file)
                  (setq s nil
                        return-value t))
                 (t (setq s (cdr s))))))
            return-value))))
