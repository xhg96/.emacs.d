;; gtest helper commands
;; Author: zhenyun.yzy
;; Version: 0.1

;; 光标放在 gtest fixture 的代码块里，按快捷键即运行当前这一个测试。
;; Usage:
;; put me in ~/.emacs.d/lisp
;; add codes below for loading and key binding in your .emacs file:
;;
;; (load-file "~/.emacs.d/lisp/rtp-gtest-tools.el")
;; (global-set-key (kbd "C-x j") 'run-this-gtest-fixture)

(defun mkcmd (line)
  (let* ((parts (split-string line "[\\(,\\) ]+"))
         (test-name (nth 1 parts))
         (fixture-name (nth 2 parts)))
    (concat "bazel test //... --config=rtp --strip=never --copt -g --test_arg=gtest_filter=" test-name "." fixture-name)))
(defun fixture? (line)
  (string-prefix-p "TEST_F" line))
(defun iter-test (contents idx)
  (if (< idx 1)
      0
    (if (fixture? (nth idx contents))
        idx
      (iter-test contents (- idx 1)))))

(defun current-buffer-lines ()
  (let* ((cur-buffer (current-buffer))
         (content (buffer-substring-no-properties 1 (point-max))))
    (split-string content "\n")))

(defun run-this-gtest-fixture ()
  (interactive)
  (let* ((lines (current-buffer-lines))
         (linum (string-to-number (substring (what-line) 5)))
         (fixture-linum (iter-test lines (- linum 1)))
         (compile-cmd (mkcmd (nth fixture-linum lines))))
    (compile compile-cmd)))
