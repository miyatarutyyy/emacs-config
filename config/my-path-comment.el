;; -*- lexical-binding: t -*-

(defun my--normalize-path-separator (path)
  "PATH の区切り文字を / に統一する。"
  (subst-char-in-string ?\\ ?/ path))

(defun my--project-pseudo-absolute-path (root file)
  "ROOT をルートとして FILE の疑似絶対パスを返す。
返り値は `root-name/relative/path/to/file` の形式。"
  (let* ((root-dir (directory-file-name (expand-file-name root)))
         (file-path (expand-file-name file)))
    (unless (file-in-directory-p file-path root-dir)
      (error "現在のファイルは指定したルート配下にありません: %s" file-path))
    (let* ((root-name (file-name-nondirectory root-dir))
           (relative-path (file-relative-name file-path root-dir)))
      (concat root-name "/" (my--normalize-path-separator relative-path)))))

(defun my--make-comment-line (text)
  "現在の major mode に応じて TEXT をコメント 1 行にして返す。"
  (let ((cs (or comment-start ""))
        (ce (or comment-end "")))
    (when (string-empty-p (string-trim cs))
      (error "この major mode では comment-start が定義されていません"))
    (setq cs (string-trim-right cs))
    (setq ce (string-trim ce))
    (if (string-empty-p ce)
        (concat cs " " text)
      (concat cs " " text " " ce))))

(defun my-insert-project-pseudo-absolute-path-comment (root)
  "ROOT を起点とした疑似絶対パスを、現在行にコメント 1 行として挿入する。"
  (interactive
   (list
    (read-directory-name "Set Project Root Dir: " nil nil t)))
  (unless buffer-file-name
    (error "このバッファはファイルに紐づいていません"))
  (let* ((pseudo-path (my--project-pseudo-absolute-path root buffer-file-name))
         (comment-line (my--make-comment-line pseudo-path)))
    (save-excursion
      (beginning-of-line)
      (insert comment-line "\n"))))

(provide 'my-path-comment)
