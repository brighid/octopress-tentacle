;;; octopress-tentacle.el --- Tools for Octopress/Jekyll blogs. -*- coding: utf-8 -*-

;; Copyright (C) 2012 Brighid McDonnell

;; Author: Brighid McDonnell <brighid@stronglyemergent.com>
;; Created: 12 Sep 2012
;; Version: 0.3
;; Keywords: tools, octopress, jekyll

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Change Log:
;; * 0.1-3: create defcustoms, rake-applying defuns, & octopress-make-new-post

;;; Commentary:
;; This chunk of elisp exists because, in classic engineer fashion, I didn't
;; like the existing options. I want it to be better than other elisp that
;; deals with Octopress in these ways:
;; * Portable: not tied to a single person's setup, nor tied to rvm or rbenv.
;;   The other elisp-octopress implementations I've seen have not done this.
;; * Extensible: I want this tool to support multiple blogs. I come from fandom
;;   culture as well as engineering culture. Engineers usually maintain just
;;   one blog, if any, but it's pretty routine in the fandom community for
;;   people to maintain multiple writing streams for different purposes. This
;;   is also where the name comes from: the Octopress octopus has many
;;   tentacles, and each could be doing something different.
;; * Graceful: the other elisp-octopress implementations I've seen are brittle
;;   and prone to leaving users looking at an elisp stack trace. I tried to
;;   make sure that this one tells you what it's doing via *Messages*, leaves
;;   notes when it falls over, and uses the Customize facility to make life
;;   easier for the user.

;; Issues:
;; * I could declare a dependency on markdown-mode, but choose not to because
;;   editing Markdown content in text-mode is simple. Similarly, there are some
;;   tempting things in the cl package, but I tried to stay away from those.
;; * Currently I have no idea whether this works on emacs in a Windows
;;   environment, I would be happy to hear from ntemacs users on that point.
;; * Requires some customization - you have to enter a blog name and a path for
;;   `octopress-blog-paths', otherwise things won't function. Once a blog exists,
;;   the module will do its best to accomodate you and not make you customize
;;   further. It's infeasible to automatically locate Octopress installs or I'd
;;   try doing that.

;;; Code:

(defconst octopress-tentacle-version "0.3"
  "Version of the octopress-tentacle module.")

(defcustom octopress-blog-paths `(("my_blog" .
                              ,(concat (file-name-as-directory (getenv "HOME")) "blog")))
  "An alist of Octopress blogs and their paths.
Each local blog has a name that other Octopress-related functions will use to
refer to it. The default is provided both as an example and because more users
are likely to have the local copy of their Octopress blog at ~/blog than
anywhere else. The path to the blog should be the path to the directory that
holds _config.yml, config.rb, Rakefile, etc.

Elements are structured as ( NAME . PATH ).
  NAME is the short name of the blog, a string.
  PATH is the location of the blog's files - the directory must already exist."

  :type '(alist :key-type (string :tag "Blog name (keep it short)")
                :value-type (directory :tag "Path to blog's local files"))
  :risky nil
  :group 'octopress
)

;; It's possible that we'll want a defcustom that controls the
;; fallback-to-default-blog behavior. Let's not implement that until what we
;; already are attempting, is working.

(defcustom octopress-blog-authors `(("my_blog" . ,(user-full-name)))
  "An alist of Octopress blogs and default authors for each.
Each blog has a default author associated with it that will be added to new
posts to that blog. This feature is meant for users who maintain more than one
Octopress blog, for users who incorporate posts from others (e.g. submissions)
into their own blog, or for users who maintain 'in-character' fiction-centric
blogs that use a conceit of multiple authorship.

Elements are structured as ( BLOG . NAME ).
  BLOG is the short name of the blog, a string.
  NAME is the author name to use for the given blog, as a string."

  :type '(alist :key-type (string :tag "Blog name (must match a name in octopress-blog-paths)")
                :value-type (string :tag "Default author name to use for the given blog"))
  :risky nil
  :group 'octopress
)

(defcustom octopress-clobber-existing-posts nil
  "Whether to overwrite existing posts when there are collisions.
This variable is consulted when a new post or page is being created and the
filename that Octopress chooses for it collides with an existing filename. When
this variable is nil, we bail out instead of touching the existing
file. Otherwise the existing file gets clobbered."
  :type 'boolean :risky nil :group 'octopress)

;; FUTURE: Add a couple of hooks - e.g. after-create-new-post,
;; after-create-new-page, after-generate (allows auto-deploy!), things like
;; that to make it easier to tweak this stuff.

(defun octopress-blog-name-or-default (blog-name)
  "Returns the default blog unless BLOG-NAME is a known blog.
If the given argument is not in `octopress-blog-paths', this function returns
the default blog (i.e. the first in the list), otherwise it returns BLOG-NAME."
  (if (assoc blog-name octopress-blog-paths)
      blog-name
    (message "Couldn't find a saved blog with the given name, falling back on the default blog.")
    (caar octopress-blog-paths)))

(defun octopress-blog-author-or-default (blog-name)
  "Returns the default author for BLOG-NAME if there is one."
  (let ((blog-name (octopress-blog-name-or-default blog-name))
        (author-exists (assoc blog-name octopress-blog-authors)))
    ;; TODO: Figure out how to indicate author fallback, parallel to how we
    ;; message about blog-name fallback.
    (when author-exists
      (cdr author-exists))))

(defmacro octopress-with-defaults (blog-name &optional author &rest body)
  "Enables falling back to default blog, author, etc.
Designed to eliminate boilerplate from functions that repeatedly needed to
check whether a blog exists in `octopress-blog-paths' and to get subsquent data
from there: this makes the whole shebang available and wraps BODY in a
let-block."
  (setq blog-name (octopress-blog-name-or-default blog-name))
  (setq author (if (not author) (octopress-blog-author-or-default blog-name) author))
  `(let ((blog-name ,blog-name)
        (author ,author)
        (blog-path ,(file-name-as-directory (cdr (assoc blog-name octopress-blog-paths))))
        (home-path ,(getenv "HOME")))
     ,@body))

(defun octopress-check-ruby-flavor (blog-name)
  "Figure out which flavor of Ruby the BLOG-NAME blog is running under.
By \"flavor\" of Ruby we mean \"combination of version number and
rvm/rbenv/system-ruby choice\".

This function checks for use of rbenv first, then checks rvm, then falls back
on system Ruby. It returns the kind of Ruby that the blog appears to be running
and the version thereof. This function is assuming that no-one is using both
rbenv and rvm and that everyone who is using rbenv or rvm is using them for all
of their octopress blogs - if those assumptions don't hold, this function may
break.

Returns a list whose car a string for the kind of ruby found and whose cdr is a
string for the version number."
;; FUTURE: turn this into an automatically calculated defcustom. Not yet,
;; though, because just running a few non-destructive shell commands is cheap.
  (octopress-with-defaults blog-name nil
    (cond
     ((string-match "set by" (shell-command-to-string "bash -l -c 'rbenv version'")) (setq ruby-kind "rbenv"))
     ((string-match "ruby" (shell-command-to-string "rvm info")) (setq ruby-kind "rvm"))
     ((string-match "ruby" (shell-command-to-string "ruby --version")) (setq ruby-kind "system"))
     (t (error "Can't find a local Ruby to run")))
    (cond
     ((string-equal ruby-kind "rbenv")
      (setq ruby-version (shell-command-to-string (concat "cat " (file-name-as-directory blog-path) ".rbenv-version"))))
     ((string-equal ruby-kind "rvm")
      (setq ruby-version (replace-regexp-in-string "rvm use " "" (shell-command-to-string (concat "cat " (file-name-as-directory blog-path) ".rvmrc")))))
     ((string-equal ruby-kind "system")
      (setq ruby-version (replace-regexp-in-string "ruby \\([123]\\.[0-9][^\\[\\[()]+\\) .+" "\\1" (shell-command-to-string "ruby --version")))))
    ;; Remove the trailing newline that comes with shell commands.
    (setq ruby-version (replace-regexp-in-string "\\(.+\\)[\n]$" "\\1" ruby-version))
    (message (concat "Using kind: '" ruby-kind "' and version: " ruby-version " for task."))
    (list ruby-kind ruby-version)))

(defun octopress-ruby-kind-is (blog-name ruby-kind)
  "Returns t if BLOG-NAME's ruby flavor is RUBY-KIND."
  (if (string-equal (car (octopress-check-ruby-flavor blog-name)) ruby-kind)
      ruby-kind
    nil))

(defun octopress-ruby-version-is (blog-name)
  "Returns a string holding the version number of BLOG-NAME's Ruby."
  (cdr (octopress-check-ruby-flavor blog-name)))

(defun octopress-rake-task (blog-name task output-buffer)
  "Run a rake command for the given blog.
This function changes the current directory to the path for BLOG-NAME and then
uses rake to execute TASK in that environment, doing its best to respect the
constraints imposed by rbenv/rvm. Output will be sent to OUTPUT-BUFFER, which
is not optional because we assume that the caller cares about the output."
  (octopress-with-defaults blog-name nil
    (cd blog-path)
    ;; `blog-path' provided by `octopress-with-defaults'
    (let ((rake-format-string "rake %s")
          (rake-command-prefix "bash -l -c '")
          (rake-command-suffix "'")
          (ruby-version (octopress-ruby-version-is blog-name)))
      (when octopress-clobber-existing-posts
        (concat "yes | " rake-format-string))
      ;; TODO: Fail more gracefully when clobber-existing is nil.
      (cond
       ((octopress-ruby-kind-is blog-name "rbenv")
        (setq rake-format-string
              (concat "eval \"$(rbenv init -)\" && rbenv local "
                      ruby-version
                      " && "
                      rake-format-string)))
       ((octopress-ruby-kind-is blog-name "rvm")
        (setq rake-format-string
              (concat "rvm use " ruby-version " && " rake-format-string)))
              ;; This should make rvm happy.
       (t (message (format "Using system Ruby for task '%s'" task))))
      (shell-command
       (concat rake-command-prefix
               (format rake-format-string task)
               rake-command-suffix)
       output-buffer  ;; send both stdout and
       output-buffer) ;; stderr to our working buffer.
      )))

(defun octopress-make-new-post (post-title &optional blog-name author)
  "Create a new OctoPress blog post, prompting user for a title.
Used interactively, creates a new post for the first blog in the
`octopress-blog-paths' alist with the Author field filled in based on the
corresponding `octopress-blog-authors' entry, if any.

Used programmatically, creates a post for the BLOG-NAME blog if BLOG-NAME is
given and exists in `octopress-blog-paths' or for the default blog if BLOG-NAME
is omitted. If AUTHOR is provided, its value is used for the post, if not, the
default name is used.

After running the necessary shell commands, this function visits, in the
current window, the file that represents the new blog post. Because it is
designed to be invoked when you want to write a blog post right this moment, it
does not use `save-window-excursion' or similar."

  (interactive "MPost title: ")
  (octopress-with-defaults blog-name author
    (when (region-active-p)
      ;; if we've got an active region, base new post on its contents.
      (let ((p1 (region-beginning))
            (p2 (region-end)))
      (setq new-post-blockquote
            (concat "<blockquote>" (buffer-substring-no-properties p1 p2) "</blockquote>"))))
    (setq working-buffer (generate-new-buffer "*Octopress New Post*"))
    (switch-to-buffer working-buffer)
    (octopress-rake-task blog-name (format "new_post[\"%s\"]" post-title) working-buffer)
    (goto-char (point-max))
    (setq name-pattern "Creating new post: \\(.+/[0-9\\-]\\{11\\}.+\\.markdown\\)")
    (setq file-name-string
          (let
              ((file-name-start-point (re-search-backward name-pattern (point-min) t))
               (file-name-end-point (re-search-forward name-pattern (point-max) t)))
            (when (and file-name-start-point file-name-end-point)
              (replace-regexp-in-string name-pattern "\\1"
                (buffer-substring-no-properties file-name-start-point file-name-end-point)))))
    (unless file-name-string
      (message "Try looking in *Octopress New Post* to see command output.")
      (error "Can't find the file that should have been created"))
    (find-file (concat (file-name-as-directory blog-path) file-name-string))
    (when (and (boundp 'new-post-blockquote) new-post-blockquote)
      (goto-char (point-max))
      (insert new-post-blockquote))))

(defun octopress-make-new-page (page-title &optional blog-name author)
  "Docstring."
  (octopress-with-defaults blog-name author
    (message "Not yet implemented.")))

(defun octopress-generate-site (blog-name)
  "Docstring."
  t)

(provide 'octopress-tentacle)
;;; octopress-tentacle.el ends here
