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
;; * 0.4: better defcustom, non-broken with-blog-settings macro

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
;;   `octopress-blog-registry', otherwise things won't function. Once a blog exists,
;;   the module will do its best to accomodate you and not make you customize
;;   further. It's infeasible to automatically locate Octopress installs or I'd
;;   try doing that.

;;; Code:

(defconst octopress-tentacle-version "0.3"
  "Version of the octopress-tentacle module.")

(defgroup octopress nil
  "Octopress functions and tools."
  :prefix "octopress-"
  :group 'external
  ;; Using 'external' group because much of this is dedicated to interacting
  ;; with external Ruby tools.
  )

(defcustom octopress-blog-registry
  `(("my_blog"
     (blog-path . ,(concat (file-name-as-directory (getenv "HOME")) "blog"))
     (default-author . ,(user-full-name))
     (ruby-kind . "")
     (ruby-version . "")))
  "An alist of known blogs and metadata about them.
This structure tells octopress-tentacle that blogs exist and holds data that
makes dealing with them easier. It is a compound alist whose car, NAME, is the
name that octopress-tentacle will use to find a blog and whose cdr contains
information about the blog that octopress-tentacle needs. Only NAME and
BLOG-PATH are necessary: DEFAULT-AUTHOR is optional, while RUBY-KIND and
RUBY-VERSION contain data that octopress-tentacle will find by itself if they
are not filled in.
  * BLOG-PATH: Location of the blog's root folder - the directory that holds
               Octopress' _config.yml, config.rb, Rakefile, etc.
  * DEFAULT-AUTHOR: The author name that will be attached to new posts. This
                    feature is meant for users who maintain more than one
                    Octopress blog, for users who incorporate posts from others
                    (e.g. submissions) into their own blog, or for users who
                    maintain 'in-character' fiction-centric blogs that use a
                    conceit of multiple authorship.
  * RUBY-KIND: Whether the given blog executes using a Ruby supplied by
                 rbenv, by rvm, or by the system.
  * RUBY-VERSION: The version of Ruby that this blog uses to run Octopress.
"
  :type '(alist
          :tag "Octopress blogs"
          :key-type (string :tag "Name")
          :value-type (list :tag "Properties"
            :extra-offset 4
            (cons :format "%v"
                  (const :format "" blog-path)
                  (directory :tag "Path to blog files"))
            (cons :format "%v"
                  (const :format "" default-author)
                  (string :tag "Default author for new posts"))
            (cons :format "%v"
                  (const :format "" ruby-kind)
                  (string :tag "Type of Ruby to use"))
            (cons :format "%v"
                  (const :format "" ruby-version)
                  (string :tag "Version of Ruby to use"))))
  :group 'octopress)

(defcustom octopress-clobber-existing-posts nil
  "Whether to overwrite existing posts when there are collisions.
This variable is consulted when a new post or page is being created and the
filename that Octopress chooses for it collides with an existing filename. When
this variable is nil, we bail out instead of touching the existing
file. Otherwise the existing file gets clobbered."
  :type 'boolean :risky nil :group 'octopress)

;; FUTURE: Add a couple of hooks - e.g. after-create-new-post,
;; after-create-new-page, after-generate (allows auto-deploy!), things like
;; that to make it easier to tweak this stuff. It's also possible that we'll
;; want a defcustom that controls the fallback-to-default-blog behavior. Let's
;; not implement that until what we already are attempting, is working.

(defun octopress-blog-name-or-default (blog-name)
  "Returns the default blog unless BLOG-NAME is a known blog.
Returns BLOG-NAME unless BLOG-NAME is not in `octopress-blog-registry', in
which case it returns a string that is the name of the default blog (i.e. the
first blog in `octopress-blog-registry')."
  (if (assoc blog-name octopress-blog-registry)
      (identity blog-name)
      (message (concat
                "Couldn't find a blog named '"
                (pp-to-string blog-name)
                "', falling back on default blog '"
                (caar octopress-blog-registry)
                "'."))
      (caar octopress-blog-registry)))

(defun octopress-blog-default-author (blog-name)
  "Returns the default author for BLOG-NAME if there is one."
  (setq listed-author
        (cdr (assoc 'default-author
               (assoc blog-name octopress-blog-registry))))
  (unless (or (string-equal "" listed-author) (not listed-author)) listed-author))

(defun octopress-blog-has-no-setting (blog-name data-kind)
  "Returns t if blog BLOG-NAME lacks a setting for DATA-KIND."
  (let ((blog-data (assoc blog-name octopress-blog-registry)))
    (when (or
         (not (assoc data-kind blog-data))
         (string-equal "" (cdr (assoc data-kind blog-data))))
      t)))

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
  (setq blog-data
        (assoc (octopress-blog-name-or-default blog-name) octopress-blog-registry))
  (let ((blog-name (car blog-data))
        (blog-path (file-name-as-directory (cdr (assoc 'blog-path blog-data)))))
    (cond
     ((string-match "set by" (shell-command-to-string "bash -l -c 'rbenv version'")) (setq ruby-kind "rbenv"))
     ((string-match "ruby" (shell-command-to-string "rvm info")) (setq ruby-kind "rvm"))
     ((string-match "ruby" (shell-command-to-string "ruby --version")) (setq ruby-kind "system"))
     (t (error "Can't find a local Ruby to run")))
    (cond
     ((string-equal ruby-kind "rbenv")
      (setq ruby-version (shell-command-to-string (concat "cat " blog-path ".rbenv-version"))))
     ((string-equal ruby-kind "rvm")
      (setq ruby-version (replace-regexp-in-string "rvm use " "" (shell-command-to-string (concat "cat " blog-path ".rvmrc")))))
     ((string-equal ruby-kind "system")
      (setq ruby-version (replace-regexp-in-string "ruby \\([123]\\.[0-9][^\\[\\[()]+\\) .+" "\\1" (shell-command-to-string "ruby --version")))))
    ;; Remove the trailing newline that comes with shell commands.
    (setq ruby-version (replace-regexp-in-string "\\(.+\\)[\n]$" "\\1" ruby-version))
    ;; (message (concat "Using kind: '" ruby-kind "' and version: '" ruby-version "' for blog: " blog-name))
    (list ruby-kind ruby-version)))

(defun octopress-is-ruby-kind (blog-name ruby-kind)
  "Returns t if BLOG-NAME's ruby kind is RUBY-KIND."
  (when
      (string-equal (octopress-ruby-kind-of-blog blog-name) ruby-kind)
      ruby-kind))

;; TODO: Merge the following two functions in some sensible way.
(defun octopress-ruby-kind-of-blog (blog-name)
  "Returns a string holding the kind of BLOG-NAME's Ruby.
If BLOG-NAME's ruby kind is not listed in `octopress-blog-registry', add it to
the registry as well as reporting it to the caller."
  (unless (assoc blog-name octopress-blog-registry)
    (error "No blog with that name."))
  (let ((ruby-kind
         (cdr (assoc 'ruby-kind
          (cdr (assoc blog-name octopress-blog-registry)))
          )))
    (if (or (string-equal "" ruby-kind) (not ruby-kind))
        (let ((ruby-kind (elt (octopress-check-ruby-flavor blog-name) 0)))
          (setf
           (cdr (assoc 'ruby-kind
                       (cdr (assoc blog-name octopress-blog-registry)))) ruby-kind)
          (message (concat "Ruby kind not in registry. Set to: " ruby-kind)))
          ruby-kind)))

(defun octopress-ruby-version-of-blog (blog-name)
  "Returns a string holding the version number of BLOG-NAME's Ruby.
If BLOG-NAME's ruby version is not listed in `octopress-blog-registry', add it
to the registry as well as reporting it to the caller."
  (unless (assoc blog-name octopress-blog-registry)
    (error "No blog with that name."))
  (let ((ruby-version
         (cdr (assoc 'ruby-version
          (cdr (assoc blog-name octopress-blog-registry)))
          )))
    (if (or (string-equal "" ruby-version) (not ruby-version))
        (let ((ruby-version (elt (octopress-check-ruby-flavor blog-name) 1)))
          (setf
           (cdr (assoc 'ruby-version
                       (cdr (assoc blog-name octopress-blog-registry)))) ruby-version)
          (message (concat "Ruby version not in registry. Set to: " ruby-version)))
          ruby-version)))

(defmacro octopress-with-blog-settings (blog-name &optional author &rest body)
  "Enables falling back to default blog, author, etc.
Designed to eliminate boilerplate from functions that repeatedly needed to
check whether a blog exists in `octopress-blog-registry' and to get subsquent
data from there: this macro grabs the relevant data and makes it available in
the local by wrapping BODY in a let-block."
  (let ((blog-name-from-registry
         (lambda (blog-name)
           (octopress-blog-name-or-default blog-name)))
        (get-blog-data
         (lambda (blog key)
           (cdr (assoc key (cdr (assoc blog octopress-blog-registry)))))))
    `(let ((blog-id (funcall ,blog-name-from-registry ,blog-name)))
       (let ((blog-id blog-id)
             (author (if (not ,author) (octopress-blog-default-author blog-id) ,author))
             (blog-path (file-name-as-directory (funcall ,get-blog-data 'blog-path blog-id)))
             (ruby-kind
              (if (octopress-blog-has-no-setting 'ruby-kind blog-id)
                  (octopress-ruby-kind-of-blog blog-id)
                (funcall ,get-blog-data 'ruby-kind blog-id)))
             (ruby-version
              (if (octopress-blog-has-no-setting 'ruby-version blog-id)
                  (octopress-ruby-version-of-blog blog-id)
                (funcall ,get-blog-data 'ruby-version blog-id))))
         (message (concat "Set up with data: "
                          (pp-to-string
                           (list blog-id author blog-path ruby-kind ruby-version))))
     ,@body))))

(defun octopress-rake-task (task blog-name blog-path output-buffer)
  "Run a rake command for the given blog.
This function changes the current directory to the path for BLOG-NAME and then
uses rake to execute TASK in that environment, doing its best to respect the
constraints imposed by rbenv/rvm. Output will be sent to OUTPUT-BUFFER, which
is not optional because we assume that the caller cares about the output."
    (cd blog-path)
    (let ((rake-format-string "rake %s")
          (rake-command-prefix "bash -l -c '")
          (rake-command-suffix "'")
          (ruby-version (octopress-ruby-version-of-blog blog-name)))
      (when octopress-clobber-existing-posts
        (concat "yes | " rake-format-string))
      ;; TODO: Fail more gracefully when clobber-existing is nil.
      (cond
       ((octopress-is-ruby-kind blog-name "rbenv")
        (setq rake-format-string
              (concat "eval \"$(rbenv init -)\" && rbenv local "
                      ruby-version
                      " && "
                      rake-format-string)))
       ((octopress-is-ruby-kind blog-name "rvm")
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
      ))

(defun octopress-make-new-post (post-title &optional blog-name author)
  "Create a new OctoPress blog post, prompting user for a title.
Used interactively, creates a new post for the first blog in the
`octopress-blog-registry' alist with the Author field filled in based on the
corresponding default-author value, if any.

Used programmatically, creates a post for the BLOG-NAME blog if BLOG-NAME is
given and exists in `octopress-blog-registry' or for the default blog if BLOG-NAME
is omitted. If AUTHOR is provided, its value is used for the post, if not, the
default name is used.

After running the necessary shell commands, this function visits, in the
current window, the file that represents the new blog post. Because it is
designed to be invoked when you want to write a blog post right this moment, it
does not use `save-window-excursion' or similar."

  (interactive
   (let ((post-title (read-string "New post's title: "))
         (blog-name
          (if (> (length octopress-blog-registry) 1)
              (read-string "Use which blog? " nil nil (caar (octopress-blog-registry)))
            (caar (octopress-blog-registry)))))
     (list post-title blog-name (octopress-blog-default-author (blog-name)))))

  (message (concat
            "Using blog '" (pp-to-string blog-name)
            "' and author '" (pp-to-string author) "'."))
  (if (region-active-p)
      ;; if we've got an active region, base the new post on its contents.
      (let ((p1 (region-beginning))
            (p2 (region-end)))
        (setq new-post-blockquote
              (concat "\n<blockquote>\n" (buffer-substring-no-properties p1 p2) "\n</blockquote>\n")))
        (setq new-post-blockquote nil))
  (octopress-with-blog-settings blog-name author
    (setq working-buffer (generate-new-buffer "*Octopress New Post*"))
    (switch-to-buffer working-buffer)
    (octopress-rake-task (format "new_post[\"%s\"]" post-title) blog-name blog-path working-buffer)
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
    (find-file (concat blog-path file-name-string))
    (when (and (boundp 'new-post-blockquote) new-post-blockquote)
      (goto-char (point-max))
      (insert new-post-blockquote))
    (when author
      (goto-char (point-min))
      (re-search-forward (concat "date: " (substring (buffer-name) 0 11)) (point-max) t)
      (move-end-of-line nil)
      (insert (concat "\nAuthor: " author)))))

(defun octopress-make-new-page (page-title &optional blog-name author)
  "Docstring."
  ;; (octopress-with-blog-settings blog-name author
    (message "Not yet implemented."))

(defun octopress-generate-site (blog-name)
  "Docstring."
  t)

(provide 'octopress-tentacle)
;;; octopress-tentacle.el ends here
