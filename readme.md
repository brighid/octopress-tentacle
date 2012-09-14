# Octopress Tentacles #

### Maintaining multiple Octopress blogs with emacs ###

`octopress-tentacle.el` is an emacs facility for managing Octopress blogs. Its
goal is to make it easier for an emacs-centric writer to create posts for
multiple Octopress-based blogs and to upload to those blogs in fewer steps.

This chunk of elisp exists because, in classic engineer fashion, I didn't
like the existing options. I want it to be better than other elisp that
deals with Octopress in these ways:
* Portable: not tied to a single person's setup, nor tied to rvm or rbenv.
  The other elisp-octopress implementations I've seen have not done this.
* Extensible: I want this tool to support multiple blogs. I come from fandom
  culture as well as engineering culture. Engineers usually maintain just
  one blog, if any, but it's pretty routine in the fandom community for
  people to maintain multiple writing streams for different purposes. This
  is also where the name comes from: the Octopress octopus has many
  tentacles, and each could be doing something different.
* Graceful: the other elisp-octopress implementations I've seen are brittle
  and prone to leaving users looking at an elisp stack trace. I tried to
  make sure that this one tells you what it's doing via `*Messages*`, leaves
  notes when it falls over, and uses the Customize facility to make life
  easier for the user.

#### Issues: ####
* I could declare a dependency on markdown-mode, but choose not to because
  editing Markdown content in text-mode is simple. Similarly, there are some
  tempting things in the cl package, but I tried to stay away from those.
* Currently I have no idea whether this works on emacs in a Windows
  environment, I would be happy to hear from ntemacs users on that point.
* Requires some customization - you have to enter a blog name and a path for
  `octopress-blog-paths', otherwise things won't function. Once a blog exists,
  the module will do its best to accomodate you and not make you customize
  further. It's infeasible to automatically locate Octopress installs or I'd
  try doing that.
