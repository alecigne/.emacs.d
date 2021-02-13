(use-package elfeed
  :commands elfeed
  :bind (("C-x w" . elfeed)
         :map elfeed-search-mode-map
         ("j" . next-line)
         ("k" . previous-line)
         ("c" . swanemacs-elfeed-clear-filter))
  :config
  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))

  (setq elfeed-feeds
        '("https://news.ycombinator.com/rss"
          ("https://www.reddit.com/r/emacs/.rss" emacs)
          ("http://sachachua.com/blog/category/emacs-news/feed" emacs)
          ("http://endlessparentheses.com/atom.xml" emacs)
          ("http://www.masteringemacs.org/feed/" emacs)
          ("http://emacs-fu.blogspot.com/feeds/posts/default" emacs)
          ("http://emacsredux.com/atom.xml" emacs)
          ("http://emacshorrors.com/feed.atom" emacs)
          ("http://pragmaticemacs.com/feed/" emacs)
          ("https://xkcd.com/atom.xml")
          ("http://kitchingroup.cheme.cmu.edu/blog/category/emacs/feed/" emacs))
        elfeed-search-filter "")

  (defun swanemacs-elfeed-clear-filter ()
    (interactive)
    (setq elfeed-search-filter "")
    (elfeed-update)))

(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org))

(provide 'swanemacs-mail-news)
