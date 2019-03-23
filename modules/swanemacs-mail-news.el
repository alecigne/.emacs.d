(use-package elfeed
  :ensure   t
  :commands elfeed
  :config
  (setq elfeed-feeds
          '("https://news.ycombinator.com/rss"
            ("https://www.reddit.com/r/emacs/.rss" emacs)
            ("http://sachachua.com/blog/category/emacs-news/feed" emacs)
            ("http://endlessparentheses.com/atom.xml" emacs)
            ("http://www.masteringemacs.org/feed/" emacs)
            ("http://emacs-fu.blogspot.com/feeds/posts/default" emacs)
            ("http://emacsredux.com/atom.xml" emacs)
            ("http://emacshorrors.com/feed.atom" emacs)
            ("http://pragmaticemacs.com/feed/" emacs))
          elfeed-search-filter ""))

(provide 'swanemacs-mail-news)
