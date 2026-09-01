export SWANEMACS_ROOT := justfile_directory()

prepare:
    emacs --batch \
        --eval '(setq user-emacs-directory (file-name-as-directory (getenv "SWANEMACS_ROOT")))' \
        --load "$SWANEMACS_ROOT/early-init.el" \
        --load "$SWANEMACS_ROOT/init.el" \
        --load buttercup \
        --eval '(setq default-directory (file-name-as-directory (getenv "SWANEMACS_ROOT")))' \
        --funcall buttercup-run-discover \
        -- "$SWANEMACS_ROOT/test"
