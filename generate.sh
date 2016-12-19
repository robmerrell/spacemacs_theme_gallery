#!/usr/bin/env bash
BASE=$(dirname $BASH_SOURCE)
SAMPLE=${1:-$BASE/sample.exs}
TMPEL=$(mktemp)
TARGET=$BASE/index.html
cat > $TMPEL <<EOF
(spacemacs/toggle-debug-on-error-on)
(unless (locate-library "htmlize") (package-install 'htmlize))
(add-to-load-path "$BASE")
(cd "$BASE")
(load "generate.el")
(find-file "$SAMPLE")
(generate-theme-gallery)
(with-current-buffer (get-buffer "theme-gallery")
  (write-file "$TARGET"))
(browse-url "file://$TARGET")
(kill-emacs)
EOF
emacs --load $TMPEL && \
    echo "Gallery generated at $TARGET"
