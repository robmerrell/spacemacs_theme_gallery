# spacemacs theme gallery

http://themegallery.robdor.com/

A theme gallery for themes bundled with Spacemacs

## Generate your theme-gallery

The following command will execute `emacs` and open a buffer with the contents of `sample.exs`
and generate a `index.html` file with your currently installed themes.

```shell
./generate.sh sample.exs
```

## From inside emacs

To generate the gallery open a file that you would like to use as the theme sample code and invoke: (generate-theme-gallery)

A new buffer will be created with the contents of the theme gallery's html.

You need to have the `htmlize` package already installed for this to work.
