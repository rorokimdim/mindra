# mindra

A command-line wrapper for [diagrams](https://diagrams.github.io/) and [gloss](http://gloss.ouroborus.net/) so we can leverage them outside haskell.

The goal is to provide a good subset of features from both libraries.

See [mindra-clj](https://github.com/rorokimdim/mindra-clj) for an example of a client library. It talks to `mindra` via `stdin`/`stdout` using just formatted text.

# Current status

## Diagrams

Only the SVG backend is supported, and only a very small subset of `diagrams` is exposed. See [svg-parser](https://github.com/rorokimdim/mindra/blob/f4ca8203716f3fffc8c59ffabfd91b4b47c902e8/src/Mindra/Diagrams/Parser/SVG.hs#L753-L817) for what is supported and how the commands are parsed into diagram(s).

See [mindra-clj-diagrams](https://github.com/rorokimdim/mindra-clj#diagrams) for some examples.

## Gloss

Most of the `gloss` features are supported. We should be able to use `mindra` for creating both static pictures and animations (with event handling!). See [gloss-parser](https://github.com/rorokimdim/mindra/blob/f4ca8203716f3fffc8c59ffabfd91b4b47c902e8/src/Mindra/Gloss/Parser/Picture.hs#L255-L278) for what is supported and how the commands are parsed into gloss picture(s).

See [mindra-clj-gloss](https://github.com/rorokimdim/mindra-clj#gloss) for some examples.

# Installation

## Linux and Mac

Install:

```bash
brew install rorokimdim/brew/mindra
```

Upgrade:

```bash
brew upgrade mindra
```

Uninstall:

```bash
brew uninstall mindra
```

## Windows

Binaries are available at [releases](https://github.com/rorokimdim/mindra/releases).

## Others

No pre-built binaries available at this time. We will need to build from source using `stack install` or `cabal install`.

Install [stack](https://docs.haskellstack.org/en/stable/README/), clone this repository and run the following in repository directory.

```bash
stack install
```

# Basic usage

A. Start mindra command

```bash
mindra
```

It should print `READY INIT` which means it is ready to receive the `INIT` (initialization) command.

B. Initialize it for either diagrams or gloss

**For diagrams**

Configure for SVG of size 300px by 400px:

```bash
INIT Diagrams SVG 300 400


```

Note: Each command should be followed by a blank line.

**For gloss**

Configure for a window of size 500px by 500px, at position 10px, 10px on the screen, with the title "My Title", and white background color (red, green, blue, alpha values):

```bash
INIT Gloss
Window 500 500 10 10 "My Title"
Color 255 255 255 255


```


Note: Each command should be followed by a blank line.

C. Draw something

**For diagrams**

```bash
SVG Circle 100


```

**For gloss**

```bash
PICTURE Circle 100


```

Note: Each command should be followed by a blank line.

Hit `ESC` to close window.

# Credits

1. [Haskell](https://www.haskell.org/)
2. [Diagrams](https://diagrams.github.io/) and [Gloss](http://gloss.ouroborus.net/)
3. All of these [libraries](https://github.com/rorokimdim/mindra/blob/220050cbc0f360b6c2322b501a43cf3f7f02133f/package.yaml) and all the things they depend on
