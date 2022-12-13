# qml-integration


Provide functions to easily choose a QML file to run it with qmlscene
or with qmltestrunner.

The main motivation for this package is to make my own workflow when
programming in QML in Emacs smoother.


# Installation

At the moment the package is available in [GitHub](https://github.com/darcamo/qml-integration). You can just download the `qml-integration.el` file there and put somewhere where Emacs can find it. Alternatively, you can use something like [straight](https://github.com/raxod502/straight.el), which can directly install packages from a git repository.


# Usage

Just call `qml-integration-run-qmlscene` or
`qml-integration-run-qmltestrunner`. You can also choose a style with
`qml-integration-choose-qml-style`.


# Configuration

TODO

# Project Configuration

Emacs standard project infrastructure is used in qml-integration to
get the project root folder when finding the correct build folder.
There is nothing you need to do to configure this. However, the
`project` built-in package only uses version control to find the
project root. If you want to use a sub-folder in a repository as the
project root, or if you are not using version control, you can extend
the built-in project as described
[here](https://www.manueluberti.eu/emacs/2020/11/14/extending-project/)
in order to make Emacs recognize a folder containing an empty file
with a pre-determined name as a project.


# Screenshots

TODO
