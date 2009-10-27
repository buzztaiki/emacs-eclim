## Overview
Eclim is an Eclipse plugin which exposes Eclipse features through a
server interface.  When this server is started, the command line utility
eclim can be used to issue requests to that server.

Emacs-eclim uses the eclim server to integrate eclipse with
emacs. This project wants to bring some of the invaluable features
from eclipse to emacs.

## Installation
1. download the latest eclim distribution for your platform => [Eclim Project Page](http://eclim.sourceforge.net/)
1. install eclim and follow the instructions
1. download the emacs-eclim source code
1. add the following code to your emacs startup script

        (add-to-list 'load-path (expand-file-name "/path/to/emacs-eclim/"))
        (require 'eclim)

        (setq eclim-auto-save t)
        (global-eclim-mode)

## Usage
To get started just lunch the eclim executable that the installer placed in your
Eclipse directory.

### Projects
The easiest way to manage your eclipse projects in emacs is to use the `eclim-manage-projects` command. This opens up a new buffer containing all the projects of your eclipse workspace.

### Ant
You can use the function `eclim-ant-run` to execute a given ant target from the current project. Be aware that emacs-eclim needs to be able to locate your build.xml file. You can use the `eclim-ant-directory` variable or overwrite the `eclim--ant-buildfile-name` function to customize how eclim locates your main buildfile.

### Maven
Beside Ant you can now run maven lifecycle phases or call a specific goal. This works in all your project files, as long as emacs-eclim recognizes the eclipse project file. To run a maven phase just call `eclim-maven-lifecycle-phase-run` or `eclim-maven-run` to enter the name of a goal.

## Contributing

The project is pretty new and needs a lot of work. If you have some
spare time and want to help us improve java development on emacs, just
fork the project and send me a pull-request, once you want me to merge
it back into the repository.

## Troubleshooting
Make sure that you installed eclim correctly. The installer should have placed the following files into your Eclipse installation directory:

    $ECLIPSE_HOME/eclimd
    $ECLIPSE_HOME/plugins/org.eclim_1.4.5/bin/eclim

Furthermore emacs eclim relies on a `eclim` executable that is placed in your PATH. If you don't want to add the executable installed into your Eclipse plugin directory you can place a script with the following content in your PATH.

    #!/bin/sh
    $ECLIPSE_HOME/plugins/org.eclim_1.4.5/bin/eclim $*
