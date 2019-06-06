#!/bin/bash

# TODO
# add /usr/local/bin_special to path in config setup
# Create flexdll PR
# Check if it works without make_arch_pkg_config
# Error on each first run
# +12:00:32 ocaml.exe configure.ml -no-ask -native-compiler no -prefix ./
# /cygdrive/c/bin/cygwin_coq64_ms_dev_opam\build\makecoq_mingw.sh: line 477: ocaml.exe: command not found
#
# Do not automatically update .bashprofile - the installed env hook is super invasive
# Just do a simple eval $(opam env)

###################### COPYRIGHT/COPYLEFT ######################

# (C) 2016..2019 Intel Deutschland GmbH
# Author: Michael Soegtrop
#
# Released to the public by Intel under the
# GNU Lesser General Public License Version 2.1 or later
# See https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html

###################### Script safety and debugging settings ######################

set -o nounset
set -o errexit
set -x
# Print current wall time as part of the xtrace
export PS4='+\t '

# Set this to 1 if all module directories shall be removed before build (no incremental make)
RMDIR_BEFORE_BUILD=1

###################### NOTES #####################

# - This file goes together with MakeCoq_ForMignGW.bat, which sets up cygwin
#   with all required packages and then calls this script.
#
# - This script uses set -o errexit, so if anything fails, the script will stop
#
# - cygwin provided mingw64 packages like mingw64-x86_64-zlib are installed to
#   /usr/$TARGET_ARCH/sys-root/mingw, so we use this as install prefix

###################### ARCHITECTURES #####################

# The OS on which the build of the tool/lib runs
BUILD=$(gcc -dumpmachine)

# The OS on which the tool runs
# "`find /bin -name "*mingw32-gcc.exe"`" -dumpmachine
HOST=$TARGET_ARCH

# The OS for which the tool creates code/for which the libs are
TARGET=$TARGET_ARCH

# Cygwin uses different arch name for 32 bit than mingw/gcc
case $ARCH in
  x86_64) CYGWINARCH=x86_64 ;;
  i686)   CYGWINARCH=x86 ;;
  *)      false ;;
esac

###################### PATHS #####################

# Name and create some 'global' folders
PATCHES=~/patches
FLAGFILES=~/flagfiles
TARBALLS=~/tarballs
FILELISTS=~/filelists

mkdir -p $FLAGFILES
mkdir -p $TARBALLS
mkdir -p $FILELISTS
mkdir -p $PATCHES
cd ~

# Create source cache folder
mkdir -p "$SOURCE_LOCAL_CACHE_CFMT"

# sysroot prefix for the above /build/host/target combination
PREFIX=$CYGWIN_INSTALLDIR_MFMT/usr/$TARGET_ARCH/sys-root/mingw

# Install / Prefix folder for COQ
PREFIXCOQ=$RESULT_INSTALLDIR_MFMT

# Install / Prefix folder for OCaml
if [ "$INSTALLOCAML" == "Y" ]; then
  PREFIXOCAML=$PREFIXCOQ
else
  PREFIXOCAML=$PREFIX
fi

mkdir -p "$PREFIX/bin"
mkdir -p "$PREFIXCOQ/bin"
mkdir -p "$PREFIXOCAML/bin"

# This is required for building addons and plugins
# This must be CFMT (/cygdrive/c/...) otherwise coquelicot 3.0.2 configure fails.
# coquelicot uses which ${COQBIN}/coqc to check if coqc exists. This does not work with COQBIN in MFMT.
export COQBIN=$RESULT_INSTALLDIR_CFMT/bin/
# This must be MFMT (C:/) otherwise bignums 68a7a3d7e0b21985913a6c3ee12067f4c5ac4e20 fails
export COQLIB=$RESULT_INSTALLDIR_MFMT/lib/coq/

# Copy patches
cp -r /build/patches/* $PATCHES

###################### Copy Cygwin Setup Info #####################

# Copy Cygwin repo ini file and installed files db to tarballs folder.
# Both files together document the exact selection and version of cygwin packages.
# Do this as early as possible to avoid changes by other setups (the repo folder is shared).

# Escape cygwin mirror URL to repo folder name
CYGWIN_REPO_FOLDER=${CYGWIN_REPOSITORY}/
CYGWIN_REPO_FOLDER=${CYGWIN_REPO_FOLDER//:/%3a}
CYGWIN_REPO_FOLDER=${CYGWIN_REPO_FOLDER//\//%2f}

# Copy files
cp "$CYGWIN_LOCAL_CACHE_WFMT/$CYGWIN_REPO_FOLDER/$CYGWINARCH/setup.ini" $TARBALLS
cp /etc/setup/installed.db $TARBALLS

###################### UTILITY FUNCTIONS #####################

# ------------------------------------------------------------------------------
# Get a source tar ball, expand and patch it
# - get source archive from $SOURCE_LOCAL_CACHE_CFMT or online using wget
# - create build folder
# - extract source archive
# - patch source file if patch exists
#
# Parameters
# $1 file server name including protocol prefix
# $2 file name (without extension)
# $3 file extension
# $4 [optional] number of path levels to strip from tar (usually 1)
# $5 [optional] module name (if different from archive)
# $6 [optional] expand folder name (if different from module name)
# $7 [optional] module base name (used as 2nd choice for patches, defaults to $5)
# ------------------------------------------------------------------------------

function get_expand_source_tar {
  # Handle optional parameters
  if [ "$#" -ge 4 ] ; then
    strip=$4
  else
    strip=1
  fi

  if [ "$#" -ge 5 ] ; then
    name=$5
  else
    name=$2
  fi

  if [ "$#" -ge 6 ] ; then
    folder=$6
  else
    folder=$name
  fi

  if [ "$#" -ge 7 ] ; then
    basename=$7
  else
    basename=$name
  fi

  # Get the source archive either from the source cache or online
  if [ ! -f "$TARBALLS/$name.$3" ] ; then
    if [ -f "$SOURCE_LOCAL_CACHE_CFMT/$name.$3" ] ; then
      cp "$SOURCE_LOCAL_CACHE_CFMT/$name.$3" "$TARBALLS"
    else
      wget --progress=dot:giga "$1/$2.$3"
      if file -i "$2.$3" | grep text/html; then
        echo Download failed: "$1/$2.$3"
        echo The file wget downloaded is an html file:
        cat "$2.$3"
        exit 1
      fi
      if [ ! "$2.$3" == "$name.$3" ] ; then
        mv "$2.$3" "$name.$3"
      fi
      mv "$name.$3" "$TARBALLS"
      # Save the source archive in the source cache
      if [ -d "$SOURCE_LOCAL_CACHE_CFMT" ] ; then
        cp "$TARBALLS/$name.$3" "$SOURCE_LOCAL_CACHE_CFMT"
      fi
    fi
  fi

  # Remove build directory (clean build)
  if [ $RMDIR_BEFORE_BUILD -eq 1 ] ; then
    rm -f -r "$folder"
  fi

  # Create build directory and cd
  mkdir -p "$folder"
  cd "$folder"

  # Extract source archive
  case $3 in
    zip) CYGWINARCH=x86_64
      unzip "$TARBALLS/$name.$3"
      if [ "$strip" == "1" ] ; then
        # move subfolders of root folders one level up
        find "$(ls)" -mindepth 1 -maxdepth 1 -exec mv -- "{}" . \;
      else
        echo "Unzip strip count not supported"
        return 1
      fi
      ;;
    tar.* | tgz )
      tar xvaf "$TARBALLS/$name.$3" --strip $strip ;;
    *)
      cp "$TARBALLS/$name.$3" . ;;
  esac

  # Patch if patch file exists
  # First try specific patch file name then generic patch file name
  if [ -f "$PATCHES/$name.patch" ] ; then
    patch -p1 -i "$PATCHES/$name.patch"
  elif  [ -f "$PATCHES/$basename.patch" ] ; then
    patch -p1 -i "$PATCHES/$basename.patch"
  fi

  # Go back to base folder
  cd ..
}

# ------------------------------------------------------------------------------
# Prepare a module build
# - check if build is already done (name.finished file exists) - if so return 1
# - create name.started
# - get source archive from $SOURCE_LOCAL_CACHE_CFMT or online using wget
# - create build folder
# - cd to build folder and extract source archive
# - create bin_special subfolder and add it to $PATH
# - remember things for build_post
#
# Parameters
# $1 file server name including protocol prefix
# $2 file name (without extension)
# $3 file extension
# $4 [optional] number of path levels to strip from tar (usually 1)
# $5 [optional] module name (if different from archive)
# $6 [optional] module base name (used as 2nd choice for patches, defaults to $5)
# ------------------------------------------------------------------------------

function build_prep {
  # Handle optional parameters
  if [ "$#" -ge 4 ] ; then
    strip=$4
  else
    strip=1
  fi

  if [ "$#" -ge 5 ] ; then
    name=$5
  else
    name=$2
  fi

  if [ "$#" -ge 6 ] ; then
    basename=$6
  else
    basename=$name
  fi

  # Check if build is already done
  if [ ! -f "$FLAGFILES/$name.finished" ] ; then
    BUILD_PACKAGE_NAME=$name
    BUILD_OLDPATH=$PATH
    BUILD_OLDPWD=$(pwd)

    touch "$FLAGFILES/$name.started"

    get_expand_source_tar "$1" "$2" "$3" "$strip" "$name" "$name" "$basename"

    cd "$name"

    # Create a folder and add it to path, where we can put special binaries
    # The path is restored in build_post
    mkdir bin_special
    PATH=$(pwd)/bin_special:$PATH

    return 0
  else
    return 1
  fi
}

# ------------------------------------------------------------------------------
# Like build_prep, but doesn't download anything and doesn't create a folder
#
# Parameters
# $1 name of module
# ------------------------------------------------------------------------------

function install_prep {
  name=$1

  # Check if build is already done
  if [ ! -f "$FLAGFILES/$name.finished" ] ; then
    BUILD_PACKAGE_NAME=$name
    BUILD_OLDPATH=$PATH
    BUILD_OLDPWD=$(pwd)

    touch "$FLAGFILES/$name.started"

    return 0
  else
    return 1
  fi
}

# ------------------------------------------------------------------------------
# Finalize a module build
# - create name.finished
# - go back to base folder
# ------------------------------------------------------------------------------

function build_post {
  if [ ! -f "$FLAGFILES/$BUILD_PACKAGE_NAME.finished" ]; then
    cd "$BUILD_OLDPWD"
    touch "$FLAGFILES/$BUILD_PACKAGE_NAME.finished"
    PATH=$BUILD_OLDPATH
  fi
}

###################### PATCH HERE DOCUMENTS #####################

PATCH_MERLIN_EMACS=$(cat <<'ENDHEREDOC'
--- orig/emacs/site-lisp/merlin.el	2019-04-22 19:46:25.000000000 +0200
+++ mingw/emacs/site-lisp/merlin.el	2019-04-25 16:16:47.000000000 +0200
@@ -1651,7 +1651,7 @@
     (when (equal command 'opam)
       (with-temp-buffer
         (if (eq (call-process-shell-command
-                 "opam config var bin" nil (current-buffer) nil) 0)
+                 "cygpath $(opam config var bin)" nil (current-buffer) nil) 0)
             (progn
               (setq command (concat
                              (replace-regexp-in-string "\n$" "" (buffer-string))
ENDHEREDOC
)

PATCH_USER_SETUP_EMACS=$(cat <<'ENDHEREDOC'
--- orig/.emacs.d/opam-user-setup.el	2019-04-25 19:26:32.000000000 +0200
+++ mingw/.emacs.d/opam-user-setup.el	2019-04-25 19:25:41.000000000 +0200
@@ -34,10 +34,11 @@
         (when (string= (car var) "PATH")
           (setq exec-path (split-string (cadr var) path-separator)))))))

-(opam-update-env nil)
+;; Does not work on MinGW opam cause of MinGW vs cygwin path issues
+;; (opam-update-env nil)

 (defvar opam-share
-  (let ((reply (opam-shell-command-to-string "opam config var share --safe")))
+  (let ((reply (opam-shell-command-to-string "cygpath $(opam config var share --safe)")))
     (when reply (substring reply 0 -1))))

 (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
ENDHEREDOC
)

###################### OPAM INITIALIZATION & PACKAGES #####################

function opam_init {
  # See https://fdopen.github.io/opam-repository-mingw/installation/
  if build_prep https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.2 opam64 tar.xz 1 opam64-win-0.0.0.2 ; then
    cd ~
    rm -rf ~/.opam
    bash -x opam64-win-0.0.0.2/install.sh  # --prefix /usr/foo, the default prefix is /usr/local
    # Note: --yes only answers yes/no questions, not the y/n/f question for updating the profile
    # yes | opam init default "https://github.com/fdopen/opam-repository-mingw.git#opam2" -c "ocaml-variants.4.07.1+mingw64c" --disable-sandboxing
    # opam init [OPTION]... [NAME] [ADDRESS]
    opam init --auto-setup --compiler "ocaml-variants.4.07.1+mingw64" --disable-sandboxing --kind=local default "C:/bin/cygwin_git/home/soegtrop/opam-repository-mingw/"
    eval $(opam config env)
    build_post
  fi
}

function opam_install_depext {
  if install_prep depext ; then
    # See http://fdopen.github.io/opam-repository-mingw/depext-cygwin/
    # Note: the path is already setup in .bash_profile!
    opam install depext --yes
    opam install depext-cygwinports --yes
    eval $(opam config env)
    build_post
  fi
}

function opam_install_ocaml_edit {
  if install_prep ocaml_edit ; then
    opam install merlin --yes
    opam install tuareg --yes
    opam install ocp-indent --yes
    opam install user-setup --yes
    # Note: emacs is not automatically detected
    opam user-setup install --editors=emacs
    # Note: emacs cannot handle the C: prefixed paths produced by user-setup
    # patch merlin setup file
    echo "$PATCH_MERLIN_EMACS" | patch -d "$(cygpath $(opam conf var share))" -p1
    # Patch user-setup generated emcas file
    # The stripping of \r is required for patching
    # The paths must by cygwin paths
    # Replace \ with / except for lines containig \n or \\
    # TODO: opam-user-setup.el: ocp-indent replace \ by / does not work cause of double slash
    # Maybe replace only backslashes sourounded by usual filename chars
    sed -i -e 's/\r//g' -e 's|[C-F]:/.*/home/|/home/|' -e '/\\[\\n]/!s|\\|/|g' .emacs.d/opam-user-setup.el
    echo "$PATCH_USER_SETUP_EMACS" | patch -d "$HOME" -p1
    eval $(opam config env)
    build_post
  fi
}

function opam_install_opam_coq_deps {
  if install_prep opam_install_opam_coq_deps ; then
    opam install num --yes
    opam install cairo2 --yes
    opam install lablgtk3 --yes
    opam install lablgtk3-sourceview3 --yes
    eval $(opam config env)
    build_post
  fi
}

# This is possibly required for the language server for VSCode
function install_nodejs {
  if build_prep https://nodejs.org/dist/v10.15.3 node-v10.15.3-win-x64 zip 1 nodejs-v10.15.3 ; then
    build_post
  fi
}

###################### Emacs #####################

# Native Win32 emacs (instead of cygwin bundled emacs)

function emacs_mingw_install {
  if build_prep http://mirror.easyname.at/gnu/emacs/windows/emacs-26 emacs-26.2-x86_64 zip 1 ; then
# if build_prep http://mirror.easyname.at/gnu/emacs/windows/emacs-26 emacs-26.2-x86_64-no-deps zip 1 ; then
    build_post
  fi
}

# Teach cygwin emacs to understand Windows style paths

function emacs_install_winpath {
  # See https://www.emacswiki.org/emacs/windows-path.el
  if build_prep https://www.emacswiki.org/emacs/download windows-path el ; then
    # Get required files
    get_expand_source_tar https://www.emacswiki.org/emacs/download cygwin-mount el

    # Copy files to site-lisp folder
    cp windows-path.el /usr/share/emacs/site-lisp/
    cp cygwin-mount/cygwin-mount.el /usr/share/emacs/site-lisp/

    echo ";; Add support for windows style paths in cygwin emacs" >> ~/.emacs
    echo "(require 'windows-path)" >> ~/.emacs
    echo "(windows-path-activate)" >> ~/.emacs

    build_post
  fi
}

# Compile emcas native
# eval $(opam env --revert)
# soegtrop@DESKTOP-A1A5UC0 /usr/src/emacs-26.2-1.src
# $ cygport emacs.cygport all
# $ cygport emacs.cygport install

###################### GIT INITIALIZATION #####################

function git_init {
  if install_prep git_init ; then
    # If a proxy is required, set a socks 5 proxy for github and gitlab.inria.fr
    if [ -n "${https_proxy-unset}" ]; then
      mkdir -p ~/.ssh
      chmod 700 ~/.ssh
      # TODO: OPTIONALLY havs a socks_proxy variable
      # Not sure how common it is to have the socks 5 proxy on port 1080.
      socks_proxy=$(echo ${https_proxy} | sed -E -e 's|http://||' -e 's|:[1-9]+/?|:1080|')
      cat >> ~/.ssh/config <<ENDHEREDOC

Host github.com
  Hostname      github.com
  IdentityFile  ~/.ssh/github
  ProxyCommand  nc -X 5 -x ${socks_proxy} %h %p

Host gitlab.inria.fr
  Hostname      gitlab.inria.fr
  ProxyCommand  nc -X 5 -x ${socks_proxy} %h %p
ENDHEREDOC
      chmod 600 ~/.ssh/config
    fi
    build_post
  fi
}

##### COQ #####

# Get coq via git
function coq_git_get {
  if install_prep coq_git_get ; then
    # ToDo: make this https or git depending on if a ssh identity is given
    git clone https://github.com/coq/coq.git

    # We need filemode=true in cygwin git and filemode=false in mingw git (used by vscode)
    # - remove filemode=true setting in the local git config
    # - set filemode=true in the global (per user) cygwin git config
    # - MANUALLY set filemode=false in the global (per user) mingw git config
    cd coq
    git config --local --unset core.filemode
    git config --global        core.filemode true
    build_post
  fi
}

# Build coq via dune
function coq_build_dune {
  if install_prep coq_build_dune ; then
    cd ~/coq
    #Mimic with the -xxdir options what opam install with --prefix actually does
    #ocaml.exe configure.ml -no-ask -native-compiler no -prefix "./" -libdir "./lib/coq" -datadir "./share/coq"
    #make -f Makefile.dune voboot $MAKE_OPT
    sed -i 's|coqide_X11\.ml\.in|coqide_WIN32\.ml\.in|' ide/dune
    dune build   $MAKE_OPT
    rm -rf "$PREFIXCOQ"
    mkdir -p "$PREFIXCOQ"
    dune install $MAKE_OPT --prefix="$PREFIXCOQ"
    chmod -R 555 "$PREFIXCOQ"
    #TODO copy stub
    build_post
  fi
}

function coq_install_gtk {
  if install_prep coq_install_gtk ; then
    # create and copy gschemas.compiled
    glib-compile-schemas /usr/x86_64-w64-mingw32/sys-root/mingw/share/glib-2.0/schemas/
    mkdir -p "$PREFIXCOQ/share/glib-2.0/schemas/"
    cp /usr/x86_64-w64-mingw32/sys-root/mingw/share/glib-2.0/schemas/gschemas.compiled "$PREFIXCOQ/share/glib-2.0/schemas/"

    # Copy icons
    ICONS=$(cat <<-END
        16x16/actions/document-new.png
        16x16/actions/pan-up-symbolic.symbolic.png
        16x16/actions/window-close-symbolic.symbolic.png
        16x16/actions/window-close.png
        16x16/actions/window-maximize-symbolic.symbolic.png
        16x16/actions/window-minimize-symbolic.symbolic.png
        24x24/actions/document-save.png
        24x24/actions/go-bottom.png
        24x24/actions/go-down.png
        24x24/actions/go-jump.png
        24x24/actions/go-next.png
        24x24/actions/go-previous.png
        24x24/actions/go-top.png
        24x24/actions/go-up.png
        24x24/actions/process-stop.png
        24x24/actions/system-run.png
        24x24/actions/window-close.png
        24x24/status/dialog-information.png
END
)
    mkdir -p "$PREFIXCOQ/shareicons/Adwaita/"
    for icon in $ICONS
    do
      mkdir -p $(dirname "$PREFIXCOQ/share/icons/Adwaita/$icon")
      cp "/usr/share/icons/Adwaita/$icon" "$PREFIXCOQ/share/icons/Adwaita/$icon"
    done
    build_post
  fi
}

###################### TOP LEVEL BUILD #####################

git_init
# emacs_mingw_install
# emacs_install_winpath
opam_init
opam_install_depext
opam_install_ocaml_edit
opam_install_opam_coq_deps
coq_git_get
coq_build_dune
coq_install_gtk