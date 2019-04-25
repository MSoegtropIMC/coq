#!/bin/bash

###################### COPYRIGHT/COPYLEFT ######################

# (C) 2016..2018 Intel Deutschland GmbH
# Author: Michael Soegtrop
#
# Released to the public by Intel under the
# GNU Lesser General Public License Version 2.1 or later
# See https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
#
# With very valuable help on building GTK from
#   https://wiki.gnome.org/Projects/GTK+/Win32/MSVCCompilationOfGTKStack
#   http://www.gaia-gis.it/spatialite-3.0.0-BETA/mingw64_how_to.html

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
#
# - if mingw64-x86_64-pkg-config is installed BEFORE building libpng or pixman,
#   the .pc files are properly created in /usr/$TARGET_ARCH/sys-root/mingw/lib/pkgconfig
#
# - pango and some others uses pkg-config executable names without path, which doesn't work in cross compile mode
#   There are several possible solutions
#     1.) patch build files to get the prefix from pkg-config and use $prefix/bin/ as path
#         - doesn't work for pango because automake goes wild
#         - mingw tools are not able to handle cygwin path (they need absolute windows paths)
#     2.) export PATH=$PATH:/usr/$TARGET_ARCH/sys-root/mingw/bin
#         - a bit dangerous because this exposes much more than required
#         - mingw tools are not able to handle cygwin path (they need absolute windows paths)
#     3.) Install required tools via cygwin modules libglib2.0-devel and libgdk_pixbuf2.0-devel
#         - Possibly version compatibility issues
#         - Possibly mingw/cygwin compatibility issues, e.g. when building font or terminfo databases
#     4.) Build required tools for mingw and cygwin
#         - Possibly mingw/cygwin compatibility issues, e.g. when building font or terminfo databases
#
#   We use method 3 below
#   Method 2 can be tried by putting the cross tools in the path before the cygwin tools (in configure_profile.sh)
#
# - It is tricky to build 64 bit binaries with 32 bit cross tools and vice versa.
#   This is because the linker needs to load DLLs from C:\windows\system32, which contains
#   both 32 bit and 64 bit DLLs, and which one you get depends by some black magic on if the using
#   app is a 32 bit or 64 bit app. So better build 32 bit mingw with 32 bit cygwin and 64 with 64.
#   Alternatively the required 32 bit or 64 bit DLLs need to be copied with a 32 bit/64bit cp to some
#   folder without such black magic.
#
# - The file selection for the Coq Windows Installer is done with make install (unlike the original script)
#   Relocatble builds are first configured with prefix=./ then build and then
#   reconfigured with prefix=<installroot> before make install.


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
BUILDLOGS=~/buildlogs
FLAGFILES=~/flagfiles
TARBALLS=~/tarballs
FILELISTS=~/filelists

mkdir -p $BUILDLOGS
mkdir -p $FLAGFILES
mkdir -p $TARBALLS
mkdir -p $FILELISTS
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

###################### Copy Cygwin Setup Info #####################

# Copy Cygwin repo ini file and installed files db to tarballs folder.
# Both files together document the exact selection and version of cygwin packages.
# Do this as early as possible to avoid changes by other setups (the repo folder is shared).

# Escape URL to folder name
CYGWIN_REPO_FOLDER=${CYGWIN_REPOSITORY}/
CYGWIN_REPO_FOLDER=${CYGWIN_REPO_FOLDER//:/%3a}
CYGWIN_REPO_FOLDER=${CYGWIN_REPO_FOLDER//\//%2f}

# Copy files
cp "$CYGWIN_LOCAL_CACHE_WFMT/$CYGWIN_REPO_FOLDER/$CYGWINARCH/setup.ini" $TARBALLS
cp /etc/setup/installed.db $TARBALLS

###################### LOGGING #####################

# The current log target (first part of the log file name)
LOGTARGET=other

# For an explanation of ${COQREGTESTING:-N} search for ${parameter:-word} in
# http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html

if [ "${COQREGTESTING:-N}" == "Y" ] ; then
  # If COQREGTESTING, log to log files only
  # Log command output - take log target name from command name (like log1 make => log target is "<module>-make")
  log1() {
    { local -; set +x; } 2> /dev/null
    "$@" >"$BUILDLOGS/$LOGTARGET-$1_log.txt"  2>"$BUILDLOGS/$LOGTARGET-$1_err.txt"
  }

  # Log command output - take log target name from command name and first argument (like log2 make install => log target is "<module>-make-install")
  log2() {
    { local -; set +x; } 2> /dev/null
    "$@" >"$BUILDLOGS/$LOGTARGET-$1-$2_log.txt" 2>"$BUILDLOGS/$LOGTARGET-$1-$2_err.txt"
  }

  # Log command output - take log target name from command name and second argument (like log_1_3 ocaml setup.ml -configure => log target is "<module>-ocaml--configure")
  log_1_3() {
    { local -; set +x; } 2> /dev/null
    "$@" >"$BUILDLOGS/$LOGTARGET-$1-$3_log.txt" 2>"$BUILDLOGS/$LOGTARGET-$1-$3_err.txt"
  }

  # Log command output - log target name is first argument (like logn untar tar xvaf ... => log target is "<module>-untar")
  logn() {
    { local -; set +x; } 2> /dev/null
    LOGTARGETEX=$1
    shift
    "$@" >"$BUILDLOGS/$LOGTARGET-${LOGTARGETEX}_log.txt" 2>"$BUILDLOGS/$LOGTARGET-${LOGTARGETEX}_err.txt"
  }
else
  # If COQREGTESTING, log to log files and console
  # Log command output - take log target name from command name (like log1 make => log target is "<module>-make")
  log1() {
    { local -; set +x; } 2> /dev/null
    "$@" > >(tee "$BUILDLOGS/$LOGTARGET-$1_log.txt" | sed -e "s/^/$LOGTARGET-$1_log.txt: /") 2> >(tee "$BUILDLOGS/$LOGTARGET-$1_err.txt" | sed -e "s/^/$LOGTARGET-$1_err.txt: /" 1>&2)
  }

  # Log command output - take log target name from command name and first argument (like log2 make install => log target is "<module>-make-install")
  log2() {
    { local -; set +x; } 2> /dev/null
    "$@" > >(tee "$BUILDLOGS/$LOGTARGET-$1-$2_log.txt" | sed -e "s/^/$LOGTARGET-$1-$2_log.txt: /") 2> >(tee "$BUILDLOGS/$LOGTARGET-$1-$2_err.txt" | sed -e "s/^/$LOGTARGET-$1-$2_err.txt: /" 1>&2)
  }

  # Log command output - take log target name from command name and second argument (like log_1_3 ocaml setup.ml -configure => log target is "<module>-ocaml--configure")
  log_1_3() {
    { local -; set +x; } 2> /dev/null
    "$@" > >(tee "$BUILDLOGS/$LOGTARGET-$1-$3_log.txt" | sed -e "s/^/$LOGTARGET-$1-$3_log.txt: /") 2> >(tee "$BUILDLOGS/$LOGTARGET-$1-$3_err.txt" | sed -e "s/^/$LOGTARGET-$1-$3_err.txt: /" 1>&2)
  }

  # Log command output - log target name is first argument (like logn untar tar xvaf ... => log target is "<module>-untar")
  logn() {
    { local -; set +x; } 2> /dev/null
    LOGTARGETEX=$1
    shift
    "$@" > >(tee "$BUILDLOGS/$LOGTARGET-${LOGTARGETEX}_log.txt" | sed -e "s/^/$LOGTARGET-${LOGTARGETEX}_log.txt: /") 2> >(tee "$BUILDLOGS/$LOGTARGET-${LOGTARGETEX}_err.txt" | sed -e "s/^/$LOGTARGET-${LOGTARGETEX}_err.txt: /" 1>&2)
  }
fi

###################### 'UNFIX' SED #####################

# In Cygwin SED used to do CR-LF to LF conversion, but since sed 4.4-1 this was changed
# We replace sed with a shell script which restores the old behavior for piped input

#if [ -f /bin/sed.exe ]
#then
#  mv /bin/sed.exe /bin/sed_orig.exe
#fi
#cat  > /bin/sed << EOF
##!/bin/sh
#dos2unix | /bin/sed_orig.exe "$@"
#EOF
#chmod a+x /bin/sed

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

  # Set logging target
  logtargetold=$LOGTARGET
  LOGTARGET=$name

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
  if [ "$3" == "zip" ] ; then
    log1 unzip "$TARBALLS/$name.$3"
    if [ "$strip" == "1" ] ; then
      # move subfolders of root folders one level up
      find "$(ls)" -mindepth 1 -maxdepth 1 -exec mv -- "{}" . \;
    else
      echo "Unzip strip count not supported"
      return 1
    fi
  else
    logn untar tar xvaf "$TARBALLS/$name.$3" --strip $strip
  fi

  # Patch if patch file exists
  # First try specific patch file name then generic patch file name
  if [ -f "$PATCHES/$name.patch" ] ; then
    log1 patch -p1 -i "$PATCHES/$name.patch"
  elif  [ -f "$PATCHES/$basename.patch" ] ; then
    log1 patch -p1 -i "$PATCHES/$basename.patch"
  fi

  # Go back to base folder
  cd ..

  LOGTARGET=$logtargetold
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

  # Set installer section to not set by default
  installersection=

  # Check if build is already done
  if [ ! -f "$FLAGFILES/$name.finished" ] ; then
    BUILD_PACKAGE_NAME=$name
    BUILD_OLDPATH=$PATH
    BUILD_OLDPWD=$(pwd)
    LOGTARGET=$name

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
# Like build_prep, but gets the data from an entry in ci-basic-overlay.sh
# This assumes the following definitions exist in ci-basic-overlay.sh,
# or in a file in the user-overlays folder:
# $1_CI_REF
# $1_CI_ARCHIVEURL
# $1_CI_GITURL
# ATTENTION: variables in ci-basic-overlay.sh are loaded by load_overlay_data.
# load_overlay_data is is called at the end of make_coq (even if the build is skipped)
#
# Parameters
# $1 base name of module in ci-basic-overlay.sh, e.g. mathcomp, bignums, ...
# ------------------------------------------------------------------------------

function build_prep_overlay {
  urlvar=$1_CI_ARCHIVEURL
  gitvar=$1_CI_GITURL
  refvar=$1_CI_REF
  url=${!urlvar}
  git=${!gitvar}
  ref=${!refvar}
  ver=$(git ls-remote "$git" "refs/heads/$ref" | cut -f 1)
  if [[ "$ver" == "" ]]; then
      # $1_CI_REF must have been a tag or hash, not a branch
      ver="$ref"
  fi
  build_prep "$url" "$ver" tar.gz 1 "$1-$ver" "$1"
}

# ------------------------------------------------------------------------------
# Like build_prep, but doesn't download anything
#
# Parameters
# $1 name of module
# ------------------------------------------------------------------------------

function install_prep {
  name=$1

  # Set installer section to not set by default
  installersection=

  # Check if build is already done
  if [ ! -f "$FLAGFILES/$name.finished" ] ; then
    BUILD_PACKAGE_NAME=$name
    BUILD_OLDPATH=$PATH
    BUILD_OLDPWD=$(pwd)
    LOGTARGET=$name

    touch "$FLAGFILES/$name.started"

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
# Load overlay version variables from ci-basic-overlay.sh and user-overlays/*.sh
# ------------------------------------------------------------------------------

function load_overlay_data {
  if [ -n "${GITLAB_CI-}" ]; then
    export CI_BRANCH="$CI_COMMIT_REF_NAME"
    if [[ ${CI_BRANCH#pr-} =~ ^[0-9]*$ ]]; then
      export CI_PULL_REQUEST="${CI_BRANCH#pr-}"
    else
      export CI_PULL_REQUEST=""
    fi
  else
    export CI_BRANCH=""
    export CI_PULL_REQUEST=""
  fi

  for overlay in /build/user-overlays/*.sh; do
    . "$overlay"
  done
  . /build/ci-basic-overlay.sh
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
    LOGTARGET=other
    installer_addon_end
  fi
}

# ------------------------------------------------------------------------------
# Build and install a module using the standard configure/make/make install process
# - prepare build (as above)
# - configure
# - make
# - make install
# - finalize build (as above)
#
# parameters
# $1 file server name including protocol prefix
# $2 file name (without extension)
# $3 file extension
# $4 patch function to call between untar and configure (or true if none)
# $5.. extra configure arguments
# ------------------------------------------------------------------------------

function build_conf_make_inst {
  if build_prep "$1" "$2" "$3" ; then
    $4
    logn configure ./configure --build="$BUILD" --host="$HOST" --target="$TARGET" --prefix="$PREFIX" "${@:5}"
    # shellcheck disable=SC2086
    log1 make $MAKE_OPT
    log2 make install
    log2 make clean
    build_post
  fi
}

# ------------------------------------------------------------------------------
# Install all files given by a glob pattern to a given folder
#
# parameters
# $1 source path
# $2 pattern (in '')
# $3 target folder
# ------------------------------------------------------------------------------

function install_glob {
  SRCDIR=$(realpath -m $1)
  DESTDIR=$(realpath -m $3)
  ( cd "$SRCDIR" && find . -maxdepth 1 -type f -name "$2" -exec install -D -T  "$SRCDIR"/{} "$DESTDIR"/{} \; )
}

# ------------------------------------------------------------------------------
# Recursively Install all files given by a glob pattern to a given folder
#
# parameters
# $1 source path
# $2 pattern (in '')
# $3 target folder
# ------------------------------------------------------------------------------

function install_rec {
  SRCDIR=$(realpath -m $1)
  DESTDIR=$(realpath -m $3)
  ( cd "$SRCDIR" && find . -type f -name "$2" -exec install -D -T  "$SRCDIR"/{} "$DESTDIR"/{} \; )
}

# ------------------------------------------------------------------------------
# Write a file list of the target folder
# The file lists are used to create file lists for the windows installer
# Don't overwrite an existing file list
#
# parameters
# $1 name of file list
# ------------------------------------------------------------------------------

function list_files {
  if [ ! -e "$FILELISTS/$1" ] ; then
    ( cd "$PREFIXCOQ" && find . -type f | sort > $FILELISTS/"$1" )
  fi
}

# ------------------------------------------------------------------------------
# Write a file list of the target folder
# The file lists are used to create file lists for the windows installer
# Do overwrite an existing file list
#
# parameters
# $1 name of file list
# ------------------------------------------------------------------------------

function list_files_always {
  ( cd "$PREFIXCOQ" && find . -type f | sort > $FILELISTS/"$1" )
}

# ------------------------------------------------------------------------------
# Compute the set difference of two file lists
#
# parameters
# $1 name of list A-B (set difference of set A minus set B)
# $2 name of list A
# $3 name of list B
# ------------------------------------------------------------------------------

function diff_files {
  # See http://www.catonmat.net/blog/set-operations-in-unix-shell/ for file list set operations
  comm -23 <(sort "$FILELISTS/$2") <(sort "$FILELISTS/$3") > "$FILELISTS/$1"
}

# ------------------------------------------------------------------------------
# Filter a list of files with a regular expression
#
# parameters
# $1 name of output file list
# $2 name of input file list
# $3 name of filter regexp
# ------------------------------------------------------------------------------

function filter_files {
  grep -E "$3" "$FILELISTS/$2" > "$FILELISTS/$1"
}

# ------------------------------------------------------------------------------
# Convert a file list to NSIS installer format
#
# parameters
# $1 name of file list file (output file is the same with extension .nsi)
# ------------------------------------------------------------------------------

function files_to_nsis {
  # Split the path in the file list into path and filename and create SetOutPath and File instructions
  # Note: File /oname cannot be used, because it does not create the paths as SetOutPath does
  # Note: I didn't check if the redundant SetOutPath instructions have a bad impact on installer size or install time
  tr '/' '\\' < "$FILELISTS/$1" | sed -r 's/^\.(.*)\\([^\\]+)$/SetOutPath $INSTDIR\\\1\nFile ${COQ_SRC_PATH}\\\1\\\2/' > "$FILELISTS/$1.nsh"
}

# ------------------------------------------------------------------------------
# Create an nsis installer addon section
#
# parameters
# $1 identifier of installer section and base name of file list files
# $2 human readable name of section
# $3 description of section
# $4 flags (space separated list of keywords): off = default off
#
# $1 must be a valid NSIS identifier!
# ------------------------------------------------------------------------------

function installer_addon_section {
  installersection=$1
  list_files "addon_pre_$installersection"

  echo 'LangString' "DESC_$1" '${LANG_ENGLISH}' "\"$3\"" >> "$FILELISTS/addon_strings.nsh"

  echo '!insertmacro MUI_DESCRIPTION_TEXT' '${'"Sec_$1"'}' '$('"DESC_$1"')' >> "$FILELISTS/addon_descriptions.nsh"

  local sectionoptions=
  if [[ "$4" == *off* ]] ; then sectionoptions+=" /o" ; fi

  echo "Section $sectionoptions \"$2\" Sec_$1" >> "$FILELISTS/addon_sections.nsh"
  echo 'SetOutPath "$INSTDIR\"' >> "$FILELISTS/addon_sections.nsh"
  echo '!include "..\..\..\filelists\addon_'"$1"'.nsh"' >> "$FILELISTS/addon_sections.nsh"
  echo 'SectionEnd' >> "$FILELISTS/addon_sections.nsh"
}

# ------------------------------------------------------------------------------
# Start an installer addon dependency group
#
# parameters
# $1 identifier of the section which depends on other sections
# The parameters must match the $1 parameter of a installer_addon_section call
# ------------------------------------------------------------------------------

dependencysections=

function installer_addon_dependency_beg {
  installer_addon_dependency "$1"
  dependencysections="$1 $dependencysections"
}

# ------------------------------------------------------------------------------
# End an installer addon dependency group
# ------------------------------------------------------------------------------

function installer_addon_dependency_end {
  set -- $dependencysections
  shift
  dependencysections="$*"
}

# ------------------------------------------------------------------------------
# Create an nsis installer addon dependency entry
# This needs to be bracketed with installer_addon_dependencies_beg/end
#
# parameters
# $1 identifier of the section on which other sections might depend
# The parameters must match the $1 parameter of a installer_addon_section call
# ------------------------------------------------------------------------------

function installer_addon_dependency {
  for section in $dependencysections ; do
    echo '${CheckSectionDependency} ${Sec_'"$section"'} ${Sec_'"$1"'} '"'$section' '$1'" >> "$FILELISTS/addon_dependencies.nsh"
  done
}

# ------------------------------------------------------------------------------
# Finish an installer section after an addon build
#
# This creates the file list files
#
# parameters: none
# ------------------------------------------------------------------------------

function installer_addon_end {
  if [ -n "$installersection" ]; then
    list_files "addon_post_$installersection"
    diff_files "addon_$installersection" "addon_post_$installersection" "addon_pre_$installersection"
    files_to_nsis "addon_$installersection"
  fi
}

# ------------------------------------------------------------------------------
# Set all timeouts in all .v files to 1000
# Since timeouts can lead to CI failures, this is useful
#
# parameters: none
# ------------------------------------------------------------------------------

function coq_set_timeouts_1000 {
  find . -type f -name '*.v' -print0 | xargs -0 sed -i 's/timeout\s\+[0-9]\+/timeout 1000/g'
}

###################### PATCH HERE DOCUMENTS #####################

PATCH_MERLIN_EMACS=$(cat <<'ENDHEREDOC'
--- orig/emacs/site-lisp/merlin.el	2019-04-22 19:46:25.000000000 +0200
+++ mingw/emacs/site-lisp/merlin.el	2019-04-25 16:16:47.000000000 +0200
@@ -1648,13 +1648,13 @@
     (unless command
       (setq command (if (functionp merlin-command) (funcall merlin-command)
                       merlin-command)))
     (when (equal command 'opam)
       (with-temp-buffer
         (if (eq (call-process-shell-command
-                 "opam config var bin" nil (current-buffer) nil) 0)
+                 "cygpath $(opam config var bin)" nil (current-buffer) nil) 0)
             (progn
               (setq command (concat
                              (replace-regexp-in-string "\n$" "" (buffer-string))
                              "/ocamlmerlin"))
               (push (cons 'command command) merlin-buffer-configuration))
           (message "merlin-command: opam config failed (%S)" (buffer-string))
ENDHEREDOC
)

PATCH_USER_SETUP_EMACS=$(cat <<'ENDHEREDOC'
--- orig/.emacs.d/opam-user-setup.el	2019-04-25 19:26:32.000000000 +0200
+++ mingw/.emacs.d/opam-user-setup.el	2019-04-25 19:25:41.000000000 +0200
@@ -31,16 +31,17 @@
     (when (and env (not (string= env "")))
       (dolist (var (car (read-from-string env)))
         (setenv (car var) (cadr var))
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
 ;; OPAM-installed tools automated detection and initialisation

 (defun opam-setup-tuareg ()
@@ -121,13 +122,13 @@
 (opam-auto-tools-setup)
 ;; ## end of OPAM user-setup addition for emacs / base ## keep this line
 ;; ## added by OPAM user-setup for emacs / tuareg ## 02b72b2e82ebc9796808c7a5b6e20636 ## you can edit, but keep this line
 ;; Set to autoload tuareg from its original switch when not found in current
 ;; switch (don't load tuareg-site-file as it adds unwanted load-paths)
 (defun opam-tuareg-autoload (fct file doc args)
-  (let ((load-path (cons "/home/soegtrop/.opam/ocaml-variants.4.07.1+mingw64c/share\emacs\site-lisp" load-path)))
+  (let ((load-path (cons "/home/soegtrop/.opam/ocaml-variants.4.07.1+mingw64c/share/emacs/site-lisp" load-path)))
     (load file))
   (apply fct args))
 (when (not (member "tuareg" opam-tools-installed))
   (defun tuareg-mode (&rest args)
     (opam-tuareg-autoload 'tuareg-mode "tuareg" "Major mode for editing OCaml code" args))
   (defun tuareg-run-ocaml (&rest args)
@@ -143,12 +144,12 @@
   (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmxs" ".cmt" ".cmti" ".cmi" ".annot"))
     (add-to-list 'completion-ignored-extensions ext)))
 ;; ## end of OPAM user-setup addition for emacs / tuareg ## keep this line
 ;; ## added by OPAM user-setup for emacs / ocp-indent ## 900ff7c37d13c56d2b3ed5a14508938c ## you can edit, but keep this line
 ;; Load ocp-indent from its original switch when not found in current switch
 (when (not (assoc "ocp-indent" opam-tools-installed))
-  (autoload 'ocp-setup-indent "/home/soegtrop/.opam/ocaml-variants.4.07.1+mingw64c/share\emacs\site-lisp\ocp-indent.el" "Improved indentation for Tuareg mode")
-  (autoload 'ocp-indent-caml-mode-setup "/home/soegtrop/.opam/ocaml-variants.4.07.1+mingw64c/share\emacs\site-lisp\ocp-indent.el" "Improved indentation for Caml mode")
+  (autoload 'ocp-setup-indent "/home/soegtrop/.opam/ocaml-variants.4.07.1+mingw64c/share/emacs/site-lisp/ocp-indent.el" "Improved indentation for Tuareg mode")
+  (autoload 'ocp-indent-caml-mode-setup "/home/soegtrop/.opam/ocaml-variants.4.07.1+mingw64c/share/emacs/site-lisp/ocp-indent.el" "Improved indentation for Caml mode")
   (add-hook 'tuareg-mode-hook 'ocp-setup-indent t)
   (add-hook 'caml-mode-hook 'ocp-indent-caml-mode-setup  t)
-  (setq ocp-indent-path "/home/soegtrop/.opam/ocaml-variants.4.07.1+mingw64c/bin\\ocp-indent"))
+  (setq ocp-indent-path "/home/soegtrop/.opam/ocaml-variants.4.07.1+mingw64c/bin/ocp-indent"))
 ;; ## end of OPAM user-setup addition for emacs / ocp-indent ## keep this line
ENDHEREDOC
)

###################### OPAM INITIALIZAATION #####################

function opam_init {
  # See https://fdopen.github.io/opam-repository-mingw/installation/
  if build_prep https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.2 opam64 tar.xz 1 opam64-win-0.0.0.2 ; then
    cd ~
    bash -x opam64-win-0.0.0.2/install.sh  # --prefix /usr/foo, the default prefix is /usr/local
    # Note: --yes only answers yes/no questions, not the y/n/f question for updating the profile
    yes | opam init default "https://github.com/fdopen/opam-repository-mingw.git#opam2" -c "ocaml-variants.4.07.1+mingw64c" --disable-sandboxing
    eval $(opam config env)
    build_post
  fi
}

function install_depext {
  if install_prep depext ; then
    # See http://fdopen.github.io/opam-repository-mingw/depext-cygwin/
    # Note: the path is already setup in .bash_profile!
    opam install depext --yes
    opam install depext-cygwinports --yes
    build_post
  fi
}

function install_ocaml_edit {
  if install_prep ocaml_edit ; then
    opam install merlin --yes
    opam install tuareg --yes
    opam install user-setup --yes
    # Note: emacs is not automatically detected
    opam user-setup install --editors=emacs
    # Note: emacs cannot handle the C: prefixed paths produced by user-setup
    # The stripping of \r is required for patching
    sed -i -e 's/\r//g' -e 's|[C-F]:/.*/home/|/home/|' .emacs.d/opam-user-setup.el
    # echo "$PATCH_MERLIN_EMACS" | patch -d "$(cygpath $(opam conf var share))" -p1
    echo "$PATCH_USER_SETUP_EMACS" | patch -d "$HOME" -p1
    build_post
  fi
}

# This is possibly required for the language server for VSCode
function install_nodejs {
  if build_prep https://nodejs.org/dist/v10.15.3 node-v10.15.3-win-x64 zip 1 nodejs-v10.15.3 ; then
    build_post
  fi
}

###################### MODULE BUILD FUNCTIONS #####################

##### ARCH-pkg-config replacement #####

# cygwin replaced ARCH-pkg-config with a shell script, which doesn't work e.g. for dune on Windows.
# This builds a binary replacement for the shell script and puts it into the bin_special folder.
# There is no global installation since it is module specific what pkg-config is needed under what name.

function make_arch_pkg_config {
  gcc -DARCH="$TARGET_ARCH" -o bin_special/pkg-config.exe $PATCHES/pkg-config.c
}


##### COQ #####

# Copy one DLLfrom cygwin MINGW packages to Coq install folder

function copy_coq_dll {
  if [ "$INSTALLMODE" == "absolute" ] || [ "$INSTALLMODE" == "relocatable" ]; then
    cp "/usr/${ARCH}-w64-mingw32/sys-root/mingw/bin/$1" "$PREFIXCOQ/bin/$1"
  fi
}

# Copy required DLLs from cygwin MINGW packages to Coq install folder

function copy_coq_dlls {
  # HOW TO CREATE THE DLL LIST
  # With the list empty, after the build/install is finished, open coqide in dependency walker.
  # See http://www.dependencywalker.com/
  # Make sure to use the 32 bit / 64 bit version of depends matching the target architecture.
  # Select all missing DLLs from the module list, right click "copy filenames"
  # Delay loaded DLLs from Windows can be ignored (hour-glass icon at begin of line)
  # Do this recursively until there are no further missing DLLs (File close + reopen)
  # For running this quickly, just do "cd coq-<ver> ; copy_coq_dlls ; cd .." at the end of this script.
  # Do the same for coqc and ocamlc (usually doesn't result in additional files)

  copy_coq_dll LIBCAIRO-2.DLL
  copy_coq_dll LIBFONTCONFIG-1.DLL
  copy_coq_dll LIBFREETYPE-6.DLL
  copy_coq_dll LIBGDK-3-0.DLL
  copy_coq_dll LIBGDK_PIXBUF-2.0-0.DLL
  copy_coq_dll LIBGLIB-2.0-0.DLL
  copy_coq_dll LIBGOBJECT-2.0-0.DLL
  copy_coq_dll LIBGTK-3-0.DLL
  copy_coq_dll LIBGTKSOURCEVIEW-3.0-1.DLL
  copy_coq_dll LIBPANGO-1.0-0.DLL
  copy_coq_dll LIBATK-1.0-0.DLL
  copy_coq_dll LIBBZ2-1.DLL
  copy_coq_dll LIBCAIRO-GOBJECT-2.DLL
  copy_coq_dll LIBEPOXY-0.DLL
  copy_coq_dll LIBEXPAT-1.DLL
  copy_coq_dll LIBFFI-6.DLL
  copy_coq_dll LIBGIO-2.0-0.DLL
  copy_coq_dll LIBGMODULE-2.0-0.DLL
  copy_coq_dll LIBINTL-8.DLL
  copy_coq_dll LIBPANGOCAIRO-1.0-0.DLL
  copy_coq_dll LIBPANGOWIN32-1.0-0.DLL
  copy_coq_dll LIBPCRE-1.DLL
  copy_coq_dll LIBPIXMAN-1-0.DLL
  copy_coq_dll LIBPNG16-16.DLL
  copy_coq_dll LIBXML2-2.DLL
  copy_coq_dll ZLIB1.DLL
  copy_coq_dll ICONV.DLL
  copy_coq_dll LIBLZMA-5.DLL
  copy_coq_dll LIBPANGOFT2-1.0-0.DLL
  copy_coq_dll LIBHARFBUZZ-0.DLL

  # Depends on if GTK is built from sources
  if [ "$GTK_FROM_SOURCES" == "Y" ]; then
    echo "Building GTK from sources is currently not supported"
    exit 1
  fi;

  # Architecture dependent files
  case $ARCH in
    x86_64) copy_coq_dll LIBGCC_S_SEH-1.DLL ;;
    i686)   copy_coq_dll LIBGCC_S_SJLJ-1.DLL ;;
    *)      false ;;
  esac

  # Win pthread version change
  copy_coq_dll LIBWINPTHREAD-1.DLL
}

function copy_coq_objects {
  # copy objects only from folders which exist in the target lib directory
  find . -type d | while read -r FOLDER ; do
    if [ -e "$PREFIXCOQ/lib/coq/$FOLDER" ] ; then
      install_glob "$FOLDER" '*.cmxa' "$PREFIXCOQ/lib/coq/$FOLDER"
      install_glob "$FOLDER" '*.cmi'  "$PREFIXCOQ/lib/coq/$FOLDER"
      install_glob "$FOLDER" '*.cma'  "$PREFIXCOQ/lib/coq/$FOLDER"
      install_glob "$FOLDER" '*.cmo'  "$PREFIXCOQ/lib/coq/$FOLDER"
      install_glob "$FOLDER" '*.a'    "$PREFIXCOQ/lib/coq/$FOLDER"
      install_glob "$FOLDER" '*.o'    "$PREFIXCOQ/lib/coq/$FOLDER"
    fi
  done
}

# Copy required GTK config and suport files

function copy_coq_gtk {
  echo 'gtk-theme-name = "Default"'     >  "$PREFIX/etc/gtk-3.0/gtkrc"
  echo 'gtk-fallback-icon-theme = "Tango"' >> "$PREFIX/etc/gtk-3.0/gtkrc"

  if [ "$INSTALLMODE" == "absolute" ] || [ "$INSTALLMODE" == "relocatable" ]; then
    install_glob "$PREFIX/etc/gtk-3.0" '*'                            "$PREFIXCOQ/gtk-3.0"
    install_glob "$PREFIX/share/gtksourceview-3.0/language-specs" '*' "$PREFIXCOQ/share/gtksourceview-3.0/language-specs"
    install_glob "$PREFIX/share/gtksourceview-3.0/styles" '*'         "$PREFIXCOQ/share/gtksourceview-3.0/styles"
    install_rec  "$PREFIX/share/themes" '*'                           "$PREFIXCOQ/share/themes"

    # This below item look like a bug in make install
    if [ -d "$PREFIXCOQ/share/coq/" ] ; then
      COQSHARE="$PREFIXCOQ/share/coq/"
    else
      COQSHARE="$PREFIXCOQ/share/"
    fi

    mkdir -p "$PREFIXCOQ/ide"
    mv "$COQSHARE"*.png  "$PREFIXCOQ/ide"
    rmdir "$PREFIXCOQ/share/coq" || true
  fi
}

# Copy license and other info files

function copy_coq_license {
  if [ "$INSTALLMODE" == "absolute" ] || [ "$INSTALLMODE" == "relocatable" ]; then
    install -D doc/LICENSE                    "$PREFIXCOQ/license_readme/coq/LicenseDoc.txt"
    install -D LICENSE                        "$PREFIXCOQ/license_readme/coq/License.txt"
    install -D plugins/micromega/LICENSE.sos  "$PREFIXCOQ/license_readme/coq/LicenseMicromega.txt"
    # FIXME: this is not the micromega license
    # It only applies to code that was copied into one single file!
    install -D README.md                      "$PREFIXCOQ/license_readme/coq/ReadMe.md"
    install -D CHANGES.md                     "$PREFIXCOQ/license_readme/coq/Changes.md"
    install -D INSTALL                        "$PREFIXCOQ/license_readme/coq/Install.txt"
    install -D doc/README.md                  "$PREFIXCOQ/license_readme/coq/ReadMeDoc.md" || true
  fi
}

# Main function for creating Coq

function make_coq {
  make_ocaml
  make_num
  make_findlib
  make_lablgtk
  make_dune
  if
    case $COQ_VERSION in
      # e.g. git-v8.6 => download from https://github.com/coq/coq/archive/v8.6.zip
      # e.g. git-trunk => download from https://github.com/coq/coq/archive/trunk.zip
      git-*)
        COQ_BUILD_PATH=/build/coq-${COQ_VERSION}
        build_prep https://github.com/coq/coq/archive "${COQ_VERSION##git-}" zip 1 "coq-${COQ_VERSION}"
        ;;

      # e.g. /cygdrive/d/coqgit
      /*)
        # Todo: --exclude-vcs-ignores doesn't work because tools/coqdoc/coqdoc.sty is excluded => fix .gitignore
        # But this is not a big deal, only 2 files are removed with --exclude-vcs-ignores from a fresch clone
        COQ_BUILD_PATH=/build/coq-local
        tar -zcf $TARBALLS/coq-local.tar.gz --exclude-vcs -C "${COQ_VERSION%/*}" "${COQ_VERSION##*/}"
        build_prep NEVER-DOWNLOADED coq-local tar.gz
        ;;

      # e.g. 8.6 => https://coq.inria.fr/distrib/8.6/files/coq-8.6.tar.gz
      *)
        COQ_BUILD_PATH=/build/coq-$COQ_VERSION
        build_prep "https://coq.inria.fr/distrib/V$COQ_VERSION/files" "coq-$COQ_VERSION" tar.gz
        ;;
    esac
  then
    ocaml.exe configure.ml -no-ask -native-compiler no -prefix "./"
    make -f Makefile.dune voboot $MAKE_OPT
    dune build   $MAKE_OPT
    dune install $MAKE_OPT --prefix="$PREFIXCOQ"
    log1 copy_coq_dlls
    log1 copy_coq_gtk
    fail
    if [ "$INSTALLMODE" == "relocatable" ]; then
      # HACK: for relocatable builds, first configure with ./, then build but before install reconfigure with the real target path
      logn configure ./configure -with-doc no -prefix ./ -libdir ./lib/coq -mandir ./man
    elif [ "$INSTALLMODE" == "absolute" ]; then
      logn configure ./configure -with-doc no -prefix "$PREFIXCOQ" -libdir "$PREFIXCOQ/lib/coq" -mandir "$PREFIXCOQ/man"
    else
      logn configure ./configure -with-doc no -prefix "$PREFIXCOQ"
    fi

    # The windows resource compiler binary name is hard coded
    sed -i "s/i686-w64-mingw32-windres/$TARGET_ARCH-windres/" Makefile.build
    sed -i "s/i686-w64-mingw32-windres/$TARGET_ARCH-windres/" Makefile.ide || true

    # 8.4x doesn't support parallel make
    if [[ $COQ_VERSION == 8.4* ]] ; then
      log1 make
    else
      # shellcheck disable=SC2086
      log1 make $MAKE_OPT
    fi

    if [ "$INSTALLMODE" == "relocatable" ]; then
      logn reconfigure ./configure -with-doc no -prefix "$PREFIXCOQ" -libdir "$PREFIXCOQ/lib/coq" -mandir "$PREFIXCOQ/man"
    fi

    log2 make install
    log1 copy_coq_dlls
    log1 copy_coq_gtk

    if [ "$INSTALLOCAML" == "Y" ]; then
      copy_coq_objects
    fi

    log1 copy_coq_license

    # make clean seems to be broken for 8.5pl2
    # 1.) find | xargs fails on cygwin, can be fixed by sed -i 's|\| xargs rm -f|-exec rm -fv \{\} \+|' Makefile
    # 2.) clean of test suites fails with "cannot run complexity tests (no bogomips found)"
    # make clean

    # Copy these files somewhere the plugin builds can find them
    logn copy-basic-overlays cp dev/ci/ci-basic-overlay.sh /build/
    logn copy-user-overlays cp -r dev/ci/user-overlays /build/

    build_post
  fi

  load_overlay_data
}

###################### TOP LEVEL BUILD #####################

ocamlfind list || true

opam_init
install_depext
install_ocaml_edit