# - Find the CUDD BDD package.
# This code defines the following variables:
#
#  CUDD_FOUND                 - TRUE if CUDD was found.
#  CUDD_INCLUDE_DIRS          - Full paths to all include dirs.
#  CUDD_LIBRARIES             - Full paths to all libraries.
#
# Usage:
#  find_package(CUDD)
#
# The location of CUDD can be specified using the environment variable
# or cmake parameter DOWNWARD_CUDD_ROOT.
#
# Note that the standard FIND_PACKAGE features are supported
# (QUIET, REQUIRED, etc.).

set(IMPORTED_CONFIGURATIONS "Debug" "Release")
set(HINT_PATHS ${cudd_DIR} $ENV{CUDD_DIR})


add_library(cudd::cudd IMPORTED SHARED)
set_target_properties(cudd::cudd PROPERTIES
        IMPORTED_CONFIGURATIONS "${IMPORTED_CONFIGURATIONS}"
)

find_path(CUDD_INCLUDE_DIRS
    NAMES cudd.h
    HINTS $ENV{DOWNWARD_CUDD_ROOT}
    PATH_SUFFIXES include
    NO_DEFAULT_PATH
)
target_include_directories(cudd::cudd INTERFACE ${CUDD_INCLUDE_DIRS})

find_library(CUDD_LIBRARY
    NAMES cudd
    HINTS $ENV{DOWNWARD_CUDD_ROOT}
    PATH_SUFFIXES lib
    NO_DEFAULT_PATH
)

set(CUDD_LIBRARIES ${CUDD_LIBRARY})

# Check if everything was found and set CUDD_FOUND.
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
    CUDD
    REQUIRED_VARS CUDD_INCLUDE_DIRS CUDD_LIBRARIES
)

mark_as_advanced(CUDD_INCLUDE_DIRS CUDD_LIBRARIES)
