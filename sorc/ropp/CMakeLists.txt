################################################################################
# ropp-ufo
#
# CMake wrapper around the ROM SAF ROPP forward-model / utility source code,
# built as a JEDI/ecbuild "bundle" project. This is NOT the official
# jcsda-internal/ropp-ufo repository -- it is a locally-assembled subset of
# the upstream ROM SAF ROPP v12.0 distribution (ropp_utils + ropp_fm, plus
# the ropp_io derived-type module) containing only the pieces required by
# ufo/src/ufo/operators/gnssro/BndROPP1D, BndROPP2D and
# ufo/src/ufo/operators/groundgnss/{RefROPP1D,ZenithTotalDelayROPP}.
#
# Excluded on purpose: ropp_io's NetCDF read/write layer, ropp_pp, ropp_1dvar,
# ropp_apps, and any ropp_fm routine that pulls in the full ropp_io module
# (RO-profile <-> state conversion helpers) -- none of those are exercised by
# UFO's GNSSRO/ground-based GNSS operators, which build State1dFM/Obs1dBangle
# directly from geovals rather than through ropp_io.
################################################################################

cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

find_package( ecbuild REQUIRED )

project( ropp-ufo VERSION 12.0.0 LANGUAGES Fortran )

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_declare_project()

ecbuild_enable_fortran( REQUIRED )

add_subdirectory( src )

ecbuild_install_project( NAME ropp-ufo )

ecbuild_print_summary()
