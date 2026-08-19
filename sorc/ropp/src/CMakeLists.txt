################################################################################
# ropp-ufo library
################################################################################

find_program( M4_EXECUTABLE m4 REQUIRED )
set( M4_OPTS "-U len" )

# ------------------------------------------------------------------------------
# ropp_utils/arrays and ropp_utils/unitconvert ship as m4 templates upstream and
# are expanded into plain Fortran at build time here, exactly as ROPP's own
# autotools build does via its ".m4.f90:  $(M4) $(M4_OPTS) $< > $@" rule.
# ------------------------------------------------------------------------------
set( ARRAYS_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ropp_utils/arrays )
set( UNITCONVERT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ropp_utils/unitconvert )

function( ropp_m4_generate out_f90 work_dir in_m4 )
  # ARGN: additional .m4 files this generated file depends on
  add_custom_command(
    OUTPUT ${work_dir}/${out_f90}
    COMMAND bash -c "${M4_EXECUTABLE} ${M4_OPTS} ${in_m4} > ${out_f90}"
    WORKING_DIRECTORY ${work_dir}
    DEPENDS ${work_dir}/${in_m4} ${ARGN}
    COMMENT "m4: generating ${out_f90} from ${in_m4}"
    VERBATIM
  )
endfunction()

ropp_m4_generate( arrays.f90       ${ARRAYS_DIR} arrays.m4
                   ${ARRAYS_DIR}/auxiliary.m4 ${ARRAYS_DIR}/copy_alloc_itfc.m4
                   ${ARRAYS_DIR}/reallocate_itfc.m4 ${ARRAYS_DIR}/preallocate_itfc.m4
                   ${ARRAYS_DIR}/reverse_itfc.m4 )
ropp_m4_generate( copy_alloc.f90   ${ARRAYS_DIR} copy_alloc.m4
                   ${ARRAYS_DIR}/auxiliary.m4 ${ARRAYS_DIR}/copy_alloc_code.m4 )
ropp_m4_generate( callocate.f90    ${ARRAYS_DIR} callocate.m4
                   ${ARRAYS_DIR}/auxiliary.m4 ${ARRAYS_DIR}/callocate_code.m4 )
ropp_m4_generate( isinrange.f90    ${ARRAYS_DIR} isinrange.m4
                   ${ARRAYS_DIR}/auxiliary.m4 ${ARRAYS_DIR}/isinrange_sitfc.m4
                   ${ARRAYS_DIR}/isinrange_scode.m4 ${ARRAYS_DIR}/isinrange_aitfc.m4
                   ${ARRAYS_DIR}/isinrange_acode.m4 )
ropp_m4_generate( reallocate.f90   ${ARRAYS_DIR} reallocate.m4
                   ${ARRAYS_DIR}/auxiliary.m4 ${ARRAYS_DIR}/reallocate_code.m4 )
ropp_m4_generate( preallocate.f90  ${ARRAYS_DIR} preallocate.m4
                   ${ARRAYS_DIR}/auxiliary.m4 ${ARRAYS_DIR}/preallocate_code.m4 )
ropp_m4_generate( reverse.f90      ${ARRAYS_DIR} reverse.m4
                   ${ARRAYS_DIR}/auxiliary.m4 ${ARRAYS_DIR}/reverse_code.m4 )

ropp_m4_generate( unitconvert.f90  ${UNITCONVERT_DIR} unitconvert.m4
                   ${UNITCONVERT_DIR}/unitconvert_aux.m4 ${UNITCONVERT_DIR}/ut_convert_sitfc.m4
                   ${UNITCONVERT_DIR}/ut_convert_aitfc.m4 )
ropp_m4_generate( ut_convert.f90   ${UNITCONVERT_DIR} ut_convert.m4
                   ${UNITCONVERT_DIR}/unitconvert_aux.m4 ${UNITCONVERT_DIR}/ut_convert_scode.m4
                   ${UNITCONVERT_DIR}/ut_convert_acode.m4 )

set( ropp_m4_generated_files
     ${ARRAYS_DIR}/arrays.f90
     ${ARRAYS_DIR}/copy_alloc.f90
     ${ARRAYS_DIR}/callocate.f90
     ${ARRAYS_DIR}/isinrange.f90
     ${ARRAYS_DIR}/reallocate.f90
     ${ARRAYS_DIR}/preallocate.f90
     ${ARRAYS_DIR}/reverse.f90
     ${UNITCONVERT_DIR}/unitconvert.f90
     ${UNITCONVERT_DIR}/ut_convert.f90
)
set_source_files_properties( ${ropp_m4_generated_files} PROPERTIES GENERATED TRUE )

# ------------------------------------------------------------------------------
# ropp_utils sources (checked-in, non-templated)
# ------------------------------------------------------------------------------
list( APPEND ropp_utils_src_files
  ropp_utils/arrays/blend.f90
  ropp_utils/arrays/cross_product.f90
  ropp_utils/arrays/getrun.f90
  ropp_utils/arrays/imaxloc.f90
  ropp_utils/arrays/iminloc.f90
  ropp_utils/arrays/l2norm.f90
  ropp_utils/arrays/locate.f90
  ropp_utils/arrays/nruns.f90
  ropp_utils/arrays/outer_and.f90
  ropp_utils/arrays/outer_product.f90
  ropp_utils/arrays/setminus.f90
  ropp_utils/arrays/sort.f90
  ropp_utils/arrays/swap.f90
  ropp_utils/arrays/uniq.f90
  ropp_utils/arrays/unit_vector.f90
  ropp_utils/arrays/where_indices.f90

  ropp_utils/common/FileDelete.f90
  ropp_utils/common/GetIOUnit.f90
  ropp_utils/common/ToLower.f90
  ropp_utils/common/ToUpper.f90
  ropp_utils/common/ropp_utils.f90
  ropp_utils/common/ropp_utils_version.f90
  ropp_utils/common/system.f90
  ropp_utils/common/typeSizes.f90

  ropp_utils/coordinates/cart2geod.f90
  ropp_utils/coordinates/coordinates.f90
  ropp_utils/coordinates/curvature.f90
  ropp_utils/coordinates/earth.f90
  # ecf2eci/eci2ecf/eci2eci: ROPP ships both a SOFA-library-backed and a
  # self-contained "_nosofa" implementation of each (mutually exclusive --
  # see ropp_utils/build/Makefile.am's HAVE_SOFA conditional). We use the
  # _nosofa variants to avoid an extra external (SOFA) dependency.
  ropp_utils/coordinates/ecf2eci_nosofa.f90
  ropp_utils/coordinates/eci2ecf_nosofa.f90
  ropp_utils/coordinates/eci2eci_nosofa.f90
  ropp_utils/coordinates/geod2cart.f90
  ropp_utils/coordinates/impact_parameter.f90
  ropp_utils/coordinates/occ_point.f90
  ropp_utils/coordinates/plane_coordinates.f90
  ropp_utils/coordinates/tangent_point.f90
  ropp_utils/coordinates/vectors.f90

  ropp_utils/datetime/augmentutc.f90
  ropp_utils/datetime/caltojul.f90
  ropp_utils/datetime/convertdt.f90
  ropp_utils/datetime/date_and_time_utc.f90
  ropp_utils/datetime/datetimeoffset.f90
  ropp_utils/datetime/datetimeprogs.f90
  ropp_utils/datetime/datetimetypes.f90
  ropp_utils/datetime/gpstoutc.f90
  ropp_utils/datetime/leapseconds.f90
  ropp_utils/datetime/monthofyear.f90
  ropp_utils/datetime/timesince.f90

  ropp_utils/geodesy/geodesy.f90
  ropp_utils/geodesy/geometric2geopotential.f90
  ropp_utils/geodesy/geopotential2geometric.f90
  ropp_utils/geodesy/gravity.f90
  ropp_utils/geodesy/great_circle_distance.f90
  ropp_utils/geodesy/r_eff.f90

  ropp_utils/math/lngamma.f90
  ropp_utils/math/ropp_math.f90

  ropp_utils/messages/assert.f90
  # message.f90 vs message_bs_escape.f90 are mutually exclusive alternates
  # picked by ROPP's configure based on whether the Fortran compiler treats
  # backslash as a string escape (see ropp_utils/build/Makefile.am's
  # FC_BACKSLASH_ESCAPE conditional). ifort/ifx do not by default, so we use
  # the plain (non-escaped) variant.
  ropp_utils/messages/message.f90
  ropp_utils/messages/message_get_addinfo.f90
  ropp_utils/messages/message_get_program.f90
  ropp_utils/messages/message_get_routine.f90
  ropp_utils/messages/message_set_addinfo.f90
  ropp_utils/messages/message_set_program.f90
  ropp_utils/messages/message_set_routine.f90
  ropp_utils/messages/messages.f90

  ropp_utils/unitconvert/ropp_unit_conversion.f90
)

# ------------------------------------------------------------------------------
# ropp_io: only the derived-type definitions module (ROprof etc.) are needed,
# to satisfy explicit-interface USE statements in ropp_fm/iono/ropp_fm_iono.f90.
# The NetCDF-dependent read/write routines (module ropp_io) are intentionally
# NOT included -- nothing UFO calls requires them.
# ------------------------------------------------------------------------------
list( APPEND ropp_io_src_files
  ropp_io/ropp_io_types.f90
)

# ------------------------------------------------------------------------------
# ropp_fm sources
#
# Excluded from ropp_fm/common: ropp_fm_obs2roprof.f90, ropp_fm_copy.f90,
# ropp_fm_roprof2obs.f90, ropp_fm_roprof2state.f90, ropp_fm_set_units.f90,
# ropp_fm_state2roprof.f90 -- these USE the full ropp_io module (NetCDF I/O)
# and are not declared in ropp_fm.f90's public interface, nor called from
# the bangle_1d/bangle_2d/refrac_1d forward/TL/AD paths UFO exercises.
#
# Excluded from ropp_fm/iono: ropp_fm_iono_set_default.f90,
# ropp_fm_iono_unpack_bangle.f90 -- same reason (ropp_io-dependent); the
# bangle_1d/2d TL/AD/forward code only calls ropp_fm_iono_bangle[_tl|_ad].
# ------------------------------------------------------------------------------
list( APPEND ropp_fm_src_files
  ropp_fm/common/ropp_fm.f90
  ropp_fm/common/ropp_fm_compress.f90
  ropp_fm/common/ropp_fm_compress_2d.f90
  ropp_fm/common/ropp_fm_compress_2d_ad.f90
  ropp_fm/common/ropp_fm_compress_2d_tl.f90
  ropp_fm/common/ropp_fm_compress_ad.f90
  ropp_fm/common/ropp_fm_compress_single.f90
  ropp_fm/common/ropp_fm_compress_single_ad.f90
  ropp_fm/common/ropp_fm_compress_single_tl.f90
  ropp_fm/common/ropp_fm_compress_tl.f90
  ropp_fm/common/ropp_fm_constants.f90
  ropp_fm/common/ropp_fm_free.f90
  ropp_fm/common/ropp_fm_interpol.f90
  ropp_fm/common/ropp_fm_interpol_ad.f90
  ropp_fm/common/ropp_fm_interpol_log.f90
  ropp_fm/common/ropp_fm_interpol_log_ad.f90
  ropp_fm/common/ropp_fm_interpol_log_tl.f90
  ropp_fm/common/ropp_fm_interpol_tl.f90
  ropp_fm/common/ropp_fm_levels.f90
  ropp_fm/common/ropp_fm_obs2obs.f90
  ropp_fm/common/ropp_fm_spline_2dba.f90
  ropp_fm/common/ropp_fm_spline_2dba_ad.f90
  ropp_fm/common/ropp_fm_spline_2dba_tl.f90
  ropp_fm/common/ropp_fm_spline_ba.f90
  ropp_fm/common/ropp_fm_spline_ba_ad.f90
  ropp_fm/common/ropp_fm_spline_ba_tl.f90
  ropp_fm/common/ropp_fm_spline_init.f90
  ropp_fm/common/ropp_fm_spline_init_ad.f90
  ropp_fm/common/ropp_fm_spline_init_tl.f90
  ropp_fm/common/ropp_fm_spline_log.f90
  ropp_fm/common/ropp_fm_spline_log_ad.f90
  ropp_fm/common/ropp_fm_spline_log_tl.f90
  ropp_fm/common/ropp_fm_splint.f90
  ropp_fm/common/ropp_fm_splint_ad.f90
  ropp_fm/common/ropp_fm_splint_tl.f90
  ropp_fm/common/ropp_fm_state2state.f90
  ropp_fm/common/ropp_fm_types.f90
  ropp_fm/common/ropp_fm_version.f90

  ropp_fm/iono/ropp_fm_asinh.f90
  ropp_fm/iono/ropp_fm_dzorro_dlg.f90
  ropp_fm/iono/ropp_fm_iono.f90
  ropp_fm/iono/ropp_fm_iono_bangle.f90
  ropp_fm/iono/ropp_fm_iono_bangle_above_leo.f90
  ropp_fm/iono/ropp_fm_iono_bangle_above_leo_ad.f90
  ropp_fm/iono/ropp_fm_iono_bangle_above_leo_tl.f90
  ropp_fm/iono/ropp_fm_iono_bangle_ad.f90
  ropp_fm/iono/ropp_fm_iono_bangle_tl.f90
  ropp_fm/iono/ropp_fm_zorro.f90

  ropp_fm/math/matrix_types.f90

  ropp_fm/bangle_1d/ropp_fm_abel.f90
  ropp_fm/bangle_1d/ropp_fm_abel_ad.f90
  ropp_fm/bangle_1d/ropp_fm_abel_tl.f90
  # ropp_fm_bangle_1d_grad.f90 is intentionally excluded: it's a Jacobian
  # routine used only by ROPP's own 1DVar tooling (not called by UFO), and it
  # pulls in model_ecmwf/model_meto (which we don't otherwise need).
  ropp_fm/bangle_1d/ropp_fm_bangle_1d.f90
  ropp_fm/bangle_1d/ropp_fm_bangle_1d_ad.f90
  ropp_fm/bangle_1d/ropp_fm_bangle_1d_tl.f90

  ropp_fm/bangle_2d/ropp_fm_2d_plane.f90
  ropp_fm/bangle_2d/ropp_fm_alpha2drk.f90
  ropp_fm/bangle_2d/ropp_fm_alpha2drk_ad.f90
  ropp_fm/bangle_2d/ropp_fm_alpha2drk_ec.f90
  ropp_fm/bangle_2d/ropp_fm_alpha2drk_ec_ad.f90
  ropp_fm/bangle_2d/ropp_fm_alpha2drk_ec_tl.f90
  ropp_fm/bangle_2d/ropp_fm_alpha2drk_tl.f90
  ropp_fm/bangle_2d/ropp_fm_bangle_2d.f90
  ropp_fm/bangle_2d/ropp_fm_bangle_2d_ad.f90
  ropp_fm/bangle_2d/ropp_fm_bangle_2d_tl.f90
  ropp_fm/bangle_2d/ropp_fm_gpspderivs.f90
  ropp_fm/bangle_2d/ropp_fm_gpspderivs_ad.f90
  ropp_fm/bangle_2d/ropp_fm_gpspderivs_tl.f90

  # ropp_fm_refrac_1d_grad.f90 excluded for the same reason as
  # ropp_fm_bangle_1d_grad.f90 above.
  ropp_fm/refrac_1d/ropp_fm_refrac_1d.f90
  ropp_fm/refrac_1d/ropp_fm_refrac_1d_ad.f90
  ropp_fm/refrac_1d/ropp_fm_refrac_1d_new.f90
  ropp_fm/refrac_1d/ropp_fm_refrac_1d_new_ad.f90
  ropp_fm/refrac_1d/ropp_fm_refrac_1d_new_tl.f90
  ropp_fm/refrac_1d/ropp_fm_refrac_1d_tl.f90
  ropp_fm/refrac_1d/ropp_fm_tdry.f90
  ropp_fm/refrac_1d/ropp_fm_tdry_1d.f90
)

set( ropp_ufo_src_files
     ${ropp_utils_src_files}
     ${ropp_io_src_files}
     ${ropp_fm_src_files}
     ${ropp_m4_generated_files}
)

## Create library target -- name MUST be exactly "ropp-ufo": ufo/src/ufo/CMakeLists.txt
## does target_link_libraries(ufo PUBLIC ropp-ufo) when ropp-ufo_FOUND is true.
ecbuild_add_library( TARGET          ropp-ufo
                      SOURCES        ${ropp_ufo_src_files}
                      LINKER_LANGUAGE Fortran
                    )

# Fortran module output directory for build and install interfaces
set( MODULE_DIR module/${PROJECT_NAME}/${CMAKE_Fortran_COMPILER_ID}/${CMAKE_Fortran_COMPILER_VERSION} )
set_target_properties( ${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR} )
install( DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR}/ DESTINATION ${MODULE_DIR} )
target_include_directories( ropp-ufo INTERFACE
                             $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/${MODULE_DIR}>
                             $<INSTALL_INTERFACE:${MODULE_DIR}> )
