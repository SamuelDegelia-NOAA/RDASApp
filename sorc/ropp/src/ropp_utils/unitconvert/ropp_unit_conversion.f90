! $Id: ropp_unit_conversion.f90 2213 2009-06-29 10:26:43Z frhl $
!
!****f* Initialisation/ropp_unit_conversion
!
! NAME
!   ropp_unit_conversion - unit conversion
!
! SYNOPSIS
!   CALL ropp_unit_conversion ( from_unit, to_unit, slope, intercept )
!
! INPUTS
!   from_unit      Original data unit
!   to_unit        Required output data unit
!
! OUTPUTS
!   slope          Scaling factor for unit conversion
!   intercept      Offset for unit conversion
!
! USES
!   typsesizes
!   To_Lower
!
! DESCRIPTION
!
! AUTHOR
!   Met Office, Exeter, UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

SUBROUTINE ropp_unit_conversion ( from_unit, to_unit, slope, intercept )

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

  USE typesizes,     ONLY: wp => EightByteReal
  
  IMPLICIT NONE

  CHARACTER(len = *), INTENT(in)  :: from_unit 
  CHARACTER(len = *), INTENT(in)  :: to_unit 
  REAL(wp),           INTENT(out) :: slope
  REAL(wp),           INTENT(out) :: intercept
  
  CHARACTER(len = LEN_TRIM(from_unit)) :: unit1
  CHARACTER(len = LEN_TRIM(to_unit))   :: unit2

  INTEGER                         :: i

!-------------------------------------------------------------------------------
! 2. Trim unit strings (avoid null characters)
!-------------------------------------------------------------------------------

  slope = 1.0_wp
  intercept = 0.0_wp

  unit1 = TRIM(from_unit)
  unit2 = TRIM(to_unit)
  IF (INDEX(from_unit,char(0)) > 0) THEN
    unit1(INDEX(from_unit,char(0)):INDEX(from_unit,char(0))) = char(32)
  ENDIF
  CALL To_Lower(unit1)
  CALL To_Lower(unit2)

! Further trim time units

  IF (INDEX(unit1, 'since') > 2) THEN
    DO i=INDEX(unit1, 'since'),LEN(unit1)
      unit1(i:i) = char(32)
    ENDDO
  ENDIF

  IF (INDEX(unit2, 'since') > 2) THEN
    DO i=INDEX(unit2, 'since'),LEN(unit2)
      unit2(i:i) = char(32)
    ENDDO
  ENDIF


  SELECT CASE (TRIM(unit1))

!-------------------------------------------------------------------------------
! 3. Lengths
!-------------------------------------------------------------------------------
 
    CASE('metre','meter','metres','meters','m')
      SELECT CASE (TRIM(unit2))
      CASE('kilometre','kilometer','kilometres','kilometers','km')
        slope = 0.001_wp
      CASE('centimetre','centimeter','centimetres','centimeters','cm')
        slope = 100.0_wp 
      CASE('millimetre','millimeter','millimetres','millimeters','mm')
        slope = 1000.0_wp 
      END SELECT
    CASE('kilometre','kilometer','kilometres','kilometers','km')
      SELECT CASE (TRIM(unit2))
      CASE('metre','meter','metres','meters','m')
        slope = 1000.0_wp
      CASE('centimetre','centimeter','centimetres','centimeters','cm')
        slope = 100000.0_wp 
      CASE('millimetre','millimeter','millimetres','millimeters','mm')
        slope = 1000000.0_wp 
      END SELECT
   
!-------------------------------------------------------------------------------
! 4. Pressure
!-------------------------------------------------------------------------------

      CASE('hpa','hectopascal','hectopascals','mb','millibar','millibars')
        SELECT CASE (TRIM(unit2))
        CASE('pa','pascal','pascals') 
          slope = 100.0_wp
        CASE('atm','atmosphere','atmospheres')
          slope = 0.000986923_wp
        END SELECT
      CASE('pa','pascal','pascals')
        SELECT CASE (TRIM(unit2))
        CASE('hpa','hectopascal','hectopascals','mb','millibar','millibars')
          slope = 0.01_wp
        CASE('atm','atmosphere','atmospheres')
          slope = 9.86923e-6_wp
        END SELECT
      CASE('atm','atmosphere','atmospheres')
        SELECT CASE (TRIM(unit2))
        CASE('pa','pascal','pascals') 
          slope = 101325.0_wp
        CASE('hpa','hectopascal','hectopascals','mb','millibar','millibars') 
          slope = 1013.25_wp 
        END SELECT

!-------------------------------------------------------------------------------
! 5. Temperature
!-------------------------------------------------------------------------------

      CASE('kelvin','k','deg k','deg kelvin','deg_k','deg_kelvin','degk',     &
           'degree k','degree kelvin','degree_k','degree_kelvin','degreek',   &
           'degrees k','degrees kelvin','degrees_k','degrees_kelvin',         &
           'degreesk')
        SELECT CASE (TRIM(unit2))
        CASE('celsius','deg c','deg_c','degc','deg celsius','deg_celsius',    &
           'degree c','degree celsius','degree_c','degreec','degree_celsius', &
           'degrees c','degrees celsius','degrees_c','degreesc',              &
           'degrees_celsius')
          slope = 1.0_wp
          intercept = -273.15_wp
        CASE('farenheit','deg f','deg_f','degf','deg farenheit',              &
             'deg_farenheit','degree f','degree farenheit','degree_f',        &
             'degreef','degree_farenheit','degrees f','degrees farenheit',    &
             'degrees_f','degreesf','degrees_farenheit') 
          slope = 1.8_wp
          intercept = -459.67_wp
        END SELECT
      CASE('celsius','deg c','deg_c','degc','deg celsius','deg_celsius',      &
           'degree c','degree celsius','degree_c','degreec','degree_celsius', &
           'degrees c','degrees celsius','degrees_c','degreesc',              &
           'degrees_celsius')
        SELECT CASE (TRIM(unit2))
        CASE('kelvin','k','deg k','deg kelvin','deg_k','deg_kelvin','degk',   &
           'degree k','degree kelvin','degree_k','degree_kelvin','degreek',   &
           'degrees k','degrees kelvin','degrees_k','degrees_kelvin',         &
           'degreesk')
          slope = 1.0_wp
          intercept = 273.15_wp
        CASE('farenheit','deg f','deg_f','degf','deg farenheit',              &
             'deg_farenheit','degree f','degree farenheit','degree_f',        &
             'degreef','degree_farenheit','degrees f','degrees farenheit',    &
             'degrees_f','degreesf','degrees_farenheit')
          slope = 1.8_wp
          intercept = 32.0_wp
        END SELECT
      CASE('farenheit','deg f','deg_f','degf','deg farenheit',                &
           'deg_farenheit','degree f','degree farenheit','degree_f',          &
           'degreef','degree_farenheit','degrees f','degrees farenheit',      &
           'degrees_f','degreesf','degrees_farenheit')    
        SELECT CASE (TRIM(unit2))
        CASE('kelvin','k','deg k','deg kelvin','deg_k','deg_kelvin','degk',   &
           'degree k','degree kelvin','degree_k','degree_kelvin','degreek',   &
           'degrees k','degrees kelvin','degrees_k','degrees_kelvin',         &
           'degreesk')
          slope = 0.555556_wp
          intercept = 255.372_wp
        CASE('celsius','deg c','deg_c','degc','deg celsius','deg_celsius',    &
           'degree c','degree celsius','degree_c','degreec','degree_celsius', &
           'degrees c','degrees celsius','degrees_c','degreesc',              &
           'degrees_celsius')
          slope = 0.555556_wp
          intercept = -17.7778_wp
        END SELECT

!-------------------------------------------------------------------------------
! 6. Humidity
!------------------------------------------------------------------------------- 

      CASE('g/kg','gram/kilogram','g/kilogram','gram/kg','grams/kilogram',    &
           'grams/kg','grams/kilograms','g / kg','gram / kilogram',           &
           'g / kilogram','gram / kg','grams / kilogram','grams / kg',        &
           'grams / kilograms')
        SELECT CASE (TRIM(unit2))
        CASE('kg/kg','kilogram/kilogram','kg/kilogram','kilogram/kg',         &
             'kilograms/kilogram','kilograms/kg','kilograms/kilograms',       &
             'kg / kg','kilogram / kilogram','kg / kilogram','kilogram / kg', &
             'kilograms / kilogram','kilograms / kg','kilograms / kilograms')
          slope = 0.001_wp
        CASE('g/g','gram/gram','g/gram','gram/g','grams/gram',    &
           'grams/g','grams/grams','g / g','gram / gram',           &
           'g / gram','gram / g','grams / gram','grams / g',        &
           'grams / grams')
          slope = 0.001_wp
        END SELECT

      CASE('kg/kg','kilogram/kilogram','kg/kilogram','kilogram/kg',           &
             'kilograms/kilogram','kilograms/kg','kilograms/kilograms',       &
             'kg / kg','kilogram / kilogram','kg / kilogram','kilogram / kg', &
             'kilograms / kilogram','kilograms / kg','kilograms / kilograms')     
        SELECT CASE (TRIM(unit2))
        CASE('g/kg','gram/kilogram','g/kilogram','gram/kg','grams/kilogram',  &
             'grams/kg','grams/kilograms','g / kg','gram / kilogram',         &
             'g / kilogram','gram / kg','grams / kilogram','grams / kg',      &
             'grams / kilograms')
          slope = 1000.0_wp
        CASE('g/g','gram/gram','g/gram','gram/g','grams/gram',    &
           'grams/g','grams/grams','g / g','gram / gram',           &
           'g / gram','gram / g','grams / gram','grams / g',        &
           'grams / grams')
          slope = 1.0_wp
        END SELECT

      CASE('g/g','gram/gram','g/gram','gram/g','grams/gram',    &
           'grams/g','grams/grams','g / g','gram / gram',           &
           'g / gram','gram / g','grams / gram','grams / g',        &
           'grams / grams')
        SELECT CASE (TRIM(unit2))
          CASE('g/kg','gram/kilogram','g/kilogram','gram/kg','grams/kilogram',&
             'grams/kg','grams/kilograms','g / kg','gram / kilogram',         &
             'g / kilogram','gram / kg','grams / kilogram','grams / kg',      &
             'grams / kilograms')
            slope = 1000.0_wp
          CASE('kg/kg','kilogram/kilogram','kg/kilogram','kilogram/kg',       &
             'kilograms/kilogram','kilograms/kg','kilograms/kilograms',       &
             'kg / kg','kilogram / kilogram','kg / kilogram','kilogram / kg', &
             'kilograms / kilogram','kilograms / kg','kilograms / kilograms')  
            slope = 1.0_wp
          END SELECT

!-------------------------------------------------------------------------------
! 7. Longitude
!-------------------------------------------------------------------------------

      CASE('degree_east','degrees_east','degree_e','degrees_e',     &
           'degreee','degreese')
        SELECT CASE (TRIM(unit2))
        CASE('degree_west','degrees_west','degree_w','degrees_w',   &
             'degreew','degreesw')
          slope = -1.0_wp
        END SELECT
      CASE('degree_west','degrees_west','degree_w','degrees_w',     &
             'degreew','degreesw')
        SELECT CASE (TRIM(unit2))
        CASE('degree_east','degrees_east','degree_e','degrees_e',   &
           'degreee','degreese')
          slope = -1.0_wp
        END SELECT

!-------------------------------------------------------------------------------
! 8. Time
!-------------------------------------------------------------------------------

        CASE('day','days')
          SELECT CASE (TRIM(unit2))
          CASE('h','hour','hours')
            slope = 24.0_wp
          CASE('min','minute','minutes')
            slope = 24.0_wp * 60.0_wp
          CASE('s','sec','second','seconds')
            slope = 24.0_wp * 60.0_wp * 60.0_wp
          END SELECT
        CASE('h','hour','hours')
          SELECT CASE (TRIM(unit2))
          CASE('day','days')
            slope = 1.0_wp / 24.0_wp
          CASE('min','minute','minutes')
            slope = 60.0_wp
          CASE('s','sec','second','seconds')
            slope = 60.0_wp * 60.0_wp
          END SELECT
        CASE('min','minute','minutes')
          SELECT CASE (TRIM(unit2))
          CASE('day','days')
            slope = 1.0_wp / (60.0_wp * 24.0_wp)
          CASE('h','hour','hours')
            slope = 1.0_wp / 60.0_wp
          CASE('s','sec','second','seconds')
            slope = 60.0_wp
          END SELECT
        CASE('s','sec','second','seconds')
          SELECT CASE (TRIM(unit2))
          CASE('day','days')
            slope = 1.0_wp / (60.0_wp * 60.0_wp * 24.0_wp)
          CASE('h','hour','hours')
            slope = 1.0_wp / (60.0_wp * 60.0_wp)
          CASE('min','minute','minutes')
            slope = 1.0 / 60.0_wp
          END SELECT

!-------------------------------------------------------------------------------
! 9. Matched aliases
!-------------------------------------------------------------------------------

      CASE('%','percent')
        SELECT CASE (TRIM(unit2))
        CASE('%','percent')
          slope = 1.0_wp
        END SELECT

      CASE('rad','radian','radians')
        SELECT CASE (TRIM(unit2))
        CASE('rad','radians')
          slope = 1.0_wp
        CASE('deg','degree','degrees','degree_t','degrees_t', &
             'degree_north','degrees_north','degree_east','degrees_east')
          slope = 180.0_wp / 3.141592653589793238_wp 
        END SELECT

      CASE('arc_degree','deg','degree','degrees','de',                    &
           'degree_north','degrees_north','degree_n',                     &
           'degrees_n','degreen','degreesn','degree_true',                &
           'degrees_true','degree_t','degrees_t','degreet','degreest')
        SELECT CASE (TRIM(unit2))
        CASE('arc_degree','deg','degree','degrees','de',                    &
             'degree_north','degrees_north','degree_n',                     &
             'degrees_n','degreen','degreesn','degree_east','degrees_east', &
             'degree_e','degrees_e','degreee','degreese','degree_true',     &
             'degrees_true','degree_t','degrees_t','degreet','degreest')
          slope = 1.0_wp
        CASE('rad','radian','radians')
          slope = 3.141592653589793238_wp / 180.0_wp
        END SELECT

      CASE('v/v','volt/volt','v / v', 'volt / volt')
        SELECT CASE (TRIM(unit2))
        CASE('v/v','volt/volt','v / v', 'volt / volt')
          slope = 1.0_wp
        END SELECT

      CASE('yr','year','years')
        SELECT CASE (TRIM(unit2))
        CASE('yr','year','years')
          slope = 1.0_wp
        END SELECT

      CASE('month','months')
        SELECT CASE (TRIM(unit2))
        CASE('month','months')
          slope = 1.0_wp
        END SELECT  

      CASE('ms','millisec','millisecond','milliseconds')
        SELECT CASE (TRIM(unit2))
        CASE('ms','millisec','millisecond','milliseconds')
          slope = 1.0_wp
        END SELECT

      CASE('bit','bits')
        SELECT CASE (TRIM(unit2))
        CASE('bit','bits')
          slope = 1.0_wp
        END SELECT  

      CASE('n-unit','n-units')
        SELECT CASE (TRIM(unit2))
        CASE('n-unit','n-units')
          slope = 1.0_wp
        END SELECT  
        
!-------------------------------------------------------------------------------
! 10. Speed
!-------------------------------------------------------------------------------

    CASE('m/s','m/second','m / s', 'm / second',                       &
         'meter/second','meters/second','meters/seconds',              & 
         'meter / second','meters / second','meters / seconds',        &
         'metre/second','metres/second','metres/seconds',              &
         'metre / second','metres / second','metres / seconds')
      SELECT CASE (TRIM(unit2))
      CASE('m/h','m/hour','m / h', 'm / hour',                         &
         'meter/hour','meters/hour','meters/hours',                    &
         'meter / hour','meters / hour','meters / hours',              &
         'metre/hour','metres/hour','metres/hours',                    &
         'metre / hour','metres / hour','metres / hours')
        slope = 3600.0_wp
      CASE('km/s','km/second','km / s', 'km / second',                      &
         'kilometer/second','kilometers/second','kilometers/seconds',       &
         'kilometer / second','kilometers / second','kilometers / seconds', &
         'kilometre/second','kilometres/second','kilometres/seconds',       &
         'kilometre / second','kilometres / second','kilometres / seconds')
        slope = 0.001_wp
      CASE('km/h','km/hour','km / hour', 'km / hours',                  &
         'kilometer/hour','kilometers/hour','kilometers/hours',         &
         'kilometer / hour','kilometers / hour','kilometers / hours',   &
         'kilometre/hour','kilometres/hour','kilometres/hours',         &
         'kilometre / hour','kilometres / hour','kilometres / hours') 
        slope = 3.6_wp
      CASE('mile/s','mile/second','mile / s', 'mile / second',          &
           'miles/second','miles/seconds',',miles / second','miles / seconds') 
        slope = 0.000621371_wp
      CASE('mile/h','mile/hour','mile / h', 'mile / hour',              &
           'miles/hour','miles/hours',',miles / hour','miles / hours')
        slope = 2.23694_wp
      END SELECT
    CASE('m/h','m/hour','m / h', 'm / hour',                            &
         'meter/hour','meters/hour','meters/hours',                     &
         'meter / hour','meters / hour','meters / hours',               &
         'metre/hour','metres/hour','metres/hours',                     &
         'metre / hour','metres / hour','metres / hours') 
      SELECT CASE (TRIM(unit2))
      CASE('m/s','m/second','m / s', 'm / second',                      &
         'meter/second','meters/second','meters/seconds',               &
         'meter / second','meters / second','meters / seconds',         &
         'metre/second','metres/second','metres/seconds',               &
         'metre / second','metres / second','metres / seconds')
        slope = 1.0_wp / 3600.0_wp
      CASE('km/s','km/second','km / s', 'km / second',                      &
         'kilometer/second','kilometers/second','kilometers/seconds',       &
         'kilometer / second','kilometers / second','kilometers / seconds', &
         'kilometre/second','kilometres/second','kilometres/seconds',       &
         'kilometre / second','kilometres / second','kilometres / seconds')
        slope = 1.0_wp / 3600000.0_wp
      CASE('km/h','km/hour','km / hour', 'km / hours',                  &
         'kilometer/hour','kilometers/hour','kilometers/hours',         &
         'kilometer / hour','kilometers / hour','kilometers / hours',   &
         'kilometre/hour','kilometres/hour','kilometres/hours',         &
         'kilometre / hour','kilometres / hour','kilometres / hours') 
        slope = 0.001_wp
      CASE('mile/s','mile/second','mile / s', 'mile / second',          &
           'miles/second','miles/seconds',',miles / second','miles / seconds') 
        slope = 1.72603e-7_wp
      CASE('mile/h','mile/hour','mile / h', 'mile / hour',              &
           'miles/hour','miles/hours',',miles / hour','miles / hours')
        slope = 0.000621371_wp
      END SELECT
    CASE('km/s','km/second','km / s', 'km / second',                        &
         'kilometer/second','kilometers/second','kilometers/seconds',       &
         'kilometer / second','kilometers / second','kilometers / seconds', &
         'kilometre/second','kilometres/second','kilometres/seconds',       &
         'kilometre / second','kilometres / second','kilometres / seconds')
      SELECT CASE (TRIM(unit2))
      CASE('m/s','m/second','m / s', 'm / second',                     &
         'meter/second','meters/second','meters/seconds',              &
         'meter / second','meters / second','meters / seconds',        &
         'metre/second','metres/second','metres/seconds',              &
         'metre / second','metres / second','metres / seconds')
        slope = 1000.0_wp
      CASE('m/h','m/hour','m / h', 'm / hour',                     &
         'meter/hour','meters/hour','meters/hours',                &
         'meter / hour','meters / hour','meters / hours',          &
         'metre/hour','metres/hour','metres/hours',                &
         'metre / hour','metres / hour','metres / hours') 
        slope = 3.6e6_wp
      CASE('km/h','km/hour','km / hour', 'km / hours',                &
         'kilometer/hour','kilometers/hour','kilometers/hours',       &
         'kilometer / hour','kilometers / hour','kilometers / hours', &
         'kilometre/hour','kilometres/hour','kilometres/hours',       &
         'kilometre / hour','kilometres / hour','kilometres / hours') 
        slope = 3600.0_wp
      CASE('mile/s','mile/second','mile / s', 'mile / second',        &
           'miles/second','miles/seconds',',miles / second','miles / seconds') 
        slope = 0.621371_wp 
      CASE('mile/h','mile/hour','mile / h', 'mile / hour',            &
           'miles/hour','miles/hours',',miles / hour','miles / hours')
        slope = 2236.94_wp
      END SELECT
    CASE('km/h','km/hour','km / hour', 'km / hours',                  &
         'kilometer/hour','kilometers/hour','kilometers/hours',       &
         'kilometer / hour','kilometers / hour','kilometers / hours', &
         'kilometre/hour','kilometres/hour','kilometres/hours',       &
         'kilometre / hour','kilometres / hour','kilometres / hours') 
      SELECT CASE (TRIM(unit2))
      CASE('m/s','m/second','m / s', 'm / second',                    &
         'meter/second','meters/second','meters/seconds',             &
         'meter / second','meters / second','meters / seconds',       &
         'metre/second','metres/second','metres/seconds',             &
         'metre / second','metres / second','metres / seconds')
        slope = 1.0_wp / 3.6_wp
      CASE('m/h','m/hour','m / h', 'm / hour',                        &
         'meter/hour','meters/hour','meters/hours',                   &
         'meter / hour','meters / hour','meters / hours',             &
         'metre/hour','metres/hour','metres/hours',                   &
         'metre / hour','metres / hour','metres / hours') 
        slope = 1000.0_wp
      CASE('km/s','km/second','km / s', 'km / second',                      &
         'kilometer/second','kilometers/second','kilometers/seconds',       &
         'kilometer / second','kilometers / second','kilometers / seconds', &
         'kilometre/second','kilometres/second','kilometres/seconds',       &
         'kilometre / second','kilometres / second','kilometres / seconds')
        slope = 1.0_wp / 3600.0_wp
      CASE('mile/s','mile/second','mile / s', 'mile / second',        &
           'miles/second','miles/seconds',',miles / second','miles / seconds') 
        slope = 0.000172603_wp
      CASE('mile/h','mile/hour','mile / h', 'mile / hour',            &
           'miles/hour','miles/hours',',miles / hour','miles / hours')
        slope = 0.621371_wp
      END SELECT
    CASE('mile/s','mile/second','mile / s', 'mile / second',         &
         'miles/second','miles/seconds',',miles / second','miles / seconds') 
      SELECT CASE (TRIM(unit2))
      CASE('m/s','m/second','m / s', 'm / second',                   &
         'meter/second','meters/second','meters/seconds',            &
         'meter / second','meters / second','meters / seconds',      &
         'metre/second','metres/second','metres/seconds',            &
         'metre / second','metres / second','metres / seconds')
        slope = 1609.34_wp
      CASE('m/h','m/hour','m / h', 'm / hour',                    &
         'meter/hour','meters/hour','meters/hours',               &
         'meter / hour','meters / hour','meters / hours',         &
         'metre/hour','metres/hour','metres/hours',               &
         'metre / hour','metres / hour','metres / hours') 
        slope = 5.79364e6_wp
      CASE('km/s','km/second','km / s', 'km / second',                      &
         'kilometer/second','kilometers/second','kilometers/seconds',       &
         'kilometer / second','kilometers / second','kilometers / seconds', &
         'kilometre/second','kilometres/second','kilometres/seconds',       &
         'kilometre / second','kilometres / second','kilometres / seconds')
        slope = 1.60934_wp
      CASE('km/h','km/hour','km / hour', 'km / hours',                &
         'kilometer/hour','kilometers/hour','kilometers/hours',       &
         'kilometer / hour','kilometers / hour','kilometers / hours', &
         'kilometre/hour','kilometres/hour','kilometres/hours',       &
         'kilometre / hour','kilometres / hour','kilometres / hours') 
        slope = 5793.64_wp
      CASE('mile/h','mile/hour','mile / h', 'mile / hour',         &
           'miles/hour','miles/hours',',miles / hour','miles / hours')
        slope = 3600.0_wp
      END SELECT
    CASE('mile/h','mile/hour','mile / h', 'mile / hour',         &
         'miles/hour','miles/hours',',miles / hour','miles / hours')
      SELECT CASE (TRIM(unit2))
      CASE('m/s','m/second','m / s', 'm / second',                   &
         'meter/second','meters/second','meters/seconds',            &
         'meter / second','meters / second','meters / seconds',      &
         'metre/second','metres/second','metres/seconds',            &
         'metre / second','metres / second','metres / seconds')
        slope = 0.44704_wp
      CASE('m/h','m/hour','m / h', 'm / hour',                    &
         'meter/hour','meters/hour','meters/hours',               &
         'meter / hour','meters / hour','meters / hours',         &
         'metre/hour','metres/hour','metres/hours',               &
         'metre / hour','metres / hour','metres / hours') 
        slope = 1609.34_wp
      CASE('km/s','km/second','km / s', 'km / second',                      &
         'kilometer/second','kilometers/second','kilometers/seconds',       &
         'kilometer / second','kilometers / second','kilometers / seconds', &
         'kilometre/second','kilometres/second','kilometres/seconds',       &
         'kilometre / second','kilometres / second','kilometres / seconds')
        slope = 0.00044704_wp
      CASE('km/h','km/hour','km / hour', 'km / hours',                &
         'kilometer/hour','kilometers/hour','kilometers/hours',       &
         'kilometer / hour','kilometers / hour','kilometers / hours', &
         'kilometre/hour','kilometres/hour','kilometres/hours',       &
         'kilometre / hour','kilometres / hour','kilometres / hours') 
        slope = 1.60934_wp
      CASE('mile/s','mile/second','mile / s', 'mile / second',        &
         'miles/second','miles/seconds',',miles / second','miles / seconds') 
        slope = 1.0_wp / 3600.0_wp
      END SELECT

!-------------------------------------------------------------------------------
! 11. Geopotential height
!-------------------------------------------------------------------------------

    CASE('geopotential metre','geopotential meter','geopotential metres',   &
         'geopotential meters','geopotential m','gpm','gp m')
      SELECT CASE (TRIM(unit2))
      CASE('geoppotential kilometre','geopotential kilometer','geopotential kilometres',  &
           'geopotential kilometers','geopotential km','gpkm','gp km')
        slope = 0.001_wp
      END SELECT
    CASE('geoppotential kilometre','geopotential kilometer','geopotential kilometres',  &
           'geopotential kilometers','geopotential km','gpkm','gp km')
      SELECT CASE (TRIM(unit2))
      CASE('geopotential metre','geopotential meter','geopotential metres',   &
         'geopotential meters','geopotential m','gpm','gp m')
        slope = 1000.0_wp
      END SELECT

!-------------------------------------------------------------------------------
! 12. Number density
!-------------------------------------------------------------------------------

    CASE('m-3','/m3','per m3','metre-3','/metre3','per metre3', &
         'meter-3','/meter3','per meter3')
      SELECT CASE (TRIM(unit2))
      CASE('cm-3','/cm3','per cm3','centimetre-3','/centimetre3', &
           'per centimetre3','centimeter-3','/centimeter3','per centimeter3')
        slope = 0.000001_wp
      END SELECT
    CASE('cm-3','/cm3','per cm3','centimetre-3','/centimetre3', &
         'per centimetre3','centimeter-3','/centimeter3','per centimeter3')
      SELECT CASE (TRIM(unit2))
      CASE('m-3','/m3','per m3','metre-3','/metre3','per metre3', &
           'meter-3','/meter3','per meter3')
        slope = 1000000.0_wp
      END SELECT

!-------------------------------------------------------------------------------
! 13. Line-integrated number density (e.g. TEC)
!-------------------------------------------------------------------------------

    CASE('m-2','/m2','per m2','metre-2','/metre2','per metre2', &
         'meter-2','/meter2','per meter2')
      SELECT CASE (TRIM(unit2))
      CASE('cm-2','/cm2','per cm2','centimetre-2','/centimetre2', &
           'per centimetre2','centimeter-2','/centimeter2','per centimeter2')
        slope = 0.0001_wp
      CASE('tecu','TECU')
        slope = 1.0E-16_wp
      END SELECT
    CASE('cm-2','/cm2','per cm2','centimetre-2','/centimetre2', &
         'per centimetre2','centimeter-2','/centimeter2','per centimeter2')
      SELECT CASE (TRIM(unit2))
      CASE('m-2','/m2','per m2','metre-2','/metre2','per metre2', &
           'meter-2','/meter2','per meter2')
        slope = 10000.0_wp
      CASE('tecu','TECU')
        slope = 1.0E-12_wp
      END SELECT
    CASE('tecu','TECU')
      SELECT CASE (TRIM(unit2))
      CASE('m-2','/m2','per m2','metre-2','/metre2','per metre2', &
           'meter-2','/meter2','per meter2')
        slope = 1.0E16_wp
      CASE('cm-2','/cm2','per cm2','centimetre-2','/centimetre2', &
           'per centimetre2','centimeter-2','/centimeter2','per centimeter2')
        slope = 1.0E12_wp
      END SELECT

!-------------------------------------------------------------------------------
! 14. Frequency
!-------------------------------------------------------------------------------

    CASE('hz','hertz','cps','c/s','cycles/sec','cycle/sec', &
         'cycles per sec', 'cycle per sec')

      SELECT CASE (TRIM(unit2))
      CASE('mhz','megahertz','mcps','mc/s','megacycles/sec','megacycle/sec', &
           'megacycles per sec', 'megacycle per sec')
        slope = 1.0E-6_wp
      CASE('ghz','gigahertz','gcps','gc/s','gigacycles/sec','gigacycle/sec', &
           'gigacycles per sec', 'gigacycle per sec')
        slope = 1.0E-9_wp
      END SELECT

    CASE('mhz','megahertz','mcps','mc/s','megacycles/sec','megacycle/sec', &
           'megacycles per sec', 'megacycle per sec')

      SELECT CASE (TRIM(unit2))
      CASE('ghz','gigahertz','gcps','gc/s','gigacycles/sec','gigacycle/sec', &
           'gigacycles per sec', 'gigacycle per sec')
        slope = 1.0E-3_wp
      CASE('hz','hertz','cps','c/s','cycles/sec','cycle/sec', &
           'cycles per sec', 'cycle per sec')
        slope = 1.0E6_wp
      END SELECT

    CASE('ghz','gigahertz','gcps','gc/s','gigacycles/sec','gigacycle/sec', &
         'gigacycles per sec', 'gigacycle per sec')
      SELECT CASE (TRIM(unit2))
      CASE('hz','hertz','cps','c/s','cycles/sec','cycle/sec', &
           'cycles per sec', 'cycle per sec')
        slope = 1.0E9_wp
      CASE('mhz','megahertz','mcps','mc/s','megacycles/sec','megacycle/sec', &
           'megacycles per sec', 'megacycle per sec')
        slope = 1.0E3_wp
      END SELECT

!-------------------------------------------------------------------------------
! 15. Default
!-------------------------------------------------------------------------------

      CASE DEFAULT
        WRITE (*, "(A)") 'WARNING: Unit ' // TRIM(unit1) //    &
                         ' not recognised.' 
        WRITE (*, "(A)") 'Check required unit conversion'
        WRITE (*, "(A)") 'Amend ropp_utils/ropp_unit_conversion.f90 as required'

    END SELECT

CONTAINS

  SUBROUTINE To_Lower ( string )
    
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(INOUT) :: string
    
    CHARACTER (LEN=26), PARAMETER :: UPPER="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    CHARACTER (LEN=26), PARAMETER :: lower="abcdefghijklmnopqrstuvwxyz"
    INTEGER :: i, j
    
    DO i = 1, LEN_TRIM(string)
      j = INDEX ( UPPER, string(i:i) )
      IF ( j > 0 ) string(i:i) = lower(j:j)
    END DO
    
  END SUBROUTINE To_Lower

END SUBROUTINE ropp_unit_conversion
