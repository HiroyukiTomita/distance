*
* distance.F bo pass_thru.F
*
* Jonathan Callahan
* Feb 19th 1998
*
* This function sets the second argument to be equal
* to the first one.
*


*
* In this subroutine we provide information about
* the function.  The user configurable information 
* consists of the following:
*
* descr              Text description of the function
*
* num_args           Required number of arguments
*
* axis_inheritance   Type of axis for the result
*                       ( CUSTOM, IMPLIED_BY_ARGS, NORMAL, ABSTRACT )
*                       CUSTOM          - user defined axis
*                       IMPLIED_BY_ARGS - same axis as the incoming argument
*                       NORMAL          - the result is normal to this axis
*                       ABSTRACT        - an axis which only has index values
*
* piecemeal_ok       For memory optimization:
*                       axes where calculation may be performed piecemeal
*                       ( YES, NO )
* 
*
* For each argument we provide the following information:
*
* name               Text name for an argument
*
* unit               Text units for an argument
*
* desc               Text description of an argument
*
* axis_influence     Are this argument's axes the same as the result grid?
*                       ( YES, NO )
*
* axis_extend       How much does Ferret need to extend arg limits relative to result 
*


      SUBROUTINE distance_init(id)

      INCLUDE 'ferret_cmn/EF_Util.cmn'

      INTEGER id, arg

      CALL ef_version_test(ef_version)

* **********************************************************************
*                                            USER CONFIGURABLE PORTION |
*                                                                      |
*                                                                      V

      CALL ef_set_desc(id,
     . '(distance cal from a center point) sets result equal to input' )

      CALL ef_set_num_args(id, 3)
      CALL ef_set_axis_inheritance(id, IMPLIED_BY_ARGS, 
     .     IMPLIED_BY_ARGS, IMPLIED_BY_ARGS, IMPLIED_BY_ARGS)
      CALL ef_set_piecemeal_ok(id, NO, NO, NO, NO)

      arg = 1
      CALL ef_set_arg_name(id, arg, 'A')
      CALL ef_set_arg_unit(id, arg, ' ')
      CALL ef_set_arg_desc(id, arg, 'Target variable')
      CALL ef_set_axis_influence(id, arg, YES, YES, YES, YES)

      arg = 2
      CALL ef_set_arg_name(id, arg, 'xloc')
      CALL ef_set_arg_unit(id, arg, 'deg.E')
      CALL ef_set_arg_desc(id, arg, 'longitude of center point')
      CALL ef_set_axis_influence(id, arg, NO, NO, NO, NO)

      arg = 3
      CALL ef_set_arg_name(id, arg, 'yloc')
      CALL ef_set_arg_unit(id, arg, 'deg.N')
      CALL ef_set_arg_desc(id, arg, 'latitude of center point')
      CALL ef_set_axis_influence(id, arg, NO, NO, NO, NO)

      CALL ef_set_num_work_arrays(id,2)
*                                                                      ^
*                                                                      |
*                                            USER CONFIGURABLE PORTION |
* **********************************************************************

      RETURN 
      END


*
* In this subroutine we compute the result
*
      SUBROUTINE distance_compute(id, arg_1, arg_2, arg_3, result,
     &                            xax, yax )

      INCLUDE 'ferret_cmn/EF_Util.cmn'
      INCLUDE 'ferret_cmn/EF_mem_subsc.cmn'

      INTEGER id

      REAL bad_flag(1:EF_MAX_ARGS), bad_flag_result
      REAL arg_1(mem1lox:mem1hix, mem1loy:mem1hiy, 
     .           mem1loz:mem1hiz, mem1lot:mem1hit)
      REAL result(memreslox:memreshix, memresloy:memreshiy,
     .            memresloz:memreshiz, memreslot:memreshit)

* After initialization, the 'res_' arrays contain indexing information 
* for the result axes.  The 'arg_' arrays will contain the indexing 
* information for each variable's axes. 

      INTEGER res_lo_ss(4), res_hi_ss(4), res_incr(4)
      INTEGER arg_lo_ss(4,1:EF_MAX_ARGS), arg_hi_ss(4,1:EF_MAX_ARGS),
     .     arg_incr(4,1:EF_MAX_ARGS)

* **********************************************************************
*                                            USER CONFIGURABLE PORTION |
*                                                                      |
*                                                                      V

      INTEGER i,j,k,l
      INTEGER i1, j1, k1, l1
      REAL*8 xax(wrk1lox:wrk1hix, wrk1loy:wrk1hiy,
     . wrk1loz:wrk1hiz, wrk1lot:wrk1hit)
      REAL*8 yax(wrk2lox:wrk2hix, wrk2loy:wrk2hiy,
     . wrk2loz:wrk2hiz, wrk2lot:wrk2hit)

      ZLON1=arg_2 !123.3
      ZLAT1=arg_3 !17.5

      PI=3.14159
      E2=0.006694470
      AR=6378.137
      P=PI/180.0
      DST=1.0

      CALL ef_get_res_subscripts(id, res_lo_ss, res_hi_ss, res_incr)
      CALL ef_get_arg_subscripts(id, arg_lo_ss, arg_hi_ss, arg_incr)
      CALL ef_get_bad_flags(id, bad_flag, bad_flag_result)

      CALL ef_get_coordinates(id, ARG1, X_AXIS, arg_lo_ss(X_AXIS,ARG1),
     &                      arg_hi_ss(X_AXIS,ARG1),xax)

      CALL ef_get_coordinates(id, ARG1, Y_AXIS, arg_lo_ss(Y_AXIS,ARG1),
     &                      arg_hi_ss(Y_AXIS,ARG1),yax)

      i1 = arg_lo_ss(X_AXIS,ARG1)
      DO 400 i=res_lo_ss(X_AXIS), res_hi_ss(X_AXIS)

         j1 = arg_lo_ss(Y_AXIS,ARG1)
         DO 300 j=res_lo_ss(Y_AXIS), res_hi_ss(Y_AXIS)

            k1 = arg_lo_ss(Z_AXIS,ARG1)
            DO 200 k=res_lo_ss(Z_AXIS), res_hi_ss(Z_AXIS)

               l1 = arg_lo_ss(T_AXIS,ARG1)
               DO 100 l=res_lo_ss(T_AXIS), res_hi_ss(T_AXIS)

                  IF ( arg_1(i,j,k,l) .EQ. bad_flag(1) ) THEN

                     result(i,j,k,l) = bad_flag_result

* Or you could bail out if there is bad data
*
*            CALL EF_BAIL_OUT(id,'I refuse to work with bad data.')

                  ELSE
                     ZLON2=xax(i1,1,1,1)
                     ZLAT2=yax(1,j1,1,1)
                     CLAT = ( (ZLAT1 + ZLAT2)/2.0 ) * P
                     Q = 1.0 - (E2*(SIN(CLAT))*(SIN(CLAT)))
                     DLAT=P*((AR*(1.0-E2))/Q**1.5)
                     DLON=P*((AR*COS(CLAT))/SQRT(Q))
                     ELATI=((ZLAT1-ZLAT2)*DLAT)*((ZLAT1-ZLAT2)*DLAT)
                     ELONG=((ZLON1-ZLON2)*DLON)*((ZLON1-ZLON2)*DLON)
                     DST=SQRT(ELATI+ELONG)
                     result(i,j,k,l) = DST
                  END IF

                  l1 = l1 + arg_incr(T_AXIS,ARG1)
 100           CONTINUE

               k1 = k1 + arg_incr(Z_AXIS,ARG1)
 200        CONTINUE

            j1 = j1 + arg_incr(Y_AXIS,ARG1)
 300     CONTINUE

         i1 = i1 + arg_incr(X_AXIS,ARG1)
 400  CONTINUE
      
*                                                                      ^
*                                                                      |
*                                            USER CONFIGURABLE PORTION |
* **********************************************************************

      RETURN 
      END

      SUBROUTINE distance_work_size(id)
      INCLUDE 'ferret_cmn/EF_Util.cmn'
      INCLUDE 'ferret_cmn/EF_mem_subsc.cmn'
      INTEGER id

* Set the work arrays, X/Y/Z/T dimensions

      INTEGER nxout, nx2
      INTEGER arg_lo_ss(4,1:EF_MAX_ARGS), arg_hi_ss(4,1:EF_MAX_ARGS),
     &        arg_incr(4,1:EF_MAX_ARGS)

      CALL ef_get_arg_subscripts(id, arg_lo_ss, arg_hi_ss, arg_incr)

      nxout = 1 + arg_hi_ss(X_AXIS,ARG1) - arg_lo_ss(X_AXIS,ARG1)
      nx2 = nxout* 2

      nyout = 1 + arg_hi_ss(Y_AXIS,ARG1) - arg_lo_ss(Y_AXIS,ARG1)
      ny2 = nyout* 2

* Define work array XAX
* ef_set_work_array_dims(id, iarray, xlo, ylo, zlo, tlo, xhi, yhi, zhi,
* thi) 

      CALL ef_set_work_array_dims (id, 1, 1, 1, 1, 1, nx2, 1, 1, 1)
      CALL ef_set_work_array_dims (id, 2, 1, 1, 1, 1, 1, ny2, 1, 1)

      RETURN
      END
