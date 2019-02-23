
























MODULE module_domain

   USE module_driver_constants
   USE module_machine
   USE module_configure
   USE module_wrf_error
   USE module_utility
   USE module_domain_type

   
   
   
   

   
   
   
   
   

   TYPE(domain) , POINTER :: head_grid , new_grid , next_grid , old_grid

   
   
   
   

   TYPE domain_levels
      TYPE(domain) , POINTER                              :: first_domain
   END TYPE domain_levels

   TYPE(domain_levels) , DIMENSION(max_levels)            :: head_for_each_level

   
   TYPE(domain), POINTER :: current_grid
   LOGICAL, SAVE :: current_grid_set = .FALSE.

   
   PRIVATE domain_time_test_print
   PRIVATE test_adjust_io_timestr

   INTERFACE get_ijk_from_grid
     MODULE PROCEDURE get_ijk_from_grid1, get_ijk_from_grid2
   END INTERFACE

   INTEGER, PARAMETER :: max_hst_mods = 200

CONTAINS

   SUBROUTINE adjust_domain_dims_for_move( grid , dx, dy )
    IMPLICIT NONE

    TYPE( domain ), POINTER   :: grid
    INTEGER, INTENT(IN) ::  dx, dy

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
            grid%sm31  = grid%sm31 + dx
            grid%em31  = grid%em31 + dx
            grid%sm32  = grid%sm32 + dy
            grid%em32  = grid%em32 + dy
            grid%sp31  = grid%sp31 + dx
            grid%ep31  = grid%ep31 + dx
            grid%sp32  = grid%sp32 + dy
            grid%ep32  = grid%ep32 + dy
            grid%sd31  = grid%sd31 + dx
            grid%ed31  = grid%ed31 + dx
            grid%sd32  = grid%sd32 + dy
            grid%ed32  = grid%ed32 + dy

       CASE  ( DATA_ORDER_YXZ )
            grid%sm31  = grid%sm31 + dy
            grid%em31  = grid%em31 + dy
            grid%sm32  = grid%sm32 + dx
            grid%em32  = grid%em32 + dx
            grid%sp31  = grid%sp31 + dy
            grid%ep31  = grid%ep31 + dy
            grid%sp32  = grid%sp32 + dx
            grid%ep32  = grid%ep32 + dx
            grid%sd31  = grid%sd31 + dy
            grid%ed31  = grid%ed31 + dy
            grid%sd32  = grid%sd32 + dx
            grid%ed32  = grid%ed32 + dx

       CASE  ( DATA_ORDER_ZXY )
            grid%sm32  = grid%sm32 + dx
            grid%em32  = grid%em32 + dx
            grid%sm33  = grid%sm33 + dy
            grid%em33  = grid%em33 + dy
            grid%sp32  = grid%sp32 + dx
            grid%ep32  = grid%ep32 + dx
            grid%sp33  = grid%sp33 + dy
            grid%ep33  = grid%ep33 + dy
            grid%sd32  = grid%sd32 + dx
            grid%ed32  = grid%ed32 + dx
            grid%sd33  = grid%sd33 + dy
            grid%ed33  = grid%ed33 + dy

       CASE  ( DATA_ORDER_ZYX )
            grid%sm32  = grid%sm32 + dy
            grid%em32  = grid%em32 + dy
            grid%sm33  = grid%sm33 + dx
            grid%em33  = grid%em33 + dx
            grid%sp32  = grid%sp32 + dy
            grid%ep32  = grid%ep32 + dy
            grid%sp33  = grid%sp33 + dx
            grid%ep33  = grid%ep33 + dx
            grid%sd32  = grid%sd32 + dy
            grid%ed32  = grid%ed32 + dy
            grid%sd33  = grid%sd33 + dx
            grid%ed33  = grid%ed33 + dx

       CASE  ( DATA_ORDER_XZY )
            grid%sm31  = grid%sm31 + dx
            grid%em31  = grid%em31 + dx
            grid%sm33  = grid%sm33 + dy
            grid%em33  = grid%em33 + dy
            grid%sp31  = grid%sp31 + dx
            grid%ep31  = grid%ep31 + dx
            grid%sp33  = grid%sp33 + dy
            grid%ep33  = grid%ep33 + dy
            grid%sd31  = grid%sd31 + dx
            grid%ed31  = grid%ed31 + dx
            grid%sd33  = grid%sd33 + dy
            grid%ed33  = grid%ed33 + dy

       CASE  ( DATA_ORDER_YZX )
            grid%sm31  = grid%sm31 + dy
            grid%em31  = grid%em31 + dy
            grid%sm33  = grid%sm33 + dx
            grid%em33  = grid%em33 + dx
            grid%sp31  = grid%sp31 + dy
            grid%ep31  = grid%ep31 + dy
            grid%sp33  = grid%sp33 + dx
            grid%ep33  = grid%ep33 + dx
            grid%sd31  = grid%sd31 + dy
            grid%ed31  = grid%ed31 + dy
            grid%sd33  = grid%sd33 + dx
            grid%ed33  = grid%ed33 + dx

    END SELECT data_ordering


    RETURN
   END SUBROUTINE adjust_domain_dims_for_move

   SUBROUTINE get_ijk_from_grid1 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
    IMPLICIT NONE
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey

     CALL get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )
     data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_YXZ )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_ZXY )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_ZYX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_XZY )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
       CASE  ( DATA_ORDER_YZX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
     END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid1

   SUBROUTINE get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )

    IMPLICIT NONE

    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe

    data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           ids = grid%sd31 ; ide = grid%ed31 ; jds = grid%sd32 ; jde = grid%ed32 ; kds = grid%sd33 ; kde = grid%ed33 ;
           ims = grid%sm31 ; ime = grid%em31 ; jms = grid%sm32 ; jme = grid%em32 ; kms = grid%sm33 ; kme = grid%em33 ;
           ips = grid%sp31 ; ipe = grid%ep31 ; jps = grid%sp32 ; jpe = grid%ep32 ; kps = grid%sp33 ; kpe = grid%ep33 ; 
       CASE  ( DATA_ORDER_YXZ )
           ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd33  ; kde = grid%ed33  ; 
           ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm33  ; kme = grid%em33  ; 
           ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp33  ; kpe = grid%ep33  ; 
       CASE  ( DATA_ORDER_ZXY )
           ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd31  ; kde = grid%ed31  ; 
           ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm31  ; kme = grid%em31  ; 
           ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp31  ; kpe = grid%ep31  ; 
       CASE  ( DATA_ORDER_ZYX )
           ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd32  ; jde = grid%ed32  ; kds = grid%sd31  ; kde = grid%ed31  ; 
           ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm32  ; jme = grid%em32  ; kms = grid%sm31  ; kme = grid%em31  ; 
           ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp32  ; jpe = grid%ep32  ; kps = grid%sp31  ; kpe = grid%ep31  ; 
       CASE  ( DATA_ORDER_XZY )
           ids = grid%sd31  ; ide = grid%ed31  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd32  ; kde = grid%ed32  ; 
           ims = grid%sm31  ; ime = grid%em31  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm32  ; kme = grid%em32  ; 
           ips = grid%sp31  ; ipe = grid%ep31  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp32  ; kpe = grid%ep32  ; 
       CASE  ( DATA_ORDER_YZX )
           ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd32  ; kde = grid%ed32  ; 
           ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm32  ; kme = grid%em32  ; 
           ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp32  ; kpe = grid%ep32  ; 
    END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid2




   SUBROUTINE get_ijk_from_subgrid (  grid ,                &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0    )
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0
   
    INTEGER              ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe
     CALL get_ijk_from_grid (  grid ,                         &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )
     ids0 = ids
     ide0 = ide * grid%sr_x
     ims0 = (ims-1)*grid%sr_x+1
     ime0 = ime * grid%sr_x
     ips0 = (ips-1)*grid%sr_x+1
     ipe0 = ipe * grid%sr_x

     jds0 = jds
     jde0 = jde * grid%sr_y
     jms0 = (jms-1)*grid%sr_y+1
     jme0 = jme * grid%sr_y
     jps0 = (jps-1)*grid%sr_y+1
     jpe0 = jpe * grid%sr_y

     kds0 = kds
     kde0 = kde
     kms0 = kms
     kme0 = kme
     kps0 = kps
     kpe0 = kpe
   RETURN
   END SUBROUTINE get_ijk_from_subgrid




   SUBROUTINE wrf_patch_domain( id , domdesc , parent, parent_id , parent_domdesc , &
                            sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                            sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                            sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                        sp1x , ep1x , sm1x , em1x , &
                                        sp2x , ep2x , sm2x , em2x , &
                                        sp3x , ep3x , sm3x , em3x , &
                                        sp1y , ep1y , sm1y , em1y , &
                                        sp2y , ep2y , sm2y , em2y , &
                                        sp3y , ep3y , sm3y , em3y , &
                            bdx , bdy , bdy_mask )
















































   USE module_machine
   IMPLICIT NONE
   LOGICAL, DIMENSION(4), INTENT(OUT)  :: bdy_mask
   INTEGER, INTENT(IN)   :: sd1 , ed1 , sd2 , ed2 , sd3 , ed3 , bdx , bdy
   INTEGER, INTENT(OUT)  :: sp1  , ep1  , sp2  , ep2  , sp3  , ep3  , &  
                            sm1  , em1  , sm2  , em2  , sm3  , em3
   INTEGER, INTENT(OUT)  :: sp1x , ep1x , sp2x , ep2x , sp3x , ep3x , &  
                            sm1x , em1x , sm2x , em2x , sm3x , em3x
   INTEGER, INTENT(OUT)  :: sp1y , ep1y , sp2y , ep2y , sp3y , ep3y , &  
                            sm1y , em1y , sm2y , em2y , sm3y , em3y
   INTEGER, INTENT(IN)   :: id , parent_id , parent_domdesc
   INTEGER, INTENT(INOUT)  :: domdesc
   TYPE(domain), POINTER :: parent



   INTEGER spec_bdy_width

   CALL nl_get_spec_bdy_width( 1, spec_bdy_width )















   CALL wrf_dm_patch_domain( id , domdesc , parent_id , parent_domdesc , &
                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &
                                         sp1x , ep1x , sm1x , em1x , &
                                         sp2x , ep2x , sm2x , em2x , &
                                         sp3x , ep3x , sm3x , em3x , &
                                         sp1y , ep1y , sm1y , em1y , &
                                         sp2y , ep2y , sm2y , em2y , &
                                         sp3y , ep3y , sm3y , em3y , &
                             bdx , bdy )

   SELECT CASE ( model_data_order )
      CASE ( DATA_ORDER_XYZ )
   bdy_mask( P_XSB ) = ( sd1                  <= sp1 .AND. sp1 <= sd1+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd2                  <= sp2 .AND. sp2 <= sd2+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed1-spec_bdy_width-1 <= ep1 .AND. ep1 <= ed1                  )
   bdy_mask( P_YEB ) = ( ed2-spec_bdy_width-1 <= ep2 .AND. ep2 <= ed2                  )
      CASE ( DATA_ORDER_YXZ )
   bdy_mask( P_XSB ) = ( sd2                  <= sp2 .AND. sp2 <= sd2+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd1                  <= sp1 .AND. sp1 <= sd1+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed2-spec_bdy_width-1 <= ep2 .AND. ep2 <= ed2                  )
   bdy_mask( P_YEB ) = ( ed1-spec_bdy_width-1 <= ep1 .AND. ep1 <= ed1                  )
      CASE ( DATA_ORDER_ZXY )
   bdy_mask( P_XSB ) = ( sd2                  <= sp2 .AND. sp2 <= sd2+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd3                  <= sp3 .AND. sp3 <= sd3+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed2-spec_bdy_width-1 <= ep2 .AND. ep2 <= ed2                  )
   bdy_mask( P_YEB ) = ( ed3-spec_bdy_width-1 <= ep3 .AND. ep3 <= ed3                  )
      CASE ( DATA_ORDER_ZYX )
   bdy_mask( P_XSB ) = ( sd3                  <= sp3 .AND. sp3 <= sd3+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd2                  <= sp2 .AND. sp2 <= sd2+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed3-spec_bdy_width-1 <= ep3 .AND. ep3 <= ed3                  )
   bdy_mask( P_YEB ) = ( ed2-spec_bdy_width-1 <= ep2 .AND. ep2 <= ed2                  )
      CASE ( DATA_ORDER_XZY )
   bdy_mask( P_XSB ) = ( sd1                  <= sp1 .AND. sp1 <= sd1+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd3                  <= sp3 .AND. sp3 <= sd3+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed1-spec_bdy_width-1 <= ep1 .AND. ep1 <= ed1                  )
   bdy_mask( P_YEB ) = ( ed3-spec_bdy_width-1 <= ep3 .AND. ep3 <= ed3                  )
      CASE ( DATA_ORDER_YZX )
   bdy_mask( P_XSB ) = ( sd3                  <= sp3 .AND. sp3 <= sd3+spec_bdy_width-1 )
   bdy_mask( P_YSB ) = ( sd1                  <= sp1 .AND. sp1 <= sd1+spec_bdy_width-1 )
   bdy_mask( P_XEB ) = ( ed3-spec_bdy_width-1 <= ep3 .AND. ep3 <= ed3                  )
   bdy_mask( P_YEB ) = ( ed1-spec_bdy_width-1 <= ep1 .AND. ep1 <= ed1                  )
   END SELECT


   RETURN
   END SUBROUTINE wrf_patch_domain

   SUBROUTINE alloc_and_configure_domain ( domain_id , active_this_task, grid , parent, kid )









































      IMPLICIT NONE

      

      INTEGER , INTENT(IN)            :: domain_id
      LOGICAL , OPTIONAL, INTENT(IN)  :: active_this_task 
      TYPE( domain ) , POINTER        :: grid
      TYPE( domain ) , POINTER        :: parent
      INTEGER , INTENT(IN)            :: kid    

      
      INTEGER                     :: sd1 , ed1 , sp1 , ep1 , sm1 , em1
      INTEGER                     :: sd2 , ed2 , sp2 , ep2 , sm2 , em2
      INTEGER                     :: sd3 , ed3 , sp3 , ep3 , sm3 , em3

      INTEGER                     :: sd1x , ed1x , sp1x , ep1x , sm1x , em1x
      INTEGER                     :: sd2x , ed2x , sp2x , ep2x , sm2x , em2x
      INTEGER                     :: sd3x , ed3x , sp3x , ep3x , sm3x , em3x

      INTEGER                     :: sd1y , ed1y , sp1y , ep1y , sm1y , em1y
      INTEGER                     :: sd2y , ed2y , sp2y , ep2y , sm2y , em2y
      INTEGER                     :: sd3y , ed3y , sp3y , ep3y , sm3y , em3y

      TYPE(domain) , POINTER      :: new_grid
      INTEGER                     :: i
      INTEGER                     :: parent_id , parent_domdesc , new_domdesc
      INTEGER                     :: bdyzone_x , bdyzone_y
      INTEGER                     :: nx, ny
      LOGICAL :: active


      active = .TRUE.
      IF ( PRESENT( active_this_task ) ) THEN
         active = active_this_task
      ENDIF






      data_ordering : SELECT CASE ( model_data_order )
        CASE  ( DATA_ORDER_XYZ )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_YXZ )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_vert( domain_id , sd3 )
          CALL nl_get_e_vert( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed1-sd1+1

        CASE  ( DATA_ORDER_ZXY )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_we( domain_id , sd2 )
          CALL nl_get_e_we( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed2-sd2+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_ZYX )

          CALL nl_get_s_vert( domain_id , sd1 )
          CALL nl_get_e_vert( domain_id , ed1 )
          CALL nl_get_s_sn( domain_id , sd2 )
          CALL nl_get_e_sn( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed2-sd2+1

        CASE  ( DATA_ORDER_XZY )

          CALL nl_get_s_we( domain_id , sd1 )
          CALL nl_get_e_we( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_sn( domain_id , sd3 )
          CALL nl_get_e_sn( domain_id , ed3 )
          nx = ed1-sd1+1
          ny = ed3-sd3+1

        CASE  ( DATA_ORDER_YZX )

          CALL nl_get_s_sn( domain_id , sd1 )
          CALL nl_get_e_sn( domain_id , ed1 )
          CALL nl_get_s_vert( domain_id , sd2 )
          CALL nl_get_e_vert( domain_id , ed2 )
          CALL nl_get_s_we( domain_id , sd3 )
          CALL nl_get_e_we( domain_id , ed3 )
          nx = ed3-sd3+1
          ny = ed1-sd1+1

      END SELECT data_ordering

      IF ( num_time_levels > 3 ) THEN
        WRITE ( wrf_err_message , * ) 'alloc_and_configure_domain: ', &
          'Incorrect value for num_time_levels ', num_time_levels
        CALL wrf_error_fatal3("<stdin>",614,&
TRIM ( wrf_err_message ) )
      ENDIF

      IF (ASSOCIATED(parent)) THEN
        parent_id = parent%id
        parent_domdesc = parent%domdesc
      ELSE
        parent_id = -1
        parent_domdesc = -1
      ENDIF


      CALL get_bdyzone_x( bdyzone_x )
      CALL get_bdyzone_y( bdyzone_y )

      ALLOCATE ( new_grid )
      ALLOCATE( new_grid%head_statevars )
      new_grid%head_statevars%Ndim = 0
      NULLIFY( new_grid%head_statevars%next)
      new_grid%tail_statevars => new_grid%head_statevars 

      ALLOCATE ( new_grid%parents( max_parents ) ) 
      ALLOCATE ( new_grid%nests( max_nests ) )
      NULLIFY( new_grid%sibling )
      DO i = 1, max_nests
         NULLIFY( new_grid%nests(i)%ptr )
      ENDDO
      NULLIFY  (new_grid%next)
      NULLIFY  (new_grid%same_level)
      NULLIFY  (new_grid%i_start)
      NULLIFY  (new_grid%j_start)
      NULLIFY  (new_grid%i_end)
      NULLIFY  (new_grid%j_end)
      ALLOCATE( new_grid%domain_clock )
      new_grid%domain_clock_created = .FALSE.
      ALLOCATE( new_grid%alarms( MAX_WRF_ALARMS ) )    
      ALLOCATE( new_grid%alarms_created( MAX_WRF_ALARMS ) )
      DO i = 1, MAX_WRF_ALARMS
        new_grid%alarms_created( i ) = .FALSE.
      ENDDO
      new_grid%time_set = .FALSE.
      new_grid%is_intermediate = .FALSE.
      new_grid%have_displayed_alloc_stats = .FALSE.

      new_grid%tiling_latch = .FALSE.  

      
      
      
      
      

 
      IF ( domain_id .NE. 1 ) THEN
         new_grid%parents(1)%ptr => parent
         new_grid%num_parents = 1
         parent%nests(kid)%ptr => new_grid
         new_grid%child_of_parent(1) = kid    
         parent%num_nests = parent%num_nests + 1
      END IF
      new_grid%id = domain_id                 
      new_grid%active_this_task = active

      CALL wrf_patch_domain( domain_id  , new_domdesc , parent, parent_id, parent_domdesc , &

                             sd1 , ed1 , sp1 , ep1 , sm1 , em1 , &     
                             sd2 , ed2 , sp2 , ep2 , sm2 , em2 , &     
                             sd3 , ed3 , sp3 , ep3 , sm3 , em3 , &

                                     sp1x , ep1x , sm1x , em1x , &     
                                     sp2x , ep2x , sm2x , em2x , &
                                     sp3x , ep3x , sm3x , em3x , &

                                     sp1y , ep1y , sm1y , em1y , &     
                                     sp2y , ep2y , sm2y , em2y , &
                                     sp3y , ep3y , sm3y , em3y , &

                         bdyzone_x  , bdyzone_y , new_grid%bdy_mask &
      ) 


      new_grid%domdesc = new_domdesc
      new_grid%num_nests = 0
      new_grid%num_siblings = 0
      new_grid%num_parents = 0
      new_grid%max_tiles   = 0
      new_grid%num_tiles_spec   = 0
      new_grid%nframes   = 0         









        
      new_grid%active_this_task = active
      CALL alloc_space_field ( new_grid, domain_id , 3 , 3 , .FALSE. , active,     &
                               sd1, ed1, sd2, ed2, sd3, ed3,       &
                               sm1,  em1,  sm2,  em2,  sm3,  em3,  &
                               sp1,  ep1,  sp2,  ep2,  sp3,  ep3,  &
                               sp1x, ep1x, sp2x, ep2x, sp3x, ep3x, &
                               sp1y, ep1y, sp2y, ep2y, sp3y, ep3y, &
                               sm1x, em1x, sm2x, em2x, sm3x, em3x, &   
                               sm1y, em1y, sm2y, em2y, sm3y, em3y  &   
      )







      new_grid%stepping_to_time = .FALSE.
      new_grid%adaptation_domain = 1
      new_grid%last_step_updated = -1



      new_grid%sd31                            = sd1 
      new_grid%ed31                            = ed1
      new_grid%sp31                            = sp1 
      new_grid%ep31                            = ep1 
      new_grid%sm31                            = sm1 
      new_grid%em31                            = em1
      new_grid%sd32                            = sd2 
      new_grid%ed32                            = ed2
      new_grid%sp32                            = sp2 
      new_grid%ep32                            = ep2 
      new_grid%sm32                            = sm2 
      new_grid%em32                            = em2
      new_grid%sd33                            = sd3 
      new_grid%ed33                            = ed3
      new_grid%sp33                            = sp3 
      new_grid%ep33                            = ep3 
      new_grid%sm33                            = sm3 
      new_grid%em33                            = em3

      new_grid%sp31x                           = sp1x
      new_grid%ep31x                           = ep1x
      new_grid%sm31x                           = sm1x
      new_grid%em31x                           = em1x
      new_grid%sp32x                           = sp2x
      new_grid%ep32x                           = ep2x
      new_grid%sm32x                           = sm2x
      new_grid%em32x                           = em2x
      new_grid%sp33x                           = sp3x
      new_grid%ep33x                           = ep3x
      new_grid%sm33x                           = sm3x
      new_grid%em33x                           = em3x

      new_grid%sp31y                           = sp1y
      new_grid%ep31y                           = ep1y
      new_grid%sm31y                           = sm1y
      new_grid%em31y                           = em1y
      new_grid%sp32y                           = sp2y
      new_grid%ep32y                           = ep2y
      new_grid%sm32y                           = sm2y
      new_grid%em32y                           = em2y
      new_grid%sp33y                           = sp3y
      new_grid%ep33y                           = ep3y
      new_grid%sm33y                           = sm3y
      new_grid%em33y                           = em3y

      SELECT CASE ( model_data_order )
         CASE  ( DATA_ORDER_XYZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YXZ )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd2 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed2 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp2 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep2 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm2 ;
            new_grid%em21 = em1 ; new_grid%em22 = em2 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_ZXY )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_ZYX )
            new_grid%sd21 = sd2 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed2 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp2 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep2 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm2 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em2 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd2
            new_grid%ed11 = ed2
            new_grid%sp11 = sp2
            new_grid%ep11 = ep2
            new_grid%sm11 = sm2
            new_grid%em11 = em2
         CASE  ( DATA_ORDER_XZY )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
         CASE  ( DATA_ORDER_YZX )
            new_grid%sd21 = sd1 ; new_grid%sd22 = sd3 ;
            new_grid%ed21 = ed1 ; new_grid%ed22 = ed3 ;
            new_grid%sp21 = sp1 ; new_grid%sp22 = sp3 ;
            new_grid%ep21 = ep1 ; new_grid%ep22 = ep3 ;
            new_grid%sm21 = sm1 ; new_grid%sm22 = sm3 ;
            new_grid%em21 = em1 ; new_grid%em22 = em3 ;
            new_grid%sd11 = sd1
            new_grid%ed11 = ed1
            new_grid%sp11 = sp1
            new_grid%ep11 = ep1
            new_grid%sm11 = sm1
            new_grid%em11 = em1
      END SELECT

      CALL med_add_config_info_to_grid ( new_grid )           



      new_grid%tiled                           = .false.
      new_grid%patched                         = .false.
      NULLIFY(new_grid%mapping)




      grid => new_grid

 

      IF ( grid%active_this_task ) THEN

        ALLOCATE( grid%lattsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%lontsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%nametsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%desctsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%itsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%jtsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%id_tsloc( grid%max_ts_locs ) )
        ALLOCATE( grid%ts_filename( grid%max_ts_locs ) )
        grid%ntsloc        = 0
        grid%ntsloc_domain = 0


        ALLOCATE( grid%track_time_in( grid%track_loc_in ) )
        ALLOCATE( grid%track_lat_in( grid%track_loc_in ) )
        ALLOCATE( grid%track_lon_in( grid%track_loc_in ) )
  
        ALLOCATE( grid%track_time_domain( grid%track_loc_in ) )
        ALLOCATE( grid%track_lat_domain( grid%track_loc_in ) )
        ALLOCATE( grid%track_lon_domain( grid%track_loc_in ) )
        ALLOCATE( grid%track_i( grid%track_loc_in ) )
        ALLOCATE( grid%track_j( grid%track_loc_in ) )

      grid%track_loc        = 0
      grid%track_loc_domain = 0
      grid%track_have_calculated = .FALSE.
      grid%track_have_input      = .FALSE.
      ELSE
        WRITE (wrf_err_message,*)"Not allocating time series storage for domain ",domain_id," on this set of tasks"
        CALL wrf_message(TRIM(wrf_err_message))
      ENDIF

      CALL wrf_get_dm_communicator_for_id( grid%id, grid%communicator )
      CALL wrf_dm_define_comms( grid )

      grid%interp_mp = .true.

   END SUBROUTINE alloc_and_configure_domain

   SUBROUTINE get_fieldstr(ix,c,instr,outstr,noutstr,noerr)
     IMPLICIT NONE
     INTEGER, INTENT(IN)          :: ix
     CHARACTER*(*), INTENT(IN)    :: c
     CHARACTER*(*), INTENT(IN)    :: instr
     CHARACTER*(*), INTENT(OUT)   :: outstr
     INTEGER,       INTENT(IN)    :: noutstr  
     LOGICAL,       INTENT(INOUT) :: noerr     
     
     INTEGER, PARAMETER :: MAX_DEXES = 100
     INTEGER I, PREV, IDEX
     INTEGER DEXES(MAX_DEXES)
     outstr = ""
     prev = 1
     dexes(1) = 1
     DO i = 2,MAX_DEXES
       idex = INDEX(instr(prev:LEN(TRIM(instr))),c)
       IF ( idex .GT. 0 ) THEN
         dexes(i) = idex+prev
         prev = dexes(i)+1
       ELSE
         dexes(i) = LEN(TRIM(instr))+2
       ENDIF
     ENDDO

     IF     ( (dexes(ix+1)-2)-(dexes(ix)) .GT. noutstr ) THEN
       noerr = .FALSE.  
     ELSE IF( dexes(ix) .EQ. dexes(ix+1) ) THEN 
       noerr = .FALSE.  
     ELSE
       outstr = instr(dexes(ix):(dexes(ix+1)-2))
       noerr = noerr .AND. .TRUE.
     ENDIF
   END SUBROUTINE get_fieldstr

   SUBROUTINE change_to_lower_case(instr,outstr)
     CHARACTER*(*) ,INTENT(IN)  :: instr
     CHARACTER*(*) ,INTENT(OUT) :: outstr

     CHARACTER*1                :: c
     INTEGER       ,PARAMETER   :: upper_to_lower =IACHAR('a')-IACHAR('A')
     INTEGER                    :: i,n,n1

     outstr = ' '
     N = len(instr)
     N1 = len(outstr)
     N = MIN(N,N1)
     outstr(1:N) = instr(1:N)
     DO i=1,N
       c = instr(i:i)
       if('A'<=c .and. c <='Z') outstr(i:i)=achar(iachar(c)+upper_to_lower)
     ENDDO
     RETURN
   END SUBROUTINE change_to_lower_case


   SUBROUTINE modify_io_masks1 ( grid , id )
      IMPLICIT NONE

      INTEGER              , INTENT(IN  )  :: id
      TYPE(domain), POINTER                :: grid
      
      TYPE(fieldlist), POINTER :: p, q
      INTEGER, PARAMETER :: read_unit = 10
      LOGICAL, EXTERNAL  :: wrf_dm_on_monitor
      CHARACTER*256      :: fname, inln, mess, dname, t1, lookee
      CHARACTER*256      :: fieldlst
      CHARACTER*1        :: op, strmtyp
      CHARACTER*3        :: strmid
      CHARACTER*10       :: strmtyp_name
      INTEGER            :: io_status
      INTEGER            :: strmtyp_int, count_em
      INTEGER            :: lineno, fieldno, istrm, retval, itrace
      LOGICAL            :: keepgoing, noerr, gavewarning, ignorewarning, found
      LOGICAL, SAVE      :: you_warned_me = .FALSE.
      LOGICAL, SAVE      :: you_warned_me2(max_hst_mods,max_domains) = .FALSE.

      gavewarning = .FALSE.

      CALL nl_get_iofields_filename( id, fname )

      IF ( grid%is_intermediate ) RETURN                
      IF ( TRIM(fname) .EQ. "NONE_SPECIFIED" ) RETURN   

      IF ( wrf_dm_on_monitor() ) THEN
        OPEN ( UNIT   = read_unit    ,      &
               FILE   = TRIM(fname)      ,      &
               FORM   = "FORMATTED"      ,      &
               STATUS = "OLD"            ,      &
               IOSTAT = io_status         )
        IF ( io_status .EQ. 0 ) THEN   
          keepgoing = .TRUE.
          lineno = 0
          count_em = 0    
          DO WHILE ( keepgoing )
            READ(UNIT=read_unit,FMT='(A)',IOSTAT=io_status) inln
            keepgoing = (io_status .EQ. 0) .AND. (LEN(TRIM(inln)) .GT. 0)  
            IF ( keepgoing ) THEN
              lineno = lineno + 1
              IF ( .NOT. LEN(TRIM(inln)) .LT. LEN(inln) ) THEN
                WRITE(mess,*)'W A R N I N G : Line ',lineno,' of ',TRIM(fname),' is too long. Limit is ',LEN(inln),' characters.' 
                gavewarning = .TRUE.
              ENDIF
              IF ( INDEX(inln,'#') .EQ. 0 ) THEN   
                IF ( keepgoing ) THEN
                  noerr = .TRUE.
                  CALL get_fieldstr(1,':',inln,op,1,noerr)          
                  IF ( TRIM(op) .NE. '+' .AND. TRIM(op) .NE. '-' ) THEN
                    WRITE(mess,*)'W A R N I N G : unknown operation ',TRIM(op),' (should be + or -). Line ',lineno
                    gavewarning = .TRUE.
                  ENDIF
                  CALL get_fieldstr(2,':',inln,t1,1,noerr)          
                  CALL change_to_lower_case(t1,strmtyp) 

                  SELECT CASE (TRIM(strmtyp))
                  CASE ('h')
                     strmtyp_name = 'history'
                     strmtyp_int  = first_history
                  CASE ('i')
                     strmtyp_name = 'input'
                     strmtyp_int  = first_input
                  CASE DEFAULT
                     WRITE(mess,*)'W A R N I N G : unknown stream type ',TRIM(strmtyp),'. Line ',lineno
                     gavewarning = .TRUE.
                  END SELECT

                  CALL get_fieldstr(3,':',inln,strmid,3,noerr)      
                  READ(strmid,'(I3)') istrm
                  IF ( istrm .LT. 0 .OR. istrm .GT. last_history ) THEN
                    WRITE(mess,*)'W A R N I N G : invalid stream id ',istrm,' (should be 0 <= id <= ',last_history,'). Line ',lineno
                    gavewarning = .TRUE.
                  ENDIF
                  CALL get_fieldstr(4,':',inln,fieldlst,1024,noerr) 
                  IF ( noerr ) THEN
                    fieldno = 1
                    CALL get_fieldstr(fieldno,',',fieldlst,t1,256,noerr)
                    CALL change_to_lower_case(t1,lookee)
                    DO WHILE ( noerr )    
                      p => grid%head_statevars%next
                      found = .FALSE.
                      count_em = count_em + 1
                      DO WHILE ( ASSOCIATED( p ) )
  
                        IF ( p%Ndim .EQ. 4 .AND. p%scalar_array ) THEN
  
                          DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                            CALL change_to_lower_case( p%dname_table( grid%id, itrace ) , dname ) 

                            IF ( TRIM(dname) .EQ. TRIM(lookee) ) &
                            CALL warn_me_or_set_mask (id, istrm, lineno, strmtyp_int, count_em, op, &
                                                      strmtyp_name, dname, fname, lookee,      &
                                                      p%streams_table(grid%id,itrace)%stream,  &
                                                      mess, found, you_warned_me2)
                          ENDDO
                        ELSE 
                          IF ( p%Ntl .GT. 0 ) THEN
                            CALL change_to_lower_case(p%DataName(1:LEN(TRIM(p%DataName))-2),dname)
                          ELSE
                            CALL change_to_lower_case(p%DataName,dname)
                          ENDIF
  
                          IF ( TRIM(dname) .EQ. TRIM(lookee) ) &
                          CALL warn_me_or_set_mask (id, istrm, lineno, strmtyp_int, count_em, op, &
                                                    strmtyp_name, dname, fname, lookee,      &
                                                    p%streams, mess, found, you_warned_me2)
                        ENDIF
                        p => p%next
                      ENDDO
                      IF ( .NOT. found ) THEN
                        WRITE(mess,*)'W A R N I N G : Unable to modify mask for ',TRIM(lookee),&
                                     '.  Variable not found. File: ',TRIM(fname),' at line ',lineno
                        CALL wrf_message(mess)
                        gavewarning = .TRUE.
                      ENDIF
                      fieldno = fieldno + 1
                      CALL get_fieldstr(fieldno,',',fieldlst,t1,256,noerr)
                      CALL change_to_lower_case(t1,lookee)
                    ENDDO
                  ELSE
                    WRITE(mess,*)'W A R N I N G : Problem reading ',TRIM(fname),' at line ',lineno
                    CALL wrf_message(mess)
                    gavewarning = .TRUE.
                  ENDIF
                ENDIF  
              ENDIF    
            ENDIF      
          ENDDO
        ELSE
          WRITE(mess,*)'W A R N I N G : Problem opening ',TRIM(fname)
          CALL wrf_message(mess)
          gavewarning = .TRUE.
        ENDIF
        CLOSE( read_unit )
        IF ( gavewarning ) THEN
          CALL nl_get_ignore_iofields_warning(1,ignorewarning)
          IF ( .NOT. ignorewarning ) THEN
            CALL wrf_message(mess)
            WRITE(mess,*)'modify_io_masks: problems reading ',TRIM(fname) 
            CALL wrf_message(mess)
            CALL wrf_error_fatal3("<stdin>",1116,&
'Set ignore_iofields_warn to true in namelist to ignore')
          ELSE
            IF ( .NOT. you_warned_me ) THEN
              if ( .NOT. you_warned_me2(count_em,id) ) CALL wrf_message(mess)  
              WRITE(mess,*)'Ignoring problems reading ',TRIM(fname) 
              CALL wrf_message(mess)
              CALL wrf_message('Continuing.  To make this a fatal error, set ignore_iofields_warn to false in namelist' )
              CALL wrf_message(' ')
              you_warned_me = .TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF  


      p => grid%head_statevars%next
      DO WHILE ( ASSOCIATED( p ) )
        IF ( p%Ndim .EQ. 4 .AND. p%scalar_array ) THEN

          DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
            CALL wrf_dm_bcast_integer( p%streams_table(grid%id,itrace)%stream, (((2*(25)+2))/(4*8)+1) )
          ENDDO

        ELSE
          CALL wrf_dm_bcast_integer( p%streams, (((2*(25)+2))/(4*8)+1) )
        ENDIF
        p => p%next
      ENDDO
      
   END SUBROUTINE modify_io_masks1

   SUBROUTINE warn_me_or_set_mask (id, istrm, lineno, strmtyp_int, count_em, op, &
                                   strmtyp_name, dname, fname, lookee,      &
                                   p_stream, mess, found, you_warned_me2)

      IMPLICIT NONE






     INTEGER,       INTENT(IN )   :: id, istrm, lineno, strmtyp_int
     INTEGER,       INTENT(IN )   :: p_stream(*), count_em
     CHARACTER*1,   INTENT(IN )   :: op
     CHARACTER*10,  INTENT(IN )   :: strmtyp_name
     CHARACTER*256, INTENT(IN )   :: dname, fname, lookee
     CHARACTER*256, INTENT(OUT)   :: mess
     LOGICAL,       INTENT(OUT)   :: found
     LOGICAL,       INTENT(INOUT) :: you_warned_me2(max_hst_mods,max_domains)
   
     INTEGER                      :: retval

     found = .TRUE.
     IF      ( TRIM(op) .EQ. '+' ) THEN
       CALL get_mask( p_stream, strmtyp_int + istrm - 1, retval )
       IF ( retval .NE. 0 ) THEN
         WRITE(mess,*) 'Domain ',id, ' W A R N I N G : Variable ',TRIM(lookee),' already on ', &
                       TRIM(strmtyp_name), ' stream ',istrm, '.  File: ', TRIM(fname),' at line ',lineno
       ELSE
         WRITE(mess,*) 'Domain ', id, ' Setting ', TRIM(strmtyp_name), ' stream ',istrm,' for ', &
                                  TRIM(DNAME)  ; CALL wrf_debug(1,mess)
         CALL set_mask( p_stream, strmtyp_int + istrm - 1 )
       ENDIF
     ELSE IF ( TRIM(op) .EQ. '-' ) THEN
       CALL get_mask( p_stream, strmtyp_int + istrm - 1, retval )
       IF ( retval .EQ. 0 ) THEN
         WRITE(mess,*) 'Domain ',id, ' W A R N I N G : Variable ',TRIM(lookee),' already off ', &
                       TRIM(strmtyp_name), ' stream ',istrm, '. File: ',TRIM(fname),' at line ',lineno
       ELSE
         WRITE(mess,*) 'Domain ', id, ' Resetting ', TRIM(strmtyp_name), ' stream ',istrm,' for ', &
                                    TRIM(DNAME)  ; CALL wrf_debug(1,mess) 
         CALL reset_mask( p_stream, strmtyp_int + istrm - 1)
       ENDIF
     ENDIF
     IF ( count_em > max_hst_mods ) THEN
       WRITE(mess,*)'ERROR module_domain:  Array size for you_warned_me2 is fixed at ',max_hst_mods
       CALL wrf_message(mess)
       CALL wrf_error_fatal3("<stdin>",1195,&
'Did you really type > max_hst_mods fields into ', TRIM(fname) ,' ?')
     ELSE
       IF ( .NOT. you_warned_me2(count_em,id) ) THEN
         CALL wrf_message(mess)     
         you_warned_me2(count_em,id) = .TRUE.
       ENDIF
     ENDIF

   END SUBROUTINE warn_me_or_set_mask 







   SUBROUTINE alloc_space_field ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in,  &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      USE module_alloc_space_0, ONLY : alloc_space_field_core_0
      USE module_alloc_space_1, ONLY : alloc_space_field_core_1
      USE module_alloc_space_2, ONLY : alloc_space_field_core_2
      USE module_alloc_space_3, ONLY : alloc_space_field_core_3
      USE module_alloc_space_4, ONLY : alloc_space_field_core_4
      USE module_alloc_space_5, ONLY : alloc_space_field_core_5
      USE module_alloc_space_6, ONLY : alloc_space_field_core_6
      USE module_alloc_space_7, ONLY : alloc_space_field_core_7
      USE module_alloc_space_8, ONLY : alloc_space_field_core_8
      USE module_alloc_space_9, ONLY : alloc_space_field_core_9

      IMPLICIT NONE

      

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval_in   
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sp31, ep31, sp32, ep32, sp33, ep33
      INTEGER , INTENT(IN)            :: sp31x, ep31x, sp32x, ep32x, sp33x, ep33x
      INTEGER , INTENT(IN)            :: sp31y, ep31y, sp32y, ep32y, sp33y, ep33y
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      
      
      
      
      INTEGER , INTENT(IN)            :: tl_in
  
      
      
      LOGICAL , INTENT(IN)            :: inter_domain_in, okay_to_alloc_in

      
      INTEGER(KIND=8)  num_bytes_allocated
      INTEGER  idum1, idum2

      IF ( grid%id .EQ. 1 ) CALL wrf_message ( &
          'DYNAMICS OPTION: Eulerian Mass Coordinate ')

      CALL set_scalar_indices_from_config( id , idum1 , idum2 )

      num_bytes_allocated = 0 

      
      CALL alloc_space_field_core_0 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated , &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_1 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_2 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_3 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_4 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_5 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_6 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_7 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_8 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )
      CALL alloc_space_field_core_9 ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in, num_bytes_allocated ,  &
                                    sd31, ed31, sd32, ed32, sd33, ed33, &
                                    sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                    sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                    sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                    sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                    sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                    sm31y, em31y, sm32y, em32y, sm33y, em33y )

      IF ( .NOT. grid%have_displayed_alloc_stats ) THEN
        
        
        WRITE(wrf_err_message,*)&
            'alloc_space_field: domain ',id,', ',num_bytes_allocated,' bytes allocated'
        CALL  wrf_debug( 0, wrf_err_message )
        grid%have_displayed_alloc_stats = .TRUE.   
      ENDIF


      grid%alloced_sd31=sd31
      grid%alloced_ed31=ed31
      grid%alloced_sd32=sd32
      grid%alloced_ed32=ed32
      grid%alloced_sd33=sd33
      grid%alloced_ed33=ed33
      grid%alloced_sm31=sm31
      grid%alloced_em31=em31
      grid%alloced_sm32=sm32
      grid%alloced_em32=em32
      grid%alloced_sm33=sm33
      grid%alloced_em33=em33
      grid%alloced_sm31x=sm31x
      grid%alloced_em31x=em31x
      grid%alloced_sm32x=sm32x
      grid%alloced_em32x=em32x
      grid%alloced_sm33x=sm33x
      grid%alloced_em33x=em33x
      grid%alloced_sm31y=sm31y
      grid%alloced_em31y=em31y
      grid%alloced_sm32y=sm32y
      grid%alloced_em32y=em32y
      grid%alloced_sm33y=sm33y
      grid%alloced_em33y=em33y

      grid%allocated=.TRUE.

   END SUBROUTINE alloc_space_field

   
   
   
   
   

   SUBROUTINE ensure_space_field ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in,  &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      IMPLICIT NONE

      

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval_in   
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sp31, ep31, sp32, ep32, sp33, ep33
      INTEGER , INTENT(IN)            :: sp31x, ep31x, sp32x, ep32x, sp33x, ep33x
      INTEGER , INTENT(IN)            :: sp31y, ep31y, sp32y, ep32y, sp33y, ep33y
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      
      
      
      
      INTEGER , INTENT(IN)            :: tl_in
  
      
      
      LOGICAL , INTENT(IN)            :: inter_domain_in, okay_to_alloc_in
      LOGICAL                         :: size_changed

      size_changed=         .not. ( &
         grid%alloced_sd31 .eq. sd31 .and. grid%alloced_ed31 .eq. ed31 .and. &
         grid%alloced_sd32 .eq. sd32 .and. grid%alloced_ed32 .eq. ed32 .and. &
         grid%alloced_sd33 .eq. sd33 .and. grid%alloced_ed33 .eq. ed33 .and. &
         grid%alloced_sm31 .eq. sm31 .and. grid%alloced_em31 .eq. em31 .and. &
         grid%alloced_sm32 .eq. sm32 .and. grid%alloced_em32 .eq. em32 .and. &
         grid%alloced_sm33 .eq. sm33 .and. grid%alloced_em33 .eq. em33 .and. &
         grid%alloced_sm31x .eq. sm31x .and. grid%alloced_em31x .eq. em31x .and. &
         grid%alloced_sm32x .eq. sm32x .and. grid%alloced_em32x .eq. em32x .and. &
         grid%alloced_sm33x .eq. sm33x .and. grid%alloced_em33x .eq. em33x .and. &
         grid%alloced_sm31y .eq. sm31y .and. grid%alloced_em31y .eq. em31y .and. &
         grid%alloced_sm32y .eq. sm32y .and. grid%alloced_em32y .eq. em32y .and. &
         grid%alloced_sm33y .eq. sm33y .and. grid%alloced_em33y .eq. em33y &
      )
      if(.not. grid%allocated .or. size_changed) then
         if(.not. grid%allocated) then
            call wrf_debug(1,'ensure_space_field: calling alloc_space_field because a grid was not allocated.')
         else
            if(size_changed) &
                 call wrf_debug(1,'ensure_space_field: deallocating and reallocating a grid because grid size changed.')
         end if
         if(grid%allocated) &
              call dealloc_space_field( grid )
         call alloc_space_field ( grid,   id, setinitval_in ,  tl_in , inter_domain_in , okay_to_alloc_in,  &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sp31 , ep31 , sp32 , ep32 , sp33 , ep33 , &
                                  sp31x, ep31x, sp32x, ep32x, sp33x, ep33x, &
                                  sp31y, ep31y, sp32y, ep32y, sp33y, ep33y, &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )
      end if

   END SUBROUTINE ensure_space_field






   SUBROUTINE dealloc_space_domain ( id )
      
      IMPLICIT NONE

      

      INTEGER , INTENT(IN)            :: id

      

      TYPE(domain) , POINTER          :: grid
      LOGICAL                         :: found

      

      grid => head_grid
      old_grid => head_grid
      found = .FALSE.

      
      
      

      find_grid : DO WHILE ( ASSOCIATED(grid) ) 
         IF ( grid%id == id ) THEN
            found = .TRUE.
            old_grid%next => grid%next
            CALL domain_destroy( grid )
            EXIT find_grid
         END IF
         old_grid => grid
         grid     => grid%next
      END DO find_grid

      IF ( .NOT. found ) THEN
         WRITE ( wrf_err_message , * ) 'module_domain: ', &
           'dealloc_space_domain: Could not de-allocate grid id ',id
         CALL wrf_error_fatal3("<stdin>",1507,&
TRIM( wrf_err_message ) ) 
      END IF

   END SUBROUTINE dealloc_space_domain








   SUBROUTINE domain_destroy ( grid )
      
      IMPLICIT NONE

      

      TYPE(domain) , POINTER          :: grid

      CALL dealloc_space_field ( grid )
      CALL dealloc_linked_lists( grid )
      DEALLOCATE( grid%parents )
      DEALLOCATE( grid%nests )
      
      CALL domain_clock_destroy( grid )
      CALL domain_alarms_destroy( grid )
      IF ( ASSOCIATED( grid%i_start ) ) THEN
        DEALLOCATE( grid%i_start ) 
      ENDIF
      IF ( ASSOCIATED( grid%i_end ) ) THEN
        DEALLOCATE( grid%i_end )
      ENDIF
      IF ( ASSOCIATED( grid%j_start ) ) THEN
        DEALLOCATE( grid%j_start )
      ENDIF
      IF ( ASSOCIATED( grid%j_end ) ) THEN
        DEALLOCATE( grid%j_end )
      ENDIF
      IF ( ASSOCIATED( grid%itsloc ) ) THEN
        DEALLOCATE( grid%itsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%jtsloc ) ) THEN
        DEALLOCATE( grid%jtsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%id_tsloc ) ) THEN
        DEALLOCATE( grid%id_tsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%lattsloc ) ) THEN
        DEALLOCATE( grid%lattsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%lontsloc ) ) THEN
        DEALLOCATE( grid%lontsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%nametsloc ) ) THEN
        DEALLOCATE( grid%nametsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%desctsloc ) ) THEN
        DEALLOCATE( grid%desctsloc )
      ENDIF 
      IF ( ASSOCIATED( grid%ts_filename ) ) THEN
        DEALLOCATE( grid%ts_filename )
      ENDIF 
      IF ( ASSOCIATED( grid%track_time_in ) ) THEN
        DEALLOCATE( grid%track_time_in )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lat_in ) ) THEN
        DEALLOCATE( grid%track_lat_in )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lon_in ) ) THEN
        DEALLOCATE( grid%track_lon_in )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_i ) ) THEN
        DEALLOCATE( grid%track_i )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_j ) ) THEN
        DEALLOCATE( grid%track_j )
      ENDIF

      IF ( ASSOCIATED( grid%track_time_domain ) ) THEN
        DEALLOCATE( grid%track_time_domain )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lat_domain ) ) THEN
        DEALLOCATE( grid%track_lat_domain )
      ENDIF
 
      IF ( ASSOCIATED( grid%track_lon_domain ) ) THEN
        DEALLOCATE( grid%track_lon_domain )
      ENDIF
      DEALLOCATE( grid )
      NULLIFY( grid )

   END SUBROUTINE domain_destroy

   SUBROUTINE dealloc_linked_lists ( grid )
      IMPLICIT NONE
      TYPE(domain), POINTER :: grid
      TYPE(fieldlist), POINTER :: p, q
      p => grid%head_statevars
      DO WHILE ( ASSOCIATED( p ) )
         q => p ; p => p%next ; DEALLOCATE(q)
      ENDDO
      NULLIFY(grid%head_statevars) ; NULLIFY( grid%tail_statevars)
      IF ( .NOT. grid%is_intermediate ) THEN
        ALLOCATE( grid%head_statevars )
        NULLIFY( grid%head_statevars%next)
        grid%tail_statevars => grid%head_statevars
      ENDIF
   END SUBROUTINE dealloc_linked_lists

   RECURSIVE SUBROUTINE show_nest_subtree ( grid )
      TYPE(domain), POINTER :: grid
      INTEGER myid
      INTEGER kid
      IF ( .NOT. ASSOCIATED( grid ) ) RETURN
      myid = grid%id
      DO kid = 1, max_nests
        IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
          IF ( grid%nests(kid)%ptr%id .EQ. myid ) THEN
            CALL wrf_error_fatal3("<stdin>",1632,&
'show_nest_subtree: nest hierarchy corrupted' )
          ENDIF
          CALL show_nest_subtree( grid%nests(kid)%ptr )
        ENDIF
      ENDDO
   END SUBROUTINE show_nest_subtree
   







   SUBROUTINE dealloc_space_field ( grid )
      
      IMPLICIT NONE

      

      TYPE(domain)              , POINTER :: grid

      

      INTEGER                             ::  ierr







IF ( ASSOCIATED( grid%xlat ) ) THEN 
  DEALLOCATE(grid%xlat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1668,&
'frame/module_domain.f: Failed to deallocate grid%xlat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlong ) ) THEN 
  DEALLOCATE(grid%xlong,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1675,&
'frame/module_domain.f: Failed to deallocate grid%xlong. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lu_index ) ) THEN 
  DEALLOCATE(grid%lu_index,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1682,&
'frame/module_domain.f: Failed to deallocate grid%lu_index. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lu_mask ) ) THEN 
  DEALLOCATE(grid%lu_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1689,&
'frame/module_domain.f: Failed to deallocate grid%lu_mask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%znu ) ) THEN 
  DEALLOCATE(grid%znu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1696,&
'frame/module_domain.f: Failed to deallocate grid%znu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%znw ) ) THEN 
  DEALLOCATE(grid%znw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1703,&
'frame/module_domain.f: Failed to deallocate grid%znw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zs ) ) THEN 
  DEALLOCATE(grid%zs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1710,&
'frame/module_domain.f: Failed to deallocate grid%zs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzs ) ) THEN 
  DEALLOCATE(grid%dzs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1717,&
'frame/module_domain.f: Failed to deallocate grid%dzs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traj_i ) ) THEN 
  DEALLOCATE(grid%traj_i,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1724,&
'frame/module_domain.f: Failed to deallocate grid%traj_i. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traj_j ) ) THEN 
  DEALLOCATE(grid%traj_j,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1731,&
'frame/module_domain.f: Failed to deallocate grid%traj_j. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traj_k ) ) THEN 
  DEALLOCATE(grid%traj_k,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1738,&
'frame/module_domain.f: Failed to deallocate grid%traj_k. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traj_long ) ) THEN 
  DEALLOCATE(grid%traj_long,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1745,&
'frame/module_domain.f: Failed to deallocate grid%traj_long. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traj_lat ) ) THEN 
  DEALLOCATE(grid%traj_lat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1752,&
'frame/module_domain.f: Failed to deallocate grid%traj_lat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_gc ) ) THEN 
  DEALLOCATE(grid%u_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1759,&
'frame/module_domain.f: Failed to deallocate grid%u_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_gc ) ) THEN 
  DEALLOCATE(grid%v_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1766,&
'frame/module_domain.f: Failed to deallocate grid%v_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_gc ) ) THEN 
  DEALLOCATE(grid%t_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1773,&
'frame/module_domain.f: Failed to deallocate grid%t_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_gc ) ) THEN 
  DEALLOCATE(grid%rh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1780,&
'frame/module_domain.f: Failed to deallocate grid%rh_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ght_gc ) ) THEN 
  DEALLOCATE(grid%ght_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1787,&
'frame/module_domain.f: Failed to deallocate grid%ght_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_gc ) ) THEN 
  DEALLOCATE(grid%p_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1794,&
'frame/module_domain.f: Failed to deallocate grid%p_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%prho_gc ) ) THEN 
  DEALLOCATE(grid%prho_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1801,&
'frame/module_domain.f: Failed to deallocate grid%prho_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlat_gc ) ) THEN 
  DEALLOCATE(grid%xlat_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1808,&
'frame/module_domain.f: Failed to deallocate grid%xlat_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlong_gc ) ) THEN 
  DEALLOCATE(grid%xlong_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1815,&
'frame/module_domain.f: Failed to deallocate grid%xlong_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_gc ) ) THEN 
  DEALLOCATE(grid%ht_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1822,&
'frame/module_domain.f: Failed to deallocate grid%ht_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%var_sso ) ) THEN 
  DEALLOCATE(grid%var_sso,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1829,&
'frame/module_domain.f: Failed to deallocate grid%var_sso. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lap_hgt ) ) THEN 
  DEALLOCATE(grid%lap_hgt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1836,&
'frame/module_domain.f: Failed to deallocate grid%lap_hgt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_gc ) ) THEN 
  DEALLOCATE(grid%tsk_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1843,&
'frame/module_domain.f: Failed to deallocate grid%tsk_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tavgsfc ) ) THEN 
  DEALLOCATE(grid%tavgsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1850,&
'frame/module_domain.f: Failed to deallocate grid%tavgsfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tmn_gc ) ) THEN 
  DEALLOCATE(grid%tmn_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1857,&
'frame/module_domain.f: Failed to deallocate grid%tmn_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pslv_gc ) ) THEN 
  DEALLOCATE(grid%pslv_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1864,&
'frame/module_domain.f: Failed to deallocate grid%pslv_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sct_dom_gc ) ) THEN 
  DEALLOCATE(grid%sct_dom_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1871,&
'frame/module_domain.f: Failed to deallocate grid%sct_dom_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%scb_dom_gc ) ) THEN 
  DEALLOCATE(grid%scb_dom_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1878,&
'frame/module_domain.f: Failed to deallocate grid%scb_dom_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%greenfrac ) ) THEN 
  DEALLOCATE(grid%greenfrac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1885,&
'frame/module_domain.f: Failed to deallocate grid%greenfrac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albedo12m ) ) THEN 
  DEALLOCATE(grid%albedo12m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1892,&
'frame/module_domain.f: Failed to deallocate grid%albedo12m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lai12m ) ) THEN 
  DEALLOCATE(grid%lai12m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1899,&
'frame/module_domain.f: Failed to deallocate grid%lai12m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pd_gc ) ) THEN 
  DEALLOCATE(grid%pd_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1906,&
'frame/module_domain.f: Failed to deallocate grid%pd_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pdrho_gc ) ) THEN 
  DEALLOCATE(grid%pdrho_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1913,&
'frame/module_domain.f: Failed to deallocate grid%pdrho_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psfc_gc ) ) THEN 
  DEALLOCATE(grid%psfc_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1920,&
'frame/module_domain.f: Failed to deallocate grid%psfc_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%intq_gc ) ) THEN 
  DEALLOCATE(grid%intq_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1927,&
'frame/module_domain.f: Failed to deallocate grid%intq_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pdhs ) ) THEN 
  DEALLOCATE(grid%pdhs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1934,&
'frame/module_domain.f: Failed to deallocate grid%pdhs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_gc ) ) THEN 
  DEALLOCATE(grid%qv_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1941,&
'frame/module_domain.f: Failed to deallocate grid%qv_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh_gc ) ) THEN 
  DEALLOCATE(grid%sh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1948,&
'frame/module_domain.f: Failed to deallocate grid%sh_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cl_gc ) ) THEN 
  DEALLOCATE(grid%cl_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1955,&
'frame/module_domain.f: Failed to deallocate grid%cl_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cf_gc ) ) THEN 
  DEALLOCATE(grid%cf_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1962,&
'frame/module_domain.f: Failed to deallocate grid%cf_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icefrac_gc ) ) THEN 
  DEALLOCATE(grid%icefrac_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1969,&
'frame/module_domain.f: Failed to deallocate grid%icefrac_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icepct ) ) THEN 
  DEALLOCATE(grid%icepct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1976,&
'frame/module_domain.f: Failed to deallocate grid%icepct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qr_gc ) ) THEN 
  DEALLOCATE(grid%qr_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1983,&
'frame/module_domain.f: Failed to deallocate grid%qr_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_gc ) ) THEN 
  DEALLOCATE(grid%qc_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1990,&
'frame/module_domain.f: Failed to deallocate grid%qc_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qs_gc ) ) THEN 
  DEALLOCATE(grid%qs_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",1997,&
'frame/module_domain.f: Failed to deallocate grid%qs_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qi_gc ) ) THEN 
  DEALLOCATE(grid%qi_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2004,&
'frame/module_domain.f: Failed to deallocate grid%qi_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qg_gc ) ) THEN 
  DEALLOCATE(grid%qg_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2011,&
'frame/module_domain.f: Failed to deallocate grid%qg_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qh_gc ) ) THEN 
  DEALLOCATE(grid%qh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2018,&
'frame/module_domain.f: Failed to deallocate grid%qh_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qni_gc ) ) THEN 
  DEALLOCATE(grid%qni_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2025,&
'frame/module_domain.f: Failed to deallocate grid%qni_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnc_gc ) ) THEN 
  DEALLOCATE(grid%qnc_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2032,&
'frame/module_domain.f: Failed to deallocate grid%qnc_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnr_gc ) ) THEN 
  DEALLOCATE(grid%qnr_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2039,&
'frame/module_domain.f: Failed to deallocate grid%qnr_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qns_gc ) ) THEN 
  DEALLOCATE(grid%qns_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2046,&
'frame/module_domain.f: Failed to deallocate grid%qns_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qng_gc ) ) THEN 
  DEALLOCATE(grid%qng_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2053,&
'frame/module_domain.f: Failed to deallocate grid%qng_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnh_gc ) ) THEN 
  DEALLOCATE(grid%qnh_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2060,&
'frame/module_domain.f: Failed to deallocate grid%qnh_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_gc ) ) THEN 
  DEALLOCATE(grid%qnwfa_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2067,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_gc ) ) THEN 
  DEALLOCATE(grid%qnifa_gc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2074,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_gc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_now ) ) THEN 
  DEALLOCATE(grid%qnwfa_now,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2081,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_now. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_jan ) ) THEN 
  DEALLOCATE(grid%qnwfa_jan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2088,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_jan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_feb ) ) THEN 
  DEALLOCATE(grid%qnwfa_feb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2095,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_feb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_mar ) ) THEN 
  DEALLOCATE(grid%qnwfa_mar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2102,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_mar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_apr ) ) THEN 
  DEALLOCATE(grid%qnwfa_apr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2109,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_apr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_may ) ) THEN 
  DEALLOCATE(grid%qnwfa_may,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2116,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_may. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_jun ) ) THEN 
  DEALLOCATE(grid%qnwfa_jun,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2123,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_jun. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_jul ) ) THEN 
  DEALLOCATE(grid%qnwfa_jul,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2130,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_jul. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_aug ) ) THEN 
  DEALLOCATE(grid%qnwfa_aug,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2137,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_aug. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_sep ) ) THEN 
  DEALLOCATE(grid%qnwfa_sep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2144,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_sep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_oct ) ) THEN 
  DEALLOCATE(grid%qnwfa_oct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2151,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_oct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_nov ) ) THEN 
  DEALLOCATE(grid%qnwfa_nov,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2158,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_nov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa_dec ) ) THEN 
  DEALLOCATE(grid%qnwfa_dec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2165,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa_dec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_now ) ) THEN 
  DEALLOCATE(grid%qnifa_now,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2172,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_now. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_jan ) ) THEN 
  DEALLOCATE(grid%qnifa_jan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2179,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_jan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_feb ) ) THEN 
  DEALLOCATE(grid%qnifa_feb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2186,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_feb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_mar ) ) THEN 
  DEALLOCATE(grid%qnifa_mar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2193,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_mar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_apr ) ) THEN 
  DEALLOCATE(grid%qnifa_apr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2200,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_apr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_may ) ) THEN 
  DEALLOCATE(grid%qnifa_may,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2207,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_may. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_jun ) ) THEN 
  DEALLOCATE(grid%qnifa_jun,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2214,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_jun. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_jul ) ) THEN 
  DEALLOCATE(grid%qnifa_jul,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2221,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_jul. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_aug ) ) THEN 
  DEALLOCATE(grid%qnifa_aug,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2228,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_aug. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_sep ) ) THEN 
  DEALLOCATE(grid%qnifa_sep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2235,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_sep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_oct ) ) THEN 
  DEALLOCATE(grid%qnifa_oct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2242,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_oct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_nov ) ) THEN 
  DEALLOCATE(grid%qnifa_nov,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2249,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_nov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa_dec ) ) THEN 
  DEALLOCATE(grid%qnifa_dec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2256,&
'frame/module_domain.f: Failed to deallocate grid%qnifa_dec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qntemp ) ) THEN 
  DEALLOCATE(grid%qntemp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2263,&
'frame/module_domain.f: Failed to deallocate grid%qntemp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qntemp2 ) ) THEN 
  DEALLOCATE(grid%qntemp2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2270,&
'frame/module_domain.f: Failed to deallocate grid%qntemp2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_max_p ) ) THEN 
  DEALLOCATE(grid%t_max_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2277,&
'frame/module_domain.f: Failed to deallocate grid%t_max_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ght_max_p ) ) THEN 
  DEALLOCATE(grid%ght_max_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2284,&
'frame/module_domain.f: Failed to deallocate grid%ght_max_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%max_p ) ) THEN 
  DEALLOCATE(grid%max_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2291,&
'frame/module_domain.f: Failed to deallocate grid%max_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_min_p ) ) THEN 
  DEALLOCATE(grid%t_min_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2298,&
'frame/module_domain.f: Failed to deallocate grid%t_min_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ght_min_p ) ) THEN 
  DEALLOCATE(grid%ght_min_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2305,&
'frame/module_domain.f: Failed to deallocate grid%ght_min_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%min_p ) ) THEN 
  DEALLOCATE(grid%min_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2312,&
'frame/module_domain.f: Failed to deallocate grid%min_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hgtmaxw ) ) THEN 
  DEALLOCATE(grid%hgtmaxw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2319,&
'frame/module_domain.f: Failed to deallocate grid%hgtmaxw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hgttrop ) ) THEN 
  DEALLOCATE(grid%hgttrop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2326,&
'frame/module_domain.f: Failed to deallocate grid%hgttrop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pmaxw ) ) THEN 
  DEALLOCATE(grid%pmaxw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2333,&
'frame/module_domain.f: Failed to deallocate grid%pmaxw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pmaxwnn ) ) THEN 
  DEALLOCATE(grid%pmaxwnn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2340,&
'frame/module_domain.f: Failed to deallocate grid%pmaxwnn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ptrop ) ) THEN 
  DEALLOCATE(grid%ptrop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2347,&
'frame/module_domain.f: Failed to deallocate grid%ptrop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ptropnn ) ) THEN 
  DEALLOCATE(grid%ptropnn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2354,&
'frame/module_domain.f: Failed to deallocate grid%ptropnn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tmaxw ) ) THEN 
  DEALLOCATE(grid%tmaxw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2361,&
'frame/module_domain.f: Failed to deallocate grid%tmaxw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ttrop ) ) THEN 
  DEALLOCATE(grid%ttrop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2368,&
'frame/module_domain.f: Failed to deallocate grid%ttrop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%umaxw ) ) THEN 
  DEALLOCATE(grid%umaxw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2375,&
'frame/module_domain.f: Failed to deallocate grid%umaxw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%utrop ) ) THEN 
  DEALLOCATE(grid%utrop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2382,&
'frame/module_domain.f: Failed to deallocate grid%utrop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vmaxw ) ) THEN 
  DEALLOCATE(grid%vmaxw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2389,&
'frame/module_domain.f: Failed to deallocate grid%vmaxw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vtrop ) ) THEN 
  DEALLOCATE(grid%vtrop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2396,&
'frame/module_domain.f: Failed to deallocate grid%vtrop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%erod ) ) THEN 
  DEALLOCATE(grid%erod,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2403,&
'frame/module_domain.f: Failed to deallocate grid%erod. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_1 ) ) THEN 
  DEALLOCATE(grid%u_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2410,&
'frame/module_domain.f: Failed to deallocate grid%u_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_2 ) ) THEN 
  DEALLOCATE(grid%u_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2417,&
'frame/module_domain.f: Failed to deallocate grid%u_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_bxs ) ) THEN 
  DEALLOCATE(grid%u_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2424,&
'frame/module_domain.f: Failed to deallocate grid%u_bxs. ')
 endif
  NULLIFY(grid%u_bxs)
ENDIF
IF ( ASSOCIATED( grid%u_bxe ) ) THEN 
  DEALLOCATE(grid%u_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2432,&
'frame/module_domain.f: Failed to deallocate grid%u_bxe. ')
 endif
  NULLIFY(grid%u_bxe)
ENDIF
IF ( ASSOCIATED( grid%u_bys ) ) THEN 
  DEALLOCATE(grid%u_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2440,&
'frame/module_domain.f: Failed to deallocate grid%u_bys. ')
 endif
  NULLIFY(grid%u_bys)
ENDIF
IF ( ASSOCIATED( grid%u_bye ) ) THEN 
  DEALLOCATE(grid%u_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2448,&
'frame/module_domain.f: Failed to deallocate grid%u_bye. ')
 endif
  NULLIFY(grid%u_bye)
ENDIF
IF ( ASSOCIATED( grid%u_btxs ) ) THEN 
  DEALLOCATE(grid%u_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2456,&
'frame/module_domain.f: Failed to deallocate grid%u_btxs. ')
 endif
  NULLIFY(grid%u_btxs)
ENDIF
IF ( ASSOCIATED( grid%u_btxe ) ) THEN 
  DEALLOCATE(grid%u_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2464,&
'frame/module_domain.f: Failed to deallocate grid%u_btxe. ')
 endif
  NULLIFY(grid%u_btxe)
ENDIF
IF ( ASSOCIATED( grid%u_btys ) ) THEN 
  DEALLOCATE(grid%u_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2472,&
'frame/module_domain.f: Failed to deallocate grid%u_btys. ')
 endif
  NULLIFY(grid%u_btys)
ENDIF
IF ( ASSOCIATED( grid%u_btye ) ) THEN 
  DEALLOCATE(grid%u_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2480,&
'frame/module_domain.f: Failed to deallocate grid%u_btye. ')
 endif
  NULLIFY(grid%u_btye)
ENDIF
IF ( ASSOCIATED( grid%ru ) ) THEN 
  DEALLOCATE(grid%ru,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2488,&
'frame/module_domain.f: Failed to deallocate grid%ru. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ru_m ) ) THEN 
  DEALLOCATE(grid%ru_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2495,&
'frame/module_domain.f: Failed to deallocate grid%ru_m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ru_tend ) ) THEN 
  DEALLOCATE(grid%ru_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2502,&
'frame/module_domain.f: Failed to deallocate grid%ru_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_save ) ) THEN 
  DEALLOCATE(grid%u_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2509,&
'frame/module_domain.f: Failed to deallocate grid%u_save. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_force ) ) THEN 
  DEALLOCATE(grid%z_force,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2516,&
'frame/module_domain.f: Failed to deallocate grid%z_force. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_force_tend ) ) THEN 
  DEALLOCATE(grid%z_force_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2523,&
'frame/module_domain.f: Failed to deallocate grid%z_force_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_g ) ) THEN 
  DEALLOCATE(grid%u_g,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2530,&
'frame/module_domain.f: Failed to deallocate grid%u_g. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_g_tend ) ) THEN 
  DEALLOCATE(grid%u_g_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2537,&
'frame/module_domain.f: Failed to deallocate grid%u_g_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_1 ) ) THEN 
  DEALLOCATE(grid%v_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2544,&
'frame/module_domain.f: Failed to deallocate grid%v_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_2 ) ) THEN 
  DEALLOCATE(grid%v_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2551,&
'frame/module_domain.f: Failed to deallocate grid%v_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_bxs ) ) THEN 
  DEALLOCATE(grid%v_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2558,&
'frame/module_domain.f: Failed to deallocate grid%v_bxs. ')
 endif
  NULLIFY(grid%v_bxs)
ENDIF
IF ( ASSOCIATED( grid%v_bxe ) ) THEN 
  DEALLOCATE(grid%v_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2566,&
'frame/module_domain.f: Failed to deallocate grid%v_bxe. ')
 endif
  NULLIFY(grid%v_bxe)
ENDIF
IF ( ASSOCIATED( grid%v_bys ) ) THEN 
  DEALLOCATE(grid%v_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2574,&
'frame/module_domain.f: Failed to deallocate grid%v_bys. ')
 endif
  NULLIFY(grid%v_bys)
ENDIF
IF ( ASSOCIATED( grid%v_bye ) ) THEN 
  DEALLOCATE(grid%v_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2582,&
'frame/module_domain.f: Failed to deallocate grid%v_bye. ')
 endif
  NULLIFY(grid%v_bye)
ENDIF
IF ( ASSOCIATED( grid%v_btxs ) ) THEN 
  DEALLOCATE(grid%v_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2590,&
'frame/module_domain.f: Failed to deallocate grid%v_btxs. ')
 endif
  NULLIFY(grid%v_btxs)
ENDIF
IF ( ASSOCIATED( grid%v_btxe ) ) THEN 
  DEALLOCATE(grid%v_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2598,&
'frame/module_domain.f: Failed to deallocate grid%v_btxe. ')
 endif
  NULLIFY(grid%v_btxe)
ENDIF
IF ( ASSOCIATED( grid%v_btys ) ) THEN 
  DEALLOCATE(grid%v_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2606,&
'frame/module_domain.f: Failed to deallocate grid%v_btys. ')
 endif
  NULLIFY(grid%v_btys)
ENDIF
IF ( ASSOCIATED( grid%v_btye ) ) THEN 
  DEALLOCATE(grid%v_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2614,&
'frame/module_domain.f: Failed to deallocate grid%v_btye. ')
 endif
  NULLIFY(grid%v_btye)
ENDIF
IF ( ASSOCIATED( grid%rv ) ) THEN 
  DEALLOCATE(grid%rv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2622,&
'frame/module_domain.f: Failed to deallocate grid%rv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rv_m ) ) THEN 
  DEALLOCATE(grid%rv_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2629,&
'frame/module_domain.f: Failed to deallocate grid%rv_m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rv_tend ) ) THEN 
  DEALLOCATE(grid%rv_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2636,&
'frame/module_domain.f: Failed to deallocate grid%rv_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_save ) ) THEN 
  DEALLOCATE(grid%v_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2643,&
'frame/module_domain.f: Failed to deallocate grid%v_save. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_g ) ) THEN 
  DEALLOCATE(grid%v_g,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2650,&
'frame/module_domain.f: Failed to deallocate grid%v_g. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_g_tend ) ) THEN 
  DEALLOCATE(grid%v_g_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2657,&
'frame/module_domain.f: Failed to deallocate grid%v_g_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_1 ) ) THEN 
  DEALLOCATE(grid%w_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2664,&
'frame/module_domain.f: Failed to deallocate grid%w_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_2 ) ) THEN 
  DEALLOCATE(grid%w_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2671,&
'frame/module_domain.f: Failed to deallocate grid%w_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_bxs ) ) THEN 
  DEALLOCATE(grid%w_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2678,&
'frame/module_domain.f: Failed to deallocate grid%w_bxs. ')
 endif
  NULLIFY(grid%w_bxs)
ENDIF
IF ( ASSOCIATED( grid%w_bxe ) ) THEN 
  DEALLOCATE(grid%w_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2686,&
'frame/module_domain.f: Failed to deallocate grid%w_bxe. ')
 endif
  NULLIFY(grid%w_bxe)
ENDIF
IF ( ASSOCIATED( grid%w_bys ) ) THEN 
  DEALLOCATE(grid%w_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2694,&
'frame/module_domain.f: Failed to deallocate grid%w_bys. ')
 endif
  NULLIFY(grid%w_bys)
ENDIF
IF ( ASSOCIATED( grid%w_bye ) ) THEN 
  DEALLOCATE(grid%w_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2702,&
'frame/module_domain.f: Failed to deallocate grid%w_bye. ')
 endif
  NULLIFY(grid%w_bye)
ENDIF
IF ( ASSOCIATED( grid%w_btxs ) ) THEN 
  DEALLOCATE(grid%w_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2710,&
'frame/module_domain.f: Failed to deallocate grid%w_btxs. ')
 endif
  NULLIFY(grid%w_btxs)
ENDIF
IF ( ASSOCIATED( grid%w_btxe ) ) THEN 
  DEALLOCATE(grid%w_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2718,&
'frame/module_domain.f: Failed to deallocate grid%w_btxe. ')
 endif
  NULLIFY(grid%w_btxe)
ENDIF
IF ( ASSOCIATED( grid%w_btys ) ) THEN 
  DEALLOCATE(grid%w_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2726,&
'frame/module_domain.f: Failed to deallocate grid%w_btys. ')
 endif
  NULLIFY(grid%w_btys)
ENDIF
IF ( ASSOCIATED( grid%w_btye ) ) THEN 
  DEALLOCATE(grid%w_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2734,&
'frame/module_domain.f: Failed to deallocate grid%w_btye. ')
 endif
  NULLIFY(grid%w_btye)
ENDIF
IF ( ASSOCIATED( grid%ww ) ) THEN 
  DEALLOCATE(grid%ww,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2742,&
'frame/module_domain.f: Failed to deallocate grid%ww. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rw ) ) THEN 
  DEALLOCATE(grid%rw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2749,&
'frame/module_domain.f: Failed to deallocate grid%rw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ww_m ) ) THEN 
  DEALLOCATE(grid%ww_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2756,&
'frame/module_domain.f: Failed to deallocate grid%ww_m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_subs ) ) THEN 
  DEALLOCATE(grid%w_subs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2763,&
'frame/module_domain.f: Failed to deallocate grid%w_subs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_subs_tend ) ) THEN 
  DEALLOCATE(grid%w_subs_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2770,&
'frame/module_domain.f: Failed to deallocate grid%w_subs_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ph_1 ) ) THEN 
  DEALLOCATE(grid%ph_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2777,&
'frame/module_domain.f: Failed to deallocate grid%ph_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ph_2 ) ) THEN 
  DEALLOCATE(grid%ph_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2784,&
'frame/module_domain.f: Failed to deallocate grid%ph_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ph_bxs ) ) THEN 
  DEALLOCATE(grid%ph_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2791,&
'frame/module_domain.f: Failed to deallocate grid%ph_bxs. ')
 endif
  NULLIFY(grid%ph_bxs)
ENDIF
IF ( ASSOCIATED( grid%ph_bxe ) ) THEN 
  DEALLOCATE(grid%ph_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2799,&
'frame/module_domain.f: Failed to deallocate grid%ph_bxe. ')
 endif
  NULLIFY(grid%ph_bxe)
ENDIF
IF ( ASSOCIATED( grid%ph_bys ) ) THEN 
  DEALLOCATE(grid%ph_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2807,&
'frame/module_domain.f: Failed to deallocate grid%ph_bys. ')
 endif
  NULLIFY(grid%ph_bys)
ENDIF
IF ( ASSOCIATED( grid%ph_bye ) ) THEN 
  DEALLOCATE(grid%ph_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2815,&
'frame/module_domain.f: Failed to deallocate grid%ph_bye. ')
 endif
  NULLIFY(grid%ph_bye)
ENDIF
IF ( ASSOCIATED( grid%ph_btxs ) ) THEN 
  DEALLOCATE(grid%ph_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2823,&
'frame/module_domain.f: Failed to deallocate grid%ph_btxs. ')
 endif
  NULLIFY(grid%ph_btxs)
ENDIF
IF ( ASSOCIATED( grid%ph_btxe ) ) THEN 
  DEALLOCATE(grid%ph_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2831,&
'frame/module_domain.f: Failed to deallocate grid%ph_btxe. ')
 endif
  NULLIFY(grid%ph_btxe)
ENDIF
IF ( ASSOCIATED( grid%ph_btys ) ) THEN 
  DEALLOCATE(grid%ph_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2839,&
'frame/module_domain.f: Failed to deallocate grid%ph_btys. ')
 endif
  NULLIFY(grid%ph_btys)
ENDIF
IF ( ASSOCIATED( grid%ph_btye ) ) THEN 
  DEALLOCATE(grid%ph_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2847,&
'frame/module_domain.f: Failed to deallocate grid%ph_btye. ')
 endif
  NULLIFY(grid%ph_btye)
ENDIF
IF ( ASSOCIATED( grid%phb ) ) THEN 
  DEALLOCATE(grid%phb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2855,&
'frame/module_domain.f: Failed to deallocate grid%phb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%phb_fine ) ) THEN 
  DEALLOCATE(grid%phb_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2862,&
'frame/module_domain.f: Failed to deallocate grid%phb_fine. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ph0 ) ) THEN 
  DEALLOCATE(grid%ph0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2869,&
'frame/module_domain.f: Failed to deallocate grid%ph0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%php ) ) THEN 
  DEALLOCATE(grid%php,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2876,&
'frame/module_domain.f: Failed to deallocate grid%php. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_phy_m_t0 ) ) THEN 
  DEALLOCATE(grid%th_phy_m_t0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2883,&
'frame/module_domain.f: Failed to deallocate grid%th_phy_m_t0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_1 ) ) THEN 
  DEALLOCATE(grid%t_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2890,&
'frame/module_domain.f: Failed to deallocate grid%t_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_2 ) ) THEN 
  DEALLOCATE(grid%t_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2897,&
'frame/module_domain.f: Failed to deallocate grid%t_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_bxs ) ) THEN 
  DEALLOCATE(grid%t_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2904,&
'frame/module_domain.f: Failed to deallocate grid%t_bxs. ')
 endif
  NULLIFY(grid%t_bxs)
ENDIF
IF ( ASSOCIATED( grid%t_bxe ) ) THEN 
  DEALLOCATE(grid%t_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2912,&
'frame/module_domain.f: Failed to deallocate grid%t_bxe. ')
 endif
  NULLIFY(grid%t_bxe)
ENDIF
IF ( ASSOCIATED( grid%t_bys ) ) THEN 
  DEALLOCATE(grid%t_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2920,&
'frame/module_domain.f: Failed to deallocate grid%t_bys. ')
 endif
  NULLIFY(grid%t_bys)
ENDIF
IF ( ASSOCIATED( grid%t_bye ) ) THEN 
  DEALLOCATE(grid%t_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2928,&
'frame/module_domain.f: Failed to deallocate grid%t_bye. ')
 endif
  NULLIFY(grid%t_bye)
ENDIF
IF ( ASSOCIATED( grid%t_btxs ) ) THEN 
  DEALLOCATE(grid%t_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2936,&
'frame/module_domain.f: Failed to deallocate grid%t_btxs. ')
 endif
  NULLIFY(grid%t_btxs)
ENDIF
IF ( ASSOCIATED( grid%t_btxe ) ) THEN 
  DEALLOCATE(grid%t_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2944,&
'frame/module_domain.f: Failed to deallocate grid%t_btxe. ')
 endif
  NULLIFY(grid%t_btxe)
ENDIF
IF ( ASSOCIATED( grid%t_btys ) ) THEN 
  DEALLOCATE(grid%t_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2952,&
'frame/module_domain.f: Failed to deallocate grid%t_btys. ')
 endif
  NULLIFY(grid%t_btys)
ENDIF
IF ( ASSOCIATED( grid%t_btye ) ) THEN 
  DEALLOCATE(grid%t_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2960,&
'frame/module_domain.f: Failed to deallocate grid%t_btye. ')
 endif
  NULLIFY(grid%t_btye)
ENDIF
IF ( ASSOCIATED( grid%t_init ) ) THEN 
  DEALLOCATE(grid%t_init,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2968,&
'frame/module_domain.f: Failed to deallocate grid%t_init. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_save ) ) THEN 
  DEALLOCATE(grid%t_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2975,&
'frame/module_domain.f: Failed to deallocate grid%t_save. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_upstream_x ) ) THEN 
  DEALLOCATE(grid%th_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2982,&
'frame/module_domain.f: Failed to deallocate grid%th_upstream_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%th_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2989,&
'frame/module_domain.f: Failed to deallocate grid%th_upstream_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_upstream_y ) ) THEN 
  DEALLOCATE(grid%th_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",2996,&
'frame/module_domain.f: Failed to deallocate grid%th_upstream_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%th_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3003,&
'frame/module_domain.f: Failed to deallocate grid%th_upstream_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_x ) ) THEN 
  DEALLOCATE(grid%qv_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3010,&
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%qv_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3017,&
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_y ) ) THEN 
  DEALLOCATE(grid%qv_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3024,&
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%qv_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3031,&
'frame/module_domain.f: Failed to deallocate grid%qv_upstream_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_x ) ) THEN 
  DEALLOCATE(grid%ql_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3038,&
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%ql_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3045,&
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_y ) ) THEN 
  DEALLOCATE(grid%ql_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3052,&
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%ql_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3059,&
'frame/module_domain.f: Failed to deallocate grid%ql_upstream_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_upstream_x ) ) THEN 
  DEALLOCATE(grid%u_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3066,&
'frame/module_domain.f: Failed to deallocate grid%u_upstream_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%u_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3073,&
'frame/module_domain.f: Failed to deallocate grid%u_upstream_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_upstream_y ) ) THEN 
  DEALLOCATE(grid%u_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3080,&
'frame/module_domain.f: Failed to deallocate grid%u_upstream_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%u_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3087,&
'frame/module_domain.f: Failed to deallocate grid%u_upstream_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_upstream_x ) ) THEN 
  DEALLOCATE(grid%v_upstream_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3094,&
'frame/module_domain.f: Failed to deallocate grid%v_upstream_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_upstream_x_tend ) ) THEN 
  DEALLOCATE(grid%v_upstream_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3101,&
'frame/module_domain.f: Failed to deallocate grid%v_upstream_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_upstream_y ) ) THEN 
  DEALLOCATE(grid%v_upstream_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3108,&
'frame/module_domain.f: Failed to deallocate grid%v_upstream_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_upstream_y_tend ) ) THEN 
  DEALLOCATE(grid%v_upstream_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3115,&
'frame/module_domain.f: Failed to deallocate grid%v_upstream_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_t_tend ) ) THEN 
  DEALLOCATE(grid%th_t_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3122,&
'frame/module_domain.f: Failed to deallocate grid%th_t_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_t_tend ) ) THEN 
  DEALLOCATE(grid%qv_t_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3129,&
'frame/module_domain.f: Failed to deallocate grid%qv_t_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_largescale ) ) THEN 
  DEALLOCATE(grid%th_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3136,&
'frame/module_domain.f: Failed to deallocate grid%th_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_largescale_tend ) ) THEN 
  DEALLOCATE(grid%th_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3143,&
'frame/module_domain.f: Failed to deallocate grid%th_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_largescale ) ) THEN 
  DEALLOCATE(grid%qv_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3150,&
'frame/module_domain.f: Failed to deallocate grid%qv_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_largescale_tend ) ) THEN 
  DEALLOCATE(grid%qv_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3157,&
'frame/module_domain.f: Failed to deallocate grid%qv_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_largescale ) ) THEN 
  DEALLOCATE(grid%ql_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3164,&
'frame/module_domain.f: Failed to deallocate grid%ql_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ql_largescale_tend ) ) THEN 
  DEALLOCATE(grid%ql_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3171,&
'frame/module_domain.f: Failed to deallocate grid%ql_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_largescale ) ) THEN 
  DEALLOCATE(grid%u_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3178,&
'frame/module_domain.f: Failed to deallocate grid%u_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_largescale_tend ) ) THEN 
  DEALLOCATE(grid%u_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3185,&
'frame/module_domain.f: Failed to deallocate grid%u_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_largescale ) ) THEN 
  DEALLOCATE(grid%v_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3192,&
'frame/module_domain.f: Failed to deallocate grid%v_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_largescale_tend ) ) THEN 
  DEALLOCATE(grid%v_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3199,&
'frame/module_domain.f: Failed to deallocate grid%v_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_largescale ) ) THEN 
  DEALLOCATE(grid%tau_largescale,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3206,&
'frame/module_domain.f: Failed to deallocate grid%tau_largescale. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_largescale_tend ) ) THEN 
  DEALLOCATE(grid%tau_largescale_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3213,&
'frame/module_domain.f: Failed to deallocate grid%tau_largescale_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_x ) ) THEN 
  DEALLOCATE(grid%tau_x,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3220,&
'frame/module_domain.f: Failed to deallocate grid%tau_x. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_x_tend ) ) THEN 
  DEALLOCATE(grid%tau_x_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3227,&
'frame/module_domain.f: Failed to deallocate grid%tau_x_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_y ) ) THEN 
  DEALLOCATE(grid%tau_y,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3234,&
'frame/module_domain.f: Failed to deallocate grid%tau_y. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_y_tend ) ) THEN 
  DEALLOCATE(grid%tau_y_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3241,&
'frame/module_domain.f: Failed to deallocate grid%tau_y_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soil_forcing_val ) ) THEN 
  DEALLOCATE(grid%t_soil_forcing_val,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3248,&
'frame/module_domain.f: Failed to deallocate grid%t_soil_forcing_val. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soil_forcing_tend ) ) THEN 
  DEALLOCATE(grid%t_soil_forcing_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3255,&
'frame/module_domain.f: Failed to deallocate grid%t_soil_forcing_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q_soil_forcing_val ) ) THEN 
  DEALLOCATE(grid%q_soil_forcing_val,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3262,&
'frame/module_domain.f: Failed to deallocate grid%q_soil_forcing_val. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q_soil_forcing_tend ) ) THEN 
  DEALLOCATE(grid%q_soil_forcing_tend,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3269,&
'frame/module_domain.f: Failed to deallocate grid%q_soil_forcing_tend. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tau_soil ) ) THEN 
  DEALLOCATE(grid%tau_soil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3276,&
'frame/module_domain.f: Failed to deallocate grid%tau_soil. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soil_depth_force ) ) THEN 
  DEALLOCATE(grid%soil_depth_force,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3283,&
'frame/module_domain.f: Failed to deallocate grid%soil_depth_force. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu_1 ) ) THEN 
  DEALLOCATE(grid%mu_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3290,&
'frame/module_domain.f: Failed to deallocate grid%mu_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu_2 ) ) THEN 
  DEALLOCATE(grid%mu_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3297,&
'frame/module_domain.f: Failed to deallocate grid%mu_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu_bxs ) ) THEN 
  DEALLOCATE(grid%mu_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3304,&
'frame/module_domain.f: Failed to deallocate grid%mu_bxs. ')
 endif
  NULLIFY(grid%mu_bxs)
ENDIF
IF ( ASSOCIATED( grid%mu_bxe ) ) THEN 
  DEALLOCATE(grid%mu_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3312,&
'frame/module_domain.f: Failed to deallocate grid%mu_bxe. ')
 endif
  NULLIFY(grid%mu_bxe)
ENDIF
IF ( ASSOCIATED( grid%mu_bys ) ) THEN 
  DEALLOCATE(grid%mu_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3320,&
'frame/module_domain.f: Failed to deallocate grid%mu_bys. ')
 endif
  NULLIFY(grid%mu_bys)
ENDIF
IF ( ASSOCIATED( grid%mu_bye ) ) THEN 
  DEALLOCATE(grid%mu_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3328,&
'frame/module_domain.f: Failed to deallocate grid%mu_bye. ')
 endif
  NULLIFY(grid%mu_bye)
ENDIF
IF ( ASSOCIATED( grid%mu_btxs ) ) THEN 
  DEALLOCATE(grid%mu_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3336,&
'frame/module_domain.f: Failed to deallocate grid%mu_btxs. ')
 endif
  NULLIFY(grid%mu_btxs)
ENDIF
IF ( ASSOCIATED( grid%mu_btxe ) ) THEN 
  DEALLOCATE(grid%mu_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3344,&
'frame/module_domain.f: Failed to deallocate grid%mu_btxe. ')
 endif
  NULLIFY(grid%mu_btxe)
ENDIF
IF ( ASSOCIATED( grid%mu_btys ) ) THEN 
  DEALLOCATE(grid%mu_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3352,&
'frame/module_domain.f: Failed to deallocate grid%mu_btys. ')
 endif
  NULLIFY(grid%mu_btys)
ENDIF
IF ( ASSOCIATED( grid%mu_btye ) ) THEN 
  DEALLOCATE(grid%mu_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3360,&
'frame/module_domain.f: Failed to deallocate grid%mu_btye. ')
 endif
  NULLIFY(grid%mu_btye)
ENDIF
IF ( ASSOCIATED( grid%mub ) ) THEN 
  DEALLOCATE(grid%mub,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3368,&
'frame/module_domain.f: Failed to deallocate grid%mub. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mub_fine ) ) THEN 
  DEALLOCATE(grid%mub_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3375,&
'frame/module_domain.f: Failed to deallocate grid%mub_fine. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mub_save ) ) THEN 
  DEALLOCATE(grid%mub_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3382,&
'frame/module_domain.f: Failed to deallocate grid%mub_save. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu0 ) ) THEN 
  DEALLOCATE(grid%mu0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3389,&
'frame/module_domain.f: Failed to deallocate grid%mu0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mudf ) ) THEN 
  DEALLOCATE(grid%mudf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3396,&
'frame/module_domain.f: Failed to deallocate grid%mudf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%muu ) ) THEN 
  DEALLOCATE(grid%muu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3403,&
'frame/module_domain.f: Failed to deallocate grid%muu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%muus ) ) THEN 
  DEALLOCATE(grid%muus,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3410,&
'frame/module_domain.f: Failed to deallocate grid%muus. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%muv ) ) THEN 
  DEALLOCATE(grid%muv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3417,&
'frame/module_domain.f: Failed to deallocate grid%muv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%muvs ) ) THEN 
  DEALLOCATE(grid%muvs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3424,&
'frame/module_domain.f: Failed to deallocate grid%muvs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mut ) ) THEN 
  DEALLOCATE(grid%mut,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3431,&
'frame/module_domain.f: Failed to deallocate grid%mut. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%muts ) ) THEN 
  DEALLOCATE(grid%muts,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3438,&
'frame/module_domain.f: Failed to deallocate grid%muts. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nest_pos ) ) THEN 
  DEALLOCATE(grid%nest_pos,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3445,&
'frame/module_domain.f: Failed to deallocate grid%nest_pos. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nest_mask ) ) THEN 
  DEALLOCATE(grid%nest_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3452,&
'frame/module_domain.f: Failed to deallocate grid%nest_mask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_coarse ) ) THEN 
  DEALLOCATE(grid%ht_coarse,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3459,&
'frame/module_domain.f: Failed to deallocate grid%ht_coarse. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tke_1 ) ) THEN 
  DEALLOCATE(grid%tke_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3466,&
'frame/module_domain.f: Failed to deallocate grid%tke_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tke_2 ) ) THEN 
  DEALLOCATE(grid%tke_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3473,&
'frame/module_domain.f: Failed to deallocate grid%tke_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p ) ) THEN 
  DEALLOCATE(grid%p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3480,&
'frame/module_domain.f: Failed to deallocate grid%p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%al ) ) THEN 
  DEALLOCATE(grid%al,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3487,&
'frame/module_domain.f: Failed to deallocate grid%al. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alt ) ) THEN 
  DEALLOCATE(grid%alt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3494,&
'frame/module_domain.f: Failed to deallocate grid%alt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alb ) ) THEN 
  DEALLOCATE(grid%alb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3501,&
'frame/module_domain.f: Failed to deallocate grid%alb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zx ) ) THEN 
  DEALLOCATE(grid%zx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3508,&
'frame/module_domain.f: Failed to deallocate grid%zx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zy ) ) THEN 
  DEALLOCATE(grid%zy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3515,&
'frame/module_domain.f: Failed to deallocate grid%zy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rdz ) ) THEN 
  DEALLOCATE(grid%rdz,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3522,&
'frame/module_domain.f: Failed to deallocate grid%rdz. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rdzw ) ) THEN 
  DEALLOCATE(grid%rdzw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3529,&
'frame/module_domain.f: Failed to deallocate grid%rdzw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pb ) ) THEN 
  DEALLOCATE(grid%pb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3536,&
'frame/module_domain.f: Failed to deallocate grid%pb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rho ) ) THEN 
  DEALLOCATE(grid%rho,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3543,&
'frame/module_domain.f: Failed to deallocate grid%rho. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fnm ) ) THEN 
  DEALLOCATE(grid%fnm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3550,&
'frame/module_domain.f: Failed to deallocate grid%fnm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fnp ) ) THEN 
  DEALLOCATE(grid%fnp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3557,&
'frame/module_domain.f: Failed to deallocate grid%fnp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rdnw ) ) THEN 
  DEALLOCATE(grid%rdnw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3564,&
'frame/module_domain.f: Failed to deallocate grid%rdnw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rdn ) ) THEN 
  DEALLOCATE(grid%rdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3571,&
'frame/module_domain.f: Failed to deallocate grid%rdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dnw ) ) THEN 
  DEALLOCATE(grid%dnw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3578,&
'frame/module_domain.f: Failed to deallocate grid%dnw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dn ) ) THEN 
  DEALLOCATE(grid%dn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3585,&
'frame/module_domain.f: Failed to deallocate grid%dn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_base ) ) THEN 
  DEALLOCATE(grid%t_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3592,&
'frame/module_domain.f: Failed to deallocate grid%t_base. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z ) ) THEN 
  DEALLOCATE(grid%z,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3599,&
'frame/module_domain.f: Failed to deallocate grid%z. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_at_w ) ) THEN 
  DEALLOCATE(grid%z_at_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3606,&
'frame/module_domain.f: Failed to deallocate grid%z_at_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_hyd ) ) THEN 
  DEALLOCATE(grid%p_hyd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3613,&
'frame/module_domain.f: Failed to deallocate grid%p_hyd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_hyd_w ) ) THEN 
  DEALLOCATE(grid%p_hyd_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3620,&
'frame/module_domain.f: Failed to deallocate grid%p_hyd_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2 ) ) THEN 
  DEALLOCATE(grid%q2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3627,&
'frame/module_domain.f: Failed to deallocate grid%q2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2 ) ) THEN 
  DEALLOCATE(grid%t2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3634,&
'frame/module_domain.f: Failed to deallocate grid%t2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2 ) ) THEN 
  DEALLOCATE(grid%th2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3641,&
'frame/module_domain.f: Failed to deallocate grid%th2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psfc ) ) THEN 
  DEALLOCATE(grid%psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3648,&
'frame/module_domain.f: Failed to deallocate grid%psfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10 ) ) THEN 
  DEALLOCATE(grid%u10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3655,&
'frame/module_domain.f: Failed to deallocate grid%u10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10 ) ) THEN 
  DEALLOCATE(grid%v10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3662,&
'frame/module_domain.f: Failed to deallocate grid%v10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lpi ) ) THEN 
  DEALLOCATE(grid%lpi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3669,&
'frame/module_domain.f: Failed to deallocate grid%lpi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uratx ) ) THEN 
  DEALLOCATE(grid%uratx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3676,&
'frame/module_domain.f: Failed to deallocate grid%uratx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vratx ) ) THEN 
  DEALLOCATE(grid%vratx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3683,&
'frame/module_domain.f: Failed to deallocate grid%vratx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tratx ) ) THEN 
  DEALLOCATE(grid%tratx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3690,&
'frame/module_domain.f: Failed to deallocate grid%tratx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%obs_savwt ) ) THEN 
  DEALLOCATE(grid%obs_savwt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3697,&
'frame/module_domain.f: Failed to deallocate grid%obs_savwt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%power ) ) THEN 
  DEALLOCATE(grid%power,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3704,&
'frame/module_domain.f: Failed to deallocate grid%power. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%imask_nostag ) ) THEN 
  DEALLOCATE(grid%imask_nostag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3711,&
'frame/module_domain.f: Failed to deallocate grid%imask_nostag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%imask_xstag ) ) THEN 
  DEALLOCATE(grid%imask_xstag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3718,&
'frame/module_domain.f: Failed to deallocate grid%imask_xstag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%imask_ystag ) ) THEN 
  DEALLOCATE(grid%imask_ystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3725,&
'frame/module_domain.f: Failed to deallocate grid%imask_ystag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%imask_xystag ) ) THEN 
  DEALLOCATE(grid%imask_xystag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3732,&
'frame/module_domain.f: Failed to deallocate grid%imask_xystag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%moist ) ) THEN 
  DEALLOCATE(grid%moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3739,&
'frame/module_domain.f: Failed to deallocate grid%moist. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%moist_bxs ) ) THEN 
  DEALLOCATE(grid%moist_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3746,&
'frame/module_domain.f: Failed to deallocate grid%moist_bxs. ')
 endif
  NULLIFY(grid%moist_bxs)
ENDIF
IF ( ASSOCIATED( grid%moist_bxe ) ) THEN 
  DEALLOCATE(grid%moist_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3754,&
'frame/module_domain.f: Failed to deallocate grid%moist_bxe. ')
 endif
  NULLIFY(grid%moist_bxe)
ENDIF
IF ( ASSOCIATED( grid%moist_bys ) ) THEN 
  DEALLOCATE(grid%moist_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3762,&
'frame/module_domain.f: Failed to deallocate grid%moist_bys. ')
 endif
  NULLIFY(grid%moist_bys)
ENDIF
IF ( ASSOCIATED( grid%moist_bye ) ) THEN 
  DEALLOCATE(grid%moist_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3770,&
'frame/module_domain.f: Failed to deallocate grid%moist_bye. ')
 endif
  NULLIFY(grid%moist_bye)
ENDIF
IF ( ASSOCIATED( grid%moist_btxs ) ) THEN 
  DEALLOCATE(grid%moist_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3778,&
'frame/module_domain.f: Failed to deallocate grid%moist_btxs. ')
 endif
  NULLIFY(grid%moist_btxs)
ENDIF
IF ( ASSOCIATED( grid%moist_btxe ) ) THEN 
  DEALLOCATE(grid%moist_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3786,&
'frame/module_domain.f: Failed to deallocate grid%moist_btxe. ')
 endif
  NULLIFY(grid%moist_btxe)
ENDIF
IF ( ASSOCIATED( grid%moist_btys ) ) THEN 
  DEALLOCATE(grid%moist_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3794,&
'frame/module_domain.f: Failed to deallocate grid%moist_btys. ')
 endif
  NULLIFY(grid%moist_btys)
ENDIF
IF ( ASSOCIATED( grid%moist_btye ) ) THEN 
  DEALLOCATE(grid%moist_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3802,&
'frame/module_domain.f: Failed to deallocate grid%moist_btye. ')
 endif
  NULLIFY(grid%moist_btye)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist ) ) THEN 
  DEALLOCATE(grid%dfi_moist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3810,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bxs ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3817,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bxs. ')
 endif
  NULLIFY(grid%dfi_moist_bxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bxe ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3825,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bxe. ')
 endif
  NULLIFY(grid%dfi_moist_bxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bys ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3833,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bys. ')
 endif
  NULLIFY(grid%dfi_moist_bys)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_bye ) ) THEN 
  DEALLOCATE(grid%dfi_moist_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3841,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_bye. ')
 endif
  NULLIFY(grid%dfi_moist_bye)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btxs ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3849,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btxs. ')
 endif
  NULLIFY(grid%dfi_moist_btxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btxe ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3857,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btxe. ')
 endif
  NULLIFY(grid%dfi_moist_btxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btys ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3865,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btys. ')
 endif
  NULLIFY(grid%dfi_moist_btys)
ENDIF
IF ( ASSOCIATED( grid%dfi_moist_btye ) ) THEN 
  DEALLOCATE(grid%dfi_moist_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3873,&
'frame/module_domain.f: Failed to deallocate grid%dfi_moist_btye. ')
 endif
  NULLIFY(grid%dfi_moist_btye)
ENDIF
IF ( ASSOCIATED( grid%qvold ) ) THEN 
  DEALLOCATE(grid%qvold,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3881,&
'frame/module_domain.f: Failed to deallocate grid%qvold. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rimi ) ) THEN 
  DEALLOCATE(grid%rimi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3888,&
'frame/module_domain.f: Failed to deallocate grid%rimi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnwfa2d ) ) THEN 
  DEALLOCATE(grid%qnwfa2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3895,&
'frame/module_domain.f: Failed to deallocate grid%qnwfa2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnifa2d ) ) THEN 
  DEALLOCATE(grid%qnifa2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3902,&
'frame/module_domain.f: Failed to deallocate grid%qnifa2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%re_cloud ) ) THEN 
  DEALLOCATE(grid%re_cloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3909,&
'frame/module_domain.f: Failed to deallocate grid%re_cloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%re_ice ) ) THEN 
  DEALLOCATE(grid%re_ice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3916,&
'frame/module_domain.f: Failed to deallocate grid%re_ice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%re_snow ) ) THEN 
  DEALLOCATE(grid%re_snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3923,&
'frame/module_domain.f: Failed to deallocate grid%re_snow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_re_cloud ) ) THEN 
  DEALLOCATE(grid%dfi_re_cloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3930,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_cloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_re_ice ) ) THEN 
  DEALLOCATE(grid%dfi_re_ice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3937,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_ice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_re_snow ) ) THEN 
  DEALLOCATE(grid%dfi_re_snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3944,&
'frame/module_domain.f: Failed to deallocate grid%dfi_re_snow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%scalar ) ) THEN 
  DEALLOCATE(grid%scalar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3951,&
'frame/module_domain.f: Failed to deallocate grid%scalar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%scalar_bxs ) ) THEN 
  DEALLOCATE(grid%scalar_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3958,&
'frame/module_domain.f: Failed to deallocate grid%scalar_bxs. ')
 endif
  NULLIFY(grid%scalar_bxs)
ENDIF
IF ( ASSOCIATED( grid%scalar_bxe ) ) THEN 
  DEALLOCATE(grid%scalar_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3966,&
'frame/module_domain.f: Failed to deallocate grid%scalar_bxe. ')
 endif
  NULLIFY(grid%scalar_bxe)
ENDIF
IF ( ASSOCIATED( grid%scalar_bys ) ) THEN 
  DEALLOCATE(grid%scalar_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3974,&
'frame/module_domain.f: Failed to deallocate grid%scalar_bys. ')
 endif
  NULLIFY(grid%scalar_bys)
ENDIF
IF ( ASSOCIATED( grid%scalar_bye ) ) THEN 
  DEALLOCATE(grid%scalar_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3982,&
'frame/module_domain.f: Failed to deallocate grid%scalar_bye. ')
 endif
  NULLIFY(grid%scalar_bye)
ENDIF
IF ( ASSOCIATED( grid%scalar_btxs ) ) THEN 
  DEALLOCATE(grid%scalar_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3990,&
'frame/module_domain.f: Failed to deallocate grid%scalar_btxs. ')
 endif
  NULLIFY(grid%scalar_btxs)
ENDIF
IF ( ASSOCIATED( grid%scalar_btxe ) ) THEN 
  DEALLOCATE(grid%scalar_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",3998,&
'frame/module_domain.f: Failed to deallocate grid%scalar_btxe. ')
 endif
  NULLIFY(grid%scalar_btxe)
ENDIF
IF ( ASSOCIATED( grid%scalar_btys ) ) THEN 
  DEALLOCATE(grid%scalar_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4006,&
'frame/module_domain.f: Failed to deallocate grid%scalar_btys. ')
 endif
  NULLIFY(grid%scalar_btys)
ENDIF
IF ( ASSOCIATED( grid%scalar_btye ) ) THEN 
  DEALLOCATE(grid%scalar_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4014,&
'frame/module_domain.f: Failed to deallocate grid%scalar_btye. ')
 endif
  NULLIFY(grid%scalar_btye)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar ) ) THEN 
  DEALLOCATE(grid%dfi_scalar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4022,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bxs ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4029,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bxs. ')
 endif
  NULLIFY(grid%dfi_scalar_bxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bxe ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4037,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bxe. ')
 endif
  NULLIFY(grid%dfi_scalar_bxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bys ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4045,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bys. ')
 endif
  NULLIFY(grid%dfi_scalar_bys)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_bye ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4053,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_bye. ')
 endif
  NULLIFY(grid%dfi_scalar_bye)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btxs ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4061,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btxs. ')
 endif
  NULLIFY(grid%dfi_scalar_btxs)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btxe ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4069,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btxe. ')
 endif
  NULLIFY(grid%dfi_scalar_btxe)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btys ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4077,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btys. ')
 endif
  NULLIFY(grid%dfi_scalar_btys)
ENDIF
IF ( ASSOCIATED( grid%dfi_scalar_btye ) ) THEN 
  DEALLOCATE(grid%dfi_scalar_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4085,&
'frame/module_domain.f: Failed to deallocate grid%dfi_scalar_btye. ')
 endif
  NULLIFY(grid%dfi_scalar_btye)
ENDIF
IF ( ASSOCIATED( grid%fcx ) ) THEN 
  DEALLOCATE(grid%fcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4093,&
'frame/module_domain.f: Failed to deallocate grid%fcx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gcx ) ) THEN 
  DEALLOCATE(grid%gcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4100,&
'frame/module_domain.f: Failed to deallocate grid%gcx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soil_layers ) ) THEN 
  DEALLOCATE(grid%soil_layers,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4107,&
'frame/module_domain.f: Failed to deallocate grid%soil_layers. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soil_levels ) ) THEN 
  DEALLOCATE(grid%soil_levels,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4114,&
'frame/module_domain.f: Failed to deallocate grid%soil_levels. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st ) ) THEN 
  DEALLOCATE(grid%st,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4121,&
'frame/module_domain.f: Failed to deallocate grid%st. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm ) ) THEN 
  DEALLOCATE(grid%sm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4128,&
'frame/module_domain.f: Failed to deallocate grid%sm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw ) ) THEN 
  DEALLOCATE(grid%sw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4135,&
'frame/module_domain.f: Failed to deallocate grid%sw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt ) ) THEN 
  DEALLOCATE(grid%soilt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4142,&
'frame/module_domain.f: Failed to deallocate grid%soilt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm ) ) THEN 
  DEALLOCATE(grid%soilm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4149,&
'frame/module_domain.f: Failed to deallocate grid%soilm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm000007 ) ) THEN 
  DEALLOCATE(grid%sm000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4156,&
'frame/module_domain.f: Failed to deallocate grid%sm000007. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm007028 ) ) THEN 
  DEALLOCATE(grid%sm007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4163,&
'frame/module_domain.f: Failed to deallocate grid%sm007028. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm028100 ) ) THEN 
  DEALLOCATE(grid%sm028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4170,&
'frame/module_domain.f: Failed to deallocate grid%sm028100. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm100255 ) ) THEN 
  DEALLOCATE(grid%sm100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4177,&
'frame/module_domain.f: Failed to deallocate grid%sm100255. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st000007 ) ) THEN 
  DEALLOCATE(grid%st000007,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4184,&
'frame/module_domain.f: Failed to deallocate grid%st000007. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st007028 ) ) THEN 
  DEALLOCATE(grid%st007028,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4191,&
'frame/module_domain.f: Failed to deallocate grid%st007028. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st028100 ) ) THEN 
  DEALLOCATE(grid%st028100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4198,&
'frame/module_domain.f: Failed to deallocate grid%st028100. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st100255 ) ) THEN 
  DEALLOCATE(grid%st100255,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4205,&
'frame/module_domain.f: Failed to deallocate grid%st100255. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm000010 ) ) THEN 
  DEALLOCATE(grid%sm000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4212,&
'frame/module_domain.f: Failed to deallocate grid%sm000010. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm010040 ) ) THEN 
  DEALLOCATE(grid%sm010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4219,&
'frame/module_domain.f: Failed to deallocate grid%sm010040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm040100 ) ) THEN 
  DEALLOCATE(grid%sm040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4226,&
'frame/module_domain.f: Failed to deallocate grid%sm040100. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm100200 ) ) THEN 
  DEALLOCATE(grid%sm100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4233,&
'frame/module_domain.f: Failed to deallocate grid%sm100200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sm010200 ) ) THEN 
  DEALLOCATE(grid%sm010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4240,&
'frame/module_domain.f: Failed to deallocate grid%sm010200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm000 ) ) THEN 
  DEALLOCATE(grid%soilm000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4247,&
'frame/module_domain.f: Failed to deallocate grid%soilm000. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm005 ) ) THEN 
  DEALLOCATE(grid%soilm005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4254,&
'frame/module_domain.f: Failed to deallocate grid%soilm005. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm020 ) ) THEN 
  DEALLOCATE(grid%soilm020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4261,&
'frame/module_domain.f: Failed to deallocate grid%soilm020. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm040 ) ) THEN 
  DEALLOCATE(grid%soilm040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4268,&
'frame/module_domain.f: Failed to deallocate grid%soilm040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm160 ) ) THEN 
  DEALLOCATE(grid%soilm160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4275,&
'frame/module_domain.f: Failed to deallocate grid%soilm160. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilm300 ) ) THEN 
  DEALLOCATE(grid%soilm300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4282,&
'frame/module_domain.f: Failed to deallocate grid%soilm300. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw000010 ) ) THEN 
  DEALLOCATE(grid%sw000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4289,&
'frame/module_domain.f: Failed to deallocate grid%sw000010. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw010040 ) ) THEN 
  DEALLOCATE(grid%sw010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4296,&
'frame/module_domain.f: Failed to deallocate grid%sw010040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw040100 ) ) THEN 
  DEALLOCATE(grid%sw040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4303,&
'frame/module_domain.f: Failed to deallocate grid%sw040100. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw100200 ) ) THEN 
  DEALLOCATE(grid%sw100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4310,&
'frame/module_domain.f: Failed to deallocate grid%sw100200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sw010200 ) ) THEN 
  DEALLOCATE(grid%sw010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4317,&
'frame/module_domain.f: Failed to deallocate grid%sw010200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw000 ) ) THEN 
  DEALLOCATE(grid%soilw000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4324,&
'frame/module_domain.f: Failed to deallocate grid%soilw000. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw005 ) ) THEN 
  DEALLOCATE(grid%soilw005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4331,&
'frame/module_domain.f: Failed to deallocate grid%soilw005. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw020 ) ) THEN 
  DEALLOCATE(grid%soilw020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4338,&
'frame/module_domain.f: Failed to deallocate grid%soilw020. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw040 ) ) THEN 
  DEALLOCATE(grid%soilw040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4345,&
'frame/module_domain.f: Failed to deallocate grid%soilw040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw160 ) ) THEN 
  DEALLOCATE(grid%soilw160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4352,&
'frame/module_domain.f: Failed to deallocate grid%soilw160. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilw300 ) ) THEN 
  DEALLOCATE(grid%soilw300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4359,&
'frame/module_domain.f: Failed to deallocate grid%soilw300. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st000010 ) ) THEN 
  DEALLOCATE(grid%st000010,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4366,&
'frame/module_domain.f: Failed to deallocate grid%st000010. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st010040 ) ) THEN 
  DEALLOCATE(grid%st010040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4373,&
'frame/module_domain.f: Failed to deallocate grid%st010040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st040100 ) ) THEN 
  DEALLOCATE(grid%st040100,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4380,&
'frame/module_domain.f: Failed to deallocate grid%st040100. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st100200 ) ) THEN 
  DEALLOCATE(grid%st100200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4387,&
'frame/module_domain.f: Failed to deallocate grid%st100200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%st010200 ) ) THEN 
  DEALLOCATE(grid%st010200,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4394,&
'frame/module_domain.f: Failed to deallocate grid%st010200. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt000 ) ) THEN 
  DEALLOCATE(grid%soilt000,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4401,&
'frame/module_domain.f: Failed to deallocate grid%soilt000. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt005 ) ) THEN 
  DEALLOCATE(grid%soilt005,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4408,&
'frame/module_domain.f: Failed to deallocate grid%soilt005. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt020 ) ) THEN 
  DEALLOCATE(grid%soilt020,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4415,&
'frame/module_domain.f: Failed to deallocate grid%soilt020. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt040 ) ) THEN 
  DEALLOCATE(grid%soilt040,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4422,&
'frame/module_domain.f: Failed to deallocate grid%soilt040. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt160 ) ) THEN 
  DEALLOCATE(grid%soilt160,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4429,&
'frame/module_domain.f: Failed to deallocate grid%soilt160. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt300 ) ) THEN 
  DEALLOCATE(grid%soilt300,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4436,&
'frame/module_domain.f: Failed to deallocate grid%soilt300. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%topostdv ) ) THEN 
  DEALLOCATE(grid%topostdv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4443,&
'frame/module_domain.f: Failed to deallocate grid%topostdv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%toposlpx ) ) THEN 
  DEALLOCATE(grid%toposlpx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4450,&
'frame/module_domain.f: Failed to deallocate grid%toposlpx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%toposlpy ) ) THEN 
  DEALLOCATE(grid%toposlpy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4457,&
'frame/module_domain.f: Failed to deallocate grid%toposlpy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slope ) ) THEN 
  DEALLOCATE(grid%slope,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4464,&
'frame/module_domain.f: Failed to deallocate grid%slope. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slp_azi ) ) THEN 
  DEALLOCATE(grid%slp_azi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4471,&
'frame/module_domain.f: Failed to deallocate grid%slp_azi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shdmax ) ) THEN 
  DEALLOCATE(grid%shdmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4478,&
'frame/module_domain.f: Failed to deallocate grid%shdmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shdmin ) ) THEN 
  DEALLOCATE(grid%shdmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4485,&
'frame/module_domain.f: Failed to deallocate grid%shdmin. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snoalb ) ) THEN 
  DEALLOCATE(grid%snoalb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4492,&
'frame/module_domain.f: Failed to deallocate grid%snoalb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%toposoil ) ) THEN 
  DEALLOCATE(grid%toposoil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4499,&
'frame/module_domain.f: Failed to deallocate grid%toposoil. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%landusef ) ) THEN 
  DEALLOCATE(grid%landusef,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4506,&
'frame/module_domain.f: Failed to deallocate grid%landusef. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilctop ) ) THEN 
  DEALLOCATE(grid%soilctop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4513,&
'frame/module_domain.f: Failed to deallocate grid%soilctop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilcbot ) ) THEN 
  DEALLOCATE(grid%soilcbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4520,&
'frame/module_domain.f: Failed to deallocate grid%soilcbot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilcat ) ) THEN 
  DEALLOCATE(grid%soilcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4527,&
'frame/module_domain.f: Failed to deallocate grid%soilcat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vegcat ) ) THEN 
  DEALLOCATE(grid%vegcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4534,&
'frame/module_domain.f: Failed to deallocate grid%vegcat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tslb ) ) THEN 
  DEALLOCATE(grid%tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4541,&
'frame/module_domain.f: Failed to deallocate grid%tslb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_hour ) ) THEN 
  DEALLOCATE(grid%ts_hour,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4548,&
'frame/module_domain.f: Failed to deallocate grid%ts_hour. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_u ) ) THEN 
  DEALLOCATE(grid%ts_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4555,&
'frame/module_domain.f: Failed to deallocate grid%ts_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_v ) ) THEN 
  DEALLOCATE(grid%ts_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4562,&
'frame/module_domain.f: Failed to deallocate grid%ts_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_q ) ) THEN 
  DEALLOCATE(grid%ts_q,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4569,&
'frame/module_domain.f: Failed to deallocate grid%ts_q. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_t ) ) THEN 
  DEALLOCATE(grid%ts_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4576,&
'frame/module_domain.f: Failed to deallocate grid%ts_t. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_psfc ) ) THEN 
  DEALLOCATE(grid%ts_psfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4583,&
'frame/module_domain.f: Failed to deallocate grid%ts_psfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_glw ) ) THEN 
  DEALLOCATE(grid%ts_glw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4590,&
'frame/module_domain.f: Failed to deallocate grid%ts_glw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_gsw ) ) THEN 
  DEALLOCATE(grid%ts_gsw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4597,&
'frame/module_domain.f: Failed to deallocate grid%ts_gsw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_hfx ) ) THEN 
  DEALLOCATE(grid%ts_hfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4604,&
'frame/module_domain.f: Failed to deallocate grid%ts_hfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_lh ) ) THEN 
  DEALLOCATE(grid%ts_lh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4611,&
'frame/module_domain.f: Failed to deallocate grid%ts_lh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_tsk ) ) THEN 
  DEALLOCATE(grid%ts_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4618,&
'frame/module_domain.f: Failed to deallocate grid%ts_tsk. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_tslb ) ) THEN 
  DEALLOCATE(grid%ts_tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4625,&
'frame/module_domain.f: Failed to deallocate grid%ts_tslb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_clw ) ) THEN 
  DEALLOCATE(grid%ts_clw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4632,&
'frame/module_domain.f: Failed to deallocate grid%ts_clw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_rainc ) ) THEN 
  DEALLOCATE(grid%ts_rainc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4639,&
'frame/module_domain.f: Failed to deallocate grid%ts_rainc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_rainnc ) ) THEN 
  DEALLOCATE(grid%ts_rainnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4646,&
'frame/module_domain.f: Failed to deallocate grid%ts_rainnc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_u_profile ) ) THEN 
  DEALLOCATE(grid%ts_u_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4653,&
'frame/module_domain.f: Failed to deallocate grid%ts_u_profile. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_v_profile ) ) THEN 
  DEALLOCATE(grid%ts_v_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4660,&
'frame/module_domain.f: Failed to deallocate grid%ts_v_profile. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_gph_profile ) ) THEN 
  DEALLOCATE(grid%ts_gph_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4667,&
'frame/module_domain.f: Failed to deallocate grid%ts_gph_profile. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_th_profile ) ) THEN 
  DEALLOCATE(grid%ts_th_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4674,&
'frame/module_domain.f: Failed to deallocate grid%ts_th_profile. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_qv_profile ) ) THEN 
  DEALLOCATE(grid%ts_qv_profile,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4681,&
'frame/module_domain.f: Failed to deallocate grid%ts_qv_profile. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzr ) ) THEN 
  DEALLOCATE(grid%dzr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4688,&
'frame/module_domain.f: Failed to deallocate grid%dzr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzb ) ) THEN 
  DEALLOCATE(grid%dzb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4695,&
'frame/module_domain.f: Failed to deallocate grid%dzb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzg ) ) THEN 
  DEALLOCATE(grid%dzg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4702,&
'frame/module_domain.f: Failed to deallocate grid%dzg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%urb_param ) ) THEN 
  DEALLOCATE(grid%urb_param,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4709,&
'frame/module_domain.f: Failed to deallocate grid%urb_param. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lp_urb2d ) ) THEN 
  DEALLOCATE(grid%lp_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4716,&
'frame/module_domain.f: Failed to deallocate grid%lp_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hi_urb2d ) ) THEN 
  DEALLOCATE(grid%hi_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4723,&
'frame/module_domain.f: Failed to deallocate grid%hi_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lb_urb2d ) ) THEN 
  DEALLOCATE(grid%lb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4730,&
'frame/module_domain.f: Failed to deallocate grid%lb_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hgt_urb2d ) ) THEN 
  DEALLOCATE(grid%hgt_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4737,&
'frame/module_domain.f: Failed to deallocate grid%hgt_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fad0_urb2d ) ) THEN 
  DEALLOCATE(grid%fad0_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4744,&
'frame/module_domain.f: Failed to deallocate grid%fad0_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fad135_urb2d ) ) THEN 
  DEALLOCATE(grid%fad135_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4751,&
'frame/module_domain.f: Failed to deallocate grid%fad135_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fad45_urb2d ) ) THEN 
  DEALLOCATE(grid%fad45_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4758,&
'frame/module_domain.f: Failed to deallocate grid%fad45_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pad_urb2d ) ) THEN 
  DEALLOCATE(grid%pad_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4765,&
'frame/module_domain.f: Failed to deallocate grid%pad_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fad90_urb2d ) ) THEN 
  DEALLOCATE(grid%fad90_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4772,&
'frame/module_domain.f: Failed to deallocate grid%fad90_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rad_urb2d ) ) THEN 
  DEALLOCATE(grid%rad_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4779,&
'frame/module_domain.f: Failed to deallocate grid%rad_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mh_urb2d ) ) THEN 
  DEALLOCATE(grid%mh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4786,&
'frame/module_domain.f: Failed to deallocate grid%mh_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%stdh_urb2d ) ) THEN 
  DEALLOCATE(grid%stdh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4793,&
'frame/module_domain.f: Failed to deallocate grid%stdh_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lf_urb2d ) ) THEN 
  DEALLOCATE(grid%lf_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4800,&
'frame/module_domain.f: Failed to deallocate grid%lf_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%car_urb2d ) ) THEN 
  DEALLOCATE(grid%car_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4807,&
'frame/module_domain.f: Failed to deallocate grid%car_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2w_urb2d ) ) THEN 
  DEALLOCATE(grid%h2w_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4814,&
'frame/module_domain.f: Failed to deallocate grid%h2w_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%svf_urb2d ) ) THEN 
  DEALLOCATE(grid%svf_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4821,&
'frame/module_domain.f: Failed to deallocate grid%svf_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z0s_urb2d ) ) THEN 
  DEALLOCATE(grid%z0s_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4828,&
'frame/module_domain.f: Failed to deallocate grid%z0s_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z0r_urb2d ) ) THEN 
  DEALLOCATE(grid%z0r_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4835,&
'frame/module_domain.f: Failed to deallocate grid%z0r_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z0m_urb2d ) ) THEN 
  DEALLOCATE(grid%z0m_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4842,&
'frame/module_domain.f: Failed to deallocate grid%z0m_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zds_urb2d ) ) THEN 
  DEALLOCATE(grid%zds_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4849,&
'frame/module_domain.f: Failed to deallocate grid%zds_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zdm_urb2d ) ) THEN 
  DEALLOCATE(grid%zdm_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4856,&
'frame/module_domain.f: Failed to deallocate grid%zdm_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zdr_urb2d ) ) THEN 
  DEALLOCATE(grid%zdr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4863,&
'frame/module_domain.f: Failed to deallocate grid%zdr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smois ) ) THEN 
  DEALLOCATE(grid%smois,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4870,&
'frame/module_domain.f: Failed to deallocate grid%smois. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh2o ) ) THEN 
  DEALLOCATE(grid%sh2o,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4877,&
'frame/module_domain.f: Failed to deallocate grid%sh2o. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smcrel ) ) THEN 
  DEALLOCATE(grid%smcrel,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4884,&
'frame/module_domain.f: Failed to deallocate grid%smcrel. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xice ) ) THEN 
  DEALLOCATE(grid%xice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4891,&
'frame/module_domain.f: Failed to deallocate grid%xice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icedepth ) ) THEN 
  DEALLOCATE(grid%icedepth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4898,&
'frame/module_domain.f: Failed to deallocate grid%icedepth. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xicem ) ) THEN 
  DEALLOCATE(grid%xicem,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4905,&
'frame/module_domain.f: Failed to deallocate grid%xicem. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albsi ) ) THEN 
  DEALLOCATE(grid%albsi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4912,&
'frame/module_domain.f: Failed to deallocate grid%albsi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowsi ) ) THEN 
  DEALLOCATE(grid%snowsi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4919,&
'frame/module_domain.f: Failed to deallocate grid%snowsi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smstav ) ) THEN 
  DEALLOCATE(grid%smstav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4926,&
'frame/module_domain.f: Failed to deallocate grid%smstav. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smstot ) ) THEN 
  DEALLOCATE(grid%smstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4933,&
'frame/module_domain.f: Failed to deallocate grid%smstot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soldrain ) ) THEN 
  DEALLOCATE(grid%soldrain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4940,&
'frame/module_domain.f: Failed to deallocate grid%soldrain. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfcheadrt ) ) THEN 
  DEALLOCATE(grid%sfcheadrt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4947,&
'frame/module_domain.f: Failed to deallocate grid%sfcheadrt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%infxsrt ) ) THEN 
  DEALLOCATE(grid%infxsrt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4954,&
'frame/module_domain.f: Failed to deallocate grid%infxsrt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfcrunoff ) ) THEN 
  DEALLOCATE(grid%sfcrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4961,&
'frame/module_domain.f: Failed to deallocate grid%sfcrunoff. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%udrunoff ) ) THEN 
  DEALLOCATE(grid%udrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4968,&
'frame/module_domain.f: Failed to deallocate grid%udrunoff. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ivgtyp ) ) THEN 
  DEALLOCATE(grid%ivgtyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4975,&
'frame/module_domain.f: Failed to deallocate grid%ivgtyp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%isltyp ) ) THEN 
  DEALLOCATE(grid%isltyp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4982,&
'frame/module_domain.f: Failed to deallocate grid%isltyp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vegfra ) ) THEN 
  DEALLOCATE(grid%vegfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4989,&
'frame/module_domain.f: Failed to deallocate grid%vegfra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfcevp ) ) THEN 
  DEALLOCATE(grid%sfcevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",4996,&
'frame/module_domain.f: Failed to deallocate grid%sfcevp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grdflx ) ) THEN 
  DEALLOCATE(grid%grdflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5003,&
'frame/module_domain.f: Failed to deallocate grid%grdflx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acgrdflx ) ) THEN 
  DEALLOCATE(grid%acgrdflx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5010,&
'frame/module_domain.f: Failed to deallocate grid%acgrdflx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfcexc ) ) THEN 
  DEALLOCATE(grid%sfcexc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5017,&
'frame/module_domain.f: Failed to deallocate grid%sfcexc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acsnow ) ) THEN 
  DEALLOCATE(grid%acsnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5024,&
'frame/module_domain.f: Failed to deallocate grid%acsnow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acrunoff ) ) THEN 
  DEALLOCATE(grid%acrunoff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5031,&
'frame/module_domain.f: Failed to deallocate grid%acrunoff. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acsnom ) ) THEN 
  DEALLOCATE(grid%acsnom,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5038,&
'frame/module_domain.f: Failed to deallocate grid%acsnom. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snow ) ) THEN 
  DEALLOCATE(grid%snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5045,&
'frame/module_domain.f: Failed to deallocate grid%snow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowh ) ) THEN 
  DEALLOCATE(grid%snowh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5052,&
'frame/module_domain.f: Failed to deallocate grid%snowh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canwat ) ) THEN 
  DEALLOCATE(grid%canwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5059,&
'frame/module_domain.f: Failed to deallocate grid%canwat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlaidyn ) ) THEN 
  DEALLOCATE(grid%xlaidyn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5066,&
'frame/module_domain.f: Failed to deallocate grid%xlaidyn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sstsk ) ) THEN 
  DEALLOCATE(grid%sstsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5073,&
'frame/module_domain.f: Failed to deallocate grid%sstsk. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lake_depth ) ) THEN 
  DEALLOCATE(grid%lake_depth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5080,&
'frame/module_domain.f: Failed to deallocate grid%lake_depth. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dtw ) ) THEN 
  DEALLOCATE(grid%dtw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5087,&
'frame/module_domain.f: Failed to deallocate grid%dtw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uoce ) ) THEN 
  DEALLOCATE(grid%uoce,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5094,&
'frame/module_domain.f: Failed to deallocate grid%uoce. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%voce ) ) THEN 
  DEALLOCATE(grid%voce,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5101,&
'frame/module_domain.f: Failed to deallocate grid%voce. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hcoeff ) ) THEN 
  DEALLOCATE(grid%hcoeff,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5108,&
'frame/module_domain.f: Failed to deallocate grid%hcoeff. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_p ) ) THEN 
  DEALLOCATE(grid%dfi_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5115,&
'frame/module_domain.f: Failed to deallocate grid%dfi_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_al ) ) THEN 
  DEALLOCATE(grid%dfi_al,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5122,&
'frame/module_domain.f: Failed to deallocate grid%dfi_al. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_mu ) ) THEN 
  DEALLOCATE(grid%dfi_mu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5129,&
'frame/module_domain.f: Failed to deallocate grid%dfi_mu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_phb ) ) THEN 
  DEALLOCATE(grid%dfi_phb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5136,&
'frame/module_domain.f: Failed to deallocate grid%dfi_phb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_ph0 ) ) THEN 
  DEALLOCATE(grid%dfi_ph0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5143,&
'frame/module_domain.f: Failed to deallocate grid%dfi_ph0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_php ) ) THEN 
  DEALLOCATE(grid%dfi_php,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5150,&
'frame/module_domain.f: Failed to deallocate grid%dfi_php. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_u ) ) THEN 
  DEALLOCATE(grid%dfi_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5157,&
'frame/module_domain.f: Failed to deallocate grid%dfi_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_v ) ) THEN 
  DEALLOCATE(grid%dfi_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5164,&
'frame/module_domain.f: Failed to deallocate grid%dfi_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_w ) ) THEN 
  DEALLOCATE(grid%dfi_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5171,&
'frame/module_domain.f: Failed to deallocate grid%dfi_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_ww ) ) THEN 
  DEALLOCATE(grid%dfi_ww,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5178,&
'frame/module_domain.f: Failed to deallocate grid%dfi_ww. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_t ) ) THEN 
  DEALLOCATE(grid%dfi_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5185,&
'frame/module_domain.f: Failed to deallocate grid%dfi_t. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_rh ) ) THEN 
  DEALLOCATE(grid%dfi_rh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5192,&
'frame/module_domain.f: Failed to deallocate grid%dfi_rh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_ph ) ) THEN 
  DEALLOCATE(grid%dfi_ph,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5199,&
'frame/module_domain.f: Failed to deallocate grid%dfi_ph. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_pb ) ) THEN 
  DEALLOCATE(grid%dfi_pb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5206,&
'frame/module_domain.f: Failed to deallocate grid%dfi_pb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_alt ) ) THEN 
  DEALLOCATE(grid%dfi_alt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5213,&
'frame/module_domain.f: Failed to deallocate grid%dfi_alt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_tke ) ) THEN 
  DEALLOCATE(grid%dfi_tke,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5220,&
'frame/module_domain.f: Failed to deallocate grid%dfi_tke. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_tten_rad ) ) THEN 
  DEALLOCATE(grid%dfi_tten_rad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5227,&
'frame/module_domain.f: Failed to deallocate grid%dfi_tten_rad. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_tslb ) ) THEN 
  DEALLOCATE(grid%dfi_tslb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5234,&
'frame/module_domain.f: Failed to deallocate grid%dfi_tslb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_smois ) ) THEN 
  DEALLOCATE(grid%dfi_smois,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5241,&
'frame/module_domain.f: Failed to deallocate grid%dfi_smois. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_snow ) ) THEN 
  DEALLOCATE(grid%dfi_snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5248,&
'frame/module_domain.f: Failed to deallocate grid%dfi_snow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_snowh ) ) THEN 
  DEALLOCATE(grid%dfi_snowh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5255,&
'frame/module_domain.f: Failed to deallocate grid%dfi_snowh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_canwat ) ) THEN 
  DEALLOCATE(grid%dfi_canwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5262,&
'frame/module_domain.f: Failed to deallocate grid%dfi_canwat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_smfr3d ) ) THEN 
  DEALLOCATE(grid%dfi_smfr3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5269,&
'frame/module_domain.f: Failed to deallocate grid%dfi_smfr3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_keepfr3dflag ) ) THEN 
  DEALLOCATE(grid%dfi_keepfr3dflag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5276,&
'frame/module_domain.f: Failed to deallocate grid%dfi_keepfr3dflag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_rural ) ) THEN 
  DEALLOCATE(grid%tsk_rural,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5283,&
'frame/module_domain.f: Failed to deallocate grid%tsk_rural. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tr_urb2d ) ) THEN 
  DEALLOCATE(grid%tr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5290,&
'frame/module_domain.f: Failed to deallocate grid%tr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgr_urb2d ) ) THEN 
  DEALLOCATE(grid%tgr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5297,&
'frame/module_domain.f: Failed to deallocate grid%tgr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tb_urb2d ) ) THEN 
  DEALLOCATE(grid%tb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5304,&
'frame/module_domain.f: Failed to deallocate grid%tb_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tg_urb2d ) ) THEN 
  DEALLOCATE(grid%tg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5311,&
'frame/module_domain.f: Failed to deallocate grid%tg_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tc_urb2d ) ) THEN 
  DEALLOCATE(grid%tc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5318,&
'frame/module_domain.f: Failed to deallocate grid%tc_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_urb2d ) ) THEN 
  DEALLOCATE(grid%qc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5325,&
'frame/module_domain.f: Failed to deallocate grid%qc_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uc_urb2d ) ) THEN 
  DEALLOCATE(grid%uc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5332,&
'frame/module_domain.f: Failed to deallocate grid%uc_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xxxr_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5339,&
'frame/module_domain.f: Failed to deallocate grid%xxxr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xxxb_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5346,&
'frame/module_domain.f: Failed to deallocate grid%xxxb_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xxxg_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5353,&
'frame/module_domain.f: Failed to deallocate grid%xxxg_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xxxc_urb2d ) ) THEN 
  DEALLOCATE(grid%xxxc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5360,&
'frame/module_domain.f: Failed to deallocate grid%xxxc_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmcr_urb2d ) ) THEN 
  DEALLOCATE(grid%cmcr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5367,&
'frame/module_domain.f: Failed to deallocate grid%cmcr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%drelr_urb2d ) ) THEN 
  DEALLOCATE(grid%drelr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5374,&
'frame/module_domain.f: Failed to deallocate grid%drelr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%drelb_urb2d ) ) THEN 
  DEALLOCATE(grid%drelb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5381,&
'frame/module_domain.f: Failed to deallocate grid%drelb_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%drelg_urb2d ) ) THEN 
  DEALLOCATE(grid%drelg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5388,&
'frame/module_domain.f: Failed to deallocate grid%drelg_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flxhumr_urb2d ) ) THEN 
  DEALLOCATE(grid%flxhumr_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5395,&
'frame/module_domain.f: Failed to deallocate grid%flxhumr_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flxhumb_urb2d ) ) THEN 
  DEALLOCATE(grid%flxhumb_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5402,&
'frame/module_domain.f: Failed to deallocate grid%flxhumb_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flxhumg_urb2d ) ) THEN 
  DEALLOCATE(grid%flxhumg_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5409,&
'frame/module_domain.f: Failed to deallocate grid%flxhumg_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgrl_urb3d ) ) THEN 
  DEALLOCATE(grid%tgrl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5416,&
'frame/module_domain.f: Failed to deallocate grid%tgrl_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smr_urb3d ) ) THEN 
  DEALLOCATE(grid%smr_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5423,&
'frame/module_domain.f: Failed to deallocate grid%smr_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%trl_urb3d ) ) THEN 
  DEALLOCATE(grid%trl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5430,&
'frame/module_domain.f: Failed to deallocate grid%trl_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tbl_urb3d ) ) THEN 
  DEALLOCATE(grid%tbl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5437,&
'frame/module_domain.f: Failed to deallocate grid%tbl_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgl_urb3d ) ) THEN 
  DEALLOCATE(grid%tgl_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5444,&
'frame/module_domain.f: Failed to deallocate grid%tgl_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh_urb2d ) ) THEN 
  DEALLOCATE(grid%sh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5451,&
'frame/module_domain.f: Failed to deallocate grid%sh_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_urb2d ) ) THEN 
  DEALLOCATE(grid%lh_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5458,&
'frame/module_domain.f: Failed to deallocate grid%lh_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%g_urb2d ) ) THEN 
  DEALLOCATE(grid%g_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5465,&
'frame/module_domain.f: Failed to deallocate grid%g_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rn_urb2d ) ) THEN 
  DEALLOCATE(grid%rn_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5472,&
'frame/module_domain.f: Failed to deallocate grid%rn_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_urb2d ) ) THEN 
  DEALLOCATE(grid%ts_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5479,&
'frame/module_domain.f: Failed to deallocate grid%ts_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%frc_urb2d ) ) THEN 
  DEALLOCATE(grid%frc_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5486,&
'frame/module_domain.f: Failed to deallocate grid%frc_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%utype_urb2d ) ) THEN 
  DEALLOCATE(grid%utype_urb2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5493,&
'frame/module_domain.f: Failed to deallocate grid%utype_urb2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%trb_urb4d ) ) THEN 
  DEALLOCATE(grid%trb_urb4d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5500,&
'frame/module_domain.f: Failed to deallocate grid%trb_urb4d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tw1_urb4d ) ) THEN 
  DEALLOCATE(grid%tw1_urb4d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5507,&
'frame/module_domain.f: Failed to deallocate grid%tw1_urb4d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tw2_urb4d ) ) THEN 
  DEALLOCATE(grid%tw2_urb4d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5514,&
'frame/module_domain.f: Failed to deallocate grid%tw2_urb4d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgb_urb4d ) ) THEN 
  DEALLOCATE(grid%tgb_urb4d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5521,&
'frame/module_domain.f: Failed to deallocate grid%tgb_urb4d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tlev_urb3d ) ) THEN 
  DEALLOCATE(grid%tlev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5528,&
'frame/module_domain.f: Failed to deallocate grid%tlev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qlev_urb3d ) ) THEN 
  DEALLOCATE(grid%qlev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5535,&
'frame/module_domain.f: Failed to deallocate grid%qlev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tw1lev_urb3d ) ) THEN 
  DEALLOCATE(grid%tw1lev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5542,&
'frame/module_domain.f: Failed to deallocate grid%tw1lev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tw2lev_urb3d ) ) THEN 
  DEALLOCATE(grid%tw2lev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5549,&
'frame/module_domain.f: Failed to deallocate grid%tw2lev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tglev_urb3d ) ) THEN 
  DEALLOCATE(grid%tglev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5556,&
'frame/module_domain.f: Failed to deallocate grid%tglev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tflev_urb3d ) ) THEN 
  DEALLOCATE(grid%tflev_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5563,&
'frame/module_domain.f: Failed to deallocate grid%tflev_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sf_ac_urb3d ) ) THEN 
  DEALLOCATE(grid%sf_ac_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5570,&
'frame/module_domain.f: Failed to deallocate grid%sf_ac_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lf_ac_urb3d ) ) THEN 
  DEALLOCATE(grid%lf_ac_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5577,&
'frame/module_domain.f: Failed to deallocate grid%lf_ac_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cm_ac_urb3d ) ) THEN 
  DEALLOCATE(grid%cm_ac_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5584,&
'frame/module_domain.f: Failed to deallocate grid%cm_ac_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfvent_urb3d ) ) THEN 
  DEALLOCATE(grid%sfvent_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5591,&
'frame/module_domain.f: Failed to deallocate grid%sfvent_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfvent_urb3d ) ) THEN 
  DEALLOCATE(grid%lfvent_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5598,&
'frame/module_domain.f: Failed to deallocate grid%lfvent_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfwin1_urb3d ) ) THEN 
  DEALLOCATE(grid%sfwin1_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5605,&
'frame/module_domain.f: Failed to deallocate grid%sfwin1_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfwin2_urb3d ) ) THEN 
  DEALLOCATE(grid%sfwin2_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5612,&
'frame/module_domain.f: Failed to deallocate grid%sfwin2_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfw1_urb3d ) ) THEN 
  DEALLOCATE(grid%sfw1_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5619,&
'frame/module_domain.f: Failed to deallocate grid%sfw1_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfw2_urb3d ) ) THEN 
  DEALLOCATE(grid%sfw2_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5626,&
'frame/module_domain.f: Failed to deallocate grid%sfw2_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfr_urb3d ) ) THEN 
  DEALLOCATE(grid%sfr_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5633,&
'frame/module_domain.f: Failed to deallocate grid%sfr_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sfg_urb3d ) ) THEN 
  DEALLOCATE(grid%sfg_urb3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5640,&
'frame/module_domain.f: Failed to deallocate grid%sfg_urb3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmr_sfcdif ) ) THEN 
  DEALLOCATE(grid%cmr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5647,&
'frame/module_domain.f: Failed to deallocate grid%cmr_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chr_sfcdif ) ) THEN 
  DEALLOCATE(grid%chr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5654,&
'frame/module_domain.f: Failed to deallocate grid%chr_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmc_sfcdif ) ) THEN 
  DEALLOCATE(grid%cmc_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5661,&
'frame/module_domain.f: Failed to deallocate grid%cmc_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chc_sfcdif ) ) THEN 
  DEALLOCATE(grid%chc_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5668,&
'frame/module_domain.f: Failed to deallocate grid%chc_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmgr_sfcdif ) ) THEN 
  DEALLOCATE(grid%cmgr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5675,&
'frame/module_domain.f: Failed to deallocate grid%cmgr_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chgr_sfcdif ) ) THEN 
  DEALLOCATE(grid%chgr_sfcdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5682,&
'frame/module_domain.f: Failed to deallocate grid%chgr_sfcdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%coszen ) ) THEN 
  DEALLOCATE(grid%coszen,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5689,&
'frame/module_domain.f: Failed to deallocate grid%coszen. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hrang ) ) THEN 
  DEALLOCATE(grid%hrang,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5696,&
'frame/module_domain.f: Failed to deallocate grid%hrang. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rhosnf ) ) THEN 
  DEALLOCATE(grid%rhosnf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5703,&
'frame/module_domain.f: Failed to deallocate grid%rhosnf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowfallac ) ) THEN 
  DEALLOCATE(grid%snowfallac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5710,&
'frame/module_domain.f: Failed to deallocate grid%snowfallac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%precipfr ) ) THEN 
  DEALLOCATE(grid%precipfr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5717,&
'frame/module_domain.f: Failed to deallocate grid%precipfr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smfr3d ) ) THEN 
  DEALLOCATE(grid%smfr3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5724,&
'frame/module_domain.f: Failed to deallocate grid%smfr3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%keepfr3dflag ) ) THEN 
  DEALLOCATE(grid%keepfr3dflag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5731,&
'frame/module_domain.f: Failed to deallocate grid%keepfr3dflag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swvisdir ) ) THEN 
  DEALLOCATE(grid%swvisdir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5738,&
'frame/module_domain.f: Failed to deallocate grid%swvisdir. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swvisdif ) ) THEN 
  DEALLOCATE(grid%swvisdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5745,&
'frame/module_domain.f: Failed to deallocate grid%swvisdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swnirdir ) ) THEN 
  DEALLOCATE(grid%swnirdir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5752,&
'frame/module_domain.f: Failed to deallocate grid%swnirdir. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swnirdif ) ) THEN 
  DEALLOCATE(grid%swnirdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5759,&
'frame/module_domain.f: Failed to deallocate grid%swnirdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alswvisdir ) ) THEN 
  DEALLOCATE(grid%alswvisdir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5766,&
'frame/module_domain.f: Failed to deallocate grid%alswvisdir. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alswvisdif ) ) THEN 
  DEALLOCATE(grid%alswvisdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5773,&
'frame/module_domain.f: Failed to deallocate grid%alswvisdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alswnirdir ) ) THEN 
  DEALLOCATE(grid%alswnirdir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5780,&
'frame/module_domain.f: Failed to deallocate grid%alswnirdir. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alswnirdif ) ) THEN 
  DEALLOCATE(grid%alswnirdif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5787,&
'frame/module_domain.f: Failed to deallocate grid%alswnirdif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ra ) ) THEN 
  DEALLOCATE(grid%ra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5794,&
'frame/module_domain.f: Failed to deallocate grid%ra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rs ) ) THEN 
  DEALLOCATE(grid%rs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5801,&
'frame/module_domain.f: Failed to deallocate grid%rs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lai ) ) THEN 
  DEALLOCATE(grid%lai,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5808,&
'frame/module_domain.f: Failed to deallocate grid%lai. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vegf_px ) ) THEN 
  DEALLOCATE(grid%vegf_px,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5815,&
'frame/module_domain.f: Failed to deallocate grid%vegf_px. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2obs ) ) THEN 
  DEALLOCATE(grid%t2obs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5822,&
'frame/module_domain.f: Failed to deallocate grid%t2obs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2obs ) ) THEN 
  DEALLOCATE(grid%q2obs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5829,&
'frame/module_domain.f: Failed to deallocate grid%q2obs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%imperv ) ) THEN 
  DEALLOCATE(grid%imperv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5836,&
'frame/module_domain.f: Failed to deallocate grid%imperv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canfra ) ) THEN 
  DEALLOCATE(grid%canfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5843,&
'frame/module_domain.f: Failed to deallocate grid%canfra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fm ) ) THEN 
  DEALLOCATE(grid%fm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5850,&
'frame/module_domain.f: Failed to deallocate grid%fm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fh ) ) THEN 
  DEALLOCATE(grid%fh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5857,&
'frame/module_domain.f: Failed to deallocate grid%fh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wspd ) ) THEN 
  DEALLOCATE(grid%wspd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5864,&
'frame/module_domain.f: Failed to deallocate grid%wspd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%br ) ) THEN 
  DEALLOCATE(grid%br,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5871,&
'frame/module_domain.f: Failed to deallocate grid%br. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zol ) ) THEN 
  DEALLOCATE(grid%zol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5878,&
'frame/module_domain.f: Failed to deallocate grid%zol. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wstar_ysu ) ) THEN 
  DEALLOCATE(grid%wstar_ysu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5885,&
'frame/module_domain.f: Failed to deallocate grid%wstar_ysu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%delta_ysu ) ) THEN 
  DEALLOCATE(grid%delta_ysu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5892,&
'frame/module_domain.f: Failed to deallocate grid%delta_ysu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exch_h ) ) THEN 
  DEALLOCATE(grid%exch_h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5899,&
'frame/module_domain.f: Failed to deallocate grid%exch_h. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exch_m ) ) THEN 
  DEALLOCATE(grid%exch_m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5906,&
'frame/module_domain.f: Failed to deallocate grid%exch_m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ct ) ) THEN 
  DEALLOCATE(grid%ct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5913,&
'frame/module_domain.f: Failed to deallocate grid%ct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thz0 ) ) THEN 
  DEALLOCATE(grid%thz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5920,&
'frame/module_domain.f: Failed to deallocate grid%thz0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z0 ) ) THEN 
  DEALLOCATE(grid%z0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5927,&
'frame/module_domain.f: Failed to deallocate grid%z0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qz0 ) ) THEN 
  DEALLOCATE(grid%qz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5934,&
'frame/module_domain.f: Failed to deallocate grid%qz0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uz0 ) ) THEN 
  DEALLOCATE(grid%uz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5941,&
'frame/module_domain.f: Failed to deallocate grid%uz0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vz0 ) ) THEN 
  DEALLOCATE(grid%vz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5948,&
'frame/module_domain.f: Failed to deallocate grid%vz0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsfc ) ) THEN 
  DEALLOCATE(grid%qsfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5955,&
'frame/module_domain.f: Failed to deallocate grid%qsfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%akhs ) ) THEN 
  DEALLOCATE(grid%akhs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5962,&
'frame/module_domain.f: Failed to deallocate grid%akhs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%akms ) ) THEN 
  DEALLOCATE(grid%akms,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5969,&
'frame/module_domain.f: Failed to deallocate grid%akms. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kpbl ) ) THEN 
  DEALLOCATE(grid%kpbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5976,&
'frame/module_domain.f: Failed to deallocate grid%kpbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%akpbl ) ) THEN 
  DEALLOCATE(grid%akpbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5983,&
'frame/module_domain.f: Failed to deallocate grid%akpbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tshltr ) ) THEN 
  DEALLOCATE(grid%tshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5990,&
'frame/module_domain.f: Failed to deallocate grid%tshltr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qshltr ) ) THEN 
  DEALLOCATE(grid%qshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",5997,&
'frame/module_domain.f: Failed to deallocate grid%qshltr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pshltr ) ) THEN 
  DEALLOCATE(grid%pshltr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6004,&
'frame/module_domain.f: Failed to deallocate grid%pshltr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th10 ) ) THEN 
  DEALLOCATE(grid%th10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6011,&
'frame/module_domain.f: Failed to deallocate grid%th10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q10 ) ) THEN 
  DEALLOCATE(grid%q10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6018,&
'frame/module_domain.f: Failed to deallocate grid%q10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%massflux_edkf ) ) THEN 
  DEALLOCATE(grid%massflux_edkf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6025,&
'frame/module_domain.f: Failed to deallocate grid%massflux_edkf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%entr_edkf ) ) THEN 
  DEALLOCATE(grid%entr_edkf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6032,&
'frame/module_domain.f: Failed to deallocate grid%entr_edkf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%detr_edkf ) ) THEN 
  DEALLOCATE(grid%detr_edkf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6039,&
'frame/module_domain.f: Failed to deallocate grid%detr_edkf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thl_up ) ) THEN 
  DEALLOCATE(grid%thl_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6046,&
'frame/module_domain.f: Failed to deallocate grid%thl_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thv_up ) ) THEN 
  DEALLOCATE(grid%thv_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6053,&
'frame/module_domain.f: Failed to deallocate grid%thv_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rv_up ) ) THEN 
  DEALLOCATE(grid%rv_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6060,&
'frame/module_domain.f: Failed to deallocate grid%rv_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rt_up ) ) THEN 
  DEALLOCATE(grid%rt_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6067,&
'frame/module_domain.f: Failed to deallocate grid%rt_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rc_up ) ) THEN 
  DEALLOCATE(grid%rc_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6074,&
'frame/module_domain.f: Failed to deallocate grid%rc_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_up ) ) THEN 
  DEALLOCATE(grid%u_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6081,&
'frame/module_domain.f: Failed to deallocate grid%u_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_up ) ) THEN 
  DEALLOCATE(grid%v_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6088,&
'frame/module_domain.f: Failed to deallocate grid%v_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%frac_up ) ) THEN 
  DEALLOCATE(grid%frac_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6095,&
'frame/module_domain.f: Failed to deallocate grid%frac_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rc_mf ) ) THEN 
  DEALLOCATE(grid%rc_mf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6102,&
'frame/module_domain.f: Failed to deallocate grid%rc_mf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%te_temf ) ) THEN 
  DEALLOCATE(grid%te_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6109,&
'frame/module_domain.f: Failed to deallocate grid%te_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kh_temf ) ) THEN 
  DEALLOCATE(grid%kh_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6116,&
'frame/module_domain.f: Failed to deallocate grid%kh_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%km_temf ) ) THEN 
  DEALLOCATE(grid%km_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6123,&
'frame/module_domain.f: Failed to deallocate grid%km_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shf_temf ) ) THEN 
  DEALLOCATE(grid%shf_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6130,&
'frame/module_domain.f: Failed to deallocate grid%shf_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qf_temf ) ) THEN 
  DEALLOCATE(grid%qf_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6137,&
'frame/module_domain.f: Failed to deallocate grid%qf_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uw_temf ) ) THEN 
  DEALLOCATE(grid%uw_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6144,&
'frame/module_domain.f: Failed to deallocate grid%uw_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vw_temf ) ) THEN 
  DEALLOCATE(grid%vw_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6151,&
'frame/module_domain.f: Failed to deallocate grid%vw_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wupd_temf ) ) THEN 
  DEALLOCATE(grid%wupd_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6158,&
'frame/module_domain.f: Failed to deallocate grid%wupd_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mf_temf ) ) THEN 
  DEALLOCATE(grid%mf_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6165,&
'frame/module_domain.f: Failed to deallocate grid%mf_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thup_temf ) ) THEN 
  DEALLOCATE(grid%thup_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6172,&
'frame/module_domain.f: Failed to deallocate grid%thup_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtup_temf ) ) THEN 
  DEALLOCATE(grid%qtup_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6179,&
'frame/module_domain.f: Failed to deallocate grid%qtup_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qlup_temf ) ) THEN 
  DEALLOCATE(grid%qlup_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6186,&
'frame/module_domain.f: Failed to deallocate grid%qlup_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cf3d_temf ) ) THEN 
  DEALLOCATE(grid%cf3d_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6193,&
'frame/module_domain.f: Failed to deallocate grid%cf3d_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hd_temf ) ) THEN 
  DEALLOCATE(grid%hd_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6200,&
'frame/module_domain.f: Failed to deallocate grid%hd_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lcl_temf ) ) THEN 
  DEALLOCATE(grid%lcl_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6207,&
'frame/module_domain.f: Failed to deallocate grid%lcl_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hct_temf ) ) THEN 
  DEALLOCATE(grid%hct_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6214,&
'frame/module_domain.f: Failed to deallocate grid%hct_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfm_temf ) ) THEN 
  DEALLOCATE(grid%cfm_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6221,&
'frame/module_domain.f: Failed to deallocate grid%cfm_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wm_temf ) ) THEN 
  DEALLOCATE(grid%wm_temf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6228,&
'frame/module_domain.f: Failed to deallocate grid%wm_temf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qke ) ) THEN 
  DEALLOCATE(grid%qke,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6235,&
'frame/module_domain.f: Failed to deallocate grid%qke. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qshear ) ) THEN 
  DEALLOCATE(grid%qshear,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6242,&
'frame/module_domain.f: Failed to deallocate grid%qshear. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qbuoy ) ) THEN 
  DEALLOCATE(grid%qbuoy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6249,&
'frame/module_domain.f: Failed to deallocate grid%qbuoy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qdiss ) ) THEN 
  DEALLOCATE(grid%qdiss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6256,&
'frame/module_domain.f: Failed to deallocate grid%qdiss. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qwt ) ) THEN 
  DEALLOCATE(grid%qwt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6263,&
'frame/module_domain.f: Failed to deallocate grid%qwt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dqke ) ) THEN 
  DEALLOCATE(grid%dqke,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6270,&
'frame/module_domain.f: Failed to deallocate grid%dqke. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsq ) ) THEN 
  DEALLOCATE(grid%tsq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6277,&
'frame/module_domain.f: Failed to deallocate grid%tsq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsq ) ) THEN 
  DEALLOCATE(grid%qsq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6284,&
'frame/module_domain.f: Failed to deallocate grid%qsq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cov ) ) THEN 
  DEALLOCATE(grid%cov,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6291,&
'frame/module_domain.f: Failed to deallocate grid%cov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh3d ) ) THEN 
  DEALLOCATE(grid%sh3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6298,&
'frame/module_domain.f: Failed to deallocate grid%sh3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ch ) ) THEN 
  DEALLOCATE(grid%ch,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6305,&
'frame/module_domain.f: Failed to deallocate grid%ch. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_a ) ) THEN 
  DEALLOCATE(grid%edmf_a,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6312,&
'frame/module_domain.f: Failed to deallocate grid%edmf_a. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_w ) ) THEN 
  DEALLOCATE(grid%edmf_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6319,&
'frame/module_domain.f: Failed to deallocate grid%edmf_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_thl ) ) THEN 
  DEALLOCATE(grid%edmf_thl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6326,&
'frame/module_domain.f: Failed to deallocate grid%edmf_thl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_qt ) ) THEN 
  DEALLOCATE(grid%edmf_qt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6333,&
'frame/module_domain.f: Failed to deallocate grid%edmf_qt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_ent ) ) THEN 
  DEALLOCATE(grid%edmf_ent,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6340,&
'frame/module_domain.f: Failed to deallocate grid%edmf_ent. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edmf_qc ) ) THEN 
  DEALLOCATE(grid%edmf_qc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6347,&
'frame/module_domain.f: Failed to deallocate grid%edmf_qc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nupdraft ) ) THEN 
  DEALLOCATE(grid%nupdraft,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6354,&
'frame/module_domain.f: Failed to deallocate grid%nupdraft. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%maxmf ) ) THEN 
  DEALLOCATE(grid%maxmf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6361,&
'frame/module_domain.f: Failed to deallocate grid%maxmf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fgdp ) ) THEN 
  DEALLOCATE(grid%fgdp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6368,&
'frame/module_domain.f: Failed to deallocate grid%fgdp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfgdp ) ) THEN 
  DEALLOCATE(grid%dfgdp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6375,&
'frame/module_domain.f: Failed to deallocate grid%dfgdp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vdfg ) ) THEN 
  DEALLOCATE(grid%vdfg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6382,&
'frame/module_domain.f: Failed to deallocate grid%vdfg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exch_tke ) ) THEN 
  DEALLOCATE(grid%exch_tke,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6389,&
'frame/module_domain.f: Failed to deallocate grid%exch_tke. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dtaux3d ) ) THEN 
  DEALLOCATE(grid%dtaux3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6396,&
'frame/module_domain.f: Failed to deallocate grid%dtaux3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dtauy3d ) ) THEN 
  DEALLOCATE(grid%dtauy3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6403,&
'frame/module_domain.f: Failed to deallocate grid%dtauy3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dusfcg ) ) THEN 
  DEALLOCATE(grid%dusfcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6410,&
'frame/module_domain.f: Failed to deallocate grid%dusfcg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dvsfcg ) ) THEN 
  DEALLOCATE(grid%dvsfcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6417,&
'frame/module_domain.f: Failed to deallocate grid%dvsfcg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%var2d ) ) THEN 
  DEALLOCATE(grid%var2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6424,&
'frame/module_domain.f: Failed to deallocate grid%var2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%oc12d ) ) THEN 
  DEALLOCATE(grid%oc12d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6431,&
'frame/module_domain.f: Failed to deallocate grid%oc12d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%oa1 ) ) THEN 
  DEALLOCATE(grid%oa1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6438,&
'frame/module_domain.f: Failed to deallocate grid%oa1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%oa2 ) ) THEN 
  DEALLOCATE(grid%oa2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6445,&
'frame/module_domain.f: Failed to deallocate grid%oa2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%oa3 ) ) THEN 
  DEALLOCATE(grid%oa3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6452,&
'frame/module_domain.f: Failed to deallocate grid%oa3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%oa4 ) ) THEN 
  DEALLOCATE(grid%oa4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6459,&
'frame/module_domain.f: Failed to deallocate grid%oa4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ol1 ) ) THEN 
  DEALLOCATE(grid%ol1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6466,&
'frame/module_domain.f: Failed to deallocate grid%ol1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ol2 ) ) THEN 
  DEALLOCATE(grid%ol2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6473,&
'frame/module_domain.f: Failed to deallocate grid%ol2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ol3 ) ) THEN 
  DEALLOCATE(grid%ol3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6480,&
'frame/module_domain.f: Failed to deallocate grid%ol3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ol4 ) ) THEN 
  DEALLOCATE(grid%ol4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6487,&
'frame/module_domain.f: Failed to deallocate grid%ol4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ctopo ) ) THEN 
  DEALLOCATE(grid%ctopo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6494,&
'frame/module_domain.f: Failed to deallocate grid%ctopo. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ctopo2 ) ) THEN 
  DEALLOCATE(grid%ctopo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6501,&
'frame/module_domain.f: Failed to deallocate grid%ctopo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%a_u_bep ) ) THEN 
  DEALLOCATE(grid%a_u_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6508,&
'frame/module_domain.f: Failed to deallocate grid%a_u_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%a_v_bep ) ) THEN 
  DEALLOCATE(grid%a_v_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6515,&
'frame/module_domain.f: Failed to deallocate grid%a_v_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%a_t_bep ) ) THEN 
  DEALLOCATE(grid%a_t_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6522,&
'frame/module_domain.f: Failed to deallocate grid%a_t_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%a_q_bep ) ) THEN 
  DEALLOCATE(grid%a_q_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6529,&
'frame/module_domain.f: Failed to deallocate grid%a_q_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%a_e_bep ) ) THEN 
  DEALLOCATE(grid%a_e_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6536,&
'frame/module_domain.f: Failed to deallocate grid%a_e_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%b_u_bep ) ) THEN 
  DEALLOCATE(grid%b_u_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6543,&
'frame/module_domain.f: Failed to deallocate grid%b_u_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%b_v_bep ) ) THEN 
  DEALLOCATE(grid%b_v_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6550,&
'frame/module_domain.f: Failed to deallocate grid%b_v_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%b_t_bep ) ) THEN 
  DEALLOCATE(grid%b_t_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6557,&
'frame/module_domain.f: Failed to deallocate grid%b_t_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%b_q_bep ) ) THEN 
  DEALLOCATE(grid%b_q_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6564,&
'frame/module_domain.f: Failed to deallocate grid%b_q_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%b_e_bep ) ) THEN 
  DEALLOCATE(grid%b_e_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6571,&
'frame/module_domain.f: Failed to deallocate grid%b_e_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dlg_bep ) ) THEN 
  DEALLOCATE(grid%dlg_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6578,&
'frame/module_domain.f: Failed to deallocate grid%dlg_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dl_u_bep ) ) THEN 
  DEALLOCATE(grid%dl_u_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6585,&
'frame/module_domain.f: Failed to deallocate grid%dl_u_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sf_bep ) ) THEN 
  DEALLOCATE(grid%sf_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6592,&
'frame/module_domain.f: Failed to deallocate grid%sf_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vl_bep ) ) THEN 
  DEALLOCATE(grid%vl_bep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6599,&
'frame/module_domain.f: Failed to deallocate grid%vl_bep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tke_pbl ) ) THEN 
  DEALLOCATE(grid%tke_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6606,&
'frame/module_domain.f: Failed to deallocate grid%tke_pbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%el_pbl ) ) THEN 
  DEALLOCATE(grid%el_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6613,&
'frame/module_domain.f: Failed to deallocate grid%el_pbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wu_tur ) ) THEN 
  DEALLOCATE(grid%wu_tur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6620,&
'frame/module_domain.f: Failed to deallocate grid%wu_tur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wv_tur ) ) THEN 
  DEALLOCATE(grid%wv_tur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6627,&
'frame/module_domain.f: Failed to deallocate grid%wv_tur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wt_tur ) ) THEN 
  DEALLOCATE(grid%wt_tur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6634,&
'frame/module_domain.f: Failed to deallocate grid%wt_tur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wq_tur ) ) THEN 
  DEALLOCATE(grid%wq_tur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6641,&
'frame/module_domain.f: Failed to deallocate grid%wq_tur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%htop ) ) THEN 
  DEALLOCATE(grid%htop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6648,&
'frame/module_domain.f: Failed to deallocate grid%htop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hbot ) ) THEN 
  DEALLOCATE(grid%hbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6655,&
'frame/module_domain.f: Failed to deallocate grid%hbot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%htopr ) ) THEN 
  DEALLOCATE(grid%htopr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6662,&
'frame/module_domain.f: Failed to deallocate grid%htopr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hbotr ) ) THEN 
  DEALLOCATE(grid%hbotr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6669,&
'frame/module_domain.f: Failed to deallocate grid%hbotr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cutop ) ) THEN 
  DEALLOCATE(grid%cutop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6676,&
'frame/module_domain.f: Failed to deallocate grid%cutop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cubot ) ) THEN 
  DEALLOCATE(grid%cubot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6683,&
'frame/module_domain.f: Failed to deallocate grid%cubot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cuppt ) ) THEN 
  DEALLOCATE(grid%cuppt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6690,&
'frame/module_domain.f: Failed to deallocate grid%cuppt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rswtoa ) ) THEN 
  DEALLOCATE(grid%rswtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6697,&
'frame/module_domain.f: Failed to deallocate grid%rswtoa. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rlwtoa ) ) THEN 
  DEALLOCATE(grid%rlwtoa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6704,&
'frame/module_domain.f: Failed to deallocate grid%rlwtoa. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%czmean ) ) THEN 
  DEALLOCATE(grid%czmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6711,&
'frame/module_domain.f: Failed to deallocate grid%czmean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfracl ) ) THEN 
  DEALLOCATE(grid%cfracl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6718,&
'frame/module_domain.f: Failed to deallocate grid%cfracl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfracm ) ) THEN 
  DEALLOCATE(grid%cfracm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6725,&
'frame/module_domain.f: Failed to deallocate grid%cfracm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfrach ) ) THEN 
  DEALLOCATE(grid%cfrach,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6732,&
'frame/module_domain.f: Failed to deallocate grid%cfrach. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acfrst ) ) THEN 
  DEALLOCATE(grid%acfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6739,&
'frame/module_domain.f: Failed to deallocate grid%acfrst. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ncfrst ) ) THEN 
  DEALLOCATE(grid%ncfrst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6746,&
'frame/module_domain.f: Failed to deallocate grid%ncfrst. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acfrcv ) ) THEN 
  DEALLOCATE(grid%acfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6753,&
'frame/module_domain.f: Failed to deallocate grid%acfrcv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ncfrcv ) ) THEN 
  DEALLOCATE(grid%ncfrcv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6760,&
'frame/module_domain.f: Failed to deallocate grid%ncfrcv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%o3rad ) ) THEN 
  DEALLOCATE(grid%o3rad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6767,&
'frame/module_domain.f: Failed to deallocate grid%o3rad. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerodm ) ) THEN 
  DEALLOCATE(grid%aerodm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6774,&
'frame/module_domain.f: Failed to deallocate grid%aerodm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pina ) ) THEN 
  DEALLOCATE(grid%pina,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6781,&
'frame/module_domain.f: Failed to deallocate grid%pina. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerod ) ) THEN 
  DEALLOCATE(grid%aerod,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6788,&
'frame/module_domain.f: Failed to deallocate grid%aerod. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aodtot ) ) THEN 
  DEALLOCATE(grid%aodtot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6795,&
'frame/module_domain.f: Failed to deallocate grid%aodtot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aeromcu ) ) THEN 
  DEALLOCATE(grid%aeromcu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6802,&
'frame/module_domain.f: Failed to deallocate grid%aeromcu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aeropcu ) ) THEN 
  DEALLOCATE(grid%aeropcu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6809,&
'frame/module_domain.f: Failed to deallocate grid%aeropcu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerocu ) ) THEN 
  DEALLOCATE(grid%aerocu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6816,&
'frame/module_domain.f: Failed to deallocate grid%aerocu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerovar ) ) THEN 
  DEALLOCATE(grid%aerovar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6823,&
'frame/module_domain.f: Failed to deallocate grid%aerovar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ozmixm ) ) THEN 
  DEALLOCATE(grid%ozmixm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6830,&
'frame/module_domain.f: Failed to deallocate grid%ozmixm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pin ) ) THEN 
  DEALLOCATE(grid%pin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6837,&
'frame/module_domain.f: Failed to deallocate grid%pin. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%m_ps_1 ) ) THEN 
  DEALLOCATE(grid%m_ps_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6844,&
'frame/module_domain.f: Failed to deallocate grid%m_ps_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%m_ps_2 ) ) THEN 
  DEALLOCATE(grid%m_ps_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6851,&
'frame/module_domain.f: Failed to deallocate grid%m_ps_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerosolc_1 ) ) THEN 
  DEALLOCATE(grid%aerosolc_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6858,&
'frame/module_domain.f: Failed to deallocate grid%aerosolc_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerosolc_2 ) ) THEN 
  DEALLOCATE(grid%aerosolc_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6865,&
'frame/module_domain.f: Failed to deallocate grid%aerosolc_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%m_hybi ) ) THEN 
  DEALLOCATE(grid%m_hybi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6872,&
'frame/module_domain.f: Failed to deallocate grid%m_hybi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%f_ice_phy ) ) THEN 
  DEALLOCATE(grid%f_ice_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6879,&
'frame/module_domain.f: Failed to deallocate grid%f_ice_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%f_rain_phy ) ) THEN 
  DEALLOCATE(grid%f_rain_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6886,&
'frame/module_domain.f: Failed to deallocate grid%f_rain_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%f_rimef_phy ) ) THEN 
  DEALLOCATE(grid%f_rimef_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6893,&
'frame/module_domain.f: Failed to deallocate grid%f_rimef_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qndropsource ) ) THEN 
  DEALLOCATE(grid%qndropsource,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6900,&
'frame/module_domain.f: Failed to deallocate grid%qndropsource. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_tmp ) ) THEN 
  DEALLOCATE(grid%om_tmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6907,&
'frame/module_domain.f: Failed to deallocate grid%om_tmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_s ) ) THEN 
  DEALLOCATE(grid%om_s,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6914,&
'frame/module_domain.f: Failed to deallocate grid%om_s. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_depth ) ) THEN 
  DEALLOCATE(grid%om_depth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6921,&
'frame/module_domain.f: Failed to deallocate grid%om_depth. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_u ) ) THEN 
  DEALLOCATE(grid%om_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6928,&
'frame/module_domain.f: Failed to deallocate grid%om_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_v ) ) THEN 
  DEALLOCATE(grid%om_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6935,&
'frame/module_domain.f: Failed to deallocate grid%om_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_lat ) ) THEN 
  DEALLOCATE(grid%om_lat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6942,&
'frame/module_domain.f: Failed to deallocate grid%om_lat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_lon ) ) THEN 
  DEALLOCATE(grid%om_lon,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6949,&
'frame/module_domain.f: Failed to deallocate grid%om_lon. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_ml ) ) THEN 
  DEALLOCATE(grid%om_ml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6956,&
'frame/module_domain.f: Failed to deallocate grid%om_ml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_tini ) ) THEN 
  DEALLOCATE(grid%om_tini,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6963,&
'frame/module_domain.f: Failed to deallocate grid%om_tini. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%om_sini ) ) THEN 
  DEALLOCATE(grid%om_sini,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6970,&
'frame/module_domain.f: Failed to deallocate grid%om_sini. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cupflag ) ) THEN 
  DEALLOCATE(grid%cupflag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6977,&
'frame/module_domain.f: Failed to deallocate grid%cupflag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slopesfc ) ) THEN 
  DEALLOCATE(grid%slopesfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6984,&
'frame/module_domain.f: Failed to deallocate grid%slopesfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slopeez ) ) THEN 
  DEALLOCATE(grid%slopeez,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6991,&
'frame/module_domain.f: Failed to deallocate grid%slopeez. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sigmasfc ) ) THEN 
  DEALLOCATE(grid%sigmasfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",6998,&
'frame/module_domain.f: Failed to deallocate grid%sigmasfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sigmaez ) ) THEN 
  DEALLOCATE(grid%sigmaez,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7005,&
'frame/module_domain.f: Failed to deallocate grid%sigmaez. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shall ) ) THEN 
  DEALLOCATE(grid%shall,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7012,&
'frame/module_domain.f: Failed to deallocate grid%shall. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taucloud ) ) THEN 
  DEALLOCATE(grid%taucloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7019,&
'frame/module_domain.f: Failed to deallocate grid%taucloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tactive ) ) THEN 
  DEALLOCATE(grid%tactive,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7026,&
'frame/module_domain.f: Failed to deallocate grid%tactive. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tcloud_cup ) ) THEN 
  DEALLOCATE(grid%tcloud_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7033,&
'frame/module_domain.f: Failed to deallocate grid%tcloud_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wcloudbase ) ) THEN 
  DEALLOCATE(grid%wcloudbase,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7040,&
'frame/module_domain.f: Failed to deallocate grid%wcloudbase. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%activefrac ) ) THEN 
  DEALLOCATE(grid%activefrac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7047,&
'frame/module_domain.f: Failed to deallocate grid%activefrac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfratend_cup ) ) THEN 
  DEALLOCATE(grid%cldfratend_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7054,&
'frame/module_domain.f: Failed to deallocate grid%cldfratend_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_cup ) ) THEN 
  DEALLOCATE(grid%cldfra_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7061,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%updfra_cup ) ) THEN 
  DEALLOCATE(grid%updfra_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7068,&
'frame/module_domain.f: Failed to deallocate grid%updfra_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_iu_cup ) ) THEN 
  DEALLOCATE(grid%qc_iu_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7075,&
'frame/module_domain.f: Failed to deallocate grid%qc_iu_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_ic_cup ) ) THEN 
  DEALLOCATE(grid%qc_ic_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7082,&
'frame/module_domain.f: Failed to deallocate grid%qc_ic_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qndrop_ic_cup ) ) THEN 
  DEALLOCATE(grid%qndrop_ic_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7089,&
'frame/module_domain.f: Failed to deallocate grid%qndrop_ic_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wup_cup ) ) THEN 
  DEALLOCATE(grid%wup_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7096,&
'frame/module_domain.f: Failed to deallocate grid%wup_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wact_cup ) ) THEN 
  DEALLOCATE(grid%wact_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7103,&
'frame/module_domain.f: Failed to deallocate grid%wact_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wulcl_cup ) ) THEN 
  DEALLOCATE(grid%wulcl_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7110,&
'frame/module_domain.f: Failed to deallocate grid%wulcl_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mfup_cup ) ) THEN 
  DEALLOCATE(grid%mfup_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7117,&
'frame/module_domain.f: Failed to deallocate grid%mfup_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mfup_ent_cup ) ) THEN 
  DEALLOCATE(grid%mfup_ent_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7124,&
'frame/module_domain.f: Failed to deallocate grid%mfup_ent_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mfdn_cup ) ) THEN 
  DEALLOCATE(grid%mfdn_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7131,&
'frame/module_domain.f: Failed to deallocate grid%mfdn_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mfdn_ent_cup ) ) THEN 
  DEALLOCATE(grid%mfdn_ent_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7138,&
'frame/module_domain.f: Failed to deallocate grid%mfdn_ent_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fcvt_qc_to_pr_cup ) ) THEN 
  DEALLOCATE(grid%fcvt_qc_to_pr_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7145,&
'frame/module_domain.f: Failed to deallocate grid%fcvt_qc_to_pr_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fcvt_qc_to_qi_cup ) ) THEN 
  DEALLOCATE(grid%fcvt_qc_to_qi_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7152,&
'frame/module_domain.f: Failed to deallocate grid%fcvt_qc_to_qi_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fcvt_qi_to_pr_cup ) ) THEN 
  DEALLOCATE(grid%fcvt_qi_to_pr_cup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7159,&
'frame/module_domain.f: Failed to deallocate grid%fcvt_qi_to_pr_cup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tstar ) ) THEN 
  DEALLOCATE(grid%tstar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7166,&
'frame/module_domain.f: Failed to deallocate grid%tstar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lnterms ) ) THEN 
  DEALLOCATE(grid%lnterms,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7173,&
'frame/module_domain.f: Failed to deallocate grid%lnterms. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lnint ) ) THEN 
  DEALLOCATE(grid%lnint,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7180,&
'frame/module_domain.f: Failed to deallocate grid%lnint. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h_diabatic ) ) THEN 
  DEALLOCATE(grid%h_diabatic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7187,&
'frame/module_domain.f: Failed to deallocate grid%h_diabatic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_diabatic ) ) THEN 
  DEALLOCATE(grid%qv_diabatic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7194,&
'frame/module_domain.f: Failed to deallocate grid%qv_diabatic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_diabatic ) ) THEN 
  DEALLOCATE(grid%qc_diabatic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7201,&
'frame/module_domain.f: Failed to deallocate grid%qc_diabatic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msft ) ) THEN 
  DEALLOCATE(grid%msft,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7208,&
'frame/module_domain.f: Failed to deallocate grid%msft. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfu ) ) THEN 
  DEALLOCATE(grid%msfu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7215,&
'frame/module_domain.f: Failed to deallocate grid%msfu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfv ) ) THEN 
  DEALLOCATE(grid%msfv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7222,&
'frame/module_domain.f: Failed to deallocate grid%msfv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msftx ) ) THEN 
  DEALLOCATE(grid%msftx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7229,&
'frame/module_domain.f: Failed to deallocate grid%msftx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfty ) ) THEN 
  DEALLOCATE(grid%msfty,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7236,&
'frame/module_domain.f: Failed to deallocate grid%msfty. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfux ) ) THEN 
  DEALLOCATE(grid%msfux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7243,&
'frame/module_domain.f: Failed to deallocate grid%msfux. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfuy ) ) THEN 
  DEALLOCATE(grid%msfuy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7250,&
'frame/module_domain.f: Failed to deallocate grid%msfuy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfvx ) ) THEN 
  DEALLOCATE(grid%msfvx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7257,&
'frame/module_domain.f: Failed to deallocate grid%msfvx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfvx_inv ) ) THEN 
  DEALLOCATE(grid%msfvx_inv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7264,&
'frame/module_domain.f: Failed to deallocate grid%msfvx_inv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%msfvy ) ) THEN 
  DEALLOCATE(grid%msfvy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7271,&
'frame/module_domain.f: Failed to deallocate grid%msfvy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%f ) ) THEN 
  DEALLOCATE(grid%f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7278,&
'frame/module_domain.f: Failed to deallocate grid%f. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%e ) ) THEN 
  DEALLOCATE(grid%e,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7285,&
'frame/module_domain.f: Failed to deallocate grid%e. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sina ) ) THEN 
  DEALLOCATE(grid%sina,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7292,&
'frame/module_domain.f: Failed to deallocate grid%sina. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cosa ) ) THEN 
  DEALLOCATE(grid%cosa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7299,&
'frame/module_domain.f: Failed to deallocate grid%cosa. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht ) ) THEN 
  DEALLOCATE(grid%ht,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7306,&
'frame/module_domain.f: Failed to deallocate grid%ht. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_fine ) ) THEN 
  DEALLOCATE(grid%ht_fine,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7313,&
'frame/module_domain.f: Failed to deallocate grid%ht_fine. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_int ) ) THEN 
  DEALLOCATE(grid%ht_int,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7320,&
'frame/module_domain.f: Failed to deallocate grid%ht_int. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_input ) ) THEN 
  DEALLOCATE(grid%ht_input,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7327,&
'frame/module_domain.f: Failed to deallocate grid%ht_input. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_smooth ) ) THEN 
  DEALLOCATE(grid%ht_smooth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7334,&
'frame/module_domain.f: Failed to deallocate grid%ht_smooth. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_shad ) ) THEN 
  DEALLOCATE(grid%ht_shad,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7341,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bxs ) ) THEN 
  DEALLOCATE(grid%ht_shad_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7348,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bxs. ')
 endif
  NULLIFY(grid%ht_shad_bxs)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bxe ) ) THEN 
  DEALLOCATE(grid%ht_shad_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7356,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bxe. ')
 endif
  NULLIFY(grid%ht_shad_bxe)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bys ) ) THEN 
  DEALLOCATE(grid%ht_shad_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7364,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bys. ')
 endif
  NULLIFY(grid%ht_shad_bys)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_bye ) ) THEN 
  DEALLOCATE(grid%ht_shad_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7372,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_bye. ')
 endif
  NULLIFY(grid%ht_shad_bye)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btxs ) ) THEN 
  DEALLOCATE(grid%ht_shad_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7380,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btxs. ')
 endif
  NULLIFY(grid%ht_shad_btxs)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btxe ) ) THEN 
  DEALLOCATE(grid%ht_shad_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7388,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btxe. ')
 endif
  NULLIFY(grid%ht_shad_btxe)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btys ) ) THEN 
  DEALLOCATE(grid%ht_shad_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7396,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btys. ')
 endif
  NULLIFY(grid%ht_shad_btys)
ENDIF
IF ( ASSOCIATED( grid%ht_shad_btye ) ) THEN 
  DEALLOCATE(grid%ht_shad_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7404,&
'frame/module_domain.f: Failed to deallocate grid%ht_shad_btye. ')
 endif
  NULLIFY(grid%ht_shad_btye)
ENDIF
IF ( ASSOCIATED( grid%shadowmask ) ) THEN 
  DEALLOCATE(grid%shadowmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7412,&
'frame/module_domain.f: Failed to deallocate grid%shadowmask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk ) ) THEN 
  DEALLOCATE(grid%tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7419,&
'frame/module_domain.f: Failed to deallocate grid%tsk. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_tsk ) ) THEN 
  DEALLOCATE(grid%dfi_tsk,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7426,&
'frame/module_domain.f: Failed to deallocate grid%dfi_tsk. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_save ) ) THEN 
  DEALLOCATE(grid%tsk_save,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7433,&
'frame/module_domain.f: Failed to deallocate grid%tsk_save. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_base ) ) THEN 
  DEALLOCATE(grid%u_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7440,&
'frame/module_domain.f: Failed to deallocate grid%u_base. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_base ) ) THEN 
  DEALLOCATE(grid%v_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7447,&
'frame/module_domain.f: Failed to deallocate grid%v_base. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_base ) ) THEN 
  DEALLOCATE(grid%qv_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7454,&
'frame/module_domain.f: Failed to deallocate grid%qv_base. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_base ) ) THEN 
  DEALLOCATE(grid%z_base,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7461,&
'frame/module_domain.f: Failed to deallocate grid%z_base. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tlwdn ) ) THEN 
  DEALLOCATE(grid%tlwdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7468,&
'frame/module_domain.f: Failed to deallocate grid%tlwdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tlwup ) ) THEN 
  DEALLOCATE(grid%tlwup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7475,&
'frame/module_domain.f: Failed to deallocate grid%tlwup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slwdn ) ) THEN 
  DEALLOCATE(grid%slwdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7482,&
'frame/module_domain.f: Failed to deallocate grid%slwdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slwup ) ) THEN 
  DEALLOCATE(grid%slwup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7489,&
'frame/module_domain.f: Failed to deallocate grid%slwup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tswdn ) ) THEN 
  DEALLOCATE(grid%tswdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7496,&
'frame/module_domain.f: Failed to deallocate grid%tswdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tswup ) ) THEN 
  DEALLOCATE(grid%tswup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7503,&
'frame/module_domain.f: Failed to deallocate grid%tswup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sswdn ) ) THEN 
  DEALLOCATE(grid%sswdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7510,&
'frame/module_domain.f: Failed to deallocate grid%sswdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sswup ) ) THEN 
  DEALLOCATE(grid%sswup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7517,&
'frame/module_domain.f: Failed to deallocate grid%sswup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rushten ) ) THEN 
  DEALLOCATE(grid%rushten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7524,&
'frame/module_domain.f: Failed to deallocate grid%rushten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rvshten ) ) THEN 
  DEALLOCATE(grid%rvshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7531,&
'frame/module_domain.f: Failed to deallocate grid%rvshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthshten ) ) THEN 
  DEALLOCATE(grid%rthshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7538,&
'frame/module_domain.f: Failed to deallocate grid%rthshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvshten ) ) THEN 
  DEALLOCATE(grid%rqvshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7545,&
'frame/module_domain.f: Failed to deallocate grid%rqvshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqrshten ) ) THEN 
  DEALLOCATE(grid%rqrshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7552,&
'frame/module_domain.f: Failed to deallocate grid%rqrshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqcshten ) ) THEN 
  DEALLOCATE(grid%rqcshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7559,&
'frame/module_domain.f: Failed to deallocate grid%rqcshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqsshten ) ) THEN 
  DEALLOCATE(grid%rqsshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7566,&
'frame/module_domain.f: Failed to deallocate grid%rqsshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqishten ) ) THEN 
  DEALLOCATE(grid%rqishten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7573,&
'frame/module_domain.f: Failed to deallocate grid%rqishten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqgshten ) ) THEN 
  DEALLOCATE(grid%rqgshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7580,&
'frame/module_domain.f: Failed to deallocate grid%rqgshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqcnshten ) ) THEN 
  DEALLOCATE(grid%rqcnshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7587,&
'frame/module_domain.f: Failed to deallocate grid%rqcnshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqinshten ) ) THEN 
  DEALLOCATE(grid%rqinshten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7594,&
'frame/module_domain.f: Failed to deallocate grid%rqinshten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rucuten ) ) THEN 
  DEALLOCATE(grid%rucuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7601,&
'frame/module_domain.f: Failed to deallocate grid%rucuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rvcuten ) ) THEN 
  DEALLOCATE(grid%rvcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7608,&
'frame/module_domain.f: Failed to deallocate grid%rvcuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthcuten ) ) THEN 
  DEALLOCATE(grid%rthcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7615,&
'frame/module_domain.f: Failed to deallocate grid%rthcuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvcuten ) ) THEN 
  DEALLOCATE(grid%rqvcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7622,&
'frame/module_domain.f: Failed to deallocate grid%rqvcuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqrcuten ) ) THEN 
  DEALLOCATE(grid%rqrcuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7629,&
'frame/module_domain.f: Failed to deallocate grid%rqrcuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqccuten ) ) THEN 
  DEALLOCATE(grid%rqccuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7636,&
'frame/module_domain.f: Failed to deallocate grid%rqccuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqscuten ) ) THEN 
  DEALLOCATE(grid%rqscuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7643,&
'frame/module_domain.f: Failed to deallocate grid%rqscuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqicuten ) ) THEN 
  DEALLOCATE(grid%rqicuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7650,&
'frame/module_domain.f: Failed to deallocate grid%rqicuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqcncuten ) ) THEN 
  DEALLOCATE(grid%rqcncuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7657,&
'frame/module_domain.f: Failed to deallocate grid%rqcncuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqincuten ) ) THEN 
  DEALLOCATE(grid%rqincuten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7664,&
'frame/module_domain.f: Failed to deallocate grid%rqincuten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w0avg ) ) THEN 
  DEALLOCATE(grid%w0avg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7671,&
'frame/module_domain.f: Failed to deallocate grid%w0avg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainc ) ) THEN 
  DEALLOCATE(grid%rainc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7678,&
'frame/module_domain.f: Failed to deallocate grid%rainc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainsh ) ) THEN 
  DEALLOCATE(grid%rainsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7685,&
'frame/module_domain.f: Failed to deallocate grid%rainsh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainnc ) ) THEN 
  DEALLOCATE(grid%rainnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7692,&
'frame/module_domain.f: Failed to deallocate grid%rainnc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_rainc ) ) THEN 
  DEALLOCATE(grid%i_rainc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7699,&
'frame/module_domain.f: Failed to deallocate grid%i_rainc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_rainnc ) ) THEN 
  DEALLOCATE(grid%i_rainnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7706,&
'frame/module_domain.f: Failed to deallocate grid%i_rainnc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pratec ) ) THEN 
  DEALLOCATE(grid%pratec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7713,&
'frame/module_domain.f: Failed to deallocate grid%pratec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pratesh ) ) THEN 
  DEALLOCATE(grid%pratesh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7720,&
'frame/module_domain.f: Failed to deallocate grid%pratesh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincv ) ) THEN 
  DEALLOCATE(grid%raincv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7727,&
'frame/module_domain.f: Failed to deallocate grid%raincv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainshv ) ) THEN 
  DEALLOCATE(grid%rainshv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7734,&
'frame/module_domain.f: Failed to deallocate grid%rainshv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainncv ) ) THEN 
  DEALLOCATE(grid%rainncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7741,&
'frame/module_domain.f: Failed to deallocate grid%rainncv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainbl ) ) THEN 
  DEALLOCATE(grid%rainbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7748,&
'frame/module_domain.f: Failed to deallocate grid%rainbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snownc ) ) THEN 
  DEALLOCATE(grid%snownc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7755,&
'frame/module_domain.f: Failed to deallocate grid%snownc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%graupelnc ) ) THEN 
  DEALLOCATE(grid%graupelnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7762,&
'frame/module_domain.f: Failed to deallocate grid%graupelnc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailnc ) ) THEN 
  DEALLOCATE(grid%hailnc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7769,&
'frame/module_domain.f: Failed to deallocate grid%hailnc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowncv ) ) THEN 
  DEALLOCATE(grid%snowncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7776,&
'frame/module_domain.f: Failed to deallocate grid%snowncv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%graupelncv ) ) THEN 
  DEALLOCATE(grid%graupelncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7783,&
'frame/module_domain.f: Failed to deallocate grid%graupelncv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailncv ) ) THEN 
  DEALLOCATE(grid%hailncv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7790,&
'frame/module_domain.f: Failed to deallocate grid%hailncv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%refl_10cm ) ) THEN 
  DEALLOCATE(grid%refl_10cm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7797,&
'frame/module_domain.f: Failed to deallocate grid%refl_10cm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mskf_refl_10cm ) ) THEN 
  DEALLOCATE(grid%mskf_refl_10cm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7804,&
'frame/module_domain.f: Failed to deallocate grid%mskf_refl_10cm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th_old ) ) THEN 
  DEALLOCATE(grid%th_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7811,&
'frame/module_domain.f: Failed to deallocate grid%th_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qv_old ) ) THEN 
  DEALLOCATE(grid%qv_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7818,&
'frame/module_domain.f: Failed to deallocate grid%qv_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vmi3d ) ) THEN 
  DEALLOCATE(grid%vmi3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7825,&
'frame/module_domain.f: Failed to deallocate grid%vmi3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%di3d ) ) THEN 
  DEALLOCATE(grid%di3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7832,&
'frame/module_domain.f: Failed to deallocate grid%di3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rhopo3d ) ) THEN 
  DEALLOCATE(grid%rhopo3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7839,&
'frame/module_domain.f: Failed to deallocate grid%rhopo3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vmi3d_2 ) ) THEN 
  DEALLOCATE(grid%vmi3d_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7846,&
'frame/module_domain.f: Failed to deallocate grid%vmi3d_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%di3d_2 ) ) THEN 
  DEALLOCATE(grid%di3d_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7853,&
'frame/module_domain.f: Failed to deallocate grid%di3d_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rhopo3d_2 ) ) THEN 
  DEALLOCATE(grid%rhopo3d_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7860,&
'frame/module_domain.f: Failed to deallocate grid%rhopo3d_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nca ) ) THEN 
  DEALLOCATE(grid%nca,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7867,&
'frame/module_domain.f: Failed to deallocate grid%nca. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lowlyr ) ) THEN 
  DEALLOCATE(grid%lowlyr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7874,&
'frame/module_domain.f: Failed to deallocate grid%lowlyr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mass_flux ) ) THEN 
  DEALLOCATE(grid%mass_flux,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7881,&
'frame/module_domain.f: Failed to deallocate grid%mass_flux. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_dp ) ) THEN 
  DEALLOCATE(grid%cldfra_dp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7888,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_dp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_sh ) ) THEN 
  DEALLOCATE(grid%cldfra_sh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7895,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_sh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_up ) ) THEN 
  DEALLOCATE(grid%w_up,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7902,&
'frame/module_domain.f: Failed to deallocate grid%w_up. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%udr_kf ) ) THEN 
  DEALLOCATE(grid%udr_kf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7909,&
'frame/module_domain.f: Failed to deallocate grid%udr_kf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ddr_kf ) ) THEN 
  DEALLOCATE(grid%ddr_kf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7916,&
'frame/module_domain.f: Failed to deallocate grid%ddr_kf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uer_kf ) ) THEN 
  DEALLOCATE(grid%uer_kf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7923,&
'frame/module_domain.f: Failed to deallocate grid%uer_kf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%der_kf ) ) THEN 
  DEALLOCATE(grid%der_kf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7930,&
'frame/module_domain.f: Failed to deallocate grid%der_kf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%timec_kf ) ) THEN 
  DEALLOCATE(grid%timec_kf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7937,&
'frame/module_domain.f: Failed to deallocate grid%timec_kf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_gr ) ) THEN 
  DEALLOCATE(grid%apr_gr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7944,&
'frame/module_domain.f: Failed to deallocate grid%apr_gr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_w ) ) THEN 
  DEALLOCATE(grid%apr_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7951,&
'frame/module_domain.f: Failed to deallocate grid%apr_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_mc ) ) THEN 
  DEALLOCATE(grid%apr_mc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7958,&
'frame/module_domain.f: Failed to deallocate grid%apr_mc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_st ) ) THEN 
  DEALLOCATE(grid%apr_st,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7965,&
'frame/module_domain.f: Failed to deallocate grid%apr_st. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_as ) ) THEN 
  DEALLOCATE(grid%apr_as,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7972,&
'frame/module_domain.f: Failed to deallocate grid%apr_as. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_capma ) ) THEN 
  DEALLOCATE(grid%apr_capma,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7979,&
'frame/module_domain.f: Failed to deallocate grid%apr_capma. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_capme ) ) THEN 
  DEALLOCATE(grid%apr_capme,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7986,&
'frame/module_domain.f: Failed to deallocate grid%apr_capme. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%apr_capmi ) ) THEN 
  DEALLOCATE(grid%apr_capmi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",7993,&
'frame/module_domain.f: Failed to deallocate grid%apr_capmi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edt_out ) ) THEN 
  DEALLOCATE(grid%edt_out,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8000,&
'frame/module_domain.f: Failed to deallocate grid%edt_out. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xmb_shallow ) ) THEN 
  DEALLOCATE(grid%xmb_shallow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8007,&
'frame/module_domain.f: Failed to deallocate grid%xmb_shallow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%k22_shallow ) ) THEN 
  DEALLOCATE(grid%k22_shallow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8014,&
'frame/module_domain.f: Failed to deallocate grid%k22_shallow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kbcon_shallow ) ) THEN 
  DEALLOCATE(grid%kbcon_shallow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8021,&
'frame/module_domain.f: Failed to deallocate grid%kbcon_shallow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ktop_shallow ) ) THEN 
  DEALLOCATE(grid%ktop_shallow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8028,&
'frame/module_domain.f: Failed to deallocate grid%ktop_shallow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%k22_deep ) ) THEN 
  DEALLOCATE(grid%k22_deep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8035,&
'frame/module_domain.f: Failed to deallocate grid%k22_deep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kbcon_deep ) ) THEN 
  DEALLOCATE(grid%kbcon_deep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8042,&
'frame/module_domain.f: Failed to deallocate grid%kbcon_deep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ktop_deep ) ) THEN 
  DEALLOCATE(grid%ktop_deep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8049,&
'frame/module_domain.f: Failed to deallocate grid%ktop_deep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xf_ens ) ) THEN 
  DEALLOCATE(grid%xf_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8056,&
'frame/module_domain.f: Failed to deallocate grid%xf_ens. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pr_ens ) ) THEN 
  DEALLOCATE(grid%pr_ens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8063,&
'frame/module_domain.f: Failed to deallocate grid%pr_ens. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cugd_tten ) ) THEN 
  DEALLOCATE(grid%cugd_tten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8070,&
'frame/module_domain.f: Failed to deallocate grid%cugd_tten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cugd_qvten ) ) THEN 
  DEALLOCATE(grid%cugd_qvten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8077,&
'frame/module_domain.f: Failed to deallocate grid%cugd_qvten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cugd_ttens ) ) THEN 
  DEALLOCATE(grid%cugd_ttens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8084,&
'frame/module_domain.f: Failed to deallocate grid%cugd_ttens. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cugd_qvtens ) ) THEN 
  DEALLOCATE(grid%cugd_qvtens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8091,&
'frame/module_domain.f: Failed to deallocate grid%cugd_qvtens. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cugd_qcten ) ) THEN 
  DEALLOCATE(grid%cugd_qcten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8098,&
'frame/module_domain.f: Failed to deallocate grid%cugd_qcten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gd_cloud ) ) THEN 
  DEALLOCATE(grid%gd_cloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8105,&
'frame/module_domain.f: Failed to deallocate grid%gd_cloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gd_cloud2 ) ) THEN 
  DEALLOCATE(grid%gd_cloud2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8112,&
'frame/module_domain.f: Failed to deallocate grid%gd_cloud2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gd_cldfr ) ) THEN 
  DEALLOCATE(grid%gd_cldfr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8119,&
'frame/module_domain.f: Failed to deallocate grid%gd_cldfr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincv_a ) ) THEN 
  DEALLOCATE(grid%raincv_a,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8126,&
'frame/module_domain.f: Failed to deallocate grid%raincv_a. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincv_b ) ) THEN 
  DEALLOCATE(grid%raincv_b,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8133,&
'frame/module_domain.f: Failed to deallocate grid%raincv_b. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gd_cloud_a ) ) THEN 
  DEALLOCATE(grid%gd_cloud_a,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8140,&
'frame/module_domain.f: Failed to deallocate grid%gd_cloud_a. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gd_cloud2_a ) ) THEN 
  DEALLOCATE(grid%gd_cloud2_a,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8147,&
'frame/module_domain.f: Failed to deallocate grid%gd_cloud2_a. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_cu ) ) THEN 
  DEALLOCATE(grid%qc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8154,&
'frame/module_domain.f: Failed to deallocate grid%qc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qi_cu ) ) THEN 
  DEALLOCATE(grid%qi_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8161,&
'frame/module_domain.f: Failed to deallocate grid%qi_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qr_cu ) ) THEN 
  DEALLOCATE(grid%qr_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8168,&
'frame/module_domain.f: Failed to deallocate grid%qr_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qs_cu ) ) THEN 
  DEALLOCATE(grid%qs_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8175,&
'frame/module_domain.f: Failed to deallocate grid%qs_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nc_cu ) ) THEN 
  DEALLOCATE(grid%nc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8182,&
'frame/module_domain.f: Failed to deallocate grid%nc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ni_cu ) ) THEN 
  DEALLOCATE(grid%ni_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8189,&
'frame/module_domain.f: Failed to deallocate grid%ni_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nr_cu ) ) THEN 
  DEALLOCATE(grid%nr_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8196,&
'frame/module_domain.f: Failed to deallocate grid%nr_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ns_cu ) ) THEN 
  DEALLOCATE(grid%ns_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8203,&
'frame/module_domain.f: Failed to deallocate grid%ns_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ccn_cu ) ) THEN 
  DEALLOCATE(grid%ccn_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8210,&
'frame/module_domain.f: Failed to deallocate grid%ccn_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cu_uaf ) ) THEN 
  DEALLOCATE(grid%cu_uaf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8217,&
'frame/module_domain.f: Failed to deallocate grid%cu_uaf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%efcs ) ) THEN 
  DEALLOCATE(grid%efcs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8224,&
'frame/module_domain.f: Failed to deallocate grid%efcs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%efis ) ) THEN 
  DEALLOCATE(grid%efis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8231,&
'frame/module_domain.f: Failed to deallocate grid%efis. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%efcg ) ) THEN 
  DEALLOCATE(grid%efcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8238,&
'frame/module_domain.f: Failed to deallocate grid%efcg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%efig ) ) THEN 
  DEALLOCATE(grid%efig,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8245,&
'frame/module_domain.f: Failed to deallocate grid%efig. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%efsg ) ) THEN 
  DEALLOCATE(grid%efsg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8252,&
'frame/module_domain.f: Failed to deallocate grid%efsg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%efss ) ) THEN 
  DEALLOCATE(grid%efss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8259,&
'frame/module_domain.f: Failed to deallocate grid%efss. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wact ) ) THEN 
  DEALLOCATE(grid%wact,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8266,&
'frame/module_domain.f: Failed to deallocate grid%wact. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ccn1_gs ) ) THEN 
  DEALLOCATE(grid%ccn1_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8273,&
'frame/module_domain.f: Failed to deallocate grid%ccn1_gs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ccn2_gs ) ) THEN 
  DEALLOCATE(grid%ccn2_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8280,&
'frame/module_domain.f: Failed to deallocate grid%ccn2_gs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ccn3_gs ) ) THEN 
  DEALLOCATE(grid%ccn3_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8287,&
'frame/module_domain.f: Failed to deallocate grid%ccn3_gs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ccn4_gs ) ) THEN 
  DEALLOCATE(grid%ccn4_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8294,&
'frame/module_domain.f: Failed to deallocate grid%ccn4_gs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ccn5_gs ) ) THEN 
  DEALLOCATE(grid%ccn5_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8301,&
'frame/module_domain.f: Failed to deallocate grid%ccn5_gs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ccn6_gs ) ) THEN 
  DEALLOCATE(grid%ccn6_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8308,&
'frame/module_domain.f: Failed to deallocate grid%ccn6_gs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ccn7_gs ) ) THEN 
  DEALLOCATE(grid%ccn7_gs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8315,&
'frame/module_domain.f: Failed to deallocate grid%ccn7_gs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_bl ) ) THEN 
  DEALLOCATE(grid%qc_bl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8322,&
'frame/module_domain.f: Failed to deallocate grid%qc_bl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthften ) ) THEN 
  DEALLOCATE(grid%rthften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8329,&
'frame/module_domain.f: Failed to deallocate grid%rthften. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvften ) ) THEN 
  DEALLOCATE(grid%rqvften,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8336,&
'frame/module_domain.f: Failed to deallocate grid%rqvften. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthraten ) ) THEN 
  DEALLOCATE(grid%rthraten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8343,&
'frame/module_domain.f: Failed to deallocate grid%rthraten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthratenlw ) ) THEN 
  DEALLOCATE(grid%rthratenlw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8350,&
'frame/module_domain.f: Failed to deallocate grid%rthratenlw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthratensw ) ) THEN 
  DEALLOCATE(grid%rthratensw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8357,&
'frame/module_domain.f: Failed to deallocate grid%rthratensw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra ) ) THEN 
  DEALLOCATE(grid%cldfra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8364,&
'frame/module_domain.f: Failed to deallocate grid%cldfra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_old ) ) THEN 
  DEALLOCATE(grid%cldfra_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8371,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_bl ) ) THEN 
  DEALLOCATE(grid%cldfra_bl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8378,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_bl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldt ) ) THEN 
  DEALLOCATE(grid%cldt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8385,&
'frame/module_domain.f: Failed to deallocate grid%cldt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdown ) ) THEN 
  DEALLOCATE(grid%swdown,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8392,&
'frame/module_domain.f: Failed to deallocate grid%swdown. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdownc ) ) THEN 
  DEALLOCATE(grid%swdownc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8399,&
'frame/module_domain.f: Failed to deallocate grid%swdownc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gsw ) ) THEN 
  DEALLOCATE(grid%gsw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8406,&
'frame/module_domain.f: Failed to deallocate grid%gsw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%glw ) ) THEN 
  DEALLOCATE(grid%glw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8413,&
'frame/module_domain.f: Failed to deallocate grid%glw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swnorm ) ) THEN 
  DEALLOCATE(grid%swnorm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8420,&
'frame/module_domain.f: Failed to deallocate grid%swnorm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%diffuse_frac ) ) THEN 
  DEALLOCATE(grid%diffuse_frac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8427,&
'frame/module_domain.f: Failed to deallocate grid%diffuse_frac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swddir ) ) THEN 
  DEALLOCATE(grid%swddir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8434,&
'frame/module_domain.f: Failed to deallocate grid%swddir. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swddirc ) ) THEN 
  DEALLOCATE(grid%swddirc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8441,&
'frame/module_domain.f: Failed to deallocate grid%swddirc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swddni ) ) THEN 
  DEALLOCATE(grid%swddni,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8448,&
'frame/module_domain.f: Failed to deallocate grid%swddni. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swddnic ) ) THEN 
  DEALLOCATE(grid%swddnic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8455,&
'frame/module_domain.f: Failed to deallocate grid%swddnic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swddif ) ) THEN 
  DEALLOCATE(grid%swddif,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8462,&
'frame/module_domain.f: Failed to deallocate grid%swddif. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gx ) ) THEN 
  DEALLOCATE(grid%gx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8469,&
'frame/module_domain.f: Failed to deallocate grid%gx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bx ) ) THEN 
  DEALLOCATE(grid%bx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8476,&
'frame/module_domain.f: Failed to deallocate grid%bx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gg ) ) THEN 
  DEALLOCATE(grid%gg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8483,&
'frame/module_domain.f: Failed to deallocate grid%gg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bb ) ) THEN 
  DEALLOCATE(grid%bb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8490,&
'frame/module_domain.f: Failed to deallocate grid%bb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%coszen_ref ) ) THEN 
  DEALLOCATE(grid%coszen_ref,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8497,&
'frame/module_domain.f: Failed to deallocate grid%coszen_ref. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdown_ref ) ) THEN 
  DEALLOCATE(grid%swdown_ref,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8504,&
'frame/module_domain.f: Failed to deallocate grid%swdown_ref. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swddir_ref ) ) THEN 
  DEALLOCATE(grid%swddir_ref,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8511,&
'frame/module_domain.f: Failed to deallocate grid%swddir_ref. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aod5502d ) ) THEN 
  DEALLOCATE(grid%aod5502d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8518,&
'frame/module_domain.f: Failed to deallocate grid%aod5502d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%angexp2d ) ) THEN 
  DEALLOCATE(grid%angexp2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8525,&
'frame/module_domain.f: Failed to deallocate grid%angexp2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerssa2d ) ) THEN 
  DEALLOCATE(grid%aerssa2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8532,&
'frame/module_domain.f: Failed to deallocate grid%aerssa2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aerasy2d ) ) THEN 
  DEALLOCATE(grid%aerasy2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8539,&
'frame/module_domain.f: Failed to deallocate grid%aerasy2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aod5503d ) ) THEN 
  DEALLOCATE(grid%aod5503d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8546,&
'frame/module_domain.f: Failed to deallocate grid%aod5503d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taod5503d ) ) THEN 
  DEALLOCATE(grid%taod5503d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8553,&
'frame/module_domain.f: Failed to deallocate grid%taod5503d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taod5502d ) ) THEN 
  DEALLOCATE(grid%taod5502d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8560,&
'frame/module_domain.f: Failed to deallocate grid%taod5502d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2min ) ) THEN 
  DEALLOCATE(grid%t2min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8567,&
'frame/module_domain.f: Failed to deallocate grid%t2min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2max ) ) THEN 
  DEALLOCATE(grid%t2max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8574,&
'frame/module_domain.f: Failed to deallocate grid%t2max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tt2min ) ) THEN 
  DEALLOCATE(grid%tt2min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8581,&
'frame/module_domain.f: Failed to deallocate grid%tt2min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tt2max ) ) THEN 
  DEALLOCATE(grid%tt2max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8588,&
'frame/module_domain.f: Failed to deallocate grid%tt2max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2mean ) ) THEN 
  DEALLOCATE(grid%t2mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8595,&
'frame/module_domain.f: Failed to deallocate grid%t2mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2std ) ) THEN 
  DEALLOCATE(grid%t2std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8602,&
'frame/module_domain.f: Failed to deallocate grid%t2std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2min ) ) THEN 
  DEALLOCATE(grid%q2min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8609,&
'frame/module_domain.f: Failed to deallocate grid%q2min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2max ) ) THEN 
  DEALLOCATE(grid%q2max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8616,&
'frame/module_domain.f: Failed to deallocate grid%q2max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tq2min ) ) THEN 
  DEALLOCATE(grid%tq2min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8623,&
'frame/module_domain.f: Failed to deallocate grid%tq2min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tq2max ) ) THEN 
  DEALLOCATE(grid%tq2max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8630,&
'frame/module_domain.f: Failed to deallocate grid%tq2max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2mean ) ) THEN 
  DEALLOCATE(grid%q2mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8637,&
'frame/module_domain.f: Failed to deallocate grid%q2mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2std ) ) THEN 
  DEALLOCATE(grid%q2std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8644,&
'frame/module_domain.f: Failed to deallocate grid%q2std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%skintempmin ) ) THEN 
  DEALLOCATE(grid%skintempmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8651,&
'frame/module_domain.f: Failed to deallocate grid%skintempmin. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%skintempmax ) ) THEN 
  DEALLOCATE(grid%skintempmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8658,&
'frame/module_domain.f: Failed to deallocate grid%skintempmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tskintempmin ) ) THEN 
  DEALLOCATE(grid%tskintempmin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8665,&
'frame/module_domain.f: Failed to deallocate grid%tskintempmin. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tskintempmax ) ) THEN 
  DEALLOCATE(grid%tskintempmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8672,&
'frame/module_domain.f: Failed to deallocate grid%tskintempmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%skintempmean ) ) THEN 
  DEALLOCATE(grid%skintempmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8679,&
'frame/module_domain.f: Failed to deallocate grid%skintempmean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%skintempstd ) ) THEN 
  DEALLOCATE(grid%skintempstd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8686,&
'frame/module_domain.f: Failed to deallocate grid%skintempstd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10max ) ) THEN 
  DEALLOCATE(grid%u10max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8693,&
'frame/module_domain.f: Failed to deallocate grid%u10max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10max ) ) THEN 
  DEALLOCATE(grid%v10max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8700,&
'frame/module_domain.f: Failed to deallocate grid%v10max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spduv10max ) ) THEN 
  DEALLOCATE(grid%spduv10max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8707,&
'frame/module_domain.f: Failed to deallocate grid%spduv10max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tspduv10max ) ) THEN 
  DEALLOCATE(grid%tspduv10max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8714,&
'frame/module_domain.f: Failed to deallocate grid%tspduv10max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10mean ) ) THEN 
  DEALLOCATE(grid%u10mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8721,&
'frame/module_domain.f: Failed to deallocate grid%u10mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10mean ) ) THEN 
  DEALLOCATE(grid%v10mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8728,&
'frame/module_domain.f: Failed to deallocate grid%v10mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spduv10mean ) ) THEN 
  DEALLOCATE(grid%spduv10mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8735,&
'frame/module_domain.f: Failed to deallocate grid%spduv10mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10std ) ) THEN 
  DEALLOCATE(grid%u10std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8742,&
'frame/module_domain.f: Failed to deallocate grid%u10std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10std ) ) THEN 
  DEALLOCATE(grid%v10std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8749,&
'frame/module_domain.f: Failed to deallocate grid%v10std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spduv10std ) ) THEN 
  DEALLOCATE(grid%spduv10std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8756,&
'frame/module_domain.f: Failed to deallocate grid%spduv10std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincvmax ) ) THEN 
  DEALLOCATE(grid%raincvmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8763,&
'frame/module_domain.f: Failed to deallocate grid%raincvmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainncvmax ) ) THEN 
  DEALLOCATE(grid%rainncvmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8770,&
'frame/module_domain.f: Failed to deallocate grid%rainncvmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%traincvmax ) ) THEN 
  DEALLOCATE(grid%traincvmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8777,&
'frame/module_domain.f: Failed to deallocate grid%traincvmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%trainncvmax ) ) THEN 
  DEALLOCATE(grid%trainncvmax,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8784,&
'frame/module_domain.f: Failed to deallocate grid%trainncvmax. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincvmean ) ) THEN 
  DEALLOCATE(grid%raincvmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8791,&
'frame/module_domain.f: Failed to deallocate grid%raincvmean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainncvmean ) ) THEN 
  DEALLOCATE(grid%rainncvmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8798,&
'frame/module_domain.f: Failed to deallocate grid%rainncvmean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%raincvstd ) ) THEN 
  DEALLOCATE(grid%raincvstd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8805,&
'frame/module_domain.f: Failed to deallocate grid%raincvstd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rainncvstd ) ) THEN 
  DEALLOCATE(grid%rainncvstd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8812,&
'frame/module_domain.f: Failed to deallocate grid%rainncvstd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswupt ) ) THEN 
  DEALLOCATE(grid%acswupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8819,&
'frame/module_domain.f: Failed to deallocate grid%acswupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswuptc ) ) THEN 
  DEALLOCATE(grid%acswuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8826,&
'frame/module_domain.f: Failed to deallocate grid%acswuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswdnt ) ) THEN 
  DEALLOCATE(grid%acswdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8833,&
'frame/module_domain.f: Failed to deallocate grid%acswdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswdntc ) ) THEN 
  DEALLOCATE(grid%acswdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8840,&
'frame/module_domain.f: Failed to deallocate grid%acswdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswupb ) ) THEN 
  DEALLOCATE(grid%acswupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8847,&
'frame/module_domain.f: Failed to deallocate grid%acswupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswupbc ) ) THEN 
  DEALLOCATE(grid%acswupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8854,&
'frame/module_domain.f: Failed to deallocate grid%acswupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswdnb ) ) THEN 
  DEALLOCATE(grid%acswdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8861,&
'frame/module_domain.f: Failed to deallocate grid%acswdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%acswdnbc ) ) THEN 
  DEALLOCATE(grid%acswdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8868,&
'frame/module_domain.f: Failed to deallocate grid%acswdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwupt ) ) THEN 
  DEALLOCATE(grid%aclwupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8875,&
'frame/module_domain.f: Failed to deallocate grid%aclwupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwuptc ) ) THEN 
  DEALLOCATE(grid%aclwuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8882,&
'frame/module_domain.f: Failed to deallocate grid%aclwuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwdnt ) ) THEN 
  DEALLOCATE(grid%aclwdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8889,&
'frame/module_domain.f: Failed to deallocate grid%aclwdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwdntc ) ) THEN 
  DEALLOCATE(grid%aclwdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8896,&
'frame/module_domain.f: Failed to deallocate grid%aclwdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwupb ) ) THEN 
  DEALLOCATE(grid%aclwupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8903,&
'frame/module_domain.f: Failed to deallocate grid%aclwupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwupbc ) ) THEN 
  DEALLOCATE(grid%aclwupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8910,&
'frame/module_domain.f: Failed to deallocate grid%aclwupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwdnb ) ) THEN 
  DEALLOCATE(grid%aclwdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8917,&
'frame/module_domain.f: Failed to deallocate grid%aclwdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclwdnbc ) ) THEN 
  DEALLOCATE(grid%aclwdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8924,&
'frame/module_domain.f: Failed to deallocate grid%aclwdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswupt ) ) THEN 
  DEALLOCATE(grid%i_acswupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8931,&
'frame/module_domain.f: Failed to deallocate grid%i_acswupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswuptc ) ) THEN 
  DEALLOCATE(grid%i_acswuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8938,&
'frame/module_domain.f: Failed to deallocate grid%i_acswuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswdnt ) ) THEN 
  DEALLOCATE(grid%i_acswdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8945,&
'frame/module_domain.f: Failed to deallocate grid%i_acswdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswdntc ) ) THEN 
  DEALLOCATE(grid%i_acswdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8952,&
'frame/module_domain.f: Failed to deallocate grid%i_acswdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswupb ) ) THEN 
  DEALLOCATE(grid%i_acswupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8959,&
'frame/module_domain.f: Failed to deallocate grid%i_acswupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswupbc ) ) THEN 
  DEALLOCATE(grid%i_acswupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8966,&
'frame/module_domain.f: Failed to deallocate grid%i_acswupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswdnb ) ) THEN 
  DEALLOCATE(grid%i_acswdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8973,&
'frame/module_domain.f: Failed to deallocate grid%i_acswdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_acswdnbc ) ) THEN 
  DEALLOCATE(grid%i_acswdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8980,&
'frame/module_domain.f: Failed to deallocate grid%i_acswdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwupt ) ) THEN 
  DEALLOCATE(grid%i_aclwupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8987,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwuptc ) ) THEN 
  DEALLOCATE(grid%i_aclwuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",8994,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwdnt ) ) THEN 
  DEALLOCATE(grid%i_aclwdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9001,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwdntc ) ) THEN 
  DEALLOCATE(grid%i_aclwdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9008,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwupb ) ) THEN 
  DEALLOCATE(grid%i_aclwupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9015,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwupbc ) ) THEN 
  DEALLOCATE(grid%i_aclwupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9022,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwdnb ) ) THEN 
  DEALLOCATE(grid%i_aclwdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9029,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_aclwdnbc ) ) THEN 
  DEALLOCATE(grid%i_aclwdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9036,&
'frame/module_domain.f: Failed to deallocate grid%i_aclwdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupt ) ) THEN 
  DEALLOCATE(grid%swupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9043,&
'frame/module_domain.f: Failed to deallocate grid%swupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swuptc ) ) THEN 
  DEALLOCATE(grid%swuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9050,&
'frame/module_domain.f: Failed to deallocate grid%swuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swuptcln ) ) THEN 
  DEALLOCATE(grid%swuptcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9057,&
'frame/module_domain.f: Failed to deallocate grid%swuptcln. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnt ) ) THEN 
  DEALLOCATE(grid%swdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9064,&
'frame/module_domain.f: Failed to deallocate grid%swdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdntc ) ) THEN 
  DEALLOCATE(grid%swdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9071,&
'frame/module_domain.f: Failed to deallocate grid%swdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdntcln ) ) THEN 
  DEALLOCATE(grid%swdntcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9078,&
'frame/module_domain.f: Failed to deallocate grid%swdntcln. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupb ) ) THEN 
  DEALLOCATE(grid%swupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9085,&
'frame/module_domain.f: Failed to deallocate grid%swupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupbc ) ) THEN 
  DEALLOCATE(grid%swupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9092,&
'frame/module_domain.f: Failed to deallocate grid%swupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupbcln ) ) THEN 
  DEALLOCATE(grid%swupbcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9099,&
'frame/module_domain.f: Failed to deallocate grid%swupbcln. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnb ) ) THEN 
  DEALLOCATE(grid%swdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9106,&
'frame/module_domain.f: Failed to deallocate grid%swdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnbc ) ) THEN 
  DEALLOCATE(grid%swdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9113,&
'frame/module_domain.f: Failed to deallocate grid%swdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnbcln ) ) THEN 
  DEALLOCATE(grid%swdnbcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9120,&
'frame/module_domain.f: Failed to deallocate grid%swdnbcln. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupt ) ) THEN 
  DEALLOCATE(grid%lwupt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9127,&
'frame/module_domain.f: Failed to deallocate grid%lwupt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwuptc ) ) THEN 
  DEALLOCATE(grid%lwuptc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9134,&
'frame/module_domain.f: Failed to deallocate grid%lwuptc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwuptcln ) ) THEN 
  DEALLOCATE(grid%lwuptcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9141,&
'frame/module_domain.f: Failed to deallocate grid%lwuptcln. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnt ) ) THEN 
  DEALLOCATE(grid%lwdnt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9148,&
'frame/module_domain.f: Failed to deallocate grid%lwdnt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdntc ) ) THEN 
  DEALLOCATE(grid%lwdntc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9155,&
'frame/module_domain.f: Failed to deallocate grid%lwdntc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdntcln ) ) THEN 
  DEALLOCATE(grid%lwdntcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9162,&
'frame/module_domain.f: Failed to deallocate grid%lwdntcln. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupb ) ) THEN 
  DEALLOCATE(grid%lwupb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9169,&
'frame/module_domain.f: Failed to deallocate grid%lwupb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupbc ) ) THEN 
  DEALLOCATE(grid%lwupbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9176,&
'frame/module_domain.f: Failed to deallocate grid%lwupbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupbcln ) ) THEN 
  DEALLOCATE(grid%lwupbcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9183,&
'frame/module_domain.f: Failed to deallocate grid%lwupbcln. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnb ) ) THEN 
  DEALLOCATE(grid%lwdnb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9190,&
'frame/module_domain.f: Failed to deallocate grid%lwdnb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnbc ) ) THEN 
  DEALLOCATE(grid%lwdnbc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9197,&
'frame/module_domain.f: Failed to deallocate grid%lwdnbc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnbcln ) ) THEN 
  DEALLOCATE(grid%lwdnbcln,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9204,&
'frame/module_domain.f: Failed to deallocate grid%lwdnbcln. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swcf ) ) THEN 
  DEALLOCATE(grid%swcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9211,&
'frame/module_domain.f: Failed to deallocate grid%swcf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwcf ) ) THEN 
  DEALLOCATE(grid%lwcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9218,&
'frame/module_domain.f: Failed to deallocate grid%lwcf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%olr ) ) THEN 
  DEALLOCATE(grid%olr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9225,&
'frame/module_domain.f: Failed to deallocate grid%olr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlat_u ) ) THEN 
  DEALLOCATE(grid%xlat_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9232,&
'frame/module_domain.f: Failed to deallocate grid%xlat_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlong_u ) ) THEN 
  DEALLOCATE(grid%xlong_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9239,&
'frame/module_domain.f: Failed to deallocate grid%xlong_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlat_v ) ) THEN 
  DEALLOCATE(grid%xlat_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9246,&
'frame/module_domain.f: Failed to deallocate grid%xlat_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xlong_v ) ) THEN 
  DEALLOCATE(grid%xlong_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9253,&
'frame/module_domain.f: Failed to deallocate grid%xlong_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albedo ) ) THEN 
  DEALLOCATE(grid%albedo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9260,&
'frame/module_domain.f: Failed to deallocate grid%albedo. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%clat ) ) THEN 
  DEALLOCATE(grid%clat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9267,&
'frame/module_domain.f: Failed to deallocate grid%clat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albbck ) ) THEN 
  DEALLOCATE(grid%albbck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9274,&
'frame/module_domain.f: Failed to deallocate grid%albbck. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%embck ) ) THEN 
  DEALLOCATE(grid%embck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9281,&
'frame/module_domain.f: Failed to deallocate grid%embck. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%emiss ) ) THEN 
  DEALLOCATE(grid%emiss,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9288,&
'frame/module_domain.f: Failed to deallocate grid%emiss. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snotime ) ) THEN 
  DEALLOCATE(grid%snotime,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9295,&
'frame/module_domain.f: Failed to deallocate grid%snotime. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%noahres ) ) THEN 
  DEALLOCATE(grid%noahres,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9302,&
'frame/module_domain.f: Failed to deallocate grid%noahres. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldefi ) ) THEN 
  DEALLOCATE(grid%cldefi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9309,&
'frame/module_domain.f: Failed to deallocate grid%cldefi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rublten ) ) THEN 
  DEALLOCATE(grid%rublten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9316,&
'frame/module_domain.f: Failed to deallocate grid%rublten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rvblten ) ) THEN 
  DEALLOCATE(grid%rvblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9323,&
'frame/module_domain.f: Failed to deallocate grid%rvblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthblten ) ) THEN 
  DEALLOCATE(grid%rthblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9330,&
'frame/module_domain.f: Failed to deallocate grid%rthblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvblten ) ) THEN 
  DEALLOCATE(grid%rqvblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9337,&
'frame/module_domain.f: Failed to deallocate grid%rqvblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqcblten ) ) THEN 
  DEALLOCATE(grid%rqcblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9344,&
'frame/module_domain.f: Failed to deallocate grid%rqcblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqiblten ) ) THEN 
  DEALLOCATE(grid%rqiblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9351,&
'frame/module_domain.f: Failed to deallocate grid%rqiblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqniblten ) ) THEN 
  DEALLOCATE(grid%rqniblten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9358,&
'frame/module_domain.f: Failed to deallocate grid%rqniblten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flx4 ) ) THEN 
  DEALLOCATE(grid%flx4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9365,&
'frame/module_domain.f: Failed to deallocate grid%flx4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fvb ) ) THEN 
  DEALLOCATE(grid%fvb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9372,&
'frame/module_domain.f: Failed to deallocate grid%fvb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fbur ) ) THEN 
  DEALLOCATE(grid%fbur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9379,&
'frame/module_domain.f: Failed to deallocate grid%fbur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fgsn ) ) THEN 
  DEALLOCATE(grid%fgsn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9386,&
'frame/module_domain.f: Failed to deallocate grid%fgsn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_mosaic ) ) THEN 
  DEALLOCATE(grid%tsk_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9393,&
'frame/module_domain.f: Failed to deallocate grid%tsk_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsfc_mosaic ) ) THEN 
  DEALLOCATE(grid%qsfc_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9400,&
'frame/module_domain.f: Failed to deallocate grid%qsfc_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tslb_mosaic ) ) THEN 
  DEALLOCATE(grid%tslb_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9407,&
'frame/module_domain.f: Failed to deallocate grid%tslb_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smois_mosaic ) ) THEN 
  DEALLOCATE(grid%smois_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9414,&
'frame/module_domain.f: Failed to deallocate grid%smois_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh2o_mosaic ) ) THEN 
  DEALLOCATE(grid%sh2o_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9421,&
'frame/module_domain.f: Failed to deallocate grid%sh2o_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canwat_mosaic ) ) THEN 
  DEALLOCATE(grid%canwat_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9428,&
'frame/module_domain.f: Failed to deallocate grid%canwat_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snow_mosaic ) ) THEN 
  DEALLOCATE(grid%snow_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9435,&
'frame/module_domain.f: Failed to deallocate grid%snow_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowh_mosaic ) ) THEN 
  DEALLOCATE(grid%snowh_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9442,&
'frame/module_domain.f: Failed to deallocate grid%snowh_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowc_mosaic ) ) THEN 
  DEALLOCATE(grid%snowc_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9449,&
'frame/module_domain.f: Failed to deallocate grid%snowc_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albedo_mosaic ) ) THEN 
  DEALLOCATE(grid%albedo_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9456,&
'frame/module_domain.f: Failed to deallocate grid%albedo_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albbck_mosaic ) ) THEN 
  DEALLOCATE(grid%albbck_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9463,&
'frame/module_domain.f: Failed to deallocate grid%albbck_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%emiss_mosaic ) ) THEN 
  DEALLOCATE(grid%emiss_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9470,&
'frame/module_domain.f: Failed to deallocate grid%emiss_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%embck_mosaic ) ) THEN 
  DEALLOCATE(grid%embck_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9477,&
'frame/module_domain.f: Failed to deallocate grid%embck_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%znt_mosaic ) ) THEN 
  DEALLOCATE(grid%znt_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9484,&
'frame/module_domain.f: Failed to deallocate grid%znt_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z0_mosaic ) ) THEN 
  DEALLOCATE(grid%z0_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9491,&
'frame/module_domain.f: Failed to deallocate grid%z0_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lai_mosaic ) ) THEN 
  DEALLOCATE(grid%lai_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9498,&
'frame/module_domain.f: Failed to deallocate grid%lai_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rs_mosaic ) ) THEN 
  DEALLOCATE(grid%rs_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9505,&
'frame/module_domain.f: Failed to deallocate grid%rs_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_mosaic ) ) THEN 
  DEALLOCATE(grid%hfx_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9512,&
'frame/module_domain.f: Failed to deallocate grid%hfx_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qfx_mosaic ) ) THEN 
  DEALLOCATE(grid%qfx_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9519,&
'frame/module_domain.f: Failed to deallocate grid%qfx_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_mosaic ) ) THEN 
  DEALLOCATE(grid%lh_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9526,&
'frame/module_domain.f: Failed to deallocate grid%lh_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grdflx_mosaic ) ) THEN 
  DEALLOCATE(grid%grdflx_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9533,&
'frame/module_domain.f: Failed to deallocate grid%grdflx_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snotime_mosaic ) ) THEN 
  DEALLOCATE(grid%snotime_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9540,&
'frame/module_domain.f: Failed to deallocate grid%snotime_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tr_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tr_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9547,&
'frame/module_domain.f: Failed to deallocate grid%tr_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tb_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tb_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9554,&
'frame/module_domain.f: Failed to deallocate grid%tb_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tg_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tg_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9561,&
'frame/module_domain.f: Failed to deallocate grid%tg_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tc_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%tc_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9568,&
'frame/module_domain.f: Failed to deallocate grid%tc_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%ts_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9575,&
'frame/module_domain.f: Failed to deallocate grid%ts_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ts_rul2d_mosaic ) ) THEN 
  DEALLOCATE(grid%ts_rul2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9582,&
'frame/module_domain.f: Failed to deallocate grid%ts_rul2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qc_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%qc_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9589,&
'frame/module_domain.f: Failed to deallocate grid%qc_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uc_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%uc_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9596,&
'frame/module_domain.f: Failed to deallocate grid%uc_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%trl_urb3d_mosaic ) ) THEN 
  DEALLOCATE(grid%trl_urb3d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9603,&
'frame/module_domain.f: Failed to deallocate grid%trl_urb3d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tbl_urb3d_mosaic ) ) THEN 
  DEALLOCATE(grid%tbl_urb3d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9610,&
'frame/module_domain.f: Failed to deallocate grid%tbl_urb3d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgl_urb3d_mosaic ) ) THEN 
  DEALLOCATE(grid%tgl_urb3d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9617,&
'frame/module_domain.f: Failed to deallocate grid%tgl_urb3d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sh_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%sh_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9624,&
'frame/module_domain.f: Failed to deallocate grid%sh_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%lh_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9631,&
'frame/module_domain.f: Failed to deallocate grid%lh_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%g_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%g_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9638,&
'frame/module_domain.f: Failed to deallocate grid%g_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rn_urb2d_mosaic ) ) THEN 
  DEALLOCATE(grid%rn_urb2d_mosaic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9645,&
'frame/module_domain.f: Failed to deallocate grid%rn_urb2d_mosaic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mosaic_cat_index ) ) THEN 
  DEALLOCATE(grid%mosaic_cat_index,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9652,&
'frame/module_domain.f: Failed to deallocate grid%mosaic_cat_index. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%landusef2 ) ) THEN 
  DEALLOCATE(grid%landusef2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9659,&
'frame/module_domain.f: Failed to deallocate grid%landusef2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mp_restart_state ) ) THEN 
  DEALLOCATE(grid%mp_restart_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9666,&
'frame/module_domain.f: Failed to deallocate grid%mp_restart_state. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tbpvs_state ) ) THEN 
  DEALLOCATE(grid%tbpvs_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9673,&
'frame/module_domain.f: Failed to deallocate grid%tbpvs_state. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tbpvs0_state ) ) THEN 
  DEALLOCATE(grid%tbpvs0_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9680,&
'frame/module_domain.f: Failed to deallocate grid%tbpvs0_state. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lu_state ) ) THEN 
  DEALLOCATE(grid%lu_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9687,&
'frame/module_domain.f: Failed to deallocate grid%lu_state. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_phy ) ) THEN 
  DEALLOCATE(grid%t_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9694,&
'frame/module_domain.f: Failed to deallocate grid%t_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_phy ) ) THEN 
  DEALLOCATE(grid%u_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9701,&
'frame/module_domain.f: Failed to deallocate grid%u_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_phy ) ) THEN 
  DEALLOCATE(grid%v_phy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9708,&
'frame/module_domain.f: Failed to deallocate grid%v_phy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tmn ) ) THEN 
  DEALLOCATE(grid%tmn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9715,&
'frame/module_domain.f: Failed to deallocate grid%tmn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tyr ) ) THEN 
  DEALLOCATE(grid%tyr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9722,&
'frame/module_domain.f: Failed to deallocate grid%tyr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tyra ) ) THEN 
  DEALLOCATE(grid%tyra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9729,&
'frame/module_domain.f: Failed to deallocate grid%tyra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tdly ) ) THEN 
  DEALLOCATE(grid%tdly,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9736,&
'frame/module_domain.f: Failed to deallocate grid%tdly. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tlag ) ) THEN 
  DEALLOCATE(grid%tlag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9743,&
'frame/module_domain.f: Failed to deallocate grid%tlag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xland ) ) THEN 
  DEALLOCATE(grid%xland,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9750,&
'frame/module_domain.f: Failed to deallocate grid%xland. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cplmask ) ) THEN 
  DEALLOCATE(grid%cplmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9757,&
'frame/module_domain.f: Failed to deallocate grid%cplmask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%znt ) ) THEN 
  DEALLOCATE(grid%znt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9764,&
'frame/module_domain.f: Failed to deallocate grid%znt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ck ) ) THEN 
  DEALLOCATE(grid%ck,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9771,&
'frame/module_domain.f: Failed to deallocate grid%ck. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cka ) ) THEN 
  DEALLOCATE(grid%cka,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9778,&
'frame/module_domain.f: Failed to deallocate grid%cka. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cd ) ) THEN 
  DEALLOCATE(grid%cd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9785,&
'frame/module_domain.f: Failed to deallocate grid%cd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cda ) ) THEN 
  DEALLOCATE(grid%cda,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9792,&
'frame/module_domain.f: Failed to deallocate grid%cda. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ust ) ) THEN 
  DEALLOCATE(grid%ust,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9799,&
'frame/module_domain.f: Failed to deallocate grid%ust. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ustm ) ) THEN 
  DEALLOCATE(grid%ustm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9806,&
'frame/module_domain.f: Failed to deallocate grid%ustm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rmol ) ) THEN 
  DEALLOCATE(grid%rmol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9813,&
'frame/module_domain.f: Failed to deallocate grid%rmol. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mol ) ) THEN 
  DEALLOCATE(grid%mol,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9820,&
'frame/module_domain.f: Failed to deallocate grid%mol. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pblh ) ) THEN 
  DEALLOCATE(grid%pblh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9827,&
'frame/module_domain.f: Failed to deallocate grid%pblh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%capg ) ) THEN 
  DEALLOCATE(grid%capg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9834,&
'frame/module_domain.f: Failed to deallocate grid%capg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thc ) ) THEN 
  DEALLOCATE(grid%thc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9841,&
'frame/module_domain.f: Failed to deallocate grid%thc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx ) ) THEN 
  DEALLOCATE(grid%hfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9848,&
'frame/module_domain.f: Failed to deallocate grid%hfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qfx ) ) THEN 
  DEALLOCATE(grid%qfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9855,&
'frame/module_domain.f: Failed to deallocate grid%qfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh ) ) THEN 
  DEALLOCATE(grid%lh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9862,&
'frame/module_domain.f: Failed to deallocate grid%lh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%achfx ) ) THEN 
  DEALLOCATE(grid%achfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9869,&
'frame/module_domain.f: Failed to deallocate grid%achfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wstar ) ) THEN 
  DEALLOCATE(grid%wstar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9876,&
'frame/module_domain.f: Failed to deallocate grid%wstar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aclhf ) ) THEN 
  DEALLOCATE(grid%aclhf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9883,&
'frame/module_domain.f: Failed to deallocate grid%aclhf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flhc ) ) THEN 
  DEALLOCATE(grid%flhc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9890,&
'frame/module_domain.f: Failed to deallocate grid%flhc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flqc ) ) THEN 
  DEALLOCATE(grid%flqc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9897,&
'frame/module_domain.f: Failed to deallocate grid%flqc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsg ) ) THEN 
  DEALLOCATE(grid%qsg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9904,&
'frame/module_domain.f: Failed to deallocate grid%qsg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qvg ) ) THEN 
  DEALLOCATE(grid%qvg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9911,&
'frame/module_domain.f: Failed to deallocate grid%qvg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_qvg ) ) THEN 
  DEALLOCATE(grid%dfi_qvg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9918,&
'frame/module_domain.f: Failed to deallocate grid%dfi_qvg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qcg ) ) THEN 
  DEALLOCATE(grid%qcg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9925,&
'frame/module_domain.f: Failed to deallocate grid%qcg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dew ) ) THEN 
  DEALLOCATE(grid%dew,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9932,&
'frame/module_domain.f: Failed to deallocate grid%dew. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilt1 ) ) THEN 
  DEALLOCATE(grid%soilt1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9939,&
'frame/module_domain.f: Failed to deallocate grid%soilt1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_soilt1 ) ) THEN 
  DEALLOCATE(grid%dfi_soilt1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9946,&
'frame/module_domain.f: Failed to deallocate grid%dfi_soilt1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsnav ) ) THEN 
  DEALLOCATE(grid%tsnav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9953,&
'frame/module_domain.f: Failed to deallocate grid%tsnav. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_tsnav ) ) THEN 
  DEALLOCATE(grid%dfi_tsnav,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9960,&
'frame/module_domain.f: Failed to deallocate grid%dfi_tsnav. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%regime ) ) THEN 
  DEALLOCATE(grid%regime,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9967,&
'frame/module_domain.f: Failed to deallocate grid%regime. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowc ) ) THEN 
  DEALLOCATE(grid%snowc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9974,&
'frame/module_domain.f: Failed to deallocate grid%snowc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfi_snowc ) ) THEN 
  DEALLOCATE(grid%dfi_snowc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9981,&
'frame/module_domain.f: Failed to deallocate grid%dfi_snowc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mavail ) ) THEN 
  DEALLOCATE(grid%mavail,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9988,&
'frame/module_domain.f: Failed to deallocate grid%mavail. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tkesfcf ) ) THEN 
  DEALLOCATE(grid%tkesfcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",9995,&
'frame/module_domain.f: Failed to deallocate grid%tkesfcf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sr ) ) THEN 
  DEALLOCATE(grid%sr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10002,&
'frame/module_domain.f: Failed to deallocate grid%sr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%potevp ) ) THEN 
  DEALLOCATE(grid%potevp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10009,&
'frame/module_domain.f: Failed to deallocate grid%potevp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snopcx ) ) THEN 
  DEALLOCATE(grid%snopcx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10016,&
'frame/module_domain.f: Failed to deallocate grid%snopcx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soiltb ) ) THEN 
  DEALLOCATE(grid%soiltb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10023,&
'frame/module_domain.f: Failed to deallocate grid%soiltb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taucldi ) ) THEN 
  DEALLOCATE(grid%taucldi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10030,&
'frame/module_domain.f: Failed to deallocate grid%taucldi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taucldc ) ) THEN 
  DEALLOCATE(grid%taucldc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10037,&
'frame/module_domain.f: Failed to deallocate grid%taucldc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor11 ) ) THEN 
  DEALLOCATE(grid%defor11,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10044,&
'frame/module_domain.f: Failed to deallocate grid%defor11. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor22 ) ) THEN 
  DEALLOCATE(grid%defor22,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10051,&
'frame/module_domain.f: Failed to deallocate grid%defor22. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor12 ) ) THEN 
  DEALLOCATE(grid%defor12,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10058,&
'frame/module_domain.f: Failed to deallocate grid%defor12. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor33 ) ) THEN 
  DEALLOCATE(grid%defor33,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10065,&
'frame/module_domain.f: Failed to deallocate grid%defor33. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor13 ) ) THEN 
  DEALLOCATE(grid%defor13,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10072,&
'frame/module_domain.f: Failed to deallocate grid%defor13. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%defor23 ) ) THEN 
  DEALLOCATE(grid%defor23,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10079,&
'frame/module_domain.f: Failed to deallocate grid%defor23. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xkmv ) ) THEN 
  DEALLOCATE(grid%xkmv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10086,&
'frame/module_domain.f: Failed to deallocate grid%xkmv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xkmh ) ) THEN 
  DEALLOCATE(grid%xkmh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10093,&
'frame/module_domain.f: Failed to deallocate grid%xkmh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xkhv ) ) THEN 
  DEALLOCATE(grid%xkhv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10100,&
'frame/module_domain.f: Failed to deallocate grid%xkhv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xkhh ) ) THEN 
  DEALLOCATE(grid%xkhh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10107,&
'frame/module_domain.f: Failed to deallocate grid%xkhh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%div ) ) THEN 
  DEALLOCATE(grid%div,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10114,&
'frame/module_domain.f: Failed to deallocate grid%div. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bn2 ) ) THEN 
  DEALLOCATE(grid%bn2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10121,&
'frame/module_domain.f: Failed to deallocate grid%bn2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rundgdten ) ) THEN 
  DEALLOCATE(grid%rundgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10128,&
'frame/module_domain.f: Failed to deallocate grid%rundgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rvndgdten ) ) THEN 
  DEALLOCATE(grid%rvndgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10135,&
'frame/module_domain.f: Failed to deallocate grid%rvndgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthndgdten ) ) THEN 
  DEALLOCATE(grid%rthndgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10142,&
'frame/module_domain.f: Failed to deallocate grid%rthndgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rphndgdten ) ) THEN 
  DEALLOCATE(grid%rphndgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10149,&
'frame/module_domain.f: Failed to deallocate grid%rphndgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvndgdten ) ) THEN 
  DEALLOCATE(grid%rqvndgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10156,&
'frame/module_domain.f: Failed to deallocate grid%rqvndgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rmundgdten ) ) THEN 
  DEALLOCATE(grid%rmundgdten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10163,&
'frame/module_domain.f: Failed to deallocate grid%rmundgdten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdda3d ) ) THEN 
  DEALLOCATE(grid%fdda3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10170,&
'frame/module_domain.f: Failed to deallocate grid%fdda3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdda2d ) ) THEN 
  DEALLOCATE(grid%fdda2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10177,&
'frame/module_domain.f: Failed to deallocate grid%fdda2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10_ndg_old ) ) THEN 
  DEALLOCATE(grid%u10_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10184,&
'frame/module_domain.f: Failed to deallocate grid%u10_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10_ndg_new ) ) THEN 
  DEALLOCATE(grid%u10_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10191,&
'frame/module_domain.f: Failed to deallocate grid%u10_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10_ndg_old ) ) THEN 
  DEALLOCATE(grid%v10_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10198,&
'frame/module_domain.f: Failed to deallocate grid%v10_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10_ndg_new ) ) THEN 
  DEALLOCATE(grid%v10_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10205,&
'frame/module_domain.f: Failed to deallocate grid%v10_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2_ndg_old ) ) THEN 
  DEALLOCATE(grid%t2_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10212,&
'frame/module_domain.f: Failed to deallocate grid%t2_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2_ndg_new ) ) THEN 
  DEALLOCATE(grid%t2_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10219,&
'frame/module_domain.f: Failed to deallocate grid%t2_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2_ndg_old ) ) THEN 
  DEALLOCATE(grid%th2_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10226,&
'frame/module_domain.f: Failed to deallocate grid%th2_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2_ndg_new ) ) THEN 
  DEALLOCATE(grid%th2_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10233,&
'frame/module_domain.f: Failed to deallocate grid%th2_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2_ndg_old ) ) THEN 
  DEALLOCATE(grid%q2_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10240,&
'frame/module_domain.f: Failed to deallocate grid%q2_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2_ndg_new ) ) THEN 
  DEALLOCATE(grid%q2_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10247,&
'frame/module_domain.f: Failed to deallocate grid%q2_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_ndg_old ) ) THEN 
  DEALLOCATE(grid%rh_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10254,&
'frame/module_domain.f: Failed to deallocate grid%rh_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_ndg_new ) ) THEN 
  DEALLOCATE(grid%rh_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10261,&
'frame/module_domain.f: Failed to deallocate grid%rh_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psl_ndg_old ) ) THEN 
  DEALLOCATE(grid%psl_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10268,&
'frame/module_domain.f: Failed to deallocate grid%psl_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psl_ndg_new ) ) THEN 
  DEALLOCATE(grid%psl_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10275,&
'frame/module_domain.f: Failed to deallocate grid%psl_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ps_ndg_old ) ) THEN 
  DEALLOCATE(grid%ps_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10282,&
'frame/module_domain.f: Failed to deallocate grid%ps_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ps_ndg_new ) ) THEN 
  DEALLOCATE(grid%ps_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10289,&
'frame/module_domain.f: Failed to deallocate grid%ps_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tob_ndg_old ) ) THEN 
  DEALLOCATE(grid%tob_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10296,&
'frame/module_domain.f: Failed to deallocate grid%tob_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%odis_ndg_old ) ) THEN 
  DEALLOCATE(grid%odis_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10303,&
'frame/module_domain.f: Failed to deallocate grid%odis_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tob_ndg_new ) ) THEN 
  DEALLOCATE(grid%tob_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10310,&
'frame/module_domain.f: Failed to deallocate grid%tob_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%odis_ndg_new ) ) THEN 
  DEALLOCATE(grid%odis_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10317,&
'frame/module_domain.f: Failed to deallocate grid%odis_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sn_ndg_new ) ) THEN 
  DEALLOCATE(grid%sn_ndg_new,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10324,&
'frame/module_domain.f: Failed to deallocate grid%sn_ndg_new. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sn_ndg_old ) ) THEN 
  DEALLOCATE(grid%sn_ndg_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10331,&
'frame/module_domain.f: Failed to deallocate grid%sn_ndg_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sda_hfx ) ) THEN 
  DEALLOCATE(grid%sda_hfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10338,&
'frame/module_domain.f: Failed to deallocate grid%sda_hfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sda_qfx ) ) THEN 
  DEALLOCATE(grid%sda_qfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10345,&
'frame/module_domain.f: Failed to deallocate grid%sda_qfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qnorm ) ) THEN 
  DEALLOCATE(grid%qnorm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10352,&
'frame/module_domain.f: Failed to deallocate grid%qnorm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_both ) ) THEN 
  DEALLOCATE(grid%hfx_both,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10359,&
'frame/module_domain.f: Failed to deallocate grid%hfx_both. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qfx_both ) ) THEN 
  DEALLOCATE(grid%qfx_both,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10366,&
'frame/module_domain.f: Failed to deallocate grid%qfx_both. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_fdda ) ) THEN 
  DEALLOCATE(grid%hfx_fdda,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10373,&
'frame/module_domain.f: Failed to deallocate grid%hfx_fdda. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%abstot ) ) THEN 
  DEALLOCATE(grid%abstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10380,&
'frame/module_domain.f: Failed to deallocate grid%abstot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%absnxt ) ) THEN 
  DEALLOCATE(grid%absnxt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10387,&
'frame/module_domain.f: Failed to deallocate grid%absnxt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%emstot ) ) THEN 
  DEALLOCATE(grid%emstot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10394,&
'frame/module_domain.f: Failed to deallocate grid%emstot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dpsdt ) ) THEN 
  DEALLOCATE(grid%dpsdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10401,&
'frame/module_domain.f: Failed to deallocate grid%dpsdt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dmudt ) ) THEN 
  DEALLOCATE(grid%dmudt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10408,&
'frame/module_domain.f: Failed to deallocate grid%dmudt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pk1m ) ) THEN 
  DEALLOCATE(grid%pk1m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10415,&
'frame/module_domain.f: Failed to deallocate grid%pk1m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu_2m ) ) THEN 
  DEALLOCATE(grid%mu_2m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10422,&
'frame/module_domain.f: Failed to deallocate grid%mu_2m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wspd10max ) ) THEN 
  DEALLOCATE(grid%wspd10max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10429,&
'frame/module_domain.f: Failed to deallocate grid%wspd10max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_up_max ) ) THEN 
  DEALLOCATE(grid%w_up_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10436,&
'frame/module_domain.f: Failed to deallocate grid%w_up_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_dn_max ) ) THEN 
  DEALLOCATE(grid%w_dn_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10443,&
'frame/module_domain.f: Failed to deallocate grid%w_dn_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%refd_max ) ) THEN 
  DEALLOCATE(grid%refd_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10450,&
'frame/module_domain.f: Failed to deallocate grid%refd_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%up_heli_max ) ) THEN 
  DEALLOCATE(grid%up_heli_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10457,&
'frame/module_domain.f: Failed to deallocate grid%up_heli_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_mean ) ) THEN 
  DEALLOCATE(grid%w_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10464,&
'frame/module_domain.f: Failed to deallocate grid%w_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grpl_max ) ) THEN 
  DEALLOCATE(grid%grpl_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10471,&
'frame/module_domain.f: Failed to deallocate grid%grpl_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uh ) ) THEN 
  DEALLOCATE(grid%uh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10478,&
'frame/module_domain.f: Failed to deallocate grid%uh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_colmean ) ) THEN 
  DEALLOCATE(grid%w_colmean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10485,&
'frame/module_domain.f: Failed to deallocate grid%w_colmean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%numcolpts ) ) THEN 
  DEALLOCATE(grid%numcolpts,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10492,&
'frame/module_domain.f: Failed to deallocate grid%numcolpts. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grpl_colint ) ) THEN 
  DEALLOCATE(grid%grpl_colint,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10499,&
'frame/module_domain.f: Failed to deallocate grid%grpl_colint. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hail_maxk1 ) ) THEN 
  DEALLOCATE(grid%hail_maxk1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10506,&
'frame/module_domain.f: Failed to deallocate grid%hail_maxk1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hail_max2d ) ) THEN 
  DEALLOCATE(grid%hail_max2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10513,&
'frame/module_domain.f: Failed to deallocate grid%hail_max2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%prec_acc_c ) ) THEN 
  DEALLOCATE(grid%prec_acc_c,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10520,&
'frame/module_domain.f: Failed to deallocate grid%prec_acc_c. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%prec_acc_nc ) ) THEN 
  DEALLOCATE(grid%prec_acc_nc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10527,&
'frame/module_domain.f: Failed to deallocate grid%prec_acc_nc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snow_acc_nc ) ) THEN 
  DEALLOCATE(grid%snow_acc_nc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10534,&
'frame/module_domain.f: Failed to deallocate grid%snow_acc_nc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%advh_t ) ) THEN 
  DEALLOCATE(grid%advh_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10541,&
'frame/module_domain.f: Failed to deallocate grid%advh_t. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%advz_t ) ) THEN 
  DEALLOCATE(grid%advz_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10548,&
'frame/module_domain.f: Failed to deallocate grid%advz_t. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tml ) ) THEN 
  DEALLOCATE(grid%tml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10555,&
'frame/module_domain.f: Failed to deallocate grid%tml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t0ml ) ) THEN 
  DEALLOCATE(grid%t0ml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10562,&
'frame/module_domain.f: Failed to deallocate grid%t0ml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hml ) ) THEN 
  DEALLOCATE(grid%hml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10569,&
'frame/module_domain.f: Failed to deallocate grid%hml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h0ml ) ) THEN 
  DEALLOCATE(grid%h0ml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10576,&
'frame/module_domain.f: Failed to deallocate grid%h0ml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%huml ) ) THEN 
  DEALLOCATE(grid%huml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10583,&
'frame/module_domain.f: Failed to deallocate grid%huml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hvml ) ) THEN 
  DEALLOCATE(grid%hvml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10590,&
'frame/module_domain.f: Failed to deallocate grid%hvml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tmoml ) ) THEN 
  DEALLOCATE(grid%tmoml,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10597,&
'frame/module_domain.f: Failed to deallocate grid%tmoml. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_z ) ) THEN 
  DEALLOCATE(grid%track_z,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10604,&
'frame/module_domain.f: Failed to deallocate grid%track_z. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_t ) ) THEN 
  DEALLOCATE(grid%track_t,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10611,&
'frame/module_domain.f: Failed to deallocate grid%track_t. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_p ) ) THEN 
  DEALLOCATE(grid%track_p,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10618,&
'frame/module_domain.f: Failed to deallocate grid%track_p. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_u ) ) THEN 
  DEALLOCATE(grid%track_u,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10625,&
'frame/module_domain.f: Failed to deallocate grid%track_u. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_v ) ) THEN 
  DEALLOCATE(grid%track_v,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10632,&
'frame/module_domain.f: Failed to deallocate grid%track_v. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_w ) ) THEN 
  DEALLOCATE(grid%track_w,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10639,&
'frame/module_domain.f: Failed to deallocate grid%track_w. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_rh ) ) THEN 
  DEALLOCATE(grid%track_rh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10646,&
'frame/module_domain.f: Failed to deallocate grid%track_rh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_alt ) ) THEN 
  DEALLOCATE(grid%track_alt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10653,&
'frame/module_domain.f: Failed to deallocate grid%track_alt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_ele ) ) THEN 
  DEALLOCATE(grid%track_ele,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10660,&
'frame/module_domain.f: Failed to deallocate grid%track_ele. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_aircraft ) ) THEN 
  DEALLOCATE(grid%track_aircraft,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10667,&
'frame/module_domain.f: Failed to deallocate grid%track_aircraft. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qcloud ) ) THEN 
  DEALLOCATE(grid%track_qcloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10674,&
'frame/module_domain.f: Failed to deallocate grid%track_qcloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qrain ) ) THEN 
  DEALLOCATE(grid%track_qrain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10681,&
'frame/module_domain.f: Failed to deallocate grid%track_qrain. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qice ) ) THEN 
  DEALLOCATE(grid%track_qice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10688,&
'frame/module_domain.f: Failed to deallocate grid%track_qice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qsnow ) ) THEN 
  DEALLOCATE(grid%track_qsnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10695,&
'frame/module_domain.f: Failed to deallocate grid%track_qsnow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qgraup ) ) THEN 
  DEALLOCATE(grid%track_qgraup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10702,&
'frame/module_domain.f: Failed to deallocate grid%track_qgraup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%track_qvapor ) ) THEN 
  DEALLOCATE(grid%track_qvapor,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10709,&
'frame/module_domain.f: Failed to deallocate grid%track_qvapor. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail1 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10716,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail2 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10723,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail3 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10730,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail4 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10737,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_dhail5 ) ) THEN 
  DEALLOCATE(grid%hailcast_dhail5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10744,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_dhail5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_diam_max ) ) THEN 
  DEALLOCATE(grid%hailcast_diam_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10751,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_diam_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_diam_mean ) ) THEN 
  DEALLOCATE(grid%hailcast_diam_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10758,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_diam_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_diam_std ) ) THEN 
  DEALLOCATE(grid%hailcast_diam_std,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10765,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_diam_std. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_wup_mask ) ) THEN 
  DEALLOCATE(grid%hailcast_wup_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10772,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_wup_mask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hailcast_wdur ) ) THEN 
  DEALLOCATE(grid%hailcast_wdur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10779,&
'frame/module_domain.f: Failed to deallocate grid%hailcast_wdur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ic_flashcount ) ) THEN 
  DEALLOCATE(grid%ic_flashcount,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10786,&
'frame/module_domain.f: Failed to deallocate grid%ic_flashcount. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ic_flashrate ) ) THEN 
  DEALLOCATE(grid%ic_flashrate,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10793,&
'frame/module_domain.f: Failed to deallocate grid%ic_flashrate. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cg_flashcount ) ) THEN 
  DEALLOCATE(grid%cg_flashcount,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10800,&
'frame/module_domain.f: Failed to deallocate grid%cg_flashcount. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cg_flashrate ) ) THEN 
  DEALLOCATE(grid%cg_flashrate,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10807,&
'frame/module_domain.f: Failed to deallocate grid%cg_flashrate. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iccg_in_num ) ) THEN 
  DEALLOCATE(grid%iccg_in_num,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10814,&
'frame/module_domain.f: Failed to deallocate grid%iccg_in_num. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iccg_in_den ) ) THEN 
  DEALLOCATE(grid%iccg_in_den,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10821,&
'frame/module_domain.f: Failed to deallocate grid%iccg_in_den. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%varobs ) ) THEN 
  DEALLOCATE(grid%fdob%varobs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10828,&
'frame/module_domain.f: Failed to deallocate grid%fdob%varobs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%errf ) ) THEN 
  DEALLOCATE(grid%fdob%errf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10835,&
'frame/module_domain.f: Failed to deallocate grid%fdob%errf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%timeob ) ) THEN 
  DEALLOCATE(grid%fdob%timeob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10842,&
'frame/module_domain.f: Failed to deallocate grid%fdob%timeob. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%nlevs_ob ) ) THEN 
  DEALLOCATE(grid%fdob%nlevs_ob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10849,&
'frame/module_domain.f: Failed to deallocate grid%fdob%nlevs_ob. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%lev_in_ob ) ) THEN 
  DEALLOCATE(grid%fdob%lev_in_ob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10856,&
'frame/module_domain.f: Failed to deallocate grid%fdob%lev_in_ob. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%plfo ) ) THEN 
  DEALLOCATE(grid%fdob%plfo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10863,&
'frame/module_domain.f: Failed to deallocate grid%fdob%plfo. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%elevob ) ) THEN 
  DEALLOCATE(grid%fdob%elevob,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10870,&
'frame/module_domain.f: Failed to deallocate grid%fdob%elevob. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%rio ) ) THEN 
  DEALLOCATE(grid%fdob%rio,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10877,&
'frame/module_domain.f: Failed to deallocate grid%fdob%rio. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%rjo ) ) THEN 
  DEALLOCATE(grid%fdob%rjo,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10884,&
'frame/module_domain.f: Failed to deallocate grid%fdob%rjo. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%rko ) ) THEN 
  DEALLOCATE(grid%fdob%rko,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10891,&
'frame/module_domain.f: Failed to deallocate grid%fdob%rko. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%obsprt ) ) THEN 
  DEALLOCATE(grid%fdob%obsprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10898,&
'frame/module_domain.f: Failed to deallocate grid%fdob%obsprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%latprt ) ) THEN 
  DEALLOCATE(grid%fdob%latprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10905,&
'frame/module_domain.f: Failed to deallocate grid%fdob%latprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%lonprt ) ) THEN 
  DEALLOCATE(grid%fdob%lonprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10912,&
'frame/module_domain.f: Failed to deallocate grid%fdob%lonprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%mlatprt ) ) THEN 
  DEALLOCATE(grid%fdob%mlatprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10919,&
'frame/module_domain.f: Failed to deallocate grid%fdob%mlatprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%mlonprt ) ) THEN 
  DEALLOCATE(grid%fdob%mlonprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10926,&
'frame/module_domain.f: Failed to deallocate grid%fdob%mlonprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%stnidprt ) ) THEN 
  DEALLOCATE(grid%fdob%stnidprt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10933,&
'frame/module_domain.f: Failed to deallocate grid%fdob%stnidprt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdob%base_state ) ) THEN 
  DEALLOCATE(grid%fdob%base_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10940,&
'frame/module_domain.f: Failed to deallocate grid%fdob%base_state. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_xxx ) ) THEN 
  DEALLOCATE(grid%t_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10947,&
'frame/module_domain.f: Failed to deallocate grid%t_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_xxx ) ) THEN 
  DEALLOCATE(grid%u_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10954,&
'frame/module_domain.f: Failed to deallocate grid%u_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ru_xxx ) ) THEN 
  DEALLOCATE(grid%ru_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10961,&
'frame/module_domain.f: Failed to deallocate grid%ru_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_xxx ) ) THEN 
  DEALLOCATE(grid%v_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10968,&
'frame/module_domain.f: Failed to deallocate grid%v_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rv_xxx ) ) THEN 
  DEALLOCATE(grid%rv_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10975,&
'frame/module_domain.f: Failed to deallocate grid%rv_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_xxx ) ) THEN 
  DEALLOCATE(grid%w_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10982,&
'frame/module_domain.f: Failed to deallocate grid%w_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ww_xxx ) ) THEN 
  DEALLOCATE(grid%ww_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10989,&
'frame/module_domain.f: Failed to deallocate grid%ww_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ph_xxx ) ) THEN 
  DEALLOCATE(grid%ph_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",10996,&
'frame/module_domain.f: Failed to deallocate grid%ph_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dum_yyy ) ) THEN 
  DEALLOCATE(grid%dum_yyy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11003,&
'frame/module_domain.f: Failed to deallocate grid%dum_yyy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fourd_xxx ) ) THEN 
  DEALLOCATE(grid%fourd_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11010,&
'frame/module_domain.f: Failed to deallocate grid%fourd_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%clat_xxx ) ) THEN 
  DEALLOCATE(grid%clat_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11017,&
'frame/module_domain.f: Failed to deallocate grid%clat_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ht_xxx ) ) THEN 
  DEALLOCATE(grid%ht_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11024,&
'frame/module_domain.f: Failed to deallocate grid%ht_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mf_xxx ) ) THEN 
  DEALLOCATE(grid%mf_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11031,&
'frame/module_domain.f: Failed to deallocate grid%mf_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dif_analysis ) ) THEN 
  DEALLOCATE(grid%dif_analysis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11038,&
'frame/module_domain.f: Failed to deallocate grid%dif_analysis. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dif_xxx ) ) THEN 
  DEALLOCATE(grid%dif_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11045,&
'frame/module_domain.f: Failed to deallocate grid%dif_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dif_yyy ) ) THEN 
  DEALLOCATE(grid%dif_yyy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11052,&
'frame/module_domain.f: Failed to deallocate grid%dif_yyy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn_hist ) ) THEN 
  DEALLOCATE(grid%lfn_hist,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11059,&
'frame/module_domain.f: Failed to deallocate grid%lfn_hist. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn_time ) ) THEN 
  DEALLOCATE(grid%lfn_time,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11066,&
'frame/module_domain.f: Failed to deallocate grid%lfn_time. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nfuel_cat ) ) THEN 
  DEALLOCATE(grid%nfuel_cat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11073,&
'frame/module_domain.f: Failed to deallocate grid%nfuel_cat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zsf ) ) THEN 
  DEALLOCATE(grid%zsf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11080,&
'frame/module_domain.f: Failed to deallocate grid%zsf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzdxf ) ) THEN 
  DEALLOCATE(grid%dzdxf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11087,&
'frame/module_domain.f: Failed to deallocate grid%dzdxf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzdyf ) ) THEN 
  DEALLOCATE(grid%dzdyf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11094,&
'frame/module_domain.f: Failed to deallocate grid%dzdyf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tign_g ) ) THEN 
  DEALLOCATE(grid%tign_g,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11101,&
'frame/module_domain.f: Failed to deallocate grid%tign_g. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rthfrten ) ) THEN 
  DEALLOCATE(grid%rthfrten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11108,&
'frame/module_domain.f: Failed to deallocate grid%rthfrten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rqvfrten ) ) THEN 
  DEALLOCATE(grid%rqvfrten,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11115,&
'frame/module_domain.f: Failed to deallocate grid%rqvfrten. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avg_fuel_frac ) ) THEN 
  DEALLOCATE(grid%avg_fuel_frac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11122,&
'frame/module_domain.f: Failed to deallocate grid%avg_fuel_frac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grnhfx ) ) THEN 
  DEALLOCATE(grid%grnhfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11129,&
'frame/module_domain.f: Failed to deallocate grid%grnhfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grnqfx ) ) THEN 
  DEALLOCATE(grid%grnqfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11136,&
'frame/module_domain.f: Failed to deallocate grid%grnqfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canhfx ) ) THEN 
  DEALLOCATE(grid%canhfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11143,&
'frame/module_domain.f: Failed to deallocate grid%canhfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canqfx ) ) THEN 
  DEALLOCATE(grid%canqfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11150,&
'frame/module_domain.f: Failed to deallocate grid%canqfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uah ) ) THEN 
  DEALLOCATE(grid%uah,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11157,&
'frame/module_domain.f: Failed to deallocate grid%uah. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vah ) ) THEN 
  DEALLOCATE(grid%vah,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11164,&
'frame/module_domain.f: Failed to deallocate grid%vah. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grnhfx_fu ) ) THEN 
  DEALLOCATE(grid%grnhfx_fu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11171,&
'frame/module_domain.f: Failed to deallocate grid%grnhfx_fu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grnqfx_fu ) ) THEN 
  DEALLOCATE(grid%grnqfx_fu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11178,&
'frame/module_domain.f: Failed to deallocate grid%grnqfx_fu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn ) ) THEN 
  DEALLOCATE(grid%lfn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11185,&
'frame/module_domain.f: Failed to deallocate grid%lfn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn_0 ) ) THEN 
  DEALLOCATE(grid%lfn_0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11192,&
'frame/module_domain.f: Failed to deallocate grid%lfn_0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn_1 ) ) THEN 
  DEALLOCATE(grid%lfn_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11199,&
'frame/module_domain.f: Failed to deallocate grid%lfn_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn_2 ) ) THEN 
  DEALLOCATE(grid%lfn_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11206,&
'frame/module_domain.f: Failed to deallocate grid%lfn_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn_s0 ) ) THEN 
  DEALLOCATE(grid%lfn_s0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11213,&
'frame/module_domain.f: Failed to deallocate grid%lfn_s0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn_s1 ) ) THEN 
  DEALLOCATE(grid%lfn_s1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11220,&
'frame/module_domain.f: Failed to deallocate grid%lfn_s1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn_s2 ) ) THEN 
  DEALLOCATE(grid%lfn_s2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11227,&
'frame/module_domain.f: Failed to deallocate grid%lfn_s2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfn_s3 ) ) THEN 
  DEALLOCATE(grid%lfn_s3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11234,&
'frame/module_domain.f: Failed to deallocate grid%lfn_s3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fuel_frac ) ) THEN 
  DEALLOCATE(grid%fuel_frac,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11241,&
'frame/module_domain.f: Failed to deallocate grid%fuel_frac. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fire_area ) ) THEN 
  DEALLOCATE(grid%fire_area,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11248,&
'frame/module_domain.f: Failed to deallocate grid%fire_area. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uf ) ) THEN 
  DEALLOCATE(grid%uf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11255,&
'frame/module_domain.f: Failed to deallocate grid%uf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vf ) ) THEN 
  DEALLOCATE(grid%vf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11262,&
'frame/module_domain.f: Failed to deallocate grid%vf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fgrnhfx ) ) THEN 
  DEALLOCATE(grid%fgrnhfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11269,&
'frame/module_domain.f: Failed to deallocate grid%fgrnhfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fgrnqfx ) ) THEN 
  DEALLOCATE(grid%fgrnqfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11276,&
'frame/module_domain.f: Failed to deallocate grid%fgrnqfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fcanhfx ) ) THEN 
  DEALLOCATE(grid%fcanhfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11283,&
'frame/module_domain.f: Failed to deallocate grid%fcanhfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fcanqfx ) ) THEN 
  DEALLOCATE(grid%fcanqfx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11290,&
'frame/module_domain.f: Failed to deallocate grid%fcanqfx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ros ) ) THEN 
  DEALLOCATE(grid%ros,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11297,&
'frame/module_domain.f: Failed to deallocate grid%ros. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%burnt_area_dt ) ) THEN 
  DEALLOCATE(grid%burnt_area_dt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11304,&
'frame/module_domain.f: Failed to deallocate grid%burnt_area_dt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flame_length ) ) THEN 
  DEALLOCATE(grid%flame_length,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11311,&
'frame/module_domain.f: Failed to deallocate grid%flame_length. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ros_front ) ) THEN 
  DEALLOCATE(grid%ros_front,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11318,&
'frame/module_domain.f: Failed to deallocate grid%ros_front. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fxlong ) ) THEN 
  DEALLOCATE(grid%fxlong,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11325,&
'frame/module_domain.f: Failed to deallocate grid%fxlong. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fxlat ) ) THEN 
  DEALLOCATE(grid%fxlat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11332,&
'frame/module_domain.f: Failed to deallocate grid%fxlat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fuel_time ) ) THEN 
  DEALLOCATE(grid%fuel_time,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11339,&
'frame/module_domain.f: Failed to deallocate grid%fuel_time. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bbb ) ) THEN 
  DEALLOCATE(grid%bbb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11346,&
'frame/module_domain.f: Failed to deallocate grid%bbb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%betafl ) ) THEN 
  DEALLOCATE(grid%betafl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11353,&
'frame/module_domain.f: Failed to deallocate grid%betafl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%phiwc ) ) THEN 
  DEALLOCATE(grid%phiwc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11360,&
'frame/module_domain.f: Failed to deallocate grid%phiwc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%r_0 ) ) THEN 
  DEALLOCATE(grid%r_0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11367,&
'frame/module_domain.f: Failed to deallocate grid%r_0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fgip ) ) THEN 
  DEALLOCATE(grid%fgip,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11374,&
'frame/module_domain.f: Failed to deallocate grid%fgip. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ischap ) ) THEN 
  DEALLOCATE(grid%ischap,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11381,&
'frame/module_domain.f: Failed to deallocate grid%ischap. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fz0 ) ) THEN 
  DEALLOCATE(grid%fz0,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11388,&
'frame/module_domain.f: Failed to deallocate grid%fz0. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iboros ) ) THEN 
  DEALLOCATE(grid%iboros,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11395,&
'frame/module_domain.f: Failed to deallocate grid%iboros. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tracer ) ) THEN 
  DEALLOCATE(grid%tracer,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11402,&
'frame/module_domain.f: Failed to deallocate grid%tracer. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tracer_bxs ) ) THEN 
  DEALLOCATE(grid%tracer_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11409,&
'frame/module_domain.f: Failed to deallocate grid%tracer_bxs. ')
 endif
  NULLIFY(grid%tracer_bxs)
ENDIF
IF ( ASSOCIATED( grid%tracer_bxe ) ) THEN 
  DEALLOCATE(grid%tracer_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11417,&
'frame/module_domain.f: Failed to deallocate grid%tracer_bxe. ')
 endif
  NULLIFY(grid%tracer_bxe)
ENDIF
IF ( ASSOCIATED( grid%tracer_bys ) ) THEN 
  DEALLOCATE(grid%tracer_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11425,&
'frame/module_domain.f: Failed to deallocate grid%tracer_bys. ')
 endif
  NULLIFY(grid%tracer_bys)
ENDIF
IF ( ASSOCIATED( grid%tracer_bye ) ) THEN 
  DEALLOCATE(grid%tracer_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11433,&
'frame/module_domain.f: Failed to deallocate grid%tracer_bye. ')
 endif
  NULLIFY(grid%tracer_bye)
ENDIF
IF ( ASSOCIATED( grid%tracer_btxs ) ) THEN 
  DEALLOCATE(grid%tracer_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11441,&
'frame/module_domain.f: Failed to deallocate grid%tracer_btxs. ')
 endif
  NULLIFY(grid%tracer_btxs)
ENDIF
IF ( ASSOCIATED( grid%tracer_btxe ) ) THEN 
  DEALLOCATE(grid%tracer_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11449,&
'frame/module_domain.f: Failed to deallocate grid%tracer_btxe. ')
 endif
  NULLIFY(grid%tracer_btxe)
ENDIF
IF ( ASSOCIATED( grid%tracer_btys ) ) THEN 
  DEALLOCATE(grid%tracer_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11457,&
'frame/module_domain.f: Failed to deallocate grid%tracer_btys. ')
 endif
  NULLIFY(grid%tracer_btys)
ENDIF
IF ( ASSOCIATED( grid%tracer_btye ) ) THEN 
  DEALLOCATE(grid%tracer_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11465,&
'frame/module_domain.f: Failed to deallocate grid%tracer_btye. ')
 endif
  NULLIFY(grid%tracer_btye)
ENDIF
IF ( ASSOCIATED( grid%avgflx_rum ) ) THEN 
  DEALLOCATE(grid%avgflx_rum,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11473,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_rum. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_rvm ) ) THEN 
  DEALLOCATE(grid%avgflx_rvm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11480,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_rvm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_wwm ) ) THEN 
  DEALLOCATE(grid%avgflx_wwm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11487,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_wwm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_cfu1 ) ) THEN 
  DEALLOCATE(grid%avgflx_cfu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11494,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_cfu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_cfd1 ) ) THEN 
  DEALLOCATE(grid%avgflx_cfd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11501,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_cfd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_dfu1 ) ) THEN 
  DEALLOCATE(grid%avgflx_dfu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11508,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_dfu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_efu1 ) ) THEN 
  DEALLOCATE(grid%avgflx_efu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11515,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_efu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_dfd1 ) ) THEN 
  DEALLOCATE(grid%avgflx_dfd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11522,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_dfd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%avgflx_efd1 ) ) THEN 
  DEALLOCATE(grid%avgflx_efd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11529,&
'frame/module_domain.f: Failed to deallocate grid%avgflx_efd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfu1 ) ) THEN 
  DEALLOCATE(grid%cfu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11536,&
'frame/module_domain.f: Failed to deallocate grid%cfu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cfd1 ) ) THEN 
  DEALLOCATE(grid%cfd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11543,&
'frame/module_domain.f: Failed to deallocate grid%cfd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfu1 ) ) THEN 
  DEALLOCATE(grid%dfu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11550,&
'frame/module_domain.f: Failed to deallocate grid%dfu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%efu1 ) ) THEN 
  DEALLOCATE(grid%efu1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11557,&
'frame/module_domain.f: Failed to deallocate grid%efu1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dfd1 ) ) THEN 
  DEALLOCATE(grid%dfd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11564,&
'frame/module_domain.f: Failed to deallocate grid%dfd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%efd1 ) ) THEN 
  DEALLOCATE(grid%efd1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11571,&
'frame/module_domain.f: Failed to deallocate grid%efd1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vertstrucc ) ) THEN 
  DEALLOCATE(grid%vertstrucc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11578,&
'frame/module_domain.f: Failed to deallocate grid%vertstrucc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vertstrucs ) ) THEN 
  DEALLOCATE(grid%vertstrucs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11585,&
'frame/module_domain.f: Failed to deallocate grid%vertstrucs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_sf ) ) THEN 
  DEALLOCATE(grid%field_sf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11592,&
'frame/module_domain.f: Failed to deallocate grid%field_sf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_pbl ) ) THEN 
  DEALLOCATE(grid%field_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11599,&
'frame/module_domain.f: Failed to deallocate grid%field_pbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_conv ) ) THEN 
  DEALLOCATE(grid%field_conv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11606,&
'frame/module_domain.f: Failed to deallocate grid%field_conv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ru_tendf_stoch ) ) THEN 
  DEALLOCATE(grid%ru_tendf_stoch,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11613,&
'frame/module_domain.f: Failed to deallocate grid%ru_tendf_stoch. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rv_tendf_stoch ) ) THEN 
  DEALLOCATE(grid%rv_tendf_stoch,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11620,&
'frame/module_domain.f: Failed to deallocate grid%rv_tendf_stoch. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rt_tendf_stoch ) ) THEN 
  DEALLOCATE(grid%rt_tendf_stoch,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11627,&
'frame/module_domain.f: Failed to deallocate grid%rt_tendf_stoch. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_pert ) ) THEN 
  DEALLOCATE(grid%rand_pert,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11634,&
'frame/module_domain.f: Failed to deallocate grid%rand_pert. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pattern_spp_conv ) ) THEN 
  DEALLOCATE(grid%pattern_spp_conv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11641,&
'frame/module_domain.f: Failed to deallocate grid%pattern_spp_conv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pattern_spp_pbl ) ) THEN 
  DEALLOCATE(grid%pattern_spp_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11648,&
'frame/module_domain.f: Failed to deallocate grid%pattern_spp_pbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pattern_spp_lsm ) ) THEN 
  DEALLOCATE(grid%pattern_spp_lsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11655,&
'frame/module_domain.f: Failed to deallocate grid%pattern_spp_lsm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rstoch ) ) THEN 
  DEALLOCATE(grid%rstoch,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11662,&
'frame/module_domain.f: Failed to deallocate grid%rstoch. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_real ) ) THEN 
  DEALLOCATE(grid%rand_real,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11669,&
'frame/module_domain.f: Failed to deallocate grid%rand_real. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_imag ) ) THEN 
  DEALLOCATE(grid%rand_imag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11676,&
'frame/module_domain.f: Failed to deallocate grid%rand_imag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spstreamforcc ) ) THEN 
  DEALLOCATE(grid%spstreamforcc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11683,&
'frame/module_domain.f: Failed to deallocate grid%spstreamforcc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spstreamforcs ) ) THEN 
  DEALLOCATE(grid%spstreamforcs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11690,&
'frame/module_domain.f: Failed to deallocate grid%spstreamforcs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spstream_amp ) ) THEN 
  DEALLOCATE(grid%spstream_amp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11697,&
'frame/module_domain.f: Failed to deallocate grid%spstream_amp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sptforcc ) ) THEN 
  DEALLOCATE(grid%sptforcc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11704,&
'frame/module_domain.f: Failed to deallocate grid%sptforcc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sptforcs ) ) THEN 
  DEALLOCATE(grid%sptforcs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11711,&
'frame/module_domain.f: Failed to deallocate grid%sptforcs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spt_amp ) ) THEN 
  DEALLOCATE(grid%spt_amp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11718,&
'frame/module_domain.f: Failed to deallocate grid%spt_amp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcc ) ) THEN 
  DEALLOCATE(grid%spforcc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11725,&
'frame/module_domain.f: Failed to deallocate grid%spforcc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcs ) ) THEN 
  DEALLOCATE(grid%spforcs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11732,&
'frame/module_domain.f: Failed to deallocate grid%spforcs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sp_amp ) ) THEN 
  DEALLOCATE(grid%sp_amp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11739,&
'frame/module_domain.f: Failed to deallocate grid%sp_amp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcc2 ) ) THEN 
  DEALLOCATE(grid%spforcc2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11746,&
'frame/module_domain.f: Failed to deallocate grid%spforcc2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcs2 ) ) THEN 
  DEALLOCATE(grid%spforcs2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11753,&
'frame/module_domain.f: Failed to deallocate grid%spforcs2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sp_amp2 ) ) THEN 
  DEALLOCATE(grid%sp_amp2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11760,&
'frame/module_domain.f: Failed to deallocate grid%sp_amp2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcc3 ) ) THEN 
  DEALLOCATE(grid%spforcc3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11767,&
'frame/module_domain.f: Failed to deallocate grid%spforcc3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcs3 ) ) THEN 
  DEALLOCATE(grid%spforcs3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11774,&
'frame/module_domain.f: Failed to deallocate grid%spforcs3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sp_amp3 ) ) THEN 
  DEALLOCATE(grid%sp_amp3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11781,&
'frame/module_domain.f: Failed to deallocate grid%sp_amp3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcc4 ) ) THEN 
  DEALLOCATE(grid%spforcc4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11788,&
'frame/module_domain.f: Failed to deallocate grid%spforcc4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcs4 ) ) THEN 
  DEALLOCATE(grid%spforcs4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11795,&
'frame/module_domain.f: Failed to deallocate grid%spforcs4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sp_amp4 ) ) THEN 
  DEALLOCATE(grid%sp_amp4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11802,&
'frame/module_domain.f: Failed to deallocate grid%sp_amp4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcc5 ) ) THEN 
  DEALLOCATE(grid%spforcc5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11809,&
'frame/module_domain.f: Failed to deallocate grid%spforcc5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spforcs5 ) ) THEN 
  DEALLOCATE(grid%spforcs5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11816,&
'frame/module_domain.f: Failed to deallocate grid%spforcs5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sp_amp5 ) ) THEN 
  DEALLOCATE(grid%sp_amp5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11823,&
'frame/module_domain.f: Failed to deallocate grid%sp_amp5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spptforcc ) ) THEN 
  DEALLOCATE(grid%spptforcc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11830,&
'frame/module_domain.f: Failed to deallocate grid%spptforcc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%spptforcs ) ) THEN 
  DEALLOCATE(grid%spptforcs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11837,&
'frame/module_domain.f: Failed to deallocate grid%spptforcs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sppt_amp ) ) THEN 
  DEALLOCATE(grid%sppt_amp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11844,&
'frame/module_domain.f: Failed to deallocate grid%sppt_amp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vertampt ) ) THEN 
  DEALLOCATE(grid%vertampt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11851,&
'frame/module_domain.f: Failed to deallocate grid%vertampt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vertampuv ) ) THEN 
  DEALLOCATE(grid%vertampuv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11858,&
'frame/module_domain.f: Failed to deallocate grid%vertampuv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_sppt ) ) THEN 
  DEALLOCATE(grid%iseedarr_sppt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11865,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_sppt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_skebs ) ) THEN 
  DEALLOCATE(grid%iseedarr_skebs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11872,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_skebs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_rand_pert ) ) THEN 
  DEALLOCATE(grid%iseedarr_rand_pert,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11879,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_rand_pert. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_spp_conv ) ) THEN 
  DEALLOCATE(grid%iseedarr_spp_conv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11886,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_spp_conv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_spp_pbl ) ) THEN 
  DEALLOCATE(grid%iseedarr_spp_pbl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11893,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_spp_pbl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iseedarr_spp_lsm ) ) THEN 
  DEALLOCATE(grid%iseedarr_spp_lsm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11900,&
'frame/module_domain.f: Failed to deallocate grid%iseedarr_spp_lsm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_real_xxx ) ) THEN 
  DEALLOCATE(grid%rand_real_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11907,&
'frame/module_domain.f: Failed to deallocate grid%rand_real_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_real_yyy ) ) THEN 
  DEALLOCATE(grid%rand_real_yyy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11914,&
'frame/module_domain.f: Failed to deallocate grid%rand_real_yyy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_imag_xxx ) ) THEN 
  DEALLOCATE(grid%rand_imag_xxx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11921,&
'frame/module_domain.f: Failed to deallocate grid%rand_imag_xxx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rand_imag_yyy ) ) THEN 
  DEALLOCATE(grid%rand_imag_yyy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11928,&
'frame/module_domain.f: Failed to deallocate grid%rand_imag_yyy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nba_mij ) ) THEN 
  DEALLOCATE(grid%nba_mij,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11935,&
'frame/module_domain.f: Failed to deallocate grid%nba_mij. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nba_rij ) ) THEN 
  DEALLOCATE(grid%nba_rij,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11942,&
'frame/module_domain.f: Failed to deallocate grid%nba_rij. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tauresx2d ) ) THEN 
  DEALLOCATE(grid%tauresx2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11949,&
'frame/module_domain.f: Failed to deallocate grid%tauresx2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tauresy2d ) ) THEN 
  DEALLOCATE(grid%tauresy2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11956,&
'frame/module_domain.f: Failed to deallocate grid%tauresy2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tpert2d ) ) THEN 
  DEALLOCATE(grid%tpert2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11963,&
'frame/module_domain.f: Failed to deallocate grid%tpert2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qpert2d ) ) THEN 
  DEALLOCATE(grid%qpert2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11970,&
'frame/module_domain.f: Failed to deallocate grid%qpert2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wpert2d ) ) THEN 
  DEALLOCATE(grid%wpert2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11977,&
'frame/module_domain.f: Failed to deallocate grid%wpert2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%turbtype3d ) ) THEN 
  DEALLOCATE(grid%turbtype3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11984,&
'frame/module_domain.f: Failed to deallocate grid%turbtype3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smaw3d ) ) THEN 
  DEALLOCATE(grid%smaw3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11991,&
'frame/module_domain.f: Failed to deallocate grid%smaw3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wsedl3d ) ) THEN 
  DEALLOCATE(grid%wsedl3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",11998,&
'frame/module_domain.f: Failed to deallocate grid%wsedl3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rliq ) ) THEN 
  DEALLOCATE(grid%rliq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12005,&
'frame/module_domain.f: Failed to deallocate grid%rliq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dlf ) ) THEN 
  DEALLOCATE(grid%dlf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12012,&
'frame/module_domain.f: Failed to deallocate grid%dlf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%precz ) ) THEN 
  DEALLOCATE(grid%precz,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12019,&
'frame/module_domain.f: Failed to deallocate grid%precz. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmdt ) ) THEN 
  DEALLOCATE(grid%zmdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12026,&
'frame/module_domain.f: Failed to deallocate grid%zmdt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmdq ) ) THEN 
  DEALLOCATE(grid%zmdq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12033,&
'frame/module_domain.f: Failed to deallocate grid%zmdq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmdice ) ) THEN 
  DEALLOCATE(grid%zmdice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12040,&
'frame/module_domain.f: Failed to deallocate grid%zmdice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmdliq ) ) THEN 
  DEALLOCATE(grid%zmdliq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12047,&
'frame/module_domain.f: Failed to deallocate grid%zmdliq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evaptzm ) ) THEN 
  DEALLOCATE(grid%evaptzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12054,&
'frame/module_domain.f: Failed to deallocate grid%evaptzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fzsntzm ) ) THEN 
  DEALLOCATE(grid%fzsntzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12061,&
'frame/module_domain.f: Failed to deallocate grid%fzsntzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evsntzm ) ) THEN 
  DEALLOCATE(grid%evsntzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12068,&
'frame/module_domain.f: Failed to deallocate grid%evsntzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evapqzm ) ) THEN 
  DEALLOCATE(grid%evapqzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12075,&
'frame/module_domain.f: Failed to deallocate grid%evapqzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmflxprc ) ) THEN 
  DEALLOCATE(grid%zmflxprc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12082,&
'frame/module_domain.f: Failed to deallocate grid%zmflxprc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmflxsnw ) ) THEN 
  DEALLOCATE(grid%zmflxsnw,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12089,&
'frame/module_domain.f: Failed to deallocate grid%zmflxsnw. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmntprpd ) ) THEN 
  DEALLOCATE(grid%zmntprpd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12096,&
'frame/module_domain.f: Failed to deallocate grid%zmntprpd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmntsnpd ) ) THEN 
  DEALLOCATE(grid%zmntsnpd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12103,&
'frame/module_domain.f: Failed to deallocate grid%zmntsnpd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmeiheat ) ) THEN 
  DEALLOCATE(grid%zmeiheat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12110,&
'frame/module_domain.f: Failed to deallocate grid%zmeiheat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmfmcdzm ) ) THEN 
  DEALLOCATE(grid%cmfmcdzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12117,&
'frame/module_domain.f: Failed to deallocate grid%cmfmcdzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%preccdzm ) ) THEN 
  DEALLOCATE(grid%preccdzm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12124,&
'frame/module_domain.f: Failed to deallocate grid%preccdzm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pconvb ) ) THEN 
  DEALLOCATE(grid%pconvb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12131,&
'frame/module_domain.f: Failed to deallocate grid%pconvb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pconvt ) ) THEN 
  DEALLOCATE(grid%pconvt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12138,&
'frame/module_domain.f: Failed to deallocate grid%pconvt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cape ) ) THEN 
  DEALLOCATE(grid%cape,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12145,&
'frame/module_domain.f: Failed to deallocate grid%cape. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmmtu ) ) THEN 
  DEALLOCATE(grid%zmmtu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12152,&
'frame/module_domain.f: Failed to deallocate grid%zmmtu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmmtv ) ) THEN 
  DEALLOCATE(grid%zmmtv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12159,&
'frame/module_domain.f: Failed to deallocate grid%zmmtv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmmu ) ) THEN 
  DEALLOCATE(grid%zmmu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12166,&
'frame/module_domain.f: Failed to deallocate grid%zmmu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmmd ) ) THEN 
  DEALLOCATE(grid%zmmd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12173,&
'frame/module_domain.f: Failed to deallocate grid%zmmd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmupgu ) ) THEN 
  DEALLOCATE(grid%zmupgu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12180,&
'frame/module_domain.f: Failed to deallocate grid%zmupgu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmupgd ) ) THEN 
  DEALLOCATE(grid%zmupgd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12187,&
'frame/module_domain.f: Failed to deallocate grid%zmupgd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmvpgu ) ) THEN 
  DEALLOCATE(grid%zmvpgu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12194,&
'frame/module_domain.f: Failed to deallocate grid%zmvpgu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmvpgd ) ) THEN 
  DEALLOCATE(grid%zmvpgd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12201,&
'frame/module_domain.f: Failed to deallocate grid%zmvpgd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmicuu ) ) THEN 
  DEALLOCATE(grid%zmicuu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12208,&
'frame/module_domain.f: Failed to deallocate grid%zmicuu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmicud ) ) THEN 
  DEALLOCATE(grid%zmicud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12215,&
'frame/module_domain.f: Failed to deallocate grid%zmicud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmicvu ) ) THEN 
  DEALLOCATE(grid%zmicvu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12222,&
'frame/module_domain.f: Failed to deallocate grid%zmicvu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zmicvd ) ) THEN 
  DEALLOCATE(grid%zmicvd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12229,&
'frame/module_domain.f: Failed to deallocate grid%zmicvd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evapcdp3d ) ) THEN 
  DEALLOCATE(grid%evapcdp3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12236,&
'frame/module_domain.f: Failed to deallocate grid%evapcdp3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icwmrdp3d ) ) THEN 
  DEALLOCATE(grid%icwmrdp3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12243,&
'frame/module_domain.f: Failed to deallocate grid%icwmrdp3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rprddp3d ) ) THEN 
  DEALLOCATE(grid%rprddp3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12250,&
'frame/module_domain.f: Failed to deallocate grid%rprddp3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dp3d ) ) THEN 
  DEALLOCATE(grid%dp3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12257,&
'frame/module_domain.f: Failed to deallocate grid%dp3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%du3d ) ) THEN 
  DEALLOCATE(grid%du3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12264,&
'frame/module_domain.f: Failed to deallocate grid%du3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ed3d ) ) THEN 
  DEALLOCATE(grid%ed3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12271,&
'frame/module_domain.f: Failed to deallocate grid%ed3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%eu3d ) ) THEN 
  DEALLOCATE(grid%eu3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12278,&
'frame/module_domain.f: Failed to deallocate grid%eu3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%md3d ) ) THEN 
  DEALLOCATE(grid%md3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12285,&
'frame/module_domain.f: Failed to deallocate grid%md3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%mu3d ) ) THEN 
  DEALLOCATE(grid%mu3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12292,&
'frame/module_domain.f: Failed to deallocate grid%mu3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dsubcld2d ) ) THEN 
  DEALLOCATE(grid%dsubcld2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12299,&
'frame/module_domain.f: Failed to deallocate grid%dsubcld2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ideep2d ) ) THEN 
  DEALLOCATE(grid%ideep2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12306,&
'frame/module_domain.f: Failed to deallocate grid%ideep2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%jt2d ) ) THEN 
  DEALLOCATE(grid%jt2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12313,&
'frame/module_domain.f: Failed to deallocate grid%jt2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%maxg2d ) ) THEN 
  DEALLOCATE(grid%maxg2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12320,&
'frame/module_domain.f: Failed to deallocate grid%maxg2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lengath2d ) ) THEN 
  DEALLOCATE(grid%lengath2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12327,&
'frame/module_domain.f: Failed to deallocate grid%lengath2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmfsl ) ) THEN 
  DEALLOCATE(grid%cmfsl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12334,&
'frame/module_domain.f: Failed to deallocate grid%cmfsl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmflq ) ) THEN 
  DEALLOCATE(grid%cmflq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12341,&
'frame/module_domain.f: Failed to deallocate grid%cmflq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmfmc ) ) THEN 
  DEALLOCATE(grid%cmfmc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12348,&
'frame/module_domain.f: Failed to deallocate grid%cmfmc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmfmc2 ) ) THEN 
  DEALLOCATE(grid%cmfmc2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12355,&
'frame/module_domain.f: Failed to deallocate grid%cmfmc2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfrash ) ) THEN 
  DEALLOCATE(grid%cldfrash,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12362,&
'frame/module_domain.f: Failed to deallocate grid%cldfrash. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cush ) ) THEN 
  DEALLOCATE(grid%cush,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12369,&
'frame/module_domain.f: Failed to deallocate grid%cush. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evapcsh ) ) THEN 
  DEALLOCATE(grid%evapcsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12376,&
'frame/module_domain.f: Failed to deallocate grid%evapcsh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icwmrsh ) ) THEN 
  DEALLOCATE(grid%icwmrsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12383,&
'frame/module_domain.f: Failed to deallocate grid%icwmrsh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowsh ) ) THEN 
  DEALLOCATE(grid%snowsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12390,&
'frame/module_domain.f: Failed to deallocate grid%snowsh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rprdsh ) ) THEN 
  DEALLOCATE(grid%rprdsh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12397,&
'frame/module_domain.f: Failed to deallocate grid%rprdsh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rliq2 ) ) THEN 
  DEALLOCATE(grid%rliq2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12404,&
'frame/module_domain.f: Failed to deallocate grid%rliq2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dlf2 ) ) THEN 
  DEALLOCATE(grid%dlf2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12411,&
'frame/module_domain.f: Failed to deallocate grid%dlf2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shfrc3d ) ) THEN 
  DEALLOCATE(grid%shfrc3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12418,&
'frame/module_domain.f: Failed to deallocate grid%shfrc3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtflx_cu ) ) THEN 
  DEALLOCATE(grid%qtflx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12425,&
'frame/module_domain.f: Failed to deallocate grid%qtflx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slflx_cu ) ) THEN 
  DEALLOCATE(grid%slflx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12432,&
'frame/module_domain.f: Failed to deallocate grid%slflx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uflx_cu ) ) THEN 
  DEALLOCATE(grid%uflx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12439,&
'frame/module_domain.f: Failed to deallocate grid%uflx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vflx_cu ) ) THEN 
  DEALLOCATE(grid%vflx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12446,&
'frame/module_domain.f: Failed to deallocate grid%vflx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtten_cu ) ) THEN 
  DEALLOCATE(grid%qtten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12453,&
'frame/module_domain.f: Failed to deallocate grid%qtten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%slten_cu ) ) THEN 
  DEALLOCATE(grid%slten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12460,&
'frame/module_domain.f: Failed to deallocate grid%slten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uten_cu ) ) THEN 
  DEALLOCATE(grid%uten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12467,&
'frame/module_domain.f: Failed to deallocate grid%uten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vten_cu ) ) THEN 
  DEALLOCATE(grid%vten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12474,&
'frame/module_domain.f: Failed to deallocate grid%vten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qvten_cu ) ) THEN 
  DEALLOCATE(grid%qvten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12481,&
'frame/module_domain.f: Failed to deallocate grid%qvten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qlten_cu ) ) THEN 
  DEALLOCATE(grid%qlten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12488,&
'frame/module_domain.f: Failed to deallocate grid%qlten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qiten_cu ) ) THEN 
  DEALLOCATE(grid%qiten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12495,&
'frame/module_domain.f: Failed to deallocate grid%qiten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cbmf_cu ) ) THEN 
  DEALLOCATE(grid%cbmf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12502,&
'frame/module_domain.f: Failed to deallocate grid%cbmf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ufrcinvbase_cu ) ) THEN 
  DEALLOCATE(grid%ufrcinvbase_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12509,&
'frame/module_domain.f: Failed to deallocate grid%ufrcinvbase_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ufrclcl_cu ) ) THEN 
  DEALLOCATE(grid%ufrclcl_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12516,&
'frame/module_domain.f: Failed to deallocate grid%ufrclcl_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%winvbase_cu ) ) THEN 
  DEALLOCATE(grid%winvbase_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12523,&
'frame/module_domain.f: Failed to deallocate grid%winvbase_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wlcl_cu ) ) THEN 
  DEALLOCATE(grid%wlcl_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12530,&
'frame/module_domain.f: Failed to deallocate grid%wlcl_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%plcl_cu ) ) THEN 
  DEALLOCATE(grid%plcl_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12537,&
'frame/module_domain.f: Failed to deallocate grid%plcl_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pinv_cu ) ) THEN 
  DEALLOCATE(grid%pinv_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12544,&
'frame/module_domain.f: Failed to deallocate grid%pinv_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%plfc_cu ) ) THEN 
  DEALLOCATE(grid%plfc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12551,&
'frame/module_domain.f: Failed to deallocate grid%plfc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pbup_cu ) ) THEN 
  DEALLOCATE(grid%pbup_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12558,&
'frame/module_domain.f: Failed to deallocate grid%pbup_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ppen_cu ) ) THEN 
  DEALLOCATE(grid%ppen_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12565,&
'frame/module_domain.f: Failed to deallocate grid%ppen_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtsrc_cu ) ) THEN 
  DEALLOCATE(grid%qtsrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12572,&
'frame/module_domain.f: Failed to deallocate grid%qtsrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thlsrc_cu ) ) THEN 
  DEALLOCATE(grid%thlsrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12579,&
'frame/module_domain.f: Failed to deallocate grid%thlsrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thvlsrc_cu ) ) THEN 
  DEALLOCATE(grid%thvlsrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12586,&
'frame/module_domain.f: Failed to deallocate grid%thvlsrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%emkfbup_cu ) ) THEN 
  DEALLOCATE(grid%emkfbup_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12593,&
'frame/module_domain.f: Failed to deallocate grid%emkfbup_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cin_cu ) ) THEN 
  DEALLOCATE(grid%cin_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12600,&
'frame/module_domain.f: Failed to deallocate grid%cin_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cinlcl_cu ) ) THEN 
  DEALLOCATE(grid%cinlcl_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12607,&
'frame/module_domain.f: Failed to deallocate grid%cinlcl_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cbmflimit_cu ) ) THEN 
  DEALLOCATE(grid%cbmflimit_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12614,&
'frame/module_domain.f: Failed to deallocate grid%cbmflimit_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tkeavg_cu ) ) THEN 
  DEALLOCATE(grid%tkeavg_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12621,&
'frame/module_domain.f: Failed to deallocate grid%tkeavg_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zinv_cu ) ) THEN 
  DEALLOCATE(grid%zinv_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12628,&
'frame/module_domain.f: Failed to deallocate grid%zinv_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rcwp_cu ) ) THEN 
  DEALLOCATE(grid%rcwp_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12635,&
'frame/module_domain.f: Failed to deallocate grid%rcwp_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rlwp_cu ) ) THEN 
  DEALLOCATE(grid%rlwp_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12642,&
'frame/module_domain.f: Failed to deallocate grid%rlwp_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%riwp_cu ) ) THEN 
  DEALLOCATE(grid%riwp_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12649,&
'frame/module_domain.f: Failed to deallocate grid%riwp_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tophgt_cu ) ) THEN 
  DEALLOCATE(grid%tophgt_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12656,&
'frame/module_domain.f: Failed to deallocate grid%tophgt_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wu_cu ) ) THEN 
  DEALLOCATE(grid%wu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12663,&
'frame/module_domain.f: Failed to deallocate grid%wu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ufrc_cu ) ) THEN 
  DEALLOCATE(grid%ufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12670,&
'frame/module_domain.f: Failed to deallocate grid%ufrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtu_cu ) ) THEN 
  DEALLOCATE(grid%qtu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12677,&
'frame/module_domain.f: Failed to deallocate grid%qtu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thlu_cu ) ) THEN 
  DEALLOCATE(grid%thlu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12684,&
'frame/module_domain.f: Failed to deallocate grid%thlu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thvu_cu ) ) THEN 
  DEALLOCATE(grid%thvu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12691,&
'frame/module_domain.f: Failed to deallocate grid%thvu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uu_cu ) ) THEN 
  DEALLOCATE(grid%uu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12698,&
'frame/module_domain.f: Failed to deallocate grid%uu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vu_cu ) ) THEN 
  DEALLOCATE(grid%vu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12705,&
'frame/module_domain.f: Failed to deallocate grid%vu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qtu_emf_cu ) ) THEN 
  DEALLOCATE(grid%qtu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12712,&
'frame/module_domain.f: Failed to deallocate grid%qtu_emf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%thlu_emf_cu ) ) THEN 
  DEALLOCATE(grid%thlu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12719,&
'frame/module_domain.f: Failed to deallocate grid%thlu_emf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uu_emf_cu ) ) THEN 
  DEALLOCATE(grid%uu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12726,&
'frame/module_domain.f: Failed to deallocate grid%uu_emf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vu_emf_cu ) ) THEN 
  DEALLOCATE(grid%vu_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12733,&
'frame/module_domain.f: Failed to deallocate grid%vu_emf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%umf_cu ) ) THEN 
  DEALLOCATE(grid%umf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12740,&
'frame/module_domain.f: Failed to deallocate grid%umf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%uemf_cu ) ) THEN 
  DEALLOCATE(grid%uemf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12747,&
'frame/module_domain.f: Failed to deallocate grid%uemf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qcu_cu ) ) THEN 
  DEALLOCATE(grid%qcu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12754,&
'frame/module_domain.f: Failed to deallocate grid%qcu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qlu_cu ) ) THEN 
  DEALLOCATE(grid%qlu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12761,&
'frame/module_domain.f: Failed to deallocate grid%qlu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qiu_cu ) ) THEN 
  DEALLOCATE(grid%qiu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12768,&
'frame/module_domain.f: Failed to deallocate grid%qiu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cufrc_cu ) ) THEN 
  DEALLOCATE(grid%cufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12775,&
'frame/module_domain.f: Failed to deallocate grid%cufrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fer_cu ) ) THEN 
  DEALLOCATE(grid%fer_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12782,&
'frame/module_domain.f: Failed to deallocate grid%fer_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdr_cu ) ) THEN 
  DEALLOCATE(grid%fdr_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12789,&
'frame/module_domain.f: Failed to deallocate grid%fdr_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dwten_cu ) ) THEN 
  DEALLOCATE(grid%dwten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12796,&
'frame/module_domain.f: Failed to deallocate grid%dwten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%diten_cu ) ) THEN 
  DEALLOCATE(grid%diten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12803,&
'frame/module_domain.f: Failed to deallocate grid%diten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qrten_cu ) ) THEN 
  DEALLOCATE(grid%qrten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12810,&
'frame/module_domain.f: Failed to deallocate grid%qrten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsten_cu ) ) THEN 
  DEALLOCATE(grid%qsten_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12817,&
'frame/module_domain.f: Failed to deallocate grid%qsten_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flxrain_cu ) ) THEN 
  DEALLOCATE(grid%flxrain_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12824,&
'frame/module_domain.f: Failed to deallocate grid%flxrain_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flxsnow_cu ) ) THEN 
  DEALLOCATE(grid%flxsnow_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12831,&
'frame/module_domain.f: Failed to deallocate grid%flxsnow_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ntraprd_cu ) ) THEN 
  DEALLOCATE(grid%ntraprd_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12838,&
'frame/module_domain.f: Failed to deallocate grid%ntraprd_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ntsnprd_cu ) ) THEN 
  DEALLOCATE(grid%ntsnprd_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12845,&
'frame/module_domain.f: Failed to deallocate grid%ntsnprd_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%excessu_cu ) ) THEN 
  DEALLOCATE(grid%excessu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12852,&
'frame/module_domain.f: Failed to deallocate grid%excessu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%excessu0_cu ) ) THEN 
  DEALLOCATE(grid%excessu0_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12859,&
'frame/module_domain.f: Failed to deallocate grid%excessu0_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xc_cu ) ) THEN 
  DEALLOCATE(grid%xc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12866,&
'frame/module_domain.f: Failed to deallocate grid%xc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aquad_cu ) ) THEN 
  DEALLOCATE(grid%aquad_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12873,&
'frame/module_domain.f: Failed to deallocate grid%aquad_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bquad_cu ) ) THEN 
  DEALLOCATE(grid%bquad_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12880,&
'frame/module_domain.f: Failed to deallocate grid%bquad_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cquad_cu ) ) THEN 
  DEALLOCATE(grid%cquad_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12887,&
'frame/module_domain.f: Failed to deallocate grid%cquad_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bogbot_cu ) ) THEN 
  DEALLOCATE(grid%bogbot_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12894,&
'frame/module_domain.f: Failed to deallocate grid%bogbot_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bogtop_cu ) ) THEN 
  DEALLOCATE(grid%bogtop_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12901,&
'frame/module_domain.f: Failed to deallocate grid%bogtop_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_uwcu_cu ) ) THEN 
  DEALLOCATE(grid%exit_uwcu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12908,&
'frame/module_domain.f: Failed to deallocate grid%exit_uwcu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_conden_cu ) ) THEN 
  DEALLOCATE(grid%exit_conden_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12915,&
'frame/module_domain.f: Failed to deallocate grid%exit_conden_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_klclmkx_cu ) ) THEN 
  DEALLOCATE(grid%exit_klclmkx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12922,&
'frame/module_domain.f: Failed to deallocate grid%exit_klclmkx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_klfcmkx_cu ) ) THEN 
  DEALLOCATE(grid%exit_klfcmkx_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12929,&
'frame/module_domain.f: Failed to deallocate grid%exit_klfcmkx_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_ufrc_cu ) ) THEN 
  DEALLOCATE(grid%exit_ufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12936,&
'frame/module_domain.f: Failed to deallocate grid%exit_ufrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_wtw_cu ) ) THEN 
  DEALLOCATE(grid%exit_wtw_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12943,&
'frame/module_domain.f: Failed to deallocate grid%exit_wtw_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_drycore_cu ) ) THEN 
  DEALLOCATE(grid%exit_drycore_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12950,&
'frame/module_domain.f: Failed to deallocate grid%exit_drycore_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_wu_cu ) ) THEN 
  DEALLOCATE(grid%exit_wu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12957,&
'frame/module_domain.f: Failed to deallocate grid%exit_wu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_cufliter_cu ) ) THEN 
  DEALLOCATE(grid%exit_cufliter_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12964,&
'frame/module_domain.f: Failed to deallocate grid%exit_cufliter_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_kinv1_cu ) ) THEN 
  DEALLOCATE(grid%exit_kinv1_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12971,&
'frame/module_domain.f: Failed to deallocate grid%exit_kinv1_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%exit_rei_cu ) ) THEN 
  DEALLOCATE(grid%exit_rei_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12978,&
'frame/module_domain.f: Failed to deallocate grid%exit_rei_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_shcu_cu ) ) THEN 
  DEALLOCATE(grid%limit_shcu_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12985,&
'frame/module_domain.f: Failed to deallocate grid%limit_shcu_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_negcon_cu ) ) THEN 
  DEALLOCATE(grid%limit_negcon_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12992,&
'frame/module_domain.f: Failed to deallocate grid%limit_negcon_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_ufrc_cu ) ) THEN 
  DEALLOCATE(grid%limit_ufrc_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",12999,&
'frame/module_domain.f: Failed to deallocate grid%limit_ufrc_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_ppen_cu ) ) THEN 
  DEALLOCATE(grid%limit_ppen_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13006,&
'frame/module_domain.f: Failed to deallocate grid%limit_ppen_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_emf_cu ) ) THEN 
  DEALLOCATE(grid%limit_emf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13013,&
'frame/module_domain.f: Failed to deallocate grid%limit_emf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_cinlcl_cu ) ) THEN 
  DEALLOCATE(grid%limit_cinlcl_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13020,&
'frame/module_domain.f: Failed to deallocate grid%limit_cinlcl_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_cin_cu ) ) THEN 
  DEALLOCATE(grid%limit_cin_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13027,&
'frame/module_domain.f: Failed to deallocate grid%limit_cin_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_cbmf_cu ) ) THEN 
  DEALLOCATE(grid%limit_cbmf_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13034,&
'frame/module_domain.f: Failed to deallocate grid%limit_cbmf_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%limit_rei_cu ) ) THEN 
  DEALLOCATE(grid%limit_rei_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13041,&
'frame/module_domain.f: Failed to deallocate grid%limit_rei_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ind_delcin_cu ) ) THEN 
  DEALLOCATE(grid%ind_delcin_cu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13048,&
'frame/module_domain.f: Failed to deallocate grid%ind_delcin_cu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_old_mp ) ) THEN 
  DEALLOCATE(grid%rh_old_mp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13055,&
'frame/module_domain.f: Failed to deallocate grid%rh_old_mp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lcd_old_mp ) ) THEN 
  DEALLOCATE(grid%lcd_old_mp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13062,&
'frame/module_domain.f: Failed to deallocate grid%lcd_old_mp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_old_mp ) ) THEN 
  DEALLOCATE(grid%cldfra_old_mp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13069,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_old_mp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_mp ) ) THEN 
  DEALLOCATE(grid%cldfra_mp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13076,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_mp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_mp_all ) ) THEN 
  DEALLOCATE(grid%cldfra_mp_all,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13083,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_mp_all. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%iradius ) ) THEN 
  DEALLOCATE(grid%iradius,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13090,&
'frame/module_domain.f: Failed to deallocate grid%iradius. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lradius ) ) THEN 
  DEALLOCATE(grid%lradius,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13097,&
'frame/module_domain.f: Failed to deallocate grid%lradius. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfra_conv ) ) THEN 
  DEALLOCATE(grid%cldfra_conv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13104,&
'frame/module_domain.f: Failed to deallocate grid%cldfra_conv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfrai ) ) THEN 
  DEALLOCATE(grid%cldfrai,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13111,&
'frame/module_domain.f: Failed to deallocate grid%cldfrai. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cldfral ) ) THEN 
  DEALLOCATE(grid%cldfral,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13118,&
'frame/module_domain.f: Failed to deallocate grid%cldfral. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%numc ) ) THEN 
  DEALLOCATE(grid%numc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13125,&
'frame/module_domain.f: Failed to deallocate grid%numc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nump ) ) THEN 
  DEALLOCATE(grid%nump,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13132,&
'frame/module_domain.f: Failed to deallocate grid%nump. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sabv ) ) THEN 
  DEALLOCATE(grid%sabv,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13139,&
'frame/module_domain.f: Failed to deallocate grid%sabv. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sabg ) ) THEN 
  DEALLOCATE(grid%sabg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13146,&
'frame/module_domain.f: Failed to deallocate grid%sabg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwup ) ) THEN 
  DEALLOCATE(grid%lwup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13153,&
'frame/module_domain.f: Failed to deallocate grid%lwup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lhsoi ) ) THEN 
  DEALLOCATE(grid%lhsoi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13160,&
'frame/module_domain.f: Failed to deallocate grid%lhsoi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lhveg ) ) THEN 
  DEALLOCATE(grid%lhveg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13167,&
'frame/module_domain.f: Failed to deallocate grid%lhveg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lhtran ) ) THEN 
  DEALLOCATE(grid%lhtran,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13174,&
'frame/module_domain.f: Failed to deallocate grid%lhtran. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snl ) ) THEN 
  DEALLOCATE(grid%snl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13181,&
'frame/module_domain.f: Failed to deallocate grid%snl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowdp ) ) THEN 
  DEALLOCATE(grid%snowdp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13188,&
'frame/module_domain.f: Failed to deallocate grid%snowdp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wtc ) ) THEN 
  DEALLOCATE(grid%wtc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13195,&
'frame/module_domain.f: Failed to deallocate grid%wtc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wtp ) ) THEN 
  DEALLOCATE(grid%wtp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13202,&
'frame/module_domain.f: Failed to deallocate grid%wtp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osno ) ) THEN 
  DEALLOCATE(grid%h2osno,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13209,&
'frame/module_domain.f: Failed to deallocate grid%h2osno. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_grnd ) ) THEN 
  DEALLOCATE(grid%t_grnd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13216,&
'frame/module_domain.f: Failed to deallocate grid%t_grnd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_veg ) ) THEN 
  DEALLOCATE(grid%t_veg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13223,&
'frame/module_domain.f: Failed to deallocate grid%t_veg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2ocan ) ) THEN 
  DEALLOCATE(grid%h2ocan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13230,&
'frame/module_domain.f: Failed to deallocate grid%h2ocan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2ocan_col ) ) THEN 
  DEALLOCATE(grid%h2ocan_col,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13237,&
'frame/module_domain.f: Failed to deallocate grid%h2ocan_col. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2m_max ) ) THEN 
  DEALLOCATE(grid%t2m_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13244,&
'frame/module_domain.f: Failed to deallocate grid%t2m_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2m_min ) ) THEN 
  DEALLOCATE(grid%t2m_min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13251,&
'frame/module_domain.f: Failed to deallocate grid%t2m_min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2clm ) ) THEN 
  DEALLOCATE(grid%t2clm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13258,&
'frame/module_domain.f: Failed to deallocate grid%t2clm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_ref2m ) ) THEN 
  DEALLOCATE(grid%t_ref2m,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13265,&
'frame/module_domain.f: Failed to deallocate grid%t_ref2m. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13272,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13279,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13286,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13293,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq_s5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq_s5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13300,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq_s5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13307,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13314,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13321,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13328,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13335,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq6 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq6,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13342,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq6. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq7 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq7,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13349,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq7. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq8 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq8,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13356,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq8. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq9 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq9,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13363,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq9. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq10 ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13370,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13377,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13384,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13391,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13398,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice_s5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice_s5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13405,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice_s5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13412,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13419,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13426,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13433,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13440,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice6 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice6,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13447,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice6. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice7 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice7,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13454,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice7. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice8 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice8,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13461,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice8. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice9 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice9,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13468,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice9. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice10 ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13475,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s1 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13482,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s2 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13489,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s3 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13496,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s4 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13503,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno_s5 ) ) THEN 
  DEALLOCATE(grid%t_soisno_s5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13510,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno_s5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno1 ) ) THEN 
  DEALLOCATE(grid%t_soisno1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13517,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno2 ) ) THEN 
  DEALLOCATE(grid%t_soisno2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13524,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno3 ) ) THEN 
  DEALLOCATE(grid%t_soisno3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13531,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno4 ) ) THEN 
  DEALLOCATE(grid%t_soisno4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13538,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno5 ) ) THEN 
  DEALLOCATE(grid%t_soisno5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13545,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno6 ) ) THEN 
  DEALLOCATE(grid%t_soisno6,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13552,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno6. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno7 ) ) THEN 
  DEALLOCATE(grid%t_soisno7,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13559,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno7. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno8 ) ) THEN 
  DEALLOCATE(grid%t_soisno8,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13566,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno8. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno9 ) ) THEN 
  DEALLOCATE(grid%t_soisno9,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13573,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno9. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno10 ) ) THEN 
  DEALLOCATE(grid%t_soisno10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13580,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzsnow1 ) ) THEN 
  DEALLOCATE(grid%dzsnow1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13587,&
'frame/module_domain.f: Failed to deallocate grid%dzsnow1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzsnow2 ) ) THEN 
  DEALLOCATE(grid%dzsnow2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13594,&
'frame/module_domain.f: Failed to deallocate grid%dzsnow2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzsnow3 ) ) THEN 
  DEALLOCATE(grid%dzsnow3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13601,&
'frame/module_domain.f: Failed to deallocate grid%dzsnow3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzsnow4 ) ) THEN 
  DEALLOCATE(grid%dzsnow4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13608,&
'frame/module_domain.f: Failed to deallocate grid%dzsnow4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzsnow5 ) ) THEN 
  DEALLOCATE(grid%dzsnow5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13615,&
'frame/module_domain.f: Failed to deallocate grid%dzsnow5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowrds1 ) ) THEN 
  DEALLOCATE(grid%snowrds1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13622,&
'frame/module_domain.f: Failed to deallocate grid%snowrds1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowrds2 ) ) THEN 
  DEALLOCATE(grid%snowrds2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13629,&
'frame/module_domain.f: Failed to deallocate grid%snowrds2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowrds3 ) ) THEN 
  DEALLOCATE(grid%snowrds3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13636,&
'frame/module_domain.f: Failed to deallocate grid%snowrds3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowrds4 ) ) THEN 
  DEALLOCATE(grid%snowrds4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13643,&
'frame/module_domain.f: Failed to deallocate grid%snowrds4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowrds5 ) ) THEN 
  DEALLOCATE(grid%snowrds5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13650,&
'frame/module_domain.f: Failed to deallocate grid%snowrds5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake1 ) ) THEN 
  DEALLOCATE(grid%t_lake1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13657,&
'frame/module_domain.f: Failed to deallocate grid%t_lake1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake2 ) ) THEN 
  DEALLOCATE(grid%t_lake2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13664,&
'frame/module_domain.f: Failed to deallocate grid%t_lake2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake3 ) ) THEN 
  DEALLOCATE(grid%t_lake3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13671,&
'frame/module_domain.f: Failed to deallocate grid%t_lake3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake4 ) ) THEN 
  DEALLOCATE(grid%t_lake4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13678,&
'frame/module_domain.f: Failed to deallocate grid%t_lake4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake5 ) ) THEN 
  DEALLOCATE(grid%t_lake5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13685,&
'frame/module_domain.f: Failed to deallocate grid%t_lake5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake6 ) ) THEN 
  DEALLOCATE(grid%t_lake6,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13692,&
'frame/module_domain.f: Failed to deallocate grid%t_lake6. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake7 ) ) THEN 
  DEALLOCATE(grid%t_lake7,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13699,&
'frame/module_domain.f: Failed to deallocate grid%t_lake7. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake8 ) ) THEN 
  DEALLOCATE(grid%t_lake8,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13706,&
'frame/module_domain.f: Failed to deallocate grid%t_lake8. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake9 ) ) THEN 
  DEALLOCATE(grid%t_lake9,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13713,&
'frame/module_domain.f: Failed to deallocate grid%t_lake9. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake10 ) ) THEN 
  DEALLOCATE(grid%t_lake10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13720,&
'frame/module_domain.f: Failed to deallocate grid%t_lake10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol1 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13727,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol2 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13734,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol3 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13741,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol4 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13748,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol5 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol5,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13755,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol5. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol6 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol6,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13762,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol6. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol7 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol7,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13769,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol7. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol8 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol8,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13776,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol8. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol9 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol9,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13783,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol9. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol10 ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol10,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13790,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol10. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%albedosubgrid ) ) THEN 
  DEALLOCATE(grid%albedosubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13797,&
'frame/module_domain.f: Failed to deallocate grid%albedosubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lhsubgrid ) ) THEN 
  DEALLOCATE(grid%lhsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13804,&
'frame/module_domain.f: Failed to deallocate grid%lhsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfxsubgrid ) ) THEN 
  DEALLOCATE(grid%hfxsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13811,&
'frame/module_domain.f: Failed to deallocate grid%hfxsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupsubgrid ) ) THEN 
  DEALLOCATE(grid%lwupsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13818,&
'frame/module_domain.f: Failed to deallocate grid%lwupsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2subgrid ) ) THEN 
  DEALLOCATE(grid%q2subgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13825,&
'frame/module_domain.f: Failed to deallocate grid%q2subgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sabvsubgrid ) ) THEN 
  DEALLOCATE(grid%sabvsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13832,&
'frame/module_domain.f: Failed to deallocate grid%sabvsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sabgsubgrid ) ) THEN 
  DEALLOCATE(grid%sabgsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13839,&
'frame/module_domain.f: Failed to deallocate grid%sabgsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nrasubgrid ) ) THEN 
  DEALLOCATE(grid%nrasubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13846,&
'frame/module_domain.f: Failed to deallocate grid%nrasubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupsubgrid ) ) THEN 
  DEALLOCATE(grid%swupsubgrid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13853,&
'frame/module_domain.f: Failed to deallocate grid%swupsubgrid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lake2d ) ) THEN 
  DEALLOCATE(grid%lake2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13860,&
'frame/module_domain.f: Failed to deallocate grid%lake2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lakedepth2d ) ) THEN 
  DEALLOCATE(grid%lakedepth2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13867,&
'frame/module_domain.f: Failed to deallocate grid%lakedepth2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%savedtke12d ) ) THEN 
  DEALLOCATE(grid%savedtke12d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13874,&
'frame/module_domain.f: Failed to deallocate grid%savedtke12d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowdp2d ) ) THEN 
  DEALLOCATE(grid%snowdp2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13881,&
'frame/module_domain.f: Failed to deallocate grid%snowdp2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osno2d ) ) THEN 
  DEALLOCATE(grid%h2osno2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13888,&
'frame/module_domain.f: Failed to deallocate grid%h2osno2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snl2d ) ) THEN 
  DEALLOCATE(grid%snl2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13895,&
'frame/module_domain.f: Failed to deallocate grid%snl2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_grnd2d ) ) THEN 
  DEALLOCATE(grid%t_grnd2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13902,&
'frame/module_domain.f: Failed to deallocate grid%t_grnd2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_lake3d ) ) THEN 
  DEALLOCATE(grid%t_lake3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13909,&
'frame/module_domain.f: Failed to deallocate grid%t_lake3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lake_icefrac3d ) ) THEN 
  DEALLOCATE(grid%lake_icefrac3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13916,&
'frame/module_domain.f: Failed to deallocate grid%lake_icefrac3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_lake3d ) ) THEN 
  DEALLOCATE(grid%z_lake3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13923,&
'frame/module_domain.f: Failed to deallocate grid%z_lake3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dz_lake3d ) ) THEN 
  DEALLOCATE(grid%dz_lake3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13930,&
'frame/module_domain.f: Failed to deallocate grid%dz_lake3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_soisno3d ) ) THEN 
  DEALLOCATE(grid%t_soisno3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13937,&
'frame/module_domain.f: Failed to deallocate grid%t_soisno3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_ice3d ) ) THEN 
  DEALLOCATE(grid%h2osoi_ice3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13944,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_ice3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_liq3d ) ) THEN 
  DEALLOCATE(grid%h2osoi_liq3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13951,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_liq3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%h2osoi_vol3d ) ) THEN 
  DEALLOCATE(grid%h2osoi_vol3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13958,&
'frame/module_domain.f: Failed to deallocate grid%h2osoi_vol3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z3d ) ) THEN 
  DEALLOCATE(grid%z3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13965,&
'frame/module_domain.f: Failed to deallocate grid%z3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dz3d ) ) THEN 
  DEALLOCATE(grid%dz3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13972,&
'frame/module_domain.f: Failed to deallocate grid%dz3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zi3d ) ) THEN 
  DEALLOCATE(grid%zi3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13979,&
'frame/module_domain.f: Failed to deallocate grid%zi3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%watsat3d ) ) THEN 
  DEALLOCATE(grid%watsat3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13986,&
'frame/module_domain.f: Failed to deallocate grid%watsat3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%csol3d ) ) THEN 
  DEALLOCATE(grid%csol3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",13993,&
'frame/module_domain.f: Failed to deallocate grid%csol3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tkmg3d ) ) THEN 
  DEALLOCATE(grid%tkmg3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14000,&
'frame/module_domain.f: Failed to deallocate grid%tkmg3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tkdry3d ) ) THEN 
  DEALLOCATE(grid%tkdry3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14007,&
'frame/module_domain.f: Failed to deallocate grid%tkdry3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tksatu3d ) ) THEN 
  DEALLOCATE(grid%tksatu3d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14014,&
'frame/module_domain.f: Failed to deallocate grid%tksatu3d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_fm ) ) THEN 
  DEALLOCATE(grid%ssib_fm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14021,&
'frame/module_domain.f: Failed to deallocate grid%ssib_fm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_fh ) ) THEN 
  DEALLOCATE(grid%ssib_fh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14028,&
'frame/module_domain.f: Failed to deallocate grid%ssib_fh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_cm ) ) THEN 
  DEALLOCATE(grid%ssib_cm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14035,&
'frame/module_domain.f: Failed to deallocate grid%ssib_cm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssibxdd ) ) THEN 
  DEALLOCATE(grid%ssibxdd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14042,&
'frame/module_domain.f: Failed to deallocate grid%ssibxdd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_br ) ) THEN 
  DEALLOCATE(grid%ssib_br,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14049,&
'frame/module_domain.f: Failed to deallocate grid%ssib_br. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_lhf ) ) THEN 
  DEALLOCATE(grid%ssib_lhf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14056,&
'frame/module_domain.f: Failed to deallocate grid%ssib_lhf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_shf ) ) THEN 
  DEALLOCATE(grid%ssib_shf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14063,&
'frame/module_domain.f: Failed to deallocate grid%ssib_shf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_ghf ) ) THEN 
  DEALLOCATE(grid%ssib_ghf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14070,&
'frame/module_domain.f: Failed to deallocate grid%ssib_ghf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_egs ) ) THEN 
  DEALLOCATE(grid%ssib_egs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14077,&
'frame/module_domain.f: Failed to deallocate grid%ssib_egs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_eci ) ) THEN 
  DEALLOCATE(grid%ssib_eci,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14084,&
'frame/module_domain.f: Failed to deallocate grid%ssib_eci. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_ect ) ) THEN 
  DEALLOCATE(grid%ssib_ect,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14091,&
'frame/module_domain.f: Failed to deallocate grid%ssib_ect. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_egi ) ) THEN 
  DEALLOCATE(grid%ssib_egi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14098,&
'frame/module_domain.f: Failed to deallocate grid%ssib_egi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_egt ) ) THEN 
  DEALLOCATE(grid%ssib_egt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14105,&
'frame/module_domain.f: Failed to deallocate grid%ssib_egt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_sdn ) ) THEN 
  DEALLOCATE(grid%ssib_sdn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14112,&
'frame/module_domain.f: Failed to deallocate grid%ssib_sdn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_sup ) ) THEN 
  DEALLOCATE(grid%ssib_sup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14119,&
'frame/module_domain.f: Failed to deallocate grid%ssib_sup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_ldn ) ) THEN 
  DEALLOCATE(grid%ssib_ldn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14126,&
'frame/module_domain.f: Failed to deallocate grid%ssib_ldn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_lup ) ) THEN 
  DEALLOCATE(grid%ssib_lup,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14133,&
'frame/module_domain.f: Failed to deallocate grid%ssib_lup. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_wat ) ) THEN 
  DEALLOCATE(grid%ssib_wat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14140,&
'frame/module_domain.f: Failed to deallocate grid%ssib_wat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_shc ) ) THEN 
  DEALLOCATE(grid%ssib_shc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14147,&
'frame/module_domain.f: Failed to deallocate grid%ssib_shc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_shg ) ) THEN 
  DEALLOCATE(grid%ssib_shg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14154,&
'frame/module_domain.f: Failed to deallocate grid%ssib_shg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_lai ) ) THEN 
  DEALLOCATE(grid%ssib_lai,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14161,&
'frame/module_domain.f: Failed to deallocate grid%ssib_lai. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_vcf ) ) THEN 
  DEALLOCATE(grid%ssib_vcf,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14168,&
'frame/module_domain.f: Failed to deallocate grid%ssib_vcf. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_z00 ) ) THEN 
  DEALLOCATE(grid%ssib_z00,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14175,&
'frame/module_domain.f: Failed to deallocate grid%ssib_z00. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ssib_veg ) ) THEN 
  DEALLOCATE(grid%ssib_veg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14182,&
'frame/module_domain.f: Failed to deallocate grid%ssib_veg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%isnow ) ) THEN 
  DEALLOCATE(grid%isnow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14189,&
'frame/module_domain.f: Failed to deallocate grid%isnow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swe ) ) THEN 
  DEALLOCATE(grid%swe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14196,&
'frame/module_domain.f: Failed to deallocate grid%swe. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowden ) ) THEN 
  DEALLOCATE(grid%snowden,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14203,&
'frame/module_domain.f: Failed to deallocate grid%snowden. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snowdepth ) ) THEN 
  DEALLOCATE(grid%snowdepth,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14210,&
'frame/module_domain.f: Failed to deallocate grid%snowdepth. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tkair ) ) THEN 
  DEALLOCATE(grid%tkair,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14217,&
'frame/module_domain.f: Failed to deallocate grid%tkair. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzo1 ) ) THEN 
  DEALLOCATE(grid%dzo1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14224,&
'frame/module_domain.f: Failed to deallocate grid%dzo1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wo1 ) ) THEN 
  DEALLOCATE(grid%wo1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14231,&
'frame/module_domain.f: Failed to deallocate grid%wo1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssn1 ) ) THEN 
  DEALLOCATE(grid%tssn1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14238,&
'frame/module_domain.f: Failed to deallocate grid%tssn1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssno1 ) ) THEN 
  DEALLOCATE(grid%tssno1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14245,&
'frame/module_domain.f: Failed to deallocate grid%tssno1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bwo1 ) ) THEN 
  DEALLOCATE(grid%bwo1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14252,&
'frame/module_domain.f: Failed to deallocate grid%bwo1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bto1 ) ) THEN 
  DEALLOCATE(grid%bto1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14259,&
'frame/module_domain.f: Failed to deallocate grid%bto1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cto1 ) ) THEN 
  DEALLOCATE(grid%cto1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14266,&
'frame/module_domain.f: Failed to deallocate grid%cto1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fio1 ) ) THEN 
  DEALLOCATE(grid%fio1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14273,&
'frame/module_domain.f: Failed to deallocate grid%fio1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flo1 ) ) THEN 
  DEALLOCATE(grid%flo1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14280,&
'frame/module_domain.f: Failed to deallocate grid%flo1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bio1 ) ) THEN 
  DEALLOCATE(grid%bio1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14287,&
'frame/module_domain.f: Failed to deallocate grid%bio1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%blo1 ) ) THEN 
  DEALLOCATE(grid%blo1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14294,&
'frame/module_domain.f: Failed to deallocate grid%blo1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ho1 ) ) THEN 
  DEALLOCATE(grid%ho1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14301,&
'frame/module_domain.f: Failed to deallocate grid%ho1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzo2 ) ) THEN 
  DEALLOCATE(grid%dzo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14308,&
'frame/module_domain.f: Failed to deallocate grid%dzo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wo2 ) ) THEN 
  DEALLOCATE(grid%wo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14315,&
'frame/module_domain.f: Failed to deallocate grid%wo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssn2 ) ) THEN 
  DEALLOCATE(grid%tssn2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14322,&
'frame/module_domain.f: Failed to deallocate grid%tssn2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssno2 ) ) THEN 
  DEALLOCATE(grid%tssno2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14329,&
'frame/module_domain.f: Failed to deallocate grid%tssno2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bwo2 ) ) THEN 
  DEALLOCATE(grid%bwo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14336,&
'frame/module_domain.f: Failed to deallocate grid%bwo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bto2 ) ) THEN 
  DEALLOCATE(grid%bto2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14343,&
'frame/module_domain.f: Failed to deallocate grid%bto2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cto2 ) ) THEN 
  DEALLOCATE(grid%cto2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14350,&
'frame/module_domain.f: Failed to deallocate grid%cto2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fio2 ) ) THEN 
  DEALLOCATE(grid%fio2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14357,&
'frame/module_domain.f: Failed to deallocate grid%fio2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flo2 ) ) THEN 
  DEALLOCATE(grid%flo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14364,&
'frame/module_domain.f: Failed to deallocate grid%flo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bio2 ) ) THEN 
  DEALLOCATE(grid%bio2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14371,&
'frame/module_domain.f: Failed to deallocate grid%bio2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%blo2 ) ) THEN 
  DEALLOCATE(grid%blo2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14378,&
'frame/module_domain.f: Failed to deallocate grid%blo2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ho2 ) ) THEN 
  DEALLOCATE(grid%ho2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14385,&
'frame/module_domain.f: Failed to deallocate grid%ho2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzo3 ) ) THEN 
  DEALLOCATE(grid%dzo3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14392,&
'frame/module_domain.f: Failed to deallocate grid%dzo3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wo3 ) ) THEN 
  DEALLOCATE(grid%wo3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14399,&
'frame/module_domain.f: Failed to deallocate grid%wo3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssn3 ) ) THEN 
  DEALLOCATE(grid%tssn3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14406,&
'frame/module_domain.f: Failed to deallocate grid%tssn3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssno3 ) ) THEN 
  DEALLOCATE(grid%tssno3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14413,&
'frame/module_domain.f: Failed to deallocate grid%tssno3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bwo3 ) ) THEN 
  DEALLOCATE(grid%bwo3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14420,&
'frame/module_domain.f: Failed to deallocate grid%bwo3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bto3 ) ) THEN 
  DEALLOCATE(grid%bto3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14427,&
'frame/module_domain.f: Failed to deallocate grid%bto3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cto3 ) ) THEN 
  DEALLOCATE(grid%cto3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14434,&
'frame/module_domain.f: Failed to deallocate grid%cto3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fio3 ) ) THEN 
  DEALLOCATE(grid%fio3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14441,&
'frame/module_domain.f: Failed to deallocate grid%fio3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flo3 ) ) THEN 
  DEALLOCATE(grid%flo3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14448,&
'frame/module_domain.f: Failed to deallocate grid%flo3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bio3 ) ) THEN 
  DEALLOCATE(grid%bio3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14455,&
'frame/module_domain.f: Failed to deallocate grid%bio3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%blo3 ) ) THEN 
  DEALLOCATE(grid%blo3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14462,&
'frame/module_domain.f: Failed to deallocate grid%blo3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ho3 ) ) THEN 
  DEALLOCATE(grid%ho3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14469,&
'frame/module_domain.f: Failed to deallocate grid%ho3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dzo4 ) ) THEN 
  DEALLOCATE(grid%dzo4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14476,&
'frame/module_domain.f: Failed to deallocate grid%dzo4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wo4 ) ) THEN 
  DEALLOCATE(grid%wo4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14483,&
'frame/module_domain.f: Failed to deallocate grid%wo4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssn4 ) ) THEN 
  DEALLOCATE(grid%tssn4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14490,&
'frame/module_domain.f: Failed to deallocate grid%tssn4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tssno4 ) ) THEN 
  DEALLOCATE(grid%tssno4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14497,&
'frame/module_domain.f: Failed to deallocate grid%tssno4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bwo4 ) ) THEN 
  DEALLOCATE(grid%bwo4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14504,&
'frame/module_domain.f: Failed to deallocate grid%bwo4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bto4 ) ) THEN 
  DEALLOCATE(grid%bto4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14511,&
'frame/module_domain.f: Failed to deallocate grid%bto4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cto4 ) ) THEN 
  DEALLOCATE(grid%cto4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14518,&
'frame/module_domain.f: Failed to deallocate grid%cto4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fio4 ) ) THEN 
  DEALLOCATE(grid%fio4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14525,&
'frame/module_domain.f: Failed to deallocate grid%fio4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flo4 ) ) THEN 
  DEALLOCATE(grid%flo4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14532,&
'frame/module_domain.f: Failed to deallocate grid%flo4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bio4 ) ) THEN 
  DEALLOCATE(grid%bio4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14539,&
'frame/module_domain.f: Failed to deallocate grid%bio4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%blo4 ) ) THEN 
  DEALLOCATE(grid%blo4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14546,&
'frame/module_domain.f: Failed to deallocate grid%blo4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ho4 ) ) THEN 
  DEALLOCATE(grid%ho4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14553,&
'frame/module_domain.f: Failed to deallocate grid%ho4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%isnowxy ) ) THEN 
  DEALLOCATE(grid%isnowxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14560,&
'frame/module_domain.f: Failed to deallocate grid%isnowxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tvxy ) ) THEN 
  DEALLOCATE(grid%tvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14567,&
'frame/module_domain.f: Failed to deallocate grid%tvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgxy ) ) THEN 
  DEALLOCATE(grid%tgxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14574,&
'frame/module_domain.f: Failed to deallocate grid%tgxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canicexy ) ) THEN 
  DEALLOCATE(grid%canicexy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14581,&
'frame/module_domain.f: Failed to deallocate grid%canicexy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%canliqxy ) ) THEN 
  DEALLOCATE(grid%canliqxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14588,&
'frame/module_domain.f: Failed to deallocate grid%canliqxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%eahxy ) ) THEN 
  DEALLOCATE(grid%eahxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14595,&
'frame/module_domain.f: Failed to deallocate grid%eahxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tahxy ) ) THEN 
  DEALLOCATE(grid%tahxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14602,&
'frame/module_domain.f: Failed to deallocate grid%tahxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cmxy ) ) THEN 
  DEALLOCATE(grid%cmxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14609,&
'frame/module_domain.f: Failed to deallocate grid%cmxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chxy ) ) THEN 
  DEALLOCATE(grid%chxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14616,&
'frame/module_domain.f: Failed to deallocate grid%chxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fwetxy ) ) THEN 
  DEALLOCATE(grid%fwetxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14623,&
'frame/module_domain.f: Failed to deallocate grid%fwetxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sneqvoxy ) ) THEN 
  DEALLOCATE(grid%sneqvoxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14630,&
'frame/module_domain.f: Failed to deallocate grid%sneqvoxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%alboldxy ) ) THEN 
  DEALLOCATE(grid%alboldxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14637,&
'frame/module_domain.f: Failed to deallocate grid%alboldxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qsnowxy ) ) THEN 
  DEALLOCATE(grid%qsnowxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14644,&
'frame/module_domain.f: Failed to deallocate grid%qsnowxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wslakexy ) ) THEN 
  DEALLOCATE(grid%wslakexy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14651,&
'frame/module_domain.f: Failed to deallocate grid%wslakexy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zwtxy ) ) THEN 
  DEALLOCATE(grid%zwtxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14658,&
'frame/module_domain.f: Failed to deallocate grid%zwtxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%waxy ) ) THEN 
  DEALLOCATE(grid%waxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14665,&
'frame/module_domain.f: Failed to deallocate grid%waxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wtxy ) ) THEN 
  DEALLOCATE(grid%wtxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14672,&
'frame/module_domain.f: Failed to deallocate grid%wtxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsnoxy ) ) THEN 
  DEALLOCATE(grid%tsnoxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14679,&
'frame/module_domain.f: Failed to deallocate grid%tsnoxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%zsnsoxy ) ) THEN 
  DEALLOCATE(grid%zsnsoxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14686,&
'frame/module_domain.f: Failed to deallocate grid%zsnsoxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snicexy ) ) THEN 
  DEALLOCATE(grid%snicexy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14693,&
'frame/module_domain.f: Failed to deallocate grid%snicexy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%snliqxy ) ) THEN 
  DEALLOCATE(grid%snliqxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14700,&
'frame/module_domain.f: Failed to deallocate grid%snliqxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lfmassxy ) ) THEN 
  DEALLOCATE(grid%lfmassxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14707,&
'frame/module_domain.f: Failed to deallocate grid%lfmassxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rtmassxy ) ) THEN 
  DEALLOCATE(grid%rtmassxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14714,&
'frame/module_domain.f: Failed to deallocate grid%rtmassxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%stmassxy ) ) THEN 
  DEALLOCATE(grid%stmassxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14721,&
'frame/module_domain.f: Failed to deallocate grid%stmassxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%woodxy ) ) THEN 
  DEALLOCATE(grid%woodxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14728,&
'frame/module_domain.f: Failed to deallocate grid%woodxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%stblcpxy ) ) THEN 
  DEALLOCATE(grid%stblcpxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14735,&
'frame/module_domain.f: Failed to deallocate grid%stblcpxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fastcpxy ) ) THEN 
  DEALLOCATE(grid%fastcpxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14742,&
'frame/module_domain.f: Failed to deallocate grid%fastcpxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%xsaixy ) ) THEN 
  DEALLOCATE(grid%xsaixy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14749,&
'frame/module_domain.f: Failed to deallocate grid%xsaixy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%taussxy ) ) THEN 
  DEALLOCATE(grid%taussxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14756,&
'frame/module_domain.f: Failed to deallocate grid%taussxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2mvxy ) ) THEN 
  DEALLOCATE(grid%t2mvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14763,&
'frame/module_domain.f: Failed to deallocate grid%t2mvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2mbxy ) ) THEN 
  DEALLOCATE(grid%t2mbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14770,&
'frame/module_domain.f: Failed to deallocate grid%t2mbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2mvxy ) ) THEN 
  DEALLOCATE(grid%q2mvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14777,&
'frame/module_domain.f: Failed to deallocate grid%q2mvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2mbxy ) ) THEN 
  DEALLOCATE(grid%q2mbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14784,&
'frame/module_domain.f: Failed to deallocate grid%q2mbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tradxy ) ) THEN 
  DEALLOCATE(grid%tradxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14791,&
'frame/module_domain.f: Failed to deallocate grid%tradxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%neexy ) ) THEN 
  DEALLOCATE(grid%neexy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14798,&
'frame/module_domain.f: Failed to deallocate grid%neexy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gppxy ) ) THEN 
  DEALLOCATE(grid%gppxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14805,&
'frame/module_domain.f: Failed to deallocate grid%gppxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%nppxy ) ) THEN 
  DEALLOCATE(grid%nppxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14812,&
'frame/module_domain.f: Failed to deallocate grid%nppxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fvegxy ) ) THEN 
  DEALLOCATE(grid%fvegxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14819,&
'frame/module_domain.f: Failed to deallocate grid%fvegxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qinxy ) ) THEN 
  DEALLOCATE(grid%qinxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14826,&
'frame/module_domain.f: Failed to deallocate grid%qinxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%runsfxy ) ) THEN 
  DEALLOCATE(grid%runsfxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14833,&
'frame/module_domain.f: Failed to deallocate grid%runsfxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%runsbxy ) ) THEN 
  DEALLOCATE(grid%runsbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14840,&
'frame/module_domain.f: Failed to deallocate grid%runsbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ecanxy ) ) THEN 
  DEALLOCATE(grid%ecanxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14847,&
'frame/module_domain.f: Failed to deallocate grid%ecanxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%edirxy ) ) THEN 
  DEALLOCATE(grid%edirxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14854,&
'frame/module_domain.f: Failed to deallocate grid%edirxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%etranxy ) ) THEN 
  DEALLOCATE(grid%etranxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14861,&
'frame/module_domain.f: Failed to deallocate grid%etranxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fsaxy ) ) THEN 
  DEALLOCATE(grid%fsaxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14868,&
'frame/module_domain.f: Failed to deallocate grid%fsaxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%firaxy ) ) THEN 
  DEALLOCATE(grid%firaxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14875,&
'frame/module_domain.f: Failed to deallocate grid%firaxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%aparxy ) ) THEN 
  DEALLOCATE(grid%aparxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14882,&
'frame/module_domain.f: Failed to deallocate grid%aparxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psnxy ) ) THEN 
  DEALLOCATE(grid%psnxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14889,&
'frame/module_domain.f: Failed to deallocate grid%psnxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%savxy ) ) THEN 
  DEALLOCATE(grid%savxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14896,&
'frame/module_domain.f: Failed to deallocate grid%savxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sagxy ) ) THEN 
  DEALLOCATE(grid%sagxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14903,&
'frame/module_domain.f: Failed to deallocate grid%sagxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rssunxy ) ) THEN 
  DEALLOCATE(grid%rssunxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14910,&
'frame/module_domain.f: Failed to deallocate grid%rssunxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rsshaxy ) ) THEN 
  DEALLOCATE(grid%rsshaxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14917,&
'frame/module_domain.f: Failed to deallocate grid%rsshaxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%bgapxy ) ) THEN 
  DEALLOCATE(grid%bgapxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14924,&
'frame/module_domain.f: Failed to deallocate grid%bgapxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%wgapxy ) ) THEN 
  DEALLOCATE(grid%wgapxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14931,&
'frame/module_domain.f: Failed to deallocate grid%wgapxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgvxy ) ) THEN 
  DEALLOCATE(grid%tgvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14938,&
'frame/module_domain.f: Failed to deallocate grid%tgvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tgbxy ) ) THEN 
  DEALLOCATE(grid%tgbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14945,&
'frame/module_domain.f: Failed to deallocate grid%tgbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chvxy ) ) THEN 
  DEALLOCATE(grid%chvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14952,&
'frame/module_domain.f: Failed to deallocate grid%chvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chbxy ) ) THEN 
  DEALLOCATE(grid%chbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14959,&
'frame/module_domain.f: Failed to deallocate grid%chbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shgxy ) ) THEN 
  DEALLOCATE(grid%shgxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14966,&
'frame/module_domain.f: Failed to deallocate grid%shgxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shcxy ) ) THEN 
  DEALLOCATE(grid%shcxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14973,&
'frame/module_domain.f: Failed to deallocate grid%shcxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%shbxy ) ) THEN 
  DEALLOCATE(grid%shbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14980,&
'frame/module_domain.f: Failed to deallocate grid%shbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evgxy ) ) THEN 
  DEALLOCATE(grid%evgxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14987,&
'frame/module_domain.f: Failed to deallocate grid%evgxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evbxy ) ) THEN 
  DEALLOCATE(grid%evbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",14994,&
'frame/module_domain.f: Failed to deallocate grid%evbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ghvxy ) ) THEN 
  DEALLOCATE(grid%ghvxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15001,&
'frame/module_domain.f: Failed to deallocate grid%ghvxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ghbxy ) ) THEN 
  DEALLOCATE(grid%ghbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15008,&
'frame/module_domain.f: Failed to deallocate grid%ghbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%irgxy ) ) THEN 
  DEALLOCATE(grid%irgxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15015,&
'frame/module_domain.f: Failed to deallocate grid%irgxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ircxy ) ) THEN 
  DEALLOCATE(grid%ircxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15022,&
'frame/module_domain.f: Failed to deallocate grid%ircxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%irbxy ) ) THEN 
  DEALLOCATE(grid%irbxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15029,&
'frame/module_domain.f: Failed to deallocate grid%irbxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%trxy ) ) THEN 
  DEALLOCATE(grid%trxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15036,&
'frame/module_domain.f: Failed to deallocate grid%trxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%evcxy ) ) THEN 
  DEALLOCATE(grid%evcxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15043,&
'frame/module_domain.f: Failed to deallocate grid%evcxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chleafxy ) ) THEN 
  DEALLOCATE(grid%chleafxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15050,&
'frame/module_domain.f: Failed to deallocate grid%chleafxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chucxy ) ) THEN 
  DEALLOCATE(grid%chucxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15057,&
'frame/module_domain.f: Failed to deallocate grid%chucxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chv2xy ) ) THEN 
  DEALLOCATE(grid%chv2xy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15064,&
'frame/module_domain.f: Failed to deallocate grid%chv2xy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chb2xy ) ) THEN 
  DEALLOCATE(grid%chb2xy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15071,&
'frame/module_domain.f: Failed to deallocate grid%chb2xy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chstarxy ) ) THEN 
  DEALLOCATE(grid%chstarxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15078,&
'frame/module_domain.f: Failed to deallocate grid%chstarxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smoiseq ) ) THEN 
  DEALLOCATE(grid%smoiseq,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15085,&
'frame/module_domain.f: Failed to deallocate grid%smoiseq. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%smcwtdxy ) ) THEN 
  DEALLOCATE(grid%smcwtdxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15092,&
'frame/module_domain.f: Failed to deallocate grid%smcwtdxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rechxy ) ) THEN 
  DEALLOCATE(grid%rechxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15099,&
'frame/module_domain.f: Failed to deallocate grid%rechxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%deeprechxy ) ) THEN 
  DEALLOCATE(grid%deeprechxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15106,&
'frame/module_domain.f: Failed to deallocate grid%deeprechxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%areaxy ) ) THEN 
  DEALLOCATE(grid%areaxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15113,&
'frame/module_domain.f: Failed to deallocate grid%areaxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qrfxy ) ) THEN 
  DEALLOCATE(grid%qrfxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15120,&
'frame/module_domain.f: Failed to deallocate grid%qrfxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qrfsxy ) ) THEN 
  DEALLOCATE(grid%qrfsxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15127,&
'frame/module_domain.f: Failed to deallocate grid%qrfsxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qspringxy ) ) THEN 
  DEALLOCATE(grid%qspringxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15134,&
'frame/module_domain.f: Failed to deallocate grid%qspringxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qspringsxy ) ) THEN 
  DEALLOCATE(grid%qspringsxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15141,&
'frame/module_domain.f: Failed to deallocate grid%qspringsxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qslatxy ) ) THEN 
  DEALLOCATE(grid%qslatxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15148,&
'frame/module_domain.f: Failed to deallocate grid%qslatxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pexpxy ) ) THEN 
  DEALLOCATE(grid%pexpxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15155,&
'frame/module_domain.f: Failed to deallocate grid%pexpxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rivercondxy ) ) THEN 
  DEALLOCATE(grid%rivercondxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15162,&
'frame/module_domain.f: Failed to deallocate grid%rivercondxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fdepthxy ) ) THEN 
  DEALLOCATE(grid%fdepthxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15169,&
'frame/module_domain.f: Failed to deallocate grid%fdepthxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%eqzwt ) ) THEN 
  DEALLOCATE(grid%eqzwt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15176,&
'frame/module_domain.f: Failed to deallocate grid%eqzwt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rechclim ) ) THEN 
  DEALLOCATE(grid%rechclim,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15183,&
'frame/module_domain.f: Failed to deallocate grid%rechclim. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%riverbedxy ) ) THEN 
  DEALLOCATE(grid%riverbedxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15190,&
'frame/module_domain.f: Failed to deallocate grid%riverbedxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilcomp ) ) THEN 
  DEALLOCATE(grid%soilcomp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15197,&
'frame/module_domain.f: Failed to deallocate grid%soilcomp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilcl1 ) ) THEN 
  DEALLOCATE(grid%soilcl1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15204,&
'frame/module_domain.f: Failed to deallocate grid%soilcl1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilcl2 ) ) THEN 
  DEALLOCATE(grid%soilcl2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15211,&
'frame/module_domain.f: Failed to deallocate grid%soilcl2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilcl3 ) ) THEN 
  DEALLOCATE(grid%soilcl3,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15218,&
'frame/module_domain.f: Failed to deallocate grid%soilcl3. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%soilcl4 ) ) THEN 
  DEALLOCATE(grid%soilcl4,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15225,&
'frame/module_domain.f: Failed to deallocate grid%soilcl4. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grainxy ) ) THEN 
  DEALLOCATE(grid%grainxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15232,&
'frame/module_domain.f: Failed to deallocate grid%grainxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gddxy ) ) THEN 
  DEALLOCATE(grid%gddxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15239,&
'frame/module_domain.f: Failed to deallocate grid%gddxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%croptype ) ) THEN 
  DEALLOCATE(grid%croptype,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15246,&
'frame/module_domain.f: Failed to deallocate grid%croptype. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%planting ) ) THEN 
  DEALLOCATE(grid%planting,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15253,&
'frame/module_domain.f: Failed to deallocate grid%planting. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%harvest ) ) THEN 
  DEALLOCATE(grid%harvest,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15260,&
'frame/module_domain.f: Failed to deallocate grid%harvest. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%season_gdd ) ) THEN 
  DEALLOCATE(grid%season_gdd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15267,&
'frame/module_domain.f: Failed to deallocate grid%season_gdd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%cropcat ) ) THEN 
  DEALLOCATE(grid%cropcat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15274,&
'frame/module_domain.f: Failed to deallocate grid%cropcat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pgsxy ) ) THEN 
  DEALLOCATE(grid%pgsxy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15281,&
'frame/module_domain.f: Failed to deallocate grid%pgsxy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%gecros_state ) ) THEN 
  DEALLOCATE(grid%gecros_state,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15288,&
'frame/module_domain.f: Failed to deallocate grid%gecros_state. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ql ) ) THEN 
  DEALLOCATE(grid%kext_ql,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15295,&
'frame/module_domain.f: Failed to deallocate grid%kext_ql. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qic ) ) THEN 
  DEALLOCATE(grid%kext_qic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15302,&
'frame/module_domain.f: Failed to deallocate grid%kext_qic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qip ) ) THEN 
  DEALLOCATE(grid%kext_qip,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15309,&
'frame/module_domain.f: Failed to deallocate grid%kext_qip. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qid ) ) THEN 
  DEALLOCATE(grid%kext_qid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15316,&
'frame/module_domain.f: Failed to deallocate grid%kext_qid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qs ) ) THEN 
  DEALLOCATE(grid%kext_qs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15323,&
'frame/module_domain.f: Failed to deallocate grid%kext_qs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qg ) ) THEN 
  DEALLOCATE(grid%kext_qg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15330,&
'frame/module_domain.f: Failed to deallocate grid%kext_qg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qh ) ) THEN 
  DEALLOCATE(grid%kext_qh,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15337,&
'frame/module_domain.f: Failed to deallocate grid%kext_qh. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_qa ) ) THEN 
  DEALLOCATE(grid%kext_qa,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15344,&
'frame/module_domain.f: Failed to deallocate grid%kext_qa. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qic ) ) THEN 
  DEALLOCATE(grid%kext_ft_qic,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15351,&
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qic. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qip ) ) THEN 
  DEALLOCATE(grid%kext_ft_qip,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15358,&
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qip. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qid ) ) THEN 
  DEALLOCATE(grid%kext_ft_qid,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15365,&
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qid. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qs ) ) THEN 
  DEALLOCATE(grid%kext_ft_qs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15372,&
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qs. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%kext_ft_qg ) ) THEN 
  DEALLOCATE(grid%kext_ft_qg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15379,&
'frame/module_domain.f: Failed to deallocate grid%kext_ft_qg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%height ) ) THEN 
  DEALLOCATE(grid%height,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15386,&
'frame/module_domain.f: Failed to deallocate grid%height. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tempc ) ) THEN 
  DEALLOCATE(grid%tempc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15393,&
'frame/module_domain.f: Failed to deallocate grid%tempc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_pl ) ) THEN 
  DEALLOCATE(grid%p_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15400,&
'frame/module_domain.f: Failed to deallocate grid%p_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_pl ) ) THEN 
  DEALLOCATE(grid%u_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15407,&
'frame/module_domain.f: Failed to deallocate grid%u_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_pl ) ) THEN 
  DEALLOCATE(grid%v_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15414,&
'frame/module_domain.f: Failed to deallocate grid%v_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_pl ) ) THEN 
  DEALLOCATE(grid%t_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15421,&
'frame/module_domain.f: Failed to deallocate grid%t_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_pl ) ) THEN 
  DEALLOCATE(grid%rh_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15428,&
'frame/module_domain.f: Failed to deallocate grid%rh_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ght_pl ) ) THEN 
  DEALLOCATE(grid%ght_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15435,&
'frame/module_domain.f: Failed to deallocate grid%ght_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%s_pl ) ) THEN 
  DEALLOCATE(grid%s_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15442,&
'frame/module_domain.f: Failed to deallocate grid%s_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%td_pl ) ) THEN 
  DEALLOCATE(grid%td_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15449,&
'frame/module_domain.f: Failed to deallocate grid%td_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q_pl ) ) THEN 
  DEALLOCATE(grid%q_pl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15456,&
'frame/module_domain.f: Failed to deallocate grid%q_pl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%z_zl ) ) THEN 
  DEALLOCATE(grid%z_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15463,&
'frame/module_domain.f: Failed to deallocate grid%z_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u_zl ) ) THEN 
  DEALLOCATE(grid%u_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15470,&
'frame/module_domain.f: Failed to deallocate grid%u_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v_zl ) ) THEN 
  DEALLOCATE(grid%v_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15477,&
'frame/module_domain.f: Failed to deallocate grid%v_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t_zl ) ) THEN 
  DEALLOCATE(grid%t_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15484,&
'frame/module_domain.f: Failed to deallocate grid%t_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rh_zl ) ) THEN 
  DEALLOCATE(grid%rh_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15491,&
'frame/module_domain.f: Failed to deallocate grid%rh_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%ght_zl ) ) THEN 
  DEALLOCATE(grid%ght_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15498,&
'frame/module_domain.f: Failed to deallocate grid%ght_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%s_zl ) ) THEN 
  DEALLOCATE(grid%s_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15505,&
'frame/module_domain.f: Failed to deallocate grid%s_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%td_zl ) ) THEN 
  DEALLOCATE(grid%td_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15512,&
'frame/module_domain.f: Failed to deallocate grid%td_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q_zl ) ) THEN 
  DEALLOCATE(grid%q_zl,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15519,&
'frame/module_domain.f: Failed to deallocate grid%q_zl. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tcoli_max ) ) THEN 
  DEALLOCATE(grid%tcoli_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15526,&
'frame/module_domain.f: Failed to deallocate grid%tcoli_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%grpl_flx_max ) ) THEN 
  DEALLOCATE(grid%grpl_flx_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15533,&
'frame/module_domain.f: Failed to deallocate grid%grpl_flx_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%refd_com ) ) THEN 
  DEALLOCATE(grid%refd_com,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15540,&
'frame/module_domain.f: Failed to deallocate grid%refd_com. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%refd ) ) THEN 
  DEALLOCATE(grid%refd,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15547,&
'frame/module_domain.f: Failed to deallocate grid%refd. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vil ) ) THEN 
  DEALLOCATE(grid%vil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15554,&
'frame/module_domain.f: Failed to deallocate grid%vil. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%radarvil ) ) THEN 
  DEALLOCATE(grid%radarvil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15561,&
'frame/module_domain.f: Failed to deallocate grid%radarvil. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%echotop ) ) THEN 
  DEALLOCATE(grid%echotop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15568,&
'frame/module_domain.f: Failed to deallocate grid%echotop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%fzlev ) ) THEN 
  DEALLOCATE(grid%fzlev,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15575,&
'frame/module_domain.f: Failed to deallocate grid%fzlev. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icingtop ) ) THEN 
  DEALLOCATE(grid%icingtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15582,&
'frame/module_domain.f: Failed to deallocate grid%icingtop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icingbot ) ) THEN 
  DEALLOCATE(grid%icingbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15589,&
'frame/module_domain.f: Failed to deallocate grid%icingbot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qicing_lg ) ) THEN 
  DEALLOCATE(grid%qicing_lg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15596,&
'frame/module_domain.f: Failed to deallocate grid%qicing_lg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qicing_sm ) ) THEN 
  DEALLOCATE(grid%qicing_sm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15603,&
'frame/module_domain.f: Failed to deallocate grid%qicing_sm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qicing_lg_max ) ) THEN 
  DEALLOCATE(grid%qicing_lg_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15610,&
'frame/module_domain.f: Failed to deallocate grid%qicing_lg_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%qicing_sm_max ) ) THEN 
  DEALLOCATE(grid%qicing_sm_max,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15617,&
'frame/module_domain.f: Failed to deallocate grid%qicing_sm_max. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icing_lg ) ) THEN 
  DEALLOCATE(grid%icing_lg,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15624,&
'frame/module_domain.f: Failed to deallocate grid%icing_lg. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%icing_sm ) ) THEN 
  DEALLOCATE(grid%icing_sm,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15631,&
'frame/module_domain.f: Failed to deallocate grid%icing_sm. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_mslp ) ) THEN 
  DEALLOCATE(grid%afwa_mslp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15638,&
'frame/module_domain.f: Failed to deallocate grid%afwa_mslp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_heatidx ) ) THEN 
  DEALLOCATE(grid%afwa_heatidx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15645,&
'frame/module_domain.f: Failed to deallocate grid%afwa_heatidx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_wchill ) ) THEN 
  DEALLOCATE(grid%afwa_wchill,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15652,&
'frame/module_domain.f: Failed to deallocate grid%afwa_wchill. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_fits ) ) THEN 
  DEALLOCATE(grid%afwa_fits,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15659,&
'frame/module_domain.f: Failed to deallocate grid%afwa_fits. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_tlyrbot ) ) THEN 
  DEALLOCATE(grid%afwa_tlyrbot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15666,&
'frame/module_domain.f: Failed to deallocate grid%afwa_tlyrbot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_tlyrtop ) ) THEN 
  DEALLOCATE(grid%afwa_tlyrtop,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15673,&
'frame/module_domain.f: Failed to deallocate grid%afwa_tlyrtop. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_turb ) ) THEN 
  DEALLOCATE(grid%afwa_turb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15680,&
'frame/module_domain.f: Failed to deallocate grid%afwa_turb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_llturb ) ) THEN 
  DEALLOCATE(grid%afwa_llturb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15687,&
'frame/module_domain.f: Failed to deallocate grid%afwa_llturb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_llturblgt ) ) THEN 
  DEALLOCATE(grid%afwa_llturblgt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15694,&
'frame/module_domain.f: Failed to deallocate grid%afwa_llturblgt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_llturbmdt ) ) THEN 
  DEALLOCATE(grid%afwa_llturbmdt,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15701,&
'frame/module_domain.f: Failed to deallocate grid%afwa_llturbmdt. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_llturbsvr ) ) THEN 
  DEALLOCATE(grid%afwa_llturbsvr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15708,&
'frame/module_domain.f: Failed to deallocate grid%afwa_llturbsvr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_precip ) ) THEN 
  DEALLOCATE(grid%afwa_precip,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15715,&
'frame/module_domain.f: Failed to deallocate grid%afwa_precip. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_totprecip ) ) THEN 
  DEALLOCATE(grid%afwa_totprecip,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15722,&
'frame/module_domain.f: Failed to deallocate grid%afwa_totprecip. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_rain ) ) THEN 
  DEALLOCATE(grid%afwa_rain,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15729,&
'frame/module_domain.f: Failed to deallocate grid%afwa_rain. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_snow ) ) THEN 
  DEALLOCATE(grid%afwa_snow,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15736,&
'frame/module_domain.f: Failed to deallocate grid%afwa_snow. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_ice ) ) THEN 
  DEALLOCATE(grid%afwa_ice,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15743,&
'frame/module_domain.f: Failed to deallocate grid%afwa_ice. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_fzra ) ) THEN 
  DEALLOCATE(grid%afwa_fzra,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15750,&
'frame/module_domain.f: Failed to deallocate grid%afwa_fzra. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_snowfall ) ) THEN 
  DEALLOCATE(grid%afwa_snowfall,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15757,&
'frame/module_domain.f: Failed to deallocate grid%afwa_snowfall. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_vis ) ) THEN 
  DEALLOCATE(grid%afwa_vis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15764,&
'frame/module_domain.f: Failed to deallocate grid%afwa_vis. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_vis_alpha ) ) THEN 
  DEALLOCATE(grid%afwa_vis_alpha,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15771,&
'frame/module_domain.f: Failed to deallocate grid%afwa_vis_alpha. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_vis_dust ) ) THEN 
  DEALLOCATE(grid%afwa_vis_dust,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15778,&
'frame/module_domain.f: Failed to deallocate grid%afwa_vis_dust. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cloud ) ) THEN 
  DEALLOCATE(grid%afwa_cloud,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15785,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cloud. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cloud_ceil ) ) THEN 
  DEALLOCATE(grid%afwa_cloud_ceil,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15792,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cloud_ceil. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cape ) ) THEN 
  DEALLOCATE(grid%afwa_cape,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15799,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cape. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cin ) ) THEN 
  DEALLOCATE(grid%afwa_cin,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15806,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cin. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cape_mu ) ) THEN 
  DEALLOCATE(grid%afwa_cape_mu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15813,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cape_mu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_cin_mu ) ) THEN 
  DEALLOCATE(grid%afwa_cin_mu,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15820,&
'frame/module_domain.f: Failed to deallocate grid%afwa_cin_mu. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_zlfc ) ) THEN 
  DEALLOCATE(grid%afwa_zlfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15827,&
'frame/module_domain.f: Failed to deallocate grid%afwa_zlfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_plfc ) ) THEN 
  DEALLOCATE(grid%afwa_plfc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15834,&
'frame/module_domain.f: Failed to deallocate grid%afwa_plfc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_lidx ) ) THEN 
  DEALLOCATE(grid%afwa_lidx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15841,&
'frame/module_domain.f: Failed to deallocate grid%afwa_lidx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_pwat ) ) THEN 
  DEALLOCATE(grid%afwa_pwat,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15848,&
'frame/module_domain.f: Failed to deallocate grid%afwa_pwat. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%midrh_min ) ) THEN 
  DEALLOCATE(grid%midrh_min,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15855,&
'frame/module_domain.f: Failed to deallocate grid%midrh_min. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%midrh_min_old ) ) THEN 
  DEALLOCATE(grid%midrh_min_old,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15862,&
'frame/module_domain.f: Failed to deallocate grid%midrh_min_old. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_hail ) ) THEN 
  DEALLOCATE(grid%afwa_hail,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15869,&
'frame/module_domain.f: Failed to deallocate grid%afwa_hail. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_llws ) ) THEN 
  DEALLOCATE(grid%afwa_llws,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15876,&
'frame/module_domain.f: Failed to deallocate grid%afwa_llws. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%afwa_tornado ) ) THEN 
  DEALLOCATE(grid%afwa_tornado,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15883,&
'frame/module_domain.f: Failed to deallocate grid%afwa_tornado. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tornado_mask ) ) THEN 
  DEALLOCATE(grid%tornado_mask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15890,&
'frame/module_domain.f: Failed to deallocate grid%tornado_mask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tornado_dur ) ) THEN 
  DEALLOCATE(grid%tornado_dur,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15897,&
'frame/module_domain.f: Failed to deallocate grid%tornado_dur. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psfc_mean ) ) THEN 
  DEALLOCATE(grid%psfc_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15904,&
'frame/module_domain.f: Failed to deallocate grid%psfc_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_mean ) ) THEN 
  DEALLOCATE(grid%tsk_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15911,&
'frame/module_domain.f: Failed to deallocate grid%tsk_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pmsl_mean ) ) THEN 
  DEALLOCATE(grid%pmsl_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15918,&
'frame/module_domain.f: Failed to deallocate grid%pmsl_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2_mean ) ) THEN 
  DEALLOCATE(grid%t2_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15925,&
'frame/module_domain.f: Failed to deallocate grid%t2_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2_mean ) ) THEN 
  DEALLOCATE(grid%th2_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15932,&
'frame/module_domain.f: Failed to deallocate grid%th2_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2_mean ) ) THEN 
  DEALLOCATE(grid%q2_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15939,&
'frame/module_domain.f: Failed to deallocate grid%q2_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10_mean ) ) THEN 
  DEALLOCATE(grid%u10_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15946,&
'frame/module_domain.f: Failed to deallocate grid%u10_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10_mean ) ) THEN 
  DEALLOCATE(grid%v10_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15953,&
'frame/module_domain.f: Failed to deallocate grid%v10_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_mean ) ) THEN 
  DEALLOCATE(grid%hfx_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15960,&
'frame/module_domain.f: Failed to deallocate grid%hfx_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_mean ) ) THEN 
  DEALLOCATE(grid%lh_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15967,&
'frame/module_domain.f: Failed to deallocate grid%lh_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnb_mean ) ) THEN 
  DEALLOCATE(grid%swdnb_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15974,&
'frame/module_domain.f: Failed to deallocate grid%swdnb_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%glw_mean ) ) THEN 
  DEALLOCATE(grid%glw_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15981,&
'frame/module_domain.f: Failed to deallocate grid%glw_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupb_mean ) ) THEN 
  DEALLOCATE(grid%lwupb_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15988,&
'frame/module_domain.f: Failed to deallocate grid%lwupb_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupb_mean ) ) THEN 
  DEALLOCATE(grid%swupb_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",15995,&
'frame/module_domain.f: Failed to deallocate grid%swupb_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupt_mean ) ) THEN 
  DEALLOCATE(grid%swupt_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16002,&
'frame/module_domain.f: Failed to deallocate grid%swupt_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnt_mean ) ) THEN 
  DEALLOCATE(grid%swdnt_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16009,&
'frame/module_domain.f: Failed to deallocate grid%swdnt_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupt_mean ) ) THEN 
  DEALLOCATE(grid%lwupt_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16016,&
'frame/module_domain.f: Failed to deallocate grid%lwupt_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnt_mean ) ) THEN 
  DEALLOCATE(grid%lwdnt_mean,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16023,&
'frame/module_domain.f: Failed to deallocate grid%lwdnt_mean. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psfc_diurn ) ) THEN 
  DEALLOCATE(grid%psfc_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16030,&
'frame/module_domain.f: Failed to deallocate grid%psfc_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_diurn ) ) THEN 
  DEALLOCATE(grid%tsk_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16037,&
'frame/module_domain.f: Failed to deallocate grid%tsk_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2_diurn ) ) THEN 
  DEALLOCATE(grid%t2_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16044,&
'frame/module_domain.f: Failed to deallocate grid%t2_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2_diurn ) ) THEN 
  DEALLOCATE(grid%th2_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16051,&
'frame/module_domain.f: Failed to deallocate grid%th2_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2_diurn ) ) THEN 
  DEALLOCATE(grid%q2_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16058,&
'frame/module_domain.f: Failed to deallocate grid%q2_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10_diurn ) ) THEN 
  DEALLOCATE(grid%u10_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16065,&
'frame/module_domain.f: Failed to deallocate grid%u10_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10_diurn ) ) THEN 
  DEALLOCATE(grid%v10_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16072,&
'frame/module_domain.f: Failed to deallocate grid%v10_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_diurn ) ) THEN 
  DEALLOCATE(grid%hfx_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16079,&
'frame/module_domain.f: Failed to deallocate grid%hfx_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_diurn ) ) THEN 
  DEALLOCATE(grid%lh_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16086,&
'frame/module_domain.f: Failed to deallocate grid%lh_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnb_diurn ) ) THEN 
  DEALLOCATE(grid%swdnb_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16093,&
'frame/module_domain.f: Failed to deallocate grid%swdnb_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%glw_diurn ) ) THEN 
  DEALLOCATE(grid%glw_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16100,&
'frame/module_domain.f: Failed to deallocate grid%glw_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupb_diurn ) ) THEN 
  DEALLOCATE(grid%lwupb_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16107,&
'frame/module_domain.f: Failed to deallocate grid%lwupb_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupb_diurn ) ) THEN 
  DEALLOCATE(grid%swupb_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16114,&
'frame/module_domain.f: Failed to deallocate grid%swupb_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupt_diurn ) ) THEN 
  DEALLOCATE(grid%swupt_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16121,&
'frame/module_domain.f: Failed to deallocate grid%swupt_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnt_diurn ) ) THEN 
  DEALLOCATE(grid%swdnt_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16128,&
'frame/module_domain.f: Failed to deallocate grid%swdnt_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupt_diurn ) ) THEN 
  DEALLOCATE(grid%lwupt_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16135,&
'frame/module_domain.f: Failed to deallocate grid%lwupt_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnt_diurn ) ) THEN 
  DEALLOCATE(grid%lwdnt_diurn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16142,&
'frame/module_domain.f: Failed to deallocate grid%lwdnt_diurn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%psfc_dtmp ) ) THEN 
  DEALLOCATE(grid%psfc_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16149,&
'frame/module_domain.f: Failed to deallocate grid%psfc_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%tsk_dtmp ) ) THEN 
  DEALLOCATE(grid%tsk_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16156,&
'frame/module_domain.f: Failed to deallocate grid%tsk_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%t2_dtmp ) ) THEN 
  DEALLOCATE(grid%t2_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16163,&
'frame/module_domain.f: Failed to deallocate grid%t2_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%th2_dtmp ) ) THEN 
  DEALLOCATE(grid%th2_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16170,&
'frame/module_domain.f: Failed to deallocate grid%th2_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%q2_dtmp ) ) THEN 
  DEALLOCATE(grid%q2_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16177,&
'frame/module_domain.f: Failed to deallocate grid%q2_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%u10_dtmp ) ) THEN 
  DEALLOCATE(grid%u10_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16184,&
'frame/module_domain.f: Failed to deallocate grid%u10_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%v10_dtmp ) ) THEN 
  DEALLOCATE(grid%v10_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16191,&
'frame/module_domain.f: Failed to deallocate grid%v10_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%hfx_dtmp ) ) THEN 
  DEALLOCATE(grid%hfx_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16198,&
'frame/module_domain.f: Failed to deallocate grid%hfx_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lh_dtmp ) ) THEN 
  DEALLOCATE(grid%lh_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16205,&
'frame/module_domain.f: Failed to deallocate grid%lh_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnb_dtmp ) ) THEN 
  DEALLOCATE(grid%swdnb_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16212,&
'frame/module_domain.f: Failed to deallocate grid%swdnb_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%glw_dtmp ) ) THEN 
  DEALLOCATE(grid%glw_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16219,&
'frame/module_domain.f: Failed to deallocate grid%glw_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupb_dtmp ) ) THEN 
  DEALLOCATE(grid%lwupb_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16226,&
'frame/module_domain.f: Failed to deallocate grid%lwupb_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupb_dtmp ) ) THEN 
  DEALLOCATE(grid%swupb_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16233,&
'frame/module_domain.f: Failed to deallocate grid%swupb_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swupt_dtmp ) ) THEN 
  DEALLOCATE(grid%swupt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16240,&
'frame/module_domain.f: Failed to deallocate grid%swupt_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%swdnt_dtmp ) ) THEN 
  DEALLOCATE(grid%swdnt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16247,&
'frame/module_domain.f: Failed to deallocate grid%swdnt_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwupt_dtmp ) ) THEN 
  DEALLOCATE(grid%lwupt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16254,&
'frame/module_domain.f: Failed to deallocate grid%lwupt_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lwdnt_dtmp ) ) THEN 
  DEALLOCATE(grid%lwdnt_dtmp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16261,&
'frame/module_domain.f: Failed to deallocate grid%lwdnt_dtmp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%rscghis_2d ) ) THEN 
  DEALLOCATE(grid%rscghis_2d,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16268,&
'frame/module_domain.f: Failed to deallocate grid%rscghis_2d. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%induc ) ) THEN 
  DEALLOCATE(grid%induc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16275,&
'frame/module_domain.f: Failed to deallocate grid%induc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%noninduc ) ) THEN 
  DEALLOCATE(grid%noninduc,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16282,&
'frame/module_domain.f: Failed to deallocate grid%noninduc. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sctot ) ) THEN 
  DEALLOCATE(grid%sctot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16289,&
'frame/module_domain.f: Failed to deallocate grid%sctot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%elecmag ) ) THEN 
  DEALLOCATE(grid%elecmag,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16296,&
'frame/module_domain.f: Failed to deallocate grid%elecmag. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%elecx ) ) THEN 
  DEALLOCATE(grid%elecx,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16303,&
'frame/module_domain.f: Failed to deallocate grid%elecx. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%elecy ) ) THEN 
  DEALLOCATE(grid%elecy,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16310,&
'frame/module_domain.f: Failed to deallocate grid%elecy. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%elecz ) ) THEN 
  DEALLOCATE(grid%elecz,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16317,&
'frame/module_domain.f: Failed to deallocate grid%elecz. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pot ) ) THEN 
  DEALLOCATE(grid%pot,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16324,&
'frame/module_domain.f: Failed to deallocate grid%pot. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%light ) ) THEN 
  DEALLOCATE(grid%light,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16331,&
'frame/module_domain.f: Failed to deallocate grid%light. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lightdens ) ) THEN 
  DEALLOCATE(grid%lightdens,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16338,&
'frame/module_domain.f: Failed to deallocate grid%lightdens. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lightdis ) ) THEN 
  DEALLOCATE(grid%lightdis,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16345,&
'frame/module_domain.f: Failed to deallocate grid%lightdis. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flshi ) ) THEN 
  DEALLOCATE(grid%flshi,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16352,&
'frame/module_domain.f: Failed to deallocate grid%flshi. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flshn ) ) THEN 
  DEALLOCATE(grid%flshn,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16359,&
'frame/module_domain.f: Failed to deallocate grid%flshn. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%flshp ) ) THEN 
  DEALLOCATE(grid%flshp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16366,&
'frame/module_domain.f: Failed to deallocate grid%flshp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_u_tend_perturb ) ) THEN 
  DEALLOCATE(grid%field_u_tend_perturb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16373,&
'frame/module_domain.f: Failed to deallocate grid%field_u_tend_perturb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_v_tend_perturb ) ) THEN 
  DEALLOCATE(grid%field_v_tend_perturb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16380,&
'frame/module_domain.f: Failed to deallocate grid%field_v_tend_perturb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%field_t_tend_perturb ) ) THEN 
  DEALLOCATE(grid%field_t_tend_perturb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16387,&
'frame/module_domain.f: Failed to deallocate grid%field_t_tend_perturb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c1h ) ) THEN 
  DEALLOCATE(grid%c1h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16394,&
'frame/module_domain.f: Failed to deallocate grid%c1h. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c2h ) ) THEN 
  DEALLOCATE(grid%c2h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16401,&
'frame/module_domain.f: Failed to deallocate grid%c2h. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c1f ) ) THEN 
  DEALLOCATE(grid%c1f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16408,&
'frame/module_domain.f: Failed to deallocate grid%c1f. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c2f ) ) THEN 
  DEALLOCATE(grid%c2f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16415,&
'frame/module_domain.f: Failed to deallocate grid%c2f. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c3h ) ) THEN 
  DEALLOCATE(grid%c3h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16422,&
'frame/module_domain.f: Failed to deallocate grid%c3h. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c4h ) ) THEN 
  DEALLOCATE(grid%c4h,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16429,&
'frame/module_domain.f: Failed to deallocate grid%c4h. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c3f ) ) THEN 
  DEALLOCATE(grid%c3f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16436,&
'frame/module_domain.f: Failed to deallocate grid%c3f. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%c4f ) ) THEN 
  DEALLOCATE(grid%c4f,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16443,&
'frame/module_domain.f: Failed to deallocate grid%c4f. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pcb ) ) THEN 
  DEALLOCATE(grid%pcb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16450,&
'frame/module_domain.f: Failed to deallocate grid%pcb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pc_1 ) ) THEN 
  DEALLOCATE(grid%pc_1,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16457,&
'frame/module_domain.f: Failed to deallocate grid%pc_1. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pc_2 ) ) THEN 
  DEALLOCATE(grid%pc_2,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16464,&
'frame/module_domain.f: Failed to deallocate grid%pc_2. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pc_bxs ) ) THEN 
  DEALLOCATE(grid%pc_bxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16471,&
'frame/module_domain.f: Failed to deallocate grid%pc_bxs. ')
 endif
  NULLIFY(grid%pc_bxs)
ENDIF
IF ( ASSOCIATED( grid%pc_bxe ) ) THEN 
  DEALLOCATE(grid%pc_bxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16479,&
'frame/module_domain.f: Failed to deallocate grid%pc_bxe. ')
 endif
  NULLIFY(grid%pc_bxe)
ENDIF
IF ( ASSOCIATED( grid%pc_bys ) ) THEN 
  DEALLOCATE(grid%pc_bys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16487,&
'frame/module_domain.f: Failed to deallocate grid%pc_bys. ')
 endif
  NULLIFY(grid%pc_bys)
ENDIF
IF ( ASSOCIATED( grid%pc_bye ) ) THEN 
  DEALLOCATE(grid%pc_bye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16495,&
'frame/module_domain.f: Failed to deallocate grid%pc_bye. ')
 endif
  NULLIFY(grid%pc_bye)
ENDIF
IF ( ASSOCIATED( grid%pc_btxs ) ) THEN 
  DEALLOCATE(grid%pc_btxs,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16503,&
'frame/module_domain.f: Failed to deallocate grid%pc_btxs. ')
 endif
  NULLIFY(grid%pc_btxs)
ENDIF
IF ( ASSOCIATED( grid%pc_btxe ) ) THEN 
  DEALLOCATE(grid%pc_btxe,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16511,&
'frame/module_domain.f: Failed to deallocate grid%pc_btxe. ')
 endif
  NULLIFY(grid%pc_btxe)
ENDIF
IF ( ASSOCIATED( grid%pc_btys ) ) THEN 
  DEALLOCATE(grid%pc_btys,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16519,&
'frame/module_domain.f: Failed to deallocate grid%pc_btys. ')
 endif
  NULLIFY(grid%pc_btys)
ENDIF
IF ( ASSOCIATED( grid%pc_btye ) ) THEN 
  DEALLOCATE(grid%pc_btye,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16527,&
'frame/module_domain.f: Failed to deallocate grid%pc_btye. ')
 endif
  NULLIFY(grid%pc_btye)
ENDIF
IF ( ASSOCIATED( grid%p_wif_now ) ) THEN 
  DEALLOCATE(grid%p_wif_now,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16535,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_now. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_jan ) ) THEN 
  DEALLOCATE(grid%p_wif_jan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16542,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_jan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_feb ) ) THEN 
  DEALLOCATE(grid%p_wif_feb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16549,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_feb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_mar ) ) THEN 
  DEALLOCATE(grid%p_wif_mar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16556,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_mar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_apr ) ) THEN 
  DEALLOCATE(grid%p_wif_apr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16563,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_apr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_may ) ) THEN 
  DEALLOCATE(grid%p_wif_may,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16570,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_may. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_jun ) ) THEN 
  DEALLOCATE(grid%p_wif_jun,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16577,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_jun. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_jul ) ) THEN 
  DEALLOCATE(grid%p_wif_jul,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16584,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_jul. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_aug ) ) THEN 
  DEALLOCATE(grid%p_wif_aug,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16591,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_aug. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_sep ) ) THEN 
  DEALLOCATE(grid%p_wif_sep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16598,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_sep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_oct ) ) THEN 
  DEALLOCATE(grid%p_wif_oct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16605,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_oct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_nov ) ) THEN 
  DEALLOCATE(grid%p_wif_nov,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16612,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_nov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%p_wif_dec ) ) THEN 
  DEALLOCATE(grid%p_wif_dec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16619,&
'frame/module_domain.f: Failed to deallocate grid%p_wif_dec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_now ) ) THEN 
  DEALLOCATE(grid%w_wif_now,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16626,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_now. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_jan ) ) THEN 
  DEALLOCATE(grid%w_wif_jan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16633,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_jan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_feb ) ) THEN 
  DEALLOCATE(grid%w_wif_feb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16640,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_feb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_mar ) ) THEN 
  DEALLOCATE(grid%w_wif_mar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16647,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_mar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_apr ) ) THEN 
  DEALLOCATE(grid%w_wif_apr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16654,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_apr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_may ) ) THEN 
  DEALLOCATE(grid%w_wif_may,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16661,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_may. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_jun ) ) THEN 
  DEALLOCATE(grid%w_wif_jun,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16668,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_jun. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_jul ) ) THEN 
  DEALLOCATE(grid%w_wif_jul,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16675,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_jul. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_aug ) ) THEN 
  DEALLOCATE(grid%w_wif_aug,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16682,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_aug. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_sep ) ) THEN 
  DEALLOCATE(grid%w_wif_sep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16689,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_sep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_oct ) ) THEN 
  DEALLOCATE(grid%w_wif_oct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16696,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_oct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_nov ) ) THEN 
  DEALLOCATE(grid%w_wif_nov,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16703,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_nov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%w_wif_dec ) ) THEN 
  DEALLOCATE(grid%w_wif_dec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16710,&
'frame/module_domain.f: Failed to deallocate grid%w_wif_dec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_now ) ) THEN 
  DEALLOCATE(grid%i_wif_now,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16717,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_now. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_jan ) ) THEN 
  DEALLOCATE(grid%i_wif_jan,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16724,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_jan. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_feb ) ) THEN 
  DEALLOCATE(grid%i_wif_feb,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16731,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_feb. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_mar ) ) THEN 
  DEALLOCATE(grid%i_wif_mar,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16738,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_mar. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_apr ) ) THEN 
  DEALLOCATE(grid%i_wif_apr,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16745,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_apr. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_may ) ) THEN 
  DEALLOCATE(grid%i_wif_may,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16752,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_may. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_jun ) ) THEN 
  DEALLOCATE(grid%i_wif_jun,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16759,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_jun. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_jul ) ) THEN 
  DEALLOCATE(grid%i_wif_jul,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16766,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_jul. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_aug ) ) THEN 
  DEALLOCATE(grid%i_wif_aug,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16773,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_aug. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_sep ) ) THEN 
  DEALLOCATE(grid%i_wif_sep,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16780,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_sep. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_oct ) ) THEN 
  DEALLOCATE(grid%i_wif_oct,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16787,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_oct. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_nov ) ) THEN 
  DEALLOCATE(grid%i_wif_nov,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16794,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_nov. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%i_wif_dec ) ) THEN 
  DEALLOCATE(grid%i_wif_dec,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16801,&
'frame/module_domain.f: Failed to deallocate grid%i_wif_dec. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sealevelp ) ) THEN 
  DEALLOCATE(grid%sealevelp,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16808,&
'frame/module_domain.f: Failed to deallocate grid%sealevelp. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%temperature ) ) THEN 
  DEALLOCATE(grid%temperature,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16815,&
'frame/module_domain.f: Failed to deallocate grid%temperature. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%geoheight ) ) THEN 
  DEALLOCATE(grid%geoheight,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16822,&
'frame/module_domain.f: Failed to deallocate grid%geoheight. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%pressure ) ) THEN 
  DEALLOCATE(grid%pressure,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16829,&
'frame/module_domain.f: Failed to deallocate grid%pressure. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%umet ) ) THEN 
  DEALLOCATE(grid%umet,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16836,&
'frame/module_domain.f: Failed to deallocate grid%umet. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%vmet ) ) THEN 
  DEALLOCATE(grid%vmet,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16843,&
'frame/module_domain.f: Failed to deallocate grid%vmet. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%speed ) ) THEN 
  DEALLOCATE(grid%speed,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16850,&
'frame/module_domain.f: Failed to deallocate grid%speed. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%dir ) ) THEN 
  DEALLOCATE(grid%dir,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16857,&
'frame/module_domain.f: Failed to deallocate grid%dir. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%landmask ) ) THEN 
  DEALLOCATE(grid%landmask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16864,&
'frame/module_domain.f: Failed to deallocate grid%landmask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%lakemask ) ) THEN 
  DEALLOCATE(grid%lakemask,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16871,&
'frame/module_domain.f: Failed to deallocate grid%lakemask. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sst ) ) THEN 
  DEALLOCATE(grid%sst,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16878,&
'frame/module_domain.f: Failed to deallocate grid%sst. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%sst_input ) ) THEN 
  DEALLOCATE(grid%sst_input,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16885,&
'frame/module_domain.f: Failed to deallocate grid%sst_input. ')
 endif
ENDIF
IF ( ASSOCIATED( grid%chem ) ) THEN 
  DEALLOCATE(grid%chem,STAT=ierr)
 if (ierr.ne.0) then
 CALL wrf_error_fatal3("<stdin>",16892,&
'frame/module_domain.f: Failed to deallocate grid%chem. ')
 endif
ENDIF


   END SUBROUTINE dealloc_space_field



   RECURSIVE SUBROUTINE find_grid_by_id ( id, in_grid, result_grid )
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: id
      TYPE(domain), POINTER     :: in_grid 
      TYPE(domain), POINTER     :: result_grid






      TYPE(domain), POINTER     :: grid_ptr
      INTEGER                   :: kid
      LOGICAL                   :: found
      found = .FALSE.
      NULLIFY(result_grid)
      IF ( ASSOCIATED( in_grid ) ) THEN
        IF ( in_grid%id .EQ. id ) THEN
           result_grid => in_grid
        ELSE
           grid_ptr => in_grid
           DO WHILE ( ASSOCIATED( grid_ptr ) .AND. .NOT. found )
              DO kid = 1, max_nests
                 IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) .AND. .NOT. found ) THEN
                    CALL find_grid_by_id ( id, grid_ptr%nests(kid)%ptr, result_grid )
                    IF ( ASSOCIATED( result_grid ) ) THEN
                      IF ( result_grid%id .EQ. id ) found = .TRUE.
                    ENDIF
                 ENDIF
              ENDDO
              IF ( .NOT. found ) grid_ptr => grid_ptr%sibling
           ENDDO
        ENDIF
      ENDIF
      RETURN
   END SUBROUTINE find_grid_by_id


   FUNCTION first_loc_integer ( array , search ) RESULT ( loc ) 
 
      IMPLICIT NONE

      

      INTEGER , INTENT(IN) , DIMENSION(:) :: array
      INTEGER , INTENT(IN)                :: search

      

      INTEGER                             :: loc






      
      

      INTEGER :: loop

      loc = -1
      find : DO loop = 1 , SIZE(array)
         IF ( search == array(loop) ) THEN         
            loc = loop
            EXIT find
         END IF
      END DO find

   END FUNCTION first_loc_integer

   SUBROUTINE init_module_domain
   END SUBROUTINE init_module_domain










      FUNCTION domain_get_current_time ( grid ) RESULT ( current_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: current_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, CurrTime=current_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",16999,&
            'domain_get_current_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_current_time


      FUNCTION domain_get_start_time ( grid ) RESULT ( start_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: start_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, StartTime=start_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",17019,&
            'domain_get_start_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_start_time


      FUNCTION domain_get_stop_time ( grid ) RESULT ( stop_time ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: stop_time
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, StopTime=stop_time, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",17039,&
            'domain_get_stop_time:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_stop_time


      FUNCTION domain_get_time_step ( grid ) RESULT ( time_step ) 
        IMPLICIT NONE




        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_TimeInterval) :: time_step
        
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, timeStep=time_step, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",17059,&
            'domain_get_time_step:  WRFU_ClockGet failed' )
        ENDIF
      END FUNCTION domain_get_time_step


      FUNCTION domain_get_advanceCount ( grid ) RESULT ( advanceCount ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        INTEGER :: advanceCount
        
        INTEGER(WRFU_KIND_I8) :: advanceCountLcl
        INTEGER :: rc
        CALL WRFU_ClockGet( grid%domain_clock, &
                            advanceCount=advanceCountLcl, &
                            rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",17082,&
            'domain_get_advanceCount:  WRFU_ClockGet failed' )
        ENDIF
        advanceCount = advanceCountLcl
      END FUNCTION domain_get_advanceCount


      SUBROUTINE domain_alarms_destroy ( grid )
        IMPLICIT NONE





        TYPE(domain), INTENT(INOUT) :: grid
        
        INTEGER                     :: alarmid

        IF ( ASSOCIATED( grid%alarms ) .AND. &
             ASSOCIATED( grid%alarms_created ) ) THEN
          DO alarmid = 1, MAX_WRF_ALARMS
            IF ( grid%alarms_created( alarmid ) ) THEN
              CALL WRFU_AlarmDestroy( grid%alarms( alarmid ) )
              grid%alarms_created( alarmid ) = .FALSE.
            ENDIF
          ENDDO
          DEALLOCATE( grid%alarms )
          NULLIFY( grid%alarms )
          DEALLOCATE( grid%alarms_created )
          NULLIFY( grid%alarms_created )
        ENDIF
      END SUBROUTINE domain_alarms_destroy


      SUBROUTINE domain_clock_destroy ( grid )
        IMPLICIT NONE




        TYPE(domain), INTENT(INOUT) :: grid
        IF ( ASSOCIATED( grid%domain_clock ) ) THEN
          IF ( grid%domain_clock_created ) THEN
            CALL WRFU_ClockDestroy( grid%domain_clock )
            grid%domain_clock_created = .FALSE.
          ENDIF
          DEALLOCATE( grid%domain_clock )
          NULLIFY( grid%domain_clock )
        ENDIF
      END SUBROUTINE domain_clock_destroy


      FUNCTION domain_last_time_step ( grid ) RESULT ( LAST_TIME ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: LAST_TIME
        LAST_TIME =   domain_get_stop_time( grid ) .EQ. &
                    ( domain_get_current_time( grid ) + &
                      domain_get_time_step( grid ) )
      END FUNCTION domain_last_time_step



      FUNCTION domain_clockisstoptime ( grid ) RESULT ( is_stop_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_stop_time
        INTEGER :: rc
        is_stop_time = WRFU_ClockIsStopTime( grid%domain_clock , rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",17164,&
            'domain_clockisstoptime:  WRFU_ClockIsStopTime() failed' )
        ENDIF
      END FUNCTION domain_clockisstoptime



      FUNCTION domain_clockisstopsubtime ( grid ) RESULT ( is_stop_subtime ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_stop_subtime
        INTEGER :: rc
        TYPE(WRFU_TimeInterval) :: timeStep
        TYPE(WRFU_Time) :: currentTime
        LOGICAL :: positive_timestep
        is_stop_subtime = .FALSE.
        CALL domain_clock_get( grid, time_step=timeStep, &
                                     current_time=currentTime )
        positive_timestep = ESMF_TimeIntervalIsPositive( timeStep )
        IF ( positive_timestep ) THEN


          IF ( ESMF_TimeGE( currentTime, grid%stop_subtime ) ) THEN
            is_stop_subtime = .TRUE.
          ENDIF
        ELSE


          IF ( ESMF_TimeLE( currentTime, grid%stop_subtime ) ) THEN
            is_stop_subtime = .TRUE.
          ENDIF
        ENDIF
      END FUNCTION domain_clockisstopsubtime




      FUNCTION domain_get_sim_start_time ( grid ) RESULT ( simulationStartTime ) 
        IMPLICIT NONE












        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_Time) :: simulationStartTime
        
        INTEGER :: rc
        INTEGER :: simulation_start_year,   simulation_start_month, &
                   simulation_start_day,    simulation_start_hour , &
                   simulation_start_minute, simulation_start_second
        CALL nl_get_simulation_start_year   ( 1, simulation_start_year   )
        CALL nl_get_simulation_start_month  ( 1, simulation_start_month  )
        CALL nl_get_simulation_start_day    ( 1, simulation_start_day    )
        CALL nl_get_simulation_start_hour   ( 1, simulation_start_hour   )
        CALL nl_get_simulation_start_minute ( 1, simulation_start_minute )
        CALL nl_get_simulation_start_second ( 1, simulation_start_second )
        CALL WRFU_TimeSet( simulationStartTime,       &
                           YY=simulation_start_year,  &
                           MM=simulation_start_month, &
                           DD=simulation_start_day,   &
                           H=simulation_start_hour,   &
                           M=simulation_start_minute, &
                           S=simulation_start_second, &
                           rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL nl_get_start_year   ( 1, simulation_start_year   )
          CALL nl_get_start_month  ( 1, simulation_start_month  )
          CALL nl_get_start_day    ( 1, simulation_start_day    )
          CALL nl_get_start_hour   ( 1, simulation_start_hour   )
          CALL nl_get_start_minute ( 1, simulation_start_minute )
          CALL nl_get_start_second ( 1, simulation_start_second )
          CALL wrf_debug( 150, "WARNING:  domain_get_sim_start_time using head_grid start time from namelist" )
          CALL WRFU_TimeSet( simulationStartTime,       &
                             YY=simulation_start_year,  &
                             MM=simulation_start_month, &
                             DD=simulation_start_day,   &
                             H=simulation_start_hour,   &
                             M=simulation_start_minute, &
                             S=simulation_start_second, &
                             rc=rc )
        ENDIF
        RETURN
      END FUNCTION domain_get_sim_start_time

      FUNCTION domain_get_time_since_sim_start ( grid ) RESULT ( time_since_sim_start ) 
        IMPLICIT NONE









        TYPE(domain), INTENT(IN) :: grid
        
        TYPE(WRFU_TimeInterval) :: time_since_sim_start
        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_simstarttime
        lcl_simstarttime = domain_get_sim_start_time( grid )
        lcl_currtime = domain_get_current_time ( grid )
        time_since_sim_start = lcl_currtime - lcl_simstarttime
      END FUNCTION domain_get_time_since_sim_start




      SUBROUTINE domain_clock_get( grid, current_time,                &
                                         current_timestr,             &
                                         current_timestr_frac,        &
                                         start_time, start_timestr,   &
                                         stop_time, stop_timestr,     &
                                         time_step, time_stepstr,     &
                                         time_stepstr_frac,           &
                                         advanceCount,                &
                                         currentDayOfYearReal,        &
                                         minutesSinceSimulationStart, &
                                         timeSinceSimulationStart,    &
                                         simulationStartTime,         &
                                         simulationStartTimeStr )
        IMPLICIT NONE
        TYPE(domain),            INTENT(IN)              :: grid
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: current_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: current_timestr
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: current_timestr_frac
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: start_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: start_timestr
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: stop_time
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: stop_timestr
        TYPE(WRFU_TimeInterval), INTENT(  OUT), OPTIONAL :: time_step
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: time_stepstr
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: time_stepstr_frac
        INTEGER,                 INTENT(  OUT), OPTIONAL :: advanceCount
        
        
        REAL,                    INTENT(  OUT), OPTIONAL :: currentDayOfYearReal
        
        
        TYPE(WRFU_Time),         INTENT(  OUT), OPTIONAL :: simulationStartTime
        CHARACTER (LEN=*),       INTENT(  OUT), OPTIONAL :: simulationStartTimeStr
        
        
        TYPE(WRFU_TimeInterval), INTENT(  OUT), OPTIONAL :: timeSinceSimulationStart
        
        REAL,                    INTENT(  OUT), OPTIONAL :: minutesSinceSimulationStart






        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_stoptime, lcl_starttime
        TYPE(WRFU_Time) :: lcl_simulationStartTime
        TYPE(WRFU_TimeInterval) :: lcl_time_step, lcl_timeSinceSimulationStart
        INTEGER :: days, seconds, Sn, Sd, rc
        CHARACTER (LEN=256) :: tmp_str
        CHARACTER (LEN=256) :: frac_str
        REAL(WRFU_KIND_R8) :: currentDayOfYearR8
        IF ( PRESENT( start_time ) ) THEN
          start_time = domain_get_start_time ( grid )
        ENDIF
        IF ( PRESENT( start_timestr ) ) THEN
          lcl_starttime = domain_get_start_time ( grid )
          CALL wrf_timetoa ( lcl_starttime, start_timestr )
        ENDIF
        IF ( PRESENT( time_step ) ) THEN
          time_step = domain_get_time_step ( grid )
        ENDIF
        IF ( PRESENT( time_stepstr ) ) THEN
          lcl_time_step = domain_get_time_step ( grid )
          CALL WRFU_TimeIntervalGet( lcl_time_step, &
                                     timeString=time_stepstr, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",17354,&
              'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_stepstr_frac ) ) THEN
          lcl_time_step = domain_get_time_step ( grid )
          CALL WRFU_TimeIntervalGet( lcl_time_step, timeString=tmp_str, &
                                     Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",17363,&
              'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          CALL fraction_to_string( Sn, Sd, frac_str )
          time_stepstr_frac = TRIM(tmp_str)//TRIM(frac_str)
        ENDIF
        IF ( PRESENT( advanceCount ) ) THEN
          advanceCount = domain_get_advanceCount ( grid )
        ENDIF
        
        
        
        
        
        
        IF ( PRESENT( current_time ) ) THEN
          current_time = domain_get_current_time ( grid )
        ENDIF
        IF ( PRESENT( current_timestr ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL wrf_timetoa ( lcl_currtime, current_timestr )
        ENDIF
        
        IF ( PRESENT( current_timestr_frac ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL wrf_timetoa ( lcl_currtime, tmp_str )
          CALL WRFU_TimeGet( lcl_currtime, Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",17391,&
              'domain_clock_get:  WRFU_TimeGet() failed' )
          ENDIF
          CALL fraction_to_string( Sn, Sd, frac_str )
          current_timestr_frac = TRIM(tmp_str)//TRIM(frac_str)
        ENDIF
        IF ( PRESENT( stop_time ) ) THEN
          stop_time = domain_get_stop_time ( grid )
        ENDIF
        IF ( PRESENT( stop_timestr ) ) THEN
          lcl_stoptime = domain_get_stop_time ( grid )
          CALL wrf_timetoa ( lcl_stoptime, stop_timestr )
        ENDIF
        IF ( PRESENT( currentDayOfYearReal ) ) THEN
          lcl_currtime = domain_get_current_time ( grid )
          CALL WRFU_TimeGet( lcl_currtime, dayOfYear_r8=currentDayOfYearR8, &
                             rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",17409,&
                   'domain_clock_get:  WRFU_TimeGet(dayOfYear_r8) failed' )
          ENDIF
          currentDayOfYearReal = REAL( currentDayOfYearR8 ) - 1.0
        ENDIF
        IF ( PRESENT( simulationStartTime ) ) THEN
          simulationStartTime = domain_get_sim_start_time( grid )
        ENDIF
        IF ( PRESENT( simulationStartTimeStr ) ) THEN
          lcl_simulationStartTime = domain_get_sim_start_time( grid )
          CALL wrf_timetoa ( lcl_simulationStartTime, simulationStartTimeStr )
        ENDIF
        IF ( PRESENT( timeSinceSimulationStart ) ) THEN
          timeSinceSimulationStart = domain_get_time_since_sim_start( grid )
        ENDIF
        IF ( PRESENT( minutesSinceSimulationStart ) ) THEN
          lcl_timeSinceSimulationStart = domain_get_time_since_sim_start( grid )
          CALL WRFU_TimeIntervalGet( lcl_timeSinceSimulationStart, &
                                     D=days, S=seconds, Sn=Sn, Sd=Sd, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",17429,&
                   'domain_clock_get:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          
          minutesSinceSimulationStart = ( REAL( days ) * 24. * 60. ) + &
                                        ( REAL( seconds ) / 60. )
          IF ( Sd /= 0 ) THEN
            minutesSinceSimulationStart = minutesSinceSimulationStart + &
                                          ( ( REAL( Sn ) / REAL( Sd ) ) / 60. )
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_clock_get

      FUNCTION domain_clockisstarttime ( grid ) RESULT ( is_start_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_start_time
        TYPE(WRFU_Time) :: start_time, current_time
        CALL domain_clock_get( grid, current_time=current_time, &
                                     start_time=start_time )
        is_start_time = ( current_time == start_time )
      END FUNCTION domain_clockisstarttime

      FUNCTION domain_clockissimstarttime ( grid ) RESULT ( is_sim_start_time ) 
        IMPLICIT NONE





        TYPE(domain), INTENT(IN) :: grid
        
        LOGICAL :: is_sim_start_time
        TYPE(WRFU_Time) :: simulationStartTime, current_time
        CALL domain_clock_get( grid, current_time=current_time, &
                                     simulationStartTime=simulationStartTime )
        is_sim_start_time = ( current_time == simulationStartTime )
      END FUNCTION domain_clockissimstarttime




      SUBROUTINE domain_clock_create( grid, StartTime, &
                                            StopTime,  &
                                            TimeStep )
        IMPLICIT NONE
        TYPE(domain),            INTENT(INOUT) :: grid
        TYPE(WRFU_Time),         INTENT(IN   ) :: StartTime
        TYPE(WRFU_Time),         INTENT(IN   ) :: StopTime
        TYPE(WRFU_TimeInterval), INTENT(IN   ) :: TimeStep





        
        INTEGER :: rc
        grid%domain_clock = WRFU_ClockCreate( TimeStep= TimeStep,  &
                                              StartTime=StartTime, &
                                              StopTime= StopTime,  &
                                              rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",17498,&
            'domain_clock_create:  WRFU_ClockCreate() failed' )
        ENDIF
        grid%domain_clock_created = .TRUE.
        RETURN
      END SUBROUTINE domain_clock_create



      SUBROUTINE domain_alarm_create( grid, alarm_id, interval, &
                                            begin_time, end_time )
        USE module_utility
        IMPLICIT NONE
        TYPE(domain), POINTER :: grid
        INTEGER, INTENT(IN) :: alarm_id
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: interval
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: begin_time
        TYPE(WRFU_TimeInterval), INTENT(IN), OPTIONAL :: end_time





        
        INTEGER :: rc




        LOGICAL :: interval_only, all_args, no_args
        TYPE(WRFU_Time) :: startTime
        interval_only = .FALSE.
        all_args = .FALSE.
        no_args = .FALSE.
        IF ( ( .NOT. PRESENT( begin_time ) ) .AND. &
             ( .NOT. PRESENT( end_time   ) ) .AND. &
             (       PRESENT( interval   ) ) ) THEN
           interval_only = .TRUE.
        ELSE IF ( ( .NOT. PRESENT( begin_time ) ) .AND. &
                  ( .NOT. PRESENT( end_time   ) ) .AND. &
                  ( .NOT. PRESENT( interval   ) ) ) THEN
           no_args = .TRUE.
        ELSE IF ( (       PRESENT( begin_time ) ) .AND. &
                  (       PRESENT( end_time   ) ) .AND. &
                  (       PRESENT( interval   ) ) ) THEN
           all_args = .TRUE.
        ELSE
           CALL wrf_error_fatal3("<stdin>",17545,&
             'ERROR in domain_alarm_create:  bad argument list' )
        ENDIF
        CALL domain_clock_get( grid, start_time=startTime )
        IF ( interval_only ) THEN
           grid%io_intervals( alarm_id ) = interval
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock, &
                               RingInterval=interval,   &
                               rc=rc )
        ELSE IF ( no_args ) THEN
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock, &
                               RingTime=startTime,      &
                               rc=rc )
        ELSE IF ( all_args ) THEN
           grid%io_intervals( alarm_id ) = interval
           grid%alarms( alarm_id ) = &
             WRFU_AlarmCreate( clock=grid%domain_clock,         &
                               RingTime=startTime + begin_time, &
                               RingInterval=interval,           &
                               StopTime=startTime + end_time,   &
                               rc=rc )
        ENDIF
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",17570,&
            'domain_alarm_create:  WRFU_AlarmCreate() failed' )
        ENDIF
        CALL WRFU_AlarmRingerOff( grid%alarms( alarm_id ) , rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",17575,&
            'domain_alarm_create:  WRFU_AlarmRingerOff() failed' )
        ENDIF
        grid%alarms_created( alarm_id ) = .TRUE.
      END SUBROUTINE domain_alarm_create



      SUBROUTINE domain_clock_set( grid, current_timestr, &
                                         stop_timestr,    &
                                         time_step_seconds )
        IMPLICIT NONE
        TYPE(domain),      INTENT(INOUT)           :: grid
        CHARACTER (LEN=*), INTENT(IN   ), OPTIONAL :: current_timestr
        CHARACTER (LEN=*), INTENT(IN   ), OPTIONAL :: stop_timestr
        INTEGER,           INTENT(IN   ), OPTIONAL :: time_step_seconds






        
        TYPE(WRFU_Time) :: lcl_currtime, lcl_stoptime
        TYPE(WRFU_TimeInterval) :: tmpTimeInterval
        INTEGER :: rc
        IF ( PRESENT( current_timestr ) ) THEN
          CALL wrf_atotime( current_timestr(1:19), lcl_currtime )
          CALL WRFU_ClockSet( grid%domain_clock, currTime=lcl_currtime, &
                              rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",17606,&
              'domain_clock_set:  WRFU_ClockSet(CurrTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( stop_timestr ) ) THEN
          CALL wrf_atotime( stop_timestr(1:19), lcl_stoptime )
          CALL WRFU_ClockSet( grid%domain_clock, stopTime=lcl_stoptime, &
                              rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",17615,&
              'domain_clock_set:  WRFU_ClockSet(StopTime) failed' )
          ENDIF
        ENDIF
        IF ( PRESENT( time_step_seconds ) ) THEN
          CALL WRFU_TimeIntervalSet( tmpTimeInterval, &
                                     S=time_step_seconds, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",17623,&
              'domain_clock_set:  WRFU_TimeIntervalSet failed' )
          ENDIF
          CALL WRFU_ClockSet ( grid%domain_clock,        &
                               timeStep=tmpTimeInterval, &
                               rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",17630,&
              'domain_clock_set:  WRFU_ClockSet(TimeStep) failed' )
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_clock_set


      
      
      SUBROUTINE domain_clockprint ( level, grid, pre_str )
        IMPLICIT NONE
        INTEGER,           INTENT( IN) :: level
        TYPE(domain),      INTENT( IN) :: grid
        CHARACTER (LEN=*), INTENT( IN) :: pre_str
        CALL wrf_clockprint ( level, grid%domain_clock, pre_str )
        RETURN
      END SUBROUTINE domain_clockprint


      
      
      SUBROUTINE domain_clockadvance ( grid )
        IMPLICIT NONE
        TYPE(domain), INTENT(INOUT) :: grid
        INTEGER :: rc
        CALL domain_clockprint ( 250, grid, &
          'DEBUG domain_clockadvance():  before WRFU_ClockAdvance,' )
        CALL WRFU_ClockAdvance( grid%domain_clock, rc=rc )
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",17660,&
            'domain_clockadvance:  WRFU_ClockAdvance() failed' )
        ENDIF
        CALL domain_clockprint ( 250, grid, &
          'DEBUG domain_clockadvance():  after WRFU_ClockAdvance,' )
        
        
        CALL domain_clock_get( grid, minutesSinceSimulationStart=grid%xtime )
        CALL domain_clock_get( grid, currentDayOfYearReal=grid%julian )
        RETURN
      END SUBROUTINE domain_clockadvance



      
      
      SUBROUTINE domain_setgmtetc ( grid, start_of_simulation )
        IMPLICIT NONE
        TYPE (domain), INTENT(INOUT) :: grid
        LOGICAL,       INTENT(  OUT) :: start_of_simulation
        
        CHARACTER (LEN=132)          :: message
        TYPE(WRFU_Time)              :: simStartTime
        INTEGER                      :: hr, mn, sec, ms, rc
        CALL domain_clockprint(150, grid, &
          'DEBUG domain_setgmtetc():  get simStartTime from clock,')
        CALL domain_clock_get( grid, simulationStartTime=simStartTime, &
                                     simulationStartTimeStr=message )
        CALL WRFU_TimeGet( simStartTime, YY=grid%julyr, dayOfYear=grid%julday, &
                           H=hr, M=mn, S=sec, MS=ms, rc=rc)
        IF ( rc /= WRFU_SUCCESS ) THEN
          CALL wrf_error_fatal3("<stdin>",17691,&
            'domain_setgmtetc:  WRFU_TimeGet() failed' )
        ENDIF
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  simulation start time = [',TRIM( message ),']'
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        grid%gmt=hr+real(mn)/60.+real(sec)/3600.+real(ms)/(1000*3600)
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  julyr,hr,mn,sec,ms,julday = ', &
                                     grid%julyr,hr,mn,sec,ms,grid%julday
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        WRITE( wrf_err_message , * ) 'DEBUG domain_setgmtetc():  gmt = ',grid%gmt
        CALL wrf_debug( 150, TRIM(wrf_err_message) )
        start_of_simulation = domain_ClockIsSimStartTime(grid)
        RETURN
      END SUBROUTINE domain_setgmtetc
     


      
      
      SUBROUTINE set_current_grid_ptr( grid_ptr )
        IMPLICIT NONE
        TYPE(domain), POINTER :: grid_ptr






        current_grid_set = .TRUE.
        current_grid => grid_ptr

      END SUBROUTINE set_current_grid_ptr








      LOGICAL FUNCTION Is_alarm_tstep( grid_clock, alarm )

        IMPLICIT NONE

        TYPE (WRFU_Clock), INTENT(in)  :: grid_clock
        TYPE (WRFU_Alarm), INTENT(in)  :: alarm

        LOGICAL :: pred1, pred2, pred3

        Is_alarm_tstep = .FALSE.

        IF ( ASSOCIATED( alarm%alarmint ) ) THEN
          IF ( alarm%alarmint%Enabled ) THEN
            IF ( alarm%alarmint%RingIntervalSet ) THEN
              pred1 = .FALSE. ; pred2 = .FALSE. ; pred3 = .FALSE.
              IF ( alarm%alarmint%StopTimeSet ) THEN
                 PRED1 = ( grid_clock%clockint%CurrTime + grid_clock%clockint%TimeStep > &
                      alarm%alarmint%StopTime )
              ENDIF
              IF ( alarm%alarmint%RingTimeSet ) THEN
                 PRED2 = ( ( alarm%alarmint%RingTime - &
                      grid_clock%clockint%TimeStep <= &
                      grid_clock%clockint%CurrTime )     &
                      .AND. ( grid_clock%clockint%CurrTime < alarm%alarmint%RingTime ) )
              ENDIF
              IF ( alarm%alarmint%RingIntervalSet ) THEN
                 PRED3 = ( alarm%alarmint%PrevRingTime + &
                      alarm%alarmint%RingInterval <= &
                      grid_clock%clockint%CurrTime + grid_clock%clockint%TimeStep )
              ENDIF
              IF ( ( .NOT. ( pred1 ) ) .AND. &
                   ( ( pred2 ) .OR. ( pred3 ) ) ) THEN
                 Is_alarm_tstep = .TRUE.
              ENDIF
            ELSE IF ( alarm%alarmint%RingTimeSet ) THEN
              IF ( alarm%alarmint%RingTime -&
                   grid_clock%clockint%TimeStep <= &
                   grid_clock%clockint%CurrTime ) THEN
                 Is_alarm_tstep = .TRUE.
              ENDIF
            ENDIF
          ENDIF
        ENDIF

      END FUNCTION Is_alarm_tstep










      
      SUBROUTINE domain_time_test_print ( pre_str, name_str, res_str )
        IMPLICIT NONE
        CHARACTER (LEN=*), INTENT(IN) :: pre_str
        CHARACTER (LEN=*), INTENT(IN) :: name_str
        CHARACTER (LEN=*), INTENT(IN) :: res_str
        CHARACTER (LEN=512) :: out_str
        WRITE (out_str,                                            &
          FMT="('DOMAIN_TIME_TEST ',A,':  ',A,' = ',A)") &
          TRIM(pre_str), TRIM(name_str), TRIM(res_str)
        CALL wrf_debug( 0, TRIM(out_str) )
      END SUBROUTINE domain_time_test_print

      
      SUBROUTINE test_adjust_io_timestr( TI_h, TI_m, TI_s, &
        CT_yy,  CT_mm,  CT_dd,  CT_h,  CT_m,  CT_s,        &
        ST_yy,  ST_mm,  ST_dd,  ST_h,  ST_m,  ST_s,        &
        res_str, testname )
        INTEGER, INTENT(IN) :: TI_H
        INTEGER, INTENT(IN) :: TI_M
        INTEGER, INTENT(IN) :: TI_S
        INTEGER, INTENT(IN) :: CT_YY
        INTEGER, INTENT(IN) :: CT_MM  
        INTEGER, INTENT(IN) :: CT_DD  
        INTEGER, INTENT(IN) :: CT_H
        INTEGER, INTENT(IN) :: CT_M
        INTEGER, INTENT(IN) :: CT_S
        INTEGER, INTENT(IN) :: ST_YY
        INTEGER, INTENT(IN) :: ST_MM  
        INTEGER, INTENT(IN) :: ST_DD  
        INTEGER, INTENT(IN) :: ST_H
        INTEGER, INTENT(IN) :: ST_M
        INTEGER, INTENT(IN) :: ST_S
        CHARACTER (LEN=*), INTENT(IN) :: res_str
        CHARACTER (LEN=*), INTENT(IN) :: testname
        
        TYPE(WRFU_TimeInterval) :: TI
        TYPE(WRFU_Time) :: CT, ST
        LOGICAL :: test_passed
        INTEGER :: rc
        CHARACTER(LEN=WRFU_MAXSTR) :: TI_str, CT_str, ST_str, computed_str
        
        CALL WRFU_TimeIntervalSet( TI, H=TI_H, M=TI_M, S=TI_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeIntervalSet() ', &
                              "module_domain.F" , &
                              2750  )
        CALL WRFU_TimeIntervalGet( TI, timeString=TI_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              2755  )
        
        CALL WRFU_TimeSet( CT, YY=CT_YY, MM=CT_MM, DD=CT_DD , &
                                H=CT_H,   M=CT_M,   S=CT_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.F" , &
                              2762  )
        CALL WRFU_TimeGet( CT, timeString=CT_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              2767  )
        
        CALL WRFU_TimeSet( ST, YY=ST_YY, MM=ST_MM, DD=ST_DD , &
                                H=ST_H,   M=ST_M,   S=ST_S, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeSet() ', &
                              "module_domain.F" , &
                              2774  )
        CALL WRFU_TimeGet( ST, timeString=ST_str, rc=rc )
        CALL wrf_check_error( WRFU_SUCCESS, rc, &
                              'FAIL:  '//TRIM(testname)//'WRFU_TimeGet() ', &
                              "module_domain.F" , &
                              2779  )
        
        CALL adjust_io_timestr ( TI, CT, ST, computed_str )
        
        test_passed = .FALSE.
        IF ( LEN_TRIM(res_str) == LEN_TRIM(computed_str) ) THEN
          IF ( res_str(1:LEN_TRIM(res_str)) == computed_str(1:LEN_TRIM(computed_str)) ) THEN
            test_passed = .TRUE.
          ENDIF
        ENDIF
        
        IF ( test_passed ) THEN
          WRITE(*,FMT='(A)') 'PASS:  '//TRIM(testname)
        ELSE
          WRITE(*,*) 'FAIL:  ',TRIM(testname),':  adjust_io_timestr(',    &
            TRIM(TI_str),',',TRIM(CT_str),',',TRIM(ST_str),')  expected <', &
            TRIM(res_str),'>  but computed <',TRIM(computed_str),'>'
        ENDIF
      END SUBROUTINE test_adjust_io_timestr

      
      
      
      
      
      SUBROUTINE domain_time_test ( grid, pre_str )
        IMPLICIT NONE
        TYPE(domain),      INTENT(IN) :: grid
        CHARACTER (LEN=*), INTENT(IN) :: pre_str
        
        LOGICAL, SAVE :: one_time_tests_done = .FALSE.
        REAL :: minutesSinceSimulationStart
        INTEGER :: advance_count, rc
        REAL :: currentDayOfYearReal
        TYPE(WRFU_TimeInterval) :: timeSinceSimulationStart
        TYPE(WRFU_Time) :: simulationStartTime
        CHARACTER (LEN=512) :: res_str
        LOGICAL :: self_test_domain
        
        
        
        
        
        
        CALL nl_get_self_test_domain( 1, self_test_domain )
        IF ( self_test_domain ) THEN
          CALL domain_clock_get( grid, advanceCount=advance_count )
          WRITE ( res_str, FMT="(I8.8)" ) advance_count
          CALL domain_time_test_print( pre_str, 'advanceCount', res_str )
          CALL domain_clock_get( grid, currentDayOfYearReal=currentDayOfYearReal )
          WRITE ( res_str, FMT='(F10.6)' ) currentDayOfYearReal
          CALL domain_time_test_print( pre_str, 'currentDayOfYearReal', res_str )
          CALL domain_clock_get( grid, minutesSinceSimulationStart=minutesSinceSimulationStart )
          WRITE ( res_str, FMT='(F10.6)' ) minutesSinceSimulationStart
          CALL domain_time_test_print( pre_str, 'minutesSinceSimulationStart', res_str )
          CALL domain_clock_get( grid, current_timestr=res_str )
          CALL domain_time_test_print( pre_str, 'current_timestr', res_str )
          CALL domain_clock_get( grid, current_timestr_frac=res_str )
          CALL domain_time_test_print( pre_str, 'current_timestr_frac', res_str )
          CALL domain_clock_get( grid, timeSinceSimulationStart=timeSinceSimulationStart )
          CALL WRFU_TimeIntervalGet( timeSinceSimulationStart, timeString=res_str, rc=rc )
          IF ( rc /= WRFU_SUCCESS ) THEN
            CALL wrf_error_fatal3("<stdin>",17923,&
              'domain_time_test:  WRFU_TimeIntervalGet() failed' )
          ENDIF
          CALL domain_time_test_print( pre_str, 'timeSinceSimulationStart', res_str )
          
          
          IF ( .NOT. one_time_tests_done ) THEN
            one_time_tests_done = .TRUE.
            CALL domain_clock_get( grid, simulationStartTimeStr=res_str )
            CALL domain_time_test_print( pre_str, 'simulationStartTime', res_str )
            CALL domain_clock_get( grid, start_timestr=res_str )
            CALL domain_time_test_print( pre_str, 'start_timestr', res_str )
            CALL domain_clock_get( grid, stop_timestr=res_str )
            CALL domain_time_test_print( pre_str, 'stop_timestr', res_str )
            CALL domain_clock_get( grid, time_stepstr=res_str )
            CALL domain_time_test_print( pre_str, 'time_stepstr', res_str )
            CALL domain_clock_get( grid, time_stepstr_frac=res_str )
            CALL domain_time_test_print( pre_str, 'time_stepstr_frac', res_str )
            
            
            
            
            
            
            CALL test_adjust_io_timestr( TI_h=3, TI_m=0, TI_s=0,          &
              CT_yy=2000,  CT_mm=1,  CT_dd=26,  CT_h=0,  CT_m=0,  CT_s=0, &
              ST_yy=2000,  ST_mm=1,  ST_dd=24,  ST_h=12, ST_m=0,  ST_s=0, &
              res_str='2000-01-26_00:00:00', testname='adjust_io_timestr_1' )
            
            
            
            
            
          ENDIF
        ENDIF
        RETURN
      END SUBROUTINE domain_time_test






END MODULE module_domain









SUBROUTINE get_current_time_string( time_str )
  USE module_domain
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(OUT) :: time_str
  
  INTEGER :: debug_level_lcl

  time_str = ''
  IF ( current_grid_set ) THEN








    IF ( current_grid%time_set ) THEN

      
      CALL get_wrf_debug_level( debug_level_lcl )
      CALL set_wrf_debug_level ( 0 )
      current_grid_set = .FALSE.
      CALL domain_clock_get( current_grid, current_timestr_frac=time_str )
      
      CALL set_wrf_debug_level ( debug_level_lcl )
      current_grid_set = .TRUE.

    ENDIF
  ENDIF

END SUBROUTINE get_current_time_string






SUBROUTINE get_current_grid_name( grid_str )
  USE module_domain
  IMPLICIT NONE
  CHARACTER (LEN=*), INTENT(OUT) :: grid_str
  grid_str = ''
  IF ( current_grid_set ) THEN
    WRITE(grid_str,FMT="('d',I2.2)") current_grid%id
  ENDIF
END SUBROUTINE get_current_grid_name




   SUBROUTINE get_ijk_from_grid_ext (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
    USE module_domain
    IMPLICIT NONE
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey

     CALL get_ijk_from_grid2 (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe )
     data_ordering : SELECT CASE ( model_data_order )
       CASE  ( DATA_ORDER_XYZ )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_YXZ )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm33x ; kmex = grid%em33x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp33x ; kpex = grid%ep33x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm33y ; kmey = grid%em33y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp33y ; kpey = grid%ep33y ;
       CASE  ( DATA_ORDER_ZXY )
           imsx = grid%sm32x ; imex = grid%em32x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp32x ; ipex = grid%ep32x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm32y ; imey = grid%em32y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp32y ; ipey = grid%ep32y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_ZYX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm32x ; jmex = grid%em32x ; kmsx = grid%sm31x ; kmex = grid%em31x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp32x ; jpex = grid%ep32x ; kpsx = grid%sp31x ; kpex = grid%ep31x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm32y ; jmey = grid%em32y ; kmsy = grid%sm31y ; kmey = grid%em31y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp32y ; jpey = grid%ep32y ; kpsy = grid%sp31y ; kpey = grid%ep31y ;
       CASE  ( DATA_ORDER_XZY )
           imsx = grid%sm31x ; imex = grid%em31x ; jmsx = grid%sm33x ; jmex = grid%em33x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp31x ; ipex = grid%ep31x ; jpsx = grid%sp33x ; jpex = grid%ep33x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm31y ; imey = grid%em31y ; jmsy = grid%sm33y ; jmey = grid%em33y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp31y ; ipey = grid%ep31y ; jpsy = grid%sp33y ; jpey = grid%ep33y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
       CASE  ( DATA_ORDER_YZX )
           imsx = grid%sm33x ; imex = grid%em33x ; jmsx = grid%sm31x ; jmex = grid%em31x ; kmsx = grid%sm32x ; kmex = grid%em32x ;
           ipsx = grid%sp33x ; ipex = grid%ep33x ; jpsx = grid%sp31x ; jpex = grid%ep31x ; kpsx = grid%sp32x ; kpex = grid%ep32x ;
           imsy = grid%sm33y ; imey = grid%em33y ; jmsy = grid%sm31y ; jmey = grid%em31y ; kmsy = grid%sm32y ; kmey = grid%em32y ;
           ipsy = grid%sp33y ; ipey = grid%ep33y ; jpsy = grid%sp31y ; jpey = grid%ep31y ; kpsy = grid%sp32y ; kpey = grid%ep32y ;
     END SELECT data_ordering
   END SUBROUTINE get_ijk_from_grid_ext




   SUBROUTINE get_ijk_from_subgrid_ext (  grid ,                &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0    )
    USE module_domain
    IMPLICIT NONE
    TYPE( domain ), INTENT (IN)  :: grid
    INTEGER, INTENT(OUT) ::                                 &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0
   
    INTEGER              ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe
     CALL get_ijk_from_grid (  grid ,                         &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )
     ids0 = ids
     ide0 = ide * grid%sr_x
     ims0 = (ims-1)*grid%sr_x+1
     ime0 = ime * grid%sr_x
     ips0 = (ips-1)*grid%sr_x+1
     ipe0 = ipe * grid%sr_x

     jds0 = jds
     jde0 = jde * grid%sr_y
     jms0 = (jms-1)*grid%sr_y+1
     jme0 = jme * grid%sr_y
     jps0 = (jps-1)*grid%sr_y+1
     jpe0 = jpe * grid%sr_y

     kds0 = kds
     kde0 = kde
     kms0 = kms
     kme0 = kme
     kps0 = kps
     kpe0 = kpe
   RETURN
   END SUBROUTINE get_ijk_from_subgrid_ext


   SUBROUTINE get_dims_from_grid_id (  id   &
                          ,ds, de           &
                          ,ms, me           &
                          ,ps, pe           &
                          ,mxs, mxe         &
                          ,pxs, pxe         &
                          ,mys, mye         &
                          ,pys, pye )
    USE module_domain, ONLY : domain, head_grid, find_grid_by_id
    IMPLICIT NONE
    TYPE( domain ), POINTER  :: grid
    INTEGER, INTENT(IN ) :: id
    INTEGER, DIMENSION(3), INTENT(INOUT) ::                   &
                           ds, de           &
                          ,ms, me           &
                          ,ps, pe           &
                          ,mxs, mxe         &
                          ,pxs, pxe         &
                          ,mys, mye         &
                          ,pys, pye

     
     CHARACTER*256 mess

     NULLIFY( grid )
     CALL find_grid_by_id ( id, head_grid, grid )

     IF ( ASSOCIATED(grid) ) THEN
           ds(1) = grid%sd31 ; de(1) = grid%ed31 ; ds(2) = grid%sd32 ; de(2) = grid%ed32 ; ds(3) = grid%sd33 ; de(3) = grid%ed33 ;
           ms(1) = grid%sm31 ; me(1) = grid%em31 ; ms(2) = grid%sm32 ; me(2) = grid%em32 ; ms(3) = grid%sm33 ; me(3) = grid%em33 ;
           ps(1) = grid%sp31 ; pe(1) = grid%ep31 ; ps(2) = grid%sp32 ; pe(2) = grid%ep32 ; ps(3) = grid%sp33 ; pe(3) = grid%ep33 ;
           mxs(1) = grid%sm31x ; mxe(1) = grid%em31x 
           mxs(2) = grid%sm32x ; mxe(2) = grid%em32x 
           mxs(3) = grid%sm33x ; mxe(3) = grid%em33x 
           pxs(1) = grid%sp31x ; pxe(1) = grid%ep31x 
           pxs(2) = grid%sp32x ; pxe(2) = grid%ep32x 
           pxs(3) = grid%sp33x ; pxe(3) = grid%ep33x
           mys(1) = grid%sm31y ; mye(1) = grid%em31y 
           mys(2) = grid%sm32y ; mye(2) = grid%em32y 
           mys(3) = grid%sm33y ; mye(3) = grid%em33y 
           pys(1) = grid%sp31y ; pye(1) = grid%ep31y 
           pys(2) = grid%sp32y ; pye(2) = grid%ep32y 
           pys(3) = grid%sp33y ; pye(3) = grid%ep33y 
     ELSE
        WRITE(mess,*)'internal error: get_ijk_from_grid_id: no such grid id:',id
        CALL wrf_error_fatal3("<stdin>",18177,&
TRIM(mess))
     ENDIF

   END SUBROUTINE get_dims_from_grid_id


   SUBROUTINE get_ijk_from_grid_id (  id ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
    USE module_domain, ONLY : domain, head_grid, find_grid_by_id, get_ijk_from_grid
    IMPLICIT NONE
    TYPE( domain ), POINTER  :: grid
    INTEGER, INTENT(IN ) :: id
    INTEGER, INTENT(OUT) ::                                 &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey
     
     CHARACTER*256 mess

     NULLIFY( grid )
     CALL find_grid_by_id ( id, head_grid, grid )

     IF ( ASSOCIATED(grid) ) THEN
     CALL get_ijk_from_grid (  grid ,                   &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           ips, ipe, jps, jpe, kps, kpe,    &
                           imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )
     ELSE
        WRITE(mess,*)'internal error: get_ijk_from_grid_id: no such grid id:',id
        CALL wrf_error_fatal3("<stdin>",18221,&
TRIM(mess))
     ENDIF

   END SUBROUTINE get_ijk_from_grid_id



   SUBROUTINE modify_io_masks ( id )
     USE module_domain, ONLY : domain, modify_io_masks1, head_grid, find_grid_by_id
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: id
     TYPE(domain), POINTER :: grid
     CALL find_grid_by_id( id, head_grid, grid )
     IF ( ASSOCIATED( grid ) ) CALL modify_io_masks1( grid, id ) 
     RETURN 
   END SUBROUTINE modify_io_masks
