*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZOP_DVE_MAINTENA
*   generation date: 27.11.2018 at 23:27:51 by user AGEPPART
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZOP_DVE_MAINTENA   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
