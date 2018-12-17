*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 27.11.2018 at 23:27:52 by user AGEPPART
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTOP_DVE_CUST...................................*
DATA:  BEGIN OF STATUS_ZTOP_DVE_CUST                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTOP_DVE_CUST                 .
CONTROLS: TCTRL_ZTOP_DVE_CUST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTOP_DVE_CUST                 .
TABLES: ZTOP_DVE_CUST                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
