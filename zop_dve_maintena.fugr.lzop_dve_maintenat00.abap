*---------------------------------------------------------------------*
*    view related data declarations
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
