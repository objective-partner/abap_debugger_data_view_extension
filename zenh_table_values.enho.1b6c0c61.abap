"Name: \PR:SAPLSTPDA_TABLE_VALUES\TY:LCL_TAB\ME:INIT_ALV\SE:END\EI
ENHANCEMENT 0 ZENH_TABLE_VALUES.
   "instantiate custom event reciever
   mo_cust_rec  = NEW zcl_op_table( ).

   "set reciever as handler for Debugger Table ALV
   SET HANDLER mo_cust_rec->handle_toolbar_set  FOR me->ref_alv.

   " need to refresh alv here
   refresh_alv( ).
ENDENHANCEMENT.
