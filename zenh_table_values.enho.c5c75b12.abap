"Name: \PR:SAPLSTPDA_TABLE_VALUES\TY:LCL_TAB\ME:HANDLE_USER_COMMAND\SE:BEGIN\EI
ENHANCEMENT 0 ZENH_TABLE_VALUES.
  CASE e_ucomm.
    WHEN 'ZDATA_4_ABAP_VIEW'. "user klicked on Button to display ABAP friendly table data
      FIELD-SYMBOLS: <zz_table> TYPE ANY TABLE.
      zcl_op_debugger_integration=>debug_debugger_if_needed( ).
      TRY.
          ref_alv->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(zz_field_catalog) ).
          " field INDEX does not actually exist in table and is not useful in VALUE statement
          DELETE zz_field_catalog WHERE fieldname = 'INDEX'.
          ASSIGN me->rda_table->* TO <zz_table>. "get current table content
          DATA(zz_filtered_table) = mo_cust_rec->filter_table_from_alv( i_alv = ref_alv
                                                                        i_table = <zz_table> ).
          ASSIGN zz_filtered_table->* TO <zz_table>.

          mo_cust_rec->show_popup_w_content(
              EXPORTING
                i_table        = <zz_table>
                i_table_title  = me->table
                i_fieldcatalog = zz_field_catalog ).
        CATCH cx_root INTO DATA(zzlx_root).
          "dont want to crash, so catch all catchable exceptions here
      ENDTRY.
  ENDCASE.
ENDENHANCEMENT.
