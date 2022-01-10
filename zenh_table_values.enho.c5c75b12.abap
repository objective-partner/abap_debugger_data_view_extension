"Name: \PR:SAPLSTPDA_TABLE_VALUES\TY:LCL_TAB\ME:HANDLE_USER_COMMAND\SE:BEGIN\EI
ENHANCEMENT 0 ZENH_TABLE_VALUES.
  CASE e_ucomm.
      WHEN 'ZDATA_4_ABAP_VIEW'. "user klicked on Button to display ABAP friendly table data
       FIELD-SYMBOLS: <zz_table> TYPE any table.
       zcl_op_debugger_integration=>debug_debugger_if_needed( ).
       TRY.
         TRY.
            DATA(zz_reference_to_data) = NEW zcl_op_debugger_integration( )->get_ref_to_any_content( i_variable_name = me->table ).
            ASSIGN zz_reference_to_data->* TO <zz_table>. "de-referencing
            DATA(zz_field_catalog) = NEW zcl_op_simple_field_catalog( )->get_by_reference( zz_reference_to_data ).
         CATCH cx_root INTO DATA(lx_root).
            "could be a header table, lets try another method
            ASSIGN me->rda_table->* TO <zz_table>. "get current table content
            zz_field_catalog = me->tab_fcat.
         ENDTRY.

          mo_cust_rec->show_popup_w_content(
              EXPORTING
                i_table        = <zz_table>
                i_table_title  = me->table
                i_fieldcatalog = zz_field_catalog ).
         CATCH cx_root INTO lx_root.
           "dont want to crash, so catch all catchable exceptions here
       ENDTRY.
  ENDCASE.
ENDENHANCEMENT.
