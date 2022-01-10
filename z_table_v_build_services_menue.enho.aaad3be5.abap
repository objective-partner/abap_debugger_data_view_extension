"Name: \TY:CL_TPDA_TOOL_TABLE_NEW\ME:HANDLE_OK_CODE\SE:BEGIN\EI
ENHANCEMENT 0 Z_TABLE_V_BUILD_SERVICES_MENUE.
    IF i_action EQ 'ZD' AND i_subaction EQ 'ATA_4_ABAP'.
      FIELD-SYMBOLS: <zz_table> TYPE ANY TABLE.
      zcl_op_debugger_integration=>debug_debugger_if_needed( ).
      TRY.
        TRY.
          DATA(zz_reference_to_data) = NEW zcl_op_debugger_integration( )->get_ref_to_any_content( i_variable_name = dynp_vars-table_name ).
          DATA(zz_field_catalog) = NEW zcl_op_simple_field_catalog( )->get_by_reference( zz_reference_to_data ).
          ASSIGN zz_reference_to_data->* TO <zz_table>. "de-referencing
        CATCH cx_root INTO DATA(lx_root).
          "could be a header table, try it a different way
          create_real_clone( IMPORTING e_rda_table = DATA(l_zz_rda_table)
                e_tab_fcat  = zz_field_catalog ).
          ASSIGN l_zz_rda_table->* TO <zz_table>.
        ENDTRY.

        NEW zcl_op_table( )->show_popup_w_content(
        EXPORTING
          i_table        =  <zz_table>
          i_fieldcatalog =  zz_field_catalog
          i_table_title  =  me->dynp_vars-table_name  ).

        RETURN.
      CATCH cx_root INTO lx_root.
        "dont want to crash, so catch all catchable exceptions here
        zcl_op_debugger_integration=>debug_debugger_if_needed( ).
      ENDTRY.
    ENDIF.
ENDENHANCEMENT.
