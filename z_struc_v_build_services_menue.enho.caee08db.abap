"Name: \TY:CL_TPDA_TOOL_STRUC_VIEW\IN:IF_TPDA_TOOL\ME:HANDLE_OK_CODE\SE:BEGIN\EI
ENHANCEMENT 0 Z_STRUC_V_BUILD_SERVICES_MENUE.
 IF p_ok_code-ok_code EQ 'ZDATA_4_ABAP'.
   zcl_op_debugger_integration=>debug_debugger_if_needed( ).
   TRY.
     DATA(zz_reference_to_data) = NEW zcl_op_debugger_integration( )->get_ref_to_any_content( i_variable_name = dynp_vars-struc_name ).

     DATA(zz_field_catalog) = NEW zcl_op_simple_field_catalog( )->get_by_reference( zz_reference_to_data ).

     FIELD-SYMBOLS: <zz_structure> TYPE any.
     ASSIGN zz_reference_to_data->* TO <zz_structure>. "de-referencing

     NEW zcl_op_structure( )->show_popup_w_content( i_structure     = <zz_structure>
                                                    i_field_catalog = zz_field_catalog
                                                    i_struc_name    = dynp_vars-struc_name ).
   CATCH cx_root into DATA(lx_root).
     "dont want to crash, so catch all catchable exceptions here

   ENDTRY.

   RETURN.
 ENDIF.
ENDENHANCEMENT.
