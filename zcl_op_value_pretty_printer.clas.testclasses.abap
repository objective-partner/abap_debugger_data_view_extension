*"* use this source file for your ABAP unit test classes
CLASS ltcl_pretty_printer_should DEFINITION DEFERRED.
CLASS ltcl_pretty_printer_performanc DEFINITION DEFERRED.
CLASS zcl_op_value_pretty_printer DEFINITION LOCAL FRIENDS ltcl_pretty_printer_should ltcl_pretty_printer_performanc.
CLASS ltcl_pretty_printer_should DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO zcl_op_value_pretty_printer.
    METHODS:
      setup,
      add_indent FOR TESTING,
      table_of_integers FOR TESTING,
      table_in_structure FOR TESTING,
      performance FOR TESTING,
      accept_arrow_in_lhs FOR TESTING.
ENDCLASS.

CLASS ltcl_pretty_printer_performanc DEFINITION FINAL FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      string_literal FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_pretty_printer_should IMPLEMENTATION.

  METHOD setup.
    "as we are here in a private unit test class of zcl_op_value_pretty_printer we don't need to use the factory or injector for instantiating
    cut = NEW zcl_op_value_pretty_printer( ).
  ENDMETHOD.

  METHOD add_indent.

    "GIVEN
    DATA(input) = |IS_NESTED_STRUCTURE = VALUE #( COL1 = '1' COL2 = VALUE #( COL1 = '1' COL2 = '2' ) COL3 = '3' COL4 = VALUE #( COL1 = '1' COL2 = '2' ) ).|.

    "WHEN
    DATA(formated_string) = cut->format( input ).

    "THEN
    DATA(expected) = concat_lines_of(
        table = VALUE string_table(
            ( |IS_NESTED_STRUCTURE = VALUE #(| )
            ( |         COL1 = '1'| )
            ( |         COL2 = VALUE #(| )
            ( |                 COL1 = '1'| )
            ( |                 COL2 = '2'| )
            ( |                 )| )
            ( |         COL3 = '3'| )
            ( |         COL4 = VALUE #(| )
            ( |                 COL1 = '1'| )
            ( |                 COL2 = '2'| )
            ( |                 )| )
            ( |         ).| ) )
        sep = |\n| ).
    cl_abap_unit_assert=>assert_equals(  EXPORTING  act  = formated_string
                                                    exp  = expected
                                                    msg  = |Indent was not added correct|       ).
  ENDMETHOD.

  METHOD table_of_integers.

    "GIVEN
    DATA(input)    = |TABLE_OF_INTEGERS = VALUE #( ( 1 ) ( 2 ) ).|.

    "WHEN
    DATA(formated_string) = cut->format( input ).

    "THEN
    DATA(expected) = concat_lines_of(
        table = VALUE string_table(
            ( |TABLE_OF_INTEGERS = VALUE #(| )
            ( |         (| )
            ( |                 1 )| )
            ( |         (| )
            ( |                 2 )| )
            ( |         ).| ) )
        sep = |\n| ).
    cl_abap_unit_assert=>assert_equals(  EXPORTING  act  = formated_string
                                                    exp  = expected
                                                    msg  = |Indent was not added correct|       ).
  ENDMETHOD.

  METHOD table_in_structure.

    "GIVEN
    DATA(input)    = |TABLE_IN_STRUCTURE = VALUE #( TABLE = VALUE #( ( COL1 = '1' COL2 = '2' ) ( COL1 = '3' COL2 = '4' ) ) ).|.

    "WHEN
    DATA(formated_string) = cut->format( input ).

    "THEN
    DATA(expected) = concat_lines_of(
        table = VALUE string_table(
            ( |TABLE_IN_STRUCTURE = VALUE #(| )
            ( |         TABLE = VALUE #(| )
            ( |                 (| )
            ( |                         COL1 = '1'| )
            ( |                         COL2 = '2'| )
            ( |                         )| )
            ( |                 (| )
            ( |                         COL1 = '3'| )
            ( |                         COL2 = '4'| )
            ( |                         )| )
            ( |                 )| )
            ( |         ).| ) )
        sep = |\n| ).
    cl_abap_unit_assert=>assert_equals(  EXPORTING  act  = formated_string
                                                    exp  = expected
                                                    msg  = |Indent was not added correct|       ).

  ENDMETHOD.

  METHOD performance.

    "GIVEN
    DATA(input)    = |TABLE_IN_STRUCTURE = VALUE #(|
                  && repeat( val = | ( COL1 = '1' COL2 = '2' )| occ = 2000 )
                  && | ).|.

    "WHEN
    DATA(formated_string) = cut->format( input ).

    "THEN
    "method should take less than DURATION SHORT

  ENDMETHOD.

  METHOD accept_arrow_in_lhs.
    "GIVEN
    DATA(input) = |CUT->ST03N_TRANSACTION_PROFILE = VALUE #( ( MANDT = '800' DATE = '20210130' TCODE = '/IWBEP/R_CLEAN_UP_QRL'  ) ).|.

    "WHEN
    DATA(formated_string) = cut->format( input ).


    "THEN
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act    = formated_string
        exp    = |CUT->ST03N_TRANSACTION_PROFILE = VALUE #(\n         (\n                 MANDT = '800'\n                 DATE = '20210130'\n                 TCODE = '/IWBEP/R_CLEAN_UP_QRL'\n                  )\n         ).|
        msg    = |Could not format input|  ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_pretty_printer_performanc IMPLEMENTATION.

  METHOD string_literal.

    DATA input TYPE string.
    DATA occ TYPE i VALUE 100.

    "WHEN a big variable is to be pretty-printed

    input = |MAIN_RECORD_TABLE = VALUE #( |
        && repeat( occ = occ val =
           |( DURATION = '62 ' STATEMENT_WITH_VALUES = `OTHER (SQL)` RECORD_NUMBERS = VALUE #( ( 1 ) ( 59 ) ( 94 ) ( 837 ) ( 2763 ) ( 3023 ) ( 3047 ) ( 3707 ) ( 3783 ) ( 3798 ) (|
        && | 3805 ) ( 3821 ) ( 3845 ) ( 5397 ) ( 5407 ) ( 5427 ) ( 5503 ) ( 5518 ) ( 5525 ) ( 5554 ) ) STATEMENT_WITH_NAMES = `OTHER (SQL)` ) ( DATE = '20151117' TIME = '17:37:35.697' INSTANCE_NAME = 'atlast|
        && |itan_DEV_30' DURATION = '4 ' STATEMENT_WITH_VALUES = `SET CLIENT INFO ( EPP_COUNTER=8, SAP_PASSPORT=[53 E6 00 00 34 47 03 00 48 2A 2A 54 2F 63 6C 64 ...] (230 bytes) )` TRANSACTION = 'OS_APPLICAT|
        && |ION' PROGRAM = 'CL_ST05_TRACE_MAIN_M==========CP' DB_CONNECTION_NAME = 'R/3' DB_CONNECTION_ID = '000000000' OPERATION = 'EXECSTA' WP_ID = '5' WP_TYPE = 'DIA' USER_NAME = 'DEVELOPER' CLIENT = '100|
        && |' TRANS_ID = 'AD3DDC991A730010E005F154910B5703' EPP_ROOT_ID = '000D3AAA88F21EDAB5CD0B0330432F05' EPP_CONNECTION_ID = '00000000000000000000000000000000' RECORD_NUMBER = 2 RECORD_NUMBERS = VALUE #(|
        && | ( 2 ) ) OFFSET = 2711 STATEMENT_WITH_NAMES = `SET CLIENT INFO ( EPP_COUNTER=8, SAP_PASSPORT=[53 E6 00 00 34 47 03 00 48 2A 2A 54 2F 63 6C 64 ...] (230 bytes) )` LENGTH_OF_STATEMENT_WITH_NAMES = |
        && |113 LINE_COLOR = 'C3' ) ( DATE = '20151117' TIME = '17:37:35.697' INSTANCE_NAME = 'atlastitan_DEV_30' DURATION = '2699 ' OBJECT = 'PTC_DIRECTORY' STATEMENT_WITH_VALUES = `SELECT  WHERE "MANDT" = |
        && |'100' AND "TYPE" = N'ST05' AND "INSTANCE_NAME" = 'atlastitan_DEV_30' AND "GUID" = 0x00000000000000000000000000000000  LIMIT 1  WITH RANGE_RESTRICTION('CURRENT')` CURSOR = '459' ARRAY_SIZE = 1 TRA|
        && |NSACTION = 'OS_APPLICATION' PROGRAM = 'CL_ST05_TRACE_MAIN_M==========CP' DB_CONNECTION_NAME = 'R/3' DB_CONNECTION_ID = '000000000' OPERATION = 'REOPEN' WP_ID = '5' WP_TYPE = 'DIA' USER_NAME = 'DE|
        && |VELOPER' CLIENT = '100' TRANS_ID = 'AD3DDC991A730010E005F154910B5703' EPP_ROOT_ID = '000D3AAA88F21EDAB5CD0B0330432F05' EPP_CONNECTION_ID = '00000000000000000000000000000000' RECORD_NUMBER = 3 REC|
        && |ORD_NUMBERS = VALUE #( ( 3 ) ( 4 ) ( 5 ) ) OFFSET = 2711 STATEMENT_WITH_NAMES = `SELECT * FROM "PTC_DIRECTORY" WHERE "MANDT" = ? AND "TYPE" = N'ST05' AND "INSTANCE_NAME" = ? AND "GUID" = ? LIMIT |
        && |1  WITH RANGE_RESTRICTION('CURRENT')` LENGTH_OF_STATEMENT_WITH_NAMES = 150 VARIABLES = `CH&3&100&CH&40&atlastitan_DEV_30                      &RA&34&0x00000000000000000000000000000000&` NUMBER_OF|
        && |_VARIABLES = 3 LINE_COLOR = 'C3' ) ( DATE = '20151117' TIME = '17:37:35.700' INSTANCE_NAME = 'atlastitan_DEV_30' DURATION = '2771 ' NUMBER_OF_ROWS = 1 OBJECT = 'PTC_DIRECTORY' STATEMENT_WITH_VALU|
        && |ES = `INSERT  VALUES( '100' , 'ST05' , 'DEVELOPER' , 'atlastitan_DEV_30' , '20151117' , '173735' , 0x00000000000000000000000000000000 , '00000000' , '' , <BLOB>  )  WITH RANGE_RESTRICTION('CURREN|
        && |T')` CURSOR = '863' ARRAY_SIZE = 1 TRANSACTION = 'OS_APPLICATION' PROGRAM = 'CL_ST05_TRACE_MAIN_M==========CP' DB_CONNECTION_NAME = 'R/3' DB_CONNECTION_ID = '000000000' OPERATION = 'REEXEC' WP_ID|
        && | = '5' WP_TYPE = 'DIA' USER_NAME = 'DEVELOPER' CLIENT = '100' TRANS_ID = 'AD3DDC991A730010E005F154910B5703' EPP_ROOT_ID = '000D3AAA88F21EDAB5CD0B0330432F05' EPP_CONNECTION_ID = '00000000000000000|
        && |000000000000000' RECORD_NUMBER = 6 RECORD_NUMBERS = VALUE #( ( 6 ) ) OFFSET = 2774 STATEMENT_WITH_NAMES = `INSERT INTO "PTC_DIRECTORY" VALUES( ? , ? , ? , ? , ? , ? , ? , ? , ? , ? )  WITH RANGE_|
        && |RESTRICTION('CURRENT')` LENGTH_OF_STATEMENT_WITH_NAMES = 110 VARIABLES = `CH&3&100&CH&5&ST05 &CH&12&DEVELOPER   &CH&40&atlastitan_DEV_30                      &NU&8&20151117&NU&6&173735&RA&34&0x00|
        && |000000000000000000000000000000&NU&8&00000000&CH&80&                                                                                &BL&6&<BLOB>&` NUMBER_OF_VARIABLES = 10 LINE_COLOR = 'C3' ) ( DA|
        && |TE = '20151117' TIME = '17:37:35.703' INSTANCE_NAME = 'atlastitan_DEV_30' DURATION = '5 ' NUMBER_OF_ROWS = 1 OBJECT = 'EUDB' STATEMENT_WITH_VALUES = `S 86 %L000D3AAA88F21EDAB5CD0B0330430F05      |
        && |    339` TRANSACTION = 'OS_APPLICATION' PROGRAM = 'CL_GUI_PROPS_CONSUMER=========CP' OPERATION = 'RETRIEV' WP_ID = '5' WP_TYPE = 'DIA' USER_NAME = 'DEVELOPER' CLIENT = '100' TRANS_ID = 'AD3DDC991|
        && |A730010E005F154910B5703' EPP_ROOT_ID = '000D3AAA88F21EDAB5CD0B0330432F05' EPP_CONNECTION_ID = '00000000000000000000000000000000' RECORD_NUMBER = 7 RECORD_NUMBERS = VALUE #( ( 7 ) ) OFFSET = 1286 |
        && |STATEMENT_WITH_NAMES = `S 86 RETRIEV` VARIABLES = `%L000D3AAA88F21EDAB5CD0B0330430F05` NUMBER_OF_VARIABLES = 86 TRACE_TYPE = 'BUF' LINE_COLOR = 'C1' ) ( DATE = '20151117' TIME = '17:37:35.703' IN|
        && |STANCE_NAME = 'atlastitan_DEV_30' DURATION = '1 ' NUMBER_OF_ROWS = 1 OBJECT = 'EUDB' STATEMENT_WITH_VALUES = `S 86 %M000D3AAA88F21EDAB5CD0B0330430F05          482` TRANSACTION = 'OS_APPLICATION' |
        && |PROGRAM = 'CL_GUI_PROPS_CONSUMER=========CP' OPERATION = 'RETRIEV' WP_ID = '5' WP_TYPE = 'DIA' USER_NAME = 'DEVELOPER' CLIENT = '100' TRANS_ID = 'AD3DDC991A730010E005F154910B5703' EPP_ROOT_ID = '|
        && |000D3AAA88F21EDAB5CD0B0330432F05' EPP_CONNECTION_ID = '00000000000000000000000000000000' RECORD_NUMBER = 8 RECORD_NUMBERS = VALUE #( ( 8 ) ) OFFSET = 1327 STATEMENT_WITH_NAMES = `S 86 RETRIEV` VA|
        && |RIABLES = `%M000D3AAA88F21EDAB5CD0B0330430F05` NUMBER_OF_VARIABLES = 86 TRACE_TYPE = 'BUF' LINE_COLOR = 'C1' ) ( DATE = '20151117' TIME = '17:37:35.703' INSTANCE_NAME = 'atlastitan_DEV_30' DURATI|
        && |ON = '1 ' NUMBER_OF_ROWS = 1 OBJECT = 'EUDB' STATEMENT_WITH_VALUES = `S 86 %C000D3AAA88F21EDAB5CD0B0330430F05          553` TRANSACTION = 'OS_APPLICATION' PROGRAM = 'CL_GUI_PROPS_CONSUMER========|
        && |=CP' OPERATION = 'RETRIEV' WP_ID = '5' WP_TYPE = 'DIA' USER_NAME = 'DEVELOPER' CLIENT = '100' TRANS_ID = 'AD3DDC991A730010E005F154910B5703' EPP_ROOT_ID = '000D3AAA88F21EDAB5CD0B0330432F05' EPP_CO|
        && |NNECTION_ID = '00000000000000000000000000000000' RECORD_NUMBER = 9 RECORD_NUMBERS = VALUE #( ( 9 ) ) OFFSET = 1366 STATEMENT_WITH_NAMES = `S 86 RETRIEV` VARIABLES = `%C000D3AAA88F21EDAB5CD0B03304|
        && |30F05` NUMBER_OF_VARIABLES = 86 TRACE_TYPE = 'BUF' LINE_COLOR = 'C1' ) | )
        && |).|.

    "THEN the pretty-printing should last not too long!

    DATA(formated_string) = NEW zcl_op_value_pretty_printer( )->format( input ).

    DATA(exp) = |MAIN_RECORD_TABLE = VALUE #(\n|
        && repeat( occ = occ val = |         (\n| &
                                   |                 DURATION = '62 '\n| &
                                   |                 STATEMENT_WITH_VALUES = `OTHER (SQL)`\n| &
                                   |                 RECORD_NUMBERS = VALUE #(\n| &
                                   |                         (\n| &
                                   |                                 1 )\n| &
                                   |                         (\n| &
                                   |                                 59 )\n| &
                                   |                         (\n| &
                                   |                                 94 )\n| &
                                   |                         (\n| &
                                   |                                 837 )\n| &
                                   |                         (\n| &
                                   |                                 2763 )\n| &
                                   |                         (\n| &
                                   |                                 3023 )\n| &
                                   |                         (\n| &
                                   |                                 3047 )\n| &
                                   |                         (\n| &
                                   |                                 3707 )\n| &
                                   |                         (\n| &
                                   |                                 3783 )\n| &
                                   |                         (\n| &
                                   |                                 3798 )\n| &
                                   |                         (\n| &
                                   |                                 3805 )\n| &
                                   |                         (\n| &
                                   |                                 3821 )\n| &
                                   |                         (\n| &
                                   |                                 3845 )\n| &
                                   |                         (\n| &
                                   |                                 5397 )\n| &
                                   |                         (\n| &
                                   |                                 5407 )\n| &
                                   |                         (\n| &
                                   |                                 5427 )\n| &
                                   |                         (\n| &
                                   |                                 5503 )\n| &
                                   |                         (\n| &
                                   |                                 5518 )\n| &
                                   |                         (\n| &
                                   |                                 5525 )\n| &
                                   |                         (\n| &
                                   |                                 5554 )\n| &
                                   |                         )\n| &
                                   |                 STATEMENT_WITH_NAMES = `OTHER (SQL)`\n| &
                                   |                 )\n| &
                                   |         (\n| &
                                   |                 DATE = '20151117'\n| &
                                   |                 TIME = '17:37:35.697'\n| &
                                   |                 INSTANCE_NAME = 'atlastitan_DEV_30'\n| &
                                   |                 DURATION = '4 '\n| &
                                   |                 STATEMENT_WITH_VALUES = `SET CLIENT INFO ( EPP_COUNTER=8, SAP_PASSPORT=[53 E6 00 00 34 47 03 00 48 2A 2A 54 2F 63 6C 64 ...] (230 bytes) )`\n| &
                                   |                 TRANSACTION = 'OS_APPLICATION'\n| &
                                   |                 PROGRAM = 'CL_ST05_TRACE_MAIN_M==========CP'\n| &
                                   |                 DB_CONNECTION_NAME = 'R/3'\n| &
                                   |                 DB_CONNECTION_ID = '000000000'\n| &
                                   |                 OPERATION = 'EXECSTA'\n| &
                                   |                 WP_ID = '5'\n| &
                                   |                 WP_TYPE = 'DIA'\n| &
                                   |                 USER_NAME = 'DEVELOPER'\n| &
                                   |                 CLIENT = '100'\n| &
                                   |                 TRANS_ID = 'AD3DDC991A730010E005F154910B5703'\n| &
                                   |                 EPP_ROOT_ID = '000D3AAA88F21EDAB5CD0B0330432F05'\n| &
                                   |                 EPP_CONNECTION_ID = '00000000000000000000000000000000'\n| &
                                   |                 RECORD_NUMBER = 2\n| &
                                   |                 RECORD_NUMBERS = VALUE #(\n| &
                                   |                         (\n| &
                                   |                                 2 )\n| &
                                   |                         )\n| &
                                   |                 OFFSET = 2711\n| &
                                   |                 STATEMENT_WITH_NAMES = `SET CLIENT INFO ( EPP_COUNTER=8, SAP_PASSPORT=[53 E6 00 00 34 47 03 00 48 2A 2A 54 2F 63 6C 64 ...] (230 bytes) )`\n| &
                                   |                 LENGTH_OF_STATEMENT_WITH_NAMES = 113\n| &
                                   |                 LINE_COLOR = 'C3'\n| &
                                   |                 )\n| &
                                   |         (\n| &
                                   |                 DATE = '20151117'\n| &
                                   |                 TIME = '17:37:35.697'\n| &
                                   |                 INSTANCE_NAME = 'atlastitan_DEV_30'\n| &
                                   |                 DURATION = '2699 '\n| &
                                   |                 OBJECT = 'PTC_DIRECTORY'\n| &
                                   |                 STATEMENT_WITH_VALUES = `SELECT  WHERE "MANDT" = '100' AND "TYPE" = N'ST05' AND "INSTANCE_NAME" = 'atlastitan_DEV_30' AND | &
                                                                |"GUID" = 0x00000000000000000000000000000000  LIMIT 1  WITH RANGE_RESTRICTION('CURRENT')`\n| &
                                   |                 CURSOR = '459'\n| &
                                   |                 ARRAY_SIZE = 1\n| &
                                   |                 TRANSACTION = 'OS_APPLICATION'\n| &
                                   |                 PROGRAM = 'CL_ST05_TRACE_MAIN_M==========CP'\n| &
                                   |                 DB_CONNECTION_NAME = 'R/3'\n| &
                                   |                 DB_CONNECTION_ID = '000000000'\n| &
                                   |                 OPERATION = 'REOPEN'\n| &
                                   |                 WP_ID = '5'\n| &
                                   |                 WP_TYPE = 'DIA'\n| &
                                   |                 USER_NAME = 'DEVELOPER'\n| &
                                   |                 CLIENT = '100'\n| &
                                   |                 TRANS_ID = 'AD3DDC991A730010E005F154910B5703'\n| &
                                   |                 EPP_ROOT_ID = '000D3AAA88F21EDAB5CD0B0330432F05'\n| &
                                   |                 EPP_CONNECTION_ID = '00000000000000000000000000000000'\n| &
                                   |                 RECORD_NUMBER = 3\n| &
                                   |                 RECORD_NUMBERS = VALUE #(\n| &
                                   |                         (\n| &
                                   |                                 3 )\n| &
                                   |                         (\n| &
                                   |                                 4 )\n| &
                                   |                         (\n| &
                                   |                                 5 )\n| &
                                   |                         )\n| &
                                   |                 OFFSET = 2711\n| &
                                   |                 STATEMENT_WITH_NAMES = `SELECT * FROM "PTC_DIRECTORY" WHERE "MANDT" = ? AND "TYPE" = N'ST05' AND "INSTANCE_NAME" = ? AND "GUID" = ? LIMIT 1  WITH RANGE_RESTRICTION('CURRENT')`\n| &
                                   |                 LENGTH_OF_STATEMENT_WITH_NAMES = 150\n| &
                                   |                 VARIABLES = `CH&3&100&CH&40&atlastitan_DEV_30                      &RA&34&0x00000000000000000000000000000000&`\n| &
                                   |                 NUMBER_OF_VARIABLES = 3\n| &
                                   |                 LINE_COLOR = 'C3'\n| &
                                   |                 )\n| &
                                   |         (\n| &
                                   |                 DATE = '20151117'\n| &
                                   |                 TIME = '17:37:35.700'\n| &
                                   |                 INSTANCE_NAME = 'atlastitan_DEV_30'\n| &
                                   |                 DURATION = '2771 '\n| &
                                   |                 NUMBER_OF_ROWS = 1\n| &
                                   |                 OBJECT = 'PTC_DIRECTORY'\n| &
                                   |                 STATEMENT_WITH_VALUES = `INSERT  VALUES( '100' , 'ST05' , 'DEVELOPER' , 'atlastitan_DEV_30' , '20151117' , '173735' , | &
                                                                |0x00000000000000000000000000000000 , '00000000' , '' , <BLOB>  )  WITH RANGE_RESTRICTION('CURRENT')`\n| &
                                   |                 CURSOR = '863'\n| &
                                   |                 ARRAY_SIZE = 1\n| &
                                   |                 TRANSACTION = 'OS_APPLICATION'\n| &
                                   |                 PROGRAM = 'CL_ST05_TRACE_MAIN_M==========CP'\n| &
                                   |                 DB_CONNECTION_NAME = 'R/3'\n| &
                                   |                 DB_CONNECTION_ID = '000000000'\n| &
                                   |                 OPERATION = 'REEXEC'\n| &
                                   |                 WP_ID = '5'\n| &
                                   |                 WP_TYPE = 'DIA'\n| &
                                   |                 USER_NAME = 'DEVELOPER'\n| &
                                   |                 CLIENT = '100'\n| &
                                   |                 TRANS_ID = 'AD3DDC991A730010E005F154910B5703'\n| &
                                   |                 EPP_ROOT_ID = '000D3AAA88F21EDAB5CD0B0330432F05'\n| &
                                   |                 EPP_CONNECTION_ID = '00000000000000000000000000000000'\n| &
                                   |                 RECORD_NUMBER = 6\n| &
                                   |                 RECORD_NUMBERS = VALUE #(\n| &
                                   |                         (\n| &
                                   |                                 6 )\n| &
                                   |                         )\n| &
                                   |                 OFFSET = 2774\n| &
                                   |                 STATEMENT_WITH_NAMES = `INSERT INTO "PTC_DIRECTORY" VALUES( ? , ? , ? , ? , ? , ? , ? , ? , ? , ? )  WITH RANGE_RESTRICTION('CURRENT')`\n| &
                                   |                 LENGTH_OF_STATEMENT_WITH_NAMES = 110\n| &
                                   |                 VARIABLES = `CH&3&100&CH&5&ST05 &CH&12&DEVELOPER   &CH&40&atlastitan_DEV_30                      &NU&8&20151117&NU&6&173735&RA&34&0x00000000000000000000000000000000&NU&8&00000000&CH&80&           | &&
|                                                                     &BL&6&<BLOB>&`\n| &
                                   |                 NUMBER_OF_VARIABLES = 10\n| &
                                   |                 LINE_COLOR = 'C3'\n| &
                                   |                 )\n| &
                                   |         (\n| &
                                   |                 DATE = '20151117'\n| &
                                   |                 TIME = '17:37:35.703'\n| &
                                   |                 INSTANCE_NAME = 'atlastitan_DEV_30'\n| &
                                   |                 DURATION = '5 '\n| &
                                   |                 NUMBER_OF_ROWS = 1\n| &
                                   |                 OBJECT = 'EUDB'\n| &
                                   |                 STATEMENT_WITH_VALUES = `S 86 %L000D3AAA88F21EDAB5CD0B0330430F05          339`\n| &
                                   |                 TRANSACTION = 'OS_APPLICATION'\n| &
                                   |                 PROGRAM = 'CL_GUI_PROPS_CONSUMER=========CP'\n| &
                                   |                 OPERATION = 'RETRIEV'\n| &
                                   |                 WP_ID = '5'\n| &
                                   |                 WP_TYPE = 'DIA'\n| &
                                   |                 USER_NAME = 'DEVELOPER'\n| &
                                   |                 CLIENT = '100'\n| &
                                   |                 TRANS_ID = 'AD3DDC991A730010E005F154910B5703'\n| &
                                   |                 EPP_ROOT_ID = '000D3AAA88F21EDAB5CD0B0330432F05'\n| &
                                   |                 EPP_CONNECTION_ID = '00000000000000000000000000000000'\n| &
                                   |                 RECORD_NUMBER = 7\n| &
                                   |                 RECORD_NUMBERS = VALUE #(\n| &
                                   |                         (\n| &
                                   |                                 7 )\n| &
                                   |                         )\n| &
                                   |                 OFFSET = 1286\n| &
                                   |                 STATEMENT_WITH_NAMES = `S 86 RETRIEV`\n| &
                                   |                 VARIABLES = `%L000D3AAA88F21EDAB5CD0B0330430F05`\n| &
                                   |                 NUMBER_OF_VARIABLES = 86\n| &
                                   |                 TRACE_TYPE = 'BUF'\n| &
                                   |                 LINE_COLOR = 'C1'\n| &
                                   |                 )\n| &
                                   |         (\n| &
                                   |                 DATE = '20151117'\n| &
                                   |                 TIME = '17:37:35.703'\n| &
                                   |                 INSTANCE_NAME = 'atlastitan_DEV_30'\n| &
                                   |                 DURATION = '1 '\n| &
                                   |                 NUMBER_OF_ROWS = 1\n| &
                                   |                 OBJECT = 'EUDB'\n| &
                                   |                 STATEMENT_WITH_VALUES = `S 86 %M000D3AAA88F21EDAB5CD0B0330430F05          482`\n| &
                                   |                 TRANSACTION = 'OS_APPLICATION'\n| &
                                   |                 PROGRAM = 'CL_GUI_PROPS_CONSUMER=========CP'\n| &
                                   |                 OPERATION = 'RETRIEV'\n| &
                                   |                 WP_ID = '5'\n| &
                                   |                 WP_TYPE = 'DIA'\n| &
                                   |                 USER_NAME = 'DEVELOPER'\n| &
                                   |                 CLIENT = '100'\n| &
                                   |                 TRANS_ID = 'AD3DDC991A730010E005F154910B5703'\n| &
                                   |                 EPP_ROOT_ID = '000D3AAA88F21EDAB5CD0B0330432F05'\n| &
                                   |                 EPP_CONNECTION_ID = '00000000000000000000000000000000'\n| &
                                   |                 RECORD_NUMBER = 8\n| &
                                   |                 RECORD_NUMBERS = VALUE #(\n| &
                                   |                         (\n| &
                                   |                                 8 )\n| &
                                   |                         )\n| &
                                   |                 OFFSET = 1327\n| &
                                   |                 STATEMENT_WITH_NAMES = `S 86 RETRIEV`\n| &
                                   |                 VARIABLES = `%M000D3AAA88F21EDAB5CD0B0330430F05`\n| &
                                   |                 NUMBER_OF_VARIABLES = 86\n| &
                                   |                 TRACE_TYPE = 'BUF'\n| &
                                   |                 LINE_COLOR = 'C1'\n| &
                                   |                 )\n| &
                                   |         (\n| &
                                   |                 DATE = '20151117'\n| &
                                   |                 TIME = '17:37:35.703'\n| &
                                   |                 INSTANCE_NAME = 'atlastitan_DEV_30'\n| &
                                   |                 DURATION = '1 '\n| &
                                   |                 NUMBER_OF_ROWS = 1\n| &
                                   |                 OBJECT = 'EUDB'\n| &
                                   |                 STATEMENT_WITH_VALUES = `S 86 %C000D3AAA88F21EDAB5CD0B0330430F05          553`\n| &
                                   |                 TRANSACTION = 'OS_APPLICATION'\n| &
                                   |                 PROGRAM = 'CL_GUI_PROPS_CONSUMER=========CP'\n| &
                                   |                 OPERATION = 'RETRIEV'\n| &
                                   |                 WP_ID = '5'\n| &
                                   |                 WP_TYPE = 'DIA'\n| &
                                   |                 USER_NAME = 'DEVELOPER'\n| &
                                   |                 CLIENT = '100'\n| &
                                   |                 TRANS_ID = 'AD3DDC991A730010E005F154910B5703'\n| &
                                   |                 EPP_ROOT_ID = '000D3AAA88F21EDAB5CD0B0330432F05'\n| &
                                   |                 EPP_CONNECTION_ID = '00000000000000000000000000000000'\n| &
                                   |                 RECORD_NUMBER = 9\n| &
                                   |                 RECORD_NUMBERS = VALUE #(\n| &
                                   |                         (\n| &
                                   |                                 9 )\n| &
                                   |                         )\n| &
                                   |                 OFFSET = 1366\n| &
                                   |                 STATEMENT_WITH_NAMES = `S 86 RETRIEV`\n| &
                                   |                 VARIABLES = `%C000D3AAA88F21EDAB5CD0B0330430F05`\n| &
                                   |                 NUMBER_OF_VARIABLES = 86\n| &
                                   |                 TRACE_TYPE = 'BUF'\n| &
                                   |                 LINE_COLOR = 'C1'\n| &
                                   |                 )\n| )
        && |         ).|.


    cl_abap_unit_assert=>assert_equals( act = formated_string exp = exp ).

  ENDMETHOD.

ENDCLASS.
