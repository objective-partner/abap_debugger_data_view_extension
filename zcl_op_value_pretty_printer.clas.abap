"!this class is responsible for pretty printing
"!VALUE # operator strings
CLASS zcl_op_value_pretty_printer DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_op_pretty_printer_factory. "needed for instantiation

  PUBLIC SECTION.
    INTERFACES: zif_op_value_pretty_printer.
    ALIASES: format FOR zif_op_value_pretty_printer~format.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF t_char_offset,
             count     TYPE i,
             offset_no TYPE i,
             char(1)   TYPE c,
           END OF t_char_offset,
           tt_char_offset TYPE STANDARD TABLE OF t_char_offset WITH DEFAULT KEY,

           t_terminal     TYPE c LENGTH 14,
           t_sub_terminal TYPE c LENGTH 17.




    CONSTANTS: BEGIN OF c_terminal,
                 assign         TYPE t_terminal VALUE 'assign',
                 rhs            TYPE t_terminal VALUE 'rhs',
                 empty          TYPE t_terminal VALUE 'empty',
                 struct         TYPE t_terminal VALUE 'struct',
                 struct2        TYPE t_terminal VALUE 'struct2',
                 iteration      TYPE t_terminal VALUE 'iteration',
                 table          TYPE t_terminal VALUE 'table',
                 rhs2           TYPE t_terminal VALUE 'rhs2',
                 symbolname     TYPE t_terminal VALUE 'symbolname',
                 number         TYPE t_terminal VALUE 'number',
                 non_terminal   TYPE t_terminal VALUE 'non_terminal',
                 text_literal   TYPE t_terminal VALUE 'text_literal',
                 string_literal TYPE t_terminal VALUE 'string_literal',
                 spaces         TYPE t_terminal VALUE 'spaces',
               END OF c_terminal,

               BEGIN OF c_sub_terminal,
                 na                TYPE t_sub_terminal VALUE 'na',
                 assign_operator   TYPE t_sub_terminal VALUE '=',
                 value_operator    TYPE t_sub_terminal VALUE 'VALUE',
                 parenthesis_open  TYPE t_sub_terminal VALUE '(',
                 parenthesis_close TYPE t_sub_terminal VALUE ')',
                 empty_line        TYPE t_sub_terminal VALUE 'empty_line',
                 end_of_statement  TYPE t_sub_terminal VALUE 'end_of_statement',
               END OF c_sub_terminal,

               c_newline              TYPE abap_char1 VALUE cl_abap_char_utilities=>cr_lf,

               c_regex_lhs            TYPE string VALUE  `[A-Z][A-Z0-9_/\-\[\]\>]+` ##NO_TEXT,
               c_regex_number         TYPE string VALUE `[0-9]+` ##NO_TEXT,
               c_regex_text_literal   TYPE string VALUE `'(?:''|[^'])+'` ##NO_TEXT,
               c_regex_string_literal TYPE string VALUE '`(?:``|[^`])+`' ##NO_TEXT.

    TYPES:        BEGIN OF ty_trace,
                    parent_sync_point TYPE i,
                    terminal          TYPE t_terminal,
                    sub_terminal      TYPE t_sub_terminal,
                    offset            TYPE i,
                    length            TYPE i,
                    sync_point        TYPE i,
                    text              TYPE string,
                  END OF ty_trace,

                  BEGIN OF ty_sync_point,
                    terminal     TYPE t_terminal,
                    sub_terminal TYPE t_sub_terminal,
                    offset       TYPE i,
                    trace_index  TYPE i,
                  END OF ty_sync_point.


    DATA: steering    TYPE tt_char_offset,
          offset      TYPE i,
          text        TYPE string,
          trace_tab   TYPE TABLE OF ty_trace
                        WITH NON-UNIQUE SORTED KEY k1 COMPONENTS parent_sync_point terminal offset,
          sync_points TYPE TABLE OF ty_sync_point.
    METHODS:
      get_spaces RETURNING VALUE(r_spaces_string) TYPE string,
      get_offset_in_current_line
        IMPORTING
          i_content       TYPE string
        RETURNING
          VALUE(r_offset) TYPE i,
      get_offset_for_apostroph
        IMPORTING
          i_content       TYPE string
        RETURNING
          VALUE(r_offset) TYPE i,
      get_offset_for_closing_bracket
        IMPORTING
          i_content       TYPE string
        RETURNING
          VALUE(r_offset) TYPE i,
      clean_up_steering_tab
        IMPORTING
          i_last_char TYPE c,
      assign RAISING lcx_no_match,
      find_spaces,
      rhs RAISING lcx_no_match,
      add_empty_value_operator RAISING lcx_no_match,
      add_structure_rhs RAISING lcx_no_match,
      add_structure_content RAISING lcx_no_match,
      add_table RAISING lcx_no_match,
      rhs2 RAISING lcx_no_match,
      lhs RAISING lcx_no_match,
      add_number RAISING lcx_no_match,
      non_terminal
        IMPORTING
                  i_sub_terminal TYPE t_sub_terminal
                  i_regex        TYPE csequence
        RAISING   lcx_no_match,
      add_text_literal RAISING lcx_no_match,
      add_string_literal RAISING lcx_no_match,
      set_offset_for_regex
        IMPORTING
                  i_regex TYPE csequence
        RAISING   lcx_no_match,

      add_to_trace,
      set_sync_point
        IMPORTING
          i_terminal                        TYPE t_terminal
          i_sub_terminal                    TYPE t_sub_terminal DEFAULT c_sub_terminal-na
        RETURNING
          VALUE(r_current_sync_point_index) TYPE i,
      reset_to_sync_point
        IMPORTING
          i_sync_point_index TYPE i,
      cancel_sync_point
        IMPORTING
          i_sync_point_index TYPE i,

      add_value_open_operator RAISING lcx_no_match,

      add_parenthesis_close RAISING lcx_no_match,

      add_assign_operator RAISING lcx_no_match,

      add_end_of_statement RAISING lcx_no_match,

      add_empty_line RAISING lcx_no_match,

      add_parenthesis_open RAISING lcx_no_match.




ENDCLASS.



CLASS zcl_op_value_pretty_printer IMPLEMENTATION.

  METHOD get_spaces.
    DATA(spaces_no) = me->steering[ lines( me->steering ) ]-offset_no.
    r_spaces_string = repeat( val = ` ` occ = spaces_no ).
  ENDMETHOD.


  METHOD get_offset_in_current_line.
    SPLIT i_content  AT c_newline INTO TABLE DATA(lt_split).
    DATA(last_line) = lt_split[ lines( lt_split ) ].
    r_offset = strlen( last_line ) + 1.
  ENDMETHOD.


  METHOD get_offset_for_apostroph.
    SPLIT i_content  AT c_newline INTO TABLE DATA(lt_split).
    DATA(last_line) = lt_split[ lines( lt_split ) ].

    DATA(regex_matcher) = cl_abap_matcher=>create(  pattern = |\\S|
                                                    text    = last_line    ).

    regex_matcher->find_next( ).
    r_offset = regex_matcher->get_offset( ).

  ENDMETHOD.


  METHOD get_offset_for_closing_bracket.
    "find offset of corresponding opening bracket
    "therefor we need to loop backwards in steering table

    DATA(x_lines) = lines( me->steering ).

    DO x_lines TIMES.
      DATA(line) = me->steering[ x_lines ].
      IF line-char EQ '('.
        r_offset = line-offset_no.
        EXIT.
      ELSE.
        IF x_lines = 1.
          EXIT.
        ELSE.
          x_lines = x_lines - 1.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD clean_up_steering_tab.
    CHECK i_last_char EQ |)|.
    "a formating block is complete
    "we need to clean up our steering data and remove infos about this complete formating block
    DATA(x_lines) = lines( me->steering ).

    DO x_lines TIMES.
      DATA(line) = me->steering[ x_lines ].
      DELETE me->steering INDEX x_lines.
      IF line-char EQ |(|.
        EXIT.
      ENDIF.

      IF x_lines = 1.
        EXIT.
      ELSE.
        x_lines = x_lines - 1.
      ENDIF.

    ENDDO.

  ENDMETHOD.

  METHOD assign.
    " assign tries to build this: LHS = RHS
    TRY.
        find_spaces( ).
        DATA(current_sync_point_index) = set_sync_point( i_terminal = c_terminal-assign ).

        lhs( ).

        add_assign_operator( ).

        rhs( ).

        add_end_of_statement( ).

        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( current_sync_point_index ).
    ENDTRY.
  ENDMETHOD.

  METHOD rhs.
    "RHS could be : number | text | string | empty | struct | table
    find_spaces( ).
    DATA(current_sync_point_index) = set_sync_point( i_terminal = c_terminal-rhs ).
    TRY.
        TRY.
            add_number( ).
          CATCH lcx_no_match.
            TRY.
                add_text_literal( ).
              CATCH lcx_no_match.
                TRY.
                    add_string_literal( ).
                  CATCH lcx_no_match.
                    TRY.
                        add_empty_value_operator( ).
                      CATCH lcx_no_match.
                        TRY.
                            add_structure_rhs( ).
                          CATCH lcx_no_match.
                            add_table( ).
                        ENDTRY.
                    ENDTRY.
                ENDTRY.
            ENDTRY.
        ENDTRY.
      CLEANUP.
        reset_to_sync_point( current_sync_point_index ).
    ENDTRY.
    add_to_trace( ).
  ENDMETHOD.

  METHOD add_empty_value_operator.
    "empty : 'VALUE #( )'
    TRY.
        find_spaces( ).
        DATA(current_sync_point_index) = set_sync_point( c_terminal-empty ).
        set_offset_for_regex( `VALUE #\( \)` ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( current_sync_point_index ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_structure_rhs.
    "struct : 'VALUE #(' struct2 ')'
    TRY.
        find_spaces( ).
        DATA(current_sync_point_index) = set_sync_point( c_terminal-struct ).
        add_value_open_operator( ).
        add_structure_content( ).
        add_parenthesis_close( ).

        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( current_sync_point_index ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_structure_content.
*    struct2 : ( LHS '=' RHS )+
    TRY.
        find_spaces( ).
        DATA(structure_sync_point_index) = set_sync_point( c_terminal-struct2 ).
        set_sync_point( c_terminal-iteration ).
        lhs( ).
        add_assign_operator( ).
        rhs( ).
        add_to_trace( ).
        DO.
          TRY.
              find_spaces( ).
              DATA(point2) = set_sync_point( c_terminal-iteration ).
              TRY.
                  lhs( ).
                CATCH lcx_no_match.
                  cancel_sync_point( point2 ).
                  EXIT.
              ENDTRY.
              add_assign_operator( ).
              rhs( ).
              add_to_trace( ).
            CLEANUP.
              reset_to_sync_point( point2 ).
          ENDTRY.
        ENDDO.
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( structure_sync_point_index ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_table.
*    table : 'VALUE #(' ( '( )' | ( '(' RHS2 ')' )+ ) ')'
    TRY.
        find_spaces( ).
        DATA(point) = set_sync_point( c_terminal-table ).
        add_value_open_operator( ).
        TRY.
            add_empty_line( ).
          CATCH lcx_no_match.
            DO.
              TRY.
                  find_spaces( ).
                  DATA(point2) = set_sync_point( c_terminal-iteration ).
                  TRY.
                      add_parenthesis_open( ).

                    CATCH lcx_no_match.
                      cancel_sync_point( point2 ).
                      EXIT.
                  ENDTRY.
                  rhs2( ).
                  add_parenthesis_close( ).

                  add_to_trace( ).
                CLEANUP.
                  reset_to_sync_point( point2 ).
              ENDTRY.
            ENDDO.

        ENDTRY.
        add_parenthesis_close( ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD rhs2.
*    RHS2 : number | text | string | empty | struct2
    TRY.
        find_spaces( ).
        DATA(current_sync_point_index) = set_sync_point( c_terminal-rhs2 ).
        TRY.
            add_number( ).
          CATCH lcx_no_match.
            TRY.
                add_text_literal( ).
              CATCH lcx_no_match.
                TRY.
                    add_string_literal( ).
                  CATCH lcx_no_match.
                    TRY.
                        add_empty_value_operator( ).
                      CATCH lcx_no_match.
                        add_structure_content( ).
                    ENDTRY.
                ENDTRY.
            ENDTRY.
        ENDTRY.
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( current_sync_point_index ).
    ENDTRY.
  ENDMETHOD.

  METHOD lhs.
    TRY.
        find_spaces( ).
        DATA(current_sync_point_index) = set_sync_point( c_terminal-symbolname ).
        " SCALAR
        " STRUCTURE
        " TABLE
        " STRUCTURE-COMPONENT
        " TABLE[1]
        " TABLE[1]-COMPONENT
        " VAR_2
        " /NAMESPACE/VARIABLE
        "something like "CUT->ST03N_TRANSACTION_PROFILE" could also be a lhs


        set_offset_for_regex( c_regex_lhs ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( current_sync_point_index ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_number.
    TRY.
        find_spaces( ).
        DATA(current_sync_point_index) = set_sync_point( c_terminal-number ).
        set_offset_for_regex( c_regex_number ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( current_sync_point_index ).
    ENDTRY.
  ENDMETHOD.

  METHOD non_terminal.
    TRY.
        find_spaces( ).
        DATA(current_sync_point_index) = set_sync_point( i_terminal = c_terminal-non_terminal i_sub_terminal = i_sub_terminal ).
        set_offset_for_regex( i_regex ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( current_sync_point_index ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_text_literal.
    TRY.
        find_spaces( ).
        DATA(current_sync_point_index) = set_sync_point( c_terminal-text_literal ).
        set_offset_for_regex( c_regex_text_literal ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( current_sync_point_index ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_string_literal.
    TRY.
        find_spaces( ).
        DATA(current_sync_point_index) = set_sync_point( c_terminal-string_literal ).
        set_offset_for_regex( c_regex_string_literal ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( current_sync_point_index ).
    ENDTRY.
  ENDMETHOD.

  METHOD find_spaces.
    IF me->offset >= strlen( me->text ).
      RETURN.
    ENDIF.
    FIND FIRST OCCURRENCE OF REGEX '^\s+' IN me->text+me->offset MATCH LENGTH DATA(length).
    IF sy-subrc = 0.
      APPEND VALUE ty_trace(
              sync_point        = 0
              parent_sync_point = 0
              terminal          = c_terminal-spaces
              sub_terminal      = c_sub_terminal-na
              offset            = me->offset
              length            = length
          ) TO me->trace_tab.
      me->offset = me->offset + length.
    ENDIF.
  ENDMETHOD.

  METHOD set_offset_for_regex.
    IF me->offset >= strlen( me->text ).
      RAISE EXCEPTION TYPE lcx_no_match.
    ENDIF.

    DATA(regex2) = '^' && i_regex.
    FIND FIRST OCCURRENCE OF REGEX regex2 IN me->text+me->offset MATCH LENGTH DATA(length).
    IF sy-subrc = 0.
      me->offset = me->offset + length.
    ELSE.
      RAISE EXCEPTION TYPE lcx_no_match.
    ENDIF.
  ENDMETHOD.

  METHOD zif_op_value_pretty_printer~format.

*    start : assign
*
*    assign : symbolname = RHS
*
*    RHS : number | text | string | empty | struct | table
*
*    empty : 'VALUE #( )'
*
*    add_structure_rhs : 'VALUE #(' struct2 ')'
*
*    add_structure_content : ( LHS (left hand side) '=' RHS (right hand side) )+
*
*    table : 'VALUE #(' ( '( )' | struct2 ( '(' RHS2 ')' )+ | ( '(' RHS ')' )+ ')'
*
*    RHS2 : number | text | string | empty | struct2
*
*    LHS : ^[A-Z][A-Z0-9_\-]+
*
*    number : ^[0-9]+
*
*    text : ^'(?:''|[^'])+'

    me->text = i_unformated_value_content.
    me->offset = 0.
    me->trace_tab = VALUE #( ).
    me->sync_points = VALUE #( ).
    TRY.
        assign( ).
      CATCH:
       lcx_parser_interrupt,
       lcx_no_match INTO DATA(lx_error).
        zcl_op_debugger_integration=>debug_debugger_if_needed( ).
    ENDTRY.

    DATA(indent) = 0.
    DATA(indent_step) = 8.
    DATA(indent_spaces) = repeat( val = ` ` occ = indent ).

    LOOP AT me->trace_tab REFERENCE INTO DATA(trace_line).

      CASE trace_line->terminal.
        WHEN c_terminal-symbolname
              OR c_terminal-non_terminal
              OR c_terminal-number
              OR c_terminal-text_literal
              OR c_terminal-string_literal
              OR c_terminal-empty
              OR c_terminal-spaces.
          r_formated_content = r_formated_content && substring( val = i_unformated_value_content off = trace_line->offset len = trace_line->length ).
      ENDCASE.

      IF trace_line->terminal = c_terminal-iteration.
        r_formated_content = r_formated_content && |\n| && indent_spaces.
      ELSEIF trace_line->sub_terminal = c_sub_terminal-value_operator
            OR trace_line->sub_terminal = c_sub_terminal-parenthesis_open.
        indent = indent + indent_step.
        indent_spaces = indent_spaces && repeat( val = ` ` occ = indent_step ).
        r_formated_content = r_formated_content && |\n| && indent_spaces.
      ELSEIF trace_line->sub_terminal = c_sub_terminal-parenthesis_close.
        indent = indent - indent_step.
        SHIFT indent_spaces LEFT BY indent_step PLACES.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD add_to_trace.

    ASSIGN me->sync_points[ lines( me->sync_points ) ] TO FIELD-SYMBOL(<sync_point>).

    APPEND VALUE ty_trace(
            LET length = me->offset - <sync_point>-offset IN
            sync_point        = lines( me->sync_points )
            parent_sync_point = lines( me->sync_points ) - 1
            terminal          = <sync_point>-terminal
            sub_terminal      = <sync_point>-sub_terminal
            offset            = <sync_point>-offset
            length            = length
            "text              = text+<sync_point>-offset(length)
        ) TO me->trace_tab.

    DELETE me->sync_points INDEX lines( me->sync_points ).

    IF 0 = 1.
      RAISE EXCEPTION TYPE lcx_parser_interrupt.
    ENDIF.

  ENDMETHOD.


  METHOD set_sync_point.

    APPEND VALUE ty_sync_point(
            terminal     = i_terminal
            sub_terminal = i_sub_terminal
            offset       = me->offset
            trace_index  = lines( me->trace_tab )
        ) TO me->sync_points.

    r_current_sync_point_index = lines( me->sync_points ).


  ENDMETHOD.


  METHOD reset_to_sync_point.

    DELETE me->trace_tab FROM me->sync_points[ i_sync_point_index ]-trace_index + 1.
    me->offset = me->sync_points[ i_sync_point_index ]-offset.
    DELETE me->sync_points FROM i_sync_point_index.

    IF 0 = 1.
      RAISE EXCEPTION TYPE lcx_parser_interrupt.
    ENDIF.

  ENDMETHOD.


  METHOD cancel_sync_point.

    DELETE me->trace_tab FROM me->sync_points[ i_sync_point_index ]-trace_index + 1.
    DELETE me->sync_points FROM i_sync_point_index.

    IF 0 = 1.
      RAISE EXCEPTION TYPE lcx_parser_interrupt.
    ENDIF.

  ENDMETHOD.


  METHOD add_value_open_operator.
    non_terminal( i_sub_terminal = c_sub_terminal-value_operator i_regex = `VALUE #\(` ).
  ENDMETHOD.

  METHOD add_parenthesis_close.
    non_terminal( i_sub_terminal = c_sub_terminal-parenthesis_close i_regex = `\)` ).
  ENDMETHOD.

  METHOD add_assign_operator.
    non_terminal( i_sub_terminal = c_sub_terminal-assign_operator i_regex = '=' ).
  ENDMETHOD.

  METHOD add_end_of_statement.
    non_terminal( i_sub_terminal = c_sub_terminal-end_of_statement i_regex = '\.' ).
  ENDMETHOD.

  METHOD add_empty_line.
    non_terminal( i_sub_terminal = c_sub_terminal-empty_line i_regex = '\( \)' ).
  ENDMETHOD.

  METHOD add_parenthesis_open.
    non_terminal( i_sub_terminal = c_sub_terminal-parenthesis_open i_regex = '\(' ).
  ENDMETHOD.

ENDCLASS.
