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
           BEGIN OF ENUM enum_terminal STRUCTURE c_terminal,
             assign,
             rhs,
             empty,
             struct,
             struct2,
             iteration,
             table,
             rhs2,
             symbolname,
             number,
             non_terminal,
             text_literal,
             string_literal,
             spaces,
           END OF ENUM enum_terminal STRUCTURE c_terminal,
           BEGIN OF ENUM enum_sub_terminal STRUCTURE c_sub_terminal,
             na,                    " not applicable
             assign_operator,       " =
             value_operator,        " VALUE #(
             parenthesis_open,      " (
             parenthesis_close,     " )
             empty_line,            " ( )
             end_of_statement,      " .
           END OF ENUM enum_sub_terminal STRUCTURE c_sub_terminal,
           BEGIN OF ty_trace,
             parent_sync_point TYPE i,
             terminal          TYPE enum_terminal,
             sub_terminal      TYPE enum_sub_terminal,
             offset            TYPE i,
             length            TYPE i,
             sync_point        TYPE i,
             text              TYPE string,
           END OF ty_trace,
           BEGIN OF ty_sync_point,
             terminal     TYPE enum_terminal,
             sub_terminal TYPE enum_sub_terminal,
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
      assign
        RAISING
          lcx_no_match,
      spaces,
      rhs
        RAISING
          lcx_no_match,
      empty
        RAISING
          lcx_no_match,
      struct
        RAISING
          lcx_no_match,
      struct2
        RAISING
          lcx_no_match,
      table
        RAISING
          lcx_no_match,
      rhs2
        RAISING
          lcx_no_match,
      symbolname
        RAISING
          lcx_no_match,
      number
        RAISING
          lcx_no_match,
      non_terminal
        IMPORTING
          sub_terminal TYPE enum_sub_terminal
          regex        TYPE csequence
*        PREFERRED PARAMETER
        RAISING
          lcx_no_match,
      text_literal
        RAISING
          lcx_no_match,
      string_literal
        RAISING
          lcx_no_match,
      regex
        IMPORTING
          regex TYPE csequence
        RAISING
          lcx_no_match,
      add_to_trace,
      set_sync_point
        IMPORTING
          i_terminal        TYPE enum_terminal
          i_sub_terminal    TYPE enum_sub_terminal DEFAULT c_sub_terminal-na
        RETURNING
          VALUE(sync_point) TYPE i,
      reset_to_sync_point
        IMPORTING
          sync_point TYPE i,
      cancel_sync_point
        IMPORTING
          sync_point TYPE i.
    CONSTANTS:
      c_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>cr_lf.

ENDCLASS.



CLASS zcl_op_value_pretty_printer IMPLEMENTATION.

  METHOD get_spaces.
    DATA(spaces_no) = steering[ lines( steering ) ]-offset_no.
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

    DATA(x_lines) = lines( steering ).

    DO x_lines TIMES.
      DATA(line) = steering[ x_lines ].
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
    DATA(x_lines) = lines( steering ).

    DO x_lines TIMES.
      DATA(line) = steering[ x_lines ].
      DELETE steering INDEX x_lines.
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
*    assign : symbolname = RHS
    TRY.
        spaces( ).
        DATA(point) = set_sync_point( c_terminal-assign ).
        symbolname( ).
        non_terminal( sub_terminal = c_sub_terminal-assign_operator regex = '=' ).
        rhs( ).
        non_terminal( sub_terminal = c_sub_terminal-end_of_statement regex = '\.' ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD rhs.
*    RHS : number | text | string | empty | struct | table
    spaces( ).
    DATA(point) = set_sync_point( c_terminal-rhs ).
    TRY.
        TRY.
            number( ).
          CATCH lcx_no_match.
            TRY.
                text_literal( ).
              CATCH lcx_no_match.
                TRY.
                    string_literal( ).
                  CATCH lcx_no_match.
                    TRY.
                        empty( ).
                      CATCH lcx_no_match.
                        TRY.
                            struct( ).
                          CATCH lcx_no_match.
                            table( ).
                        ENDTRY.
                    ENDTRY.
                ENDTRY.
            ENDTRY.
        ENDTRY.
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
    add_to_trace( ).
  ENDMETHOD.

  METHOD empty.
*    empty : 'VALUE #( )'
    TRY.
        spaces( ).
        DATA(point) = set_sync_point( c_terminal-empty ).
        regex( `VALUE #\( \)` ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD struct.
*    struct : 'VALUE #(' struct2 ')'
    TRY.
        spaces( ).
        DATA(point) = set_sync_point( c_terminal-struct ).
        non_terminal( sub_terminal = c_sub_terminal-value_operator regex = `VALUE #\(` ).
        struct2( ).
        non_terminal( sub_terminal = c_sub_terminal-parenthesis_close regex = `\)` ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD struct2.
*    struct2 : ( symbolname '=' RHS )+
    TRY.
        spaces( ).
        DATA(point) = set_sync_point( c_terminal-struct2 ).
        set_sync_point( c_terminal-iteration ).
        symbolname( ).
        non_terminal( sub_terminal = c_sub_terminal-assign_operator regex = '=' ).
        rhs( ).
        add_to_trace( ).
        DO.
          TRY.
              spaces( ).
              DATA(point2) = set_sync_point( c_terminal-iteration ).
              TRY.
                  symbolname( ).
                CATCH lcx_no_match.
                  cancel_sync_point( point2 ).
                  EXIT.
              ENDTRY.
              non_terminal( sub_terminal = c_sub_terminal-assign_operator regex = '=' ).
              rhs( ).
              add_to_trace( ).
            CLEANUP.
              reset_to_sync_point( point2 ).
          ENDTRY.
        ENDDO.
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD table.
*    table : 'VALUE #(' ( '( )' | ( '(' RHS2 ')' )+ ) ')'
    TRY.
        spaces( ).
        DATA(point) = set_sync_point( c_terminal-table ).
        non_terminal( sub_terminal = c_sub_terminal-value_operator regex = `VALUE #\(` ).
        TRY.
            non_terminal( sub_terminal = c_sub_terminal-empty_line regex = '\( \)' ).
          CATCH lcx_no_match.
            DO.
              TRY.
                  spaces( ).
                  DATA(point2) = set_sync_point( c_terminal-iteration ).
                  TRY.
                      non_terminal( sub_terminal = c_sub_terminal-parenthesis_open regex = '\(' ).
                    CATCH lcx_no_match.
                      cancel_sync_point( point2 ).
                      EXIT.
                  ENDTRY.
                  rhs2( ).
                  non_terminal( sub_terminal = c_sub_terminal-parenthesis_close regex = '\)' ).
                  add_to_trace( ).
                CLEANUP.
                  reset_to_sync_point( point2 ).
              ENDTRY.
            ENDDO.

        ENDTRY.
        non_terminal( sub_terminal = c_sub_terminal-parenthesis_close regex = `\)` ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD rhs2.
*    RHS2 : number | text | string | empty | struct2
    TRY.
        spaces( ).
        DATA(point) = set_sync_point( c_terminal-rhs2 ).
        TRY.
            number( ).
          CATCH lcx_no_match.
            TRY.
                text_literal( ).
              CATCH lcx_no_match.
                TRY.
                    string_literal( ).
                  CATCH lcx_no_match.
                    TRY.
                        empty( ).
                      CATCH lcx_no_match.
                        struct2( ).
                    ENDTRY.
                ENDTRY.
            ENDTRY.
        ENDTRY.
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD symbolname.
    TRY.
        spaces( ).
        DATA(point) = set_sync_point( c_terminal-symbolname ).
        " SCALAR
        " STRUCTURE
        " TABLE
        " STRUCTURE-COMPONENT
        " TABLE[1]
        " TABLE[1]-COMPONENT
        " VAR_2
        " /NAMESPACE/VARIABLE
        regex( `[A-Z][A-Z0-9_/\-\[\]]+` ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD number.
    TRY.
        spaces( ).
        DATA(point) = set_sync_point( c_terminal-number ).
        regex( `[0-9]+` ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD non_terminal.
    TRY.
        spaces( ).
        DATA(point) = set_sync_point( i_terminal = c_terminal-non_terminal i_sub_terminal = sub_terminal ).
        regex( regex ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD text_literal.
    TRY.
        spaces( ).
        DATA(point) = set_sync_point( c_terminal-text_literal ).
        regex( `'(?:''|[^'])+'` ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD string_literal.
    TRY.
        spaces( ).
        DATA(point) = set_sync_point( c_terminal-string_literal ).
        regex( '`(?:``|[^`])+`' ).
        add_to_trace( ).
      CLEANUP.
        reset_to_sync_point( point ).
    ENDTRY.
  ENDMETHOD.

  METHOD spaces.
    IF offset >= strlen( text ).
      RETURN.
    ENDIF.
    FIND FIRST OCCURRENCE OF REGEX '^\s+' IN text+offset MATCH LENGTH DATA(length).
    IF sy-subrc = 0.
      APPEND VALUE ty_trace(
              "LET trace_line = REF #( COND #( WHEN trace_tab Is not initial then trace_tab[ lines( trace_tab ) ] ) IN
              sync_point        = 0
              parent_sync_point = 0
              terminal          = c_terminal-spaces
              sub_terminal      = c_sub_terminal-na
              offset            = offset
              length            = length
              "text              = repeat( val = ` ` occ = length )
          ) TO trace_tab.
      offset = offset + length.
    ENDIF.
  ENDMETHOD.

  METHOD regex.
    IF offset >= strlen( text ).
      RAISE EXCEPTION TYPE lcx_no_match.
    ENDIF.
    data(regex2) = '^' && regex.
    FIND FIRST OCCURRENCE OF REGEX regex2 IN text+offset MATCH LENGTH DATA(length).
    IF sy-subrc = 0.
      offset = offset + length.
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
*    struct : 'VALUE #(' struct2 ')'
*
*    struct2 : ( symbolname '=' RHS )+
*
*    table : 'VALUE #(' ( '( )' | struct2 ( '(' RHS2 ')' )+ | ( '(' RHS ')' )+ ')'
*
*    RHS2 : number | text | string | empty | struct2
*
*    symbolname : ^[A-Z][A-Z0-9_\-]+
*
*    number : ^[0-9]+
*
*    text : ^'(?:''|[^'])+'

    text = i_unformated_value_content.
    offset = 0.
    trace_tab = VALUE #( ).
    sync_points = VALUE #( ).
    TRY.
        assign( ).
      CATCH lcx_parser_interrupt.
      CATCH lcx_no_match.
        "handle exception
    ENDTRY.

    DATA(indent) = 0.
    DATA(indent_step) = 8.
    DATA(indent_spaces) = repeat( val = ` ` occ = indent ).

    LOOP AT trace_tab REFERENCE INTO DATA(trace_line).

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

    ASSIGN sync_points[ lines( sync_points ) ] TO FIELD-SYMBOL(<sync_point>).

    APPEND VALUE ty_trace(
            LET length = offset - <sync_point>-offset IN
            sync_point        = lines( sync_points )
            parent_sync_point = lines( sync_points ) - 1
            terminal          = <sync_point>-terminal
            sub_terminal      = <sync_point>-sub_terminal
            offset            = <sync_point>-offset
            length            = length
            "text              = text+<sync_point>-offset(length)
        ) TO trace_tab.

    DELETE sync_points INDEX lines( sync_points ).

    if 0 = 1.
      raise EXCEPTION type lcx_parser_interrupt.
    endif.

  ENDMETHOD.


  METHOD set_sync_point.

    APPEND VALUE ty_sync_point(
            terminal     = i_terminal
            sub_terminal = i_sub_terminal
            offset       = offset
            trace_index  = lines( trace_tab )
        ) TO sync_points.

    sync_point = lines( sync_points ).

    if 0 = 1.
      raise EXCEPTION type lcx_parser_interrupt.
    endif.

  ENDMETHOD.


  METHOD reset_to_sync_point.

    DELETE trace_tab FROM sync_points[ sync_point ]-trace_index + 1.
    offset = sync_points[ sync_point ]-offset.
    DELETE sync_points FROM sync_point.

    if 0 = 1.
      raise EXCEPTION type lcx_parser_interrupt.
    endif.

  ENDMETHOD.


  METHOD cancel_sync_point.

    DELETE trace_tab FROM sync_points[ sync_point ]-trace_index + 1.
    DELETE sync_points FROM sync_point.

    if 0 = 1.
      raise EXCEPTION type lcx_parser_interrupt.
    endif.

  ENDMETHOD.

ENDCLASS.
