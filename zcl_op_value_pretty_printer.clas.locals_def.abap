*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

class lcx_no_match definition INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        regex                   TYPE string
        offset                  TYPE i
        text_excerpt_offset     TYPE i
        text_excerpt_length     TYPE i
        text_excerpt            TYPE string
        textid                  like textid optional
        previous                like previous optional.
    DATA regex TYPE string.
    DATA offset TYPE i.
    DATA text_excerpt_offset TYPE i.
    DATA text_excerpt_length TYPE i.
    DATA text_excerpt TYPE string.
endclass.

class lcx_parser_interrupt definition INHERITING FROM cx_no_check.
endclass.
