*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

class lcx_no_match definition inheriting from cx_static_check.
endclass.

class lcx_parser_interrupt definition inheriting from cx_no_check.
endclass.

interface lif_terminal.
  types terminal_type type i.

  constants assign         type terminal_type value 0.
  constants rhs            type terminal_type value 1.
  constants empty          type terminal_type value 2.
  constants struct         type terminal_type value 3.
  constants struct2        type terminal_type value 4.
  constants iteration      type terminal_type value 5.
  constants table          type terminal_type value 6.
  constants rhs2           type terminal_type value 7.
  constants symbolname     type terminal_type value 8.
  constants number         type terminal_type value 9.
  constants non_terminal   type terminal_type value 10.
  constants text_literal   type terminal_type value 11.
  constants string_literal type terminal_type value 12.
  constants spaces         type terminal_type value 13.
endinterface.


interface lif_sub_terminal.
  types terminal_type type i.

  constants na                type terminal_type value 0.              " not applicable
  constants assign_operator   type terminal_type value 1.              " =
  constants value_operator    type terminal_type value 2.              " VALUE #(
  constants parenthesis_open  type terminal_type value 3.              " (
  constants parenthesis_close type terminal_type value 4.              " )
  constants empty_line        type terminal_type value 5.              " ( )
  constants end_of_statement  type terminal_type value 6.              " .
endinterface.
