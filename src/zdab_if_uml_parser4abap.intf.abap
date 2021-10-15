INTERFACE zdab_if_uml_parser4abap
  PUBLIC.

  METHODS:
    get_uml_element
      RETURNING VALUE(ro_uml_element) TYPE REF TO zdab_if_uml_element_api,

    get_uml_element_code
      RETURNING VALUE(rv_uml_element_code) TYPE string,

    get_uml_element_name
      RETURNING VALUE(rv_uml_element_name) TYPE string.

ENDINTERFACE.
