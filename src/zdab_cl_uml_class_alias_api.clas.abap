CLASS zdab_cl_uml_class_alias_api DEFINITION
  INHERITING FROM zdab_cl_uml_element_base_api
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      set_visibility
        IMPORTING iv_visibility TYPE string,
      get_visibility
        RETURNING VALUE(rv_visibility) TYPE string,

      set_interface
        IMPORTING iv_interface TYPE string,
      get_interface
        RETURNING VALUE(rv_interface) TYPE string,
      get_component
        RETURNING VALUE(rv_component) TYPE string,
      set_component
        IMPORTING iv_component TYPE string.

  PROTECTED SECTION.
    DATA:
      mv_visibility TYPE string,
      mv_interface  TYPE string,
      mv_component  TYPE string.

    METHODS: parse_uml_code REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZDAB_CL_UML_CLASS_ALIAS_API IMPLEMENTATION.


  METHOD set_visibility.
    mv_visibility = iv_visibility.
  ENDMETHOD.


  METHOD parse_uml_code.
    " +<AliasName> FOR <Interface>~<AliasName>
    rv_uml_code = |{ mv_visibility }| &&
                  |{ to_lower( mv_element_name ) } : { to_lower( mv_interface ) }~{ to_lower( mv_component ) }| &&
                  |{ gc_new_line }|   ##NO_TEXT.

  ENDMETHOD.


  METHOD set_interface.
    mv_interface = iv_interface.
  ENDMETHOD.


  METHOD get_component.
    rv_component = mv_component.
  ENDMETHOD.


  METHOD set_component.
    mv_component = iv_component.
  ENDMETHOD.


  METHOD get_interface.
    rv_interface = mv_interface.
  ENDMETHOD.


  METHOD get_visibility.
    rv_visibility = mv_visibility.
  ENDMETHOD.
ENDCLASS.
