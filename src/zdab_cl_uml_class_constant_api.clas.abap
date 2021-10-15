CLASS zdab_cl_uml_class_constant_api DEFINITION
  INHERITING FROM zdab_cl_uml_element_base_api
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      set_visibility
        IMPORTING iv_visibility TYPE string,

      get_visibility
        RETURNING VALUE(rv_visibility) TYPE string,

      set_field_type
        IMPORTING iv_field_type TYPE string,

      get_field_type
        RETURNING VALUE(rv_field_type) TYPE string,

      get_value
        RETURNING VALUE(rv_value) TYPE string,

      set_value
        IMPORTING iv_value TYPE string.

  PROTECTED SECTION.
    DATA:
      mv_visibility TYPE string,
      mv_field_type TYPE string,
      mv_value      TYPE string.

    METHODS: parse_uml_code REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZDAB_CL_UML_CLASS_CONSTANT_API IMPLEMENTATION.


  METHOD parse_uml_code.
    " +FieldName : FieldType

    rv_uml_code = |{ mv_visibility }| &&
                  |{ to_lower( mv_element_name ) }| &&
                  |{ COND #( WHEN mv_field_type IS NOT INITIAL THEN | : { mv_field_type }| ) }| &&
                  |{ COND #( WHEN mv_value IS NOT INITIAL THEN | - { to_lower( mv_value ) }| ) }| &&
                  |{ gc_new_line }|.

  ENDMETHOD.


  METHOD get_visibility.
    rv_visibility = mv_visibility.
  ENDMETHOD.


  METHOD set_visibility.
    mv_visibility = iv_visibility.
  ENDMETHOD.


  METHOD get_field_type.
    rv_field_type = mv_field_type.
  ENDMETHOD.


  METHOD set_field_type.
    mv_field_type = iv_field_type.
  ENDMETHOD.


  METHOD get_value.
    rv_value = mv_value.
  ENDMETHOD.


  METHOD set_value.
    mv_value = iv_value.
  ENDMETHOD.
ENDCLASS.
