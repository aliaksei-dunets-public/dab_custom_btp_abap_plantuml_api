CLASS zdab_cl_uml_class_field_api DEFINITION
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

      set_static_indicator
        IMPORTING iv_static TYPE abap_bool,
      get_static_indicator
        RETURNING VALUE(rv_static) TYPE abap_bool.

  PROTECTED SECTION.
    DATA:
      mv_visibility TYPE string,
      mv_field_type TYPE string,
      mv_static     TYPE abap_bool.

    METHODS: parse_uml_code REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZDAB_CL_UML_CLASS_FIELD_API IMPLEMENTATION.


  METHOD parse_uml_code.
    " +{static}FieldName : FieldType

    rv_uml_code = |{ mv_visibility }| &&
                  |{ COND #( WHEN mv_static = abap_true THEN |{ gc_curly_brace_open }static{ gc_curly_brace_closed } | ) }| &&
                  |{ to_lower( mv_element_name ) }| &&
                  |{ COND #( WHEN mv_field_type IS NOT INITIAL THEN | : { mv_field_type }| ) }| &&
                  |{ gc_new_line }|   ##NO_TEXT.

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


  METHOD get_static_indicator.
    rv_static = mv_static.
  ENDMETHOD.


  METHOD set_field_type.
    mv_field_type = iv_field_type.
  ENDMETHOD.


  METHOD set_static_indicator.
    me->mv_static = iv_static.
  ENDMETHOD.
ENDCLASS.
