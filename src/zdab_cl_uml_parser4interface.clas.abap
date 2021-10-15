CLASS zdab_cl_uml_parser4interface DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_interface_diagram_config,
        types              TYPE abap_boolean,
        constants          TYPE abap_boolean,
        alias              TYPE abap_boolean,
        attributes         TYPE abap_boolean,
        methods            TYPE abap_boolean,
        relationships      TYPE abap_boolean,
        deep_relationships TYPE abap_boolean,
      END OF ty_s_interface_diagram_config.

    INTERFACES zdab_if_uml_parser4abap.

    ALIASES:
      get_uml_element      FOR zdab_if_uml_parser4abap~get_uml_element,
      get_uml_element_code FOR zdab_if_uml_parser4abap~get_uml_element_code,
      get_uml_element_name FOR zdab_if_uml_parser4abap~get_uml_element_name.

    METHODS:
      constructor
        IMPORTING iv_name TYPE sxco_ao_object_name
        RAISING   zdab_cx_uml_parser,

      generate_uml_element
        IMPORTING is_diagram_config     TYPE ty_s_interface_diagram_config
        RETURNING VALUE(ro_uml_element) TYPE REF TO zdab_if_uml_element_api
        RAISING   zdab_cx_uml_parser,

      add_types
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4interface
        RAISING   zdab_cx_uml_parser,
      add_constants
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4interface
        RAISING   zdab_cx_uml_parser,
      add_alias
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4interface
        RAISING   zdab_cx_uml_parser,
      add_attributes
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4interface
        RAISING   zdab_cx_uml_parser,
      add_methods
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4interface
        RAISING   zdab_cx_uml_parser,
      add_relationships
        IMPORTING is_diagram_config  TYPE ty_s_interface_diagram_config
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4interface
        RAISING   zdab_cx_uml_parser,
      add_deep_relationships
        IMPORTING is_diagram_config  TYPE ty_s_interface_diagram_config
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4interface
        RAISING   zdab_cx_uml_parser.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_interface_name TYPE sxco_ao_object_name,
      mo_xco_interface  TYPE REF TO if_xco_ao_interface,
      mo_uml_interface  TYPE REF TO zdab_cl_uml_class_api.
ENDCLASS.

CLASS zdab_cl_uml_parser4interface IMPLEMENTATION.

  METHOD constructor.

    DATA: lv_dummy TYPE string.

    IF iv_name IS INITIAL.
      " 002 - Object name is missed
      MESSAGE e002(zdab_cust_plant_uml) INTO lv_dummy.
      RAISE EXCEPTION TYPE zdab_cx_uml_parser USING MESSAGE.
    ENDIF.

    mv_interface_name = iv_name.
    mo_xco_interface = xco_cp_abap=>interface( mv_interface_name ).

    IF mo_xco_interface->exists( ) = abap_false.
      " 004 - The interface &1 doesn't exist
      MESSAGE e004(zdab_cust_plant_uml) WITH mv_interface_name INTO lv_dummy.
      RAISE EXCEPTION TYPE zdab_cx_uml_parser USING MESSAGE.
    ENDIF.

    mo_uml_interface = NEW zdab_cl_uml_class_api( |{ mv_interface_name }| ).
    mo_uml_interface->set_class_type( zdab_if_uml_element_api=>gc_class_type-interface ).
  ENDMETHOD.

  METHOD get_uml_element.
    ro_uml_element = mo_uml_interface.
  ENDMETHOD.

  METHOD get_uml_element_code.
    rv_uml_element_code = mo_uml_interface->get_uml_code( ).
  ENDMETHOD.

  METHOD get_uml_element_name.
    rv_uml_element_name = mv_interface_name.
  ENDMETHOD.

  METHOD generate_uml_element.

    IF is_diagram_config-alias = abap_true.
      add_alias( ).
    ENDIF.

    IF is_diagram_config-attributes = abap_true.
      add_attributes( ).
    ENDIF.

    IF is_diagram_config-constants = abap_true.
      add_constants( ).
    ENDIF.

    IF is_diagram_config-types = abap_true.
      add_types( ).
    ENDIF.

    IF is_diagram_config-methods = abap_true.
      add_methods( ).
    ENDIF.

    IF is_diagram_config-relationships = abap_true.
      add_relationships( is_diagram_config ).
    ENDIF.

    IF is_diagram_config-deep_relationships = abap_true.
      add_deep_relationships( is_diagram_config ).
    ENDIF.

    ro_uml_element = mo_uml_interface.

  ENDMETHOD.

  METHOD add_alias.

******************************** INTERFACE ALIAS **********************************

    " PUBLIC ALIAS
    LOOP AT mo_xco_interface->components->alias->all->get( ) INTO DATA(lo_public_alias).

      DATA(lo_uml_public_alias) = NEW zdab_cl_uml_class_alias_api( |{ lo_public_alias->name }| ).
      lo_uml_public_alias->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
      lo_uml_public_alias->set_interface( lo_public_alias->content( )->get_interface( )->name ).
      lo_uml_public_alias->set_component( lo_public_alias->content( )->get_component( ) ).

      mo_uml_interface->add_child_element( lo_uml_public_alias ).

    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.

  METHOD add_constants.

******************************** INTERFACE CONSTANTS **********************************

    " PUBLIC CONSTANTS
    LOOP AT mo_xco_interface->components->constant->all->get( ) INTO DATA(lo_public_constants).

      DATA(lo_uml_public_constants) = NEW zdab_cl_uml_class_constant_api( |{ lo_public_constants->name }| ).
      lo_uml_public_constants->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).

      DATA(lo_public_constant_content) = lo_public_constants->content( ).
      IF lo_public_constant_content IS BOUND.
        lo_uml_public_constants->set_field_type( lo_public_constant_content->get_typing_definition( )->get_value( ) ).
        lo_uml_public_constants->set_value( lo_public_constant_content->get_value( ) ).
      ENDIF.

      mo_uml_interface->add_child_element( lo_uml_public_constants ).

    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.

  METHOD add_types.

******************************** INTERFACE TYPES *************************************

    " PUBLIC TYPES
    LOOP AT mo_xco_interface->components->type->all->get( ) INTO DATA(lo_public_types).

      DATA(lo_uml_public_types) = NEW zdab_cl_uml_class_types_api( |{ lo_public_types->name }| ).
      lo_uml_public_types->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
      lo_uml_public_types->set_field_type( lo_public_types->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_interface->add_child_element( lo_uml_public_types ).

    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.

  METHOD add_attributes.

******************************** INTERFACE DATA **************************************

    " PUBLIC INTERFACE DATA
    LOOP AT mo_xco_interface->components->class_data->all->get( ) INTO DATA(lo_public_class_data).

      DATA(lo_uml_public_interface_data) = NEW zdab_cl_uml_class_field_api( |{ lo_public_class_data->name }| ).
      lo_uml_public_interface_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
      lo_uml_public_interface_data->set_static_indicator( abap_true ).
      lo_uml_public_interface_data->set_field_type( lo_public_class_data->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_interface->add_child_element( lo_uml_public_interface_data ).

    ENDLOOP.

    " PUBLIC DATA
    LOOP AT mo_xco_interface->components->data->all->get( ) INTO DATA(lo_public_data).

      DATA(lo_uml_public_data) = NEW zdab_cl_uml_class_field_api( |{ lo_public_data->name }| ).
      lo_uml_public_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
      lo_uml_public_data->set_field_type( lo_public_data->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_interface->add_child_element( lo_uml_public_data ).

    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.

  METHOD add_methods.

******************************** INTERFACE METHODS **************************************

    " PUBLIC INTERFACE METHOD
    LOOP AT mo_xco_interface->components->class_method->all->get( ) INTO DATA(lo_public_interface_methods).

      DATA(lo_uml_public_interface_method) = NEW zdab_cl_uml_class_method_api( |{ lo_public_interface_methods->name }| ).
      lo_uml_public_interface_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
      lo_uml_public_interface_method->set_static_indicator( abap_true ).

      mo_uml_interface->add_child_element( lo_uml_public_interface_method ).

    ENDLOOP.

    " PUBLIC METHODS
    LOOP AT mo_xco_interface->components->method->all->get( ) INTO DATA(lo_public_method).

      DATA(lo_uml_public_method) = NEW zdab_cl_uml_class_method_api( |{ lo_public_method->name }| ).
      lo_uml_public_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).

      mo_uml_interface->add_child_element( lo_uml_public_method ).

    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.

  METHOD add_relationships.

    DATA:
      lo_uml_class_parser     TYPE REF TO zdab_cl_uml_parser4class,
      lo_uml_interface_parser TYPE REF TO zdab_cl_uml_parser4interface.

******************************** CLASS RELATIONSHIPS ****************************

    " INTERFACES
    LOOP AT mo_xco_interface->content( )->get_interfaces( ) INTO DATA(lo_interface).
      mo_uml_interface->set_interface( CONV #( lo_interface->name ) ).
    ENDLOOP.

    DATA(lo_relate_api) = zdab_cl_uml_element_relate_api=>get_instance( ).

    lo_relate_api->get_related_elements(
      IMPORTING
        et_related_elements = DATA(lt_related_elements)
    ).

    LOOP AT lt_related_elements ASSIGNING FIELD-SYMBOL(<ls_related_elements>)
      WHERE target_element IS NOT BOUND.

      CASE <ls_related_elements>-target_type.
        WHEN zdab_if_uml_element_api=>gc_class_type-interface.

          lo_uml_interface_parser = NEW zdab_cl_uml_parser4interface( <ls_related_elements>-target_name ).

          lo_relate_api->set_related_element(
            EXPORTING
              iv_target_name    = <ls_related_elements>-target_name
              iv_target_type    = <ls_related_elements>-target_type
              io_target_element = lo_uml_interface_parser->get_uml_element( )
          ).

          lo_relate_api->set_related_element(
            EXPORTING
              iv_target_name    = <ls_related_elements>-target_name
              iv_target_type    = <ls_related_elements>-target_type
              io_target_element = lo_uml_interface_parser->generate_uml_element( is_diagram_config )
          ).

        WHEN zdab_if_uml_element_api=>gc_class_type-class.

          lo_uml_class_parser = NEW zdab_cl_uml_parser4class( <ls_related_elements>-target_name ).

          lo_relate_api->set_related_element(
            EXPORTING
              iv_target_name    = <ls_related_elements>-target_name
              iv_target_type    = <ls_related_elements>-target_type
              io_target_element = lo_uml_class_parser->get_uml_element( )
          ).

          lo_relate_api->set_related_element(
            EXPORTING
              iv_target_name    = <ls_related_elements>-target_name
              iv_target_type    = <ls_related_elements>-target_type
              io_target_element = lo_uml_class_parser->generate_uml_element( is_diagram_config )
          ).

      ENDCASE.

    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.

  METHOD add_deep_relationships.

******************************** CLASS RELATIONSHIPS ****************************
*" INTERFACES
*LOOP AT ls_definition_content-interfaces INTO DATA(lo_interface).
*  mo_uml_class->set_interface( NEW lcl_abap_interface2uml_parser( lo_interface->name )->parse_abap2uml_element(
*                                                                                          iv_get_details   = abap_true
*                                                                                          iv_get_relations = abap_true
*                                                                                        ) ).

*  mo_uml_class->set_interface( lo_interface_parser->get_uml_element( ) ).
*ENDLOOP.


    ro_instance = me.

  ENDMETHOD.

ENDCLASS.
