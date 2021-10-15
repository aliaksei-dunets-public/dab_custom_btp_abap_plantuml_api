CLASS zdab_cl_uml_parser4class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_class_diagram_config,
        types              TYPE abap_boolean,
        constants          TYPE abap_boolean,
        alias              TYPE abap_boolean,
        attributes         TYPE abap_boolean,
        methods            TYPE abap_boolean,
        relationships      TYPE abap_boolean,
        deep_relationships TYPE abap_boolean,
      END OF ty_s_class_diagram_config.

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
        IMPORTING is_diagram_config     TYPE ty_s_class_diagram_config
        RETURNING VALUE(ro_uml_element) TYPE REF TO zdab_if_uml_element_api
        RAISING   zdab_cx_uml_parser,
      add_types
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4class
        RAISING   zdab_cx_uml_parser,
      add_constants
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4class
        RAISING   zdab_cx_uml_parser,
      add_alias
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4class
        RAISING   zdab_cx_uml_parser,
      add_attributes
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4class
        RAISING   zdab_cx_uml_parser,
      add_methods
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4class
        RAISING   zdab_cx_uml_parser,
      add_relationships
        IMPORTING is_diagram_config  TYPE ty_s_class_diagram_config
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4class
        RAISING   zdab_cx_uml_parser,
      add_deep_relationships
        IMPORTING is_diagram_config  TYPE ty_s_class_diagram_config
        RETURNING VALUE(ro_instance) TYPE REF TO zdab_cl_uml_parser4class
        RAISING   zdab_cx_uml_parser.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_class_name TYPE sxco_ao_object_name,
      mo_xco_class  TYPE REF TO if_xco_ao_class,
      mo_uml_class  TYPE REF TO zdab_cl_uml_class_api.

    METHODS:
      add_definition
        RAISING zdab_cx_uml_parser.

ENDCLASS.

CLASS zdab_cl_uml_parser4class IMPLEMENTATION.

  METHOD constructor.

    DATA: lv_dummy TYPE string.

    IF iv_name IS INITIAL.
      " 002 - Object name is missed
      MESSAGE e002(zdab_cust_plant_uml) INTO lv_dummy.
      RAISE EXCEPTION TYPE zdab_cx_uml_parser USING MESSAGE.
    ENDIF.

    mv_class_name = iv_name.
    mo_xco_class = xco_cp_abap=>class( mv_class_name ).

    IF mo_xco_class->exists( ) = abap_false.
      " 003 - The class &1 doesn't exist
      MESSAGE e003(zdab_cust_plant_uml) WITH mv_class_name INTO lv_dummy.
      RAISE EXCEPTION TYPE zdab_cx_uml_parser USING MESSAGE.
    ENDIF.

    mo_uml_class = NEW zdab_cl_uml_class_api( |{ mv_class_name }| ).
    me->add_definition( ).
  ENDMETHOD.

  METHOD get_uml_element.
    ro_uml_element = mo_uml_class.
  ENDMETHOD.

  METHOD get_uml_element_code.
    rv_uml_element_code = mo_uml_class->get_uml_code( ).
  ENDMETHOD.

  METHOD get_uml_element_name.
    rv_uml_element_name = mv_class_name.
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

    ro_uml_element = mo_uml_class.

  ENDMETHOD.

  METHOD add_definition.

******************************** CLASS DEFINITION ********************************
    DATA(ls_definition_content) = mo_xco_class->definition->content( )->get( ).

    " Set UML Class attributes
    IF ls_definition_content-abstract_indicator = abap_true.
      mo_uml_class->set_class_type( iv_class_type = zdab_if_uml_element_api=>gc_class_type-abstract ).
    ELSE.
      mo_uml_class->set_class_type( iv_class_type = zdab_if_uml_element_api=>gc_class_type-class ).
    ENDIF.

  ENDMETHOD.

  METHOD add_alias.

******************************** CLASS ALIAS **********************************

    " PUBLIC ALIAS
    LOOP AT mo_xco_class->definition->section-public->components->alias->all->get( ) INTO DATA(lo_public_alias).

      TRY.
          DATA(lo_uml_public_alias) = NEW zdab_cl_uml_class_alias_api( |{ lo_public_alias->name }| ).
          lo_uml_public_alias->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
          lo_uml_public_alias->set_interface( lo_public_alias->content( )->get_interface( )->name ).
          lo_uml_public_alias->set_component( lo_public_alias->content( )->get_component( ) ).

          mo_uml_class->add_child_element( lo_uml_public_alias ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    " PRIVATE ALIAS
    LOOP AT mo_xco_class->definition->section-private->components->alias->all->get( ) INTO DATA(lo_private_alias).
      TRY.
          DATA(lo_uml_private_alias) = NEW zdab_cl_uml_class_alias_api( |{ lo_private_alias->name }| ).
          lo_uml_private_alias->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
          lo_uml_private_alias->set_interface( lo_private_alias->content( )->get_interface( )->name ).
          lo_uml_private_alias->set_component( lo_private_alias->content( )->get_component( ) ).

          mo_uml_class->add_child_element( lo_uml_private_alias ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    " PROTECTED ALIAS
    LOOP AT mo_xco_class->definition->section-protected->components->alias->all->get( ) INTO DATA(lo_protected_alias).
      TRY.
          DATA(lo_uml_protected_alias) = NEW zdab_cl_uml_class_alias_api( |{ lo_protected_alias->name }| ).
          lo_uml_protected_alias->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).
          lo_uml_protected_alias->set_interface( lo_protected_alias->content( )->get_interface( )->name ).
          lo_uml_protected_alias->set_component( lo_protected_alias->content( )->get_component( ) ).

          mo_uml_class->add_child_element( lo_uml_protected_alias ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.

  METHOD add_constants.

******************************** CLASS CONSTANTS **********************************

    " PUBLIC CONSTANTS
    LOOP AT mo_xco_class->definition->section-public->components->constant->all->get( ) INTO DATA(lo_public_constants).

      DATA(lo_uml_public_constants) = NEW zdab_cl_uml_class_constant_api( |{ lo_public_constants->name }| ).
      lo_uml_public_constants->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).

      DATA(lo_public_constant_content) = lo_public_constants->content( ).
      IF lo_public_constant_content IS BOUND.
        lo_uml_public_constants->set_field_type( lo_public_constant_content->get_typing_definition( )->get_value( ) ).
        lo_uml_public_constants->set_value( lo_public_constant_content->get_value( ) ).
      ENDIF.

      mo_uml_class->add_child_element( lo_uml_public_constants ).

    ENDLOOP.

    " PRIVATE CONSTANTS
    LOOP AT mo_xco_class->definition->section-private->components->constant->all->get( ) INTO DATA(lo_private_constants).

      DATA(lo_uml_private_constants) = NEW zdab_cl_uml_class_constant_api( |{ lo_private_constants->name }| ).
      lo_uml_private_constants->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).

      DATA(lo_private_constant_content) = lo_private_constants->content( ).
      IF lo_private_constant_content IS BOUND.
        lo_uml_private_constants->set_field_type( lo_private_constant_content->get_typing_definition( )->get_value( ) ).
        lo_uml_private_constants->set_value( lo_private_constant_content->get_value( ) ).
      ENDIF.

      mo_uml_class->add_child_element( lo_uml_private_constants ).

    ENDLOOP.

    " PROTECTED CONSTANTS
    LOOP AT mo_xco_class->definition->section-protected->components->constant->all->get( ) INTO DATA(lo_protected_constants).

      DATA(lo_uml_protected_constants) = NEW zdab_cl_uml_class_constant_api( |{ lo_protected_constants->name }| ).
      lo_uml_protected_constants->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).

      DATA(lo_protected_constant_content) = lo_protected_constants->content( ).
      IF lo_protected_constant_content IS BOUND.
        lo_uml_protected_constants->set_field_type( lo_protected_constant_content->get_typing_definition( )->get_value( ) ).
        lo_uml_protected_constants->set_value( lo_protected_constant_content->get_value( ) ).
      ENDIF.

      mo_uml_class->add_child_element( lo_uml_protected_constants ).

    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.

  METHOD add_types.

******************************** CLASS TYPES *************************************

    " PUBLIC TYPES
    LOOP AT mo_xco_class->definition->section-public->components->type->all->get( ) INTO DATA(lo_public_types).

      DATA(lo_uml_public_types) = NEW zdab_cl_uml_class_types_api( |{ lo_public_types->name }| ).
      lo_uml_public_types->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
      lo_uml_public_types->set_field_type( lo_public_types->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_class->add_child_element( lo_uml_public_types ).

    ENDLOOP.

    " PRIVATE TYPES
    LOOP AT mo_xco_class->definition->section-private->components->type->all->get( ) INTO DATA(lo_private_types).

      DATA(lo_uml_private_types) = NEW zdab_cl_uml_class_types_api( |{ lo_private_types->name }| ).
      lo_uml_private_types->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
      lo_uml_private_types->set_field_type( lo_private_types->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_class->add_child_element( lo_uml_private_types ).

    ENDLOOP.

    " PROTECTED TYPES
    LOOP AT mo_xco_class->definition->section-protected->components->type->all->get( ) INTO DATA(lo_protected_types).

      DATA(lo_uml_protected_types) = NEW zdab_cl_uml_class_types_api( |{ lo_protected_types->name }| ).
      lo_uml_protected_types->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).
      lo_uml_protected_types->set_field_type( lo_protected_types->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_class->add_child_element( lo_uml_protected_types ).

    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.

  METHOD add_attributes.

******************************** CLASS DATA **************************************

    " PUBLIC CLASS DATA
    LOOP AT mo_xco_class->definition->section-public->components->class_data->all->get( ) INTO DATA(lo_public_class_data).

      DATA(lo_uml_public_class_data) = NEW zdab_cl_uml_class_field_api( |{ lo_public_class_data->name }| ).
      lo_uml_public_class_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
      lo_uml_public_class_data->set_static_indicator( abap_true ).
      lo_uml_public_class_data->set_field_type( lo_public_class_data->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_class->add_child_element( lo_uml_public_class_data ).

    ENDLOOP.

    " PRIVATE CLASS DATA
    LOOP AT mo_xco_class->definition->section-private->components->class_data->all->get( ) INTO DATA(lo_private_class_data).

      DATA(lo_uml_private_class_data) = NEW zdab_cl_uml_class_field_api( |{ lo_private_class_data->name }| ).
      lo_uml_private_class_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-private ).
      lo_uml_private_class_data->set_static_indicator( abap_true ).
      lo_uml_private_class_data->set_field_type( lo_private_class_data->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_class->add_child_element( lo_uml_private_class_data ).

    ENDLOOP.

    " PROTECTED CLASS DATA
    LOOP AT mo_xco_class->definition->section-protected->components->class_data->all->get( ) INTO DATA(lo_protected_class_data).

      DATA(lo_uml_protected_class_data) = NEW zdab_cl_uml_class_field_api( |{ lo_protected_class_data->name }| ).
      lo_uml_protected_class_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).
      lo_uml_protected_class_data->set_static_indicator( abap_true ).
      lo_uml_protected_class_data->set_field_type( lo_protected_class_data->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_class->add_child_element( lo_uml_protected_class_data ).

    ENDLOOP.

    " PUBLIC DATA
    LOOP AT mo_xco_class->definition->section-public->components->data->all->get( ) INTO DATA(lo_public_data).

      DATA(lo_uml_public_data) = NEW zdab_cl_uml_class_field_api( |{ lo_public_data->name }| ).
      lo_uml_public_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
      lo_uml_public_data->set_field_type( lo_public_data->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_class->add_child_element( lo_uml_public_data ).

    ENDLOOP.

    " PRIVATE DATA
    LOOP AT mo_xco_class->definition->section-private->components->data->all->get( ) INTO DATA(lo_private_data).

      DATA(lo_uml_private_data) = NEW zdab_cl_uml_class_field_api( |{ lo_private_data->name }| ).
      lo_uml_private_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-private ).
      lo_uml_private_data->set_field_type( lo_private_data->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_class->add_child_element( lo_uml_private_data ).

    ENDLOOP.

    " PROTECTED DATA
    LOOP AT mo_xco_class->definition->section-protected->components->data->all->get( ) INTO DATA(lo_protected_data).

      DATA(lo_uml_protected_data) = NEW zdab_cl_uml_class_field_api( |{ lo_protected_data->name }| ).
      lo_uml_protected_data->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).
      lo_uml_protected_data->set_field_type( lo_protected_data->content( )->get_typing_definition( )->get_value( ) ).

      mo_uml_class->add_child_element( lo_uml_protected_data ).

    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.

  METHOD add_methods.

******************************** CLASS METHODS **************************************

    " PUBLIC CLASS METHOD
    LOOP AT mo_xco_class->definition->section-public->components->class_method->all->get( ) INTO DATA(lo_public_class_methods).

      DATA(lo_uml_public_class_method) = NEW zdab_cl_uml_class_method_api( |{ lo_public_class_methods->name }| ).
      lo_uml_public_class_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).
      lo_uml_public_class_method->set_static_indicator( abap_true ).

      mo_uml_class->add_child_element( lo_uml_public_class_method ).

    ENDLOOP.

    " PRIVATE CLASS METHOD
    LOOP AT mo_xco_class->definition->section-private->components->class_method->all->get( ) INTO DATA(lo_private_class_methods).

      DATA(lo_uml_private_class_method) = NEW zdab_cl_uml_class_method_api( |{ lo_private_class_methods->name }| ).
      lo_uml_private_class_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-private ).
      lo_uml_private_class_method->set_static_indicator( abap_true ).

      mo_uml_class->add_child_element( lo_uml_private_class_method ).

    ENDLOOP.

    " PROTECTED CLASS METHOD
    LOOP AT mo_xco_class->definition->section-protected->components->class_method->all->get( ) INTO DATA(lo_protected_class_methods).

      DATA(lo_uml_protected_class_method) = NEW zdab_cl_uml_class_method_api( |{ lo_protected_class_methods->name }| ).
      lo_uml_protected_class_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).
      lo_uml_protected_class_method->set_static_indicator( abap_true ).

      mo_uml_class->add_child_element( lo_uml_protected_class_method ).

    ENDLOOP.

    " PUBLIC METHODS
    LOOP AT mo_xco_class->definition->section-public->components->method->all->get( ) INTO DATA(lo_public_method).

      DATA(lo_uml_public_method) = NEW zdab_cl_uml_class_method_api( |{ lo_public_method->name }| ).
      lo_uml_public_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-public ).

      mo_uml_class->add_child_element( lo_uml_public_method ).

    ENDLOOP.

    " PRIVATE METHODS
    LOOP AT mo_xco_class->definition->section-private->components->method->all->get( ) INTO DATA(lo_private_method).

      DATA(lo_uml_private_method) = NEW zdab_cl_uml_class_method_api( |{ lo_private_method->name }| ).
      lo_uml_private_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-private ).

      mo_uml_class->add_child_element( lo_uml_private_method ).

    ENDLOOP.

    " PROTECTED METHODS
    LOOP AT mo_xco_class->definition->section-protected->components->method->all->get( ) INTO DATA(lo_protected_method).

      DATA(lo_uml_protected_method) = NEW zdab_cl_uml_class_method_api( |{ lo_protected_method->name }| ).
      lo_uml_protected_method->set_visibility( zdab_if_uml_element_api=>gc_visibility-protected ).

      mo_uml_class->add_child_element( lo_uml_protected_method ).

    ENDLOOP.


    ro_instance = me.

  ENDMETHOD.

  METHOD add_relationships.

    DATA:
      lo_uml_class_parser     TYPE REF TO zdab_cl_uml_parser4class,
      lo_uml_interface_parser TYPE REF TO zdab_cl_uml_parser4interface.

******************************** CLASS RELATIONSHIPS ****************************

    DATA(ls_definition_content) = mo_xco_class->definition->content( )->get( ).

    " SUPER CLASS
    IF ls_definition_content-superclass IS NOT INITIAL.
      mo_uml_class->set_super_class( CONV #( ls_definition_content-superclass->name ) ).
    ENDIF.

    " INTERFACES
    LOOP AT ls_definition_content-interfaces INTO DATA(lo_interface).
      mo_uml_class->set_interface( CONV #( lo_interface->name ) ).
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

*    DATA(ls_definition_content) = mo_xco_class->definition->content( )->get( ).

******************************** CLASS RELATIONSHIPS ****************************
*" INTERFACES
*LOOP AT ls_definition_content-interfaces INTO DATA(lo_interface).
*  mo_uml_class->set_interface( NEW lcl_abap_interface2uml_parser( lo_interface->name )->parse_abap2uml_element(
*                                                                                          iv_get_details   = abap_true
*                                                                                          iv_get_relations = abap_true
*                                                                                        ) ).

*  mo_uml_class->set_interface( lo_class_parser->get_uml_element( ) ).
*ENDLOOP.


    ro_instance = me.

  ENDMETHOD.

ENDCLASS.
