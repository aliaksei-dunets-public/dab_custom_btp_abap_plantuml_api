CLASS zdab_cl_uml_diagram_api DEFINITION
  INHERITING FROM zdab_cl_uml_element_base_api
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: parse_uml_code REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZDAB_CL_UML_DIAGRAM_API IMPLEMENTATION.


  METHOD parse_uml_code.
    " @startuml <mv_element_name>
    " <mt_child_elements>
    " @enduml

    " Add Header of the element
    rv_uml_code = |@startuml { to_lower( mv_element_name ) } { gc_new_line } { gc_new_line }|   ##NO_TEXT.

    " Add Body of the element
    rv_uml_code = |{ rv_uml_code } { get_child_uml_code( ) }|.

    " Add Relations
    rv_uml_code = |{ rv_uml_code } { gc_new_line } { zdab_cl_uml_element_relate_api=>get_instance( )->get_relations_uml_code( ) }|.

    " Add Footer of the element
    rv_uml_code = |{ rv_uml_code } { gc_new_line } @enduml|   ##NO_TEXT.

  ENDMETHOD.
ENDCLASS.
