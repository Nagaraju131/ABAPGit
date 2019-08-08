*&---------------------------------------------------------------------*
*& Report ZRDEMO_SALV_02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZRDEMO_SALV_02.

CLASS lcl_cal_fields DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_salv_ida_calc_field_handler.
    TYPES:
      BEGIN OF ty_custom_fields,
        taxamount TYPE netwr,
      END OF ty_custom_fields.
    METHODS: constructor IMPORTING iv_amnt TYPE i.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      o_ida    TYPE REF TO if_salv_gui_table_ida,
      g_taxper TYPE i.
ENDCLASS.
CLASS lcl_cal_fields IMPLEMENTATION.
  METHOD constructor.

    g_taxper = iv_amnt.
  ENDMETHOD.
  METHOD if_salv_ida_calc_field_handler~get_calc_field_structure.
    ro_calc_field_structure ?= cl_abap_typedescr=>describe_by_name( 'TY_CUSTOM_FIELDS' ).
  ENDMETHOD.

  METHOD if_salv_ida_calc_field_handler~get_requested_fields.
    " Database table field name to be used
    rts_db_field_name = VALUE #( ( CONV string('NETWR') ) ). "tax amount
  ENDMETHOD.

  METHOD  if_salv_ida_calc_field_handler~calculate_line.
    DATA: ls_vbak        TYPE vbak,
          ls_calc_fields TYPE ty_custom_fields.
    ls_vbak = is_data_base_line.
    ls_calc_fields-taxamount = ( ls_vbak-netwr * g_taxper ) / 100.
    es_calculated_fields = ls_calc_fields.
  ENDMETHOD.

  METHOD if_salv_ida_calc_field_handler~end_page.

  ENDMETHOD.

  METHOD if_salv_ida_calc_field_handler~start_page.

  ENDMETHOD.
ENDCLASS.
DATA: lv_vkorg TYPE vkorg.
SELECT-OPTIONS s_vkorg FOR lv_vkorg.
PARAMETERS p_taxper TYPE i.

START-OF-SELECTION.
  " Get ref
  CHECK cl_salv_gui_table_ida=>db_capabilities( )->is_table_supported( iv_ddic_table_name = 'VBAK' ).

  " Get IDA
  DATA(o_ida) = cl_salv_gui_table_ida=>create( iv_table_name = 'VBAK'
  io_calc_field_handler = NEW lcl_cal_fields( iv_amnt = p_taxper ) ).

  " Check maximum rows recomended
  IF cl_salv_gui_table_ida=>db_capabilities( )->is_max_rows_recommended( ).
    o_ida->set_maximum_number_of_rows( iv_number_of_rows =  200 ).
  ENDIF.

  " Filter
  IF s_vkorg[] IS NOT INITIAL.
    DATA(o_sel) = NEW cl_salv_range_tab_collector( ).
    IF o_sel IS BOUND.
      o_sel->add_ranges_for_name(  iv_name = 'VKORG' it_ranges = s_vkorg[] ).
      " Collect range table
      o_sel->get_collected_ranges( IMPORTING et_named_ranges = DATA(lt_range)  ).
    ENDIF.

    " Set select Options
    o_ida->set_select_options( it_ranges = lt_range ).
  ENDIF.

  " List of fields to be displayed
  DATA: lt_field_names TYPE if_salv_gui_types_ida=>yts_field_name.
  lt_field_names =  VALUE #(
  ( CONV string('KUNNR') )
  ( CONV string('NETWR') )
  ( CONV string('SPART') )
  ( CONV string('VBELN') )
  ( CONV string('VKORG') )
  ( CONV string('VTWEG') )
  ( CONV string('TAXAMOUNT') )
  ( CONV string('WAERK') ) ).
  o_ida->field_catalog( )->get_available_fields( IMPORTING ets_field_names = DATA(lt_fields) ).
  o_ida->field_catalog( )->set_available_fields( EXPORTING its_field_names = lt_field_names ).

  " Set currency ref field
  o_ida->field_catalog( )->set_currency_reference_field( EXPORTING iv_amount_field_name = 'NETWR'
                                                            iv_currency_field_name = 'WAERK').

  " Add authorization Object

*  o_ida->add_authorization_for_object( EXPORTING
*    iv_authorization_object = 'ZVKORG'
*    it_activities =
*    VALUE if_salv_gui_types_ida=>yt_activities( ( auth_field = 'ACTVT' value  = '03' ) )
*    it_field_mapping =
*    VALUE if_salv_gui_types_ida=>yt_field_mapping( ( auth_field = 'ZVKORG' view_field = 'VKORG' ) )
*   ).

  " Display
  o_ida->fullscreen( )->display( ).
