*&---------------------------------------------------------------------*
*& Report ZRDEMO_SALV_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrdemo_salv_01.

CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_ida TYPE REF TO if_salv_gui_table_ida.

    METHODS:
      handle_db_click FOR EVENT double_click OF if_salv_gui_table_display_opt.

    METHODS:
      handle_action FOR EVENT function_selected OF if_salv_gui_toolbar_ida.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: o_ida TYPE REF TO if_salv_gui_table_ida.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD constructor.
    o_ida = io_ida.
  ENDMETHOD.
  METHOD handle_action.
    DATA:
      ls_vbak        TYPE vbak,
      lt_named_range TYPE if_salv_service_types=>yt_named_ranges.
    IF o_ida IS BOUND.
      IF o_ida->selection( )->is_row_selected( ).
        " Get Selected row
        o_ida->selection( )->get_selected_row( IMPORTING es_row = ls_vbak ).
        " Display Item
        DATA(lo_ida) = cl_salv_gui_table_ida=>create( iv_table_name = 'VBAP' ).
        INSERT VALUE #( name = 'VBELN'
                         sign = 'I'
                         option = 'EQ'
                         low = ls_vbak-vbeln ) INTO TABLE lt_named_range.
        " Set
        lo_ida->set_select_options( it_ranges = lt_named_range ).

        " Display
        lo_ida->fullscreen( )->display( ).

      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD handle_db_click.
    DATA:
      ls_vbak        TYPE vbak,
      lt_named_range TYPE if_salv_service_types=>yt_named_ranges.
    IF o_ida IS BOUND.
      IF o_ida->selection( )->is_row_selected( ).
        " Get Selected row
        o_ida->selection( )->get_selected_row( IMPORTING es_row = ls_vbak ).
        " Display Item
        DATA(lo_ida) = cl_salv_gui_table_ida=>create( iv_table_name = 'VBAP' ).
        INSERT VALUE #( name = 'VBELN'
                         sign = 'I'
                         option = 'EQ'
                         low = ls_vbak-vbeln ) INTO TABLE lt_named_range.
        " Set
        lo_ida->set_select_options( it_ranges = lt_named_range ).

        " Display
        lo_ida->fullscreen( )->display( ).

      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
DATA: lv_vkorg TYPE vkorg.
SELECT-OPTIONS s_vkorg FOR lv_vkorg.

START-OF-SELECTION.
  " Get ref
  CHECK cl_salv_gui_table_ida=>db_capabilities( )->is_table_supported( iv_ddic_table_name = 'VBAK' ).

  " Get IDA
  DATA(o_ida) = cl_salv_gui_table_ida=>create( iv_table_name = 'VBAK' ).

  " Check maximum rows recomended
  IF cl_salv_gui_table_ida=>db_capabilities( )->is_max_rows_recommended( ).
    o_ida->set_maximum_number_of_rows( iv_number_of_rows =  200 ).
  ENDIF.

  " Filter
  DATA(o_sel) = NEW cl_salv_range_tab_collector( ).
  IF o_sel IS BOUND.
    o_sel->add_ranges_for_name(  iv_name = 'VKORG' it_ranges = s_vkorg[] ).
    " Collect range table
    o_sel->get_collected_ranges( IMPORTING et_named_ranges = DATA(lt_range)  ).
  ENDIF.

  " Set select Options
  o_ida->set_select_options( it_ranges = lt_range ).


  " List of fields to be displayed
  DATA: lt_field_names TYPE if_salv_gui_types_ida=>yts_field_name.
  lt_field_names =  VALUE #(
  ( CONV string('KUNNR') )
  ( CONV string('NETWR') )
  ( CONV string('SPART') )
  ( CONV string('VBELN') )
  ( CONV string('VKORG') )
  ( CONV string('VTWEG') )
  ( CONV string('WAERK') ) ).
  o_ida->field_catalog( )->get_available_fields( IMPORTING ets_field_names = DATA(lt_fields) ).
  o_ida->field_catalog( )->set_available_fields( EXPORTING its_field_names = lt_field_names ).

  " Subtotals
  DATA: lt_sort_order TYPE if_salv_gui_types_ida=>yt_sort_rule.
  INSERT VALUE #(   field_name = 'KUNNR'
                    is_grouped = abap_true ) INTO TABLE lt_sort_order.
  INSERT VALUE #(   field_name = 'WAERK'
                    is_grouped = abap_true ) INTO TABLE lt_sort_order.

  o_ida->default_layout( )->set_sort_order( EXPORTING it_sort_order = lt_sort_order ).

  " Set currency ref field
  o_ida->field_catalog( )->set_currency_reference_field( EXPORTING iv_amount_field_name = 'NETWR'
                                                            iv_currency_field_name = 'WAERK').
**********************************************************************
  " Enable Double Click
  o_ida->display_options( )->enable_double_click( ).
  o_ida->selection( )->set_selection_mode( EXPORTING iv_mode = 'SINGLE' ).
  DATA(o_handler) = NEW lcl_handler( io_ida = o_ida ).
  SET HANDLER o_handler->handle_db_click FOR o_ida->display_options( ).
**********************************************************************

**********************************************************************
* Add button to Toolbar
  o_ida->toolbar( )->add_button( EXPORTING iv_fcode = 'DISP' iv_text = 'Display' ).
  SET HANDLER  o_handler->handle_action FOR o_ida->toolbar( ).
**********************************************************************
  " Display
  o_ida->fullscreen( )->display( ).
