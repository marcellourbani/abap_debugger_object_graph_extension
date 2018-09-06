class zcl_debug_obj_graph_factory definition public final create public .

  public section.
    class-methods create importing i_max_tab_lines type i default 200
                         returning value(r_result) type ref to zcl_debug_obj_graph_factory.

    methods: create_graph importing root         type string
                          returning value(graph) type ref to zcl_abap_graph.

    data: max_tab_lines type i read-only,
          logs          type string read-only.

  private section.
    types: begin of ty_data_desc,
             name          type string,
             descr         type ref to cl_tpda_script_data_descr,
             refname       type string,
             valuetext     type string,
             metatype      type tpda_scr_quick_info-metatype,
             hascomponents type abap_bool,
             istable       type abap_bool,
             isreference   type abap_bool,
             haschildren   type abap_bool,
           end of ty_data_desc,
           begin of ty_component,
             compname   type string,
             visibility type string,
             value      type string,
             desc       type ty_data_desc,
           end of ty_component,
           tt_component type table of ty_component with default key.

    data: graph      type ref to zcl_abap_graph.

    methods create_node_new importing desc        type zcl_debug_obj_graph_factory=>ty_data_desc
                            returning value(node) type ref to zif_abap_graph_node.
    methods get_or_create_node importing name          type string
                               returning value(nodeid) type string.

    methods get_node_id importing name      type string
                        returning value(id) type string.


    methods escapevalue importing original       type string
                        returning value(escaped) type string.

    methods get_data_desc importing name        type string
                          returning value(desc) type ty_data_desc.

    methods create_table_node
      importing desc        type zcl_debug_obj_graph_factory=>ty_data_desc
      returning value(node) type ref to zif_abap_graph_node.

    methods create_record_node
      importing desc        type zcl_debug_obj_graph_factory=>ty_data_desc
      returning value(node) type ref to zif_abap_graph_node.

    methods create_simple_node
      importing desc        type zcl_debug_obj_graph_factory=>ty_data_desc
      returning value(node) type ref to zif_abap_graph_node.
    methods get_components
      importing desc              type zcl_debug_obj_graph_factory=>ty_data_desc
      returning value(components) type zcl_debug_obj_graph_factory=>tt_component.
    methods log
      importing
        entry type string.
    methods add_record_component
      importing
                record             type ref to zcl_abap_graph_node_record
                component          type zcl_debug_obj_graph_factory=>ty_component
      returning value(componentid) type string.
    methods get_comp_value
      importing
        i_component     type zcl_debug_obj_graph_factory=>ty_component
      returning
        value(r_result) type string.
    methods decode_visibility
      importing
        acckind         type i
      returning
        value(r_result) type string.


endclass.



class zcl_debug_obj_graph_factory implementation.

  method create.

    create object r_result.

    r_result->max_tab_lines = i_max_tab_lines.

  endmethod.

  method create_graph.
    data: desc type zcl_debug_obj_graph_factory=>ty_data_desc.

    graph = zcl_abap_graph=>create( ).
    me->graph = graph.

    desc = get_data_desc( root ).

    create_node_new( desc ).

  endmethod.

  method get_node_id.
    id = name.
  endmethod.

  method escapevalue.
    escaped = original.
  endmethod.

  method get_data_desc.
    data: elem   type ref to cl_tpda_script_elemdescr,
          string type ref to cl_tpda_script_stringdescr,
          table  type ref to cl_tpda_script_tabledescr,
          info   type tpda_scr_quick_info,
          ex     type ref to cx_root.

    field-symbols: <symdataref> type tpda_sys_symbdatref,
                   <symobjref>  type tpda_sys_symbobjref.

    desc-name = name.
    desc-valuetext = '<s>UNKNOWN</s>'.

    try.
        desc-descr = cl_tpda_script_data_descr=>factory( name ).
        info  = cl_tpda_script_data_descr=>get_quick_info( name ).
        desc-metatype = info-metatype.
        case info-metatype.
          when cl_tpda_script_data_descr=>mt_string.
            string ?= desc-descr.
            desc-valuetext = string->value( ).
            desc-valuetext = cl_http_utility=>escape_html( desc-valuetext ).
          when cl_tpda_script_data_descr=>mt_simple.
            elem ?= desc-descr.
            desc-valuetext = elem->value( ).
            desc-valuetext = cl_http_utility=>escape_html( desc-valuetext ).
          when cl_tpda_script_data_descr=>mt_struct.
            desc-valuetext = '&lt;structure&gt;'.
            desc-hascomponents = abap_true.
            desc-haschildren = abap_true.
            desc-refname = name.
          when cl_tpda_script_data_descr=>mt_tab.
            table ?= desc-descr.
            if table->linecnt( ) > 0.
              desc-haschildren = abap_true.
              desc-istable = abap_true.
              desc-valuetext = '&lt;table&gt;'.
              desc-refname = name.
            else.
              desc-valuetext = '&lt;emptytable&gt;'.
            endif.
          when cl_tpda_script_data_descr=>mt_datref.
            assign info-quickdata->* to <symdataref>.
            find regex '^\{[A-Z]:initial\}$' in <symdataref>-instancename ignoring case.
            desc-isreference = abap_true.
            if sy-subrc <> 0.
              desc-haschildren = abap_true.
              desc-refname = <symdataref>-instancename.
              desc-valuetext = '&lt;reference&gt;'.
            else.
              desc-valuetext = '&lt;null reference&gt;'.
            endif.
          when cl_tpda_script_data_descr=>mt_object.
            desc-valuetext = '&lt;object&gt;'.
            desc-haschildren = abap_true.
            desc-hascomponents = abap_true.
          when cl_tpda_script_data_descr=>mt_objref.
            assign info-quickdata->* to <symobjref>.
            if <symobjref>-instancename <> '{O:initial}'.
              desc-haschildren = abap_true.
              desc-refname = <symobjref>-instancename.
              desc-valuetext = '&lt;object_ref&gt;'.
            else.
              desc-valuetext = '&lt;null object_ref&gt;'.
            endif.
            desc-isreference = abap_true.
        endcase.
      catch cx_root into ex.
        log( |get_data_desc exception { ex->get_text( ) } | ).
    endtry.

  endmethod.

  method create_node_new.

    if desc-istable = abap_true.
      node = create_table_node( desc ).
    elseif desc-hascomponents = abap_true.
      node = create_record_node( desc ).
    else.
      node = create_simple_node( desc ).
    endif.

  endmethod.

  method create_table_node.
    data: value       type string,
          id          type string,
          linkedid    type string,
          components  type tt_component,
          partid      type string,
          componentid type string,
          tabdesc     type ref to cl_tpda_script_tabledescr,
          table       type ref to zcl_abap_graph_node_table,
          linename    type string,
          line        type i,
          linedesc    type zcl_debug_obj_graph_factory=>ty_data_desc,
          ex          type ref to cx_root.
    field-symbols: <component> like line of components.


    try.
        tabdesc ?= desc-descr.
        value = escapevalue( desc-valuetext ).
        id = desc-name.
        node = table = zcl_abap_graph_node_table=>create( graph = graph id = id label = value escape = abap_false ).

        "header - use dummy components without data
        components = get_components( desc ).
        loop at components assigning <component>.
          table->setcolumn( id = <component>-compname name = <component>-compname ).
        endloop.
        do tabdesc->linecnt( ) times.
          line = sy-index.
          linename = |{ desc-name }[{ line }]|.
          linedesc = get_data_desc( linename ).

          "actual components of a line
          components = get_components( linedesc ).
          loop at components assigning <component>.
            value = escapevalue( <component>-desc-valuetext ).

            if <component>-desc-refname <> ''.
              partid = |l{ line }c{ <component>-compname }|.
            else.
              partid = ''.
            endif.

            componentid = table->setcell(
                columnid    = <component>-compname
                row         = line
                value       = value
                partid      = partid ).

            if <component>-desc-refname <> ''.
              linkedid = get_or_create_node( desc-refname ).
              if linkedid <> ''.
                node->linkto( destination = linkedid source = componentid ).
              endif.
            endif.

          endloop.
        enddo.
      catch cx_root into ex.
        log( |create_table_node exception { ex->get_text( ) } | ).
    endtry.

  endmethod.

  method create_record_node.
    data: value       type string,
          id          type string,
          linkedid    type string,
          components  type tt_component,
          record      type ref to zcl_abap_graph_node_record,
          componentid type string,
          ex          type ref to cx_root.
    field-symbols: <component> like line of components.

    try.
        components = get_components( desc ).

        value = escapevalue( desc-valuetext ).
        id = desc-name .
        node = record = zcl_abap_graph_node_record=>create( graph = graph id = id label = value escape = abap_false ).

        loop at components assigning <component>.
          componentid = add_record_component( record = record component = <component> ).

          if componentid <> ''.
            linkedid = get_or_create_node( <component>-desc-refname ).
            if linkedid <> ''.
              node->linkto( destination = linkedid source = componentid ).
            endif.
          endif.

        endloop.
      catch cx_root into ex.
        log( |create_record_node exception { ex->get_text( ) } | ).
    endtry.
  endmethod.

  method create_simple_node.
    data: value    type string,
          id       type string,
          linkedid type string.

    value = escapevalue( desc-valuetext ).
    id =  desc-name .
    node = zcl_abap_graph_node_simple=>create( graph = graph id = id label = value ).

    if desc-haschildren = abap_true.
      linkedid = get_or_create_node( desc-refname ).
      if linkedid <> ''.
        node->linkto( linkedid ).
      endif.
    endif.

  endmethod.


  method get_components.
    data: odesc         type ref to cl_tpda_script_objectdescr,
          sdesc         type ref to cl_tpda_script_structdescr,
          tdesc         type ref to cl_tpda_script_tabledescr,
          objattributes type tpda_script_object_attribut_it,
          scomponents   type tpda_script_struc_componentsit,
          refname       type string,
          match_offset  type i,
          tabcomps      type tpda_scr_table_comp_it,
          ex            type ref to cx_root,
          basename      type string.
    field-symbols: <scomponent> like line of scomponents,
                   <component>  like line of components,
                   <attribute>  like line of objattributes,
                   <tabcomp>    like line of tabcomps.
    try.

        if desc-metatype = cl_tpda_script_data_descr=>mt_object.
          odesc ?= desc-descr.
          objattributes = odesc->attributes( ).

        elseif desc-metatype = cl_tpda_script_data_descr=>mt_struct."struct
          sdesc ?= desc-descr.
          sdesc->components( importing p_components_it = scomponents ).
          find regex '\*$' in desc-name match offset match_offset.
          if match_offset = 0.
            basename = desc-name.
          else.
            basename = desc-name(match_offset).
          endif.
          basename = desc-name.
        elseif desc-metatype = cl_tpda_script_data_descr=>mt_tab.
          tdesc ?= desc-descr.
          if tdesc->linecnt( ) > 0.
            log( |get_components probable casting exception after this line | ).
            sdesc ?= tdesc->get_line_handle( 1 ).
            sdesc->components( importing p_components_it = scomponents ).
            concatenate desc-name '[1]'  into  basename.
          endif.
        endif.

        loop at objattributes assigning <attribute>.
          append initial line to components assigning <component>.
          <component>-compname = <attribute>-name.
          concatenate desc-name '-'  <attribute>-name into refname.
          <component>-desc = get_data_desc( refname ).
          <component>-visibility = decode_visibility( <attribute>-acckind ).
          <component>-value = get_comp_value( <component> ).
        endloop.

        loop at scomponents assigning <scomponent>.
          append initial line to components assigning <component>.
          <component>-compname = <scomponent>-compname.
          <component>-visibility = zcl_abap_graph_node_record=>vispublic.
          concatenate basename '-'  <component>-compname into refname.
          <component>-desc = get_data_desc( refname ).
          <component>-value = get_comp_value( <component> ).
        endloop.

      catch cx_root into ex.
        log( |get_components { ex->get_text( ) } | ).
    endtry.
    sort components by visibility descending compname ascending.
  endmethod.


  method log.
    logs = |{ logs }{ cl_abap_char_utilities=>cr_lf }{ entry }|.
  endmethod.


  method add_record_component.
    data: id     type string,
          partid type string.

    id = component-desc-name .

    if component-desc-haschildren = abap_true.
      partid = component-compname.
    else.
      partid = ''.
    endif.

    componentid = record->addcomponent(
      exporting
        name        = component-compname
        value       = component-value
        escape      = abap_false
        visibility  = component-visibility
        partid      = partid ).

  endmethod.


  method get_comp_value.

    if    i_component-desc-istable       = abap_true
       or i_component-desc-hascomponents = abap_true
       or i_component-desc-isreference   = abap_true.
      r_result = |<u>{ i_component-desc-valuetext }</u>|.
    else.
      r_result = i_component-desc-valuetext.
    endif.
  endmethod.

  method decode_visibility.
    case acckind.
      when if_tpda_control=>ak_private.
        r_result = zcl_abap_graph_node_record=>visprivate.
      when if_tpda_control=>ak_protected.
        r_result  = zcl_abap_graph_node_record=>visprotected.
      when if_tpda_control=>ak_public.
        r_result  = zcl_abap_graph_node_record=>vispublic.
      when others.
        assert 1 = 1 + 1.
    endcase.
  endmethod.

  method get_or_create_node.
    data: desc type zcl_debug_obj_graph_factory=>ty_data_desc,
          node type ref to zif_abap_graph_node.

    node = graph->get_node( name ).

    if not node is bound.
      "look up cache
      desc = get_data_desc( name ).
      node = create_node_new( desc ).
    endif.

    if node is bound.
      nodeid = node->id.
    endif.

  endmethod.

endclass.
