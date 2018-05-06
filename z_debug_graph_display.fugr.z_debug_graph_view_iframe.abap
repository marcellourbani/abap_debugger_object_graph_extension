FUNCTION Z_DEBUG_GRAPH_VIEW_IFRAME.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(GRAPHTEXT) TYPE  STRING DEFAULT 'digraph {aa -> bb}'
*"----------------------------------------------------------------------
  data:viewer    type ref to cl_gui_html_viewer,
       container type ref to cl_gui_custom_container,
       url       type w3url,
       source    type string.

  export graphtext to database indx(dg) id 'LASTGRAPH'.

  create object viewer exporting parent = cl_gui_container=>screen0." container.
  perform createsource using graphtext 'http://192.168.1.46:3000' changing source.

*  concatenate 'http://localhost:3000?graphsource=' graphtext into url.
  perform cachehtml using viewer source changing url.

*  viewer->detach_url_in_browser(  url ).

  viewer->show_url( url ).

  call selection-screen 1001.

endfunction.

form cachehtml using viewer     type ref to cl_gui_html_viewer
                                              source type string
                          changing url type w3url.
  data: xsource  type xstring,
        xdatatab type table of w3_mime, " RAW255
        size     type int4.

  call function 'SCMS_STRING_TO_XSTRING'
    exporting
      text   = source
    importing
      buffer = xsource
    exceptions
      others = 1.

  call function 'SCMS_XSTRING_TO_BINARY'
    exporting
      buffer        = xsource
    importing
      output_length = size
    tables
      binary_tab    = xdatatab.

  viewer->load_data(
    exporting
      size                   = size    " Length of Data
    importing
      assigned_url           =     url
    changing
      data_table             =   xdatatab  " data table
    exceptions
      dp_invalid_parameter   = 1
      dp_error_general       = 2
      cntl_error             = 3
      html_syntax_notcorrect = 4
      others                 = 5 ).

endform.

form createsource using graphsrc type string baseurl type string changing source type string.
  data: lines      type table of string,
        graphlines type table of string,
        graph      type string,
        iframe     type string.
  FIELD-SYMBOLS: <graphline> LIKE LINE OF graphlines.
  clear source.
  graph = graphsrc.
  replace all occurrences of '\' in graph with '\\'.
  replace all occurrences of '''' in graph with '\'''.

  split graph at cl_abap_char_utilities=>newline into table graphlines.

  concatenate  '<iframe src="' baseurl '?useparentsource=true">' into iframe.

  append:
'<!DOCTYPE html>' to lines,
'<html lang="en">' to lines,
'' to lines,
'<head>' to lines,
'    <meta charset="utf-8">' to lines,
'    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">' to lines,
'    <title>Graph display</title>' to lines,
'    <style>' to lines,
'        iframe {' to lines,
'            position: fixed;' to lines,
'            top: 0px;' to lines,
'            left: 0px;' to lines,
'            bottom: 0px;' to lines,
'            right: 0px;' to lines,
'            width: 100%;' to lines,
'            height: 100%;' to lines,
'            border: none;' to lines,
'            margin: 0;' to lines,
'            padding: 0;' to lines,
'            overflow: hidden;' to lines,
'            z-index: 999999;' to lines,
'        }' to lines,
'    </style>' to lines,
'    <script>' to lines,
'        var graphsource =  ' to lines.
  loop at graphlines assigning <graphline>.

    concatenate '''' <graphline> '\n'' +' into graph.
    append graph to lines.

  endloop.

  append:
  '       '''' ;' to lines,
  '        function receiveMessage(event) {' to lines,
  '            if (event.data === "requestGraphSource") { ' to lines,
  '                event.source.postMessage("graphSource=" + graphsource, "*");' to lines,
  '            }' to lines,
  '        }' to lines,
  '        window.addEventListener("message", receiveMessage, false);' to lines,
  '    </script>' to lines,
  '</head>' to lines,
  '' to lines,
  '<body>' to lines,
  iframe to lines,
  '        Your browser doesn''t support iframes' to lines,
  '    </iframe>' to lines,
  '</body>' to lines,
  '' to lines,
  '</html>'  to lines.

  concatenate lines of lines into source separated by  cl_abap_char_utilities=>cr_lf.
endform.
