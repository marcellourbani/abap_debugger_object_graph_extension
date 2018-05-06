function z_debug_graph_view.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(GRAPHTEXT) TYPE  STRING DEFAULT 'digraph {aa -> bb}'
*"----------------------------------------------------------------------
  data:viewer     type ref to cl_gui_html_viewer,
       container  type ref to cl_gui_custom_container,
       url(10000) type c.
  create object viewer exporting parent = container.

  concatenate 'http://localhost:3000?graphsource=' graphtext into url.

  viewer->detach_url_in_browser(  url ).

endfunction.
