FUNCTION Z_DEBUG_GRAPH_VIEW_LAST_IFRAME.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
 data:viewer    type ref to cl_gui_html_viewer,
       container type ref to cl_gui_custom_container,
       url       type w3url,
       source    type string,
       graphtext type string.

  import graphtext from database indx(dg) id 'LASTGRAPH'.
  perform createsource using graphtext 'http://192.168.1.46:3000' changing source.

  create object viewer exporting parent = cl_gui_container=>screen0." container.

  perform cachehtml using viewer source changing url.

  viewer->show_url( url ).

  call selection-screen 1001.


endfunction.
