"Name: \TY:CL_TPDA_TOOL_TABLE_NEW\IN:IF_TPDA_TOOL_SERVICES\ME:BUILD_SERVICES_MENUE\SE:END\EI
ENHANCEMENT 0 Z_TABLE_V_BUILD_SERVICES_MENUE.
 DATA l_zz_node LIKE LINE OF p_it_menue_tree.
 CLEAR l_zz_node.

 l_zz_node-node_key  = 'ZDATA_4_ABAP'.
 l_zz_node-relatkey  = cl_tpda_services_tools=>c_special.
 l_zz_node-n_image   = cl_tpda_icons=>tpda_icon_download.
 l_zz_node-relatship = cl_gui_simple_tree=>relat_last_child.
 l_zz_node-text      = 'Data for Abap View'.

 APPEND l_zz_node TO p_it_menue_tree.
ENDENHANCEMENT.
