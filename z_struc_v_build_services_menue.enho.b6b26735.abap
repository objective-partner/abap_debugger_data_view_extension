"Name: \TY:CL_TPDA_TOOL_STRUC_VIEW\IN:IF_TPDA_TOOL_SERVICES\ME:BUILD_SERVICES_MENUE\SE:END\EI
ENHANCEMENT 0 Z_STRUC_V_BUILD_SERVICES_MENUE.
  CLEAR l_node.

  l_node-node_key  = 'ZDATA_4_ABAP'.
  l_node-relatkey  = cl_tpda_services_tools=>c_special.
  l_node-n_image   = cl_tpda_icons=>tpda_icon_download.
  l_node-relatship = cl_gui_simple_tree=>relat_last_child.
  l_node-text      = 'Data for Abap View'.

  APPEND l_node TO p_it_menue_tree.

ENDENHANCEMENT.
