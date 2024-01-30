PropertyEdit:dialog
{
	label = "属性编辑";

	:row {
			:boxed_row {
				label = "属性列表";
				:list_box {
				key = "PROP_LIST";
				fixed_height = true;
				fixed_width =true;
				width = 51;
				height = 25;
				tabs = "25 48";
				tab_truncate = true;
				multiple_select = false;
			}
		}

		:boxed_column {
			label = "值";
			fixed_height = true;
			fixed_width =true;
			alignment = top;
			
			:edit_box {
				key = "VALUE_TEXT";
				width = 30;
			}
			
			:list_box {
				key = "VALUE_LIST";
				fixed_height = true;
				fixed_width =true;
				height = 20;
				width = 30;
			}
			: text {
				key = "VALUE_INFO";
				label="";
				width = 30;
				height = 5;
			}
		}
	}

	:row {
		:button {
			key="OK";
			label="确定(&O)";
			fixed_width = true;
			width = 12;
			is_default = true;
		}
		:button {
			label="取消";
			fixed_width = true;
			width = 12;
			is_cancel = true;
		}
	}
}


psk_createpath:dialog
{
	label = "创建路径";

	:row {
			:boxed_row {
				label = "属性列表";
				:list_box {
				key = "PROP_LIST";
				fixed_height = true;
				fixed_width =true;
				width = 51;
				height = 25;
				tabs = "25 48";
				tab_truncate = true;
				multiple_select = false;
			}
		}

		:boxed_column {
			label = "值";
			fixed_height = true;
			fixed_width =true;
			alignment = top;
			
			:edit_box {
				key = "VALUE_TEXT";
				width = 30;
			}
			
			:list_box {
				key = "VALUE_LIST";
				fixed_height = true;
				fixed_width =true;
				height = 20;
				width = 30;
			}
			: text {
				key = "VALUE_INFO";
				label="";
				width = 30;
				height = 5;
			}
		}
	}

	:row {
		:button {
			key="OK";
			label="确定(&O)";
			fixed_width = true;
			width = 12;
			is_default = true;
		}
		:button {
			key="FETCH";
			label="从已有路径提取(&G)";
			fixed_width = true;
			width = 12;
		}
		:button {
			key="DUCTV";
			label="创建立管";
			fixed_width = true;
			width = 12;
		}
		:spacer_0 {}
		:button {
			label="取消";
			fixed_width = true;
			width = 12;
			is_cancel = true;
		}
	}
}


psk_selectbox:dialog
{
label = "选择列表";

: column {
  : edit_box { key = "FILTER"; width = 40; }
  : list_box {
  key = "LIST";
  fixed_height = true;
  fixed_width =true;
  width = 40;
  height = 25;
  tab_truncate = true;
  multiple_select = false;
  is_default = true;
  }
}

: row {
  :button { key="OK"; label="确定(&O)"; fixed_width = true; width = 12; is_default = true;}
  :button { label="取消"; fixed_width = true; width = 12; is_cancel = true; }
}
}