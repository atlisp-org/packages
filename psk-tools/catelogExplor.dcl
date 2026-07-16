catelogExplor : dialog {
  label = "布置设备";
  :row {
    :popup_list {
      label = "目录文件";
      key = "CATELOG";
      edit_width = 24;
      fixed_width = true;
    }
  }
  :boxed_column {
    :row {
    :list_box {
        key = "KEYLIST";
        width = 120;
        height = 25;
        fixed_height = true;
        fixed_width = true;
        tabs = "10 20 30 40 50 60 70 80 90 100 110 120";
        tab_truncate = true;
      }
    }
  }
  :row {
   :ok_button {
    key="OK";
    label="确定(&O)";
   }
   :cancel_button {
    key="CANCEL";
    label="取消";
   }
  }
}

catelogExplor2 : dialog {
  label = "布置设备";
  :row {
    :popup_list {
      label = "目录文件";
      key = "CATELOG";
      edit_width = 40;
      fixed_width = true;
    }
  }
  :row {
  :boxed_column {
    label = "符号列表";
    :list_box {
        key = "KEYLIST";
        width = 30;
        height = 25;
        fixed_height = true;
        fixed_width = true;
      }
   }
  :boxed_column {
    label = "详细数据";
    :list_box {
        key = "PROPLIST";
        width = 55;
        height = 15;
        fixed_height = true;
        fixed_width = true;
        tabs = "20";
        tab_truncate = true;
        }
    :image {
        key = "IMG";
        color = 0;
        width = 30;
        height = 10;
        fixed_height = true;
        fixed_width =true;
        }
    }
  }
  :row {
   :ok_button {
    key="OK";
    label="插入(&I)";
   }
   :cancel_button {
    key="CANCEL";
    label="取消";
   }
  }
}