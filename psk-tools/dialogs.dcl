progressDlg : dialog {
  label = "进度";
  
    :row {
    :list_box {
        key = "LIST";
        width = 60;
        height = 15;
        fixed_height = true;
        fixed_width = true;
      }
    }
    
   :ok_button {
    key="OK";
    label="启动";
   }

      :cancel_button {
    label="关闭";
   }
}

aboutDialog : dialog {
  label = "关于PSK";
  
    :row {
    :list_box {
        key = "LIST";
        width = 90;
        height = 30;
        fixed_height = true;
        fixed_width = true;
      }
    }
   :cancel_button {
    label="关闭";
   }
}


ductCreateDialog : dialog {
  label = "创建方管";
  
    :row {
    :list_box {
        key = "A";
        width = 10;
        height = 15;
        fixed_height = true;
        fixed_width = true;
      }
    :list_box {
        key = "B";
        width = 10;
        height = 15;
        fixed_height = true;
        fixed_width = true;
      }
    }
   :cancel_button {
    label="关闭";
   }
}