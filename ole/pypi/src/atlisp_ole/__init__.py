# coding: utf-8
import sys,os
import argparse
import pathlib
import time
import win32com.client
from .atlisp_ole import  send_msg_to_clip,paste_img,olePicture

def main():
    if len(sys.argv)>1:
        file_name,file_ext = os.path.splitext(sys.argv[1])
        acadapp =win32com.client.Dispatch("AutoCAD.application")

        if  file_ext in [".jpg",".png",".bmp"] :
            acadapp.ActiveDocument.SendCommand('(ole:osmode-off) ')
            olePicture(sys.argv[1],acadapp)
            acadapp.ActiveDocument.SendCommand('(ole:osmode-on) ')
        else:
            with open(sys.argv[1],"r") as f:
                acadapp.ActiveDocument.SendCommand('(ole:osmode-off) ')
                imgs = f.readlines();
                print("发现 "+ str(len(imgs)) +" 个图像")
                for img in imgs:
                    img = img.strip("\n")
                    olePicture(img,acadapp)
                acadapp.ActiveDocument.SendCommand('(ole:osmode-on) ')
                return 0
    else:
        usage = """
        需要指定图像列表文件,或图像文件

        用法: atlisp-ole imglst/imgfile
        功能：自动将图像以OLE方式插入到cad中。
        """
        print(usage)
        return 1
if __name__ == '__main__':
    main()
