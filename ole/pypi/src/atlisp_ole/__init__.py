# coding: utf-8
import sys
import argparse
import tomllib
import pathlib
import time
import win32com.client,os
from PIL import Image,ImageGrab
from io import BytesIO
# pip install pywin32, win32clipboard是操作剪贴板的模块
import win32clipboard

# 图片路径，如果是当前路径，直接写文件名
# windows路径要注意是 \，例：'D:\\t.jpg'
# linux是 /，例: '~/t.jpg'

def send_msg_to_clip(type_data, msg):
    """
    操作剪贴板分四步：
    1. 打开剪贴板：OpenClipboard()
    2. 清空剪贴板，新的数据才好写进去：EmptyClipboard()
    3. 往剪贴板写入数据：SetClipboardData()
    4. 关闭剪贴板：CloseClipboard()

    :param type_data: 数据的格式，
    unicode字符通常是传 win32con.CF_UNICODETEXT
    :param msg: 要写入剪贴板的数据
    """
    win32clipboard.OpenClipboard()
    win32clipboard.EmptyClipboard()
    win32clipboard.SetClipboardData(type_data, msg)
    win32clipboard.CloseClipboard()

def paste_img(file_img):
    """
    图片转换成二进制字符串，然后以位图的格式写入剪贴板

    主要思路是用Image模块打开图片，
    用BytesIO存储图片转换之后的二进制字符串

    :param file_img: 图片的路径
    """
    # 把图片写入image变量中
    # 用open函数处理后，图像对象的模式都是 RGB
    image = Image.open(file_img)

    # 声明output字节对象
    output = BytesIO()

    # 用BMP (Bitmap) 格式存储
    # 这里是位图，然后用output字节对象来存储
    image.save(output, 'BMP')

    # BMP图片有14字节的header，需要额外去除
    data = output.getvalue()[14:]

    # 关闭
    output.close()

    # DIB: 设备无关位图(device-independent bitmap)，名如其意
    # BMP的图片有时也会以.DIB和.RLE作扩展名
    # 设置好剪贴板的数据格式，再传入对应格式的数据，才能正确向剪贴板写入数据
    send_msg_to_clip(win32clipboard.CF_DIB, data)
    
def olePicture(image_file,acadapp):
    # print("安装 `" + pkgname + "' 到CAD 中")
    paste_img(image_file)
        
    # 等待CAD忙完
    # print("正在初始化dwg,请稍等",end="")
    # 确定是否安装了@lisp core
    #acadapp.ActiveDocument.SendCommand(install_str)
    waitforcad(acadapp)
    # 在CAD中执行粘贴操作
    acadapp.ActiveDocument.SendCommand('(command "PASTECLIP" pt-ins) ')
    time.sleep(10)
    #调整比例
    acadapp.ActiveDocument.SendCommand('(ole:scale-img) ')
    time.sleep(10)
    acadapp.ActiveDocument.SendCommand('(ole:calc-ptins) ')
    print("\n正在粘贴 , 请稍等",end="")
    #waitforcad(acadapp)
    print("\n......完成")
    # confirm = input("是否保持当前CAD实例，你可在当前实例中继续操作。(Y/N): ")
    acadapp.visible=True

def main():
    if len(sys.argv)>1:
        with open(sys.argv[1],"r") as f:
            acadapp =win32com.client.Dispatch("AutoCAD.application")
            acadapp.ActiveDocument.SendCommand('(ole:osmode-off) ')
            for img in f.readlines():
                img = img.strip("\n")
                olePicture(img,acadapp)
            acadapp.ActiveDocument.SendCommand('(ole:osmode-on) ')
            return 0

    else:
        print("需要指定图像列表文件")
        return 1
