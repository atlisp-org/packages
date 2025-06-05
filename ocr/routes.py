from flask import Blueprint, render_template, request, redirect, url_for
import json
import easyocr
reader = easyocr.Reader(
    lang_list=['ch_sim', 'en'],
    gpu=False, # 默认为True
    download_enabled=True 
)
ocr_bp = Blueprint('ocr',__name__)
@ocr_bp.route('/ocr',methods=['POST'])
def ocr():
    # 将提交的数据生成图片
    # filepath = request.form.get("path")
    data = request.json
    #print(data)
    result = reader.readtext(data['filepath'], detail=1)
    # print(result)
    return str(result)
