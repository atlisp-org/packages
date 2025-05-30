from flask import  Flask,jsonify,request
import json
import easyocr

jsonobj = {};

reader = easyocr.Reader(
    lang_list=['ch_sim', 'en'],
    gpu=False, # 默认为True
    download_enabled=True 
)
app = Flask(__name__)
@app.route('/api/json',methods=['GET'])
def get_json():
    global jsonobj
    return json.dumps(jsonobj)
@app.route('/api/json',methods=['POST'])
def init_json():
    global jsonobj
    jsonobj = request.json
    return json.dumps(jsonobj)
@app.route('/api/json_set',methods=['POST'])
def set_json():
    global jsonobj
    data = request.json
    for key in data:
        jsonobj[key] = data[key]
    return json.dumps(jsonobj)

@app.route('/api/ocr',methods=['POST'])
def ocr():
    # 将提交的数据生成图片
    # filepath = request.form.get("path")
    data = request.json
    #print(data)
    result = reader.readtext(data['filepath'], detail=1)
    # print(result)
    return str(result)

if __name__ == '__main__':
    app.run(debug=True)
