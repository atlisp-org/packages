import svgwrite
import ezdxf

def svg_to_dxf(svg_file, dxf_file):
    # 创建一个新的 DXF 文档
    doc = ezdxf.new(dxfversion='R2010')
    msp = doc.modelspace()
    
    # 读取 SVG 文件
    dwg = svgwrite.Drawing(svg_file)
    
    # 遍历 SVG 元素并转换为 DXF 元素
    for element in dwg.elements:
        if isinstance(element, svgwrite.shapes.Line):
            msp.add_line((element['x1'], element['y1']), (element['x2'], element['y2']))
        elif isinstance(element, svgwrite.shapes.Circle):
            msp.add_circle((element['cx'], element['cy']), element['r'])
            # 添加更多元素类型的转换逻辑
            
            # 保存为 DXF 文件
    doc.saveas(dxf_file)
