using System;
// using System.Windows;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Threading.Tasks;

using Autodesk.AutoCAD.Runtime;
using Autodesk.AutoCAD.Windows;

using System.Diagnostics;
using System.Drawing;
using System.Threading;
using System.Text;
using System.IO;
using System.Windows.Forms;
using Microsoft.Web.WebView2.Core;
using Microsoft.Web.WebView2.WinForms;
using System.Linq;
// using System.ComponentModel;
using Autodesk.AutoCAD.DatabaseServices;// (Database, DBPoint, Line, Spline) 
using Autodesk.AutoCAD.Geometry;//(Point3d, Line3d, Curve3d) 
using Autodesk.AutoCAD.ApplicationServices;// (Application, Document) 
using Autodesk.AutoCAD.Runtime;// (CommandMethodAttribute, RXObject, CommandFlag) 
using Autodesk.AutoCAD.EditorInput;//(Editor, PromptXOptions, PromptXResult)
using AcadApp = Autodesk.AutoCAD.ApplicationServices.Application;
using System.Runtime.InteropServices;
using Application = Autodesk.AutoCAD.ApplicationServices.Application;

namespace AtPaletteSet
{
    public class AtLispPalette 
    {
        [CommandMethod("@Palette")]
	public void AtPalette()
	{
	    PaletteSet ps = new PaletteSet("@LISP");
	    ps.MinimumSize = new System.Drawing.Size(200, 600);

	    //UserControl myui = new UserControl();
	    //ps.Add("我的",myui);

	    AtLispWv atmenu = new AtLispWv("http://s3.atlisp.cn/palette.html");
	    if (null != atmenu ){
		ps.Add("@LISP", atmenu);
            }
	    AtLispWv gb = new AtLispWv("http://s3.atlisp.cn/gb");
	    if (null != gb ){
		ps.Add("国标", gb);
            }
	    AtLispWv dw = new AtLispWv("http://s3.atlisp.cn/");
	    if (null != dw ){
		ps.Add("图库", dw);
            }
	    ps.Visible = true;
	    // 设置面板样式和透明度
	    // ps.Style = PaletteSetStyles.ShowTabForSingle;
	    //ps.Opacity = 90;
	}
	
    }
}
