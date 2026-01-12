using System;
using System.Windows;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Autodesk.AutoCAD.Runtime;
using Autodesk.AutoCAD.Windows;
using Microsoft.Web.WebView2.Core;
using Microsoft.Web.WebView2.WinForms;

namespace AtPaletteSet
{
    public class AtLispPalette 
    {
	private WebView2 webView2 = new WebView2();
			     
	private static void CoreWebView2_NewWindowRequested(object sender, CoreWebView2NewWindowRequestedEventArgs e)
	{
	    if (sender is CoreWebView2 webView)
	    {
		e.Handled = true; // 阻止默认弹窗行为
		webView.Navigate(e.Uri); // 在当前 WebView2 中加载新页面
	    }
	}
	private static void CoreWebView2_WebResourceResponseReceived(object sender, CoreWebView2WebResourceResponseReceivedEventArgs e)
	{
	    var uri = e.Request.Uri;
	    Console.WriteLine($"Response received for: {uri}");
	    e.Response.Headers.ToList().ForEach(header =>
	    {
		Console.WriteLine($"Header: {header.Key} - {header.Value}");
	    });
	}
	[CommandMethod("AtPalette")]
	public void AtPalette()
	{
	    // 获取 AutoCAD 主应用对象

	    webView2.EnsureCoreWebView2Async(null);
	    webView2.CoreWebView2.NewWindowRequested += CoreWebView2_NewWindowRequested;
	    // webView2.CoreWebView2.WebResourceResponseReceived += CoreWebView2_WebResourceResponseReceived;
	    // 添加请求过滤器
	    webView2.CoreWebView2.AddWebResourceRequestedFilter("*://atlisp.cn/*", CoreWebView2WebResourceContext.Document);
	    // webView2.CoreWebView2.WebResourceRequested += WebView2_WebResourceRequested;
	    // 初始化面板集对象
	    PaletteSet ps = new PaletteSet("@LISP");
	    ps.MinimumSize = new System.Drawing.Size(200, 600);

	    // 创建用户自定义的窗体或控件
	    UserControl ctrl = new UserControl();
	    
	    // 添加控件到面板集中
	    ps.Add("@LISP", ctrl);
	    ps.Add("DW", webView2);

	    // 显示面板
	    ps.Visible = true;

	    // 设置面板样式和透明度
	    ps.Style = PaletteSetStyles.ShowTabForSingle;
	    ps.Opacity = 90;
	}
	
    }
}
