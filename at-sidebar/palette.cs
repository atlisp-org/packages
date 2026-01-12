using System;
using System.Windows;
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

namespace AtPaletteSet
{
    public partial class Form1 : UserControl
    {
	private WebView2 webView21 = new WebView2();
	public Form1()
        {
            // InitializeComponent();
            Resize += new EventHandler(Form_Resize);
            webView21.CoreWebView2InitializationCompleted += WebView21_CoreWebView2InitializationCompleted;
	    try{
		Initialize();
	    }
	    catch (System.Exception ex)
	    {
		MessageBox.Show($"WebView2 initial faile:{ex.Message}");
	    }
        }
         /// <summary>
         /// 实现自适应页面缩放
         /// </summary>
        private void Form_Resize(object sender, EventArgs e)
        {
            webView21.Size = ClientSize - new Size(webView21.Location);
        }
        /// <summary>
        /// webview 加载完毕
        /// </summary>
        private void WebView21_CoreWebView2InitializationCompleted(object sender, CoreWebView2InitializationCompletedEventArgs e)
        {
            webView21.CoreWebView2.Navigate("https://atlisp.cn/palette.html");
        }
        /// <summary>
        /// WebView2初始化
        /// </summary>
        async void Initialize()
        {
	    string userDataFolder = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "WebView2", "UserData");
	    Directory.CreateDirectory(userDataFolder); // 确保目录存在
	    var environment = await CoreWebView2Environment.CreateAsync(null, userDataFolder);
	    // webView21.CoreWebView2.Navigate("https://atlisp.cn/palette.html");
	    await webView21.EnsureCoreWebView2Async(environment);
        }
    }

    
    public class AtLispPalette 
    {
	[CommandMethod("AtPalette")]
	public void AtPalette()
	{
	    // private webView  =  CreateWebView();
	
	    PaletteSet ps = new PaletteSet("@LISP");
	    ps.MinimumSize = new System.Drawing.Size(200, 600);

	    // 创建用户自定义的窗体或控件
	    UserControl ctrl = new UserControl();
	    
	    // 添加控件到面板集中
	    ps.Add("@LISP", ctrl);
	    Form1 webView = new Form1();
	    if (null != webView ){
		ps.Add("DW", webView);
            }

	    // 显示面板
	    ps.Visible = true;

	    // 设置面板样式和透明度
	    // ps.Style = PaletteSetStyles.ShowTabForSingle;
	    // ps.Opacity = 100;
	}
	
    }
}
