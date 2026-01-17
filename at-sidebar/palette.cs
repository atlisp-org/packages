namespace AtPaletteSet
{
    public class AtLispPalette 
    {
        [CommandMethod("@Palette")]
	//[CommandMethod("AtPalette")]
	public void AtPalette()
	{
	    PaletteSet ps = new PaletteSet("@LISP");
	    ps.MinimumSize = new System.Drawing.Size(150, 600);
	    ps.Size = new System.Drawing.Size(200, 600);

	    //UserControl myui = new UserControl();
	    //ps.Add("我的",myui);
	    AtLispWv myui = new AtLispWv("http://s3.atlisp.cn/palette-mymenu.html");
	    if (null != myui ){
		ps.Add("我的", myui);
            }
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
	    ps.DockEnabled = DockSides.Left;
	    ps.Dock = DockSides.Left;
	    // 设置面板样式和透明度
	    // ps.Style = PaletteSetStyles.ShowTabForSingle;
	    //ps.Opacity = 90;
	}
	
    }
}
