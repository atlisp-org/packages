
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

	    var palettes = new Dictionary<string, string>{
		{"我的","http://s3.atlisp.cn/palette-mymenu.html"},
		{"@LISP","http://s3.atlisp.cn/palette.html"},
		{"国标", "http://s3.atlisp.cn/gb"},
		{"图库", "http://s3.atlisp.cn/dw/library/"},
		{"填充", "http://s3.atlisp.cn/dw/pattern/"}
	    };
	    for (int i = 0; i < palettes.Count; i++){
		ps.Add(palettes.ElementAt(i).Key,
		       new AtLispWv(palettes.ElementAt(i).Value));
	    }
	    ps.Visible = true;
		ps.DockEnabled = DockSides.Left | DockSides.Right;
	    ps.Dock = DockSides.Left;
	    // ps.KeepFocus=true;
	    // 设置面板样式和透明度
	    // ps.Style = PaletteSetStyles.ShowTabForSingle;
	    //ps.Opacity = 90;
	}
	
    }
}
