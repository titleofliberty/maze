unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SpinEx,
  Contnrs, Maze;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnGenerate: TButton;
    btnPath: TButton;
    lblPathCount: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    txtWidth: TSpinEditEx;
    txtHeight: TSpinEditEx;
    procedure btnGenerateClick(Sender: TObject);
    procedure btnPathClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private

  public

  end;

var
  FMaze: TMaze;
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Instantiate a maze 20 wide by 20 high
  FMaze := TMaze.Create(txtWidth.Value, txtHeight.Value, 48);
  // Generate a random maze starting in cell 0, 0
  FMaze.GenerateMaze(0, 0);
  lblPathCount.Caption := 'Path Steps: 0';
end;

procedure TForm1.btnGenerateClick(Sender: TObject);
begin
  // Generate a new maze
  FMaze.GenerateMaze(0, 0);
  lblPathCount.Caption := 'Path Steps: 0';
  // Force the form to repaint
  Invalidate;
end;

procedure TForm1.btnPathClick(Sender: TObject);
begin
  FMaze.FindPath(0, 0, FMaze.Width - 1, FMaze.Height - 1);
  lblPathCount.Caption := Format('Path Steps: %d', [FMaze.Path.Count]);
  Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  i: integer;
  rct: TRect;
  ts: TTextStyle;
  off: TPoint;
begin
  ts.SingleLine := true;
  ts.SystemFont := false;
  ts.Alignment  := taCenter;
  ts.Layout     := tlCenter;

  // Draw the maze on the form's canvas
  off := Point(25, 25);
  FMaze.RenderMaze(Self.Canvas, off);

  //Exit;

  for i := 0 to FMaze.Path.Count - 1 do
  begin
    if (i = FMaze.Path.Count - 1) then
      Canvas.Brush.Color := clRed
    else if (i = 0) then
      Canvas.Brush.Color := clGreen
    else
      Canvas.Brush.Color := clBlue;
    rct := FMaze.Path[i].Rect.AsRect;
    rct.Offset(off);
    rct.Inflate(-7, -7);
    Canvas.Brush.Style := bsSolid;
    Canvas.Ellipse(rct);
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect(rct, rct.Left, rct.Top, i.ToString, ts);
  end;
end;

end.

