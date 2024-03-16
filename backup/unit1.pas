unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Contnrs,
  Maze;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  FMaze := TMaze.Create(10, 10, 32);
  // Generate a random maze starting in cell 0, 0
  FMaze.GenerateMaze(0, 0);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Generate a new maze
  FMaze.GenerateMaze(0, 0);
  // Force the form to repaint
  Invalidate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FMaze.FindPath(1, 1, 8, 8);
  Label1.Caption := Format('Path Steps: %d', [FMaze.Path.Count]);
  Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  i: integer;
  rct: TRect;
  ts: TTextStyle;
begin
  ts.SingleLine := true;
  ts.SystemFont := false;
  ts.Alignment  := taCenter;
  ts.Layout     := tlCenter;

  // Draw the maze on the form's canvas
  FMaze.RenderMaze(Self.Canvas);

  Canvas.Font.Color:= clWhite;
  Canvas.Pen.Style := psClear;

  for i := 0 to FMaze.Path.Count - 1 do
  begin
    if (i = FMaze.Path.Count - 1) then
      Canvas.Brush.Color := clRed
    else if (i = 0) then
      Canvas.Brush.Color := clGreen
    else
      Canvas.Brush.Color := clBlue;
    rct := FMaze.Path[i].Rect.AsRect;
    rct.Inflate(-6, -6);
    Canvas.Ellipse(rct);
    rct := FMaze.Path[i].Rect.AsRect;
    Canvas.TextRect(rct, rct.Left, rct.Top, i.ToString, ts);
  end;
end;

end.

