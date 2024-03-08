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
    procedure Button1Click(Sender: TObject);
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
  FMaze := TMaze.Create(20, 20, 24);
  // Generate a random maze starting in cell 0, 0
  FMaze.GenerateMaze(0, 0);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Generate a new maze
  FMaze.GenerateMaze(10, 10);
  // Force the form to repaint
  Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  // Draw the maze on the form's canvas
  FMaze.RenderMaze(Self.Canvas);
end;

end.

