unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Contnrs;

const
  MAZE_WIDTH = 3;
  MAZE_HEIGHT = 3;

type

  { TMazePoint }

  TMazePoint = class
  public
    X: Integer;
    Y: Integer;
    constructor Create(AX, AY: Integer);
  end;

  TMazeCell = record
    Visited: Boolean;
    Walls: array [0..3] of Boolean;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    procedure RemoveWall(X1, Y1, X2, Y2: Integer);
    procedure GenerateMaze(X, Y: Integer);
    procedure InitializeMaze;
    procedure RenderMaze;
  public

  end;

var
  Maze: array[0..MAZE_HEIGHT - 1, 0..MAZE_WIDTH - 1] of TMazeCell;
  Stack: TObjectStack;
  VisitedCells: Integer;

  Form1: TForm1;

implementation

{$R *.lfm}

{ TMazePoint }

constructor TMazePoint.Create(AX, AY: Integer);
begin
  inherited Create;
  X := AX;
  Y := AY;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  X, Y, DX, DY: integer;
begin
  Stack := TObjectStack.Create;
  Randomize;
  InitializeMaze;
  GenerateMaze(0, 0);
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  RenderMaze;
end;

procedure TForm1.RemoveWall(X1, Y1, X2, Y2: Integer);
begin
  if X1 = X2 then
  begin
    if Y1 < Y2 then
    begin
      Maze[Y1][X1].Walls[3] := False;
      Maze[Y2][X2].Walls[2] := False;
    end
    else
    begin
      Maze[Y2][X2].Walls[3] := False;
      Maze[Y1][X1].Walls[2] := False;
    end;
  end
  else
  begin
    if X1 < X2 then
    begin
      Maze[Y1][X1].Walls[1] := False;
      Maze[Y2][X2].Walls[0] := False;
    end
    else
    begin
      Maze[Y2][X2].Walls[1] := False;
      Maze[Y1][X1].Walls[0] := False;
    end;
  end;
end;

procedure TForm1.GenerateMaze(X, Y: Integer);
var
  Neighbors: array[0..3] of Integer;
  NeighborCount, RandomNeighbor: Integer;
  i: Integer;
  NewX, NewY: Integer;
  mp : TMazePoint;
begin
  Maze[Y][X].Visited := True;
  Inc(VisitedCells);

  NeighborCount := 0;
  for i := 0 to 3 do
  begin
    case i of
      0: begin NewX := X - 1; NewY := Y; end;
      1: begin NewX := X + 1; NewY := Y; end;
      2: begin NewX := X; NewY := Y - 1; end;
      3: begin NewX := X; NewY := Y + 1; end;
    end;

    if (NewX >= 0) and (NewX < MAZE_WIDTH) and (NewY >= 0) and (NewY < MAZE_HEIGHT)
    and not Maze[NewY][NewX].Visited then
    begin
      Neighbors[NeighborCount] := i;
      Inc(NeighborCount);
    end;
  end;

  if NeighborCount > 0 then
  begin
    RandomNeighbor := Random(NeighborCount);
    case Neighbors[RandomNeighbor] of
      0: begin NewX := X - 1; NewY := Y; end;
      1: begin NewX := X + 1; NewY := Y; end;
      2: begin NewX := X; NewY := Y - 1; end;
      3: begin NewX := X; NewY := Y + 1; end;
    end;
    RemoveWall(X, Y, NewX, NewY);
    Stack.Push(TMazePoint.Create(NewX, NewY));
    GenerateMaze(NewX, NewY);
  end
  else
  begin
    if Stack.Count > 0 then
    begin
      mp := TMazePoint(Stack.Pop);
      GenerateMaze(mp.X, mp.Y);
    end;
  end;
end;

procedure TForm1.InitializeMaze;
var
  X, Y: Integer;
begin
  for Y := 0 to MAZE_HEIGHT - 1 do
    for X := 0 to MAZE_WIDTH - 1 do
    begin
      Maze[Y][X].Visited := False;
      Maze[Y][X].Walls[0] := True;  // Left
      Maze[Y][X].Walls[1] := True;  // Right
      Maze[Y][X].Walls[2] := True;  // Top
      Maze[Y][X].Walls[3] := True;  // Bottom
    end;
end;

procedure TForm1.RenderMaze;
var
  X, Y, C: Integer;
  rct: TRect;
  cell : TMazeCell;
begin
  C := 24;
  Self.Canvas.Brush.Color := clWhite;
  Self.Canvas.Brush.Style := bsSolid;
  Self.Canvas.Pen.Color := clRed;

  for Y := 0 to MAZE_HEIGHT - 1 do
    for X := 0 to MAZE_WIDTH - 1 do
    begin
      cell := Maze[Y][X];
      rct.Top := Y * C;
      rct.Left := X * C;
      rct.Right := rct.Left + C;
      rct.Bottom := rct.Top + C;
      if cell.Visited  then Self.Canvas.FillRect(rct);
      if cell.Walls[0] then Self.Canvas.Line(rct.Left, rct.Top, rct.Left, rct.Bottom);
      if cell.Walls[1] then Self.Canvas.Line(rct.Right, rct.Top, rct.Right, rct.Bottom);
      if cell.Walls[2] then Self.Canvas.Line(rct.Left, rct.Top, rct.Right, rct.Top);
      if cell.Walls[3] then Self.Canvas.Line(rct.Left, rct.Bottom, rct.Right, rct.Bottom);
    end;
end;

end.

