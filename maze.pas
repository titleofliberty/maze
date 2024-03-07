unit Maze;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Contnrs;


type

  { TMazePoint }

  TMazePoint = class
  private
    FX: Integer;
    FY: Integer;
  public
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    constructor Create(AX, AY: Integer);
  end;

  TMazeCell = class
  public
    property Visited  : Boolean read FVisited   write FVisited;
    property WallNorth: Boolean read FWallNorth write FWallNorth;
    property WallSouth: Boolean read FWallSouth write FWallSouth;
    property WallEast : Boolean read FWallEast  write FWallEast;
    property WallWest : Boolean read FWallWest  write FWallWest;
  end;

  TMaze = class
  private
    FVisitedCells: Integer;
    FHeight: Integer;
    FWidth : Integer;
    FStack : TObjectStack;
    FCells : array of TMazeCell;
    function GetCell(ARow, ACol: Integer): TMazeCell;
    procedure SetCell(ARow, ACol: Integer; AValue: TMazeCell);
    procedure GenerateMaze(AX, AY: Integer);
  public
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Cells[ARow, ACol: Integer]: TMazeCell read GetCell write SetCell;
    constructor Create(AWidth, AHeight: Integer);
    procedure RemoveWall(X1, Y1, X2, Y2: Integer);
    procedure InitializeMaze;
    procedure RenderMaze(ACanvas: TCanvas);
  end;

implementation

{ TMazePoint }

constructor TMazePoint.Create(AX, AY: Integer);
begin
  FX := AX;
  FY := AY;
end;

function TMaze.GetCell(ARow, ACol: Integer): TMazeCell;
begin
  result := FCells[ARow, ACol];
end;

procedure TMaze.SetCell(ARow, ACol: Integer; AValue: TMazeCell);
begin
  FCells[ARow, ACol] := AValue;
end;

constructor TMaze.Create(AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  SetLength(FCells, FHeight, FWidth);
  FStack := TObjectStack.Create;
  Randomize;
  InitializeMaze;
  GenerateMaze(0, 0);

end;

procedure TMaze.RemoveWall(X1, Y1, X2, Y2: Integer);
begin
  if X1 = X2 then
  begin
    if Y1 < Y2 then
    begin
      FCells[Y1, X1].WallSouth := False;
      FCells[Y2, X2].WallNorth := False;
    end
    else
    begin
      Maze[Y2, X2].WallSouth := False;
      Maze[Y1, X1].WallNorth := False;
    end;
  end
  else
  begin
    if X1 < X2 then
    begin
      Maze[Y1, X1].WallEast := False;
      Maze[Y2, X2].WallWest := False;
    end
    else
    begin
      Maze[Y2, X2].WallEast := False;
      Maze[Y1, X1].WallWest := False;
    end;
  end;
end;

procedure TMaze.InitializeMaze;
var
  X, Y: Integer;
begin
  for Y := 0 to MAZE_HEIGHT - 1 do
    for X := 0 to MAZE_WIDTH - 1 do
    begin
      FCells[Y, X] := TMazeCell.Create;
      FCells[Y, X].Visited   := False;
      FCells[Y, X].WallNorth := True;
      FCells[Y, X].WallSouth := True;
      FCells[Y, X].WallEast  := True;
      FCells[Y, X].WallWest  := True;
    end;
end;

procedure TMaze.RenderMaze(ACanvas: TCanvas);
var
  X, Y, C: Integer;
  rct: TRect;
  cell : TMazeCell;
begin
  C := 24;
  Self.Canvas.Brush.Color := clWhite;
  Self.Canvas.Brush.Style := bsSolid;
  Self.Canvas.Pen.Color := clRed;

  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
    begin
      cell := Cells[Y, X];
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

procedure TMaze.GenerateMaze(AX, AY: Integer);
var
  Neighbors: array[0..3] of Integer;
  NeighborCount, RandomNeighbor: Integer;
  i: Integer;
  NewX, NewY: Integer;
  mp : TMazePoint;
begin
  FCells[AY, AX].Visited := True;
  Inc(FVisitedCells);

  NeighborCount := 0;
  for i := 0 to 3 do
  begin
    case i of
      0: begin NewX := X - 1; NewY := Y; end;
      1: begin NewX := X + 1; NewY := Y; end;
      2: begin NewX := X; NewY := Y - 1; end;
      3: begin NewX := X; NewY := Y + 1; end;
    end;

    if (NewX >= 0) and (NewX < FWidth) and (NewY >= 0) and (NewY < FHeight)
    and not FCells[NewY, NewX].Visited then
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
    FStack.Push(TMazePoint.Create(NewX, NewY));
    GenerateMaze(NewX, NewY);
  end
  else
  begin
    if FStack.Count > 0 then
    begin
      mp := TMazePoint(Stack.Pop);
      GenerateMaze(mp.X, mp.Y);
    end;
  end;
end;

end.

