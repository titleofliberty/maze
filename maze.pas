unit Maze;

{$mode ObjFPC}{$H+}

{
To use this unit you will want to subclass the TMazeCell and the TMaze. Add
any data you want to associate with the TMazeCell, and you want to override
the RenderMaze of TMaze so that it draws the cells the way you want them.
}

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

  { TMazeCell }

  TMazeCell = class
  private
    FVisited: Boolean;
    FWallEast: Boolean;
    FWallNorth: Boolean;
    FWallSouth: Boolean;
    FWallWest: Boolean;
  public
    property Visited  : Boolean read FVisited   write FVisited;
    property WallNorth: Boolean read FWallNorth write FWallNorth;
    property WallSouth: Boolean read FWallSouth write FWallSouth;
    property WallEast : Boolean read FWallEast  write FWallEast;
    property WallWest : Boolean read FWallWest  write FWallWest;
  end;

  { TMaze }

  TMaze = class
  private
    FVisitedCells: Integer;
    FHeight: Integer;
    FWidth : Integer;
    FStack : TObjectStack;
    FCells : array of array of TMazeCell;
    function GetCell(ARow, ACol: Integer): TMazeCell;
    procedure SetCell(ARow, ACol: Integer; AValue: TMazeCell);
    procedure ProcessCell(AX, AY: Integer);
  public
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Cells[ARow, ACol: Integer]: TMazeCell read GetCell write SetCell;
    constructor Create(AWidth, AHeight: Integer);
    procedure RemoveWall(X1, Y1, X2, Y2: Integer);
    procedure InitializeMaze;
    procedure RenderMaze(ACanvas: TCanvas);
    procedure GenerateMaze(AX, AY: Integer);
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

procedure TMaze.ProcessCell(AX, AY: Integer);
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
      0: begin NewX := AX - 1; NewY := AY; end;
      1: begin NewX := AX + 1; NewY := AY; end;
      2: begin NewX := AX; NewY := AY - 1; end;
      3: begin NewX := AX; NewY := AY + 1; end;
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
      0: begin NewX := AX - 1; NewY := AY; end;
      1: begin NewX := AX + 1; NewY := AY; end;
      2: begin NewX := AX; NewY := AY - 1; end;
      3: begin NewX := AX; NewY := AY + 1; end;
    end;
    RemoveWall(AX, AY, NewX, NewY);
    FStack.Push(TMazePoint.Create(NewX, NewY));
    ProcessCell(NewX, NewY);
  end
  else
  begin
    if FStack.Count > 0 then
    begin
      mp := TMazePoint(FStack.Pop);
      ProcessCell(mp.X, mp.Y);
    end;
  end;
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
      FCells[Y2, X2].WallSouth := False;
      FCells[Y1, X1].WallNorth := False;
    end;
  end
  else
  begin
    if X1 < X2 then
    begin
      FCells[Y1, X1].WallEast := False;
      FCells[Y2, X2].WallWest := False;
    end
    else
    begin
      FCells[Y2, X2].WallEast := False;
      FCells[Y1, X1].WallWest := False;
    end;
  end;
end;

procedure TMaze.InitializeMaze;
var
  X, Y: Integer;
begin
  // This procedure is the equivalent of emptying/clearing
  // the maze. It is automatically called when GenerateMaze
  // is called.

  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
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
  // This procedure renders the maze on any canvas.

  C := 24;
  ACanvas.Brush.Color := clWhite;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Color := clBlue;

  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
    begin
      cell := Cells[Y, X];
      rct.Top := Y * C;
      rct.Left := X * C;
      rct.Right := rct.Left + C;
      rct.Bottom := rct.Top + C;
      if cell.Visited   then ACanvas.FillRect(rct);
      if cell.WallWest  then ACanvas.Line(rct.Left, rct.Top, rct.Left, rct.Bottom);
      if cell.WallEast  then ACanvas.Line(rct.Right, rct.Top, rct.Right, rct.Bottom);
      if cell.WallNorth then ACanvas.Line(rct.Left, rct.Top, rct.Right, rct.Top);
      if cell.WallSouth then ACanvas.Line(rct.Left, rct.Bottom, rct.Right, rct.Bottom);
    end;
end;

procedure TMaze.GenerateMaze(AX, AY: Integer);
begin
  //  To generate a maze, TMaze must reset the FStack,
  //  initialize the maze with InitializeMaze, then it
  //  can start processing the cells.

  FStack := TObjectStack.Create;
  InitializeMaze;
  ProcessCell(AX, AY);
end;

end.

