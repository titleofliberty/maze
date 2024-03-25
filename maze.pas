unit Maze;

{$mode ObjFPC}{$H+}

{
To use this unit you will want to subclass the TMazeCell and the TMaze. Add
any data you want to associate with the TMazeCell, and you want to override
the RenderMaze of TMaze so that it draws the cells the way you want them.
}

interface

uses
  Classes, SysUtils, Graphics, Contnrs, fgl;

type

  { TCellRect }

  TCellRect = class
  private
    FBottom: Integer;
    FLeft: Integer;
    FRight: Integer;
    FTop: Integer;
  public
    function AsRect: TRect;
  published
    property Top: Integer read FTop write FTop;
    property Left: Integer read FLeft write FLeft;
    property Right: Integer read FRight write FRight;
    property Bottom: Integer read FBottom write FBottom;
  end;

  { TMazePoint }

  TMazePoint = class
  private
    FRect: TRect;
    FX: Integer;
    FY: Integer;
  public
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property Rect: TRect read FRect;
    constructor Create(AX, AY: Integer);
  end;

  { TMazeCell }

  TMazeCell = class
  private
    FExplored: Boolean;
    FRect: TCellRect;
    FVisited: Boolean;
    FWallEast: Boolean;
    FWallNorth: Boolean;
    FWallSouth: Boolean;
    FWallWest: Boolean;
    FX: Integer;
    FY: Integer;
  public
    constructor Create(AX, AY: Integer);
    function WallCount: Integer;
  published
    property X    : Integer read FX;
    property Y    : Integer read FY;
    property Rect     : TCellRect read FRect write FRect;
    property Visited  : Boolean read FVisited   write FVisited;
    property Explored : Boolean read FExplored  write FExplored;
    property WallNorth: Boolean read FWallNorth write FWallNorth;
    property WallSouth: Boolean read FWallSouth write FWallSouth;
    property WallEast : Boolean read FWallEast  write FWallEast;
    property WallWest : Boolean read FWallWest  write FWallWest;
  end;

  TMazeCellList = specialize TFPGObjectList<TMazeCell>;

  { TMaze }

  TMaze = class
  private
    FCellSize: Integer;
    FDeadEnds: TMazeCellList;
    FPath: TMazeCellList;
    FVisitedCells: Integer;
    FHeight: Integer;
    FWidth : Integer;
    FStack : TObjectStack;
    FCells : array of array of TMazeCell;
    function GetCell(ARow, ACol: Integer): TMazeCell;
    procedure SetCell(ARow, ACol: Integer; AValue: TMazeCell);
    procedure ProcessCell(AX, AY: Integer);
    procedure ExploreCell(AX, AY, GoalX, GoalY: Integer);
  public
    property Path: TMazeCellList read FPath;
    property DeadEnds: TMazeCellList read FDeadEnds;
    property Cells[ARow, ACol: Integer]: TMazeCell read GetCell write SetCell;
    constructor Create(AWidth, AHeight, ACellSize: Integer);
    procedure RemoveWall(X1, Y1, X2, Y2: Integer);
    procedure InitializeMaze;
    procedure RenderMaze(ACanvas: TCanvas; AOffset: TPoint);
    procedure GenerateMaze(AX, AY: Integer);
    procedure FindPath(AX, AY, GoalX, GoalY: Integer);
    function  CompareCells(ACell, BCell: TMazeCell): Boolean;
  published
    property CellSize: Integer read FCellSize write FCellSize;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;

implementation

{ TCellRect }

function TCellRect.AsRect: TRect;
begin
  result := Rect(FLeft, FTop, FRight, FBottom);
end;

{ TMazePoint }

constructor TMazePoint.Create(AX, AY: Integer);
begin
  FX := AX;
  FY := AY;
end;

{ TMazeCell }

constructor TMazeCell.Create(AX, AY: Integer);
begin
  inherited Create;
  FX := AX;
  FY := AY;
  FVisited   := false;
  FExplored  := false;
  FWallNorth := true;
  FWallSouth := true;
  FWallEast  := true;
  FWallWest  := true;
  FRect  := TCellRect.Create;
end;

function TMazeCell.WallCount: Integer;
var
  cnt: Integer;
begin
  cnt := 0;
  if FWallEast  then cnt := cnt + 1;
  if FWallWest  then cnt := cnt + 1;
  if FWallNorth then cnt := cnt + 1;
  if FWallSouth then cnt := cnt + 1;
  result := cnt;
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

procedure TMaze.ExploreCell(AX, AY, GoalX, GoalY: Integer);
var
  Neighbors: TObjectList;
  Cell, C: TMazeCell;
begin
  Cell := FCells[AY, AX];
  Cell.Explored := true;

  if (AX = GoalX) and (AY = GoalY) then Exit;

  Neighbors := TObjectList.Create;

  if (Cell.WallNorth = false) then
  begin
    C := FCells[AY - 1, AX];
    if (C.Explored = false) then Neighbors.Add(C);
  end;

  if (Cell.WallSouth = false) then
  begin
    C := FCells[AY + 1, AX];
    if (C.Explored = false) then  Neighbors.Add(C);
  end;

  if (Cell.WallEast  = false) then
  begin
    C := FCells[AY, AX + 1];
    if (C.Explored = false) then  Neighbors.Add(C);
  end;

  if (Cell.WallWest  = false) then
  begin
    C := FCells[AY, AX - 1];
    if (C.Explored = false) then Neighbors.Add(C);
  end;

  if (Neighbors.Count = 0) then
  begin
    if (FStack.Count > 0) then
    begin
      Cell := TMazeCell(FStack.Pop);
      if Assigned(Cell) then
        ExploreCell(Cell.X, Cell.Y, GoalX, GoalY);
    end;
  end
  else
  begin
    C := TMazeCell(FStack.Peek);
    if (Assigned(C)) and (not C.Equals(Cell)) then
      FStack.Push(Cell);
    Cell := TMazeCell(Neighbors[Random(Neighbors.Count)]);
    if Assigned(Cell) then
    begin
      FStack.Push(Cell);
      ExploreCell(Cell.X, Cell.Y, GoalX, GoalY);
    end;
  end;
end;

constructor TMaze.Create(AWidth, AHeight, ACellSize: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FCellSize := ACellSize;
  SetLength(FCells, FHeight, FWidth);
  FStack := TObjectStack.Create;
  FPath  := TMazeCellList.Create;
  FDeadEnds := TMazeCellList.Create;
  Randomize;
  InitializeMaze;
  GenerateMaze(0, 0);
end;

procedure TMaze.RemoveWall(X1, Y1, X2, Y2: Integer);
begin
  if X1 = X2 then // Xs equal
  begin
    if Y1 < Y2 then // North
    begin
      FCells[Y1, X1].WallSouth := False;
      FCells[Y2, X2].WallNorth := False;
    end
    else // South
    begin
      FCells[Y2, X2].WallSouth := False;
      FCells[Y1, X1].WallNorth := False;
    end;
  end
  else // Ys equal
  begin
    if X1 < X2 then // East
    begin
      FCells[Y1, X1].WallEast := False;
      FCells[Y2, X2].WallWest := False;
    end
    else // West
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
  begin
    for X := 0 to FWidth - 1 do
    begin
      FCells[Y, X] := TMazeCell.Create(X, Y);
      FCells[Y, X].Visited   := False;
      FCells[Y, X].WallNorth := True;
      FCells[Y, X].WallSouth := True;
      FCells[Y, X].WallEast  := True;
      FCells[Y, X].WallWest  := True;
      FCells[Y, X].Rect.Top := Y * FCellSize;
      FCells[Y, X].Rect.Left := X * FCellSize;
      FCells[Y, X].Rect.Right := FCells[Y, X].Rect.Left + FCellSize;
      FCells[Y, X].Rect.Bottom := FCells[Y, X].Rect.Top + FCellSize;
    end;
  end;
end;

procedure TMaze.RenderMaze(ACanvas: TCanvas; AOffset: TPoint);
var
  X, Y, I: Integer;
  cell : TMazeCell;
  rct  : TRect;
begin
  // This procedure renders the maze on any canvas.
  //ACanvas.Brush.Color := clWhite;
  //ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Color := clWindowText;

  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
    begin
      cell := Cells[Y, X];
      rct  := cell.Rect.AsRect;
      rct.Offset(AOffset);
      if cell.WallWest  then ACanvas.Line(rct.Left,  rct.Top,    rct.Left,  rct.Bottom);
      if cell.WallEast  then ACanvas.Line(rct.Right, rct.Top,    rct.Right, rct.Bottom);
      if cell.WallNorth then ACanvas.Line(rct.Left,  rct.Top,    rct.Right, rct.Top);
      if cell.WallSouth then ACanvas.Line(rct.Left,  rct.Bottom, rct.Right, rct.Bottom);
    end;
end;

procedure TMaze.GenerateMaze(AX, AY: Integer);
var
  X, Y: Integer;
begin
  //  To generate a maze, TMaze must reset the FStack,
  //  initialize the maze with InitializeMaze, then it
  //  can start processing the cells.
  FStack := TObjectStack.Create;
  FPath  := TMazeCellList.Create;
  InitializeMaze;
  ProcessCell(AX, AY);
  FDeadEnds := TMazeCellList.Create;

  // Find zones and the cells that are dead ends;
  for Y := 0 to FHeight - 1 do
    for X := 0 to FWidth - 1 do
    begin
      if FCells[Y, X].WallCount = 3 then
        FDeadEnds.Add(FCells[Y, X]);
    end;
end;

procedure TMaze.FindPath(AX, AY, GoalX, GoalY: Integer);
begin
  FStack := TObjectStack.Create;
  FStack.Push(FCells[AX, AY]);
  ExploreCell(AX, AY, GoalX, GoalY);
  while FStack.Count > 0 do FPath.Insert(0, TMazeCell(FStack.Pop));
end;

function TMaze.CompareCells(ACell, BCell: TMazeCell): Boolean;
begin
  result := (ACell.X = BCell.X) and (ACell.Y = BCell.Y);
end;

end.

