unit entity;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Components, LogUtil;

type

  TPositionComponent = class;
  TEntity = class
  private
    FPosition: TPositionComponent;
    procedure SetPosition(CompPosition: TPositionComponent);
  public
    constructor Create;
    destructor Destroy; override;
    property position: TPositionComponent read FPosition write SetPosition;
  end;


  EComponent = (void_component, animation, position, movement);

  TComponent = class
  private
    Fid: string;
    FType: EComponent;
    FEntity: TEntity;
    function GetId(): string;
    procedure SetType(CompType: EComponent);
    procedure SetEntity(AEntity: TEntity);
  public
    constructor Create(id: string);
    destructor Destroy; override;
    property cid: string read Fid;
    property etype: EComponent read FType write SetType;
    property entity: TEntity read FEntity write SetEntity;
  end;

  TPositionComponent = class(TComponent)
  private
    FX: integer;
    FY: integer;
    procedure SetX(X: integer);
    procedure SetY(Y: integer);
  public
    constructor Create(id: string);
    destructor Destroy; override;
    property X: integer read FX write SetX;
    property Y: integer read FY write SetY;
  end;


implementation

constructor TEntity.Create;
begin

end;

destructor TEntity.Destroy;
begin
  if Assigned(FPosition) then
  begin
    LogDebug('Remove Position Component');
    FreeAndNil(FPosition);

  end;
end;

procedure TEntity.SetPosition(CompPosition: TPositionComponent);
begin
  Self.FPosition := CompPosition;
end;


function TComponent.GetId(): string;
begin
  Result := Fid;
end;

procedure TComponent.SetType(CompType: EComponent);
begin
  FType := CompType;
end;

constructor TComponent.Create(id: string);
begin
  Self.Fid := id;
  Self.FType := void_component;
end;

destructor TComponent.Destroy;
begin
  LogDebug('Destroy Component');
end;

procedure TComponent.SetEntity(AEntity: TEntity);
begin
  FEntity := AEntity;
end;

constructor TPositionComponent.Create(id: string);
begin
  inherited;
  FX := 0;
  FY := 0;
end;

destructor TPositionComponent.Destroy;
begin
  inherited;
end;

procedure TPositionComponent.SetX(X: integer);
begin
  FX := X;
end;

procedure TPositionComponent.SetY(Y: integer);
begin
  FY := Y;
end;

end.
