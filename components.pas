unit Components;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LogUtil ;

type

  EComponent = (void_component, animation, position, movement);

  TComponent = class
  private
    Fid: string;
    FType: EComponent;
    function GetId(): string;
    procedure SetType(CompType: EComponent);
  public
    constructor Create(id: string);
    destructor Destroy; override;
    property cid: string read Fid;
    property etype: EComponent read FType write SetType;
  end;

  TPositionComponent = class(TComponent)
    private
      FX: Integer;
      FY: Integer;
      Procedure SetX(X: Integer);
      Procedure SetY(Y: Integer);
    public
      constructor Create(id: String);
      destructor Destroy; override;
      property X: Integer read FX write SetX;
      property Y: Integer read FY write SetY;
  end;

implementation

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

constructor TPositionComponent.Create(id: String);
begin
  inherited;
   FX := 0;
   FY := 0;
end;

destructor TPositionComponent.Destroy;
begin
   inherited;
end;

procedure TPositionComponent.SetX(X: Integer);
begin
   FX := X;
end;

procedure TPositionComponent.SetY(Y: Integer);
begin
   FY := Y;
end;

end.
