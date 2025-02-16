unit component;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LogUtil, Generics.Collections, animation;

type

  EComponent = (void_component, animation_component, position_component,
    movement_component);

  TComponent = class
  private
    Fid: string;
    FType: EComponent;
  public
    constructor Create(cid: string);
    destructor Destroy; override;
    property id: string read Fid write Fid;
    property etype: EComponent read FType write FType;
  end;

  TPositionComponent = class(TComponent)
  private
    FX: integer;
    FY: integer;
    FPrevX: integer;
    FPrevY: integer;

  public
    constructor Create(cid: string);
    destructor Destroy; override;
    property X: integer read FX write FX;
    property Y: integer read FY write FY;
    property PX: integer read FPrevX write FPrevX;
    property PY: integer read FPrevY write FPrevY;
  end;

  TAnimationComponent = class(TComponent)
  private
    FAnimations: specialize THashMap<string, TAnimation>;
  public
    constructor Create(cid: string);
    destructor Destroy; override;
    procedure SetAnimation(animationKey: string; animationValue: TAnimation);
  end;

  TMovementComponent = class(TComponent)
  private
    DX: integer;
    DY: integer;
  public
    constructor Create(cid: string);
    destructor Destroy; override;
    property X: integer read DX write DX;
    property Y: integer read DY write DY;
  end;

implementation



constructor TComponent.Create(cid: string);
begin
  Self.Fid := cid;
  Self.FType := void_component;
end;

destructor TComponent.Destroy;
begin
  LogDebug('Destroy Component');
end;


constructor TPositionComponent.Create(cid: string);
begin
  inherited;
  FType := position_component;
  FX := 0;
  FY := 0;
end;

destructor TPositionComponent.Destroy;
begin
  inherited;
end;

constructor TAnimationComponent.Create(cid: string);
begin
  inherited;
  FAnimations := specialize THashMap<string, TAnimation>.Create;
end;

destructor TAnimationComponent.Destroy;
var
  AAnimation: TAnimation;
begin
  FAnimations.Clear;
  inherited;
end;

procedure TAnimationComponent.SetAnimation(animationKey: string;
  animationValue: TAnimation);
begin
  FAnimations.Add(animationKey, animationValue);
end;

constructor TMovementComponent.Create(cid: String);
begin
  inherited;
  FType := movement_component;

end;

destructor TMovementComponent.Destroy;
begin
  inherited;
end;


end.
