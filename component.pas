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
    FCurrentAnimation: string;
    FCurrentTick: integer;
    FCurrentFrame: integer;
    FCurrentMaxFrame: integer;
    FDuration: integer;
    FIsLoop: boolean;
  public
    constructor Create(cid: string);
    destructor Destroy; override;
    procedure SetAnimation(animationKey: string; animationValue: TAnimation);
    procedure SetCurrentAnimation(animationKey: string);
    property CurrentAnimation: string read FCurrentAnimation write SetCurrentAnimation;
    property Duration: integer read FDuration write FDuration;
    property IsLoop: boolean read FIsLoop write FIsLoop;
    property CurrentTick: integer read FCurrentTick write FCurrentTick;
    property CurrentFrame: integer read FCurrentFrame write FCurrentFrame;
    property CurrentMaxFrame: integer read FCurrentmaxFrame write FCurrentMaxFrame;
    property Animations: specialize THashMap<string, TAnimation> read FAnimations;
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

  TInputComponent = class(TComponent)
  public
    FLeft: boolean;
    FRight: boolean;
    FUp: boolean;
    FDown: boolean;
    constructor Create(cid: string);
    destructor Destroy; override;
    property left: boolean read FLeft write FLeft;
    property right: boolean read FRight write FRight;
    property up: boolean read FUp write FUp;
    property down: boolean read FDown write FDown;

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
begin

  LogDebug('TAnimationComponent.Destroy');
  FAnimations.Free;
  inherited;
end;

procedure TAnimationComponent.SetCurrentAnimation(animationKey: string);
var
  AAnimation: TAnimation;
begin
  if FAnimations.ContainsKey(animationKey) then
  begin
    FCurrentAnimation := animationKey;
    FCurrentFrame := 0;
    FCurrentTick := 0;
    FAnimations.TryGetValue(animationKey, AAnimation);

    if AAnimation <> nil then
      FCurrentMaxFrame := AAnimation.GetFrameLength - 1
    else
      FCurrentMaxFrame := 0;

  end;

end;

procedure TAnimationComponent.SetAnimation(animationKey: string;
  animationValue: TAnimation);
begin
  FAnimations.Add(animationKey, animationValue);
end;

constructor TMovementComponent.Create(cid: string);
begin
  inherited;
  FType := movement_component;

end;

destructor TMovementComponent.Destroy;
begin
  inherited;
end;


constructor TInputComponent.Create(cid: string);
begin
  inherited;
  FLeft := False;
  FRight := False;
  FUp := False;
  FDown := False;
end;

destructor TInputComponent.Destroy;
begin
  inherited;
end;

end.
