unit component;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, animation, game_types, inventory;

type

  EComponent = (void_component, animation_component, position_component,
    movement_component, inventory_component, teleport_component, transition_component);

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


  { TDepthComponent }

  TDepthComponent = class(TComponent)
  private
    FDepth: integer;
  public
    constructor Create(cid: string);
    destructor Destroy; override;
    property depth: integer read FDepth write FDepth;
  end;

  TMovementComponent = class(TComponent)
  private
    DX: integer;
    DY: integer;
    FBoost: integer;
    FBoostCooldown: integer;
  public
    constructor Create(cid: string);
    destructor Destroy; override;
    property X: integer read DX write DX;
    property Y: integer read DY write DY;
    property Boost: integer read FBoost write FBoost;
    property Cooldown: integer read FBoostCooldown write FBoostCooldown;
  end;

  TInputComponent = class(TComponent)
  private
    FLeft: boolean;
    FRight: boolean;
    FUp: boolean;
    FDown: boolean;
    FBoost: boolean;
  public
    constructor Create(cid: string);
    destructor Destroy; override;
    property left: boolean read FLeft write FLeft;
    property right: boolean read FRight write FRight;
    property up: boolean read FUp write FUp;
    property down: boolean read FDown write FDown;
    property boost: boolean read FBoost write FBoost;

  end;

  TCollideComponent = class(TComponent)
  private
    FBoundBox: RRect;
  public
    constructor Create(cid: string);
    destructor Destroy; override;
    property BoundBox: RRect read FBoundBox write FBoundBox;
  end;

  { TTeleportComponent }

  TTeleportComponent = class(TComponent)
  private
    FPos: RVec2;
  public
    constructor Create(cid: string);
    destructor Destroy; override;
    property Pos: RVec2 read FPos write FPos;
  end;

  { TInventoryComponent }
  TInventoryComponent = class(TComponent)
    private
      FInventory: TInventory;
    public
      constructor Create(cid: string);
  end;

  { TTransitionComponent }

  ETransition = (size_transition, position_transition);

  TTransitionComponent = class(TComponent)
    private
      FTransitionType: ETransition;

      FStartV: Integer;
      FEndV: Integer;

      FCurrent: Integer;

      { progress 0~ 1까지 }
      FProgress: Real;

      { Active }
      FIsActive: Boolean;

      { Duration }
      FDuration: Integer;
    public
      constructor Create(cid: string);
      destructor Destroy; override;
      property TransitionType: ETransition read FTransitionType write FTransitionType;
      property StartV: Integer read FStartV write FStartV;
      property EndV: Integer read FEndV write FEndV;
      property Current: Integer read FCurrent write FCurrent;
      property Progress: Real read FProgress write FProgress;
      property IsActive: Boolean read FIsActive write FIsActive;
      property Duration: Integer read FDuration write FDuration;
  end;

implementation

uses
  LogUtil;

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
  inherited Create(cid);
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
  LogDebug(Format('%s : animation component created', [cid]));
  FAnimations := specialize THashMap<string, TAnimation>.Create;
end;

destructor TAnimationComponent.Destroy;
begin

  LogDebug(Format('TAnimationComponent.Destroy : %s', [Self.Fid]));
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
  FCurrentAnimation := animationKey;
end;

{ TDepthComponent }

constructor TDepthComponent.Create(cid: string);
begin
  inherited;
  FDepth := 0;
end;

destructor TDepthComponent.Destroy;
begin
  inherited Destroy;
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

constructor TCollideComponent.Create(cid: string);
begin
  inherited;
end;

destructor TCollideComponent.Destroy;
begin
  inherited;
end;

{ TTeleportComponent }

constructor TTeleportComponent.Create(cid: string);
begin
  inherited;
end;

destructor TTeleportComponent.Destroy;
begin
  inherited Destroy;
end;

{ TInventoryComponent }

constructor TInventoryComponent.Create(cid: string);
begin
  inherited Create(cid);
  Self.FType:= inventory_component;
end;

{ TTransitionComponent }

constructor TTransitionComponent.Create(cid: string);
begin
  inherited Create(cid);
  Self.FType:= transition_component;
end;

destructor TTransitionComponent.Destroy;
begin
  inherited Destroy;
end;

end.
